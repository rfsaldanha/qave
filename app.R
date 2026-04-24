library(shiny)
library(bslib)
library(shinyWidgets)
library(arrow)
library(dplyr)
library(DBI)
library(RPostgres)
library(stringi)

# Game setup shared across the app.
round_count <- 10L
difficulty_choices <- c(
  "Fácil" = "easy",
  "Médio" = "medium",
  "Difícil" = "hard"
)
difficulty_multipliers <- c(
  easy = 1,
  medium = 1.25,
  hard = 1.5
)
default_difficulty <- "medium"
app_base_url <- Sys.getenv(
  "APP_BASE_URL",
  "https://qave.com.br"
)
app_title <- "Que ave é essa? Quiz de aves do Brasil"
app_description <- paste(
  "Jogue um quiz online para identificar aves do Brasil por foto e som.",
  "Explore espécies observadas em vários estados brasileiros, filtre por família",
  "e acompanhe o ranking de pontuações."
)
app_keywords <- paste(
  c(
    "quiz de aves",
    "aves do Brasil",
    "identificacao de aves",
    "bird quiz Brazil",
    "ornitologia",
    "iNaturalist",
    "aves brasileiras"
  ),
  collapse = ", "
)
app_structured_data <- sprintf(
  paste(
    "{",
    "\"@context\":\"https://schema.org\",",
    "\"@graph\":[",
    "{",
    "\"@type\":\"WebSite\",",
    "\"name\":\"Que ave é essa?\",",
    "\"url\":\"%s\",",
    "\"inLanguage\":\"pt-BR\",",
    "\"description\":\"%s\"",
    "},",
    "{",
    "\"@type\":\"WebApplication\",",
    "\"name\":\"Que ave é essa?\",",
    "\"url\":\"%s\",",
    "\"applicationCategory\":\"GameApplication\",",
    "\"operatingSystem\":\"Web\",",
    "\"inLanguage\":\"pt-BR\",",
    "\"description\":\"%s\",",
    "\"offers\":{\"@type\":\"Offer\",\"price\":\"0\",\"priceCurrency\":\"BRL\"}",
    "},",
    "{",
    "\"@type\":\"FAQPage\",",
    "\"mainEntity\":[",
    "{",
    "\"@type\":\"Question\",",
    "\"name\":\"Como funciona o quiz?\",",
    "\"acceptedAnswer\":{\"@type\":\"Answer\",\"text\":\"Cada partida tem 10 rodadas. Em cada rodada, voce ve a foto de uma ave, pode ouvir a vocalizacao quando disponivel e escolhe uma entre quatro alternativas.\"}",
    "},",
    "{",
    "\"@type\":\"Question\",",
    "\"name\":\"Quais aves aparecem no jogo?\",",
    "\"acceptedAnswer\":{\"@type\":\"Answer\",\"text\":\"O app usa registros de aves observadas em estados do Brasil com foto disponivel e, quando possivel, audio e descricao breve.\"}",
    "},",
    "{",
    "\"@type\":\"Question\",",
    "\"name\":\"Posso filtrar por estado e familia?\",",
    "\"acceptedAnswer\":{\"@type\":\"Answer\",\"text\":\"Sim. Antes de iniciar a partida, voce pode escolher um ou mais estados e limitar o jogo a familias especificas de aves.\"}",
    "}",
    "]",
    "}",
    "]",
    "}"
  ),
  app_base_url,
  app_description,
  app_base_url,
  app_description
)

required_env <- function(name) {
  value <- Sys.getenv(name, "")

  if (!nzchar(value)) {
    stop(
      sprintf(
        paste(
          "Missing required environment variable %s.",
          "Set it in your local .Renviron before running the app."
        ),
        shQuote(name)
      ),
      call. = FALSE
    )
  }

  value
}

db_config <- list(
  dbname = Sys.getenv("DB_NAME", "qave-leaderboard"),
  host = Sys.getenv(
    "DB_HOST",
    "db-postgresql-sfo3-66309-do-user-737434-0.m.db.ondigitalocean.com"
  ),
  port = as.integer(Sys.getenv("DB_PORT", "25060")),
  user = Sys.getenv("DB_USER", "qave-app"),
  password = required_env("DB_PASSWORD"),
  sslmode = Sys.getenv("DB_SSLMODE", "require")
)
db_connection <- NULL
leaderboard_cache <- NULL
leaderboard_cache_loaded_at <- as.POSIXct(NA)
leaderboard_cache_ttl_secs <- 30
leaderboard_prior_games <- 5L
state_name_lookup <- c(
  ac = "Acre",
  al = "Alagoas",
  am = "Amazonas",
  ap = "Amapá",
  ba = "Bahia",
  ce = "Ceará",
  df = "Distrito Federal",
  es = "Espírito Santo",
  go = "Goiás",
  ma = "Maranhão",
  mg = "Minas Gerais",
  ms = "Mato Grosso do Sul",
  mt = "Mato Grosso",
  pa = "Pará",
  pb = "Paraíba",
  pe = "Pernambuco",
  pi = "Piauí",
  pr = "Paraná",
  rj = "Rio de Janeiro",
  rn = "Rio Grande do Norte",
  ro = "Rondônia",
  rr = "Roraima",
  rs = "Rio Grande do Sul",
  sc = "Santa Catarina",
  se = "Sergipe",
  sp = "São Paulo",
  to = "Tocantins"
)

# Normalize user guesses and species names so matching is accent- and case-insensitive.
normalize_answer <- function(value) {
  normalized <- trimws(value)
  normalized <- stringi::stri_trans_general(normalized, "Latin-ASCII")
  normalized <- tolower(normalized)
  normalized <- gsub("[^a-z0-9]+", " ", normalized)
  normalized <- gsub("\\s+", " ", normalized)
  trimws(normalized)
}

# Choose one usable photo URL from the list available for a species.
sample_species_photo <- function(photo_urls) {
  # Each round uses a single randomly chosen photo from the species gallery.
  valid_photos <- photo_urls[!is.na(photo_urls) & nzchar(photo_urls)]

  if (length(valid_photos) == 0) {
    return(NA_character_)
  }

  sample(valid_photos, size = 1)
}

# Prefer the medium photo variant to reduce payload while keeping good quality.
normalize_photo_url <- function(url) {
  if (is.null(url) || length(url) == 0 || all(is.na(url))) {
    return(NA_character_)
  }

  normalized_url <- trimws(url)

  if (is.na(normalized_url) || !nzchar(normalized_url)) {
    return(NA_character_)
  }

  sub(
    "([/_])large(?=\\.[A-Za-z0-9]+(?:\\?|$)|\\?|$)",
    "\\1medium",
    normalized_url,
    perl = TRUE
  )
}

# Return an existing column or an NA vector when the source column is absent.
column_or_na <- function(data, column) {
  if (column %in% names(data)) {
    data[[column]]
  } else {
    rep(NA_character_, nrow(data))
  }
}

# Normalize optional text fields so the UI can rely on a consistent fallback.
value_or_default <- function(value, default) {
  ifelse(!is.na(value) & nzchar(trimws(value)), trimws(value), default)
}

# Open or refresh the shared Postgres connection used by the app.
connect_db <- function(config = db_config) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = config$dbname,
    host = config$host,
    port = config$port,
    user = config$user,
    password = config$password,
    sslmode = config$sslmode
  )
}

# Reuse one Postgres connection so leaderboard queries do not pay a reconnect cost.
get_db_connection <- function(config = db_config, reset = FALSE) {
  if (
    reset ||
      is.null(db_connection) ||
      !DBI::dbIsValid(db_connection)
  ) {
    if (!is.null(db_connection) && DBI::dbIsValid(db_connection)) {
      DBI::dbDisconnect(db_connection)
    }

    db_connection <<- connect_db(config)
  }

  db_connection
}

# Run one database operation and reconnect once if the shared handle has gone stale.
with_db_connection <- function(fn, config = db_config) {
  conn <- get_db_connection(config)

  tryCatch(
    fn(conn),
    error = function(err) {
      if (!DBI::dbIsValid(conn)) {
        conn <- get_db_connection(config, reset = TRUE)
        return(fn(conn))
      }

      stop(err)
    }
  )
}

# Cache the top leaderboard rows briefly to keep the UI responsive between refreshes.
update_leaderboard_cache <- function(data) {
  leaderboard_cache <<- data
  leaderboard_cache_loaded_at <<- Sys.time()
  invisible(data)
}

# Determine whether the cached leaderboard can satisfy the current request.
leaderboard_cache_is_fresh <- function(limit) {
  !is.null(leaderboard_cache) &&
    !is.na(leaderboard_cache_loaded_at) &&
    nrow(leaderboard_cache) >= limit &&
    as.numeric(difftime(
      Sys.time(),
      leaderboard_cache_loaded_at,
      units = "secs"
    )) <
      leaderboard_cache_ttl_secs
}

# Discover the parquet files available for each Brazilian state.
list_state_files <- function(
  pattern = "^inaturalist_birds_([a-z]{2})\\.parquet$"
) {
  parquet_files <- list.files(
    path = ".",
    pattern = pattern,
    full.names = TRUE
  )

  if (length(parquet_files) == 0) {
    stop("No state parquet files were found in the application directory.")
  }

  state_codes <- sub(pattern, "\\1", basename(parquet_files))
  state_labels <- ifelse(
    state_codes %in% names(state_name_lookup),
    sprintf("%s (%s)", state_name_lookup[state_codes], toupper(state_codes)),
    toupper(state_codes)
  )

  data.frame(
    state_code = state_codes,
    state_label = unname(state_labels),
    path = parquet_files,
    stringsAsFactors = FALSE
  ) |>
    dplyr::arrange(state_label)
}

# Load one state dataset and normalize source-specific column names for the app.
load_state_species_data <- function(path, state_code, state_label) {
  species <- arrow::read_parquet(path) |>
    as.data.frame()

  species$audio_url <- dplyr::coalesce(
    column_or_na(species, "audio_url"),
    column_or_na(species, "sound_url")
  )
  species$brief_description <- dplyr::coalesce(
    column_or_na(species, "brief_description"),
    column_or_na(species, "description_paragraph")
  )
  species$wiki_url <- dplyr::coalesce(
    column_or_na(species, "wiki_url"),
    column_or_na(species, "description_url")
  )

  species <- species |>
    dplyr::filter(
      !is.na(common_name),
      !is.na(scientific_name),
      nzchar(common_name),
      nzchar(scientific_name),
      lengths(photo_urls) > 0
    ) |>
    dplyr::mutate(
      # Keep only usable media URLs and precompute one display photo per species.
      family = dplyr::if_else(
        !is.na(family) & nzchar(family),
        family,
        "Sem familia informada"
      ),
      photo_urls = lapply(photo_urls, function(urls) {
        normalized_urls <- vapply(urls, normalize_photo_url, character(1))
        normalized_urls[!is.na(normalized_urls) & nzchar(normalized_urls)]
      }),
      has_photo = lengths(photo_urls) > 0,
      round_photo = vapply(photo_urls, sample_species_photo, character(1)),
      brief_description = value_or_default(
        brief_description,
        "Sem descrição breve disponível para esta espécie."
      ),
      audio_url = ifelse(
        !is.na(audio_url) & nzchar(trimws(audio_url)),
        trimws(audio_url),
        NA_character_
      ),
      wiki_url = ifelse(
        !is.na(wiki_url) & nzchar(trimws(wiki_url)),
        trimws(wiki_url),
        NA_character_
      ),
      state_code = state_code,
      state_label = state_label,
      common_name_norm = vapply(common_name, normalize_answer, character(1)),
      scientific_name_norm = vapply(
        scientific_name,
        normalize_answer,
        character(1)
      )
    ) |>
    dplyr::filter(has_photo, !is.na(round_photo), nzchar(round_photo)) |>
    dplyr::select(-has_photo) |>
    dplyr::distinct(common_name, scientific_name, .keep_all = TRUE)

  species
}

# Load all state datasets up front so the app can filter them reactively.
load_species_data <- function(state_files = list_state_files()) {
  species_by_state <- lapply(seq_len(nrow(state_files)), function(i) {
    load_state_species_data(
      path = state_files$path[[i]],
      state_code = state_files$state_code[[i]],
      state_label = state_files$state_label[[i]]
    )
  })

  dplyr::bind_rows(species_by_state)
}

# Create the Postgres tables used to store scores and leaderboard totals.
init_db <- function(config = db_config) {
  with_db_connection(
    function(conn) {
      DBI::dbExecute(conn, "SET client_min_messages TO warning")

      DBI::dbExecute(
        conn,
        paste(
          "CREATE TABLE IF NOT EXISTS game_results (",
          "id BIGSERIAL PRIMARY KEY,",
          "username TEXT NOT NULL,",
          "score INTEGER NOT NULL,",
          "rounds INTEGER NOT NULL,",
          "played_at TIMESTAMPTZ NOT NULL",
          ")"
        )
      )

      DBI::dbExecute(
        conn,
        paste(
          "CREATE TABLE IF NOT EXISTS leaderboard (",
          "username TEXT PRIMARY KEY,",
          "total_score INTEGER NOT NULL DEFAULT 0,",
          "games_played INTEGER NOT NULL DEFAULT 0,",
          "best_score INTEGER NOT NULL DEFAULT 0,",
          "last_score INTEGER NOT NULL DEFAULT 0,",
          "last_played_at TIMESTAMPTZ NOT NULL",
          ")"
        )
      )

      DBI::dbExecute(
        conn,
        paste(
          "CREATE INDEX IF NOT EXISTS leaderboard_fair_rank_idx",
          "ON leaderboard (best_score DESC, total_score DESC, last_played_at DESC)"
        )
      )
    },
    config = config
  )
}

# Save one finished game and update the aggregate leaderboard for the same user.
record_score <- function(
  username,
  score,
  rounds = round_count,
  config = db_config
) {
  played_at <- Sys.time()

  with_db_connection(
    function(conn) {
      DBI::dbExecute(
        conn,
        "INSERT INTO game_results (username, score, rounds, played_at) VALUES ($1, $2, $3, $4)",
        params = list(username, score, rounds, played_at)
      )

      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO leaderboard (username, total_score, games_played, best_score, last_score, last_played_at)",
          "VALUES ($1, $2, 1, $3, $4, $5)",
          "ON CONFLICT(username) DO UPDATE SET",
          "total_score = leaderboard.total_score + excluded.total_score,",
          "games_played = leaderboard.games_played + 1,",
          "best_score = GREATEST(leaderboard.best_score, excluded.best_score),",
          "last_score = excluded.last_score,",
          "last_played_at = excluded.last_played_at"
        ),
        params = list(username, score, score, score, played_at)
      )
    },
    config = config
  )
}

# Read the top scores shown on the start screen and after a game ends.
read_leaderboard <- function(limit = 10L, config = db_config, refresh = FALSE) {
  limit <- as.integer(limit)

  if (!refresh && leaderboard_cache_is_fresh(limit)) {
    return(utils::head(leaderboard_cache, limit))
  }

  leaderboard <- with_db_connection(
    function(conn) {
      DBI::dbGetQuery(
        conn,
        paste(
          "WITH ranked_leaderboard AS (",
          "SELECT",
          "username,",
          "best_score,",
          "ROUND(total_score::numeric / NULLIF(games_played, 0), 2) AS average_score,",
          "ROUND((",
          "total_score::numeric +",
          "$2::numeric * COALESCE((SUM(total_score) OVER ())::numeric / NULLIF(SUM(games_played) OVER (), 0), 0)",
          ") / NULLIF(games_played + $2, 0), 2) AS fair_score,",
          "total_score,",
          "games_played,",
          "last_score,",
          "last_played_at",
          "FROM leaderboard",
          ")",
          "SELECT",
          "username,",
          "best_score,",
          "average_score,",
          "fair_score,",
          "total_score,",
          "games_played,",
          "last_score,",
          "last_played_at",
          "FROM ranked_leaderboard",
          "ORDER BY",
          "fair_score DESC,",
          "average_score DESC,",
          "games_played DESC,",
          "best_score DESC,",
          "total_score DESC,",
          "last_played_at DESC",
          "LIMIT $1"
        ),
        params = list(limit, leaderboard_prior_games)
      )
    },
    config = config
  )

  update_leaderboard_cache(leaderboard)
  leaderboard
}

# Load game data and ensure the score database exists before the app starts.
state_files <- list_state_files()
species_data <- load_species_data(state_files)
state_choices <- stats::setNames(
  state_files$state_code,
  state_files$state_label
)
all_state_codes <- unname(state_choices)
init_db()

# Define the visual theme used by the Shiny app.
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1f6f50",
  secondary = "#f0b429",
  bg = "#f6f1e7",
  fg = "#1f2933",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Bree Serif")
)

# Build the static page shell; the main content area is filled reactively by the server.
ui <- page_fluid(
  title = app_title,
  theme = app_theme,
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$meta(name = "description", content = app_description),
    tags$meta(name = "keywords", content = app_keywords),
    tags$meta(
      name = "robots",
      content = "index,follow,max-image-preview:large"
    ),
    tags$link(rel = "canonical", href = app_base_url),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:locale", content = "pt_BR"),
    tags$meta(property = "og:site_name", content = "Que ave é essa?"),
    tags$meta(property = "og:title", content = app_title),
    tags$meta(property = "og:description", content = app_description),
    tags$meta(property = "og:url", content = app_base_url),
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:title", content = app_title),
    tags$meta(name = "twitter:description", content = app_description),
    tags$script(HTML("document.documentElement.lang = 'pt-BR';")),
    tags$script(type = "application/ld+json", HTML(app_structured_data)),
    tags$script(HTML(
      "
      (function() {
        var cookieMaxAge = 60 * 60 * 24 * 180;
        var handlersRegistered = false;

        function setCookie(name, value) {
          document.cookie = name + '=' + encodeURIComponent(value) +
            '; max-age=' + cookieMaxAge + '; path=/; SameSite=Lax';
        }

        function getCookie(name) {
          var prefix = name + '=';
          var cookies = document.cookie ? document.cookie.split('; ') : [];

          for (var i = 0; i < cookies.length; i++) {
            if (cookies[i].indexOf(prefix) === 0) {
              return decodeURIComponent(cookies[i].slice(prefix.length));
            }
          }

          return '';
        }

        function parseArrayCookie(name) {
          var value = getCookie(name);

          if (!value) {
            return [];
          }

          try {
            var parsed = JSON.parse(value);
            return Array.isArray(parsed) ? parsed : [];
          } catch (error) {
            return [];
          }
        }

        function sendSavedPreferences() {
          if (!window.Shiny || !Shiny.setInputValue) {
            return;
          }

          Shiny.setInputValue('qave_saved_preferences', {
            username: getCookie('qave_username'),
            states: parseArrayCookie('qave_states'),
            families: parseArrayCookie('qave_families'),
            difficulty: getCookie('qave_difficulty'),
            nonce: Math.random()
          }, { priority: 'event' });
        }

        function registerMessageHandlers() {
          if (handlersRegistered) {
            return;
          }

          if (!window.Shiny || !Shiny.addCustomMessageHandler) {
            return;
          }

          Shiny.addCustomMessageHandler('qave_save_preferences', function(data) {
            setCookie('qave_username', data.username || '');
            setCookie('qave_states', JSON.stringify(data.states || []));
            setCookie('qave_families', JSON.stringify(data.families || []));
            setCookie('qave_difficulty', data.difficulty || '');
          });
          handlersRegistered = true;
        }

        document.addEventListener('shiny:connected', function() {
          registerMessageHandlers();
          sendSavedPreferences();
        });
      })();
      "
    )),
    tags$script(
      async = NA,
      src = "https://www.googletagmanager.com/gtag/js?id=G-JERY6P5EXE"
    ),
    tags$script(HTML(
      "
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
      gtag('config', 'G-JERY6P5EXE');
      "
    )),
    # Custom CSS gives the game a card-based layout and bird-focused styling.
    tags$style(HTML(
      "
      body {
        background:
          radial-gradient(circle at top left, rgba(240,180,41,0.24), transparent 28%),
          linear-gradient(135deg, #f6f1e7 0%, #dcefe6 100%);
        min-height: 100vh;
      }
      .shell {
        max-width: 1080px;
        margin: 32px auto;
      }
      .hero, .panel-card {
        background: rgba(255,255,255,0.88);
        border: 1px solid rgba(31,111,80,0.14);
        border-radius: 24px;
        box-shadow: 0 18px 50px rgba(31,41,51,0.10);
        backdrop-filter: blur(6px);
      }
      .hero {
        padding: 28px;
        margin-bottom: 22px;
      }
      .hero h1 {
        margin-bottom: 8px;
      }
      .hero p {
        font-size: 1.1rem;
        margin-bottom: 0;
      }
      .panel-card {
        padding: 24px;
        margin-bottom: 20px;
      }
      .status-strip {
        display: flex;
        justify-content: space-between;
        gap: 16px;
        flex-wrap: wrap;
        margin-bottom: 18px;
        font-weight: 700;
      }
      .bird-photo {
        width: 100%;
        border-radius: 18px;
        object-fit: contain;
        max-height: 430px;
        border: 3px solid rgba(31,111,80,0.10);
        background: rgba(31,111,80,0.05);
      }
      .feedback {
        display: flex;
        align-items: flex-start;
        gap: 16px;
        padding: 14px 16px;
        border-radius: 14px;
        margin-bottom: 16px;
        font-weight: 600;
      }
      .feedback-media {
        width: 140px;
        flex: 0 0 140px;
      }
      .feedback-photo {
        width: 100%;
        border-radius: 14px;
        object-fit: cover;
        aspect-ratio: 1 / 1;
        border: 2px solid rgba(31,111,80,0.12);
        background: rgba(31,111,80,0.05);
      }
      .feedback-content {
        min-width: 0;
        flex: 1 1 auto;
      }
      .feedback-content p {
        margin: 8px 0 0;
      }
      @media (max-width: 640px) {
        .feedback {
          flex-direction: column;
        }
        .feedback-media {
          width: 100%;
          flex-basis: auto;
        }
        .feedback-photo {
          max-width: 220px;
        }
      }
      .feedback.ok {
        background: rgba(31,111,80,0.12);
        color: #114b37;
      }
      .feedback.nope {
        background: rgba(184, 62, 47, 0.12);
        color: #8a261c;
      }
      .leader-title {
        margin-bottom: 14px;
      }
      .leaderboard-loading {
        display: grid;
        place-items: center;
        gap: 14px;
        min-height: 180px;
        text-align: center;
        color: #114b37;
      }
      .leaderboard-spinner {
        width: 52px;
        height: 52px;
        border-radius: 50%;
        border: 5px solid rgba(31,111,80,0.15);
        border-top-color: #1f6f50;
        animation: leaderboard-spin 0.85s linear infinite;
      }
      @keyframes leaderboard-spin {
        from {
          transform: rotate(0deg);
        }
        to {
          transform: rotate(360deg);
        }
      }
      .score-highlight {
        font-size: 1.25rem;
        font-weight: 700;
        color: #114b37;
      }
      .btn-success {
        background-color: #1f6f50;
        border-color: #1f6f50;
      }
      .btn-warning {
        color: #1f2933;
        font-weight: 700;
      }
      .table {
        margin-bottom: 0;
      }
      .credits {
        margin-top: 14px;
        font-size: 0.95rem;
        color: #52606d;
      }
      .hint-box {
        margin: 12px 0 0;
        padding: 10px 12px;
        border-radius: 12px;
        background: rgba(240,180,41,0.16);
        color: #6b4a00;
        font-weight: 700;
      }
      .seo-copy h2,
      .seo-copy h3 {
        margin-top: 0;
      }
      .seo-copy p:last-child,
      .seo-copy ul:last-child {
        margin-bottom: 0;
      }
      .seo-copy ul {
        padding-left: 1.2rem;
      }
      .about-links {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        margin-top: 10px;
      }
    "
    ))
  ),
  div(
    class = "shell",
    div(
      class = "hero seo-copy",
      h1("Que ave é essa?"),
      p(
        "Tente identificar aves observadas em um ou mais estados do Brasil com a foto e a vocalização."
      ),
      p(
        class = "credits",
        "Créditos: imagens e sons do iNaturalist; descrições e links de referência da Wikipedia quando disponíveis."
      ),
      tags$noscript(
        tags$p(
          class = "credits",
          "Este app precisa de JavaScript para carregar o jogo interativo, mas o conteúdo desta página resume como o quiz funciona e quais dados ele utiliza."
        )
      )
    ),
    uiOutput("app_body"),
    div(
      class = "panel-card seo-copy",
      h3("Sobre"),
      p(
        "Contato: ",
        tags$a(
          href = "mailto:raphael.saldanha@fiocruz.br",
          "raphael.saldanha@fiocruz.br"
        )
      ),
      p("Código e dados do projeto:"),
      div(
        class = "about-links",
        tags$a(
          href = "https://github.com/rfsaldanha/qave",
          target = "_blank",
          rel = "noopener noreferrer",
          "App QAVE"
        ),
        tags$a(
          href = "https://github.com/rfsaldanha/bird_scrapper",
          target = "_blank",
          rel = "noopener noreferrer",
          "Bird Scrapper"
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Keep all mutable game state in one reactive object.
  state <- reactiveValues(
    started = FALSE,
    finished = FALSE,
    viewing_leaderboard = FALSE,
    leaderboard_loading = FALSE,
    username = "",
    score = 0L,
    current_index = 1L,
    questions = NULL,
    available_species = NULL,
    selected_states = all_state_codes,
    selected_families = character(0),
    difficulty = default_difficulty,
    current_choices = NULL,
    current_hint = NULL,
    current_hint_used = FALSE,
    hints_used = 0L,
    feedback = NULL,
    leaderboard = NULL,
    score_saved = FALSE,
    score_save_pending = FALSE
  )

  leaderboard_loading_ui <- function(message = "Carregando ranking...") {
    div(
      class = "leaderboard-loading",
      div(class = "leaderboard-spinner"),
      tags$p(message)
    )
  }

  leaderboard_methodology_ui <- function() {
    tags$p(
      class = "credits",
      sprintf(
        paste(
          "A Pontuação Justa ordena o ranking por consistência, não só por uma",
          "partida perfeita. Cada partida multiplica os acertos pela dificuldade:",
          "Fácil x%.2f, Médio x%.2f e Difícil x%.2f; cada dica usada remove",
          "1 ponto depois desse multiplicador. No fácil, a dica remove duas",
          "alternativas incorretas; no médio, remove uma; no difícil, não há dica.",
          "Depois, a Pontuação Justa combina a média real do jogador com a média",
          "geral do ranking, usando %d partidas de confiança. Quanto mais partidas",
          "a pessoa joga mantendo boa média, menos essa correção pesa. Em caso de",
          "empate, entram média, partidas jogadas, melhor partida, pontuação total",
          "e data mais recente."
        ),
        difficulty_multipliers[["easy"]],
        difficulty_multipliers[["medium"]],
        difficulty_multipliers[["hard"]],
        leaderboard_prior_games
      )
    )
  }

  valid_saved_values <- function(values, valid_values) {
    values <- unlist(values, use.names = FALSE)
    values <- values[!is.na(values) & nzchar(values)]
    intersect(values, valid_values)
  }

  restore_saved_preferences <- function(preferences) {
    if (!is.list(preferences)) {
      return(invisible(NULL))
    }

    saved_username <- trimws(input_value(preferences$username))
    saved_states <- valid_saved_values(preferences$states, all_state_codes)

    if (length(saved_states) == 0) {
      saved_states <- all_state_codes
    }

    available_families <- family_choices_for_states(saved_states)
    saved_families <- valid_saved_values(
      preferences$families,
      available_families
    )

    if (length(saved_families) == 0) {
      saved_families <- available_families
    }

    saved_difficulty <- input_value(preferences$difficulty)

    if (!saved_difficulty %in% unname(difficulty_choices)) {
      saved_difficulty <- default_difficulty
    }

    if (nzchar(saved_username)) {
      updateTextInput(session, "username", value = saved_username)
    }

    updatePickerInput(
      session,
      "state_filter",
      selected = saved_states
    )
    updatePickerInput(
      session,
      "family_filter",
      choices = available_families,
      selected = saved_families
    )
    updateRadioButtons(
      session,
      "difficulty",
      selected = saved_difficulty
    )

    invisible(NULL)
  }

  save_preferences_cookie <- function(
    username,
    selected_states,
    selected_families,
    difficulty
  ) {
    session$sendCustomMessage(
      "qave_save_preferences",
      list(
        username = username,
        states = unname(selected_states),
        families = unname(selected_families),
        difficulty = difficulty
      )
    )
  }

  # Defer leaderboard work until the next loop tick so the UI can paint first.
  load_leaderboard_async <- function(limit = 10L, refresh = FALSE) {
    state$leaderboard_loading <- TRUE
    state$leaderboard <- NULL

    later::later(
      function() {
        shiny::withReactiveDomain(session, {
          on.exit(
            {
              state$leaderboard_loading <- FALSE
            },
            add = TRUE
          )

          leaderboard <- tryCatch(
            read_leaderboard(limit, refresh = refresh),
            error = function(err) {
              showNotification(
                sprintf(
                  "Nao foi possivel carregar o ranking: %s",
                  conditionMessage(err)
                ),
                type = "error"
              )
              NULL
            }
          )

          state$leaderboard <- leaderboard
        })
      },
      delay = 0
    )
  }

  # Render the latest answer feedback with a short description and external link.
  feedback_ui <- function(feedback) {
    if (is.null(feedback)) {
      return(NULL)
    }

    feedback_details <- list(
      tags$strong(feedback$title),
      tags$p(feedback$description)
    )

    if (!isTRUE(is.na(feedback$link))) {
      feedback_details[[length(feedback_details) + 1L]] <- tags$a(
        href = feedback$link,
        target = "_blank",
        rel = "noopener noreferrer",
        "Ver descrição completa na Wikipedia"
      )
    }

    div(
      class = paste("feedback", feedback$class),
      div(
        class = "feedback-media",
        tags$img(
          class = "feedback-photo",
          src = feedback$photo,
          alt = feedback$title
        )
      ),
      div(
        class = "feedback-content",
        feedback_details
      )
    )
  }

  # Return the species row for the current round.
  current_species <- reactive({
    req(state$started, !state$finished, !is.null(state$questions))
    state$questions[state$current_index, , drop = FALSE]
  })

  # Restrict the game pool to the states and families chosen on the start screen.
  species_pool <- function(selected_states, selected_families) {
    selected_states <- selected_states[!is.na(selected_states)]
    selected_families <- selected_families[!is.na(selected_families)]

    if (length(selected_states) == 0 || length(selected_families) == 0) {
      return(species_data[0, , drop = FALSE])
    }

    species_data |>
      dplyr::filter(
        state_code %in% selected_states,
        family %in% selected_families
      ) |>
      dplyr::distinct(common_name, scientific_name, .keep_all = TRUE)
  }

  # Family choices depend on the states currently selected by the user.
  family_choices_for_states <- function(selected_states) {
    selected_states <- selected_states[!is.na(selected_states)]

    if (length(selected_states) == 0) {
      return(character(0))
    }

    species_data |>
      dplyr::filter(state_code %in% selected_states) |>
      dplyr::distinct(family) |>
      dplyr::arrange(family) |>
      dplyr::pull(family)
  }

  sample_distractors <- function(pool, count) {
    if (count <= 0L || nrow(pool) == 0L) {
      return(pool[0, , drop = FALSE])
    }

    dplyr::slice_sample(pool, n = min(count, nrow(pool)))
  }

  # Build four answer choices. Harder modes choose more similar distractors.
  build_round_choices <- function(
    species_row,
    available_species,
    difficulty = default_difficulty
  ) {
    base_pool <- available_species |>
      dplyr::filter(scientific_name != species_row$scientific_name[[1]])

    if (nrow(base_pool) < 3L) {
      stop("Not enough distractor species available to build the round.")
    }

    if (identical(difficulty, "hard")) {
      target_name <- paste(
        species_row$common_name_norm[[1]],
        species_row$scientific_name_norm[[1]]
      )
      candidate_names <- paste(
        base_pool$common_name_norm,
        base_pool$scientific_name_norm
      )
      distractors <- base_pool |>
        dplyr::mutate(name_distance = as.numeric(utils::adist(
          target_name,
          candidate_names
        )[1, ])) |>
        dplyr::arrange(
          dplyr::desc(family == species_row$family[[1]]),
          name_distance
        ) |>
        dplyr::slice_head(n = min(8L, nrow(base_pool))) |>
        sample_distractors(3L)
    } else if (identical(difficulty, "medium")) {
      same_family <- base_pool |>
        dplyr::filter(family == species_row$family[[1]])
      family_distractors <- sample_distractors(same_family, 3L)
      remaining_needed <- 3L - nrow(family_distractors)
      remaining_pool <- base_pool |>
        dplyr::filter(!scientific_name %in% family_distractors$scientific_name)
      distractors <- dplyr::bind_rows(
        family_distractors,
        sample_distractors(remaining_pool, remaining_needed)
      )
    } else {
      distractors <- sample_distractors(base_pool, 3L)
    }

    distractors <- distractors |>
      dplyr::select(
        common_name,
        scientific_name,
        common_name_norm,
        scientific_name_norm
      )

    dplyr::bind_rows(
      dplyr::select(
        species_row,
        common_name,
        scientific_name,
        common_name_norm,
        scientific_name_norm
      ),
      distractors
    ) |>
      dplyr::slice_sample(n = 4L) |>
      dplyr::mutate(choice_id = sprintf("choice_%d", dplyr::row_number()))
  }

  # Build radio button labels once so update and initial render share the same markup.
  build_choice_names <- function(choices) {
    unname(lapply(seq_len(nrow(choices)), function(i) {
      tags$span(
        choices$common_name[[i]],
        " ",
        tags$em(sprintf("(%s)", choices$scientific_name[[i]]))
      )
    }))
  }

  # Treat missing Shiny inputs as empty strings to simplify validation.
  input_value <- function(value) {
    if (is.null(value)) {
      ""
    } else {
      value
    }
  }

  difficulty_label <- function(difficulty = state$difficulty) {
    label <- names(difficulty_choices)[match(difficulty, difficulty_choices)]

    if (is.na(label)) {
      names(difficulty_choices)[match(default_difficulty, difficulty_choices)]
    } else {
      label
    }
  }

  difficulty_multiplier <- function(difficulty = state$difficulty) {
    multiplier <- difficulty_multipliers[[difficulty]]

    if (is.null(multiplier) || is.na(multiplier)) {
      difficulty_multipliers[[default_difficulty]]
    } else {
      multiplier
    }
  }

  final_score <- function(
    score = state$score,
    hints_used = state$hints_used,
    difficulty = state$difficulty
  ) {
    multiplied_score <- floor(as.integer(score) * difficulty_multiplier(difficulty))
    max(as.integer(multiplied_score) - as.integer(hints_used), 0L)
  }

  observeEvent(
    input$qave_saved_preferences,
    {
      session$onFlushed(
        function() {
          restore_saved_preferences(input$qave_saved_preferences)
        },
        once = TRUE
      )
    },
    ignoreInit = TRUE
  )

  # Start or restart a game by sampling the round species and resetting state.
  start_game <- function(
    username,
    selected_states,
    selected_families,
    difficulty = default_difficulty
  ) {
    available_species <- species_pool(selected_states, selected_families)
    difficulty <- if (difficulty %in% unname(difficulty_choices)) {
      difficulty
    } else {
      default_difficulty
    }

    if (length(selected_states) == 0) {
      showNotification(
        "Selecione pelo menos um estado antes de começar.",
        type = "error"
      )
      return(invisible(FALSE))
    }

    if (length(selected_families) == 0) {
      showNotification(
        "Selecione pelo menos uma familia antes de começar.",
        type = "error"
      )
      return(invisible(FALSE))
    }

    if (nrow(available_species) < round_count) {
      showNotification(
        sprintf(
          "As familias selecionadas têm apenas %d aves com foto disponível. Escolha pelo menos %d.",
          nrow(available_species),
          round_count
        ),
        type = "error"
      )
      return(invisible(FALSE))
    }

    questions <- dplyr::slice_sample(available_species, n = round_count) |>
      dplyr::mutate(
        # Resample the displayed image so a species can appear with a different photo in a new game.
        round_photo = vapply(photo_urls, sample_species_photo, character(1))
      )

    state$started <- TRUE
    state$finished <- FALSE
    state$viewing_leaderboard <- FALSE
    state$username <- username
    state$score <- 0L
    state$current_index <- 1L
    state$questions <- questions
    state$available_species <- available_species
    state$selected_states <- selected_states
    state$selected_families <- selected_families
    state$difficulty <- difficulty
    state$current_choices <- build_round_choices(
      questions[1, , drop = FALSE],
      available_species,
      difficulty
    )
    state$current_hint <- NULL
    state$current_hint_used <- FALSE
    state$hints_used <- 0L
    state$feedback <- NULL
    state$leaderboard <- NULL
    state$score_saved <- FALSE
    state$score_save_pending <- FALSE
    invisible(TRUE)
  }

  # Show the finished state immediately, then persist the score once per game.
  finish_game <- function() {
    final_username <- isolate(state$username)
    saved_score <- isolate(final_score())

    state$finished <- TRUE

    if (state$score_saved || state$score_save_pending) {
      return(invisible(NULL))
    }

    state$score_save_pending <- TRUE
    state$leaderboard_loading <- TRUE
    state$leaderboard <- NULL

    session$onFlushed(
      function() {
        later::later(
          function() {
            shiny::withReactiveDomain(session, {
              on.exit(
                {
                  state$score_save_pending <- FALSE
                },
                add = TRUE
              )

              tryCatch(
                {
                  record_score(final_username, saved_score, round_count)
                  state$score_saved <- TRUE
                  load_leaderboard_async(10L, refresh = TRUE)
                },
                error = function(err) {
                  state$leaderboard_loading <- FALSE
                  showNotification(
                    sprintf(
                      "Nao foi possivel salvar a pontuacao: %s",
                      conditionMessage(err)
                    ),
                    type = "error"
                  )
                }
              )
            })
          },
          delay = 0
        )
      },
      once = TRUE
    )

    invisible(NULL)
  }

  # Start the game only after the player provides a username.
  observeEvent(input$start_game, {
    username <- trimws(input_value(input$username))
    selected_states <- if (is.null(input$state_filter)) {
      character(0)
    } else {
      input$state_filter
    }
    selected_families <- if (is.null(input$family_filter)) {
      character(0)
    } else {
      input$family_filter
    }
    difficulty <- input_value(input$difficulty)

    if (!nzchar(username)) {
      showNotification(
        "Informe um nome de usuário antes de começar.",
        type = "error"
      )
      return()
    }

    game_started <- start_game(
      username,
      selected_states,
      selected_families,
      difficulty
    )

    if (isTRUE(game_started)) {
      save_preferences_cookie(
        username,
        selected_states,
        selected_families,
        difficulty
      )
    }
  })

  # Keep the family picker aligned with the states chosen on the start screen.
  observeEvent(
    input$state_filter,
    {
      selected_states <- if (is.null(input$state_filter)) {
        character(0)
      } else {
        input$state_filter
      }
      available_families <- family_choices_for_states(selected_states)
      current_selection <- if (is.null(input$family_filter)) {
        character(0)
      } else {
        input$family_filter
      }
      retained_families <- intersect(current_selection, available_families)

      if (length(selected_states) > 0 && length(retained_families) == 0) {
        retained_families <- available_families
      }

      updatePickerInput(
        session,
        "family_filter",
        choices = available_families,
        selected = retained_families
      )
    },
    ignoreInit = TRUE
  )

  # Open the leaderboard view from the start screen.
  observeEvent(input$show_leaderboard, {
    state$viewing_leaderboard <- TRUE
    load_leaderboard_async(10L)
  })

  # Return from the leaderboard view to the initial screen.
  observeEvent(input$back_to_start, {
    state$viewing_leaderboard <- FALSE
  })

  observeEvent(input$show_hint, {
    req(state$started, !state$finished)

    if (identical(state$difficulty, "hard")) {
      showNotification(
        "No nível difícil, não há dica disponível.",
        type = "message"
      )
      return()
    }

    if (isTRUE(state$current_hint_used)) {
      return()
    }

    species <- current_species()
    wrong_choices <- state$current_choices |>
      dplyr::filter(scientific_name != species$scientific_name[[1]])
    correct_choice <- state$current_choices |>
      dplyr::filter(scientific_name == species$scientific_name[[1]])
    remove_count <- if (identical(state$difficulty, "easy")) {
      2L
    } else {
      1L
    }
    removed_choices <- sample_distractors(wrong_choices, remove_count)
    retained_wrong_choices <- wrong_choices |>
      dplyr::filter(!choice_id %in% removed_choices$choice_id)

    state$current_choices <- dplyr::bind_rows(
      correct_choice,
      retained_wrong_choices
    ) |>
      dplyr::slice_sample(prop = 1)
    state$hints_used <- state$hints_used + 1L
    state$current_hint_used <- TRUE

    state$current_hint <- sprintf(
      "Dica: %d alternativa(s) incorreta(s) foram removidas. Usar dica reduz 1 ponto da pontuação final.",
      nrow(removed_choices)
    )
  })

  # Evaluate the selected answer, show the reveal text, and advance the game.
  observeEvent(input$submit_guess, {
    req(
      state$started,
      !state$finished,
      !is.null(state$current_choices),
      nrow(state$current_choices) >= 2L
    )

    guess <- input_value(input$guess)

    if (!nzchar(guess)) {
      showNotification(
        "Selecione uma das 4 opções antes de enviar sua resposta.",
        type = "warning"
      )
      return()
    }

    species <- isolate(current_species())
    round_choices <- isolate(state$current_choices)
    selected_choice <- round_choices |>
      dplyr::filter(choice_id == guess)

    if (nrow(selected_choice) != 1L) {
      showNotification(
        "Selecione uma das opções visíveis antes de enviar sua resposta.",
        type = "warning"
      )
      return()
    }

    matched <- identical(
      selected_choice$scientific_name_norm[[1]],
      species$scientific_name_norm[[1]]
    )

    if (matched) {
      state$score <- state$score + 1L
      state$feedback <- list(
        class = "ok",
        title = sprintf(
          "Acertou! Era %s (%s).",
          species$common_name[[1]],
          species$scientific_name[[1]]
        ),
        # Show the short description in-app and keep the full description page one click away.
        description = species$brief_description[[1]],
        link = species$wiki_url[[1]],
        photo = species$round_photo[[1]]
      )
    } else {
      state$feedback <- list(
        class = "nope",
        title = sprintf(
          "Dessa vez não. A resposta era %s (%s).",
          species$common_name[[1]],
          species$scientific_name[[1]]
        ),
        description = species$brief_description[[1]],
        link = species$wiki_url[[1]],
        photo = species$round_photo[[1]]
      )
    }

    if (state$current_index >= round_count) {
      finish_game()
    } else {
      state$current_index <- state$current_index + 1L
      state$current_choices <- build_round_choices(
        current_species(),
        state$available_species,
        state$difficulty
      )
      state$current_hint <- NULL
      state$current_hint_used <- FALSE
    }
  })

  # Replay the game for the same user after a finished round set.
  observeEvent(input$play_again, {
    start_game(
      state$username,
      state$selected_states,
      state$selected_families,
      state$difficulty
    )
  })

  # Swap between the start screen, the active round UI, and the final score view.
  output$app_body <- renderUI({
    if (!state$started) {
      if (state$viewing_leaderboard) {
        return(
          div(
            class = "panel-card",
            h3("Top 10 pontuações"),
            if (state$leaderboard_loading) {
              leaderboard_loading_ui()
            } else {
              tableOutput("leaderboard_table")
            },
            leaderboard_methodology_ui(),
            div(style = "margin-top: 18px;"),
            actionButton("back_to_start", "Voltar", class = "btn-warning")
          )
        )
      }

      return(
        div(
          class = "panel-card",
          h3("Comece uma nova partida"),
          p(
            "Escolha um nome de usuário, um ou mais estados e tente identificar 10 aves sorteadas aleatoriamente."
          ),
          textInput("username", "Nome de usuário"),
          shinyWidgets::pickerInput(
            "state_filter",
            "Estados",
            choices = state_choices,
            selected = all_state_codes,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              `actions-box` = TRUE,
              `count-selected-text` = "{0} estados selecionados",
              `deselect-all-text` = "Limpar",
              `live-search` = TRUE,
              `live-search-placeholder` = "Buscar estado",
              `none-selected-text` = "Selecione um ou mais estados",
              `none-results-text` = "Nenhum estado encontrado para {0}",
              `select-all-text` = "Selecionar todos",
              `selected-text-format` = "count > 4",
              size = 10
            )
          ),
          shinyWidgets::pickerInput(
            "family_filter",
            "Famílias",
            choices = family_choices_for_states(all_state_codes),
            selected = family_choices_for_states(all_state_codes),
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              `actions-box` = TRUE,
              `count-selected-text` = "{0} famílias selecionadas",
              `deselect-all-text` = "Limpar",
              `live-search` = TRUE,
              `live-search-placeholder` = "Buscar família",
              `none-selected-text` = "Selecione uma ou mais famílias",
              `none-results-text` = "Nenhuma família encontrada para {0}",
              `select-all-text` = "Selecionar todas",
              `selected-text-format` = "count > 4",
              size = 10
            )
          ),
          radioButtons(
            "difficulty",
            "Dificuldade",
            choices = difficulty_choices,
            selected = default_difficulty,
            inline = TRUE
          ),
          p(
            class = "credits",
            sprintf(
              paste(
                "%d estados e todas as famílias disponíveis para eles já vêm",
                "selecionados por padrão. Multiplicadores: Fácil x%.2f, Médio x%.2f",
                "e Difícil x%.2f. No nível difícil, as alternativas tendem a ser",
                "da mesma família e com nomes mais parecidos. Dicas removem duas",
                "alternativas no fácil, uma no médio, e não aparecem no difícil."
              ),
              length(all_state_codes),
              difficulty_multipliers[["easy"]],
              difficulty_multipliers[["medium"]],
              difficulty_multipliers[["hard"]]
            )
          ),
          actionButton("start_game", "Começar a jogar", class = "btn-success"),
          div(style = "margin-top: 12px;"),
          actionButton("show_leaderboard", "Ver ranking", class = "btn-warning")
        )
      )
    }

    if (!state$finished) {
      species <- current_species()
      progress_label <- sprintf(
        "Rodada %d de %d",
        state$current_index,
        round_count
      )
      score_label <- sprintf(
        "Acertos: %d | Dicas: %d | %s x%.2f | Pontuação atual: %d",
        state$score,
        state$hints_used,
        difficulty_label(),
        difficulty_multiplier(),
        final_score()
      )

      return(
        tagList(
          div(
            class = "panel-card",
            # Show status, the selected media, and the answer options for this round.
            div(
              class = "status-strip",
              span(progress_label),
              span(sprintf("Jogador: %s", state$username)),
              span(score_label)
            ),
            feedback_ui(state$feedback),
            tags$img(
              class = "bird-photo",
              src = species$round_photo[[1]],
              alt = sprintf(
                "Foto para identificar a ave da rodada %d do quiz",
                state$current_index
              )
            ),
            tags$div(
              style = "margin: 18px 0;",
              if (!isTRUE(is.na(species$audio_url[[1]]))) {
                tags$audio(
                  src = species$audio_url[[1]],
                  controls = NA,
                  preload = "none",
                  style = "width: 100%;"
                )
              } else {
                tags$p(
                  class = "credits",
                  "Esta ave não tem gravação de som disponível nesta rodada."
                )
              }
            ),
            radioButtons(
              "guess",
              "Escolha uma opção",
              choiceNames = build_choice_names(state$current_choices),
              choiceValues = unname(as.character(
                state$current_choices$choice_id
              ))
            ),
            if (!identical(state$difficulty, "hard")) {
              actionButton(
                "show_hint",
                if (identical(state$difficulty, "easy")) {
                  "Dica: remover 2 (-1 ponto)"
                } else {
                  "Dica: remover 1 (-1 ponto)"
                },
                class = "btn-success"
              )
            },
            if (!is.null(state$current_hint)) {
              div(class = "hint-box", state$current_hint)
            },
            div(style = "margin-top: 12px;"),
            actionButton(
              "submit_guess",
              "Enviar resposta",
              class = "btn-warning"
            )
          )
        )
      )
    }

    div(
      class = "panel-card",
      h3("Partida concluída"),
      p(
        class = "score-highlight",
        sprintf(
          "%s acertou %d de %d, usou %d dica(s), jogou no nível %s (x%.2f) e ficou com %d ponto(s).",
          state$username,
          state$score,
          round_count,
          state$hints_used,
          difficulty_label(),
          difficulty_multiplier(),
          final_score()
        )
      ),
      feedback_ui(state$feedback),
      h4(class = "leader-title", "Top 10 pontuações"),
      if (state$leaderboard_loading) {
        leaderboard_loading_ui("Atualizando ranking...")
      } else {
        tableOutput("leaderboard_table")
      },
      leaderboard_methodology_ui(),
      div(style = "margin-top: 18px;"),
      actionButton("play_again", "Jogar novamente", class = "btn-success")
    )
  })

  # Render the leaderboard table with localized column labels.
  output$leaderboard_table <- renderTable(
    {
      req(
        (state$finished || state$viewing_leaderboard),
        !is.null(state$leaderboard)
      )

      leaderboard <- state$leaderboard
      leaderboard$rank <- seq_len(nrow(leaderboard))
      leaderboard <- leaderboard |>
        dplyr::select(
          rank,
          username,
          fair_score,
          best_score,
          average_score,
          total_score,
          games_played,
          last_score,
          last_played_at
        )
      leaderboard$average_score <- sprintf("%.2f", leaderboard$average_score)
      leaderboard$fair_score <- sprintf("%.2f", leaderboard$fair_score)
      leaderboard$last_played_at <- if (
        inherits(leaderboard$last_played_at, "POSIXt")
      ) {
        format(
          leaderboard$last_played_at,
          "%d/%m/%Y %H:%M:%S",
          tz = "America/Sao_Paulo"
        )
      } else {
        format(
          as.POSIXct(
            as.numeric(leaderboard$last_played_at),
            origin = "1970-01-01",
            tz = "UTC"
          ),
          "%d/%m/%Y %H:%M:%S",
          tz = "America/Sao_Paulo"
        )
      }
      names(leaderboard) <- c(
        "Posição",
        "Usuário",
        "Pontuação Justa",
        "Melhor Partida",
        "Média",
        "Pontuação Total",
        "Partidas Jogadas",
        "Última Pontuação",
        "Última Partida"
      )
      leaderboard
    },
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    spacing = "m"
  )
}

# Launch the Shiny application.
shinyApp(ui, server)
