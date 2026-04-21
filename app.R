library(shiny)
library(bslib)
library(shinyWidgets)
library(arrow)
library(dplyr)
library(DBI)
library(RSQLite)
library(stringi)

# Game setup shared across the app.
round_count <- 10L
db_path <- "leaderboard.sqlite"

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

# Open a short-lived SQLite connection for one operation.
with_db_connection <- function(path = db_path, fn) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  fn(conn)
}

# Load the species dataset and normalize source-specific column names for the app.
load_species_data <- function(path = "inaturalist_birds_rj.parquet") {
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
      !is.na(audio_url),
      nzchar(common_name),
      nzchar(scientific_name),
      nzchar(audio_url),
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
        urls[!is.na(urls) & nzchar(urls)]
      }),
      round_photo = vapply(photo_urls, sample_species_photo, character(1)),
      brief_description = value_or_default(
        brief_description,
        "Sem descrição breve disponível para esta espécie."
      ),
      wiki_url = ifelse(
        !is.na(wiki_url) & nzchar(trimws(wiki_url)),
        trimws(wiki_url),
        NA_character_
      ),
      common_name_norm = vapply(common_name, normalize_answer, character(1)),
      scientific_name_norm = vapply(
        scientific_name,
        normalize_answer,
        character(1)
      )
    ) |>
    dplyr::filter(!is.na(round_photo), nzchar(round_photo)) |>
    dplyr::distinct(common_name, scientific_name, .keep_all = TRUE)

  if (nrow(species) < round_count) {
    stop("Not enough species with both image and audio to start the game.")
  }

  species
}

# Create the local SQLite tables used to store scores and leaderboard totals.
init_db <- function(path = db_path) {
  with_db_connection(path, function(conn) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS game_results (",
        "id INTEGER PRIMARY KEY AUTOINCREMENT,",
        "username TEXT NOT NULL,",
        "score INTEGER NOT NULL,",
        "rounds INTEGER NOT NULL,",
        "played_at TEXT NOT NULL",
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
        "last_played_at TEXT NOT NULL",
        ")"
      )
    )
  })
}

# Save one finished game and update the aggregate leaderboard for the same user.
record_score <- function(
  username,
  score,
  rounds = round_count,
  path = db_path
) {
  played_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  with_db_connection(path, function(conn) {
    DBI::dbExecute(
      conn,
      "INSERT INTO game_results (username, score, rounds, played_at) VALUES (?, ?, ?, ?)",
      params = list(username, score, rounds, played_at)
    )

    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO leaderboard (username, total_score, games_played, best_score, last_score, last_played_at)",
        "VALUES (?, ?, 1, ?, ?, ?)",
        "ON CONFLICT(username) DO UPDATE SET",
        "total_score = leaderboard.total_score + excluded.total_score,",
        "games_played = leaderboard.games_played + 1,",
        "best_score = MAX(leaderboard.best_score, excluded.best_score),",
        "last_score = excluded.last_score,",
        "last_played_at = excluded.last_played_at"
      ),
      params = list(username, score, score, score, played_at)
    )
  })
}

# Read the top scores shown on the start screen and after a game ends.
read_leaderboard <- function(limit = 10L, path = db_path) {
  with_db_connection(path, function(conn) {
    DBI::dbGetQuery(
      conn,
      paste(
        "SELECT username, total_score, games_played, best_score, last_score, last_played_at",
        "FROM leaderboard",
        "ORDER BY total_score DESC, best_score DESC, last_played_at DESC",
        "LIMIT ?"
      ),
      params = list(limit)
    )
  })
}

# Load game data and ensure the score database exists before the app starts.
species_data <- load_species_data()
family_choices <- sort(unique(species_data$family))
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
  theme = app_theme,
  tags$head(
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
    "
    ))
  ),
  div(
    class = "shell",
    div(
      class = "hero",
      h1("Que ave é essa?"),
      p(
        "Tente identificar aves observadas no estado do Rio de Janeiro usando a foto e a vocalização."
      ),
      p(
        class = "credits",
        "Créditos: imagens e sons do iNaturalist; descrições e links de referência da Wikipedia quando disponíveis."
      )
    ),
    uiOutput("app_body")
  )
)

server <- function(input, output, session) {
  # Keep all mutable game state in one reactive object.
  state <- reactiveValues(
    started = FALSE,
    finished = FALSE,
    viewing_leaderboard = FALSE,
    username = "",
    score = 0L,
    current_index = 1L,
    questions = NULL,
    available_species = NULL,
    selected_families = family_choices,
    current_choices = NULL,
    feedback = NULL,
    leaderboard = NULL,
    score_saved = FALSE
  )

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

  # Restrict the game pool to the families chosen on the start screen.
  species_pool <- function(selected_families) {
    selected_families <- selected_families[!is.na(selected_families)]

    if (length(selected_families) == 0) {
      return(species_data[0, , drop = FALSE])
    }

    species_data |>
      dplyr::filter(family %in% selected_families)
  }

  # Build four answer choices: the correct species plus three random distractors.
  build_round_choices <- function(species_row, available_species) {
    distractor_pool <- available_species |>
      dplyr::filter(scientific_name != species_row$scientific_name[[1]])

    if (nrow(distractor_pool) < 3L) {
      stop("Not enough distractor species available to build the round.")
    }

    distractors <- distractor_pool |>
      dplyr::slice_sample(n = 3L) |>
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

  # Refresh the radio button choices and clear any previous selection.
  reset_round_input <- function(choices = character(0)) {
    updateRadioButtons(
      session,
      "guess",
      choiceNames = build_choice_names(choices),
      choiceValues = unname(as.character(choices$choice_id)),
      selected = character(0)
    )
  }

  # Treat missing Shiny inputs as empty strings to simplify validation.
  input_value <- function(value) {
    if (is.null(value)) {
      ""
    } else {
      value
    }
  }

  # Start or restart a game by sampling the round species and resetting state.
  start_game <- function(username, selected_families) {
    available_species <- species_pool(selected_families)

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
          "As familias selecionadas têm apenas %d aves com foto e som. Escolha pelo menos %d.",
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
    state$selected_families <- selected_families
    state$current_choices <- build_round_choices(
      questions[1, , drop = FALSE],
      available_species
    )
    state$feedback <- NULL
    state$leaderboard <- NULL
    state$score_saved <- FALSE
    reset_round_input(state$current_choices)
    invisible(TRUE)
  }

  # Persist the final score once and mark the session as finished.
  finish_game <- function() {
    if (!state$score_saved) {
      record_score(state$username, state$score, round_count)
      state$leaderboard <- read_leaderboard(10L)
      state$score_saved <- TRUE
    }

    state$finished <- TRUE
  }

  # Start the game only after the player provides a username.
  observeEvent(input$start_game, {
    username <- trimws(input_value(input$username))
    selected_families <- if (is.null(input$family_filter)) {
      character(0)
    } else {
      input$family_filter
    }

    if (!nzchar(username)) {
      showNotification(
        "Informe um nome de usuário antes de começar.",
        type = "error"
      )
      return()
    }

    start_game(username, selected_families)
  })

  # Open the leaderboard view from the start screen.
  observeEvent(input$show_leaderboard, {
    state$viewing_leaderboard <- TRUE
    state$leaderboard <- read_leaderboard(10L)
  })

  # Return from the leaderboard view to the initial screen.
  observeEvent(input$back_to_start, {
    state$viewing_leaderboard <- FALSE
  })

  # Evaluate the selected answer, show the reveal text, and advance the game.
  observeEvent(input$submit_guess, {
    guess <- input_value(input$guess)

    if (!nzchar(guess)) {
      showNotification(
        "Selecione uma das 4 opções antes de enviar sua resposta.",
        type = "warning"
      )
      return()
    }

    species <- current_species()
    selected_choice <- state$current_choices |>
      dplyr::filter(choice_id == guess)
    matched <- nrow(selected_choice) == 1L &&
      identical(
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
        state$available_species
      )
      reset_round_input(state$current_choices)
    }
  })

  # Replay the game for the same user after a finished round set.
  observeEvent(input$play_again, {
    start_game(state$username, state$selected_families)
  })

  # Swap between the start screen, the active round UI, and the final score view.
  output$app_body <- renderUI({
    if (!state$started) {
      if (state$viewing_leaderboard) {
        return(
          div(
            class = "panel-card",
            h3("Top 10 pontuações"),
            tableOutput("leaderboard_table"),
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
            "Escolha um nome de usuário e tente identificar 10 aves sorteadas aleatoriamente."
          ),
          textInput("username", "Nome de usuário"),
          shinyWidgets::pickerInput(
            "family_filter",
            "Famílias",
            choices = family_choices,
            selected = family_choices,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `live-search` = TRUE,
              `live-search-placeholder` = "Buscar família",
              `none-selected-text` = "Selecione uma ou mais famílias",
              `select-all-text` = "Selecionar todas",
              `selected-text-format` = "count > 4",
              size = 10
            )
          ),
          p(
            class = "credits",
            sprintf(
              "Todas as %d famílias disponíveis já vêm selecionadas por padrão.",
              length(family_choices)
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
      score_label <- sprintf("Pontuação: %d", state$score)

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
              alt = species$common_name[[1]]
            ),
            tags$div(
              style = "margin: 18px 0;",
              tags$audio(
                src = species$audio_url[[1]],
                controls = NA,
                preload = "none",
                style = "width: 100%;"
              )
            ),
            radioButtons(
              "guess",
              "Escolha uma opção",
              choiceNames = build_choice_names(state$current_choices),
              choiceValues = unname(as.character(state$current_choices$choice_id))
            ),
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
          "%s fez %d de %d pontos.",
          state$username,
          state$score,
          round_count
        )
      ),
      feedback_ui(state$feedback),
      h4(class = "leader-title", "Top 10 pontuações"),
      tableOutput("leaderboard_table"),
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
      names(leaderboard) <- c(
        "Usuário",
        "Pontuação Total",
        "Partidas Jogadas",
        "Melhor Partida",
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
