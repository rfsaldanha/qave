library(shiny)
library(bslib)
library(arrow)
library(dplyr)
library(DBI)
library(RSQLite)
library(stringi)

round_count <- 10L
db_path <- "leaderboard.sqlite"

normalize_answer <- function(value) {
  normalized <- trimws(value)
  normalized <- stringi::stri_trans_general(normalized, "Latin-ASCII")
  normalized <- tolower(normalized)
  normalized <- gsub("[^a-z0-9]+", " ", normalized)
  normalized <- gsub("\\s+", " ", normalized)
  trimws(normalized)
}

sample_species_photo <- function(photo_urls) {
  valid_photos <- photo_urls[!is.na(photo_urls) & nzchar(photo_urls)]

  if (length(valid_photos) == 0) {
    return(NA_character_)
  }

  sample(valid_photos, size = 1)
}

load_species_data <- function(path = "wikiaves_species_rj.parquet") {
  species <- arrow::read_parquet(path) |>
    as.data.frame() |>
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
      photo_urls = lapply(photo_urls, function(urls) urls[!is.na(urls) & nzchar(urls)]),
      round_photo = vapply(photo_urls, sample_species_photo, character(1)),
      common_name_norm = vapply(common_name, normalize_answer, character(1)),
      scientific_name_norm = vapply(scientific_name, normalize_answer, character(1))
    ) |>
    dplyr::filter(!is.na(round_photo), nzchar(round_photo)) |>
    dplyr::distinct(common_name, scientific_name, .keep_all = TRUE)

  if (nrow(species) < round_count) {
    stop("Not enough species with both image and audio to start the game.")
  }

  species
}

init_db <- function(path = db_path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

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
}

record_score <- function(username, score, rounds = round_count, path = db_path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  played_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

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
}

read_leaderboard <- function(limit = 10L, path = db_path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

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
}

species_data <- load_species_data()
init_db()

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

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$style(HTML("
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
        padding: 14px 16px;
        border-radius: 14px;
        margin-bottom: 16px;
        font-weight: 600;
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
    "))
  ),
  div(
    class = "shell",
    div(
      class = "hero",
      h1("Que ave é essa?"),
      p("Tente identificar cada espécie usando a foto e a vocalização. Vale tanto o nome popular quanto o nome científico."),
      p(class = "credits", "Créditos de nomes, imagens e sons: WikiAves.")
    ),
    uiOutput("app_body")
  )
)

server <- function(input, output, session) {
  state <- reactiveValues(
    started = FALSE,
    finished = FALSE,
    viewing_leaderboard = FALSE,
    username = "",
    score = 0L,
    current_index = 1L,
    questions = NULL,
    current_choices = NULL,
    feedback = NULL,
    leaderboard = NULL,
    score_saved = FALSE
  )

  feedback_ui <- function(feedback) {
    if (is.null(feedback)) {
      return(NULL)
    }

    div(
      class = paste("feedback", feedback$class),
      tags$strong(feedback$title),
      tags$p(style = "margin: 8px 0 0;", feedback$description),
      tags$a(
        href = feedback$link,
        target = "_blank",
        rel = "noopener noreferrer",
        "Ver descrição completa no WikiAves"
      )
    )
  }

  current_species <- reactive({
    req(state$started, !state$finished, !is.null(state$questions))
    state$questions[state$current_index, , drop = FALSE]
  })

  build_round_choices <- function(species_row) {
    correct_option <- species_row$common_name[[1]]
    distractors <- species_data |>
      dplyr::filter(common_name != correct_option) |>
      dplyr::slice_sample(n = 3L) |>
      dplyr::select(common_name, scientific_name, common_name_norm)

    dplyr::bind_rows(
      dplyr::select(species_row, common_name, scientific_name, common_name_norm),
      distractors
    ) |>
      dplyr::slice_sample(n = 4L)
  }

  reset_round_input <- function(choices = character(0)) {
    choice_names <- unname(lapply(seq_len(nrow(choices)), function(i) {
      tags$span(
        choices$common_name[[i]],
        " ",
        tags$em(sprintf("(%s)", choices$scientific_name[[i]]))
      )
    }))
    choice_values <- unname(as.character(choices$common_name_norm))

    updateRadioButtons(
      session,
      "guess",
      choiceNames = choice_names,
      choiceValues = choice_values,
      selected = character(0)
    )
  }

  input_value <- function(value) {
    if (is.null(value)) {
      ""
    } else {
      value
    }
  }

  start_game <- function(username) {
    questions <- dplyr::slice_sample(species_data, n = round_count) |>
      dplyr::mutate(
        round_photo = vapply(photo_urls, sample_species_photo, character(1))
      )

    state$started <- TRUE
    state$finished <- FALSE
    state$viewing_leaderboard <- FALSE
    state$username <- username
    state$score <- 0L
    state$current_index <- 1L
    state$questions <- questions
    state$current_choices <- build_round_choices(questions[1, , drop = FALSE])
    state$feedback <- NULL
    state$leaderboard <- NULL
    state$score_saved <- FALSE
    reset_round_input(state$current_choices)
  }

  finish_game <- function() {
    if (!state$score_saved) {
      record_score(state$username, state$score, round_count)
      state$leaderboard <- read_leaderboard(10L)
      state$score_saved <- TRUE
    }

    state$finished <- TRUE
  }

  observeEvent(input$start_game, {
    username <- trimws(input_value(input$username))

    if (!nzchar(username)) {
      showNotification("Informe um nome de usuário antes de começar.", type = "error")
      return()
    }

    start_game(username)
  })

  observeEvent(input$show_leaderboard, {
    state$viewing_leaderboard <- TRUE
    state$leaderboard <- read_leaderboard(10L)
  })

  observeEvent(input$back_to_start, {
    state$viewing_leaderboard <- FALSE
  })

  observeEvent(input$submit_guess, {
    guess <- normalize_answer(input_value(input$guess))

    if (!nzchar(guess)) {
      showNotification("Selecione uma das 4 opções antes de enviar sua resposta.", type = "warning")
      return()
    }

    species <- current_species()
    accepted <- c(species$common_name_norm[[1]], species$scientific_name_norm[[1]])
    matched <- guess %in% accepted

    if (matched) {
      state$score <- state$score + 1L
      state$feedback <- list(
        class = "ok",
        title = sprintf("Acertou! Era %s (%s).", species$common_name[[1]], species$scientific_name[[1]]),
        description = species$brief_description[[1]],
        link = species$wiki_url[[1]]
      )
    } else {
      state$feedback <- list(
        class = "nope",
        title = sprintf("Dessa vez não. A resposta era %s (%s).", species$common_name[[1]], species$scientific_name[[1]]),
        description = species$brief_description[[1]],
        link = species$wiki_url[[1]]
      )
    }

    if (state$current_index >= round_count) {
      finish_game()
    } else {
      state$current_index <- state$current_index + 1L
      state$current_choices <- build_round_choices(current_species())
      reset_round_input(state$current_choices)
    }
  })

  observeEvent(input$play_again, {
    start_game(state$username)
  })

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
          p("Escolha um nome de usuário e tente identificar 10 aves sorteadas aleatoriamente."),
          textInput("username", "Nome de usuário"),
          actionButton("start_game", "Começar a jogar", class = "btn-success"),
          div(style = "margin-top: 12px;"),
          actionButton("show_leaderboard", "Ver ranking", class = "btn-warning")
        )
      )
    }

    if (!state$finished) {
      species <- current_species()
      progress_label <- sprintf("Rodada %d de %d", state$current_index, round_count)
      score_label <- sprintf("Pontuação: %d", state$score)

      return(
        tagList(
          div(
            class = "panel-card",
            div(
              class = "status-strip",
              span(progress_label),
              span(sprintf("Jogador: %s", state$username)),
              span(score_label)
            ),
            feedback_ui(state$feedback),
            tags$img(class = "bird-photo", src = species$round_photo[[1]], alt = species$common_name[[1]]),
            tags$div(
              style = "margin: 18px 0;",
              tags$audio(src = species$audio_url[[1]], controls = NA, preload = "none", style = "width: 100%;")
            ),
            radioButtons(
              "guess",
              "Escolha uma opção",
              choiceNames = unname(lapply(seq_len(nrow(state$current_choices)), function(i) {
                tags$span(
                  state$current_choices$common_name[[i]],
                  " ",
                  tags$em(sprintf("(%s)", state$current_choices$scientific_name[[i]]))
                )
              })),
              choiceValues = unname(as.character(state$current_choices$common_name_norm))
            ),
            actionButton("submit_guess", "Enviar resposta", class = "btn-warning")
          )
        )
      )
    }

    div(
      class = "panel-card",
      h3("Partida concluída"),
      p(class = "score-highlight", sprintf("%s fez %d de %d pontos.", state$username, state$score, round_count)),
      feedback_ui(state$feedback),
      h4(class = "leader-title", "Top 10 pontuações"),
      tableOutput("leaderboard_table"),
      div(style = "margin-top: 18px;"),
      actionButton("play_again", "Jogar novamente", class = "btn-success")
    )
  })

  output$leaderboard_table <- renderTable({
    req((state$finished || state$viewing_leaderboard), !is.null(state$leaderboard))

    leaderboard <- state$leaderboard
    names(leaderboard) <- c("Usuário", "Pontuação Total", "Partidas Jogadas", "Melhor Partida", "Última Pontuação", "Última Partida")
    leaderboard
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "m")
}

shinyApp(ui, server)
