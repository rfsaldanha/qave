library(rvest)

base_url <- "https://www.wikiaves.com.br"

split_js_arguments <- function(text) {
  parts <- character()
  current <- character()
  in_string <- FALSE
  escaped <- FALSE

  chars <- strsplit(text, "", fixed = TRUE)[[1]]

  for (ch in chars) {
    if (escaped) {
      current <- c(current, ch)
      escaped <- FALSE
      next
    }

    if (ch == "\\") {
      current <- c(current, ch)
      escaped <- TRUE
      next
    }

    if (ch == "'") {
      current <- c(current, ch)
      in_string <- !in_string
      next
    }

    if (!in_string && ch == ",") {
      parts <- c(parts, paste(current, collapse = ""))
      current <- character()
      next
    }

    current <- c(current, ch)
  }

  c(parts, paste(current, collapse = ""))
}

extract_lsp_calls <- function(script_text) {
  positions <- gregexpr("lsp(", script_text, fixed = TRUE)[[1]]
  positions <- positions[positions > 0]

  calls <- character()
  chars <- strsplit(script_text, "", fixed = TRUE)[[1]]

  for (start in positions) {
    idx <- start + 4
    if (idx > length(chars)) {
      next
    }

    first_char <- chars[idx]
    if (!first_char %in% c("'", " ")) {
      next
    }

    in_string <- FALSE
    escaped <- FALSE

    while (idx <= length(chars)) {
      ch <- chars[idx]

      if (escaped) {
        escaped <- FALSE
      } else if (ch == "\\") {
        escaped <- TRUE
      } else if (ch == "'") {
        in_string <- !in_string
      } else if (
        !in_string && ch == ")" && idx < length(chars) && chars[idx + 1] == ";"
      ) {
        calls <- c(calls, substr(script_text, start, idx + 1))
        break
      }

      idx <- idx + 1
    }
  }

  calls
}

parse_lsp_call <- function(call_text) {
  call_text <- sub("^lsp\\(", "", call_text)
  call_text <- sub("\\);$", "", call_text)

  parts <- trimws(split_js_arguments(call_text))
  parts[1:5] <- gsub("^'|'$", "", parts[1:5])
  parts[1:5] <- gsub("\\\\'", "'", parts[1:5], fixed = TRUE)

  data.frame(
    id = parts[1],
    family = parts[2],
    species = parts[3],
    common_name = parts[4],
    wiki_slug = parts[5],
    link = paste0(base_url, "/wiki/", parts[5]),
    photos = as.integer(parts[6]),
    sounds = as.integer(parts[7]),
    stringsAsFactors = FALSE
  )
}

fill_down <- function(x) {
  current <- ""
  for (i in seq_along(x)) {
    if (nzchar(x[i])) {
      current <- x[i]
    } else {
      x[i] <- current
    }
  }
  x
}

extract_scientific_name <- function(page, fallback = NA_character_) {
  title_node <- html_element(page, "title")
  title_text <- if (!inherits(title_node, "xml_missing")) {
    html_text2(title_node)
  } else {
    ""
  }
  match <- regexec("^.*\\(([^()]*)\\)\\s*\\|\\s*WikiAves", title_text)
  pieces <- regmatches(title_text, match)[[1]]

  if (length(pieces) >= 2 && nzchar(pieces[2])) {
    return(pieces[2])
  }

  fallback
}

extract_common_name <- function(page, fallback = NA_character_) {
  name_node <- html_element(page, "h1")
  if (inherits(name_node, "xml_missing")) {
    return(fallback)
  }

  name <- html_text2(name_node)
  if (nzchar(name)) name else fallback
}

scrape_species_page <- function(
  link,
  fallback_common_name = NA_character_,
  fallback_scientific_name = NA_character_
) {
  page <- read_html(link)

  common_name <- extract_common_name(page, fallback_common_name)
  scientific_name <- extract_scientific_name(page, fallback_scientific_name)

  main_photo <- html_attr(
    html_element(page, "meta[property='og:image']"),
    "content"
  )
  audio_mp3 <- html_attr(html_element(page, "audio source"), "src")

  data.frame(
    common_name = if (length(common_name) == 0) {
      fallback_common_name
    } else {
      common_name
    },
    scientific_name = if (length(scientific_name) == 0) {
      fallback_scientific_name
    } else {
      scientific_name
    },
    main_photo = if (length(main_photo) == 0) NA_character_ else main_photo,
    audio_mp3 = if (length(audio_mp3) == 0) NA_character_ else audio_mp3,
    page_link = link,
    stringsAsFactors = FALSE
  )
}

scrape_species_media <- function(species_table, sleep_seconds = 0) {
  media_rows <- lapply(seq_len(nrow(species_table)), function(i) {
    if (sleep_seconds > 0 && i > 1) {
      Sys.sleep(sleep_seconds)
    }

    scrape_species_page(
      link = species_table$link[i],
      fallback_common_name = species_table$common_name[i],
      fallback_scientific_name = species_table$species[i]
    )
  })

  do.call(rbind, media_rows)
}

scrape_wikiaves_tables <- function(url, sleep_seconds = 0) {
  species_table <- scrape_wikiaves_species(url)
  species_media_table <- scrape_species_media(
    species_table,
    sleep_seconds = sleep_seconds
  )

  list(
    species_table = species_table,
    species_media_table = species_media_table
  )
}

scrape_wikiaves_species <- function(url) {
  page <- read_html(url)
  script_text <- paste(
    html_text(html_elements(page, "tbody script")),
    collapse = "\n"
  )
  call_matches <- extract_lsp_calls(script_text)

  if (length(call_matches) == 0) {
    stop("No table rows were found in the page scripts.")
  }

  rows <- do.call(rbind, lapply(call_matches, parse_lsp_call))
  rows$family <- fill_down(rows$family)
  rows
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  url <- if (length(args) >= 1) {
    args[1]
  } else {
    "https://www.wikiaves.com.br/especies.php?t=c&c=3304557"
  }
  output_csv <- if (length(args) >= 2) args[2] else ""
  tables <- scrape_wikiaves_tables(url)
  species_table <- tables$species_table
  species_media_table <- tables$species_media_table

  print(utils::head(species_table, 10))
  cat("\nRows scraped:", nrow(species_table), "\n")
  cat("Media rows scraped:", nrow(species_media_table), "\n")
  print(utils::head(species_media_table, 10))

  if (nzchar(output_csv)) {
    utils::write.csv(
      species_table,
      output_csv,
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    cat("CSV saved to:", output_csv, "\n")

    media_output_csv <- sub(
      "\\.csv$",
      "_media.csv",
      output_csv,
      ignore.case = TRUE
    )
    if (identical(media_output_csv, output_csv)) {
      media_output_csv <- paste0(output_csv, "_media.csv")
    }

    utils::write.csv(
      species_media_table,
      media_output_csv,
      row.names = FALSE,
      fileEncoding = "UTF-8"
    )
    cat("Media CSV saved to:", media_output_csv, "\n")
  }
}

if (sys.nframe() == 0) {
  main()
}


tables <- scrape_wikiaves_tables(
  "https://www.wikiaves.com.br/especies.php?t=c&c=3304557"
)

arrow::write_parquet(x = tables$species_table, sink = "species.parquet")
arrow::write_parquet(
  x = tables$species_media_table,
  sink = "species_media.parquet"
)
