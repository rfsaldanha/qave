# qave

`qave` is a Shiny quiz game for identifying bird species recorded in the state of Rio de Janeiro.

Each game has 10 rounds. In every round, the app shows:

- one bird photo
- one bird audio recording
- four answer options with common and scientific names

Scores are stored in a Postgres database and shown in a leaderboard.

## Features

- 10-question bird identification game
- multiple-choice answers
- accent-insensitive answer matching
- family-based filtering before starting a game
- leaderboard backed by Postgres with total score, best score, and last score
- species descriptions and reference links when available

## Data

The app reads species data from `inaturalist_birds_rj.parquet`.

The dataset is expected to include, at minimum:

- `common_name`
- `scientific_name`
- `family`
- a photo list in `photo_urls`
- an audio field in `audio_url` or `sound_url`

If present, the app also uses:

- `brief_description` or `description_paragraph`
- `wiki_url` or `description_url`

Only species with both usable image and audio media are included in the game.

## Requirements

Install these R packages:

- `shiny`
- `bslib`
- `shinyWidgets`
- `arrow`
- `dplyr`
- `DBI`
- `RPostgres`
- `stringi`

## Run locally

Create a `.Renviron` file in the project root with your database settings. You can
copy `.Renviron.example` and replace `DB_PASSWORD` with the real password.

Example:

```sh
cp .Renviron.example .Renviron
```

The app reads these environment variables at startup:

- `DB_NAME`
- `DB_HOST`
- `DB_PORT`
- `DB_USER`
- `DB_PASSWORD`
- `DB_SSLMODE`

After creating or editing `.Renviron`, restart your R session so the variables are
available to the app.

From the project directory, start the app with:

```r
shiny::runApp()
```

or:

```sh
Rscript -e "shiny::runApp()"
```

When the app starts, it will ensure the leaderboard tables exist in the configured Postgres database.

## Deploy to DigitalOcean

For DigitalOcean App Platform, add the same database variables in the app settings.
Store `DB_PASSWORD` as a secret instead of committing it to the repository.

The simplest setup is:

1. Keep `.Renviron` only for local development.
2. Add `DB_PASSWORD` in DigitalOcean as a secret.
3. Add the remaining `DB_*` variables in DigitalOcean only if you need values that
   differ from the defaults in `app.R`.

## Gameplay

1. Enter a username.
2. Select one or more bird families.
3. Start the game.
4. Listen to the audio, inspect the photo, and choose one of the four options.
5. Review your score and leaderboard position after round 10.

## Project files

- `app.R`: complete Shiny application
- `inaturalist_birds_rj.parquet`: species dataset used by the app

## License

See [LICENSE](/Users/raphaelsaldanha/drive2/qave/LICENSE).
