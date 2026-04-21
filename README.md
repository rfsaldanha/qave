# qave

`qave` is a Shiny quiz game for identifying bird species recorded in the state of Rio de Janeiro.

Each game has 10 rounds. In every round, the app shows:

- one bird photo
- one bird audio recording
- four answer options with common and scientific names

Scores are stored locally in SQLite and shown in a leaderboard.

## Features

- 10-question bird identification game
- multiple-choice answers
- accent-insensitive answer matching
- family-based filtering before starting a game
- local leaderboard with total score, best score, and last score
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
- `RSQLite`
- `stringi`

## Run locally

From the project directory, start the app with:

```r
shiny::runApp()
```

or:

```sh
Rscript -e "shiny::runApp()"
```

When the app starts, it will also create a local SQLite database file:

- `leaderboard.sqlite`

## Gameplay

1. Enter a username.
2. Select one or more bird families.
3. Start the game.
4. Listen to the audio, inspect the photo, and choose one of the four options.
5. Review your score and leaderboard position after round 10.

## Project files

- `app.R`: complete Shiny application
- `inaturalist_birds_rj.parquet`: species dataset used by the app
- `leaderboard.sqlite`: generated locally after the first run

## License

See [LICENSE](/Users/raphaelsaldanha/drive2/qave/LICENSE).
