# Load Test

This folder contains a `k6` load test for the live DigitalOcean deployment.

What it exercises:

- `GET /` on the public app URL
- validation that the Shiny shell is returned
- a sampled batch of local JS and CSS assets referenced by the homepage

What it does not exercise:

- authenticated traffic
- full in-browser gameplay interactions
- Shiny websocket message flows after the page boots

## Default target

The script targets:

```text
https://qave-app-4plrr.ondigitalocean.app
```

Override it with `BASE_URL` if needed.

## Run

```sh
k6 run loadtest/k6-homepage.js
```

## Example: heavier run

```sh
BASE_URL=https://qave-app-4plrr.ondigitalocean.app \
STAGE_1_VUS=25 \
STAGE_2_VUS=75 \
STAGE_1_DURATION=1m \
STAGE_2_DURATION=3m \
SLEEP_SECONDS=0.5 \
k6 run loadtest/k6-homepage.js
```

## Useful outputs

```sh
k6 run --summary-export loadtest/summary.json loadtest/k6-homepage.js
```

Key thresholds built into the script:

- failed requests under 2%
- p95 request latency under 2 seconds
- checks pass rate above 99%
