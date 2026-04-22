FROM rocker/r2u:24.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
    r-cran-arrow \
    r-cran-bslib \
    r-cran-dbi \
    r-cran-dplyr \
    r-cran-later \
    r-cran-rpostgres \
    r-cran-shiny \
    r-cran-shinywidgets \
    r-cran-stringi \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY . /app

ENV HOST=0.0.0.0
ENV PORT=8080

EXPOSE 8080

CMD ["Rscript", "start-app.R"]
