host <- Sys.getenv("HOST", "0.0.0.0")
port <- as.integer(Sys.getenv("PORT", "8080"))

shiny::runApp(
  appDir = ".",
  host = host,
  port = port,
  launch.browser = FALSE
)
