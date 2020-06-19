# Final Deliverable

# load the shiny library
suppressMessages(library("shiny"))

# Create a new 'shinyApp()` using the ui and server from
# `app_ui.R`and`app_server.R`

source("app_ui.R")
source("app_server.R")

shinyApp(ui = ui, server = server)
