source("server.R", local = TRUE)
source("ui.R", local = TRUE)

shinyApp(ui = ui, server = server)

