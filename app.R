# source the core files
source("ui.R")
source("server.R")

# compile app
shinyApp(ui = ui, server = server)