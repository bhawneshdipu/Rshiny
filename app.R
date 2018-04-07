library(shinydashboard)
library(shiny)  
source("global.R")
h2o.init()
h2o.no_progress()


shinyApp(ui, server)

