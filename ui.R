## ui.R ##
library(shinydashboard)
library(shiny)  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Basic", icon = icon("th"), tabName = "basic" ),
    menuItem("Time Series", icon = icon("th"), tabName = "timeseries"),
    menuItem("Scientific Plot", icon = icon("th"), tabName = "scientific")
    
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              box(plotOutput("plot2", height = 250))
            ),
            fluidRow(
              box(plotOutput("plot3", height = 250)),
              box(plotOutput("plot4", height = 250))
            )
            
            
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Visualization"),
  sidebar,
  body
)