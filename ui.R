## ui.R ##
library(shinydashboard)
library(shiny)  

header <- dashboardHeader(title = "Visualization", titleWidth = 325,tags$li(class = "dropdown",  
                                                  a(icon("dashboard"),tags$b("Dashboard",class="nav-text"),
                                                    href="#shiny-tab-dashboard","data-toggle"="tab",
                                                    "data-value"="dashboard" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",
                                                  a(icon("sitemap"),tags$b("Box Plot",class="nav-text"),
                                                    href="#shiny-tab-boxplot","data-toggle"="tab",
                                                    "data-value"="boxplot" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",  
                                                  a(icon("line-chart"),tags$b("Anomaly Detection",class="nav-text"),
                                                    href="#shiny-tab-anomalydetection","data-toggle"="tab",
                                                    "data-value"="anomalydetection" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",  
                                                    a(icon("sliders"),tags$b("Motif Discovery Detection",class="nav-text"),
                                                      href="#shiny-tab-motifdiscoverydetection","data-toggle"="tab",
                                                      "data-value"="motifdiscoverydetection" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",  
                                                            a(icon("heartbeat"),tags$b("Machine Learning",class="nav-text"),
                                                              href="#shiny-tab-machinelearning","data-toggle"="tab",
                                                              "data-value"="machinelearning" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",  
                                                            a(icon("area-chart"),tags$b("Linear Regression",class="nav-text"),
                                                              href="#shiny-tab-linearregression","data-toggle"="tab",
                                                              "data-value"="linearregression" ,"aria-expanded"="true")
                                        ),tags$li(class = "dropdown",  
                                                           a(icon("signal"),tags$b("ARIMA",class="nav-text"),
                                                             href="#shiny-tab-arima","data-toggle"="tab",
                                                             "data-value"="arima" ,"aria-expanded"="true")
                                        )
                          )



sidebar <- dashboardSidebar(width = 325,
  sidebarMenu(id = "sidebarmenu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              conditionalPanel(
                "input.sidebarmenu == 'dashboard'",
                selectInput("selectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                            selected = 1),
                dateRangeInput("daterange", "Date Range:"),
                selectInput("selectchannel", "Select Channel:",choices = list("Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                selectInput("selectdataset", "Select Dataset:",choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1),
                selectInput("selectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hour","Day" = "day","Week" = "week","Month" = "month","Quarter" = "quarter","Year" = "year"), selected = 1)
                
              ),
              menuItem("Box Plot", icon = icon("th"), tabName = "boxplot" ),
              conditionalPanel("input.sidebarmenu === 'boxplot'",
                               sliderInput("b", "Under Basic", 1, 100, 50)
              ),
              menuItem("Anomaly Detection", icon = icon("th"), tabName = "anomalydetection"),
              conditionalPanel("input.sidebarmenu === 'anomalydetection'",
                               sliderInput("b", "Under Time series", 1, 100, 50)
              ),
              menuItem("Motif Discovery Detection", icon = icon("th"), tabName = "motifdiscoverydetection"),
              conditionalPanel("input.sidebarmenu === 'motifdiscoverydetection'",
                               sliderInput("x", "Scientific", 1, 100, 50)
              ),
              menuItem("Machine Learning", icon = icon("th"), tabName = "machinelearning"),
              conditionalPanel("input.sidebarmenu === 'machinelearning'",
                               sliderInput("x", "Scientific", 1, 100, 50)
              ),
              menuItem("Linear Regression", icon = icon("th"), tabName = "linearregression"),
              conditionalPanel("input.sidebarmenu === 'linearregression'",
                               sliderInput("x", "Scientific", 1, 100, 50)
              ),
              menuItem("ARIMA", icon = icon("th"), tabName = "arima"),
              conditionalPanel("input.sidebarmenu === 'arima'",
                               sliderInput("x", "Scientific", 1, 100, 50)
              )
              
              
  ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(column(4,
              tableOutput("sectortable")
            ),
            column(1),
            column(7,
                     img(src="sector.jpg", height = 250)
              )
            ),
            fluidRow(column(12,
                            plotlyOutput("plot11", height = 300)
                            )
            )
    ),
    tabItem(tabName = "boxplot",
            fluidRow(
              column(12,
                     plotlyOutput("plot21", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot22", height = 300)
              )
            )
    ),
    tabItem(tabName = "anomalydetection",
            fluidRow(
              column(12,
                     plotlyOutput("plot31", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot32", height = 300)
              )
            )
    ),
    tabItem(tabName = "machinelearning",
            fluidRow(
              column(12,
                     plotlyOutput("plot41", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot42", height = 300)
              )
          )
  ),
  tabItem(tabName = "linearregression",
          fluidRow(
            column(12,
                   plotlyOutput("plot51", height = 300)
            )
          ),
          fluidRow(
            column(12,
                   plotlyOutput("plot52", height = 300)
            )
          )
          
  ),
  tabItem(tabName = "arima",
          fluidRow(
            column(12,
                   plotlyOutput("plot61", height = 300)
            )
          ),
          fluidRow(
            column(12,
                   plotlyOutput("plot62", height = 300)
            )
          )
  )
  ),
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  tags$head(tags$script(src="script.js"))
  
  )
  


# Put them together into a dashboardPage
dashboardPage(skin = "black",
  header,
  sidebar,
  body
)