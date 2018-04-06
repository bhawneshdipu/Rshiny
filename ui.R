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
                                                    a(icon("sliders"),tags$b("Motif Discovery",class="nav-text"),
                                                      href="#shiny-tab-motifdiscovery","data-toggle"="tab",
                                                      "data-value"="motifdiscovery" ,"aria-expanded"="true")
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
                dateRangeInput("selectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                selectInput("selectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                selectInput("selectdataset", "Select Dataset:",choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1),
                selectInput("selectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1)
                
              ),
              menuItem("Box Plot", icon = icon("th"), tabName = "boxplot" ),
              conditionalPanel("input.sidebarmenu === 'boxplot'",
                               selectInput("selectcolor", "Select Color:",
                                           choices = list("wday.lbl " = "wday.lbl",
                                                          "Year.iso" = "year.iso",
                                                          "Half" = "half",
                                                          "Quarter"="quarter",
                                                          "Month" = "month",
                                                          "AM.PM" = "am.pm"),
                                           selected = 1),
                               
                               selectInput("selectboxplotx","X-axis:",choices=list("SUM"="SUM","SP1"="SP1")),
                               selectInput("selectboxploty","Y-axis:",choices=list("SUM"="SUM","SP1"="SP1"))
                               
              ),
              menuItem("Anomaly Detection", icon = icon("th"), tabName = "anomalydetection"),
              conditionalPanel("input.sidebarmenu === 'anomalydetection'",
                               sliderInput("selectperiod", "Period", 1, 200, 2),
                               checkboxInput("selectlastonly", "Last Only:", value = FALSE, width = NULL),
                               selectInput("selectanomalyx","X-axis:",choices=list("Index"="Index","SP11"="SP11"),selected="Index"),
                               selectInput("selectanomalyy","Y-axis:",choices=list("Index"="Index","SP11"="SP11"),selected = "SP11")
              ),
              menuItem("Motif Discovery", icon = icon("th"), tabName = "motifdiscovery"),
              conditionalPanel("input.sidebarmenu === 'motifdiscovery'",
                               selectInput("selectmotifx","X-axis:",choices=list("Index"="Index","SP11"="SP11","SP12"="SP12"),selected="SP12"),
                               selectInput("selectmotify","Y-axis:",choices=list("Index"="Index","SP11"="SP11"),selected = "SP11")
              ),
              menuItem("Machine Learning", icon = icon("th"), tabName = "machinelearning"),
              conditionalPanel("input.sidebarmenu === 'machinelearning'",
                               selectInput("machinelearningselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1)
                               
              ),
              menuItem("Linear Regression", icon = icon("th"), tabName = "linearregression"),
              conditionalPanel("input.sidebarmenu === 'linearregression'",
                               selectInput("linerregresionselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = "SP11")
                               
              ),
              menuItem("ARIMA", icon = icon("th"), tabName = "arima"),
              conditionalPanel("input.sidebarmenu === 'arima'",
                               selectInput("arimaselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = "SP11")
                               
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
            )
    ),
    tabItem(tabName = "motifdiscovery",
            fluidRow(
              column(12,
                     plotOutput("plot41", height = 300)
              )
            )
    ),
    tabItem(tabName = "machinelearning",
            fluidRow(
              column(12,
                     uiOutput("errormsg50")
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot51", height = 300)
              )
            ),fluidRow(
              column(12,
                     plotlyOutput("plot52", height = 300)
              )
            ),fluidRow(
              column(12,
                     dataTableOutput("table53")
              )
            )
  ),
  tabItem(tabName = "linearregression",
          fluidRow(
            column(12,
                   plotlyOutput("plot61", height = 300)
            )
          ),fluidRow(
                  column(12,
                        dataTableOutput("table62")
                    )
          )
  ),
  tabItem(tabName = "arima",
          fluidRow(
            column(12,
                   plotlyOutput("plot71", height = 300)
            )
          ),fluidRow(
            column(12,
                   dataTableOutput("table72")
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