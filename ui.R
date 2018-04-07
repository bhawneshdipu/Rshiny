## ui.R ##
library(shinydashboard)
library(shiny)  
library(shinyjs)


sp <- c("SP1","SP2","SP3","SP4","SP5","SP6","SP7","SP8","SP9","SP10","SP11","SP12","SP13","SP14")
ln <- c("LN1","LN2","LN3","LN4","LN5","LN6","LN7","LN8","LN9")
cs <- c("CS1","CS2","CS3","CS4","CS5","CS6","SUM")
dt<-c("Index", "Date_Time")
lst<-c(sp,ln,cs)

mychoices=setNames(lst,lst)
header <- dashboardHeader(title = "Visualization", titleWidth = 300,tags$li(class = "dropdown",  
                                                  a(icon("dashboard"),tags$b("Dashboard",class="nav-text"),
                                                    href="#shiny-tab-dashboard","data-toggle"="tab",
                                                    "data-value"="dashboard" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",
                                                  a(icon("sitemap"),tags$b("Box Plot",class="nav-text"),
                                                    href="#shiny-tab-boxplot","data-toggle"="tab",
                                                    "data-value"="boxplot" ,"aria-expanded"="true")
                                          ),tags$li(class = "dropdown",
                                                    a(icon("bar-chart"),tags$b("Bar Plot",class="nav-text"),
                                                      href="#shiny-tab-barplot","data-toggle"="tab",
                                                      "data-value"="barplot" ,"aria-expanded"="true")
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



sidebar <- dashboardSidebar(width = 300,
  sidebarMenu(id = "sidebarmenu",
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              conditionalPanel(
                "input.sidebarmenu == 'dashboard'",
                selectInput("dashboardselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                            selected = 1),
                dateRangeInput("dashboardselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                selectInput("dashboardselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                checkboxGroupInput("dashboardselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                selectInput("dashboardselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                actionButton("dashboardaction", "Apply Changes", icon = icon("play"))
              ),
              menuItem("Box Plot", icon = icon("th"), tabName = "boxplot" ),
              conditionalPanel("input.sidebarmenu === 'boxplot'",
                               selectInput("boxplotselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("boxplotselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("boxplotselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               checkboxGroupInput("boxplotselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("boxplotselectx","Select X:",choices=mychoices),
                               selectInput("boxplotselecty","Select Y:",choices=mychoices),
                               selectInput("boxplotselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("boxplotselectcolor", "Select Color:",
                                           choices = list("wday.lbl " = "wday.lbl",
                                                          "Year.iso" = "year.iso",
                                                          "Half" = "half",
                                                          "Quarter"="quarter",
                                                          "Month" = "month",
                                                          "AM.PM" = "am.pm"),
                                           selected = 1),
                               actionButton("boxplotaction", "Apply Changes", icon = icon("play"))
                               
              ),
              menuItem("Bar Plot", icon = icon("th"), tabName = "barplot" ),
              conditionalPanel("input.sidebarmenu === 'barplot'",
                               selectInput("barplotselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("barplotselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("barplotselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               checkboxGroupInput("barplotselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("barplotselectx","Select X:",choices=mychoices),
                               selectInput("barplotselecty","Select Y:",choices=mychoices),
                               selectInput("barplotselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("barplotselectcolor", "Select Color:",
                                           choices = list("wday.lbl " = "wday.lbl",
                                                          "Year.iso" = "year.iso",
                                                          "Half" = "half",
                                                          "Quarter"="quarter",
                                                          "Month" = "month",
                                                          "AM.PM" = "am.pm"),
                                           selected = 1),
                               actionButton("barplotaction", "Apply Changes", icon = icon("play"))
                               
              ),
              menuItem("Anomaly Detection", icon = icon("th"), tabName = "anomalydetection"),
              conditionalPanel("input.sidebarmenu === 'anomalydetection'",
                               selectInput("anomalyselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("anomalyselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("anomalyselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               #checkboxGroupInput("anomalyselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("anomalyselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("anomalyselectx","Select X:",choices=setNames(dt,dt),selected="Index"),
                               selectInput("anomalyselecty","Select Y:",choices=setNames(lst,lst),selected = "SP11"),
                               sliderInput("anomalyselectperiod", "Period", 1, 200, 2),
                               checkboxInput("anomalyselectlastonly", "Last Only:", value = FALSE, width = NULL),
                               actionButton("anomalyaction", "Apply Changes", icon = icon("play"))
                               
              ),
              menuItem("Motif Discovery", icon = icon("th"), tabName = "motifdiscovery"),
              conditionalPanel("input.sidebarmenu === 'motifdiscovery'",
                               selectInput("motifselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("motifselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("motifselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               #checkboxGroupInput("motifselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("motifselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("motifselectx","Select X:",choices=setNames(lst,lst),selected = "SP12"),
                               selectInput("motifselecty","Select Y:",choices=setNames(lst,lst),selected = "SP11"),
                               sliderInput("motifselectwindowsize", "Window Size:",
                                           min = 1, max = 25, value = 2
                               ),
                               actionButton("motifaction", "Apply Changes", icon = icon("play"))
                               ),
              menuItem("Machine Learning", icon = icon("th"), tabName = "machinelearning"),
              conditionalPanel("input.sidebarmenu === 'machinelearning'",
                               selectInput("machinelearningselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("motifselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("machinelearningselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               #checkboxGroupInput("motifselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("machinelearningselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("machinelearningselecttrack", "Select Track:",choices =setNames(lst,lst),selected = "SP11"),
                               sliderInput("machinelearningmaxruntime", "Max Runtime (sec):",
                                           min = 1, max = 600, value = 60
                               ),
                               actionButton("machinelearningaction", "Apply Changes", icon = icon("play"))
              ),
              menuItem("Linear Regression", icon = icon("th"), tabName = "linearregression"),
              conditionalPanel("input.sidebarmenu === 'linearregression'",
                               selectInput("linearregressionselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("linearregressionselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("linearregressionselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               #checkboxGroupInput("motifselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("linearregressionselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("linearregressionselecttrack", "Select Track:",choices =setNames(lst,lst),selected = "SP11"),
                               
                               actionButton("linearregressionaction", "Apply Changes", icon = icon("play"))
                               ),
              menuItem("ARIMA", icon = icon("th"), tabName = "arima"),
              conditionalPanel("input.sidebarmenu === 'arima'",
                               selectInput("arimaselectfile", "Select Sector:",choices = list("All " = "all","Sector 1" = "SC1", "Sector 2" = "SC2","Sector 3" = "SC3","Sector 4" = "SC4","Sector 5" = "SC5"),
                                           selected = 1),
                               dateRangeInput("arimaselectdaterange", "Date Range:",start="01-01-2016",format = "dd-mm-yyyy"),
                               selectInput("arimaselectchannel", "Select Channel:",choices = list("All"="all","Direction(1,2)"="12","Direction(3,4)"="34","Channel 1" = 1, "Channel 2" = 2,"Channel 3" = 3,"Channel 4" = 4), selected = 1),
                               
                               #checkboxGroupInput("arimaselectbins", "Select Bins:", choices = list("Speed" = "speed", "Length" = "length","Weight" = "weight"), selected = 1,inline = TRUE),
                               selectInput("arimaselectdataaggregation", "Data Aggregation:",choices = list("Minutes" = "minute", "Hour" = "hours","Day" = "days","Week" = "weeks","Months" = "months","Quarter" = "quarter","Year" = "year"), selected = 1),
                               selectInput("arimaselecttrack", "Select Track:",choices =setNames(lst,lst),selected = "SP11"),
                               
                               actionButton("arimaaction", "Apply Changes", icon = icon("play"))                               
              )
              
              
  ))

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(column(4,
              tableOutput("sectortable")
            ),
            column(1),
            column(7,
                     img(src="sector.jpg", height = 250)
              )
            ),fluidRow(
              column(12,
                     uiOutput("errormsg10")
              )
            ),
            fluidRow(column(12,
                            plotlyOutput("plot11", height = 300)
                            )
            ),
            fluidRow(column(12,
                            plotlyOutput("plot12", height = 300)
            )
            ),
            fluidRow(column(12,
                            plotlyOutput("plot13", height = 300)
            )
            )
    ),
    tabItem(tabName = "boxplot",
            fluidRow(
              column(12,
                     uiOutput("errormsg20")
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot21", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot22", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot23", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot24", height = 300)
              )
            )
    ),
    tabItem(tabName = "barplot",
            fluidRow(
              column(12,
                     uiOutput("errormsg80")
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot81", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot82", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot83", height = 300)
              )
            ),
            fluidRow(
              column(12,
                     plotlyOutput("plot84", height = 300)
              )
            )
    ),
    tabItem(tabName = "anomalydetection",
            fluidRow(
              column(12,
                     uiOutput("errormsg30")
              )
            ),            fluidRow(
              column(12,
                     plotlyOutput("plot31", height = 300)
              )
            )
    ),
    tabItem(tabName = "motifdiscovery",
            fluidRow(
              column(12,
                     uiOutput("errormsg40")
              )
            ),fluidRow(
              column(12,
                     plotlyOutput("plot41", height = 300)
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
                     tags$h2("Summary Table")
              )
            ),fluidRow(
              column(12,
                     DT::dataTableOutput("summary52")
              )
            ),fluidRow(
              column(12,
                     tags$h2("Error Table")
              )
            ),
            fluidRow(
              column(12,
                     DT::dataTableOutput("table52")
              )
            )
  ),
  tabItem(tabName = "linearregression",
          fluidRow(
            column(12,
                   uiOutput("errormsg60")
            )
          ),fluidRow(
            column(12,
                   plotlyOutput("plot61", height = 300)
            )
          ),fluidRow(
            column(12,
                   tags$h2("Summary Table")
            )
          ),fluidRow(
            column(12,
                   DT::dataTableOutput("summary62")
            )
          )
          ,fluidRow(
            column(12,
                   tags$h2("Error Table")
            )
          ),fluidRow(
            column(12,
                   DT::dataTableOutput("table62")
            )
          )
  ),
  tabItem(tabName = "arima",
          fluidRow(
            column(12,
                   uiOutput("errormsg70")
            )
          ),fluidRow(
            column(12,
                   plotlyOutput("plot71", height = 300)
            )
          ),fluidRow(
            column(12,
                   tags$h2("Summary Table")
            )
          ),fluidRow(
            column(12,
                   DT::dataTableOutput("summary72")
            )
          ),fluidRow(
            column(12,
                   tags$h2("Error Table")
            )
          ),fluidRow(
            column(12,
                   DT::dataTableOutput("table72")
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