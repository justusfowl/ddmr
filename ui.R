library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(title = "Stock Prediction"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("predictTicker", "TICKER:",
                  c("APPLE" = "APPL",
                    "GOOGLE" = "GOOGL",
                    "SIEMENS" = "SIEGY")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", tabName = "settings", icon = icon("gear")), 
      menuItem("Settings", tabName = "settings", icon = icon("gear"))
      
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                infoBoxOutput("predictTickerSelected", width=3),
                infoBoxOutput("predictTickerDataAvailable", width=3),
                infoBoxOutput("predictTickerModelSelected", width=3),
                infoBoxOutput("predictTickerModel", width=3),
                infoBoxOutput("predictTickerClosingPrice", width=3)
              ),
             
              fluidRow(
                box(width=12, plotlyOutput("predictChart"))
              ), 
              
              fluidRow(
                box(width=12, dataTableOutput('outPredTable'))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "SETTINGS",
              h2("SELECT TICKER"), 
              fluidRow(
                box(
                  selectInput("variable", "Variable:",
                              c("Cylinders" = "cyl",
                                "Transmission" = "am",
                                "Gears" = "gear"))
                )
                
              )
      ),
      tabItem(tabName = "settings",
              h2("SELECT TICKER"), 
              fluidRow(
                box(
                  selectInput("variable", "Variable:",
                              c("Cylinders" = "cyl",
                                "Transmission" = "am",
                                "Gears" = "gear"))
                )
                
              )
      )
    )
  )
)