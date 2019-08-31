library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(title = "Stock Prediction"),
  dashboardSidebar(
    sidebarMenu(

      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", tabName = "settings", icon = icon("gear")), 
      menuItem("Evaluation", tabName = "eval", icon = icon("search"))
      
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
      tabItem(tabName = "settings",
              fluidRow(
                box(width=12,
                  selectInput("predictTicker", h3("TICKER:"),
                              c("APPLE" = "APPL",
                                "GOOGLE" = "GOOGL",
                                "SIEMENS" = "SIEGY"))
                )
              ),
              fluidRow(
                box(width=12,
                    dateRangeInput("dates", h3("FORECASTING PERIOD:"))
                )
              ),
              fluidRow(
                box(width=12,
                    h3("FORECASTING PERIOD:"),
                    checkboxInput("predSelectRecommendedModel","Use best performing model (recommended)")
                )
              ),
              fluidRow(
                box(width=12,
                    radioButtons("modelTypeSelect", "manual selection",
                                c("Random Forest" = "RF",
                                  "SVM" = "SVM",
                                  "NB" = "NB",
                                  "Moving averages" = "MA"))
                )
              ), 
              menuItem("Predict", tabName = "dashboard", icon = icon("dashboard"))
      ),
      tabItem(tabName = "eval",
              h2("GENERAL APPROACH"), 
              fluidRow(
                box( width=12,
                  textOutput("txt_gen_approach")
                )
              ),
              h2("DATA FOUNDATION"), 
              fluidRow(
                box(width=3, selectInput("modEvalTicker", "TICKER:",
                                         c("APPLE" = "APPL",
                                           "GOOGLE" = "GOOGL",
                                           "SIEMENS" = "SIEGY")))
              ),
              fluidRow(
                box( width=8,
                     plotlyOutput("modEvalChart")
                ), 
                box( width=4, 
                     textOutput("txt_mod_data_summary")
                     )
              ), 
              h1("MODELLING"), 
              h3("TRAINING"),
              fluidRow(
                box( width=12,
                     h4("Feature engineering"),
                     textOutput("txt_mod_details"), 
                     code("Code for features created")
                )
              ),
              
              h3("EVALUATION & SELECTION"),

              ## THIS SECTION NEEDS TO BE CONDITIONAL UPON THE SELECTED TICKER ## 
              
              # GOOGL
              
              fluidRow(
                box( width=12,
                     h4("Model training & evaluation | GOOGL"),
                     textOutput("txt_googl_mod_eval"), 
                     code("Code for features created")
                )
              ),
              
              # APPL
              
              fluidRow(
                box( width=12,
                     h4("Model training & evaluation | GOOGL"),
                     textOutput("txt_appl_mod_eval"), 
                     code("Code for features created")
                )
              )
      )
    )
  )
)