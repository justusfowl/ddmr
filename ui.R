
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shinydashboard, plotly, quantmod, stringr, lubridate, shinycssloaders)

source("config.R")

date_today_str <- as.Date(now())
default_end_date <- as.Date(now())+5

dashboardPage(
  dashboardHeader(title = "Stock Prediction"),
  dashboardSidebar(
    sidebarMenu(

      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("predictTicker", h4("TICKER:"),
                  choices = ticker_choices),
      sliderInput("displayLastMonth", h4("HISTORIC DATA (Months):"), min=3, max=40, value=10, step=1),
      dateRangeInput("forecastingDates", h4("FORECASTING PERIOD:"), start = date_today_str, min = date_today_str, end=default_end_date),
      #checkboxInput("predSelectRecommendedModel","Use best performing model (recommended)", value=TRUE),
      radioButtons("modelTypeSelect", h4("MODEL SELECTION:"),
                   c("Auto (best performing)" = "AUTO",
                     "Linear Regression" = "LM",
                     "SVM" = "SVM",
                     "MARS" = "MARS")),
      
     
      
      # menuItem("Settings", tabName = "settings", icon = icon("gear")), 
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
                box(width=12, withSpinner(plotlyOutput("predictChartLy")))
              ), 
              
              fluidRow(
                box(width=12,downloadButton("downloadData", "Download")),
                box(width=12, dataTableOutput('outPredTable'))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "settings",
              menuItem("Predict", tabName = "dashboard", icon = icon("dashboard"))
      ),
      tabItem(tabName = "eval",
              h2("GENERAL APPROACH, MODELLING & DATA FOUNDATION"), 
              fluidRow(
                box(width=3, selectInput("modEvalTicker", "TICKER:", choices=ticker_choices))
              ),
              fluidRow(
                box( width=12,
                     plotlyOutput("modEvalChart")
                )
              ),
              h1("MODELLING"), 
              fluidRow(
                htmlOutput('pdfviewer'), 
                p("Running the Shiny-App from within R-Studio will result in automatic download of the modelling details via a new tab. Please run via the browser.")
              )
              
              
      )
    )
  )
)