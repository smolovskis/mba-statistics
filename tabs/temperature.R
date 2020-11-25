withSpinner <- function(plot) {
  addSpinner(plot)
}

temperature = tabItem(
  tabName = "temperature",
  fluidRow( 
    tabBox(title = "Temperature vs time", color = "blue", width = 10,
           tabs = list(
             list(menu = "Data", content = dataTableOutput("temp")),
             list(menu = "Land", content = withSpinner(plotlyOutput("landTemperatureBoxplot"))),
             list(menu = "Ocean", content = withSpinner(plotlyOutput("oceanTemperatureBoxplot"))),
             list(menu = "Residuals", content = withSpinner(plotlyOutput("temperatureRegression"))),
             list(menu = "Predictions", content = withSpinner(plotlyOutput("temperaturePredictions")))
           )
    )
  )
)
