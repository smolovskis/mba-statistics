withSpinner <- function(plot) {
  addSpinner(plot)
}

co2 = tabItem(
  tabName = "co2",
  fluidPage(
    tabBox(title = "CO2 vs times", color = "blue", width = 10,
           tabs = list(
             list(menu = "Data", content = dataTableOutput("co2")),
             list(menu = "Boxplot", content = withSpinner(plotlyOutput("co2Boxplot"))),
             list(menu = "Residuals", content = withSpinner(plotlyOutput("co2Regression"))),
             list(menu = "Predictions", content = withSpinner(plotlyOutput("co2Predictions"))),
             list(menu = "Log Residuals", content = withSpinner(plotlyOutput("co2LogRegression"))),
             list(menu = "Log Predictions", content = withSpinner(plotlyOutput("co2LogPredictions")))
           )
    )
  )
)
