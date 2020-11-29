library(shinyWidgets)
library(plotly)

source('tabs/util.R', local = TRUE)

temperature = withContent("temperature", "Temperature vs time", list(
  list(menu = "Data", content = dataTableOutput("temp")),
  list(menu = "Land", content = withSpinner(plotlyOutput("landTemperatureBoxplot"))),
  list(menu = "Ocean", content = withSpinner(plotlyOutput("oceanTemperatureBoxplot"))),
  list(menu = "Residuals", content = withSpinner(plotlyOutput("temperatureRegression"))),
  list(menu = "Predictions", content = withSpinner(plotlyOutput("temperaturePredictions")))
))
