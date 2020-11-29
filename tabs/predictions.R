source('tabs/util.R', local = TRUE)

predictions = withContent( "predictions", "Predictions", list(
    list(menu = "Regression", content = withSpinner(plotlyOutput("tempToCo2Regression"))),
    list(menu = "Predictions", content = withSpinner(plotlyOutput("tempToCo2Predictions")))
  )
)
  

