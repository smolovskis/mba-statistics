source('tabs/util.R', local = TRUE)

co2 = withContent("co2", "CO2 vs times", list(
     list(menu = "Data", content = dataTableOutput("co2")),
     list(menu = "Boxplot", content = withSpinner(plotlyOutput("co2Boxplot"))),
     list(menu = "Linear", content = mainPanel(
       withSpinner(plotlyOutput("co2Regression")),
       withSpinner(plotlyOutput("co2Predictions"))
     )),
     list(menu = "Log", content = mainPanel(
       withSpinner(plotlyOutput("co2LogRegression")),
       withSpinner(plotlyOutput("co2LogPredictions"))
     ))
   )
)

