createTabs = tabItem(
  tabName = "data",
  fluidRow( 
    tabBox(title = "Data", color = "blue", width = 10,
           tabs = list(
             list(menu = "CO2 levels", content = dataTableOutput("co2")),
             list(menu = "Temperature", content = dataTableOutput("temp"))
           )
    )
  )
)
