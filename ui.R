source('tabs/introduction.R', local = TRUE)
# source('tabs/tables.R', local = TRUE)
# source('tabs/graphs.R', local = TRUE)
source('tabs/temperature.R', local = TRUE)
source('tabs/co2.R', local = TRUE)

ui <- dashboardPage(
  dashboardHeader(
    color = "blue",title = "global warming", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "introduction", "Introduction", icon = icon("table")),
      # menuItem(tabName = "data", "Data", icon = icon("table")),
      menuItem(tabName = "temperature", "Temperature", icon = icon("table")),
      menuItem(tabName = "co2", "CO2", icon = icon("table"))
      # menuItem(tabName = "graph", "Graph", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      introduction,
      temperature,
      co2
    ),
  ), theme = "cerulean"
)
