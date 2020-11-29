source('tabs/introduction.R', local = TRUE)
source('tabs/temperature.R', local = TRUE)
source('tabs/co2.R', local = TRUE)
source('tabs/predictions.R', local = TRUE)
source('tabs/conclusions.R', local = TRUE)
source('tabs/about.R', local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Global warming", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "introduction", "Introduction", icon = icon("comments")),
      menuItem(tabName = "temperature", "Temperature", icon = icon("thermometer")),
      menuItem(tabName = "co2", "CO2", icon = icon("industry")),
      menuItem(tabName = "predictions", "Predictions", icon = icon("globe")),
       menuItem(tabName = "conclusions", "Conclusions", icon = icon("sync")),
      menuItem(tabName = "about", "About", icon = icon("users"))
    )
  ),
  dashboardBody( color = "teal",
    tabItems(
      introduction,
      temperature,
      co2,
      predictions,
      conclusions,
      about
    ),
  ), theme = "cerulean"
)
