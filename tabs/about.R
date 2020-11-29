source('tabs/util.R', local = TRUE)

about = tabItem(
  tabName = "about",
  fluidPage(
    br(),
    h1("MBA statistics case study: Global warming"),
    h4("Designed by: Andris Smolovskis, Peteris Gredzens"),
    h4(HTML(paste0("Code available at: ",tags$a(href = "https://github.com/smolovskis/mba-statistics", "Github"))))
  )
)