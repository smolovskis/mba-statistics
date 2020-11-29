library(shiny)
library(semantic.dashboard)

introduction <-  tabItem(
  "introduction",
  fluidPage(
    br(),
    h1("Introduction"),
    tags$div(
      tags$ul(
        tags$li(tags$span("Temperature from National Climatic Data Center")),
        tags$li(tags$span("CO2 from  Mauna Loa Observatory")),
        tags$li(tags$span("Cost of Type I error - $18 trillion ")),
        tags$li(tags$span("Cost of Type II error - $535 trillion")),
      )
    ),
    h2("Problem statements:"),
    tags$div(
      tags$ul(
        tags$li(tags$span("Is the earth actually warming?")),
        tags$li(tags$span("Are the global CO2 emissions increasing?")),
        tags$li(tags$span("Is the CO2 the main cause why the planet is warming?"))
      )
    )
  )
)