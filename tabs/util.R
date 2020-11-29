library(shinyWidgets)
library(plotly)

withSpinner <- function(plot) {
  addSpinner(plot)
}

withContent <- function (name, title, content) {
  tabItem(tabName = name, fluidRow(tabBox( title = title, color = "teal", width = 10, tabs = content)))
}

text <- function(text) {
  tags$li(tags$span(text, style = "font-size:20px;"), style = "margin-bottom:8px;")
}