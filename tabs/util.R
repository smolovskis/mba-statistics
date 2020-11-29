withSpinner <- function(plot) {
  addSpinner(plot)
}

withContent <- function (name, title, content) {
  tabItem(tabName = name, fluidRow(tabBox( title = title, color = "teal", width = 10, tabs = content)))
}