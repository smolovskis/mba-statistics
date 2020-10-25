library(shiny)
library(semantic.dashboard)
library(DT)
library(gganimate)
library(xlsx)
library(ggplot2)
library(plotly)
library(shinycssloaders)

source('tabs/introduction.R', local = TRUE)
source('tabs/tables.R', local = TRUE)
source('tabs/graphs.R', local = TRUE)

ui <- dashboardPage(
  dashboardHeader(
    menuItem(tabName = "introduction", "Introduction", icon = icon("table")),
    color = "blue",title = "global warming", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "introduction", "Introduction", icon = icon("table")),
      menuItem(tabName = "data", "Data", icon = icon("table")),
      menuItem(tabName = "graph", "Graph", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      introduction,
      createTabs,
      graphs
    ),
  ), theme = "cerulean"
)
