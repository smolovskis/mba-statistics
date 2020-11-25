library(shiny)
library(DT)
library(gganimate)
library(xlsx)
library(ggplot2)
library(plotly)
library(gifski)
library(plotly)
library(semantic.dashboard)
library(shinycssloaders)
library(shinyWidgets)
library(xlsx)
library(lubridate)

source('service.R', local = TRUE)

server <- shinyServer(function(input, output, session) {
  co2 <- na.omit(read.xlsx('C03-01b.xlsx', 1 , header=TRUE))
  temp <- na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE)) 

  output$temp <- renderDataTable(temp)
  output$landTemperatureBoxplot <- renderPlotly(landTeperatureBoxPlot(temp))
  output$oceanTemperatureBoxplot <- renderPlotly(oceanTeperatureBoxPlot(temp))
  output$temperatureRegression <- renderPlotly(temperatureRegression(temp))
  output$temperaturePredictions <- renderPlotly(temperaturePredictions(temp))
  
  output$co2 <- renderDataTable(co2, server = FALSE)
  output$temp <- renderDataTable(co2)
  output$co2Boxplot <- renderPlotly(co2Boxplot(co2))
  output$co2Regression <- renderPlotly(co2Regression(co2))
  output$co2Predictions <- renderPlotly(co2Predictions(co2))
  output$co2LogRegression <- renderPlotly(co2LogRegression(co2))
  output$co2LogPredictions <- renderPlotly(co2LogPredictions(co2))
})



# plot <- function(){
#   ggplot(co2, aes(x = Month, y = CO2)) +
#     geom_point() +
#     transition_states(
#       Year,
#       transition_length = 2,
#       state_length = 1
#     )
# }
# 
# make_gif <- function(p, speed){
#   animate(p, nframes = speed, fps = 20,
#           renderer = gifski_renderer(loop = FALSE), start_pause = 15)
# }
# 
# p <- plot()
# speed <- 60
# ani <- make_gif(p, speed)
# 
# anim_save(filename = "ani.gif", animation = ani)
# 
# output$anim_plot <- renderImage({
#   list(src = "ani.gif", contentType = 'image/gif')
# }, deleteFile = FALSE)
# co2ggplot <- ggplot(co2, aes(x = Month, y = CO2)) + geom_point() + transition_time(Year)