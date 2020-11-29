library(shiny)
library(DT)
library(gganimate)
library(xlsx)
library(ggplot2)
library(plotly)
library(lubridate)

source('service.R', local = TRUE)

server <- shinyServer(function(input, output, session) {
  co2 <- data.frame(na.omit(read.xlsx('C03-01b.xlsx', 1 , header=TRUE)))
  temp <- data.frame(na.omit(read.xlsx('C03-01a.xlsx', 1 , header=TRUE))) 

  output$temp <- DT::renderDataTable(temp)
  output$landTemperatureBoxplot <- renderPlotly(landTeperatureBoxPlot(temp))
  output$oceanTemperatureBoxplot <- renderPlotly(oceanTeperatureBoxPlot(temp))
  output$temperatureRegression <- renderPlotly(temperatureRegression(temp))
  output$temperaturePredictions <- renderPlotly(temperaturePredictions(temp))
  output$co2 <- DT::renderDataTable(co2, server = FALSE)
  output$co2Boxplot <- renderPlotly(co2Boxplot(co2))
  output$co2Regression <- renderPlotly(co2Regression(co2))
  output$co2Predictions <- renderPlotly(co2Predictions(co2))
  output$co2LogRegression <- renderPlotly(co2LogRegression(co2))
  output$co2LogPredictions <- renderPlotly(co2LogPredictions(co2))
  output$tempToCo2Regression <- renderPlotly(tempToCo2Regression(temp, co2))
  output$tempToCo2Predictions <- renderPlotly(tempToCo2Predictions(temp, co2))

  output$youtube <- renderUI({
    HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/G4H1N_yXBiA" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
  })
})
