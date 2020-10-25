library(shiny)
library(DT)
library(gganimate)
library(xlsx)
library(ggplot2)
library(plotly)

server <- shinyServer(function(input, output, session) {
  co2 <- read.xlsx('C03-01b.xlsx', 1 , header=TRUE)
  temp <- read.xlsx('C03-01a.xlsx', 1 , header=TRUE) 
  
  plot <- function(){
    ggplot(co2, aes(x = Month, y = CO2)) +
      geom_point() +
      transition_states(
        Year,
        transition_length = 2,
        state_length = 1
      )
  }
  
  make_gif <- function(p, speed){
    animate(p, nframes = speed, fps = 20,
            renderer = gifski_renderer(loop = FALSE), start_pause = 15)
  }
  
  p <- plot()
  speed <- 60
  ani <- make_gif(p, speed)

  anim_save(filename = "ani.gif", animation = ani)

  output$anim_plot <- renderImage({
    list(src = "ani.gif", contentType = 'image/gif')
  }, deleteFile = FALSE)
  
  co2ggplot <- ggplot(co2, aes(x = Month, y = CO2)) + geom_point() + transition_time(Year)
  output$co2 <- renderDataTable(co2, server = FALSE)
  output$co2plot <- renderPlotly({ ggplotly(co2ggplot) })
  output$temp <- renderDataTable(temp)
})
