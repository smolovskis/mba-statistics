library(shiny)
library(semantic.dashboard)

source('tabs/util.R', local = TRUE)

introduction <- tabItem(tabName = "introduction", 
  fluidRow(tabBox( title = "Introduction", color = "teal", width = 10, tabs = 
     list(
       list(menu = "Introduction", content =  tags$div( style = "height:500px;",
         tags$ul(
           text("Temperature from National Climatic Data Center"),
           text(HTML(paste0("CO",tags$sub("2")," from  Mauna Loa Observatory"))),
           text("Cost of Type I error - $18 trillion "),
           text("Cost of Type II error - $535 trillion"),
         ),
         tags$div( style = "display:flex;justify-content:center",
           img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/e/ed/Global_Temperature_Anomaly.gif/800px-Global_Temperature_Anomaly.gif", align = "left",height='350px',width='700px')
         )    
       )),
       list(menu = "Problem statements", content = tags$div(
         tags$ul(
           text("Is the earth actually warming?"),
           text(HTML(paste0("Are the global CO",tags$sub("2")," emissions increasing?"))),
           text(HTML(paste0("Is CO",tags$sub("2")," the main cause why the planet is warming?")))
         )
       )),
       list(menu = "Video", content = uiOutput("youtube"))
     )
   )
  )
)
