source('tabs/util.R', local = TRUE)

conclusions = tabItem( color = "teal",
  tabName = "conclusions",
  fluidPage( 
    br(),
    h1("Conclusions"),
    p(),
    tags$div(
      tags$ul(
        text("Land and ocean temperatures increasing since 1880"),
        text(HTML(paste0("CO",tags$sub("2")," level is increasing exponentialy"))),
        text(HTML(paste0("Linear relationship betwee CO",tags$sub("2")," and temperature")))
      )
    )
  )
)
