source('tabs/util.R', local = TRUE)

conclusions = tabItem( color = "teal",
  tabName = "conclusions",
  fluidPage( 
    br(),
    h1("Conclusions"),
    p(),
    tags$div(
      tags$ul(
        tags$li(tags$span("Land and ocean temperatures increasing since 1880")),
        tags$li(tags$span("CO"), tags$sub("2"), tags$span(" temperatures exponentialy")),
        tags$li(tags$span("Linear relationship betwee CO"), tags$sub("2"), tags$span(" and temperature"))
      )
    )
  )
)
