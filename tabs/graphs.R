graphs = tabItem(
  tabName = "graph",
  fluidRow(
    box(width = 8,
        title = "Graph 2",
        color = "red", ribbon = TRUE, title_side = "top right",
        column(width = 8,  plotOutput("anim_plot") %>% withSpinner(color="red", type = 6)
        )
    )
  )
)