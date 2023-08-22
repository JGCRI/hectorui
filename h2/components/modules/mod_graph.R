# Run Hector given SSP input, start and end years for model run
# Plot function

# Default values set for SSP, start/end years, and selected var for plot

graph_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    actionButton(ns("plot"),"Plot"),
    plotOutput(ns("graph"))
  )
}

graph_server <- function(id,r6) {
  moduleServer(id, function(input, output, session) {
    observe({
      #filtered_output <- filter(r6$output,variable=="RF_tot")
      output$graph <- renderPlot({
        ggplot(r6$output) +
          aes(x = year, y = value) +
          geom_line() +
          facet_wrap(~variable, scales = "free_y")
      })
    }) %>%
      bindEvent(input$plot)
  })
}


