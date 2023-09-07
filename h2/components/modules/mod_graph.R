# Run Hector given SSP input, start and end years for model run
# Plot function

# Default values set for SSP, start/end years, and selected var for plot

graph_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    actionButton(ns("plot"),"Plot"),
    plotlyOutput(ns("graph"))
  )
}

graph_server <- function(id, r6, i) {
  moduleServer(id, function(input, output, session) {
    observe({
      filtered_output <- filter(r6$output[[i()-1]],variable=="CO2_concentration") #i increases at end of mod_run so output is i-1
      output$graph <- renderPlotly({
        plot_ly(filtered_output, x = ~year, y = ~value,
                type = 'scatter', mode = 'lines',
                hovertemplate = paste(
                  "<b>Year:</b> %{x}<br>",
                  "<b>Value:</b> %{y:.2f}",
                  "<extra></extra>")
                ) %>%
          layout(xaxis = list(title="Year"),
                 yaxis = list(title="CO2 Concentration (ppmv)"),
                 title = "CO2 Concentration")
      })
    }) %>%
      bindEvent(input$plot)
  })
}