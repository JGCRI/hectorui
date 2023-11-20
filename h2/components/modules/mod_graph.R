# Run Hector given SSP input, start and end years for model run
# Plot function

# Default values set for SSP, start/end years, and selected var for plot

graph_ui <- function(id) {
  ns <- NS(id)
  fluidRow(selectInput(ns("variable"), "Select a variable to plot:",
                       c("Global Temperature at Surface" = "global_tas",
                         "CO2 Concentration" = "CO2_concentration")),
           actionButton(ns("plot"), "Plot"),
           plotlyOutput(ns("graph")))
}

graph_server <- function(id, r6, i) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (r6$save == TRUE) {
        filtered_output <-
          filter(r6$output[[r6$i()]], variable == input$variable)
        
        output$graph <- renderPlotly({
          plot_ly(
            filtered_output,
            x = ~ year,
            y = ~ value,
            type = 'scatter',
            mode = 'lines',
            hovertemplate = paste(
              "<b>Year:</b> %{x}<br>",
              "<b>Value:</b> %{y:.2f}",
              "<extra></extra>"
            )
          ) %>%
            layout(
              xaxis = list(title = "Year"),
              yaxis = list(title = "Global Temperature (C)"),
              title = "Global Temperature at Surface"
            )
        })
      }
      if (r6$save == FALSE) {
        filtered_output <-
          filter(r6$no_save, variable == input$variable)
        
        output$graph <- renderPlotly({
          plot_ly(
            filtered_output,
            x = ~ year,
            y = ~ value,
            type = 'scatter',
            mode = 'lines',
            hovertemplate = paste(
              "<b>Year:</b> %{x}<br>",
              "<b>Value:</b> %{y:.2f}",
              "<extra></extra>"
            )
          ) %>%
            layout(
              xaxis = list(title = "Year"),
              yaxis = list(title = input$variable),
              title = input$variable
            )
        })
      }
    }) %>%
      bindEvent(input$plot, ignoreNULL = FALSE, ignoreInit = FALSE)
  })
}