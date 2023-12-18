# Run Hector given SSP input, start and end years for model run
# Print function

# Default values set for SSP, start/end years, and selected var for plot

summary_ui <- function(id) {
  ns <- NS(id)
  fluidRow(actionButton(ns("print"), "Print"),
           DTOutput(ns("summary")))
}

summary_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    observe({
        browser()
      if (r6$save == TRUE) {
        hectoroutput <- r6$output[[r6$run_name()]]
        output$summary <- renderDT({datatable(hectoroutput)})
      }
      if (r6$save == FALSE) {
        hectoroutput <- r6$output
        output$summary <- renderDT({datatable(hectoroutput)})
      }
    }) %>%
      bindEvent(input$print, ignoreNULL = TRUE, ignoreInit = FALSE) # run when Print button is clicked

  })
}
