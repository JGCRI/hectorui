# Run Hector given SSP input, start and end years for model run
# Print function

# Default values set for SSP, start/end years, and selected var for plot

summary_ui <- function(id) {
  ns <- NS(id)
  fluidRow(actionButton(ns("print"), "Print"),
           tableOutput(ns("summary")))
}

summary_server <- function(id, r6, i) {
  moduleServer(id, function(input, output, session) {
    observe({
      hectoroutput <-
        r6$output[[i() - 1]] #i increases at end of mod_run so output is i-1
      output$summary <- renderTable({
        hectoroutput
      })
    }) %>%
      bindEvent(input$print) # run when Print button is clicked
    
  })
}