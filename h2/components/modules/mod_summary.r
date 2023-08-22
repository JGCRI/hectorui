# Run Hector given SSP input, start and end years for model run
# Print function

# Default values set for SSP, start/end years, and selected var for plot

summary_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    actionButton(ns("print"),"Print"),
    tableOutput(ns("summary"))
  )
}

summary_server <- function(id,r6) {
  moduleServer(id, function(input, output, session) {
    observe({
      output$summary <- renderTable({r6$output})
    }) %>%
      bindEvent(input$print) # run when Print button is clicked
    
  })
}