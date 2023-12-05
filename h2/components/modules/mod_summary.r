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
      if (r6$save == TRUE) {
        hectoroutput <- r6$output[[r6$i()]]
        output$summary <- renderDT({datatable(hectoroutput)})
      }
      if (r6$save == FALSE) {
        hectoroutput <- r6$no_save
        #filtered_output <-
        #  filter(r6$no_save, variable == r6$selected_var())
        
        output$summary <- renderDT({datatable(hectoroutput)})
      }
    }) %>%
      bindEvent(input$print, ignoreNULL = TRUE, ignoreInit = FALSE) # run when Print button is clicked
    
  })
}