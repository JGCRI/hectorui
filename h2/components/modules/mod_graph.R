# Run Hector given SSP input, start and end years for model run
# Plot function

# Default values set for SSP, start/end years, and selected var for plot

graph_ui <- function(id) {
  ns <- NS(id)
  fluidRow(selectInput(ns("variable"), "Select a variable to plot:",
                       list("Carbon Cycle" = list("Atmospheric CO2" = CONCENTRATIONS_CO2(),
                                                  "FFI Emissions" = FFI_EMISSIONS(),
                                                  "LUC Emissions" = LUC_EMISSIONS()),
                            "Concentrations" = list("N2O Concentration" = CONCENTRATIONS_N2O()),
                            "Emissions" = list("Black Carbon Emissions" = EMISSIONS_BC(),
                                               "Organic Carbon Emissions" = EMISSIONS_OC()),
                            "Forcings" = list("RF - Total" = RF_TOTAL(),
                                              "RF - Albedo" = RF_ALBEDO(),
                                              "RF - CO2" = RF_CO2(),
                                              "RF - N2O" = RF_N2O(),
                                              "RF - Black Carbon" = RF_BC(),
                                              "RF - Organic Carbon" = RF_OC(),
                                              "RF - Total SO2" = RF_SO2(),
                                              "RF - Volcanic Activity" = RF_VOL(),
                                              "RF - CH4" = RF_CH4()))),
           # selectInput(ns("variable"), "Select a variable to plot:",
           #             c("Global Mean Temperature" = "global_tas",
           #               "Atmospheric CO2" = "CO2_concentration",
           #               "RF - Total" = "RF_tot",
           #               "RF - CO2" = "RF_CO2",
           #               "Atmospheric N2O" = CONCENTRATIONS_N2O()),
           #             selected = "global_tas"),
           # other variables can be found from the fetchvars help page
           actionButton(ns("plot"), "Plot"),
           plotlyOutput(ns("graph")))
}

graph_server <- function(id, r6) {
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
        r6$selected_var <- reactive({input$variable})
        
        filtered_output <-
          filter(r6$no_save, variable == r6$selected_var())
        #browser()
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
              yaxis = list(title = r6$selected_var()),
              title = input$variable
            )
        })
      }
    }) %>%
      bindEvent(input$plot, ignoreNULL = TRUE, ignoreInit = FALSE)
  })
}