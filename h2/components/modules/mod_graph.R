# Run Hector given SSP input, start and end years for model run
# Plot function

# Default values set for SSP, start/end years, and selected var for plot

graph_ui <- function(id) {
  ns <- NS(id)

  fluidRow(selectInput(ns("variable"), "Select a variable to plot:",
                       list("Carbon Cycle" = list("Atmospheric CO2" = CONCENTRATIONS_CO2(),
                                                  "Atmospheric Carbon Pool" = ATMOSPHERIC_CO2(), # i think this is the right var?
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
                                              "RF - CH4" = RF_CH4()),
                            "Halocarbon Forcings" = list("CF4 Forcing"="RF_CF4", #function doesn't give the correct output?
                                                         "C2F6 Forcing"="RF_C2F6", 
                                                         "HFC-23 Forcing"="RF_HFC23", 
                                                         "HFC-4310 Forcing"="RF_HFC4310", 
                                                         "HFC-125 Forcing"="RF_HFC125",  
                                                         "HFC-143a Forcing"="RF_HFC143a",
                                                         "HFC-245fa Forcing"="RF_HFC245fa", 
                                                         "SF6 Forcing"="RF_SF6", 
                                                         "CFC-11 Forcing"="RF_CFC11", 
                                                         "CFC-12 Forcing"="RF_CFC12", 
                                                         "CFC-113 Forcing"="RF_CFC113",
                                                         "CFC-114 Forcing"="RF_CFC114",
                                                         "CFC-115 Forcing"="RF_CFC115",
                                                         "CCl4 Forcing"="RF_CCl4", 
                                                         "CH3CCl3 Forcing"="RF_CH3CCl3",
                                                         "Halon-1211 Forcing"="RF_halon1211",
                                                         "Halon-1301 Forcing"="RF_halon1301", 
                                                         "Halon-2402 Forcing"="RF_halon2402", 
                                                         "CH3Cl Forcing"="RF_CH3Cl", 
                                                         "CH3Br Forcing"="RF_CH3Br"),
                            "Methane" = list("Atmospheric CH4" = CONCENTRATIONS_CH4(),
                                             "CH4 Emissions" = EMISSIONS_CH4()),
                            "SO2" = list("Anthropogenic SO2"=EMISSIONS_SO2(),
                                         "Volcanic SO2"=VOLCANIC_SO2()),
                            "Temperature" = list("Global Mean Temp" = GLOBAL_TAS(),
                                                 "Equilibrium Global Temp" = GMST(), # i think?
                                                 "Ocean Surface Temp" = SST(),
                                                 "Ocean Air Temp" = OCEAN_TAS(),
                                                 "Heat Flux - Mixed Layer Ocean" = FLUX_MIXED(),
                                                 "Heat Flux - Interior Layer Ocean" = FLUX_INTERIOR(),
                                                 "Total Heat Flux - Ocean" = HEAT_FLUX()))),
           # other variables can be found from the fetchvars help page
  column(3,
           actionButton(ns("plot"), "Plot"),
           plotlyOutput(ns("graph"))
  )
           #)
}

graph_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    observe({

      if (r6$save == TRUE) {
        
        # Get labels given input
        key <- reactive({input$variable})
        title <- title[[key()]]
        ylabel <- units[[key()]]
        
        # Filter data for selected variable
        filtered_output <-
          filter(r6$output[[r6$run_name()]], variable == r6$selected_var())

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
              yaxis = list(title = ylabel),
              title = title
            )
        })
      }
      
      if (r6$save == FALSE) {
        r6$selected_var <- reactive({input$variable})
        
        # Get labels given input
        key <- reactive({input$variable})
        title <- title[[key()]]
        ylabel <- units[[key()]]
        
        # Filter data for selected variable
        filtered_output <-
          filter(r6$no_save, variable == r6$selected_var())

        output$graph <- renderPlotly({
          plot_ly(
            r6$output,
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
              yaxis = list(title = ylabel),
              title = title
            )
        })
      }
    }) %>%
      bindEvent(input$run, ignoreNULL = TRUE, ignoreInit = TRUE)
  })
}

# add reset variables button
