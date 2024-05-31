# Run the model with custom emissions
custom_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        sidebarPanel(
            h5("Custom Emissions Pathway"),
            tags$hr(class="hrNav"),
            p("Steps to run your own scenario with custom emissions:"),
            tags$ol(
                tags$li("Choose a baseline SSP scenario as your starting point"),
                tags$li("Give your new custom scenario a name"),
                tags$li("Download the emissions file template for that scenario and enter your own emissions"),
                tags$li("Upload the new customized emissions file")
            ),
            p(tags$strong("Do not edit any field names or change the CSV file in any way other than changing the data")),
            tags$table(
                tags$tr(width = "100%",
                        tags$td(width = "145", "Baseline Scenario:"),
                        tags$td(width = "155", selectInput(ns("input_custom_SSP"), label = NULL,
                                                           choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                                                                          "SSP 1-2.6"="input/hector_ssp126.ini",
                                                                          "SSP 2-4.5"="input/hector_ssp245.ini",
                                                                          "SSP 3-7.0"="input/hector_ssp370.ini",
                                                                          "SSP 4-3.4"="input/hector_ssp434.ini",
                                                                          "SSP 4-6.0"="input/hector_ssp460.ini",
                                                                          "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                                                                          "SSP 5-8.5"="input/hector_ssp585.ini"),
                                                           width=150, selected = "SSP 2-4.5"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "145", "Output Variable:"),
                        tags$td(width = "155", selectInput(ns("variable"), label = NULL,
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
                                                                                  "RF - CH4" = RF_CH4())),
                                                           selected = "Atmospheric CO2", multiple = FALSE)

                        )
                ),
                tags$tr(width = "100%",
                        tags$td(width = "145", "Your Scenario Name:"),
                        tags$td(width = "200",  textInput(ns("input_custom_scenarioName"), label = NULL, value = ""))
                )
            ),
            div(
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp119.ini'",
                    a(h6("Download SSP 1-1.9 Emissions File Template"),
                      href="inputs/ssp119_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp126.ini'",
                    a(h6("Download SSP 1-2.6 Emissions File Template"),
                      href="inputs/ssp126_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp245.ini'",
                    a(h6("Download SSP 2-4.5 Emissions File Template"),
                      href="inputs/ssp245_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp370.ini'",
                    a(h6("Download SSP 3-7.0 Emissions File Template"),
                      href="inputs/ssp370_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp434.ini'",
                    a(h6("Download SSP 4-3.4 Emissions File Template"),
                      href="inputs/ssp434_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp460.ini'",
                    a(h6("Download SSP 4-6.0 Emissions File Template"),
                      href="inputs/ssp460_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp534-over.ini'",
                    a(h6("Download SSP 5-3.4OS Emissions File Template"),
                      href="inputs/ssp534-over_emissions.csv"),
                    ns=NS(id)
                ),
                conditionalPanel(
                    condition = "input.input_custom_SSP == 'input/hector_ssp585.ini'",
                    a(h6("Download SSP 5-8.5 Emissions File Template"),
                      href="inputs/ssp585_emissions.csv"),
                    ns=NS(id)
                ),
                fileInput(ns("input_custom_emissions_file"), "Upload Custom Emissions File:", width=275,
                          buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain"))
            )
        ),
        mainPanel(
            fluidRow(
                actionButton(ns("input_load_emissions"), label="Create Scenario", width = '150px', style = "background: #0B3F8F; color: white;"),
                downloadButton(ns("downloadData"), label="Download Data", style = "background: #B8B8B8; color: black;")
            ),
            fluidRow(
                plotlyOutput(ns("graph"))
            )
        )
    )
}

custom_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    observe({

      # Require all inputs to exist
      if (is.null(input$input_custom_emissions_file) | (is.na(input$input_custom_scenarioName) | is.null(input$input_custom_scenarioName) | (input$input_custom_scenarioName == "")))
      {
        shinyalert::shinyalert("Missing Information", "Please name the scenario and load an emissions file before attempting to load the scenario.", type = "warning")
        return(NULL)
      }
      r6$run_name <- input$input_custom_scenarioName

      # Read in input data
      inifile <- system.file(input$input_custom_SSP,package="hector")
      emifile <- input$input_custom_emissions_file
      emissions_data <- read.csv(file=emifile$datapath, header=TRUE, sep=",", skip = 5)
      emissions_headers <- read.csv(file=emifile$datapath, header=FALSE, sep=",", skip = 4)
      dates_col <- emissions_data$Date
      r6$selected_var <- input$variable

      withProgress(message = paste('Creating Custom Scenario ', r6$run_name, "...\n"), value = 1/2, {
        core <- newcore(inifile, suppresslogging=TRUE, name=r6$run_name)
        run(core)
        incProgress(1/1, detail = paste("Load complete."))
        Sys.sleep(0.2)
      })

      # get data from base SSP run
      base_output <- fetchvars(core, 1745:2300, vars = list(r6$selected_var)) %>%
        mutate(run = names(which(scenarios == input$input_custom_SSP, arr.ind = FALSE)),
                           Scenario = names(which(scenarios == input$input_custom_SSP, arr.ind = FALSE)))

      # set vars and rerun core
      for(i in c(2:ncol(emissions_data))) {
        setvar(core = core, dates = emissions_data[, 1],var = colnames(emissions_data)[i], values = emissions_data[, i], unit = as.character(emissions_headers[[paste0("V",i)]][[1]]))
      }

      reset(core)
      run(core)

      # get custom output
      custom_output <- fetchvars(core, 1745:2300, vars = list(r6$selected_var)) %>%
        mutate(run = r6$run_name, Scenario = names(which(scenarios == input$input_custom_SSP, arr.ind = FALSE)))
      r6$output <- bind_rows(list(base_output,custom_output))

      # Plot
      # replace following with graph plots function in future
      output$graph <- renderPlotly({
        ggplot(r6$output) +
          geom_line(aes(x = year, y = value, color = run)) +
          labs(x = "Year", y = last(r6$output)$variable[1],
               title = paste0("Variable: ", last(r6$output)$variable[1])) +
          theme(legend.position = "bottom")
      })

    }) %>% bindEvent(input$input_load_emissions)

      output$downloadData <- downloadHandler(
          filename = function()
          {
              paste0('HectorUI_CustomEmiss_', r6$run_name, "_", Sys.Date(), '.csv')
          },

          content = function(file)
          {

              if(!is.null(r6$output))
              {
                  write.csv(as.data.frame(r6$output), file, row.names = FALSE)
              }
              else
              {
                  shinyalert::shinyalert("No active Hector cores", "Upload a custom emissions scenario before downloading.", type = "warning")
              }
          }
      )
      outputOptions(output, "downloadData", suspendWhenHidden = FALSE)

  })
}
