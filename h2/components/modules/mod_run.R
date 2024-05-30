# Run Hector using R6 module

run_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        sidebarPanel(
            tabsetPanel(
                tabPanel(class = "params", "Standard Scenarios",
                         chooseSliderSkin(skin = "Flat", color = "#375a7f"),
                         selectInput(ns("ssp_path"), label="Select SSP:",
                                     choices = scenarios,
                                     selected = "input/hector_ssp245.ini"),
                         sliderInput(ns("time"), label="Select dates:",
                                     min = 1750, max = 2300, value = c(1900,2100), sep="", width = "90%", step=5),
                         materialSwitch(ns("permafrost"), "Include Permafrost Carbon", value = FALSE),
                         h5("Model Parameters"),
                         sliderInput(ns("alpha"), label="Aerosol forcing scaling factor", # AERO_SCALE()
                                     min = 0.01, max = 1, value = 1, width = "90%"),
                         sliderInput(ns("beta"), label="CO2 fertilization factor", # BETA()
                                     min = 0.01, max = 4, value = 0.55, step=0.01, width = "90%"),
                         sliderInput(ns("diff"), label="Ocean heat diffusivity", # DIFFUSIVITY()
                                     min = 0, max = 5, value = 1.16, step=0.1, post = " cm2/s", width = "90%"),
                         sliderInput(ns("S"), label="Equilibrium climate sensitivity", # ECS()
                                     min = 1, max = 6, value = 3, step=0.1, post = " °C", width = "90%"),
                         sliderInput(ns("q10_rh"), label="Heterotrophic temperature sensitivity", # Q10_RH()
                                     min = 1, max = 5, value = 2.1, step=0.1, width = "90%"),
                         sliderInput(ns("volscl"), label="Volcanic forcing scaling factor", # VOLCANIC_SCALE()
                                     min = 0, max = 1, value = 1, width = "90%")
                )
            )
        ),
        mainPanel(
                  fluidRow(
                      column(8,
                             selectInput(ns("variable"), "Output Variable:",
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
                                         selected = "Atmospheric CO2", multiple = FALSE, width = '100%'),
                      )
                  ),
                  fluidRow(
                      actionButton(ns("run"), label="Load Graphs", width = '200px', style = "background: #0B3F8F; color: white;"),
                      downloadButton(ns("downloadData"), label="Download Raw Data", style = "background: #0B3F8F; color: white;")
                  ),
                  fluidRow(
                      br(),
                      withSpinner(plotlyOutput(ns("graph")))
                  )
        )
    )

}

run_server <- function(id, r6) {
    moduleServer(id, function(input, output, session) {

        observe({

            runs <- list()

            for(i in 1:length(input$ssp_path)) {

                r6$selected_var <- reactive({input$variable})
                r6$run_name <- reactive({input$run_name})
                r6$ini_file <- reactive({system.file(input$ssp_path[i],package="hector")})
                r6$time <- reactive({input$time})

                print("Running...") # in command line
                core <- reactive({newcore(r6$ini_file())}) # create core

                # Set parameters using inputs (function to only call setvar once in final version)
                if (input$permafrost == TRUE) {
                    setvar(core(),0,PERMAFROST_C(),865,"Pg C")
                    r6$permafrost <- "On"
                } else if (input$permafrost == FALSE) {
                    r6$permafrost <- "Off"
                }
                setvar(core(),NA,AERO_SCALE(),input$alpha,"(unitless)")
                setvar(core(),NA,BETA(),input$beta,"(unitless)")
                setvar(core(),NA,DIFFUSIVITY(),input$diff,"cm2/s")
                setvar(core(),NA,ECS(),input$S,"degC")
                setvar(core(),NA,Q10_RH(),input$q10_rh,"(unitless)")
                setvar(core(),NA,VOLCANIC_SCALE(),input$volscl,"(unitless)")

                reset(core())
                run(core())

                runs[[i]] <- fetchvars(core(), r6$time()[1]:r6$time()[2], vars = list(r6$selected_var())) %>%
                    mutate(Scenario = names(which(scenarios == input$ssp_path[i], arr.ind = FALSE)))
            }

            r6$output <- bind_rows(runs)
            print("Done")

        }) %>%
            bindEvent(input$run, ignoreNULL = FALSE, ignoreInit = FALSE)

        observe({

            output$graph <- renderPlotly({
                graph_plots(r6 = r6)
            })
            }) %>%
            bindEvent(input$run, ignoreNULL = TRUE, ignoreInit = FALSE)

        #observe({

            # Download handler for downloading the raw data output from a Hector run. This is activated upon button click.
            output$downloadData <- downloadHandler(
                filename = function()
                {
                    paste0('HectorUI_Output_', Sys.Date(), '.csv')
                },

                content = function(file)
                {
                    browser()
                    if(!is.null(r6$output))
                    {

                        header_text <- paste("File created with Hector UI - https://github.com/JGCRI/hector-ui\n" ,
                                             "Model Parameters: " , input$input_paramToggle , "\n",
                                             "Alpha:,", input$alpha, ",Beta:,", input$beta, ",Diff:,", input$diff,
                                             ",ECS:,", input$S, ",Q10:,", input$q10_rh, ",Volc:,", input$volscl,
                                             "\n")

                        cat(header_text, file = file)

                    }
                    else
                    {
                        shinyalert::shinyalert("No active Hector cores", "Please set at least one of the SSP scenarios to active or upload a custom emissions scenario before downloading.", type = "warning")
                    }
                }
            )

            outputOptions(output, "downloadData", suspendWhenHidden = FALSE)

        #}) %>%
        #    bindEvent(input$downloadData, ignoreNULL = TRUE, ignoreInit = FALSE)

    })
}
