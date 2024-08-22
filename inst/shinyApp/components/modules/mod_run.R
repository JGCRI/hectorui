# Run Hector using R6 module

run_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        sidebarPanel(id = "params",
                     chooseSliderSkin(skin = "Flat", color = "#375a7f"),
                     pickerInput(
                         inputId = ns("ssp_path"),
                         label = "Select SSPs:",
                         choices = scenarios,
                         multiple = TRUE,
                         selected = "input/hector_ssp245.ini"),
                     sliderInput(ns("time"), label="Select dates:",
                                 min = 1750, max = 2300, value = c(1900,2100), sep="", width = "90%", step=5),
                     h5("Include permafrost thaw:", id = "perm-lab"),
                     switchInput(ns("permafrost"), "Permafrost", value = TRUE, size = 'small', onStatus = "danger"),
                     h5("Model Parameters"),
                     sliderInput(ns("alpha"), label="Aerosol forcing scaling factor", # AERO_SCALE()
                                 value = 1,
                                 min = 0.01, max = 1, width = "90%"),
                     sliderInput(ns("beta"), label="CO2 fertilization factor", # BETA()
                                 value = 0.53,
                                 min = 0.01, max = 4, step=0.01, width = "90%"),
                     sliderInput(ns("diff"), label="Ocean heat diffusivity", # DIFFUSIVITY()
                                 value = 2.38,
                                 min = 0, max = 5, step=0.1, post = " cm2/s", width = "90%"),
                     sliderInput(ns("S"), label="Equilibrium climate sensitivity", # ECS()
                                 value = 3,
                                 min = 1, max = 6, step=0.1, post = " °C", width = "90%"),
                     sliderInput(ns("q10_rh"), label="Heterotrophic temperature sensitivity", # Q10_RH()
                                 value = 1.76,
                                 min = 1, max = 5, step=0.1, width = "90%"),
                     sliderInput(ns("volscl"), label="Volcanic forcing scaling factor", # VOLCANIC_SCALE()
                                 value = 1,
                                 min = 0, max = 1, width = "90%"),
                     bsPopover(ns("alpha"), title="", content = "Decreasing this means aerosols exert less radiative forcing",
                               placement = "top", trigger = "hover", options = NULL),
                     bsPopover(ns("beta"), title="", content = "Increasing this means vegetation grows faster as CO2 increases",
                               placement = "top", trigger = "hover", options = NULL),
                     bsPopover(ns("diff"), title="", content = "Increasing this means heat moves deeper into the ocean quicker",
                               placement = "top", trigger = "hover", options = NULL),
                     bsPopover(ns("S"), title="", content = "Increasing this means a larger temperature rise as CO2 increases",
                               placement = "top", trigger = "hover", options = NULL),
                     bsPopover(ns("q10_rh"), title="", content = "Increasing this means soil microbes respire faster as temperature rises",
                               placement = "top", trigger = "hover", options = NULL),
                     bsPopover(ns("volscl"), title="", content = "Decreasing this means that volcanic eruptions exert less radiative forcing",
                               placement = "top", trigger = "hover", options = NULL)
        ),
        mainPanel(
            fluidRow(
                actionButton(ns("run"), label="Load Graph", width = '150px', style = "background: #0B3F8F; color: white;"),
                downloadButton(ns("downloadData"), label="Download Data", style = "background: #B8B8B8; color: black;"),
                downloadButton(ns("downloadParam"), label="Download Parameters", style = "background: #B8B8B8; color: black;")
            ),
            fluidRow(
                br(),
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
                            selected = "Atmospheric CO2", multiple = FALSE)
            ),
            fluidRow(
                br(),
                plotlyOutput(ns("graph"))
            )
        )
    )

}

run_server <- function(id, r6) {
    moduleServer(id, function(input, output, session) {

        observe({

            r6$run_mode <- "regular"
            runs <- list()
            cores <- list()

            for(i in 1:length(input$ssp_path)) {

                r6$selected_var <- reactive({input$variable})
                r6$run_name <- reactive({input$run_name})
                r6$ini_file <- reactive({system.file(input$ssp_path[i],package="hector")})
                r6$time <- reactive({input$time})

                withProgress(message = paste("Running Hector", names(which(scenarios == input$ssp_path[i], arr.ind = FALSE)), "...\n"), value = 1/2, {
                    print("Running...") # in command line
                    core <- reactive({newcore(r6$ini_file())}) # create core

                    # Set parameters using inputs (function to only call setvar once in final version)
                    if (input$permafrost == TRUE) {
                        r6$permafrost <- "On"
                    } else if (input$permafrost == FALSE) {
                        setvar(core(),0,PERMAFROST_C(),0,"Pg C")
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

                    cores[[i]] <- core()
                    incProgress(1/1, detail = paste("Load complete."))
                    Sys.sleep(0.2)
                })
#put cores in a list and fetchvars outsite
            }

            r6$core <- cores

            for(i in 1:length(input$ssp_path)) {
                runs[[i]] <- fetchvars(r6$core[[i]], r6$time()[1]:r6$time()[2], vars = list(r6$selected_var())) %>%
                    mutate(Scenario = names(which(scenarios == input$ssp_path[i], arr.ind = FALSE)))
            }
            #browser()

            r6$output <- bind_rows(runs)
            print("Done")

            output$graph <- renderPlotly({
                graph_plots(r6 = r6)
            })

        }) %>%
            bindEvent(input$run, ignoreNULL = FALSE, ignoreInit = FALSE)

        observe({
            r6$selected_var <- reactive({input$variable})
            runs <- list(r6$output)

            for(i in 1:length(input$ssp_path)) {
                runs[[i]] <- fetchvars(r6$core[[i]], r6$time()[1]:r6$time()[2], vars = list(r6$selected_var())) %>%
                    mutate(Scenario = names(which(scenarios == input$ssp_path[i], arr.ind = FALSE)))
            }

            r6$output <- bind_rows(runs)

            output$graph <- renderPlotly({
                graph_plots(r6 = r6)
            })
        }) %>%
            bindEvent(input$variable, ignoreInit = TRUE)

            # Download handler for downloading the raw data output from a Hector run. This is activated upon button click.
            output$downloadData <- downloadHandler(
                filename = function()
                {
                    paste0('HectorUI_Output_', format(Sys.time(), "%Y-%m-%d_%H%M%S"), '.csv')
                },

                content = function(file)
                {

                    if(!is.null(r6$output))
                    {
                        write.csv(as.data.frame(r6$output), file, row.names = FALSE)
                    }
                    else
                    {
                        shinyalert::shinyalert("No active Hector cores", "Please set at least one of the SSP scenarios to active.", type = "warning")
                    }
                }
            )
            outputOptions(output, "downloadData", suspendWhenHidden = FALSE)

            output$downloadParam <- downloadHandler(
                filename = function()
                {
                    paste0('HectorUI_Parameters_', format(Sys.time(), "%Y-%m-%d_%H%M%S"), '.txt')
                },

                content = function(file)
                {

                    if(!is.null(r6$output))
                    {
                        ssp_names <- list()
                        for(i in 1:length(input$ssp_path)) {
                            ssp_names[[i]] <- names(which(scenarios == input$ssp_path[i], arr.ind = FALSE))
                        }
                        names <- paste(unlist(ssp_names), collapse = ', ')

                        header_text <- paste0("File created with Hector UI - https://github.com/JGCRI/hector-ui accessed on ", format(Sys.time(), "%Y-%m-%d %X"), "\n",
                                             "Hector Version: ", packageVersion("hector"), "\n",
                                             "SSPs Used: ", names, "\n",
                                             "Permafrost: ", r6$permafrost, "\n",
                                             "Model Parameters: " , input$input_paramToggle , "\n",
                                             "Alpha: ", input$alpha,  "\n",
                                             "Beta: ", input$beta, "\n",
                                             "Diff: ", input$diff, "\n",
                                             "ECS: ", input$S, "\n",
                                             "Q10: ", input$q10_rh, "\n",
                                             "Volc: ", input$volscl)

                        writeLines(header_text, file)
                    }
                    else
                    {
                        shinyalert::shinyalert("No active Hector cores", "Upload a custom emissions scenario before downloading.", type = "warning")
                    }
                }
            )
            outputOptions(output, "downloadParam", suspendWhenHidden = FALSE)

    })
}
