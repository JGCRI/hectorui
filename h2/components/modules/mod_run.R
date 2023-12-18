# Run Hector using R6 module

run_ui <- function(id) {
    ns <- NS(id)
    fluidRow(
        sidebarPanel(
            tabsetPanel(
                tabPanel(class = "params", "Standard Scenarios",
                         chooseSliderSkin(skin = "Flat", color = "#375a7f"),
                         selectInput(ns("ssp_path"), label="Select SSP:",
                                     choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                                                    "SSP 1-2.6"="input/hector_ssp126.ini",
                                                    "SSP 2-4.5"="input/hector_ssp245.ini",
                                                    "SSP 3-7.0"="input/hector_ssp370.ini",
                                                    "SSP 4-3.4"="input/hector_ssp434.ini",
                                                    "SSP 4-6.0"="input/hector_ssp460.ini",
                                                    "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                                                    "SSP 5-8.5"="input/hector_ssp585.ini"),
                                     selected = "input/hector_ssp245.ini"),
                         sliderInput(ns("time"), label="Select dates:",
                                     min = 1750, max = 2300, value = c(1900,2100), sep="", width = "90%", step=5),
                         h5("Model Parameters"),
                         sliderInput(ns("alpha"), label="Aerosol forcing scaling factor", # AERO_SCALE()
                                     min = 0.01, max = 1, value = 1, width = "90%"),
                         sliderInput(ns("beta"), label="CO2 fertilization factor", # BETA()
                                     min = 0.01, max = 4, value = 0.36, step=0.01, width = "90%"),
                         sliderInput(ns("diff"), label="Ocean heat diffusivity", # DIFFUSIVITY()
                                     min = 0, max = 5, value = 2.3, step=0.1, post = " cm2/s", width = "90%"),
                         sliderInput(ns("S"), label="Equilibrium climate sensitivity", # ECS()
                                     min = 1, max = 6, value = 3, step=0.1, post = " °C", width = "90%"),
                         sliderInput(ns("q10_rh"), label="Heterotrophic temperature sensitivity", # Q10_RH()
                                     min = 1, max = 5, value = 2, step=0.1, width = "90%"),
                         sliderInput(ns("volscl"), label="Volcanic forcing scaling factor", # VOLCANIC_SCALE()
                                     min = 0, max = 1, value = 1, width = "90%")
                )
            )

        ),
        mainPanel(width = 8,
                  fluidRow(
                      column(4,
                             selectInput(ns("variable"), "Choose Output Variable:",
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
                                         selected = "Atmospheric CO2", multiple = FALSE),
                      )
                  ),
                  fluidRow(
                      column(2,
                             actionBttn(ns("run"),"Run", color = "primary")
                             ),
                      column(3,
                             materialSwitch(ns("savetoggle"),"Save Run", value = FALSE)
                             ),
                      column(5,
                             textInput(ns("run_name"), label = "Run Name", placeholder = "Run 1")
                             ),
                      column(2,
                             dropdownButton(inputId = ns("dropdown"),
                                            icon = icon("gear"),
                                            circle = TRUE,
                                            status = "primary",
                                            dataTableOutput(ns("savetable")),
                                            actionButton(ns("deleteRuns"), "Delete Selected")
                             )
                             )
                  ),
                  fluidRow(
                      withSpinner(plotlyOutput(ns("graph")))
                  )
        )
    )

}

run_server <- function(id, r6) {
    moduleServer(id, function(input, output, session) {

        observe({

            if (input$savetoggle == TRUE) {

                r6$save <- TRUE

            } else {

                r6$save <- FALSE

            }

            r6$selected_var <- reactive({input$variable})
            r6$run_name <- reactive({input$run_name})
            r6$ini_file <- reactive({system.file(input$ssp_path,package="hector")})
            r6$time <- reactive({input$time})

            print("Running...") # in command line
            core <- reactive({newcore(r6$ini_file())}) # create core

            # Set parameters using inputs (function to only call setvar once in final version)
            setvar(core(),NA,AERO_SCALE(),input$alpha,"(unitless)")
            setvar(core(),NA,BETA(),input$beta,"(unitless)")
            setvar(core(),NA,DIFFUSIVITY(),input$diff,"cm2/s")
            setvar(core(),NA,ECS(),input$S,"degC")
            setvar(core(),NA,Q10_RH(),input$q10_rh,"(unitless)")
            setvar(core(),NA,VOLCANIC_SCALE(),input$volscl,"(unitless)")

            reset(core())
            run(core())

            #browser()
            if (r6$save == TRUE) {

                r6$output[[r6$run_name()]] <- fetchvars(core(), r6$time()[1]:r6$time()[2], vars = list(r6$selected_var())) %>%
                    mutate(run = r6$run_name(), Scenario = input$ssp_path)

            } else if (r6$save == FALSE) {

                r6$no_save_output <- fetchvars(core(), r6$time()[1]:r6$time()[2], vars = list(r6$selected_var())) %>%
                    mutate(ssp = input$ssp_path)

            }

            print("Done")

        }) %>%
            bindEvent(input$run, ignoreNULL = FALSE, ignoreInit = FALSE)

        observe({

            output$graph <- renderPlotly({
                graph_plots(r6 = r6)
            })
            }) %>%
            bindEvent(input$run, ignoreNULL = TRUE, ignoreInit = FALSE)

    })
}

# might be worth it to just run the core with all selectable variables. how much time would that add?
# issue seems to be that mod_run goes first, so input$variable just doesn't exist yet... maybe having
# that module containing all choices is a good idea

# fetchvars(core,1745:2300,vars=list(CONCENTRATIONS_CO2(),FFI_EMISSIONS(),LUC_EMISSIONS(),CONCENTRATIONS_N2O,EMISSIONS_BC(),EMISSIONS_OC(),RF_TOTAL(),RF_ALBEDO(),RF_N2O(),RF_CO2(),RF_BC(),RF_OC(),RF_SO2(),RF_CH4(),RF_VOL()))
