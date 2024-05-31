
choices_ui <- function(x) {

    tabsetPanel(
        tabPanel(class = "params",
                 "Standard Scenarios",
                 h5("Representative Concentration Pathways (RCPs)"),
                 shinyWidgets::awesomeCheckboxGroup(inputId = "input_RCP", label = "",
                                                    choices = c("2.6", "4.5", "6.0", "8.5"), inline = TRUE, status = "info"),
                 # shinyWidgets::prettyCheckbox(inputId = (NS(x,"input_RCP_2.6")), label = "2.6", value = FALSE, width = 45,
                 #                              inline = TRUE, icon = icon("check"), animation = "jelly", status = "info"),
                 # shinyWidgets::prettyCheckbox(inputId = (NS(x,"input_RCP_4.5")), label = "4.5", value = TRUE, width = 45,
                 #                              inline = TRUE, icon = icon("check"), animation = "jelly", status = "success"),
                 # shinyWidgets::prettyCheckbox(inputId = (NS(x,"input_RCP_6.0")), label = "6.0", value = FALSE, width = 45,
                 #                              inline = TRUE, icon = icon("check"), animation = "jelly", status = "warning"),
                 # shinyWidgets::prettyCheckbox(inputId = (NS(x,"input_RCP_8.5")), label = "8.5", value = FALSE, width = 45,
                 #                              inline = TRUE, icon = icon("check"), animation = "jelly", status = "danger"),
                 h5("Model Parameters"),
                 fluidRow(
                     column(6,
                            selectInput("input_paramToggle", label = NULL,
                                        choices =  list("Hector Default" = "default", "CanESM2" = "canesm2",
                                                        "CESM1-BGC" = "cesm1-bgc", "GFDL-ESM2G" = "gfdl-esm2g",
                                                        "MIROC-ESM" = "miroc-esm", "MPI-ESM-LR" = "mpi-esm-lr",
                                                        "MRI-ESM1" = "mri-esm1"))
                            ),
                 ),
                 fluidRow(
                     column(12,
                            sliderInput(NS(x, "input_aero"), "Aerosol forcing scaling factor", min = 0, max = 1, value = 1, width = "90%"),
                            sliderInput(NS(x,"input_beta"), "CO2 fertilization factor", min = 0, max = 4, value = 0.36, step = 0.01, width = "90%"),
                            sliderInput(NS(x,"input_diff"), "Ocean heat diffusivity", min = 0, max = 5, value = 2.3, step = 0.1, post = " cm2/s", width = "90%"),
                            sliderInput(NS(x,"input_ecs"), "Equilibrium climate sensitivity", min = 1, max = 6, value = 3, step = 0.1, post = " °C", width = "90%"),
                            sliderInput(NS(x,"input_q10"), "Heterotrophic Temperature Sensitivity", min = 1, max = 5, value = 2, step = 0.1, width = "90%"),
                            sliderInput(NS(x,"input_volc"),"Volcanic forcing scaling factor", min = 0, max = 1, value = 1, width = "90%"),

                            # Add hover popups with parameter descriptions
                            bsPopover("input_aero", "Increasing this means aerosols exert a stronger radiative forcing",
                                      placement = "top", trigger = "hover", options = NULL),
                            bsPopover("input_beta", "Increasing this means vegetation grows faster as CO2 increases",
                                      placement = "top", trigger = "hover", options = NULL),
                            bsPopover("input_diff", "Increasing this means heat moves deeper into the ocean quicker",
                                      placement = "top", trigger = "hover", options = NULL),
                            bsPopover("input_ecs", "Increasing this means a larger temperature rise as CO2 increases",
                                      placement = "top", trigger = "hover", options = NULL),
                            bsPopover("input_q10", "Increasing this means soil microbes respire faster as temperature rises",
                                      placement = "top", trigger = "hover", options = NULL),
                            bsPopover("input_volc", "Increasing this means that volcanic eruptions exert a stronger radiative forcing",
                                      placement = "top", trigger = "hover", options = NULL)
                     )
                 ),
                 chooseSliderSkin(skin = "Flat", color = "#375a7f")
        )
    )
}

choices_server <- function(x) {
    moduleServer(x, function(input, output, session) {

        paramsList <- list()
        observeEvent(input$input_aero, print(input$input_aero))

    #updateSliderInput(session, "input_aero", value=paramsList[['alpha']])

    })
}
