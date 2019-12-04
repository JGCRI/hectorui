#
# This is a Shiny web application UI document. It describes the on screen components that define the application.

library(shinyBS)

# Using Shiny FluidPage layout -  A fluid page layout consists of rows and columns in a structured format
fixedPage(theme = shinythemes::shinytheme("darkly"),
  shinythemes::themeSelector(),
  shinyalert::useShinyalert(),
  # Application title
  titlePanel("Hector: An Interactive Climate Model"),

  # Function that gets called on first load of application to load in any themes/css etc
  # Loads the custom.css file that contains custom styles and overwrites some built in styles
  tags$head
  (
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  shinyjs::useShinyjs(),

  # Main component from which all other components fall under, navbarPage, a multi-page user-interface that includes a navigation bar
  navbarPage
  (
    "Navigation:",
    # Main navigation - currently only used for one item, run scenario. Expandable
    tabPanel
    (
      "System Information",
      mainPanel
      ( width = 9,
        tabsetPanel
        (
          # Information Tab Panel
          tabPanel
          (
            p(icon("info-circle", "fa-2x"), " System Information", value="infoTab"),
            h5("Background Information"), tags$hr(class="hrNav"),
            p("This is the repository for ", tags$b("Hector"),", an open source, object-oriented, simple global climate carbon-cycle model.
                It runs essentially instantaneously while still representing the most critical global scale earth system processes,
                and is one of a class of models heavily used for for emulating complex climate models and uncertainty analyses.
                "),
            p("For example, Hector’s global temperature rise for the RCP 8.5 scenario, compared to observations and other model results, looks like this:"),
            p(img(src='https://github.com/JGCRI/hector/wiki/rcp85.png',  height="350")),
            p("The primary link to Hector model documentation is the ", a("online manual",href="https://jgcri.github.io/hector/articles/manual", target="blank"),",
                which is included in the repository in the vignettes/manual directory. The code is also documented with ", a("Doxygen-style", href="http://doxygen.org", target="blank"),
              " comments. A formal model description paper ", a("Hartin et al. 2015", href="http://www.geosci-model-dev.net/8/939/2015/gmd-8-939-2015.html", target="blank"),
              " documents its science internals and performance relative to observed data, the ", a("CMIP5", href="http://cmip-pcmdi.llnl.gov/cmip5/", target="blank"),
              " archive, and the reduced-complexity ", a("MAGICC", href="http://www.magicc.org", target="blank"), " model (as of ", a("version 1.0", href="https://github.com/JGCRI/hector/tree/v1.0", target="blank"),
              "). In addition, we have developed two package vignettes demonstrating the ", a("basics of the Hector R interface",href="http://jgcri.github.io/hector/articles/intro-to-hector.html", target="blank"),
              ", and an example application of ", a("solving for an emissions pathway", href="http://jgcri.github.io/hector/articles/hector_apply.html", target="blank"), "."),
            p("Tools and software that work with Hector:"),
            tags$ul(
              tags$li(a("GCAM", href="https://github.com/JGCRI/gcam-core", target="blank"),": Hector can be used as
                the climate component in the GCAM integrated assessment model."),
              tags$li(a("pyHector", href="https://github.com/openclimatedata/pyhector", target="blank"),": A python
                interface to Hector.")
            )
          ),
          # Citation Tab Panel
          tabPanel
          (
            p(icon("copyright", "fa-2x"), "How to Cite", value="citeTab"),
            h5("Background Information"), tags$hr(class="hrNav")
          ),
          # tabPanel( fixed=TRUE, p(icon("chalkboard-teacher", "fa-2x"), " Instructions", value="infoTab"),
          #           h5("How the system works"), tags$hr(class="hrNav")
          #
          #         ),

          # Feedback Tab Panel
          tabPanel
          (
            p(icon("comment", "fa-2x"), "Feedback/Bug Report", value="feedbackTab"),
            h5("Submit Feedback"), tags$hr(class="hrNav"),
            p("Please use the form below to contact the Hector team regarding any questions, concerns, suggestions, or problems you may encounter."),
            htmlOutput("feedbackFrame")
          )
          # tabPanel( fixed=TRUE, p(icon("chalkboard-teacher", "fa-2x"), " Instructions", value="infoTab"),
          #           h5("How the system works"), tags$hr(class="hrNav")
          #
          #         ),

        )
      )
    ),
    tabPanel
    (
      "Run Scenario",
      # The sidebar panel splist the page into a left hand nav and right side content
      sidebarPanel
      (
        width=3,
        # A tabsetPanel creates a group of tabs within the left hand nav
        tabsetPanel
        (
          # Information Tab Panel - a tab panel is an item within a tabset
          tabPanel
          ( p(icon("info-circle", "fa-1x"), "Base Scenarios", value="infoTab"),
            div(h4("RCP Scenarios"),#, icon("info-circle", "fa-1x")),
                tags$hr(class="hrNav"),
                div(class="checkboxDiv", "Choose 1 or more Scenarios:",
                    p(
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP2.6", label = "2.6", value = FALSE, width = 45, inline = TRUE, icon = icon("check")),
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP4.5", label = "4.5", value = FALSE, width = 45, inline = TRUE, icon = icon("check")),
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP6.0", label = "6.0", value = FALSE, width = 45, inline = TRUE, icon = icon("check")),
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP8.5", label = "8.5", value = FALSE, width = 45, inline = TRUE, icon = icon("check"))
                    )
                ), # radioButtons("input_Driven", "", list("Emissions Driven"), inline=TRUE),

                # Divider that holds the parameter options/controls
                div(id="myapp",
                    h4("Model Parameters"),
                    tags$hr(class="hrNav"),
                    div(class="paramDivsTopItem", selectInput("input_paramToggle", "Model Emulation:",
                                                              list("Hector Default" = "default", "CanESM2" = "canesm2", "CESM1-BGC" = "cesm1-bgc", "GFDL-ESM2G" = "gfdl-esm2g",
                                                                   "MIROC-ESM" = "miroc-esm", "MPI-ESM-LR" = "mpi-esm-lr", "MRI-ESM1" = "mri-esm1"), width = 200)),
                    popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "External model parameters", content = "Choosing a model will load a unique set of parameters that correspond to that model for emulation in Hector.", placement = "top" ),
                    div(class="paramDivs", numericInput("input_aero", "Aerosol forcing scaling factor (unitless)", width = 205,  value = NA, step = 0.01)),popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Aerosol Forcing Scaling Factor", content = "Setting this value will change the Aerosol Forcing Factor for any active Hector cores.", placement = "top" ),
                    # fixedRow(
                    #   column(6, div(class="paramDivs", numericInput("input_aero", "Aerosol forcing scaling factor (unitless)", width = 205,  value = NA, step = 0.01))),
                    #   column(2, popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Aerosol Forcing Scaling Factor", content = "This is the tooltip information part here. It is three sentence long. This is the third sentence.", placement = "top" ))
                    #   ),
                    div(class="paramDivs", numericInput("input_beta", "CO2 fertilization factor (unitless)", width = 205,  value = NA, step = 0.01, min = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "CO2 fertilization factor", content = "Setting this value will change the CO2 Forcing Factor for any active Hector cores.", placement = "top" ),
                    div(class="paramDivs", numericInput("input_diff", "Ocean heat diffusivity (cm2/s)", width = 205,  value = NA, step = 0.01, min = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Ocean heat diffusivity", content = "Setting this value will change the Ocean Heat Diffusivity for any active Hector cores.", placement = "top" ),
                    div(class="paramDivs", numericInput("input_ecs", "Equilibrium climate sensitivity (degC)", width = 205, value = NA, step = 0.01, min = 0.01, max = 25)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Equilibrium climate sensitivity", content = "Setting this value will change the Equilibrium climate sensitivity for any active Hector cores.", placement = "top" ),
                    div(class="paramDivs", numericInput("input_pco2", "Preindustrial CO2 conc. (ppmv CO2)", width = 205,  value = NA, step = 0.01, min = 250, max = 350)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Preindustrial CO2 concentration ", content = "Setting this value will change the Preindustrial CO2 concentration for any active Hector cores.", placement = "top" ),
                    div(class="paramDivs", numericInput("input_q10", "Temp. sensitivity factor (Q10) (unitless)", width = 205, value = NA, step = 0.01, min = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Temperature sensitivity factor (Q10)", content = "Setting this value will change the Temperature sensitivity factor for any active Hector cores.", placement = "top" ),
                    div(class="paramDivs", numericInput("input_volc", "Volc. forcing scaling factor (unitless)", width = 205, value = NA, step = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Volcanic forcing scaling factor", content = "Setting this value will change the Volcanic forcing scaling factor for any active Hector cores.", placement = "top" ),
                    div(class="paramDivs",
                        actionButton(inputId="set_Params", label="Set Parameters"),
                        actionButton(inputId="reset_Params", label="Reset Parameters"),
                        popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Resetting Parameters", content = "This will reset any/all parameters for each active Hector core, overwriting any custom parameter changes. Custom emissions will remain.", placement = "top" )
                    )
                ),

                # Divider that holds the custom emissions options/controls
                div(h4("Custom Emissions"),
                    tags$hr(class="hrNav"),
                    shiny::selectInput(inputId = "input_custom_emissions", label =  "Emissions:", width = 200, multiple = F,
                                       choices = list('Emissions' = list("Black Carbon Emissions" = 'e_bc',   "Organic Carbon Emissions"='e_oc'))),
                    div(
                        fixedRow(
                          column(3, shiny::textInput(inputId = "input_custom_start", label = "Start Year:",  placeholder = 1900)),
                          column(3, shiny::textInput(inputId = "input_custom_end", label = "End Year:",  placeholder = 2100)),
                          column(3, shiny::textInput(inputId = "input_emissions_value", label = "Value:"))
                        ),
                        fixedRow(
                          column(4, shinyWidgets::prettyCheckbox(inputId = "input_slope_emissions", label = "Slope Emissions", value = FALSE,  inline = TRUE, icon = icon("check"))),
                          column(4, popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Sloping Emissions", content = "Choosing this option will create a smooth slope from the starting years value to the specified value at the end year.", placement = "top" ))
                        ),
                        actionButton(inputId="input_set_custom_emissions", label="Set Emissions"),
                        actionButton(inputId="input_reset_custom_emissions", label="Reset All Emissions"),
                        popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Resetting Emissions", content = "This will reset all active Hector cores to their default state, overwriting any custom emissions AND parameter changes.", placement = "top" )
                    )
                )
              )
          )
          # ,
          # # Tab Panel
          # tabPanel
          # ( p(icon("edit", "fa-1x"), "Custom Scenarios", value="infoTab"),
          #   div(h4("Baseline Scenarios"),#, icon("info-circle", "fa-1x")),
          #       tags$hr(class="hrNav"),
          #       selectInput("input_custom_RCP", "RCP Scenario", list("2.6" = "RCP 2.6","4.5"="RCP 4.5", "6"="RCP 6.0", "8.5" = "RCP 8.5", "Custom" = "custom"), width=200, selected = "RCP 4.5"),
          #        radioButtons("input_Driven", "", list("Emissions Driven"), inline=TRUE),
          #
          #
          #       conditionalPanel(
          #         condition = "input.input_custom_RCP == 'custom'",
          #         h4("Custom Scenario"),
          #           tags$hr(class="hrNav"),
          #           textInput("input_custom_scenarioName", "Scenario Name:", width=200, value = ""),
          #           fileInput("input_custom_scenarioFile", "Upload Emissions File:", width=275, buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
          #           div(class="paramDivs", actionButton(inputId="input_load_custom", label="Load Scenario"))
          #         )
          #   ),
          #   div(id="myapp",
          #       h4("Model Parameters"),
          #       tags$hr(class="hrNav"),
          #       div(class="paramDivsTopItem", selectInput("input_paramToggle", "Model:", list("Hector Default" = "default", "CanESM2" = "canesm2", "CESM1-BGC" = "cesm1-bgc", "GFDL-ESM2G" = "gfdl-esm2g",
          #                                                                                     "MIROC-ESM" = "miroc-esm", "MPI-ESM-LR" = "mpi-esm-lr", "MRI-ESM1" = "mri-esm1"), width = 200)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "External model parameters", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_aero", "Aerosol forcing scaling factor (unitless)", width = 205,  value = NA, step = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Aerosol Forcing Scaling Factor", content = "This is the tooltip information part here. It is three sentence long. This is the third sentence.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_beta", "CO2 fertilization factor (unitless)", width = 205,  value = NA, step = 0.01, min = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "CO2 fertilization factor", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_diff", "Ocean heat diffusivity (cm2/s)", width = 205,  value = NA, step = 0.01, min = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Ocean heat diffusivity", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_ecs", "Equilibrium climate sensitivity (degC)", width = 205, value = NA, step = 0.01, min = 0.01, max = 25)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Equilibrium climate sensitivity", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_pco2", "Preindustrial CO2 conc. (ppmv CO2)", width = 205,  value = NA, step = 0.01, min = 250, max = 350)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Preindustrial CO2 concentration ", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_q10", "Temp. sensitivity factor (Q10) (unitless)", width = 205, value = NA, step = 0.01, min = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Temperature sensitivity factor (Q10)", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs", numericInput("input_volc", "Volc. forcing scaling factor (unitless)", width = 205, value = NA, step = 0.01)), popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Volcanic forcing scaling factor", content = "This is the tooltip information part here. It is 2 sentence long.", placement = "top" ),
          #       div(class="paramDivs",
          #           actionButton(inputId="set_Params", label="Set Parameters"),
          #           actionButton(inputId="reset_Params", label="Reset Parameters"),
          #           popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Resetting Emissions", content = "This will reset all active Hector cores to their default state, overwriting any custom emissions AND parameter changes.", placement = "top" )
          #       )
          #   )
          # )
        )
      ),
      # Right side panel - Main panel that is used for output
      mainPanel
      ( width = 9,
        tabsetPanel
        (
          tabPanel
          ( fixed = TRUE,
            p(icon("chart-line","fa-2x"), "Scenario Output", value="outputTab"),
            h5("Scenario Results"), tags$hr(class="hrNav"),
              p(selectInput
                ("capabilities",  "Choose capability identifiers (up to 4):",
                  list('Carbon Cycle' = list("Atmospheric CO2"="cc_co2", "Atmospheric Carbon Pool"="cc_acp", "FFI Emissions"="cc_ffi", "LUC Emissions"="cc_luc"),
                       'Concentrations' = list("Amospheric N2O"='c_an20'),
                       'Emissions' = list("Black Carbon Emissions" = 'e_bc',   "Organic Carbon Emissions"='e_oc'),
                       'Forcings' = list("RF - Total"='f_rft', "RF - Albedo"='f_alb', "RF - CO2"='f_co2', "RF - N2O"='f_n2o', "RF - H2O"='f_h2o', "RF - Ozone"='f_oz', "RF - Black Carbon"='f_bc',
                                         "RF - Organic Carbon"='f_oc', "RF - SO2 Direct"='f_so2d', "RF - SO2 Indirect"='f_so2i', "RF - SO2 Total"='f_so2t', "RF - Volcanic Activity"='f_va', "RF - CH4"='f_ch4'),
                       # 'Halocarbon Emissions' = list("CF4 Emissions"='he_cf4', "C2F6 Emissions"='he_c2f6', "HFC-23 Emissions"='he_hfc23', "HFC-32 Emissions"='he_hfc32', "HFC-4310 Emissions"= 'he_hfc4310', "HFC-125 Emissions"='he_hfc125', "HFC-134a Emissions"='he_hfc134a', "HFC-143a Emissions"='he_hfc143a', "HFC-227ea Emissions"='he_hfc227ea',
                       #                               "HFC-245fa Emissions"='he_hfc245fa', "SF6 Emissions"='he_sf6', "CFC-11 Emissions"='he_cfc11', "CFC-12 Emissions"='he_cfc12', "CFC-113 Emissions"='he_cfc113',"CFC-114 Emissions"='he_cfc114',"CFC-115 Emissions"='he_cfc115',"CCl4 Emissions"='he_ccl4', "CH3CCl3 Emissions"='he_ch3ccl3',
                       #                               "HCFC-22 Emissions"='he_hcfc22', "HCFC-141b Emissions"='he_hcfc141b', "HCFC-142b Emissions"='he_hcfc142b', "Halon-1211 Emissions"='he_halon1211', "Halon-1301 Emissions"='he_halon1301', "Halon-2402 Emissions"='he_halon2402', "CH3Cl Emissions"='he_ch3cl', "CH3Br Emissions"='he_ch3br'),
                       'Halocarbon Forcings' = list("CF4 Forcing"='hf_cf4', "C2F6 Forcing"='hf_c2f6', "HFC-23 Forcing"='hf_hfc23', "HFC-4310 Forcing"='hf_hfc4310', "HFC-125 Forcing"='hf_hfc125',  "HFC-143a Forcing"='hf_hfc143a',
                                                    "HFC-254fa Forcing"='hf_hfc245fa', "SF6 Forcing"='hf_hfcsf6', "CFC-11 Forcing"='hf_cfc11', "CFC-12 Forcing"='hf_cfc12', "CFC-113 Forcing"='hf_cfc113',"CFC-114 Forcing"='hf_cfc114',"CFC-115 Forcing"='hf_cfc115',"CCl4 Forcing"='hf_ccl4', "CH3CCl3 Forcing"='hf_ch3ccl3',
                                                    "HCFC-22 Forcing"='hf_hcfc22', "HCFC-141b Forcing"='hf_hcfc141b', "HCFC-142b Forcing"='hf_hcfc142b', "Halon-1211 Forcing"='hf_halon1211', "Halon-1301 Forcing"='hf_halon1301', "Halon-2402 Forcing"='hf_halon2402', "CH3Cl Forcing"='hf_ch3cl', "CH3Br Forcing"='hf_ch3br'),
                       'Methane' = list("Atmospheric CH4"='m_a_ch4',  "Emissions CH4"='m_e_ch4'),
                       # 'Ocean' = list("Ocean Carbon Flux"='o_cf', "Ocean Total Carbon"='o_tc', "Ocean Surface High-Lat Carbon"='o_os_hlc', "Ocean Surface Low-Lat Carbon"='o_os_llc', "Ocean Intermediate Carbon"='o_ic', "Ocean Deep Carbon"='o_dc', "Thermohaline Overturning"='o_to',
                       #                "High-Lat Overturning"='o_hl_o', "Warm-Intermediate Exchange"='o_wie', "Intermediate-Deep Exchange"='o_ide', "High Latitude Ph"='o_hl_ph', "Low Latitude Ph"='o_ll_ph', "Atmosphere-Ocean Flux - High Lat"='o_hl_aof', "Atmosphere-Ocean Flux - Low Lat"='o_ll_aof',
                       #                "Partial Pressure CO2 - High Lat"='o_hl_pp_co2',"Partial Pressure CO2 - Low Lat"='o_ll_pp_co2',"Dissolved Inorganic C - High Lat"='o_hl_dic', "Dissolved Inorganic C - Low Lat"='o_ll_dic', "Ocean Temperature - High Lat"='o_hl_t', "Ocean Temperature - Low Lat"='o_ll_t',
                       #                "Carbonate Concentration - High Lat"='o_hl_cc', "Carbonate Concentration - Low Lat"='o_ll_cc'),
                       'SO2' = list( "Anthropogenic SO2"='so2_a', "Natural CH4 Emissions"='so2_n_ch4', "Volcanic SO2"='so2_v'),
                       'Temperature' = list("Global Mean Temp"='t_gmt', "Equilibrium Global Temp"='t_egt', "Ocean Surface Temp"='t_ost', "Ocean Air Temp"='t_oat', "Land Temp Anomaly"="t_lta", "Heat Flux - Mixed Layer Ocean"='t_hf_mlo', "Heat Flux - Interior Layer Ocean"='t_hf_ilo', "Total Heat Flux - Ocean"='t_hf_t')),
                  multiple = T, width=350, selected = "t_gmt"
                ),
                actionButton(inputId="loadGraphs", label="Load Graphs", width = 200),
                downloadButton("downloadData", label="Download Raw Data")
               ),
               # plotly::plotlyOutput("plotly", width = "100%", height = "350"),
            uiOutput("plots", class = "customPlot")
          ), # end tabpanel
          tabPanel
          ( fixed = TRUE,
            p(icon("chart-line","fa-2x"), "Downscaled Maps", value="outputTab"),
            h5("Scenario Results"), tags$hr(class="hrNav")
          ) # end tabpanel
        ) # end tabsetpanel
      ) # end mainpanel
    ) # end tabpanel
  ) # end navbarpage
) # end of everything.

