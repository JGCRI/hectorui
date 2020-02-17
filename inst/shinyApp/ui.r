#
# This is a Shiny web application UI document. It describes the on screen components that define the visual application.

library(shinyBS)

# Using Shiny Fixed layout
fixedPage(theme = shinythemes::shinytheme("darkly"),
  # shinythemes::themeSelector(),
  shinyalert::useShinyalert(),
  # Application title
  h2("Hector: An Interactive Climate Model", class="titleText"),

  # Function that gets called on first load of application to load in any themes/css etc
  # Loads the custom.css file that contains custom styles and overwrites some built in styles
  tags$head
  (
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # ,
    # tags$link(href="google-analytics.js")
  ),
  shinyjs::useShinyjs(),

  # Main component from which all other components fall under, navbarPage, a multi-page user-interface that includes a navigation bar
  navbarPage
  (
    "Navigation:",
    # Main navigation - divides app into 2 sections: informational and interactive
    tabPanel
    (
      "System Information",
      # Main Panel that holds the tabs for the Information section
      mainPanel
      ( width = 9,
        tabsetPanel
        (
          # Information Tab Panel
          tabPanel
          (
            p(icon("info-circle", "fa-2x"), "Hector Information", value="infoTab"),
            h5("Background Information"), tags$hr(class="hrNav"),
            tags$table(
              tags$tr(
                tags$td(width = "50%", style="vertical-align: middle;",
                p("Welcome to the user interface for", tags$b("Hector:")," an open source, object-oriented, and interactive simple global
                climate carbon-cycle model. It runs essentially instantaneously while still representing the most critical global scale earth system processes,
                and is one of a class of models heavily used for for emulating complex climate models and uncertainty analyses."),
                        p("This interactive version is built upon previous work by developers at the ", a("Joint Global Change Research Institute (JGCRI)", href="http://globalchange.umd.edu/", target="blank"),
                        " including the development
                        of the initial C++ version of Hector, and the follow up R Package, \"Hector R\". The simple diagram below outlines their relationships:"),
                        h3("The Hector Product Family", style="text-align: center"),
                        img(src="images/Hector-sm.png")
                       ),
                tags$td(tags$figure(
                  tags$figcaption("Hector's global temperature rise for RCP 8.5 scenario, compared to observations and other model results"),
                                    img(src='https://github.com/JGCRI/hector/wiki/rcp85.png',  height="330", class="zenodo")
                                    ), style="text-align: center"
                )
              )
            ),
            br(),
            h5("Documentation/Downloads"), tags$hr(class="hrNav"),
            p("The primary link to the Hector model documentation is the ", a("online manual",href="https://jgcri.github.io/hector/articles/manual", target="blank"),",
                which is included in the vignettes/manual directory. The code is also documented with ", a("Doxygen-style", href="http://doxygen.org", target="blank"),
              " comments."),
            p("A formal model description paper via ", a("Hartin et al. 2015", href="http://www.geosci-model-dev.net/8/939/2015/gmd-8-939-2015.html", target="blank"),
              " documents its science internals and performance relative to observed data, the ", a("CMIP5", href="http://cmip-pcmdi.llnl.gov/cmip5/", target="blank"),
              " archive, and the reduced-complexity ", a("MAGICC", href="http://www.magicc.org", target="blank"), " model (as of ", a("version 1.0", href="https://github.com/JGCRI/hector/tree/v1.0", target="blank"),
              "). In addition, we have developed two package vignettes demonstrating the ", a("basics of the Hector R interface",href="http://jgcri.github.io/hector/articles/intro-to-hector.html", target="blank"),
              ", and an example application of ", a("solving for an emissions pathway", href="http://jgcri.github.io/hector/articles/hector_apply.html", target="blank"), "."),
            tags$ul(
              tags$li(h5(a("Hector User Interface package download/source link "), href="https://github.com/JGCRI/Hector-ui", target="blank")),
              tags$li(h5(a("Hector R / Hector C++ package download/source link"), href="https://github.com/JGCRI/Hector", target="blank")),
              tags$li(h5(a("Code and data for Hector calibration papers", tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3515153.svg", class="zenodo", alt="DOI")), href="https://zenodo.org/record/3515153#.Xg9iGuhKiUl", target="blank"))
            ),
            br(),
            h5("Tools and software that work with Hector"), tags$hr(class="hrNav"),
            tags$ul(
              tags$li(a("GCAM", href="https://github.com/JGCRI/gcam-core", target="blank"),": Hector can be used as
                the climate component in the GCAM integrated assessment model."),
              tags$li(a("pyHector", href="https://github.com/openclimatedata/pyhector", target="blank"),": A python
                interface to Hector."),
              tags$li(a("R/Shiny", href="https://shiny.rstudio.com/", target="blank"),": This application was built as an R-Shiny package
                      wrapper over the existing model code.")
            )
          ),
          # Citation Tab Panel
          tabPanel
          (
            p(icon("copyright", "fa-2x"), "License/How to Cite", value="citeTab"),
            h5("License Information"), tags$hr(class="hrNav"),

            tags$div(class="citationsDiv", style="width: 500px;",
               tags$table(class="citationsTable",
                 tags$tr(
                   tags$td(rowspan=2, width=45, icon("balance-scale", "fa-2x")),
                   tags$td(("All Hector applications are licensed under the")

                   ),
                   tags$tr(
                     tags$td(tags$a(h6("GNU General Public License v3.0"), href="https://github.com/JGCRI/hector/blob/master/LICENSE.md", target="blank"))
                   )
                 ),
                 tags$table(class="citationsTable",
                   tags$tr(
                     tags$td(p("Permissions of this strong copyleft license are conditioned on making available complete source code of licensed works and modifications,
                               which include larger works using a licensed work, under the same license. Copyright and license notices must be preserved.
                               Contributors provide an express grant of patent rights.")
                     )
                   )
                 )
               )

            ),
            div(class="citationsDiv",
              tags$span(class="citationsHeader", "Permissions"),
              tags$ul(class="ul-None",tags$li(icon("check"),"Commercial use"),tags$li(icon("check"),"Modification"),
                                       tags$li(icon("check"),"Distribution"),tags$li(icon("check"),"Patent use"),tags$li(icon("check"),"Private use"))
            ),
            div(class="citationsDiv",
              tags$span(class="citationsHeader", "Limitations"),
              tags$ul(class="ul-None",tags$li(icon("times")," Liability"),tags$li(icon("times")," Warranty"))
            ),
            div(class="citationsDiv",
              tags$span(class="citationsHeader", "Conditions"),
              tags$ul(class="ul-None", tags$li(icon("info"),"License and copyright notice"),tags$li(icon("info"),"State changes"),
                                       tags$li(icon("info"),"Disclose source"),tags$li(icon("info"),"Same license"))
            ),
            h5("How to Cite Hector"), tags$hr(class="hrNav"),
            p("When using graphs, figures, or other output from this applicaton please cite both the Hector Core", br(), "application as well as
              the Hector User Interface (this application). The DOI for both is provided below:"),
            tags$ul(
              tags$li(
               h5(tags$a("Hector User Interface DOI",href="https://doi.org/10.5281/zenodo.3603216", target = "blank"), tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3603216.svg", alt="DOI", class = "imgNoPadding"))),
              tags$li(
                tags$h5(a("Hector Core DOI", tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3515153.svg", alt="DOI", class = "imgNoPadding")),href="https://doi.org/10.5281/zenodo.3515153", target = "blank"))
            )
          ),

          # Feedback Tab Panel
          tabPanel
          (
            p(icon("comment", "fa-2x"), "Support", value="feedbackTab"),
            h5("Submit Feedback"), tags$hr(class="hrNav"),
            p("Please use the form below to contact the Hector team regarding any questions, concerns, suggestions, or problems you may encounter."),
            # h3("Feedback Form"),
            # textInput("feedbackEmail", "Email Address:", width = 250),
            # selectInput("feedbackType", "Type of Feedback:", choices = c("Bug Report", "Comment", "Feature Request", "Question"), width = 180),
            # textAreaInput("feedbackInfo", "Feedback:", width = 300 ),
            # actionButton("input_submit_feedback","Submit Feedback")
            htmlOutput("feedbackFrame")
          )
        )
      )
    ),
    # Main panel for the interactive section of the application
    tabPanel
    (
      "Run Scenario",
      # The sidebar panel splits the page into a left hand nav and right side content
      sidebarPanel
      (
        width=3,
        # A tabsetPanel creates a group of tabs within the left hand nav
        tabsetPanel
        (
          # Standard Scenarios (RCP) Panel
          tabPanel
          ( p( "Standard Scenarios", value="infoTab"),
            div(h5("RCP Scenarios"),#, icon("info-circle", "fa-1x")),
                tags$hr(class="hrNav"),
                div(class="checkboxDiv", "Choose 1 or more Scenarios:",
                    p(
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP2.6", label = "2.6", value = FALSE, width = 45, inline = TRUE, icon = icon("check"), animation = "pulse", status = "primary"),
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP4.5", label = "4.5", value = TRUE, width = 45, inline = TRUE, icon = icon("check"), animation = "pulse", status = "success"),
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP6.0", label = "6.0", value = FALSE, width = 45, inline = TRUE, icon = icon("check"), animation = "pulse", status = "warning"),
                    shinyWidgets::prettyCheckbox(inputId = "input_RCP8.5", label = "8.5", value = FALSE, width = 45, inline = TRUE, icon = icon("check"), animation = "pulse", status = "danger")
                    )
                ), # radioButtons("input_Driven", "", list("Emissions Driven"), inline=TRUE),

                # Divider that holds the parameter options/controls
                div(id="myapp",
                    h5("Model Parameters"),
                    tags$hr(class="hrNav"),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "External model parameters", content = "Choosing a model will load a unique set of parameters that correspond to that model for emulation in Hector.", placement = "top" )),
                              tags$td(width = "34%", div("Model Emulation:")),
                              tags$td(width = "58%", class="tdPushBottom",  selectInput("input_paramToggle", label = NULL,
                                                                  choices =  list("Hector Default" = "default", "CanESM2" = "canesm2", "CESM1-BGC" = "cesm1-bgc", "GFDL-ESM2G" = "gfdl-esm2g",
                                                                       "MIROC-ESM" = "miroc-esm", "MPI-ESM-LR" = "mpi-esm-lr", "MRI-ESM1" = "mri-esm1"), width = 190))
                      )
                    ),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Aerosol Forcing Scaling Factor", content = "Setting this value will change the Aerosol Forcing Factor for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("Aerosol forcing scaling factor (unitless)")),
                              tags$td(width = "24%", numericInput("input_aero", width = 80, label = NULL, value = NA, step = 0.01))
                              ),
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "CO2 fertilization factor", content = "Setting this value will change the CO2 Forcing Factor for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("CO2 fertilization factor (unitless)")),
                              tags$td(width = "24%", numericInput("input_beta", width = 80, label = NULL, value = NA, step = 0.01, min = 0.01))
                      ),
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Ocean heat diffusivity", content = "Setting this value will change the Ocean Heat Diffusivity for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("Ocean heat diffusivity (cm2/s)")),
                              tags$td(width = "24%", numericInput("input_diff", width = 80, label = NULL, value = NA, step = 0.01))
                      ),
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Equilibrium climate sensitivity", content = "Setting this value will change the Equilibrium climate sensitivity for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("Equilibrium climate sensitivity (degC)")),
                              tags$td(width = "24%", numericInput("input_ecs", width = 80, label = NULL, value = NA, step = 0.01, min = 0.01, max = 25))
                      ),
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Preindustrial CO2 concentration ", content = "Setting this value will change the Preindustrial CO2 concentration for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("Preindustrial CO2 conc. (ppmv CO2)")),
                              tags$td(width = "24%", numericInput("input_pco2", width = 80, label = NULL, value = NA, step = 0.01, min = 250, max = 350))
                      ),
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Temperature sensitivity factor (Q10)", content = "Setting this value will change the Temperature sensitivity factor for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("Temp. sensitivity factor (Q10) (unitless)")),
                              tags$td(width = "24%", numericInput("input_q10", width = 80, label = NULL, value = NA, step = 0.01, min = 0.01))
                      ),
                      tags$tr(width = "100%",
                              tags$td(width = "8%", align = "center", popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Volcanic forcing scaling factor", content = "Setting this value will change the Volcanic forcing scaling factor for any active Hector cores.", placement = "top" )),
                              tags$td(width = "68%", div("Volcanic forcing scaling factor (unitless)")),
                              tags$td(width = "24%", numericInput("input_volc", width = 80, label = NULL, value = NA, step = 0.01))
                      )
                    ),
                    div(class="paramDivs",
                        actionButton(inputId="set_Params", label="Set Parameters"),
                        actionButton(inputId="reset_Params", label="Reset Parameters"),
                        popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Resetting Parameters", content = "This will reset any/all parameters for each active Hector core, overwriting any custom parameter changes. Custom emissions will remain.", placement = "top" )
                    )
                ),

                # Divider that holds the custom emissions options/controls
                div(h5("Custom Emissions"),
                    tags$hr(class="hrNav"),
                    p("Note: Custom emissions are only applicable to standard scenarios (not custom created scenarios)"),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width="25%", "Emissions:"),
                              tags$td(shiny::selectInput(inputId = "input_custom_emissions", label = NULL, width = 200, multiple = F,
                                       choices = list('Emissions' = list("Black Carbon Emissions" = 'e_bc',   "Organic Carbon Emissions"='e_oc'))))
                              )
                      ),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(align = "left", shiny::textInput(inputId = "input_custom_start", label = "Start Year:", width = 65, placeholder = 1900)),
                              tags$td(width = 15),
                              tags$td(align = "left", shiny::textInput(inputId = "input_custom_end", label = "End Year:",  width = 65, placeholder = 2100)),
                              tags$td(width = 15),
                              tags$td(align = "left", shiny::textInput(inputId = "input_emissions_value", label = "Value:", width = 65)),
                              tags$td()
                      )
                    ),
                    tags$table(
                      tags$tr(width = "100%",
                              tags$td(width = "15%", align="left", class="tdPushTop", popify(div(class="paramDivsPushTop", icon("info-circle", "fa-1x")), title = "Sloping Emissions", content = "Choosing this option will create a smooth slope from the starting years value to the specified value at the end year.", placement = "top" )),
                              tags$td(width = "85%",shinyWidgets::prettyCheckbox( animation = "pulse", inputId = "input_slope_emissions", label = "Slope Emissions", value = FALSE,  inline = TRUE, icon = icon("check")))

                      )
                    ),
                    div(
                        actionButton(inputId="input_set_custom_emissions", label="Set Emissions"),
                        actionButton(inputId="input_reset_custom_emissions", label="Reset All Emissions"),
                        popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Resetting Emissions", content = "This will reset all active Standard Hector cores to their default state, overwriting any custom emissions AND parameter changes.", placement = "top" )
                    )
                )
              )
          ), # End Information Panel

          # Custom Scenarios Tab Panel
          tabPanel
          (
            p(icon("edit", "fa-1x"), "Custom Scenarios - BETA", value="infoTab"),
            div
            (
              h5("Custom Emissions Pathway"),
              tags$hr(class="hrNav"),
              p("Steps to run your own scenario with custom emissions:"),
              tags$ol(
                tags$li("Choose a baseline RCP scenario as your starting point"),
                tags$li("Give your new custom scenario a name"),
                tags$li("Download the emissions file template for that scenario and enter your own emissions"),
                tags$li("Upload the new customized emissions file")
              ),
              p(tags$strong("Do not edit any field names or change the CSV file in any way other than changing the data")),
              tags$table(
                tags$tr(width = "100%",
                  tags$td(width = "145", "Baseline Scenario:"),
                  tags$td(width = "155", selectInput("input_custom_RCP", label = NULL, list("RCP 2.6" = "RCP 2.6","RCP 4.5"="RCP 4.5", "RCP 6"="RCP 6.0", "RCP 8.5" = "RCP 8.5"), width=150, selected = "RCP 4.5"))
                ),
                tags$tr(width = "100%",
                  tags$td(width = "145", "Custom Scenario Name:"),
                  tags$td(width = "200",  textInput("input_custom_scenarioName", label = NULL, width=195, value = ""))
                )
              ),

              div(
                conditionalPanel(
                  condition = "input.input_custom_RCP == 'RCP 2.6'",
                  a(h6("Download RCP 2.6 Emissions File Template"), href="input/emissions/RCP26_custom_template.csv")
                ),
                conditionalPanel(
                  condition = "input.input_custom_RCP == 'RCP 4.5'",
                  a(h6("Download RCP 4.5 Emissions File Template"), href="input/emissions/RCP45_custom_template.csv")
                ),
                conditionalPanel(
                  condition = "input.input_custom_RCP == 'RCP 6.0'",
                  a(h6("Download RCP 6.0 Emissions File Template"), href="input/emissions/RCP6_custom_template.csv")
                ),
                conditionalPanel(
                  condition = "input.input_custom_RCP == 'RCP 8.5'",
                  a(h6("Download RCP 8.5 Emissions File Template"), href="input/emissions/RCP85_custom_template.csv")
                ),

                fileInput("input_custom_emissions_file", "Upload Custom Emissions File:", width=275, buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
                div(
                  class="paramDivs", actionButton(inputId="input_load_emissions", label="Create Scenario"))
              )
              # conditionalPanel(
              #   condition = "input.input_custom_RCP == 'custom'",
              #   h5("Custom Scenario (Advanced Users)"),
              #   tags$hr(class="hrNav"),
              #   p("To customize a scenario, first download the file package below, extract the files and follow these steps:"),
              #   tags$ol(
              #     tags$li("Customize the .INI file that corresponds closest to your desired starting point (i.e. Hector_RCP26.ini)"),
              #     tags$li("Replace every emissions file path in the .INI file with a fully qualified local path (i.e. c:\\files\\emissions.csv)"),
              #     tags$li("(Optional) Change emissions data in emissions file(s)"),
              #     tags$li("Upload the .INI file below to create a custom scenario")
              #   ),
              #   a(h6("Download Custom Scenario File Package"), href="input/HectorCustomFiles.zip"),
              #   fileInput("input_custom_scenario_ini", "Upload Custom Scenario File (.INI):", width=275, buttonLabel = "Choose File", accept = c(".ini")),
              #   fileInput("input_custom_scenario_csv", "Upload Custom Emissions File (.CSV):", width=275, buttonLabel = "Choose File", accept = c(".ini")),
              #     div(class="paramDivs", actionButton(inputId="input_load_custom", label="Create Scenario"))
              # ) # End conditional Panel

            ) # End Div
          ) # End Custom Scenarios Tab Panel
        ) # End Tabset
      ), # End Sidebar Panel

      # Right hand content panel - Main panel that is used for output
      mainPanel
      ( width = 9,
        tabsetPanel
        (
          # Graphs Tab
          tabPanel
          ( fixed = TRUE,
            p(icon("chart-line","fa-2x"), "Scenario Output", value="outputTab"),
            h5("Graphs"), tags$hr(class="hrNav"),
            tags$table
            (
              tags$tr
              (
                tags$td
                (
                  selectInput
                  ("capabilities",  "Choose Output Variables (up to 4):",
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
                    multiple = T, width=320, selected = "t_gmt")
                ),
                tags$td(style="padding-left: 5px;",
                  selectInput(inputId = "set_theme", width=150, label="Output Theme:", choices = c("Camoflauge", "Chalk", "Copper", "Dust", "Earth",  "Flat", "Flat Dark", "Fresh", "Grape",  "Grass", "Greyscale",
                                                                                             "Light", "Lilac", "Pale", "Sea", "Sky", "Solarized"), selected = "Dust")
                )
              )
            ),

            actionButton(inputId="loadGraphs", label="Load Graphs", width = 200),
            downloadButton("downloadData", label="Download Raw Data"),
            uiOutput("plots", class = "customPlot")
          ), # end Graphs Tab

          # Maps Tab
          tabPanel
          ( fixed = TRUE,
            p(icon("globe-americas","fa-2x"), "Downscaled Maps", value="outputTab"),
            h5("Maps"), tags$hr(class="hrNav"),
            p("Please note that some map model patterns are rather large and can take several seconds to load. Due to this, maps will need to be refreshed manually after any changes."),
            tags$table(
              tags$tr(
                tags$td(
                  selectInput(inputId = "mapPattern", label = "Choose Model:", width = 180,
                              choices = c("CanESM2" = "www/maps/tas_Amon_CanESM2_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                          "CESM1-BGC" = "www/maps/tas_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
                                          "GFDL-ESM2G" = "www/maps/tas_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
                                          "MIROC-ESM" = "www/maps/tas_Amon_MIROC-ESM_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                          "MPI-ESM-LR" = "www/maps/tas_Amon_MPI-ESM-LR_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                          "MRI-ESM1" = "www/maps/tas_Amon_MRI-ESM1_esmrcp85_r1i1p1_200601-210012_pattern.rds")),
                ),
                tags$td(style="padding-left: 5px;",
                  selectInput(inputId = "mapYear", label = "Choose Year:", selected = 2100, choices = c(2000:2100), multiple = F, width = 120)
                ),
                tags$td(style="padding-left: 5px;",
                  uiOutput("coreMapping")
                )
              )
            ),
            actionButton(inputId="loadMaps", label="Load Map", width = 200),
            uiOutput("maps", class = "customPlot")
          ) # End Maps Tab
        ) # End tabset panel
      ) # End mainpanel
    ) # End tabpanel
  ) # End navbarpage
) # End of everything.

