#
# This is a Shiny web application UI document. It describes the on screen components that define the visual application.

library(shinyBS)
library(shinyWidgets)

# Using Shiny Fixed layout
fluidPage(theme = shinythemes::shinytheme("readable"),
          shinyalert::useShinyalert(),
          title = "HectorUI: An Interactive Climate Model",

          # Code that gets called on first load of application to load in any themes/css etc
          # Loads the custom.css file that contains custom styles and overwrites some built in styles
          tags$head
          (
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
          ),
          shinyjs::useShinyjs(),

          tags$div(class = "container",
                   tags$img(src = "images/earth-header.png", height = "300px", width = "100%", class = "earth", alt = "Earth's atmosphere"),
                   tags$div(
                            a(
                            img(src = "images/GCIMS_logo_alt.svg", class = "logo"), href = "https://gcims.pnnl.gov/", target = "_blank"),
                            h1("HectorUI", class = "header-text-title"),
                            h2("An Interactive Climate Model", class = "header-text-sub", style = "font-weight:normal; "),
                            ),
          ),
          br(),
          # Main component from which all other components fall under, navbarPage, a multi-page user-interface that includes a navigation bar
          navbarPage
          (
            id = "nav",
            title = "",
            collapsible = TRUE,
            # Main navigation
            tabPanel(
              "Home",
              fluidRow(column(7, div(class = "home-text",
                  p("Welcome to the user interface for",
                    tags$b("Hector:"),
                    " an open source, object-oriented, and interactive simple global climate carbon-cycle model.
                                  It runs essentially instantaneously while still representing the most critical global scale earth system processes,
                                  and is one of a class of models heavily used for for emulating complex climate models and uncertainty analyses.", style = "float:left"),
                  p("This interactive version is built upon previous work by developers at the ",
                    a("Joint Global Change Research Institute (JGCRI)", href="http://globalchange.umd.edu/", target="blank"),
                    " including the development of the initial C++ version of Hector, and the follow up R Package, \"Hector R\"."),
                  style = "float: left"
                       )),
                       column(4, div(
                           br(),
                           br(),

                           actionButton(inputId = "launch_scenario",
                                                    label = "Explore Hector",
                                                    style = "background: #4174C3; color: white;
                                                             font-size: 24px; padding: 32px 24px;
                                                             box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);"),
                           align = "center"
                       ))
                       )
            ),
              tabPanel(
                  "Guides",
                  mainPanel(
                      style="vertical-align: middle;",
                      h3("Ready to get started?",
                         tags$a("View the Guide/Tutorial", href="https://jgcri.github.io/hectorui/articles/Tutorial.html", target="blank")),
                      br(),
                      HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/fBHXS7pjZcI" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')

                  )
              ),
              # Main panel for the interactive section of the application
              tabPanel(
                  "Explore Hector",
                  mainPanel(
                      width = 4,
                      tabsetPanel(
                          tabPanel(class = "params",
                              p( "Standard Scenarios", value="infoTab"),
                              div(
                                  br(),
                                  h5("Representative Concentration Pathways (RCPs)"),
                                  tags$hr(class="hrNav"),
                                  # shinyWidgets::awesomeCheckboxGroup(inputId = "input_RCP", label = "",
                                  #                                    choices = c("2.6", "4.5", "6.0", "8.5"), inline = TRUE, status = "info"),
                                  shinyWidgets::prettyCheckbox(inputId = "input_RCP_2.6", label = "2.6", value = FALSE, width = 45,
                                                               inline = TRUE, icon = icon("check"), animation = "jelly", status = "info"),
                                  shinyWidgets::prettyCheckbox(inputId = "input_RCP_4.5", label = "4.5", value = TRUE, width = 45,
                                                               inline = TRUE, icon = icon("check"), animation = "jelly", status = "success"),
                                  shinyWidgets::prettyCheckbox(inputId = "input_RCP_6.0", label = "6.0", value = FALSE, width = 45,
                                                               inline = TRUE, icon = icon("check"), animation = "jelly", status = "warning"),
                                  shinyWidgets::prettyCheckbox(inputId = "input_RCP_8.5", label = "8.5", value = FALSE, width = 45,
                                                               inline = TRUE, icon = icon("check"), animation = "jelly", status = "danger"),

                                  chooseSliderSkin(skin = "Flat", color = "#375a7f"),

                                  h5("Model Parameters"),
                                  fluidRow(
                                      column(6,
                                             selectInput("input_paramToggle", label = NULL,
                                                         choices =  list("Hector Default" = "default", "CanESM2" = "canesm2",
                                                                         "CESM1-BGC" = "cesm1-bgc", "GFDL-ESM2G" = "gfdl-esm2g",
                                                                         "MIROC-ESM" = "miroc-esm", "MPI-ESM-LR" = "mpi-esm-lr",
                                                                         "MRI-ESM1" = "mri-esm1"), width = 190)
                                             ),
                                      column(6,
                                             actionButton(inputId="reset_Params", label="Reset Parameters",
                                                          style = "background: #4174C3; color: white;")

                                      ),
                                  ),
                                  fluidRow(
                                      column(12,
                                             sliderInput("input_aero", "Aerosol forcing scaling factor", min = 0, max = 1, value = 1, width = "90%"),
                                             sliderInput("input_beta", "CO2 fertilization factor", min = 0, max = 4, value = 0.36, step = 0.01, width = "90%"),
                                             sliderInput("input_diff", "Ocean heat diffusivity", min = 0, max = 5, value = 2.3, step = 0.1, post = " cm2/s", width = "90%"),
                                             sliderInput("input_ecs", "Equilibrium climate sensitivity", min = 1, max = 6, value = 3, step = 0.1, post = " °C", width = "90%"),
                                             #sliderInput("input_pco2", "Preindustrial CO2 conc. (ppmv CO2)", min = 250, max = 300, value = 276.09, post = " ppmv CO2"), #might remove for v3
                                             sliderInput("input_q10", "Heterotrophic Temperature Sensitivity", min = 1, max = 5, value = 2, step = 0.1, width = "90%"),
                                             sliderInput("input_volc","Volcanic forcing scaling factor", min = 0, max = 1, value = 1, width = "90%"),

                                             # Add hover popups with parameter descriptions
                                             bsPopover("input_aero", "Increasing this means aerosols exert a stronger radiative forcing",
                                                       placement = "top", trigger = "hover", options = NULL),
                                             bsPopover("input_beta", "Increasing this means vegetation grows faster as CO2 increases",
                                                       placement = "top", trigger = "hover", options = NULL),
                                             bsPopover("input_diff", "Increasing this means heat moves deeper into the ocean quicker",
                                                       placement = "top", trigger = "hover", options = NULL),
                                             bsPopover("input_ecs", "Increasing this means a larger temperature rise as CO2 increases",
                                                       placement = "top", trigger = "hover", options = NULL),
                                             bsPopover("input_pco2", "Increasing this means a higher atmospheric CO2 as the model starts",
                                                       placement = "top", trigger = "hover", options = NULL),
                                             bsPopover("input_q10", "Increasing this means soil microbes respire faster as temperature rises",
                                                       placement = "top", trigger = "hover", options = NULL),
                                             bsPopover("input_volc", "Increasing this means that volcanic eruptions exert a stronger radiative forcing",
                                                       placement = "top", trigger = "hover", options = NULL)

                                            #actionButton(inputId="set_Params", label="Set Parameters", style = "background: #4174C3; color: white;"),
                                  ),
                                  ),

                                  # Divider that holds the custom emissions options/controls
                                  # div(class = "c-emissions",
                                  #     h5("Custom Emissions"),
                                  #     tags$hr(class="hrNav"),
                                  #     p("Note: Custom emissions are only applicable to standard scenarios (not custom created scenarios)"),
                                  #     tags$table(
                                  #         tags$tr(width = "100%",
                                  #                 tags$td(width="25%", "Emissions:"),
                                  #                 tags$td(shiny::selectInput(inputId = "input_custom_emissions", label = NULL, width = 200, multiple = F,
                                  #                                            choices = list('Emissions' = list("Black Carbon Emissions" = 'e_bc',   "Organic Carbon Emissions"='e_oc'))))
                                  #         )
                                  #     ),
                                  #     tags$table(
                                  #         tags$tr(width = "100%",
                                  #                 tags$td(align = "left", shiny::textInput(inputId = "input_custom_start", label = "Start Year:", width = 65)),
                                  #                 tags$td(width = 15),
                                  #                 tags$td(align = "left", shiny::textInput(inputId = "input_custom_end", label = "End Year:",  width = 65)),
                                  #                 tags$td(width = 15),
                                  #                 tags$td(align = "left", shiny::textInput(inputId = "input_emissions_value", label = "Value (Tg):", width = 65)),
                                  #                 tags$td()
                                  #         )
                                  #     ),
                                  #     tags$table(
                                  #         tags$tr(width = "100%",
                                  #                 tags$td(width = "15%", align="left", class="tdPushTop",
                                  #                         popify(div(class="paramDivsPushTop", icon("info-circle", "fa-1x")), title = "Sloping Emissions",
                                  #                                content = "Choosing this option will create a smooth slope from the starting years value to the specified value at the end year.", placement = "top" )),
                                  #                 tags$td(width = "85%",shinyWidgets::prettyCheckbox(animation = "pulse", inputId = "input_slope_emissions", label = "Slope Emissions", value = FALSE,  inline = TRUE, icon = icon("check")))
                                  #         )
                                  #     ),
                                  #     div(
                                  #         actionButton(inputId="input_set_custom_emissions", label="Set Emissions", style = "background: #4174C3; color: white;"),
                                  #         actionButton(inputId="input_reset_custom_emissions", label="Reset All Emissions", style = "background: #4174C3; color: white;"),
                                  #         popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Resetting Emissions",
                                  #                content = "This will reset all active Standard Hector cores to their default state, overwriting any custom emissions AND parameter changes.", placement = "top" )
                                  #     )
                                  # )# ) Collapsible comment
                                  # )
                              )
                          ),
                          # Custom Scenarios Tab Panel
                          tabPanel
                          (class = "params",
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
                                              tags$td(width = "155", selectInput("input_custom_RCP", label = NULL, list("RCP2.6" = "RCP-2.6","RCP 4.5"="RCP-4.5", "RCP 6"="RCP-6.0", "RCP 8.5" = "RCP-8.5"), width=150, selected = "RCP-4.5"))
                                      ),
                                      tags$tr(width = "100%",
                                              tags$td(width = "145", "Your Scenario Name:"),
                                              tags$td(width = "200",  textInput("input_custom_scenarioName", label = NULL, width=195, value = ""))
                                      )
                                  ),
                                  div(
                                      conditionalPanel(
                                          condition = "input.input_custom_RCP == 'RCP-2.6'",
                                          a(h6("Download RCP 2.6 Emissions File Template"), href="input/emissions/RCP26_custom_template.csv")
                                      ),
                                      conditionalPanel(
                                          condition = "input.input_custom_RCP == 'RCP-4.5'",
                                          a(h6("Download RCP 4.5 Emissions File Template"), href="input/emissions/RCP45_custom_template.csv")
                                      ),
                                      conditionalPanel(
                                          condition = "input.input_custom_RCP == 'RCP-6.0'",
                                          a(h6("Download RCP 6.0 Emissions File Template"), href="input/emissions/RCP6_custom_template.csv")
                                      ),
                                      conditionalPanel(
                                          condition = "input.input_custom_RCP == 'RCP-8.5'",
                                          a(h6("Download RCP 8.5 Emissions File Template"), href="input/emissions/RCP85_custom_template.csv")
                                      ),
                                      fileInput("input_custom_emissions_file", "Upload Custom Emissions File:", width=275, buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
                                      div(
                                          class="paramDivs", actionButton(inputId="input_load_emissions", label="Create Scenario"))
                                  )
                              ) # End Div
                          ) # End Custom Scenarios Tab Panel
                      )
                  ),
                  # Right hand content panel - Main panel that is used for output
                  mainPanel
                  ( width = 8,
                      tabsetPanel
                      (
                          # Graphs Tab
                          tabPanel
                          (fixed = TRUE,
                              p(icon("chart-line","fa-2x"), "Scenario Output", value="outputTab"),
                              div(
                              tags$table
                              (
                                  tags$tr
                                  (class = "output-vars",
                                      tags$td
                                      (
                                          selectInput
                                          ("capabilities",  "Choose Output Variables (up to 4):",
                                              list('Carbon Cycle' = list("Atmospheric CO2"="cc_co2", "Atmospheric Carbon Pool"="cc_acp", "FFI Emissions"="cc_ffi", "LUC Emissions"="cc_luc"),
                                                   'Concentrations' = list("Amospheric N2O"='c_an20'),
                                                   'Emissions' = list("Black Carbon Emissions" = 'e_bc', "Organic Carbon Emissions"='e_oc'), #"RF - H2O"='f_h2o', "RF - Ozone"='f_oz',
                                                   'Forcings' = list("RF - Total"='f_rft', "RF - Albedo"='f_alb', "RF - CO2"='f_co2', "RF - N2O"='f_n2o',  "RF - Black Carbon"='f_bc',
                                                                     "RF - Organic Carbon"='f_oc', "RF - SO2 Direct"='f_so2d', "RF - SO2 Indirect"='f_so2i', "RF - SO2 Total"='f_so2t', "RF - Volcanic Activity"='f_va', "RF - CH4"='f_ch4'),
                                                   # 'Halocarbon Emissions' = list("CF4 Emissions"='he_cf4', "C2F6 Emissions"='he_c2f6', "HFC-23 Emissions"='he_hfc23', "HFC-32 Emissions"='he_hfc32', "HFC-4310 Emissions"= 'he_hfc4310', "HFC-125 Emissions"='he_hfc125', "HFC-134a Emissions"='he_hfc134a', "HFC-143a Emissions"='he_hfc143a', "HFC-227ea Emissions"='he_hfc227ea',
                                                   #                               "HFC-245fa Emissions"='he_hfc245fa', "SF6 Emissions"='he_sf6', "CFC-11 Emissions"='he_cfc11', "CFC-12 Emissions"='he_cfc12', "CFC-113 Emissions"='he_cfc113',"CFC-114 Emissions"='he_cfc114',"CFC-115 Emissions"='he_cfc115',"CCl4 Emissions"='he_ccl4', "CH3CCl3 Emissions"='he_ch3ccl3',
                                                   #                               "HCFC-22 Emissions"='he_hcfc22', "HCFC-141b Emissions"='he_hcfc141b', "HCFC-142b Emissions"='he_hcfc142b', "Halon-1211 Emissions"='he_halon1211', "Halon-1301 Emissions"='he_halon1301', "Halon-2402 Emissions"='he_halon2402', "CH3Cl Emissions"='he_ch3cl', "CH3Br Emissions"='he_ch3br'),
                                                   'Halocarbon Forcings' = list("CF4 Forcing"='hf_cf4', "C2F6 Forcing"='hf_c2f6', "HFC-23 Forcing"='hf_hfc23', "HFC-4310 Forcing"='hf_hfc4310', "HFC-125 Forcing"='hf_hfc125',  "HFC-143a Forcing"='hf_hfc143a',
                                                                                "HFC-254fa Forcing"='hf_hfc245fa', "SF6 Forcing"='hf_hfcsf6', "CFC-11 Forcing"='hf_cfc11', "CFC-12 Forcing"='hf_cfc12', "CFC-113 Forcing"='hf_cfc113',"CFC-114 Forcing"='hf_cfc114',"CFC-115 Forcing"='hf_cfc115',"CCl4 Forcing"='hf_ccl4', "CH3CCl3 Forcing"='hf_ch3ccl3',
                                                                                "Halon-1211 Forcing"='hf_halon1211', "Halon-1301 Forcing"='hf_halon1301', "Halon-2402 Forcing"='hf_halon2402', "CH3Cl Forcing"='hf_ch3cl', "CH3Br Forcing"='hf_ch3br'), #"HCFC-22 Forcing"='hf_hcfc22', "HCFC-141b Forcing"='hf_hcfc141b', "HCFC-142b Forcing"='hf_hcfc142b',
                                                   'Methane' = list("Atmospheric CH4"='m_a_ch4',  "Emissions CH4"='m_e_ch4'),
                                                   # 'Ocean' = list("Ocean Carbon Flux"='o_cf', "Ocean Total Carbon"='o_tc', "Ocean Surface High-Lat Carbon"='o_os_hlc', "Ocean Surface Low-Lat Carbon"='o_os_llc', "Ocean Intermediate Carbon"='o_ic', "Ocean Deep Carbon"='o_dc', "Thermohaline Overturning"='o_to',
                                                   #                "High-Lat Overturning"='o_hl_o', "Warm-Intermediate Exchange"='o_wie', "Intermediate-Deep Exchange"='o_ide', "High Latitude Ph"='o_hl_ph', "Low Latitude Ph"='o_ll_ph', "Atmosphere-Ocean Flux - High Lat"='o_hl_aof', "Atmosphere-Ocean Flux - Low Lat"='o_ll_aof',
                                                   #                "Partial Pressure CO2 - High Lat"='o_hl_pp_co2',"Partial Pressure CO2 - Low Lat"='o_ll_pp_co2',"Dissolved Inorganic C - High Lat"='o_hl_dic', "Dissolved Inorganic C - Low Lat"='o_ll_dic', "Ocean Temperature - High Lat"='o_hl_t', "Ocean Temperature - Low Lat"='o_ll_t',
                                                   #                "Carbonate Concentration - High Lat"='o_hl_cc', "Carbonate Concentration - Low Lat"='o_ll_cc'), #  "Land Temp Anomaly"="t_lta",
                                                   'SO2' = list( "Anthropogenic SO2"='so2_a', "Natural CH4 Emissions"='so2_n_ch4', "Volcanic SO2"='so2_v'),
                                                   'Temperature' = list("Global Mean Temp"='t_gmt', "Equilibrium Global Temp"='t_egt', "Ocean Surface Temp"='t_ost', "Ocean Air Temp"='t_oat', "Heat Flux - Mixed Layer Ocean"='t_hf_mlo', "Heat Flux - Interior Layer Ocean"='t_hf_ilo', "Total Heat Flux - Ocean"='t_hf_t')),
                                              multiple = T, selected = "t_gmt")
                                      ),
                                  )
                              ),
                              actionButton(inputId="loadGraphs", label="Load Graphs", width = 200, style = "background: #0B3F8F; color: white;"),
                              downloadButton("downloadData", label="Download Raw Data", style = "background: #0B3F8F; color: white;"),
                              br(),
                              br(),
                              uiOutput("plots", class = "customPlot")
                              )
                              ), # end Graphs Tab

                          # Maps Tab
                          tabPanel
                          (class = "maps",
                              p(icon("globe-americas","fa-2x"), "World Maps", value="outputTab"),
                              br(),
                              h6("These maps are generated using ",
                                 a("fldgen,", href = "https://doi.org/10.1371/journal.pone.0223542", target = "_blank"), "a tool used to spatially downscale Hector temperature output onto the Earth System Model grid of your choosing."),
                              br(),
                              p("Please note that some map model patterns are rather large and can take several seconds to load. Due to this, maps will need to be refreshed manually after any changes."),
                              fluidRow(
                                  column(3, selectInput(inputId = "mapPattern", label = "Choose Model:", width = "100%",
                                                     choices = c("CanESM2" = "CanESM2",
                                                                 "CESM1-BGC" = "CESM1-BGC",
                                                                 "GFDL-ESM2G" = "GFDL-ESM2G",
                                                                 "MIROC-ESM" = "MIROC-ESM",
                                                                 "MPI-ESM-LR" = "MPI-ESM-LR",
                                                                 "MRI-ESM1" = "MRI-ESM1"))
                                         ),
                                  column(3, selectInput(inputId = "mapVar", label = "Choose Variable:", selected = "tas", width = "100%",
                                                     choices = c("Temperature" = "tas", "Precipitation" = "pr"), multiple = F)
                                         ),
                                  column(3, selectInput(inputId = "mapYear", label = "Choose Year:", selected = 2100, width = "100%",
                                                     choices = c(2000:2100), multiple = F)
                                         ),
                                  column(3, uiOutput("coreMapping"))




                                  # tags$tr(class = "maps",
                                  #     tags$td(
                                  #         selectInput(inputId = "mapPattern", label = "Choose Model:", width = "100%",
                                  #                     choices = c("CanESM2" = "CanESM2",
                                  #                                 "CESM1-BGC" = "CESM1-BGC",
                                  #                                 "GFDL-ESM2G" = "GFDL-ESM2G",
                                  #                                 "MIROC-ESM" = "MIROC-ESM",
                                  #                                 "MPI-ESM-LR" = "MPI-ESM-LR",
                                  #                                 "MRI-ESM1" = "MRI-ESM1"))
                                  #     ),
                                  #     tags$td(style="padding-left: 5px;",
                                  #             selectInput(inputId = "mapVar", label = "Choose Variable:", selected = "tas", width = "100%",
                                  #                         choices = c("Temperature" = "tas", "Precipitation" = "pr"), multiple = F)
                                  #     ),
                                  #     tags$td(style="padding-left: 5px;",
                                  #             selectInput(inputId = "mapYear", label = "Choose Year:", selected = 2100, width = "100%",
                                  #                         choices = c(2000:2100), multiple = F)
                                  #     ),
                                  #     tags$td(style="padding-left: 5px;", width = "100%",
                                  #             uiOutput("coreMapping")
                                  #     )
                                  # )
                              ),
                              tags$table(
                                  tags$tr(width = "100%",
                                          tags$td(width = "20", align="left", class="tdPushTop",
                                                  popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Enable Comparative Differences",
                                                         content = "This will change the temperature output on the map to show the difference between the computed temperature of the selected year and the temperature from the year 1900.",
                                                         placement = "top")),
                                          tags$td(shinyWidgets::prettyCheckbox(inputId = "input_map_compare", label = "Show as comparison to year 1900", value = FALSE,  inline = FALSE, icon = icon("check")))
                                  )
                              ),
                              tags$table(
                                  tags$tr(width = "100%",
                                          tags$td(width = "20", align="left", class="tdPushTop",
                                                  popify(div(class="paramDivs", icon("info-circle", "fa-1x")), title = "Filter by Lat/Lon",
                                                         content = "Choosing this option will allow you to zoom in to a specific lat/lon region and have the map rescale to the regional data.",
                                                         placement = "top")),
                                          tags$td(shinyWidgets::prettyCheckbox(inputId = "input_map_filter", label = "Filter by Lat/Lon", value = FALSE,  inline = TRUE, icon = icon("check"))
                                          )
                                  ),
                              ),
                              conditionalPanel(condition = "input.input_map_filter == true",
                                               tags$table(
                                                   tags$tr(width = "100%",
                                                           tags$td(align = "left", shiny::textInput(inputId = "input_lat_min", label = "Lat Min:", width = 65, value = 13)),
                                                           tags$td(width = 15),
                                                           tags$td(align = "left", shiny::textInput(inputId = "input_lat_max", label = "Lat Max:",  width = 65, value = 57)),
                                                           tags$td(width = 15),
                                                           tags$td(align = "left", shiny::textInput(inputId = "input_lon_min", label = "Lon Min:", width = 65, value = -135)),
                                                           tags$td(width = 15),
                                                           tags$td(align = "left", shiny::textInput(inputId = "input_lon_max", label = "Lon Max:", width = 65, value = -55)),
                                                           tags$td()
                                                   )
                                               ),
                              ),
                              actionButton(inputId="loadMaps", label="Load Map", width = 150),
                              downloadButton("downloadMap", label="Save Hi-Res Map", width = 150),
                              br(),
                              uiOutput("maps", class = "customPlot")
                          ) # End Maps Tab

                              ) # End tabset panel
                  ) # End mainpanel
              ),
              tabPanel
              (
                  "About",
                  # Main Panel that holds the tabs for the Information section
                  mainPanel
                  (
                      width = 9,
                      class = "about-info",
                      tabsetPanel
                      (
                          # Information Tab Panel
                          tabPanel
                          (
                              p(icon("info-circle", "fa-2x"), "Hector Information", value="infoTab"),
                              h5("Background Information"), tags$hr(class="hrNav"),
                              tags$table(
                                  tags$tr(
                                      tags$td(width = "50%",
                                              h5("Explore the Hector Product Family", style="text-align: left"),
                                              br(),
                                              h6("Source code and contribution is available on the ",
                                                a("HectorUI Github page", href = "https://github.com/JGCRI/hector", target = "_blank")),
                                      ),
                                  )
                              ),
                              br(),
                              tags$td(
                                  tags$figure(
                                  img(src='https://github.com/JGCRI/hector/wiki/rcp85.png',
                                      height="260px",
                                      class="hector-fig"),
                                  tags$figcaption("Hector's global temperature rise for RCP 8.5 scenario, compared to observations and other model results")
                              ), style="text-align: center"
                              ),
                              br(),
                              h5("Documentation/Downloads"),
                              tags$hr(class="hrNav"),
                              p("The primary link to the Hector model documentation is the ",
                                a("online manual", href="https://jgcri.github.io/hector/articles/manual", target="blank"),
                                ", which is included in the vignettes/manual directory. The code is also documented with ",
                                a("Doxygen-style", href="http://doxygen.org", target="blank"),
                                " comments."),
                              p("A formal model description paper via ",
                                a("Hartin et al. 2015", href="http://www.geosci-model-dev.net/8/939/2015/gmd-8-939-2015.html", target="blank"),
                                " documents its science internals and performance relative to observed data, the ",
                                a("CMIP5", href="http://cmip-pcmdi.llnl.gov/cmip5/",  target="blank"),
                                " archive, and the reduced-complexity ",
                                a("MAGICC",href="http://www.magicc.org", target="blank"),
                                " model (as of ",
                                a("version 1.0", href="https://github.com/JGCRI/hector/tree/v1.0", target="blank"),
                                "). In addition, we have developed two package vignettes demonstrating the ",
                                a("basics of the Hector R interface", href="http://jgcri.github.io/hector/articles/intro-to-hector.html", target="blank"),
                                ", and an example application of ",
                                a("solving for an emissions pathway", href="http://jgcri.github.io/hector/articles/hector_apply.html", target="blank"),
                                "."),
                              tags$ul(
                                  tags$li(
                                      h5(tags$a("Hector User Interface package download/source link ", href="https://github.com/JGCRI/Hector-ui", target="blank"))),
                                  tags$li(
                                      h5(tags$a("Hector R / Hector C++ package download/source link", href="https://github.com/JGCRI/Hector", target="blank"))),
                                  tags$li(
                                      h5(tags$a("Code and data for Hector calibration papers", href="https://zenodo.org/record/3515153#.Xg9iGuhKiUl", target="blank"),
                                         tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3515153.svg", class="zenodo", alt="DOI")
                                      )
                                  )
                              ),
                              br(),
                              h5("Tools and software that work with Hector"),
                              tags$hr(class="hrNav"),
                              tags$ul(
                                  tags$li(
                                      a("GCAM", href="https://github.com/JGCRI/gcam-core", target="blank"),
                                      ": Hector can be used as the climate component in the GCAM integrated assessment model."),
                                  tags$li(
                                      a("pyHector", href="https://github.com/openclimatedata/pyhector", target="blank"),
                                      ": A python interface to Hector."),
                                  tags$li(
                                      a("R/Shiny", href="https://shiny.rstudio.com/", target="blank"),
                                      ": This application was built as an R-Shiny package wrapper over the existing model code.")
                              )
                          ),
                          # Citation Tab Panel
                          tabPanel
                          (
                              p(icon("copyright", "fa-2x"),
                                "License/How to Cite",
                                value="citeTab"),
                              h5("License Information"),
                              tags$hr(class="hrNav"),
                              tags$div(class="citationsDiv", style="width: 500px;",
                                       tags$table(class="citationsTable",
                                                  tags$tr(
                                                      tags$td(rowspan=2, width=45, icon("balance-scale", "fa-2x")),
                                                      tags$td(("All Hector applications are licensed under the")
                                                      ),
                                                      tags$tr(
                                                          tags$td(
                                                              tags$a(
                                                                  h6("GNU General Public License v3.0"), href="https://github.com/JGCRI/hector/blob/master/LICENSE.md", target="blank")
                                                          )
                                                      )
                                                  ),
                                                  tags$table(class="citationsTable",
                                                             tags$tr(
                                                                 tags$td(p("Permissions of this strong copyleft license are conditioned on making available complete source code
                                                               of licensed works and modifications, which include larger works using a licensed work, under the same
                                                               license. Copyright and license notices must be preserved. Contributors provide an express grant of patent rights.")
                                                                 )
                                                             )
                                                  )
                                       )
                              ),
                              div(class="citationsDiv",
                                  tags$span(class="citationsHeader", "Permissions"),
                                  tags$ul(class="ul-None",
                                          tags$li(icon("check"),"Commercial use"),
                                          tags$li(icon("check"),"Modification"),
                                          tags$li(icon("check"),"Distribution"),
                                          tags$li(icon("check"),"Patent use"),
                                          tags$li(icon("check"),"Private use"))
                              ),
                              div(class="citationsDiv",
                                  tags$span(class="citationsHeader", "Limitations"),
                                  tags$ul(class="ul-None",
                                          tags$li(icon("times")," Liability"),
                                          tags$li(icon("times")," Warranty"))
                              ),
                              div(class="citationsDiv",
                                  tags$span(class="citationsHeader", "Conditions"),
                                  tags$ul(class="ul-None",
                                          tags$li(icon("info"),"License and copyright notice"),
                                          tags$li(icon("info"),"State changes"),
                                          tags$li(icon("info"),"Disclose source"),
                                          tags$li(icon("info"),"Same license"))
                              ),
                              h5("How to Cite Hector"),
                              tags$hr(class="hrNav"),
                              p("When using graphs, figures, or other output from this applicaton please cite both the Hector Core",
                                br(),
                                "application as well as the Hector User Interface (this application). The DOI for both is provided below:"),
                              tags$ul(
                                  tags$li(
                                      h5(tags$a("Hector User Interface DOI", href="https://doi.org/10.5281/zenodo.3603216", target = "blank"),
                                         tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3603216.svg", alt="DOI", class = "imgNoPadding"))),
                                  tags$li(
                                      h5(tags$a("Hector Core DOI", href="https://doi.org/10.5281/zenodo.3515153", target = "blank"),
                                         tags$img(src="https://zenodo.org/badge/DOI/10.5281/zenodo.3515153.svg", alt="DOI", class = "imgNoPadding")))
                              )
                          ),
                          # Feedback Tab Panel
                          tabPanel
                          (
                              p(icon("comment", "fa-2x"), "Support", value="feedbackTab"),
                              h5("Submit Feedback"), tags$hr(class="hrNav"),
                              p("Please use the form below to contact the Hector team regarding any questions, concerns, suggestions, or problems you may encounter."),
                              htmlOutput("feedbackFrame")
                          )
                      )
                  )
              ),
              tags$style(HTML(".navbar-nav {
                                float:none;
                                margin:0 auto;
                                display: block;
                                text-align: center;
                                color: #000000;
                            }

                            .navbar-nav > li {
                                display: inline-block;
                                float:none;
                                color: #000000;
                            }"))
               ), # End navbarpage
          hr(),
          p(em("This research was supported in part by the",
            a(" U.S. Department of Energy,", href = "https://www.energy.gov/"),
            a(" Office of Science", href = "https://www.energy.gov/science/office-science"),
            ", as part of research in",
            a("MultiSector Dynamics, ", href = "https://climatemodeling.science.energy.gov/program/multisector-dynamics"),
            "Earth and Environmental System Modeling Program.",
            a("The Pacific Northwest National Laboratory", href = "https://www.pnnl.gov/"),
            " is operated for DOE by Battelle Memorial Institute.
            The authors also received support for this research through the U.S. Environmental Protection Agency."),
            class = "sticky_footer", style = "font-size:12px;")
) # End of everything.

