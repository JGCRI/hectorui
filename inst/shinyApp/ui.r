#
# This is a Shiny web application.

# Define UI for application
fluidPage(
  shinyalert::useShinyalert(),
  # Application title
  titlePanel("Hector"),

  # Function that gets called on first load of application to load in any themes/css etc
  tags$head
  (
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),

  # Main component from which all other components fall under, navbarPage.
  navbarPage
  (
    "Hector",
    # multi-page user-interface that includes a navigation bar.
    tabPanel
    (
      "Run Scenario",
      sidebarPanel
      (
        width=3,
        div(h4("Baseline Scenarios"),
            selectInput("input_RCP", "RCP Scenario", list("2.6" = "RCP 2.6","4.5"="RCP 4.5", "6"="RCP 6.0", "8.5" = "RCP 8.5"), width=200, selected = "RCP 4.5"),
            radioButtons("input_Driven", "", list("Emissions Driven"), inline=TRUE),
            textInput("input_ScenarioName", "Scenario Name", width=200, value = ""),

        div(h4("Custom Scenario"),
            fileInput("input_ScenarioFile", "Upload Emissions File", width=200, buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
            actionButton(inputId="input_Driven", label="Load Scenario"))
        ),
        div
        (
          h4("Model Parameters"),
          numericInput("input_aero", "Aerosol forcing scaling factor (unitless)", width=250, value = NA, step = 0.01),
          numericInput("input_beta", "CO2 fertilization factor (unitless)", width=250, value = NA, step = 0.01),
          numericInput("input_diff", "Ocean heat diffusivity (cm2/s)", width=250, value = NA, step = 0.01),
          numericInput("input_ecs", "Equilibrium Climate Sensitivity (degC)", width=250, value = NA, step = 0.01),
          numericInput("input_pco2", "Preindustrial CO2 concentration (ppmv CO2)", width=250, value = NA, step = 0.01),
          numericInput("input_q10", "Heterotrophic respiration temp sensitivity factor (unitless)", width=250, value = NA, step = 0.01),
          numericInput("input_volc", "Volcanic forcing scaling factor (unitless)", width=250, value = NA, step = 0.01),
          actionButton(inputId="set_Params", label="Set Parameters"),
          actionButton(inputId="reset_Params", label="Reset Parameters")
        )
      ),
      mainPanel
      (
        tabsetPanel
        (
          # Information Tab Panel
          tabPanel
          (
            p(icon("table"), "Scenario Information", value="infoTab"),
            p("test information")
          ),
          tabPanel
          ( fluid = TRUE,
            p(icon("table"), "Scenario Output", value="outputTab"),
            # Output Tab Panel
            mainPanel
            (
              p(selectInput
                ("capabilities", "Choose capability identifiers:",
                  list('Carbon Cycle' = list("Atmospheric CO2"="cc_co2", "Atmospheric Carbon Pool"="cc_acp"),
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
                       'Temperature' = list("Global Mean Temp"='t_gmt', "Equilibrium Global Temp"='t_egt', "Ocean Surface Temp"='t_ost', "Ocean Air Temp"='t_oat', "Land Temp Anomaly"="t_lta", "Heat Flux - Mixed Layer Ocean"='t_hf_mlo', "Heat Flux - Interior Layer Ocean"='t_hf_ilo', "Total Heat Flux - Ocean"='t_hf_t')                  ), multiple = T
                ),  actionButton(inputId="loadGraphs", label="Load Graphs", width = 200), downloadButton("dlData", label="Download Raw Data")
               ),
              # plotOutput("plot"),
              plotly::plotlyOutput("plot2")
               #div(width = '100%', plotOutput("plot"))
            ) # end mainpanel
          ) # end tabpanel
        ) # end tabsetpanel
      ), # end mainpanel
      tabPanel
      (
        p(icon("search"), "Custom CSV File Instructions")
      )
    )
  )
)

#)
#disable(input_Driven)
# Run the application
#shinyApp(ui = ui, server = server)

# sliderInput("timeline",
#             "Timeline:",
#             min = 1950,
#             max = 2017,
#             value = c(1997, 2015)),

#format = "####"),

# actionButton(inputId = "selectAllTop",
#              label = "Select all",
#              icon = icon("check-square-o"))
# uiOutput("themesControl"), # the id

