# Run the model with custom emissions
custom_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    sidebarPanel(
      tabsetPanel(
        tabPanel(class = "params", "Custom Scenarios",
                 div
                 (
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
                             tags$td(width = "155", selectInput(ns("input_custom_SSP"), label = NULL, list("SSP 1-1.9" = "SSP 1-1.9",
                                                                                                       "SSP 1-2.6" = "SSP 1-2.6",
                                                                                                       "SSP 2-4.5" = "SSP 2-4.5",
                                                                                                       "SSP 3-7.0" = "SSP 3-7.0",
                                                                                                       "SSP 4-3.4" = "SSP 4-3.4",
                                                                                                       "SSP 4-6.0" = "SSP 4-6.0",
                                                                                                       "SSP 5-3.4OS" = "SSP 5-3.4OS",
                                                                                                       "SSP 5-8.5" = "SSP 5-8.5"), 
                                                                width=150, selected = "SSP 2-4.5"))
                     ),
                     tags$tr(width = "100%",
                             tags$td(width = "145", "Your Scenario Name:"),
                             tags$td(width = "200",  textInput(ns("input_custom_scenarioName"), label = NULL, width=195, value = ""))
                     )
                   ),
                   div(
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 1-1.9'",
                       a(h6("Download SSP 1-1.9 Emissions File Template"),
                         href="C:/Users/done231/OneDrive - PNNL/Documents/GitHub/hectorui/h2/components/inputs/ssp_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 1-2.6'",
                       a(h6("Download SSP 1-2.6 Emissions File Template"), 
                         href="components/inputs/ssp126_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 2-4.5'",
                       a(h6("Download SSP 2-4.5 Emissions File Template"), 
                         href="components/inputs/ssp245_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 3-7.0'",
                       a(h6("Download SSP 3-7.0 Emissions File Template"), 
                         href="components/inputs/ssp370_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 4-3.4'",
                       a(h6("Download SSP 4-3.4 Emissions File Template"), 
                         href="components/inputs/ssp434_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 4-6.0'",
                       a(h6("Download SSP 4-6.0 Emissions File Template"), 
                         href="components/inputs/ssp460_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 5-3.4OS'",
                       a(h6("Download SSP 5-3.4OS Emissions File Template"), 
                         href="components/inputs/ssp534-over_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 5-8.5'",
                       a(h6("Download SSP 5-8.5 Emissions File Template"), 
                         href="components/inputs/ssp585_emiss-constraints_rf.csv"),
                       ns=NS(id)
                     ),
                     fileInput(ns("input_custom_emissions_file"), "Upload Custom Emissions File:", width=275, buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
                     div(
                       class="paramDivs", actionButton(ns("input_load_emissions"), label="Create Scenario"))
                   )
                 ) # End Div
        ) # End Custom Scenarios Tab Panel
      )
    ),
    mainPanel(
      plotlyOutput(ns("plot")),
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
      scenarioName <- input$input_custom_scenarioName
      sspName <- input$input_custom_SSP
      
      # Read in input data
      inifile <- system.file("input/hector_ssp119.ini",package="hector")#system.file(input$input_custom_SSP, package='hector', mustWork=TRUE)
      file <- input$input_custom_emissions_file
      emissions_data <- read.csv(file=file$datapath, header=TRUE, sep=",", skip = 5)
      emissions_headers <- read.csv(file=file$datapath, header=FALSE, sep=",", skip = 4)
      dates_col <- emissions_data$Date

      
      core <- newcore(inifile, suppresslogging=TRUE, name=scenarioName)
      run(core)
      
      # set vars
      for(i in c(2:ncol(emissions_data))) {
        setvar(core = core, dates = emissions_data[, 1],var = colnames(emissions_data)[i], values = emissions_data[, i], unit = as.character(emissions_headers[[paste0("V",i)]][[1]]))
      }
      
      reset(core)
      run(core)
      data <- fetchvars(core,2000:2100)
      
      # Plot
      output$plot <- renderPlotly(ggplot(data = data) +
        geom_line(aes(year, value)) +
        facet_wrap("variable", scales = "free") +
        labs(title = scenarioName,
             subtitle = sspName,
             x = NULL) +
        theme(axis.text.x = element_text(angle = 90)))
      
    }) %>% bindEvent(input$input_load_emissions)
  })
}