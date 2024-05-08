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
                             tags$td(width = "155", selectInput(ns("input_custom_SSP"), label = NULL, list("SSP 1-1.9" = "input/tables/ssp119_emiss-constraints_rf.csv",
                                                                                                       "SSP 1-2.6" = "input/tables/ssp126_emiss-constraints_rf.csv",
                                                                                                       "SSP 2-4.5" = "input/tables/ssp245_emiss-constraints_rf.csv",
                                                                                                       "SSP 3-7.0" = "input/tables/ssp370_emiss-constraints_rf.csv",
                                                                                                       "SSP 4-3.4" = "input/tables/ssp434_emiss-constraints_rf.csv",
                                                                                                       "SSP 4-6.0" = "input/tables/ssp460_emiss-constraints_rf.csv",
                                                                                                       "SSP 5-3.4OS" = "input/tables/ssp534-over_emiss-constraints_rf.csv",
                                                                                                       "SSP 5-8.5" = "input/tables/ssp585_emiss-constraints_rf.csv"), 
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
                         href=system.file("input/tables/ssp119_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 1-2.6'",
                       a(h6("Download SSP 1-2.6 Emissions File Template"), 
                         href=system.file("input/tables/ssp126_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 2-4.5'",
                       a(h6("Download SSP 2-4.5 Emissions File Template"), 
                         href=system.file("input/tables/ssp245_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 3-7.0'",
                       a(h6("Download SSP 3-7.0 Emissions File Template"), 
                         href=system.file("input/tables/ssp370_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 4-3.4'",
                       a(h6("Download SSP 4-3.4 Emissions File Template"), 
                         href=system.file("input/tables/ssp434_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 4-6.0'",
                       a(h6("Download SSP 4-6.0 Emissions File Template"), 
                         href=system.file("input/tables/ssp460_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 5-3.4OS'",
                       a(h6("Download SSP 5-3.4OS Emissions File Template"), 
                         href=system.file("input/tables/ssp534-over_emiss-constraints_rf.csv",package="hector"))
                     ),
                     conditionalPanel(
                       condition = "input.input_custom_SSP == 'SSP 5-8.5'",
                       a(h6("Download SSP 5-8.5 Emissions File Template"), 
                         href=system.file("input/tables/ssp585_emiss-constraints_rf.csv",package="hector"))
                     ),
                     fileInput(ns("input_custom_emissions_file"), "Upload Custom Emissions File:", width=275, buttonLabel = "Choose File", accept = c("text/csv", ".csv", "text/comma-separated-values,text/plain")),
                     div(
                       class="paramDivs", actionButton(inputId=ns("input_load_emissions"), label="Create Scenario"))
                   )
                 ) # End Div
        ) # End Custom Scenarios Tab Panel
      )
    )
  )
}

custom_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    observe({
      print("test")
    }) %>% bindEvent(input$input_load_emissions)
  })
}