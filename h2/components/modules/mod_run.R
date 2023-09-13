# Run Hector using R6 module

run_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("ssp_path"),
      label = "Select SSP:",
      choices = list(
        "SSP 1-1.9" = "input/hector_ssp119.ini",
        "SSP 1-2.6" = "input/hector_ssp126.ini",
        "SSP 2-4.5" = "input/hector_ssp245.ini",
        "SSP 3-7.0" = "input/hector_ssp370.ini",
        "SSP 4-3.4" = "input/hector_ssp434.ini",
        "SSP 4-6.0" = "input/hector_ssp460.ini",
        "SSP 5-3.4OS" = "input/hector_ssp534-over.ini",
        "SSP 5-8.5" = "input/hector_ssp585.ini"
      ),
      selected = "input/hector_ssp119.ini"
    ),
    sliderInput(
      ns("start"),
      label = "Select start date:",
      min = 1750,
      max = 2300,
      value = 2000,
      sep = ""
    ),
    sliderInput(
      ns("end"),
      "Select end date:",
      min = 1750,
      max = 2300,
      value = 2300,
      sep = ""
    ),
    actionButton(ns("run"), "Run Model"),
    verbatimTextOutput(ns("done"))
  )
}

run_server <- function(id, r6, i) {
  moduleServer(id, function(input, output, session) {
    observe({
      # store inputs in r6 class
      r6$ini_file <-
        reactive({
          system.file(input$ssp_path, package = "hector")
        })
      r6$start <- reactive({
        input$start
      })
      r6$end <- reactive({
        input$end
      })
      
      # run hector using inputs
      print("Running...") # in command line
      core <- newcore(r6$ini_file())
      run(core)
      r6$output[[i()]] <-
        fetchvars(core, r6$start():r6$end()) %>% mutate(run = i())
      output$done <-
        renderPrint({
          as.character(i() - 1)
        }) #print run number
      print("Done") # in command line
      
      i(i() + 1) # add 1 to i. like a pseudo loop for storing output
      print(r6$output)
      #print(i())
    }) %>%
      bindEvent(input$run) # triggers when "Run Model" is clicked
  })
}