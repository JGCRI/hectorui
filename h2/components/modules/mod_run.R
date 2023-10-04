# Run Hector using R6 module

run_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    textInput(ns("core_name"), "Input name for core:", placeholder="Unnamed Hector core"),
    selectInput(ns("ssp_path"), label="Select SSP:",
                choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                               "SSP 1-2.6"="input/hector_ssp126.ini",
                               "SSP 2-4.5"="input/hector_ssp245.ini",
                               "SSP 3-7.0"="input/hector_ssp370.ini",
                               "SSP 4-3.4"="input/hector_ssp434.ini",
                               "SSP 4-6.0"="input/hector_ssp460.ini",
                               "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                               "SSP 5-8.5"="input/hector_ssp585.ini"),
                selected = "input/hector_ssp119.ini"),
    sliderInput(ns("start"), label="Select start date:",
                min = 1750, max = 2300, value = 2000, sep=""),
    sliderInput(ns("end"), "Select end date:",
                min = 1750, max = 2300, value = 2300, sep=""),
    # radioButtons(ns("run_number"), label="Select run number:",
    #              choices = list("1" = 1,
    #                             "2" = 2,
    #                             "3" = 3,
    #                             "4" = 4,
    #                             "5" = 5,
    #                             "6" = 6,
    #                             "7" = 7,
    #                             "8" = 8),
    #              selected = "1", inline=TRUE),
    #actionButton(ns("runsave"),"Run and Save"),
    materialSwitch(ns("savetoggle"),"Save Run"),
    actionButton(ns("run"),"Run") #not actually hooked up to anything yet lol
    #verbatimTextOutput(ns("done")),
    #actionButton(ns("stop"),"show warning")
  )
}

run_server <- function(id, r6, i) {
  moduleServer(id, function(input, output, session) {
    
    # observe({
    #   # store inputs in r6 class
    #   r6$ini_file <- reactive({system.file(input$ssp_path,package="hector")})
    #   r6$start <- reactive({input$start})
    #   r6$end <- reactive({input$end})
    #   r6$i <- reactive({as.integer(input$run_number)})
    # 
    #   # sendSweetAlert(session = session,
    #   #                html = TRUE,
    #   #                text = tagList(
    #   #                  numericInput("num","Select save slot:", 1)
    #              # ))
    #   #browser()
    #   r6$i <- reactive({as.integer(input$run_number)})
    #   
    #   # run hector using inputs
    #   print("Running...") # in command line
    #   core <- reactive({newcore(r6$ini_file(),name=input$core_name)})
    #   run(core())
    #   #browser()
    #   r6$output[[r6$i()]] <- fetchvars(core(),r6$start():r6$end()) %>% mutate(run=r6$i())
    #   output$done <- renderPrint({r6$i()}) #print run number
    #   print("Done") # in command line
    #   
    #   #i(i() + 1) # add 1 to i. like a pseudo loop for storing output
    #   print(r6$output)
    #   #print(i())
    #   
    #   # if (length(r6$output) == 8 &&
    #   #     identical(
    #   #       lapply(r6$output, is.null),
    #   #       list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    #   #     ))
    #   #   shinyalert(title = "Run Limit Reached",
    #   #              text = "You have completed eight runs. Select a run to replace.",
    #   #              type = "input",
    #   #              inputType = "number")
    # }) %>%
    #   bindEvent(input$run) # triggers when "Run Model" is clicked
    
    observe({
      if (input$savetoggle == TRUE) {
        # store inputs in r6 class
        r6$ini_file <- reactive({system.file(input$ssp_path,package="hector")})
        r6$start <- reactive({input$start})
        r6$end <- reactive({input$end})
        #r6$i <- reactive({as.integer(input$run_number)})
        
        sendSweetAlert(session = session,
                       html = TRUE,
                       text = tagList(
                         radioButtons("run_number", label="Select run number:",
                                      choices = list("1" = 1,
                                                     "2" = 2,
                                                     "3" = 3,
                                                     "4" = 4,
                                                     "5" = 5,
                                                     "6" = 6,
                                                     "7" = 7,
                                                     "8" = 8),
                                      selected = "1", inline=TRUE),
        ))
        
        # run hector using inputs
        browser()
        r6$i <- reactive({input$run_number})
        print("Running...") # in command line
        core <- reactive({newcore(r6$ini_file(),name=input$core_name)})
        run(core())
        #browser()
        r6$output[[r6$i()]] <- fetchvars(core(),r6$start():r6$end()) %>% mutate(run=r6$i())
        output$done <- renderPrint({r6$i()}) #print run number
        print("Done") # in command line
        r6$save <- TRUE
      }
      if (input$savetoggle == FALSE) {
        r6$ini_file <- reactive({system.file(input$ssp_path,package="hector")})
        r6$start <- reactive({input$start})
        r6$end <- reactive({input$end})
        
        print("Running...") # in command line
        core <- reactive({newcore(r6$ini_file(),name=input$core_name)})
        run(core())
        
        r6$no_save <- fetchvars(core(),r6$start():r6$end())
        print("Done")
        r6$save <- FALSE
      }
    }) %>%
      bindEvent(input$run)
  })
}