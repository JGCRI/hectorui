# Function to load in custom emissions

loadCustomEmissions <- function() {
  print("in load custom")
  
  if (is.null(input$input_custom_emissions_file) | (is.na(input$input_custom_scenarioName) | is.null(input$input_custom_scenarioName) | (input$input_custom_scenarioName == "")))
  {
    shinyalert::shinyalert("Missing Information", "Please name the scenario and load an emissions file before attempting to load the scenario.", type = "warning")
    return(NULL)
  }
  
  scenarioName <- input$input_custom_scenarioName
  
  tryCatch(
    {
      # Load scenario and custom emissions
      inifile <- system.file(input$input_custom_SSP, package='hector', mustWork=TRUE)
      emissions_file <- input$input_custom_emissions_file$datapath
      emissions_data <- read.csv(file=emissions_file, header=TRUE, sep=",", skip = 5)
      emissions_headers <- read.csv(file=emissions_file, header=FALSE, sep=",", skip = 4)
      dates_col <- emissions_data$Date
      
      withProgress(message = paste('Creating Custom Scenario ', scenarioName, "...\n"), value = 1/2,
                   {
                     core <<- newcore(inifile, suppresslogging=TRUE, name=scenarioName)
                     run(core, 2100)
                     incProgress(1/1, detail = paste("Load complete."))
                     Sys.sleep(0.2)
                   })
      
      # Set custom emissions here
      for(i in 2:ncol(emissions_data))
      {
        setvar(core = core, dates = emissions_data[, 1],var = colnames(emissions_data)[i], values = emissions_data[, i], unit = as.character(emissions_headers[[paste0("V",i)]][[1]]))
      }
      
      reset(core)
      run(core, 2100)
      #updateSelectInput(session, inputId = "mapCore", choices = names(hcores))
      #loadGraph()
    },
    warning = function(war)
    {
      showModal(modalDialog(
        title = "Warning",
        paste("Details:  ",war
        )
      ))
    },
    error = function(err)
    {
      shinyalert::shinyalert("Custom Scenario Error",print(paste('Error attempting to load custom scenario: ',err)), type = "error")
    })
}
