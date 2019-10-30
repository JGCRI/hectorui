

# Observer function that responds to changes in inputs from the capabilities drop down field in the scenario output tab
setCapabilities <- function()
{
  print('in set capabilities')
  tryCatch(
    {
      outputVariables <<- list()
      capabilityValues <- vector()
      if(length(input$capabilities) > 0)
      {
        i <- 1
        capabilityValues <- input$capabilities
        while(i <= length(capabilityValues))
        {
          #browser()
          outputVariables[i] <<- globalCapabilities[capabilityValues[i]]
          #attr(outputVariables[[i]], 'longName') <<- attr(globalCapabilities[capabilityValues[i]], 'longName')
          i <- i+1
        }
      }
      else
      {
      }
    },
    error = function(err)
    {
      # error handler picks up where error was generated
      print(paste("\n ERROR: Set Params - ", as.character(err[1]), sep=" "))
      shinyalert::shinyalert("Error!",print(paste('Output Error: ',err)), type = "error")

    })
}

# Observer function that is activated on a change to the RCP Scenario checkboxes.
# This function will load the chosen scenario and rerun Hector
setRCP <- function(scenarioName)
{
  print("in set RCP")
  #browser()
  tryCatch(
    {
      if(scenarioName == "Custom")
      {
        if(customLoaded == TRUE)
        {
          print("CUSTOM")
        }
        else
        {
          shinyalert::shinyalert("Information:",print(paste('Please load a custom scenario first!')), type = "warning")
        }
      }
      # If scenario is checked then load it, otherwise unload it
      else if(input[[paste("input_RCP",scenarioName, sep = "")]])
      {

        hcores[[scenarioName]] <<- loadScenario(scenario = scenarioName)
        # If this is the initial application load, then we need to assign the on screen input field values to hector's default params
        if(firstLoad)
        {
          loadParameters()
        }
        # If its not first load but the parameters have changed via user input, then need to restore those values after restarting core
        else if(paramsChanged)
        {
          restoreParameters()
        }
      }
      else
      {#browser()
        hcores[[scenarioName]] <<- NULL
      }
      if(length(hcores) > 0)
        loadGraph()
    },
    error = function(err)
    {
      # error handler picks up where error was generated
      shinyalert::shinyalert("Error!",print(paste('Output Error: ',err)), type = "error")

    })
  firstLoad <<- FALSE
}

# Observer function responsible for processing the custom emissions file when the user creates a custom scenario
loadCustomScenario <- function()
{
  print("in load custom")
  #browser()
  if (is.null(input$input_ScenarioFile) | (is.na(input$input_ScenarioName) | is.null(input$input_ScenarioName) | (input$input_ScenarioName == "")))
  {
    shinyalert::shinyalert("Missing Information", "Please name the scenario and load an emissions file before attempting to load the scenario.", type = "warning")
    return(NULL)
  }
  tryCatch(
    {

      inifile <<-  Sys.glob(input$input_ScenarioFile$datapath)
      hcores[["Custom"]] <<- hector::newcore(inifile, suppresslogging=TRUE, name="custom")
      hector::run(hcore, globalVars['endDate'])

      # If this is the initial application load, then we need to assign the on screen input field values to hector's default params
      if(firstLoad)
      {
        loadParameters()
      }
      # If its not first load but the parameters have changed via user input, then need to restore those values after restarting core
      else if(paramsChanged)
      {
        restoreParameters()
      }
      customLoaded <<- TRUE
      loadGraph()
    },
    warning = function(war)
    {
      showModal(modalDialog(
        title = "Warning",
        paste("Details:  ",war)
      ))
    },
    error = function(err)
    {
      shinyalert::shinyalert("Custom Scenario Error",print(paste('Error attempting to load custom scenario: ',err)), type = "error")
    },
    finally =
      {
      })
}

# Observer function that handles the set custom emissions feature
setCustomEmissions <- function()
{
  print("in Set Custom Emissions")
  # Verify things:
  # 1. Needs to be Hector cores already instantiated first in order to set emissions
  # 2. Years need to be validated (validated to be a year and start > end if used that way)
  emission <- input$input_custom_emissions
  startDate <- as.double(input$input_custom_start)
  endDate <- as.double(input$input_custom_end)
  # browser()
  for(i in 1:length(hcores))
  {
    hector::setvar(core = hcores[[i]], dates = startDate:endDate, var = hector::FFI_EMISSIONS(), values = as.double(input$input_emissions_value), unit = "Pg")
    # setvar(hcore85, ffi85$year, FFI_EMISSIONS(), ffinew, as.character(ffi85$units[1]))
  }
  resetCore()
}

openPage <- function(url) {
  return(tags$a(href=url, "Click here!", target="_blank"))

}
