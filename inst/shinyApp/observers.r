# This file contains miscellaneous observers (all excluding those from the parameters which are in the parameters.r file)

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
        withProgress(message = paste('Loading Scenario RCP ', scenarioName, "...\n"), value = 1/2,
        {
          hcores[[scenarioName]] <<- loadScenario(scenario = scenarioName)
          incProgress(1/1, detail = paste("Load complete."))
          Sys.sleep(0.3)
        })
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
  x <- seq(1, 10, 2)
  y <- x * 3
  newx <- seq(1, 10, 0.1)
  s <- spline(x, y, xout = newx)
  emission <- input$input_custom_emissions
  startDate <- as.double(input$input_custom_start)
  endDate <- as.double(input$input_custom_end)
  hector_var <- globalCapabilities[[input$input_custom_emissions]][[1]]
  hector_unit <- attr(globalCapabilities[[input$input_custom_emissions]], "unit")

  if(input$input_slope_emissions)
  {
    for(i in 1:length(hcores))
    {
      startEmission <- hector::fetchvars(core = hcores[[i]], dates = startDate, vars = hector_var, "\n")
      x <- c(startDate, endDate)
      y <- c(startEmission["value"], as.double(input$input_emissions_value))
      z <- seq(as.integer(startDate), as.integer(endDate), 1)
      dates <- c(startDate:endDate)
      seq_out <- spline(x, y, xout = z)
      values <- unlist(seq_out["y"])
      values1 <- as.vector(values)
      #browser()
      hector::setvar(core = hcores[[i]], dates = dates, var = hector_var, values = values1, unit = hector_unit)
    }
  }
  else
  {
    for(i in 1:length(hcores))
    {
      hector::setvar(core = hcores[[i]], dates = startDate:endDate, var = hector_var, values = as.double(input$input_emissions_value), unit = hector_unit)
    }
  }
  resetCore()
  loadGraph()
}

# Observer function that resets any custom emissions inputted by the user
resetCustomEmissions <- function()
{
  restartCore()
}

openPage <- function(url) {
  return(tags$a(href=url, "Click here!", target="_blank"))

}
