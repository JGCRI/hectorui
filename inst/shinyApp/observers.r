# This file contains miscellaneous observers (all EXCEPT for those from the parameters which are in the parameters.r file)


#' Keeps a list of the selected output variables for graphs
#'
#' Observer function that responds to changes in user input from the capabilities drop down field in the scenario output tab
#' @return
#' @export
#'
#' @examples
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
          outputVariables[i] <<- globalCapabilities[capabilityValues[i]]
          i <- i+1
        }
      }
    },
    error = function(err)
    {
      # error handler picks up where error was generated
      shinyalert::shinyalert("Error!",print(paste('Output Error: ',err)), type = "error")
    })
}


#' Load the chosen scenario and rerun Hector
#'
#' Observer function that is activated on a change to the RCP Scenario checkboxes. Will load the scenario into a new Hector core
#' @param scenarioName
#'
#' @return
#' @export
#'
#' @examples
setRCP <- function(scenarioName)
{ #browser()
  print("in set RCP")
  coreName <- paste0("Standard-", scenarioName)
  tryCatch(
  {
    # If scenario is checked then load it, otherwise unload it
    if(input[[paste("input_RCP",scenarioName, sep = "")]])
    {
      withProgress(message = paste('Loading Scenario RCP ', scenarioName, "...\n"), value = 1/2,
      {
        hcores[[coreName]] <<- loadScenario(scenario = scenarioName)
        incProgress(1/1, detail = paste("Load complete."))
        Sys.sleep(0.2)
      })
    }
    else
    {
     hcores[[coreName]] <<- NULL
    }
    if(length(hcores) > 0)
    {
      updateSelectInput(session, inputId = "mapCore", choices = names(hcores))
      loadGraph()
    }
    else
      cleanPlots()
  },
  error = function(err)
  {
    # error handler picks up where error was generated
    shinyalert::shinyalert("Error!",print(paste('Output Error: ',err)), type = "error")

  })
}

#' Load custom scenario
#'
#' Observer function responsible for processing the custom scenario file when the user creates a custom scenario
#' @return
#' @export
#'
#' @examples
loadCustomScenario <- function()
{
  print("in load custom")
  #browser()
  if (is.null(input$input_custom_scenario_ini) | is.null(input$input_custom_scenario_csv) | (is.na(input$input_custom_scenarioName) | is.null(input$input_custom_scenarioName) | (input$input_custom_scenarioName == "")))
  {
    shinyalert::shinyalert("Missing Information", "Please name the scenario and load an emissions/ini file before attempting to load the scenario.", type = "warning")
    return(NULL)
  }
  scenarioName <- input$input_custom_scenarioName
  tryCatch(
    {
      withProgress(message = paste('Creating Custom Scenario ', scenarioName, "...\n"), value = 1/2,
      {
        inifile <-  Sys.glob(input$input_custom_scenario_ini$datapath)
        csvfile <- Sys.glob(input$input_custom_scenario_csv$datapath)
        hcores[[scenarioName]] <<- hector::newcore(inifile, suppresslogging=TRUE, name="custom")
        hector::run( hcores[[scenarioName]], globalVars[['endDate']])
        incProgress(1/1, detail = paste("Load complete."))
        Sys.sleep(0.2)
      })

      # If this is the initial application load, then we need to assign the on screen input field values to hector's default params
      # if(firstLoad)
      # {
        loadParameters()
      # }
      # If its not first load but the parameters have changed via user input, then need to restore those values after restarting core
      # else if(paramsChanged)
      # {
      #   restoreParameters()
      # }
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

#' Load custom emissions
#'
#' Observer function responsible for processing the custom emissions file when the user creates a custom emissions scenario
#' @return
#' @export
#'
#' @examples
loadCustomEmissions <- function()
{
   print("in load custom")
  #browser()
  if (is.null(input$input_custom_emissions_file) | (is.na(input$input_custom_scenarioName) | is.null(input$input_custom_scenarioName) | (input$input_custom_scenarioName == "")))
  {
    shinyalert::shinyalert("Missing Information", "Please name the scenario and load an emissions file before attempting to load the scenario.", type = "warning")
    return(NULL)
  }
  scenarioName <- input$input_custom_scenarioName
  tryCatch(
  {
    #browser()
    inifile <- system.file(globalScenarios[input$input_custom_RCP], package='hector', mustWork=TRUE)
    emissions_file <- input$input_custom_emissions_file$datapath
    emissions_data <- read.csv(file=emissions_file, header=TRUE, sep=",", skip = 3)
    emissions_headers <- read.csv(file=emissions_file, header=FALSE, sep=",", skip = 2)
    dates_col <- emissions_data$Date

    withProgress(message = paste('Creating Custom Scenario ', scenarioName, "...\n"), value = 1/2,
     {
        hcores[[scenarioName]] <<- hector::newcore(inifile, suppresslogging=TRUE, name=scenarioName)
        hector::run( hcores[[scenarioName]], globalVars[['endDate']])
        incProgress(1/1, detail = paste("Load complete."))
        Sys.sleep(0.2)
     })

    for(i in 2:ncol(emissions_data))
    {
      hector::setvar(core = hcores[[scenarioName]], dates = emissions_data[, 1],var = colnames(emissions_data)[i], values = emissions_data[, i], unit = as.character(emissions_headers[[paste0("V",i)]][[1]]))
    }

    hector::reset(hcores[[scenarioName]])
    hector::run(hcores[[scenarioName]], globalVars[['endDate']])
    updateSelectInput(session, inputId = "mapCore", choices = names(hcores))
    loadGraph()
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
    },
    finally =
      {
      })
}

#' Sets custom emissions set from the user input
#'
#' Observer function that handles the set custom emissions feature. Sets emissions for all active cores.
#' @return
#' @export
#'
#' @examples
setCustomEmissions <- function()
{
  print("in Set Custom Emissions")
  tryCatch(
  {
    # Verify things:
    # 1. Needs to be Hector cores already instantiated first in order to set emissions
    # 2. Years need to be validated (validated to be a year and start > end if used that way)
    #browser()
    validate(
      need(as.double(input$input_custom_start) >= globalVars[['startDate']] && as.double(input$input_custom_start) <= globalVars[['endDate']], "Please use a valid 4 digit year for start year")
    )
    validate(
      need(as.double(input$input_custom_end) >= globalVars[['startDate']] && as.double(input$input_custom_end) <= globalVars[['endDate']], "Please use a valid 4 digit year for end year")
    )
    validate(
      need(length(input$input_custom_emissions) >= 0, "Please enter a value for emissions")
    )
    # if(!input$input_custom_start | !input$input_custom_end | !input$input_custom_emissions)
    # {
    #   shinyalert::shinyalert("Missing Information", "Please fill in all values before setting emissions", type = "warning")
    #   return(NULL)
    # }
    if(length(hcores) > 0)
    {
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
          scenarioName <- names(hcores)[i]
          if(substr(scenarioName, 1, 8) =="Standard")
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
    else
    {
      shinyalert::shinyalert("No active Hector cores", "Please set at least one of the RCP scenarios to active or upload a custom emissions scenario before downloading.", type = "warning")
    }
  },
  error = function(err)
  {
    # error handler picks up where error was generated
    shinyalert::shinyalert("Error!",print(paste('Error Setting Emissions: ',err)), type = "error")
  })
}

#' Reset custom emissions
#'
#' Observer function that resets any custom emissions inputted by the user back to hector defaults. This also forces a restart which will reset parameters
#' @return
#' @export
#'
#' @examples
resetCustomEmissions <- function()
{
  print("in reset emissions")
  updateSelectInput(session = session, inputId = "input_paramToggle", selected = "default")
  restartCore()
}

#' Open URL link
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
openPage <- function(url) {
  return(tags$a(href=url, "Click here!", target="_blank"))

}

changeTheme <- function()
{
  ggthemr::ggthemr(stringr::str_to_lower(input$set_theme), type = "outer")
  loadGraph()
}

sendFeedback <- function()
{

}

updateIndex <- function()
{
  selectedIndex <<- input$mapCore
}

