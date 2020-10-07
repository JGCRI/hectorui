# This file is the application controller

library(hectorui)
library(hector)


#' Global constants for hectorui
#' @import hector
#' @name constants
NULL


# Global vars for misc items such as the run date end year (2100)
#' @details \code{globalVars}: Miscellaneous global variables
#' @rdname constants
#' @export
globalVars <- get_globalVars()


# Global vars for scale colors
#' @details \code{globalColorScales}: Scale colors
#' @rdname constants
#' @export
globalColorScales <- get_globalColorScales()


# Global file paths vector
#' @details \code{globalScenarios} Scenario input file names and paths
#' @rdname constants
#' @export
rcps <- get_rcps()


# Global file paths vector
#' @details \code{globalScenarios} Scenario input file names and paths
#' @rdname constants
#' @export
globalScenarios <- get_globalScenarios()


# Global scenario color schemes
#' @details \code{globalScenarios} Scenario color schemes
#' @rdname constants
#' @export
globalScenarioColors <- get_globalScenarioColors()


# Global temperature patterns
#' @details \code{globalTempPatterns} Create global temperature patterns list
#' @rdname constants
#' @export
globalTempPatterns <- get_globalTempPatterns()


# Global precipitation patterns list
#' @details \code{globalPrecipPatterns} Create global precipitation patterns list
#' @rdname constants
#' @export
globalPrecipPatterns <- get_globalPrecipPatterns()


# Create master list of parameter lookup strings
#' @details \code{globalParameters}: Capability strings (internal name lookup/mapping) for Hector parameters - (should match UI component ids)
#' @rdname constants
#' @export
globalParameters <- get_globalParameters()


# Default Hector parameters
#' @details \code{globalParamsDefault}: Default Parameter Set
#' @rdname constants
#' @export
globalParamsDefault <- get_globalParamsDefault()


# CanESM2 Parameter Sets
#' @details \code{globalParamsCanESM2}: CanESM2 Parameters for model emulation
#' @rdname constants
#' @export
globalParamsCanESM2 <- get_globalParamsCanESM2()


# CESM1-BGC Parameter Set
#' @details \code{globalParamsCESM1BGC}: CESM1-BGC Parameters for model emulation
#' @rdname constants
#' @export
globalParamsCESM1BGC <- get_globalParamsCESM1BGC()


# GFDL-ESM2G Parameter Set
#' @details \code{globalParamsGFDLESM2G}: GFDL-ESM2G Parameters for model emulation
#' @rdname constants
#' @export
globalParamsGFDLESM2G <- get_globalParamsGFDLESM2G()


# MIROC-ESM Parameter Set
#' @details \code{globalParamsMIROCESM}: MIROC-ESM Parameters for model emulation
#' @rdname constants
#' @export
globalParamsMIROCESM <- get_globalParamsMIROCESM()


# MPI-ESM-LR Parameter Set
#' @details \code{globalParamsMPIESM-LR}: MPI-ESM-LR Parameters for model emulation
#' @rdname constants
#' @export
globalParamsMPIESMLR <- get_globalParamsMPIESMLR()


# MRI-ESM1 Parameter Set
#' @details \code{globalParamsMRIESM1}: MRI-ESM1 Parameters for model emulation
#' @rdname constants
#' @export
globalParamsMRIESM1 <- get_globalParamsMRIESM1()


# Create master list of variable lookups for "capabilities" (output variables for graphing)
#' @details \code{globalCapabilities} Capability string (internal name lookup/mapping) for Hector output variables, organized by group
#' @rdname constants
#' @export
globalCapabilities <- get_globalCapabilities()




#' Main server/data processing function
#'
#' The server function is the main function that processes inputs and handles data i/o.
#' This is required for Shiny apps using the separate UI/Server file architecture.
#'
#' @param input - Creates the Shiny input object
#' @param output - Creates the Shiny output object
#' @param session - Creates the Shiny session object
#' @export
#'
#' @examples
server <- function(input, output, session)
{
  # Needed to interact Shiny with client side JS
  shinyjs::useShinyjs()

  # Load other source files
  source("parameters.r", local = TRUE)
  source("core.r", local = TRUE)
  source("output.r", local = TRUE)
  source("observers.r", local = TRUE)

#----- Set up non global variables in top level application scope

  outputVariables <- list()
  inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)
  hcores <- list()
  totalActivePlots <- 0
  customLoaded <- FALSE
  ggthemr::ggthemr('dust', type = "outer")
  selectedIndex <<- 1
  ggplotSave <<- ggplot2::ggplot()

  # These variables are for storing the current parameter values so that if a change is made (like loading new scenario)
  # then the custom params set by user will persist beyond core restarts
  paramsList <- list()
  paramsList <- globalParamsDefault
  assignParameters()

  # These two lines of code allows the main Hector cores object to be reactive and able to be linked with the maps scenario dropdown
  coresReactive <- reactive({
    return(names(hcores))
  })
  output$coreMapping <- renderUI({
    selectInput(inputId = "mapCore", width = 180, label = ("Available Scenarios:"), choices =  coresReactive(), selected = selectedIndex)
  })

#----- End set up local vars

#----- Set up graphs and maps

  # This UI output variable is responsible for generating the 4 graphs in the output section.
  output$plots <- renderUI(
  {
    plot_output_list <- lapply(1:4, function(i)
    {
      plotname <- paste("plot", i, sep="")
      plot <-  plotly::plotlyOutput(plotname, height = 255, width = 510)

      tags$div(class = "group-output",
               plotly::plotlyOutput(plotname, height = 255, width = 510)
      )
    })

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
  })

  # This UI output variable is responsible for rendering the downscaled maps. It is initiated as 'hidden' using shinyjs until the user first loads a map.
  output$maps <- renderUI(
  {
    map_output_list <- lapply(1:1, function(i)
    {
      mapname <- paste("map", i, sep="")
      map <-  plotly::plotlyOutput(mapname, height = 550, width = 1050)

      shinyjs::hidden(tags$div(class = "group-output", id = "map-div",
               shinycustomloader::withLoader(plotly::plotlyOutput(mapname, height = 550, width = 1050), type="text",
                                             loader = list(shinycustomloader::marquee("Please Wait... Finalizing Raster Output", style="font-size:30px; color:white; text-align:center", scrollamount = 0))))
      )
    })

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, map_output_list)
  })

  # Set initial plotting variables
  for(i in 1:4)
  {
    plotname <- paste("plot", i, sep="")
    shinyjs::hide(plotname)
  }

  # Set initial mapping variables and hide
  for(i in 1:1)
  {
    mapname <- paste("map", i, sep="")
    shinyjs::hide(mapname)
  }
#----- End set up plots and maps

#----- Set up observer functions to catch user interaction on the input fields

  observeEvent(input$capabilities, setCapabilities(), ignoreInit = FALSE)
  observeEvent(input$loadGraphs, loadGraph(), ignoreInit = TRUE)
  observeEvent(input$set_Params, setParameters(), ignoreInit = TRUE)
  observeEvent(input$input_ScenarioFile, loadScenario(), ignoreInit = TRUE)
  observeEvent(input$reset_Params, resetParams(), ignoreInit = TRUE)
  observeEvent(input$input_RCP_2.6, setRCP("RCP-2.6"), ignoreInit = TRUE)
  observeEvent(input$input_RCP_4.5, setRCP("RCP-4.5"), ignoreInit = FALSE)
  observeEvent(input$input_RCP_6.0, setRCP("RCP-6.0"), ignoreInit = TRUE)
  observeEvent(input$input_RCP_8.5, setRCP("RCP-8.5"), ignoreInit = TRUE)
  observeEvent(input$input_enableCustom, setRCP("Custom"), ignoreInit = TRUE)
  observeEvent(input$input_load_custom, loadCustomScenario(), ignoreInit = TRUE)
  observeEvent(input$input_load_emissions, loadCustomEmissions(), ignoreInit = TRUE)
  observeEvent(input$input_paramToggle, loadModelParameters(), ignoreInit = TRUE)
  observeEvent(input$input_enableCustom, toggleCustom(), suspended = TRUE)
  observeEvent(input$input_set_custom_emissions, setCustomEmissions(), ignoreInit = TRUE)
  observeEvent(input$input_reset_custom_emissions, resetCustomEmissions(), ignoreInit = TRUE)
  observeEvent(input$set_theme, changeTheme(), ignoreInit = TRUE)
  observeEvent(input$loadMaps, loadMap(), ignoreInit = TRUE)
  observeEvent(input$input_submit_feedback, sendFeedback(), ignoreInit = TRUE)
  observeEvent(input$mapCore, updateIndex(), ignoreInit = TRUE)
  observeEvent(input$saveMap, saveMap(), ignoreInit = TRUE)

  # This is a group Observer block for all of the params fields because they all respond the same way
  observe({
    input$input_pco2
    input$input_q10
    input$input_volc
    input$input_aero
    input$input_beta
    input$input_diff
    input$input_ecs
  })

#----- End observer function setup

#----- Custom Functions

  # Renders feedback form
  output$feedbackFrame <- renderUI({
    frame_link <- tags$iframe(src="https://docs.google.com/forms/d/e/1FAIpQLSf6inU3DHAE5tZo4vdgtTjtFZvw7OCuH_5xbLvnj5tdqiRVNA/viewform?embedded=true",
                              height=1100, width=700, seamless=NA)
    frame_link
  })

  toggleCustom <- function()
  {
    shinyjs::disable("input_enableCustom")
  }

  testMe <- function()
  {
    if(!is.na(input$input_beta) && (as.double(input$input_beta) < 0.01 ))
    {
      updateNumericInput(session = session, inputId = "input_beta", value = 1)
    }
  }
}

#----- End Custom Functions
