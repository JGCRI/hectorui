
library(HectorShiny)

#' Main server/data processing function
#'
#' The server function is the main function that processes inputs and handles data i/o. This is required for Shiny apps.
#'
#' @param input
#' @param output
#' @param session
#'
#' @return no return value
#' @export
#'
#' @examples
server <- function(input, output, session)
{
  # Needed to interact Shiny with client side JS
  shinyjs::useShinyjs()
  # shinyjs::disable("set_Params")
  source("parameters.r", local = TRUE)
  source("core.r", local = TRUE)
  source("output.r", local = TRUE)
  source("observers.r", local = TRUE)

#----- Set up non global variables in top level application scope

  # Keeps track of if this is the actual first load/run of this instance
  firstLoad <- TRUE
  outputVariables <- list()
  inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)
  hcores <- list()
  totalActivePlots <- 0
  customLoaded <- FALSE
  scale_colors <- vector()
  ggthemr('dust', type = "outer")

  # These variables are for storing the current parameter values so that if a change is made (like loading new scenario)
  # then the custom params set by user will persist beyond core restarts
  paramsList <- list()
  paramsList <- globalParamsDefault

  assignParameters()
  paramsChanged <- FALSE

#----- End set up local vars

#----- Setup plots

  output$plots <- renderUI(
  {
    plot_output_list <- lapply(1:4, function(i)
    {
      plotname <- paste("plot", i, sep="")
      plottitle <- paste("plottitle",attr(globalScenarios[[i]], "name"), sep="")
      tablename <- paste("tablename", attr(globalScenarios[[i]], "name"), sep="")
      plot <-  plotly::plotlyOutput(plotname, height = 270, width = 550) #%>%
               # layout(dragmode = "select") %>%
               # event_register("plotly_selecting")

      tags$div(class = "group-output",
               # textOutput(plottitle, container = h3),
               plotly::plotlyOutput(plotname, height = 270, width = 550) ,
               tableOutput(tablename)
      )
    })

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, plot_output_list)
  })

  #set initial plotting variables
  for(i in 1:4)
  {
    plotname <- paste("plot", i, sep="")
    shinyjs::hide(plotname)
  }
#----- End Setup plots

#----- Set up observer functions to catch user interaction on the input fields

  observeEvent(input$capabilities, setCapabilities(), ignoreInit = FALSE)
  observeEvent(input$loadGraphs, loadGraphProxy(), ignoreInit = TRUE)
  observeEvent(input$set_Params, setParameters(), ignoreInit = TRUE)
  observeEvent(input$input_ScenarioFile, loadScenario(), ignoreInit = TRUE)
  observeEvent(input$reset_Params, resetParams(), ignoreInit = TRUE)
  observeEvent(input$input_RCP2.6, setRCP("2.6"), ignoreInit = TRUE)
  observeEvent(input$input_RCP4.5, setRCP("4.5"), ignoreInit = FALSE)
  observeEvent(input$input_RCP6.0, setRCP("6.0"), ignoreInit = TRUE)
  observeEvent(input$input_RCP8.5, setRCP("8.5"), ignoreInit = TRUE)
  observeEvent(input$input_enableCustom, setRCP("Custom"), ignoreInit = TRUE)
  observeEvent(input$input_load_custom, loadCustomScenario(), ignoreInit = TRUE)
  observeEvent(input$input_load_emissions, loadCustomEmissions(), ignoreInit = TRUE)
  observeEvent(input$input_paramToggle, loadModelParameters(), ignoreInit = TRUE)
  observeEvent(input$input_enableCustom, toggleCustom(), suspended = TRUE)
  observeEvent(input$input_set_custom_emissions, setCustomEmissions(), ignoreInit = TRUE)
  observeEvent(input$input_reset_custom_emissions, resetCustomEmissions(), ignoreInit = TRUE)
  observeEvent(input$test, changeTheme(), ignoreInit = TRUE)

  # This is a group Observer block for all of the params fields because they all respond the same way
  observe({
    input$input_pco2
    input$input_q10
    input$input_volc
    input$input_aero
    input$input_beta
    input$input_diff
    input$input_ecs
    #setParamsChanged(TRUE)
  })

#----- End observer function setup

#----- Custom Functions

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
      #browser()
      updateNumericInput(session = session, inputId = "input_beta", value = 1)
    }
  }
}

#----- End Custom Functions
