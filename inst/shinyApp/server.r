
library(HectorShiny)

#' Main server/data processing function
#'
#' The server function is the main function that processes inputs and handles data i/o. This is required for Shiny apps.
#'
#' @param input - Creates the Shiny input object
#' @param output - Creates the Shiny output object
#' @param session - Creates the Shiny session object
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
  # firstLoad <- TRUE
  outputVariables <- list()
  inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)
  hcores <- list()
  totalActivePlots <- 0
  customLoaded <- FALSE
  scale_colors <- vector()
  ggthemr::ggthemr('dust', type = "outer")
  selectedIndex <<- 1

  # These variables are for storing the current parameter values so that if a change is made (like loading new scenario)
  # then the custom params set by user will persist beyond core restarts
  paramsList <- list()
  paramsList <- globalParamsDefault
  assignParameters()

  coresReactive <- reactive({
    return(names(hcores))
  })

  output$coreMapping <- renderUI({
    selectInput(inputId = "mapCore", width = 180, label = ("Available Scenarios:"), choices =  coresReactive(), selected = selectedIndex)
  })
#----- End set up local vars

#----- Set up plots and maps

  # This UI output variable is responsible for generating the 4 graphs in the output section.
  output$plots <- renderUI(
  {
    plot_output_list <- lapply(1:4, function(i)
    {
      plotname <- paste("plot", i, sep="")
      plot <-  plotly::plotlyOutput(plotname, height = 270, width = 550) #%>%
               # layout(dragmode = "select") %>%
               # event_register("plotly_selecting")

      tags$div(class = "group-output",
               # textOutput(plottitle, container = h3),
               plotly::plotlyOutput(plotname, height = 270, width = 550)
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
      map <-  plotly::plotlyOutput(mapname, height = 550, width = 1100)

      shinyjs::hidden(tags$div(class = "group-output", id = "map-div",
               # textOutput(title, container = h3),
               shinycustomloader::withLoader(plotly::plotlyOutput(mapname, height = 550, width = 1100), type="text", loader = list(shinycustomloader::marquee("Please Wait... Finalizing Raster Output", style="font-size:30px; color:white; text-align:center", scrollamount = 0))))
      )
    })

    # Convert the list to a tagList - this is necessary for the list of items to display properly.
    do.call(tagList, map_output_list)
    #shinyjs::hidden(id = 'maps')
  })

  #set initial plotting variables
  for(i in 1:4)
  {
    plotname <- paste("plot", i, sep="")
    shinyjs::hide(plotname)
  }

  #set initial mapping variables
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
  observeEvent(input$set_theme, changeTheme(), ignoreInit = TRUE)
  observeEvent(input$loadMaps, loadMap(), ignoreInit = TRUE)
  observeEvent(input$input_submit_feedback, sendFeedback(), ignoreInit = TRUE)
  observeEvent(input$mapCore, updateIndex(), ignoreInit = TRUE)

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
