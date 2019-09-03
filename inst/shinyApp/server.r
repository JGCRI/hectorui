

library(HectorShiny)


# Main data processing function
# The server function is the main function that processes inputs and handles data i/o. This is required for Shiny apps.
# Define server logic to set up data fields and drive UI interactions.
server <- function(input, output, session)
{
  # Needed to interact Shiny with client side JS
  shinyjs::useShinyjs()

  #----- Set up non global variables in top level application scope
    # Keeps track of if this is the actual first load/run of this instance
    firstLoad <- TRUE
    outputVariables <- vector()
    inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)

    # These variables are for storing paremeter values so that if a change is made (like loading new scenario) the custom params set by user will persist
    paramsList <- list()
    paramsChanged <- FALSE
  #----- End set up local vars

  #----- Set up observer functions to catch user interaction on the input fields
    observeEvent(input$capabilities, setCapabilities())
    observeEvent(input$loadGraphs, loadGraph())
    observeEvent(input$set_Params, setParameters())
    observeEvent(input$input_ScenarioFile, loadScenario())
    observeEvent(input$reset_Params, resetParams())
    observeEvent(input$input_RCP, setRCP())
    observeEvent(input$input_Driven, loadCustomScenario())
  #----- End observer function setup

  # Main function that loads/starts the Hector Core and runs the default scenario
  startHector <- function()
  {
    print("in start hector")
    tryCatch(
    {
      cat(paste('Looking for inputfile, scenario= ', input$input_RCP, '  file=', HectorShiny::globalScenarios[input$input_RCP]),'\n')
      inifile <<- system.file(globalScenarios[input$input_RCP], package='hector', mustWork=TRUE)
      hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name=input$input_ScenarioName)
      updateTextInput(session=session, "input_ScenarioName", value=paste(input$input_RCP))
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
      shinyalert::shinyalert("Initalization Error",print(paste('Error starting Hector: ',err)), type = "error")
    },
    finally =
      {
      })
  }

  # Function to reset (not restart via shutdown) a Hector core. A core reset should only be called when input parameters have changed.
  resetCore <- function()
  {
    print("in reset core")
    hector::reset(hcore)
    hector::run(hcore, globalVars['endDate'])
    loadGraph()
  }

  # Function to shutdown and create a new Hector core. A core restart should only be called when the scenario (RCP or custom) has changed/been loaded
  restartCore <- function()
  {
    print("in restart core")
    hcore <<- hector::shutdown(hcore)
    startHector()
    loadGraph()
  }

  # Function that assigns the user changed parameter values to the Hector core (for persistence)
  restoreParameters <- function()
  {
    print('in restore params')
    setParameters()
  }

  # Observer function to handle the user input on the reset parameters button - reset hector parameters to defaults
  resetParams <- function()
  {
    print("in reset params")
    paramsChanged <<- FALSE
    restartCore()
    loadParameters()
  }

  # Function that gets the input parameters from the hector core and maps them to the input fields
  # This would normally be called on first load or when parameters are reset.
  loadParameters <- function()
  {
    print("in load params")

    # Fetch hector parameters from core
    hdata <- hector::fetchvars(core = hcore, dates = NA, vars = globalParameters, "\n")

    # Update the on screen input components for parameters with the associated values stored in hector core
    updateNumericInput(session, "input_aero", value=round(hdata[which(hdata$variable == "alpha"), 4], 2))
    updateNumericInput(session, "input_beta", value=round(hdata[which(hdata$variable == "beta"), 4], 2))
    updateNumericInput(session, "input_diff", value=round(hdata[which(hdata$variable == "diff"), 4], 2))
    updateNumericInput(session, "input_ecs",  value=round(hdata[which(hdata$variable == "S"), 4], 2))
    updateNumericInput(session, "input_pco2", value=round(hdata[which(hdata$variable == "C0"), 4], 2))
    updateNumericInput(session, "input_q10",  value=round(hdata[which(hdata$variable == "q10_rh"), 4], 2))
    updateNumericInput(session, "input_volc", value=round(hdata[which(hdata$variable == "volscl"), 4], 2))

    # Store params in the top level variable paramsList for persistence
    paramsList['alpha']   <<- hdata[which(hdata$variable == "alpha"), 4]
    paramsList['beta']    <<- hdata[which(hdata$variable == "beta"), 4]
    paramsList['diff']    <<- hdata[which(hdata$variable == "diff"), 4]
    paramsList['S']       <<- hdata[which(hdata$variable == "S"), 4]
    paramsList['C']       <<- hdata[which(hdata$variable == "C0"), 4]
    paramsList['q10_rh']  <<- hdata[which(hdata$variable == "q10_rh"), 4]
    paramsList['volscl']  <<- hdata[which(hdata$variable == "volscl"), 4]
  }

  # Observer function that responds to changes in inputs from the capabilities drop down field in the scenario output tab
  setCapabilities <- function()
  {
    print('in set capabilities')
    outputVariables <<- list()
    capabilityValues <- vector()
    if(length(input$capabilities) > 0)
    {
      i <- 1
      capabilityValues <- input$capabilities
      while(i <= length(capabilityValues))
      {# browser()
        outputVariables[i] <<- globalCapabilities[capabilityValues[i]]
        attr(outputVariables[[i]], "name") <<- attr(globalCapabilities[capabilityValues[i]], "name")
        i <- i+1
      }
      print(outputVariables)
    }
    else
    {
    }
  }

  # Observer function that is activated on a change to the RCP Scenario drop down field.
  # This function will load the chosen scenario and rerun Hector
  setRCP <- function()
  {
    print("in set RCP")
    tryCatch(
    {
      # If this is called after initial load of Hector then need to restart core
      if(!firstLoad)
      {
        hcore <<- hector::shutdown(hcore)
      }

      startHector()
      loadGraph()
    }
    )
    firstLoad <<- FALSE
  }

  # Observer function to handle the user input on the load custom scenario button
  loadScenario <- function()
  {
    print("in load scenario")
  }

  # Observer function to handle user click on the set parameters button.
  setParameters <- function()
  {
    print("in set parameters")
    newVals <- vector()
    # Run through variables and make sure none are left empty and update the top level scope paramsList variable with any changed values
    tryCatch(
    {
      if(!is.na(input$input_aero))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['aero'], values = c(as.double(input$input_aero)), unit = "unitless")
        paramsList['alpha'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      if(!is.na(input$input_beta))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['beta'], values = c(as.double(input$input_beta)), unit = "unitless")
        paramsList['beta'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      if(!is.na(input$input_diff))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['diff'], values = c(as.double(input$input_diff)), unit = "cm2/s")
        paramsList['diff'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      if(!is.na(input$input_ecs))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['ecs'],  values = c(as.double(input$input_ecs)), unit = "degC")
        paramsList['S'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      if(!is.na(input$input_pco2))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['pco2'], values = c(as.double(input$input_pco2)), unit = "ppmv CO2")#c(150), unit="ppmv CO2")
        paramsList['C'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      if(!is.na(input$input_q10))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['q10'],  values = c(as.double(input$input_q10)), unit = "unitless")
        paramsList['q10_rh'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      if(!is.na(input$input_volc))
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['volc'], values = c(as.double(input$input_volc)), unit = "unitless")
        paramsList['volscl'] <<- as.double(input$input_aero)
        paramsChanged <<- TRUE
      }
      resetCore()
    },
    warning = function(war)
    {
      # warning handler picks up where error was generated
      showModal(modalDialog(
        title = "Important message",
        paste("Details:  ",war)
      ))
    },
    error = function(err)
    {
      # error handler picks up where error was generated
      print(paste("\n ERROR: Set Params - ", as.character(err[1]), sep=" "))
      shinyalert::shinyalert("Oops!",print(paste('Error:',err)), type = "error")

    },
    finally =
      {
        cat(file=stderr(), "\nSetVars ","****Made it thru****", " trypas", "\n")
      })
  }

  # Observer function designed to handle the loading/creation of the output graphs from the Hector model.
  loadGraph <- function()
  {
    print("in load graph")
    tryCatch(
    {
     if(length(outputVariables) >= 1)
     {
        hdata <- hector::fetchvars(core = hcore, dates = 1800:globalVars['endDate'], vars = outputVariables, "\n")
        x <- dplyr::distinct(hdata, units)
        print(outputVariables)
        ggplotGraph <- ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value, group=variable, color=variable)) + ggplot2::geom_line() + ggplot2::facet_wrap(~variable, scales='free_y') +
                              ggthemes::theme_solarized(light = TRUE)+ ggplot2::ylab(x)
        #rowser()

        #output$plot <<- NULL
        xAxis <- list(
          showspikes = TRUE,
          title = "testing"
        )
        yAxis <- list(
          showspikes = TRUE,
          title = "testing"
        )
        a <- list(
          title = "AXIS TITLE",

          showticklabels = TRUE,
          tickangle = 45,

          exponentformat = "E"
        )
        localPlot <- plotly::ggplotly(p = ggplotGraph)
        plotly::layout(p=localPlot, title="testing 123", hovermode="closest", xaxis = a, yaxis = a )
       # browser()
        output$plot2 <<-  plotly::renderPlotly(localPlot)
     }
     else
     {
      outputVariables <<- NULL
     }

    },
    warning = function(war)
    {
      # warning handler picks up where error was generated
      showModal(modalDialog(
        title = "Important message",
        paste("MY_WARNING:  ",war)
      ))

    },
    error = function(err)
    {
      # error handler picks up where error was generated
      cat(file=stderr(), "\nError ","Graphing error: ", " " , "\n")
      shinyalert::shinyalert("Error Detected:",print(paste('There was an error when attempting to update the graph:',err)), type = "error")
      #bsModal("modalExample", "Your plot", "go", size = "large","asfd",downloadButton('downloadPlot', 'Download'))

    },
    finally =
    {
      #cat(file=stderr(), "\nFinally ", "\n")
    })

  }

  loadCustomScenario <- function()
  {
    print("in load custom")

    tryCatch(
    {
      # browser()
      inifile <<-  Sys.glob(input$input_ScenarioFile$datapath)

      hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name="testing123")
      updateTextInput(session=session, "input_ScenarioName", value=paste(input$input_RCP))
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

  openPage <- function(url) {
    return(tags$a(href=url, "Click here!", target="_blank"))

  }

  # Initialize reactive values
  values <- reactiveValues()
  #values$themes <- themes

  observe({

  })

  # Download handler for downloading the raw data output from a Hector run. This is activated upon button click.
  output$dlData <- downloadHandler(
    filename = function()
    {print("in dl data")
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file)
    {
      hdata <- hector::fetchvars(core = hcore, dates = 1800:globalVars['endDate'], vars = outputVariables, "\n")
      hdata <- dplyr::mutate(hdata)
      write.csv(hdata, file, row.names = FALSE)
    }
  )


}
