
library(HectorShiny)

# Main data processing function
# The server function is the main function that processes inputs and handles data i/o. This is required for Shiny apps.
# Define server logic to set up data fields and drive UI interactions.
server <- function(input, output, session)
{
  # Needed to interact Shiny with client side JS
  shinyjs::useShinyjs()
  # shinyjs::disable("set_Params")

#----- Set up non global variables in top level application scope
  # Keeps track of if this is the actual first load/run of this instance
  firstLoad <- TRUE
  outputVariables <- vector()
  inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)
  hcores <- list()
  totalActivePlots <- 0

  # The se variables are for storing paremeter values so that if a change is made (like loading new scenario) the custom params set by user will persist
  paramsList <- list()
  paramsChanged <- FALSE
#----- End set up local vars


  output$plots <- renderUI({
    plot_output_list <- lapply(1:4, function(i)
    {
     # plotname <- paste("plot", names(globalScenarios[i]), sep="")
      plotname <- paste("plot", i, sep="")
      plottitle <- paste("plottitle",attr(globalScenarios[[i]], "name"), sep="")
      tablename <- paste("tablename", attr(globalScenarios[[i]], "name"), sep="")
      tags$div(class = "group-output",
               textOutput(plottitle, container = h3),
               plotly::plotlyOutput(plotname, height = 250, width = 500),
               tableOutput(tablename)
      )
    })

    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })


#----- Set up observer functions to catch user interaction on the input fields
  observeEvent(input$capabilities, setCapabilities(), ignoreInit = TRUE)
  observeEvent(input$loadGraphs, loadGraph(), ignoreInit = TRUE)
  observeEvent(input$set_Params, setParameters(), ignoreInit = TRUE)
  observeEvent(input$input_ScenarioFile, loadScenario(), ignoreInit = TRUE)
  observeEvent(input$reset_Params, resetParams(), ignoreInit = TRUE)
  #observeEvent(input$input_RCP, setRCP())
  observeEvent(input$input_RCP2.6, setRCP("2.6"), ignoreInit = TRUE)
  observeEvent(input$input_RCP4.5, setRCP("4.5"), ignoreInit = TRUE)
  observeEvent(input$input_RCP6.0, setRCP("6.0"), ignoreInit = TRUE)
  observeEvent(input$input_RCP8.5, setRCP("8.5"), ignoreInit = TRUE)
  observeEvent(input$input_Driven, loadCustomScenario(), ignoreInit = TRUE)
  observeEvent(input$input_paramToggle, loadModelParameters())
#----- End observer function setup

  #set initial variables


  for(i in 1:4)
  {
    plotname <- paste("plot", i, sep="")
    shinyjs::hide(plotname)
  }
#---------- CORE RELATED FUNCTIONS

  # Main function that loads/starts the Hector Core and runs the default scenario
  loadScenario <- function(scenario)
  {
    print("in load scenario")
    tryCatch(
    { #browser()
      inifile <<- system.file(globalScenarios[paste("RCP", scenario)], package='hector', mustWork=TRUE)
      hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP", scenario)]))
      updateTextInput(session=session, "input_ScenarioName", value=paste(globalScenarios[paste("RCP", scenario)]))
      hector::run(hcore, globalVars['endDate'])

    },
    error = function(err)
    {
      shinyalert::shinyalert("Initalization Error",print(paste('Error starting Hector: ',err)), type = "error")
    })
    hcore
  }

  # Function to reset (not restart via shutdown) a Hector core. A core reset should only be called when input parameters have changed.
  resetCore <- function()
  {
    print("in reset core")
    for(i in 1:length(hcores))
    {
      hector::reset(hcores[[i]])
      hector::run(hcores[[i]], globalVars['endDate'])
    }

    loadGraph()
  }

  # Function to shutdown and create a new Hector core. A core restart should only be called when the scenario (RCP or custom) has changed/been loaded
  restartCore <- function()
  {
    print("in restart core")
    for(i in 1:length(hcores))
    {
      hcores[[i]] <<- hector::shutdown(core = hcores[[i]])
      inifile <<- system.file(globalScenarios[paste("RCP", names(hcores[i]))], package='hector', mustWork=TRUE)
      hcores[[i]] <<- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP",  names(hcores[i]))]))
      hector::run(hcores[[i]], globalVars['endDate'])
    }
    # hcore <<- hector::shutdown(hcore)
   # startHector()
    loadGraph()
  }
#---------- END CORE FUNCTIONS

  # Function that maintains persistence after the user has changed parameter values to the Hector core (after scenario change)
  restoreParameters <- function()
  {
    print('in restore params')
    setParameters()
  }

  # Oberserver function that responds to changes in input from model choice drop down in the model parameters section
  loadModelParameters <- function()
  {
    print("in load model params")
    paramsGroup <- vector()
    if(input$input_paramToggle == "default")
    {
      paramsGroup <- globalParamsDefault
    }
    else if(input$input_paramToggle == "magicc")
    {
      paramsGroup <- globalParamsMAGICC
    }
    else if(input$input_paramToggle == "other")
    {
      paramsGroup <- globalParamsOther
    }
    # Update the on screen input components for parameters with the associated values from the chosen parameter group
    updateNumericInput(session, "input_aero", value=paramsGroup[[1]])
    updateNumericInput(session, "input_beta", value=paramsGroup[[2]])
    updateNumericInput(session, "input_diff", value=paramsGroup[[3]])
    updateNumericInput(session, "input_ecs",  value=paramsGroup[[4]])
    updateNumericInput(session, "input_pco2", value=paramsGroup[[5]])
    updateNumericInput(session, "input_q10",  value=paramsGroup[[6]])
    updateNumericInput(session, "input_volc", value=paramsGroup[[7]])
    if(length(hcores) > 0)
    {
      for(i in 1:length(hcores))
      {
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['aero'], values = c(as.double(paramsGroup[[1]])), unit = "unitless")
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['beta'], values = c(as.double(paramsGroup[[2]])), unit = "unitless")
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['diff'], values = c(as.double(paramsGroup[[3]])), unit = "cm2/s")
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['ecs'],  values = c(as.double(paramsGroup[[4]])), unit = "degC")
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['pco2'], values = c(as.double(paramsGroup[[5]])), unit = "ppmv CO2")
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['q10'],  values = c(as.double(paramsGroup[[6]])), unit = "unitless")
        hector::setvar(hcores[[i]], dates = NA, var = globalParameters['volc'], values = c(as.double(paramsGroup[[7]])), unit = "unitless")
      }
     # setParamsChanged(toggle = TRUE)
      resetCore()
    }
  }

  # Observer function to handle the user input on the reset parameters button - reset hector parameters to defaults
  resetParams <- function()
  {
    print("in reset params")
    loadModelParameters()
    setParamsChanged(FALSE)
    # if(length(hcores) >= 1)
    # {
    #
    #   updateSelectInput(session = session, inputId = "input_paramToggle", selected = "default")
    #   # restartCore()
    #   # loadParameters()
    # }
  }

  # Function that gets the input parameters from the hector core and maps them to the input fields
  # This would normally be called on first load or when parameters are reset.
  loadParameters <- function()
  {
    print("in load params")

    # Fetch hector parameters from core
    for(i in 1:length(hcores))
    {
      hdata <- hector::fetchvars(core = hcores[[i]], dates = NA, vars = globalParameters, "\n")
    }

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



  # Observer function to handle user click on the set parameters button.
  setParameters <- function()
  {
    print("in set parameters")
    if(length(hcores) >= 1)
    {
      newVals <- vector()
      # Run through variables and make sure none are left empty and update the top level scope paramsList variable
      # and the hector core with any changed values
      tryCatch(
      {
        if(!is.na(input$input_aero))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['aero'], values = c(as.double(input$input_aero)), unit = "unitless")
          }
          paramsList['alpha'] <<- as.double(input$input_aero)
        }
        if(!is.na(input$input_beta))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['beta'], values = c(as.double(input$input_beta)), unit = "unitless")
          }
          paramsList['beta'] <<- as.double(input$input_aero)
        }
        if(!is.na(input$input_diff))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['diff'], values = c(as.double(input$input_diff)), unit = "cm2/s")
          }
          paramsList['diff'] <<- as.double(input$input_aero)
        }
        if(!is.na(input$input_ecs))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['ecs'],  values = c(as.double(input$input_ecs)), unit = "degC")
          }
          paramsList['S'] <<- as.double(input$input_aero)
        }
        if(!is.na(input$input_pco2))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['pco2'], values = c(as.double(input$input_pco2)), unit = "ppmv CO2")#c(150), unit="ppmv CO2")
          }
          paramsList['C'] <<- as.double(input$input_aero)
        }
        if(!is.na(input$input_q10))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['q10'],  values = c(as.double(input$input_q10)), unit = "unitless")
          }
          paramsList['q10_rh'] <<- as.double(input$input_aero)
        }
        if(!is.na(input$input_volc))
        {
          for(i in 1:length(hcores))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['volc'], values = c(as.double(input$input_volc)), unit = "unitless")
          }
          paramsList['volscl'] <<- as.double(input$input_aero)
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

      })
    }
  }

  setParamsChanged <- function(toggle)
  {
    print("in set Params Changed")
    if(toggle == TRUE)
    {
      paramsChanged <<- TRUE
      # shinyjs::enable("set_Params")
      # shinyjs::toggleClass(id = "set_Params", class = "changedParamsTrue")
      # shinyjs::runjs(paste0('$("#set_Params").css({"border": "1px #AC7023 solid"})'))
    }
    else
    {

      paramsChanged <<- FALSE
      # shinyjs::toggleClass(id = "set_Params", class = "changedParamsFalse")
    }

  }

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
          outputVariables[i] <<- globalCapabilities[capabilityValues[i]]
          attr(outputVariables[[i]], "name") <<- attr(globalCapabilities[capabilityValues[i]], "name")
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

  # Observer function that is activated on a change to the RCP Scenario drop down field.
  # This function will load the chosen scenario and rerun Hector
  setRCP <- function(scenarioName)
  {
    print("in set RCP")
    #browser()
    tryCatch(
    {
      # If scenario is checked then load it, otherwise unload it
      if(input[[paste("input_RCP",scenarioName, sep = "")]])
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

  # Observer function to handle the user input on the load custom scenario button
  loadCustomScenario <- function()
  {
    print("in load custom scenario")
  }

  # Observer function designed to handle the loading/creation of the output graphs from the Hector model.
  loadGraph <- function()
  {
    print("in load graph")
    hdata <- data.frame()
    df_total <- data.frame()
    if(length(outputVariables) < 5)
    {
      tryCatch(
      {
       if(length(outputVariables) >= 1)
       {
         for (i in 1:length(outputVariables))
         {
           # Need local so that each item gets its own number. Without it, the value
           # of i in the renderPlot() will be the same across all instances, because
           # of when the expression is evaluated.
           local(
           {#
              my_i <- i
              plotname <- paste("plot", i, sep="")
              plottitle <- paste("plottitle", globalScenarios[i], sep="")
              tablename <- paste("tablename", globalScenarios[i], sep="")
              for(j in 1:length(hcores))
              {
               # browser()
                hdata <- hector::fetchvars(core = hcores[[j]], dates = 1800:globalVars['endDate'], vars = outputVariables[i], "\n")
                hdata <- dplyr::mutate(hdata, scenario=paste("RCP ", names(hcores[j])))
                df_total <- rbind(df_total,hdata)
              }
              x <- dplyr::distinct(hdata, units)
              ggplotGraph <- ggplot2::ggplot(data=df_total, ggplot2::aes(x=year, y=value, group=variable, color=scenario)) + ggplot2::geom_line() + ggplot2::facet_wrap(~variable, scales='free_y') +
                            ggthemes::theme_solarized(light = TRUE)+ ggplot2::ylab(x)
              localPlot <- plotly::ggplotly(p = ggplotGraph)
              plotly::layout(p=localPlot, title="testing 123", hovermode="closest", xaxis = a, yaxis = a )
              #browser()
              # output$plot1 <<-  plotly::renderPlotly(localPlot)

              output[[plotname]] <- plotly::renderPlotly(localPlot)
              # output[[plottitle]] <- renderText({paste("1:", my_i, ".  n is ", 4, sep = "")})
              # output[[tablename]] <- renderTable({table(x = 1:my_i, y = 1:my_i)})
             })
           }
       }
       else
       {
         #shinyalert::shinyalert("Invalid Input:", "Please choose an at least 1 output variable to view graphs.", type = "warning")
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
        shinyalert::shinyalert("Error Detected:",print(paste('There was an error when attempting to load the graph:',err)), type = "error")
      })
    }
    else
    {
      shinyalert::shinyalert("Invalid Input:", "Please choose no more than 4 output variables.", type = "warning")
    }
  }

  loadCustomScenario <- function()
  {
    print("in load custom")
    if ( is.null(input$input_ScenarioFile)) return(NULL)
    tryCatch(
    {
      # browser()
      inifile <<-  Sys.glob(input$input_ScenarioFile$datapath)

      hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name="testing123")
      updateTextInput(session=session, "input_ScenarioName", value=paste("input$input_RCP"))
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

  openPage <- function(url) {
    return(tags$a(href=url, "Click here!", target="_blank"))

  }

  # Initialize reactive values
  values <- reactiveValues()
  #values$themes <- themes

  observe({
    input$input_pco2
    input$input_q10
    input$input_volc
    input$input_aero
    input$input_beta
    input$input_diff
    input$input_ecs
    setParamsChanged(TRUE)
  })

  # Download handler for downloading the raw data output from a Hector run. This is activated upon button click.
  output$dlData <- downloadHandler(
    filename = function()
    {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file)
    {
      dataList <- list()
      df <- data.frame()
      for(i in 1:length(hcores))
      {
        hdata <- hector::fetchvars(core = hcores[[i]], dates = 1800:globalVars['endDate'], vars = outputVariables, "\n")
        hdata <- dplyr::mutate(hdata)
        hdata <- dplyr::mutate(hdata, scenario=names(globalScenarios[i]))
        df <- data.frame(hdata)
        dataList[[i]] <- df
       # browser()
      }

      write.csv(dataList, file, row.names = FALSE)
    }
  )
}
