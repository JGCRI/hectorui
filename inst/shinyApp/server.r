
# Load data processing file
#source("data_processing.R")
#themes <- sort(unique(data$theme))


# Define server logic to drive UI interactions. The server function is the main function and required for Shiny apps.
server <- function(input, output, session)
{
    shinyjs::useShinyjs()

    # Set up local variables in top level application scope
    firstLoad <- TRUE
    outputVariables <- vector()
    inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)
    # This variable is for storing paremeter values so that if a change is made (like loading new scenario) the custom params set by user will persist
    paramsList <- list()
    paramsChanged <- FALSE

    # Hector core
    hcore <- hector::newcore(inifile, suppresslogging=TRUE, name="RCP 4.5")

    # Set up observer functions for user interactive fields
    observeEvent(input$capabilities, setCapabilities())
    observeEvent(input$loadGraphs, loadGraph())
    observeEvent(input$set_Params, setParameters())
    observeEvent(input$input_ScenarioFile, loadScenario())
    observeEvent(input$reset_Params, resetParams())
    observeEvent(input$input_RCP, setRCP())


    # Function that gets the parameters that are settable from the hector core and maps them to the input fields
    # This would normally be called on first load or when parameters are reset.
    loadParameters <- function()
    {
      hdata <- hector::fetchvars(core = hcore, dates = NA, vars = globalParameters, "\n")

      # Update on screen input components for parameter with parameter values stored in hector core
      updateNumericInput(session, "input_aero", value=round(hdata[which(hdata$variable == "alpha"), 4], 2))   #df[which(df$Amount == min(df$Amount)), ]
      updateNumericInput(session, "input_beta", value=round(hdata[which(hdata$variable == "beta"), 4], 2))
      updateNumericInput(session, "input_diff", value=round(hdata[which(hdata$variable == "diff"), 4], 2))
      updateNumericInput(session, "input_ecs",  value=round(hdata[which(hdata$variable == "S"), 4], 2))
      updateNumericInput(session, "input_pco2", value=round(hdata[which(hdata$variable == "C0"), 4], 2))
      updateNumericInput(session, "input_q10",  value=round(hdata[which(hdata$variable == "q10_rh"), 4], 2))
      updateNumericInput(session, "input_volc", value=round(hdata[which(hdata$variable == "volscl"), 4], 2))

      # Store params in the top level variable paramsList for persistence
      paramsList['alpha'] <- hdata[which(hdata$variable == "alpha"), 4]
      paramsList['beta'] <- hdata[which(hdata$variable == "beta"), 4]
      paramsList['diff'] <- hdata[which(hdata$variable == "diff"), 4]
      paramsList['S'] <- hdata[which(hdata$variable == "S"), 4]
      paramsList['C'] <- hdata[which(hdata$variable == "C0"), 4]
      paramsList['q10_rh'] <- hdata[which(hdata$variable == "q10_rh"), 4]
      paramsList['volscl'] <- hdata[which(hdata$variable == "volscl"), 4]
    }

    # Observer function that responds to changes in inputs from the output capabilities drop down field
    setCapabilities <- function()
    { print('set cap')
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
      else
      {
      }
    }

    # Observer function that is activated on a change to the RCP Scenario drop down field. This function will load the chosen scenario and rerun Hector
    setRCP <- function()
    {
      tryCatch(
      {
        # If this is called after initial load of Hector then need to restart core
        if(!firstLoad)
        {
          hcore <<- hector::shutdown(hcore)
        }
        inifile <<- system.file(globalScenarios[input$input_RCP], package='hector', mustWork=TRUE)
        hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name=input$input_ScenarioName)
        updateTextInput(session=session, "input_ScenarioName", value=paste(input$input_RCP))
        hector::run(hcore, globalVars['endDate'])

        if(firstLoad)
        {
          loadParameters()
        }
       # print(hcore)
       # print(outputVariables)

        #output$plot <<- NULL
        #hdata <- hector::fetchvars(core = hcore, dates = 1800:2300, vars = fetchList, "\n")
       # output$plot <<-  renderPlot(ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
       #                               ggthemes::theme_solarized(light = TRUE))
       # loadParameters()
        loadGraph()
      }
      )
      firstLoad <<- FALSE
    }

    # Observer function to handle the user input on the reset parameters button
    resetParams <- function()
    {
      paramsChanged <- FALSE
    }

    # Observer function to handle the user input on the load custom scenario button
    loadScenario <- function()
    {

    }

    # Observer function to handle user click on the set parameters button.
    setParameters <- function()
    {
      newVals <- vector()
      tryCatch(
      {
        if(!is.na(input$input_aero))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['aero'], values = c(as.double(input$input_aero)), unit = "unitless")
          paramsList['alpha'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }
        if(!is.na(input$input_beta))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['beta'], values = c(as.double(input$input_beta)), unit = "unitless")
          paramsList['beta'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }
        if(!is.na(input$input_diff))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['diff'], values = c(as.double(input$input_diff)), unit = "cm2/s")
          paramsList['diff'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }
        if(!is.na(input$input_ecs))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['ecs'],  values = c(as.double(input$input_ecs)), unit = "degC")
          paramsList['S'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }
        if(!is.na(input$input_pco2))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['pco2'], values = c(as.double(input$input_pco2)), unit = "ppmv CO2")#c(150), unit="ppmv CO2")
          paramsList['C'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }
        if(!is.na(input$input_q10))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['q10'],  values = c(as.double(input$input_q10)), unit = "unitless")
          paramsList['q10_rh'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }
        if(!is.na(input$input_volc))
        {
          hector::setvar(hcore, dates = NA, var = globalParameters['volc'], values = c(as.double(input$input_volc)), unit = "unitless")
          paramsList['volscl'] <<- as.double(input$input_aero)
          paramsChanged <- TRUE
        }

        hector::reset(hcore)
        hector::run(hcore, globalVars['endDate'])
        loadGraph()
       # print(hcore)
       # print(head(hector::fetchvars(hcore, 1800:globalVars['endDate'], vars = c(globalCapabilities['cc_acp']))))
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
        cat(file=stderr(), "\n ERROR: Set Params - ", as.character(err[1]), sep=" ")
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
     # print(hcore)
     # print(head(hector::fetchvars(hcore, 1800:2300, vars = c(globalCapabilities['cc_acp']))))
      tryCatch(
      {
       if(length(outputVariables) >= 1)
       {


          hdata <- hector::fetchvars(core = hcore, dates = 1800:globalVars['endDate'], vars = outputVariables, "\n")
          # gg = ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
          #   ggthemes::theme_solarized(light = TRUE)
          output$plot <<-  renderPlot(ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
                                          ggthemes::theme_solarized(light = TRUE))
          #browser()
        #  hdata <- tidyr::spread(data = dplyr::select(hdata, -units), key = variable, value = value, drop=F )
         # print(head(hdata))
      #    print(outputVariables)
         # print(fetchList)

          #output$plot <<- NULL
          # localPlot <- plotly::plot_ly(data = hdata, x = ~year) %>%   plotly::layout(title = "Scenario Results",
          #                                                                     paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
          #                                                                     xaxis = list(title = "Year",
          #                                                                                  gridcolor = 'rgb(255,255,255)',
          #                                                                                  showgrid = TRUE,
          #                                                                                  showline = FALSE,
          #                                                                                  showticklabels = TRUE,
          #                                                                                  tickcolor = 'rgb(127,127,127)',
          #                                                                                  ticks = 'outside',
          #                                                                                  zeroline = FALSE),
          #                                                                     yaxis = list(title = "Value",
          #                                                                                  gridcolor = 'rgb(255,255,255)',
          #                                                                                  showgrid = TRUE,
          #                                                                                  showline = FALSE,
          #                                                                                  showticklabels = TRUE,
          #                                                                                  tickcolor = 'rgb(127,127,127)',
          #                                                                                  ticks = 'outside',
          #                                                                                  zeroline = FALSE)
          #                                                                     )
          #
          # i <- 1
          # while(i <= length(outputVariables))
          # {
          #   yVal <- as.character(outputVariables[i[1]])
          #   print("yval: ")
          #   print(yVal)
          #   localPlot %>% plotly::add_trace(y = ~Ca, name =  yVal[1], type="scatter", mode = 'lines')
          #   i <- i+1
          # }
          # output$plot <<-  plotly::renderPlotly(localPlot)
         # hdata <- hector::fetchvars(core = hcore, dates = NA, vars = globalParameters['pco2'] , "\n")
        # print(hdata)
           #output$plot <<- plotly::ggplotly(gg)
        #  data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
       # cat(file=stderr(), "\nhdata ", as.character(head(hdata)), " list", "\n")
         # shinyalert::shinyalert("plot", output$plot <<-  renderPlot(ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
          #                           ggthemes::theme_solarized(light = TRUE)), type = "" )
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
        cat(file=stderr(), "\nError ","error in graph ", " " , "\n")
        shinyalert::shinyalert("Oops!",print(paste('Error:',err)), type = "error")
        #bsModal("modalExample", "Your plot", "go", size = "large","asfd",downloadButton('downloadPlot', 'Download'))

      },
      finally =
      {
        #cat(file=stderr(), "\nFinally ", "\n")
      })

    }

    # getPage <- function(url) {
    #      return(tags$iframe(src = url,
    #                         style="width:100%;",
    #                         height = "500px"))
    #  }

    openPage <- function(url) {
      return(tags$a(href=url, "Click here!", target="_blank"))

    }



    # Initialize reactive values
    values <- reactiveValues()
    #values$themes <- themes

    observe({

    })


}
