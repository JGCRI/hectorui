
# Load data processing file
#source("data_processing.R")
#themes <- sort(unique(data$theme))


# Define server logic required to draw a histogram
server <- function(input, output, session)
{
  # Set up local variables in top level scope
    firstLoad <- TRUE
    outputVariables <- vector()
    shinyjs::useShinyjs()
    inifile <- system.file('input/hector_rcp45.ini', package='hector', mustWork=TRUE)
    hcore <- hector::newcore(inifile, suppresslogging=TRUE, name="RCP 4.5")


    observeEvent(input$capabilities, setCapabilities())
    observeEvent(input$loadGraphs, loadGraph())
    observeEvent(input$set_Params, setParameters())
    observeEvent(input$input_ScenarioFile, loadScenario())
    observeEvent(input$reset_Params, resetParams())
    observeEvent(input$input_RCP, setRCP())

    setCapabilities <- function()
    {
      capabilityValues <- vector()
      if(length(input$capabilities) > 0)
      {
        capabilityValues <<- input$capabilities
        i <- 1
        while(i <= length(capabilityValues))
        {
          outputVariables[i] <<- globalCapabilities[capabilityValues[i]]
          i <- i+1
        }
      }
    }

    setRCP <- function()
    {
      tryCatch(
        {
          if(!firstLoad)
            hcore <<- hector::shutdown(hcore)
          inifile <<- system.file(globalScenarios[input$input_RCP], package='hector', mustWork=TRUE)
          hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name=input$input_ScenarioName)
          updateTextInput(session=session, "input_ScenarioName", value=paste(input$input_RCP))
          hector::run(hcore, 2300)
         # print(hcore)
         # print(head(hector::fetchvars(hcore, 1800:2100, vars = c(globalCapabilities['cc_acp']))))\
          output$plot <<- NULL
          hdata <- hector::fetchvars(core = hcore, dates = 1800:2300, vars = outputVariables, "\n")
          output$plot <<-  renderPlot(ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
                                        ggthemes::theme_solarized(light = TRUE))
        }
      )
      firstLoad <<- FALSE
    }
    resetParams <- function()
    {

    }

    loadScenario <- function()
    {

    }

    setParameters <- function()
    {
      newVals <- vector()
      tryCatch(
      {
        hector::setvar(hcore, dates = NA, var = globalParameters['aero'], values = c(as.double(input$input_aero)), unit = "unitless")
        hector::setvar(hcore, dates = NA, var = globalParameters['beta'], values = c(as.double(input$input_beta)), unit = "unitless")
        hector::setvar(hcore, dates = NA, var = globalParameters['diff'], values = c(as.double(input$input_diff)), unit = "cm2/s")
        hector::setvar(hcore, dates = NA, var = globalParameters['ecs'],  values = c(as.double(input$input_ecs)), unit = "degC")
        hector::setvar(hcore, dates = NA, var = globalParameters['pco2'], values = c(as.double(input$input_pco2)), unit = "ppmv CO2")#c(150), unit="ppmv CO2")
        hector::setvar(hcore, dates = NA, var = globalParameters['q10'],  values = c(as.double(input$input_q10)), unit = "unitless")
        hector::setvar(hcore, dates = NA, var = globalParameters['volc'], values = c(as.double(input$input_volc)), unit = "unitless")

       # hector::reset(hcore)
        hector::run(hcore, 2300)
       # print(hcore)
       # print(head(hector::fetchvars(hcore, 1800:2100, vars = c(globalCapabilities['cc_acp']))))
      },
      warning = function(war)
      {
        # warning handler picks up where error was generated
        showModal(modalDialog(
          title = "Important message",
          paste("MY_WARNING:  ",war)
        ))
        # e <- myDivide(d,0.1) # =60
        # f <- e + 100
        # return(f)

      },
      error = function(err)
      {
        # error handler picks up where error was generated
        cat(file=stderr(), "\n ERROR: Set Params - ", as.character(err[1]), sep=" ")
        #shinyalert::shinyalert("Oops!",print(paste('Error:',err)), type = "error")
        #bsModal("modalExample", "Your plot", "go", size = "large","asfd",downloadButton('downloadPlot', 'Download'))

      },
      finally =
        {
          cat(file=stderr(), "\nSetVars ","****Made it thru****", " trypas", "\n")
          # NOTE:  Finally is evaluated in the context of of the inital
          # NOTE:  tryCatch block and 'e' will not exist if a warning
          # NOTE:  or error occurred.
          #print(paste("e =",e))

        })
    }

    loadGraph <- function()
    {
      print(hcore)
      print(head(hector::fetchvars(hcore, 1800:2300, vars = c(globalCapabilities['cc_acp']))))
      tryCatch(
      {

          #browseEnv()
          fetchList <- write.table(matrix(as.character(newVals),nrow=1), sep=",", row.names=FALSE, col.names=FALSE)
          #cat(file=stderr(), "\nnewvals- ", print(hcore),  base::as.vector(c(paste((newVals), collapse=", "))), "\n")
          hdata <- hector::fetchvars(core = hcore, dates = 1800:2300, vars = outputVariables, "\n")
          output$plot <<-  renderPlot(ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
                                                                   ggthemes::theme_solarized(light = TRUE))
       # cat(file=stderr(), "\nhdata ", as.character(head(hdata)), " list", "\n")
          shinyalert::shinyalert("plot", output$plot <<-  renderPlot(ggplot2::ggplot(data=hdata, ggplot2::aes(x=year, y=value)) + ggplot2::geom_line(col="darkgrey", size=1.1) + ggplot2::facet_wrap(~variable, scales='free_y') +
                                     ggthemes::theme_solarized(light = TRUE)), type = "" )
        }
        else
        {
          shinyalert::shinyalert("Oops!",print(paste('Error: 3')), type = "error")
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
        cat(file=stderr(), "\nFinally ", "\n")
      })

    }

    #paste(shQuote(newVals), collapse=", ")
    # output$plot = renderPlotly(
    # {
    #   plot_ly((hdata), x = ~year) %>% add_lines(y = ~value)
    # })


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
    # Add observer on select-all button
    # The following approach generates bugs when clearing partial selections
    # observe({
    #     if(input$selectAllTop == 0) return()
    #     values$themes <- themes
    # })
    # observe({
    #     if(input$selectAllBottom == 0) return()
    #     values$themes <- themes
    # })
    # observe({
    #   if(input$selectAllTop > 0) {
    #     updateCheckboxGroupInput(session=session, inputId="themes",
    #                              choices=themes, selected=themes)
    #     values$themes <- themes
    #    }
    # })
    # observe({
    #   if(input$selectAllBottom > 0) {
    #    updateCheckboxGroupInput(session=session, inputId="themes",
    #                              choices=themes, selected=themes)
    # #     values$themes <- themes
    #  }
    #  })

    # Add observer on clear-all button
    # The following approach generates bugs when clearing partial selections
    #observe({
    #    if(input$clearAllTop == 0) return()
    #    values$themes <- NULL # or use c()
    #})
    #observe({
    #    if(input$clearAllBottom == 0) return()
    #    values$themes <- NULL # or use c()
    #})
    #  observe({
    #   if(input$clearAllTop > 0) {
    #     updateCheckboxGroupInput(session=session, inputId="themes",
    #                              choices=themes, selected=NULL)
    #     values$themes <- c()
    #   }
    # })


    # Create event type checkbox
    # output$themesControl <- renderUI({
    #   checkboxGroupInput('themes', 'LEGO Themes:',
    #                      themes, selected = values$themes)
    # })
    #
    # # Prepare dataset
    # dataTable <- reactive({
    #   groupByTheme(data, input$timeline[1],
    #                input$timeline[2], input$pieces[1],
    #                input$pieces[2], input$themes)
    # })
    #
    # dataTableBySetYear <- reactive({
    #   groupByYearSet(data, input$timeline[1],
    #                  input$timeline[2], input$pieces[1],
    #                  input$pieces[2], input$themes)
    # })
    #
    # dataTableByYear <- reactive({
    #   groupByYearAgg(data, input$timeline[1],
    #                  input$timeline[2], input$pieces[1],
    #                  input$pieces[2], input$themes)
    # })
    #
    # dataTableByPiece <- reactive({
    #   groupByYearAll(data, input$timeline[1],
    #                  input$timeline[2], input$pieces[1],
    #                  input$pieces[2], input$themes)
    # })
    #
    # dataTableByPieceAvg <- reactive({
    #   groupByPieceAvg(data, input$timeline[1],
    #                   input$timeline[2], input$pieces[1],
    #                   input$pieces[2], input$themes)
    # })
    #
    # dataTableByPieceThemeAvg <- reactive({
    #   groupByPieceThemeAvg(data, input$timeline[1],
    #                        input$timeline[2], input$pieces[1],
    #                        input$pieces[2], input$themes)
    # })
    #
    # # Render data table
    # output$dTable <- renderDataTable({
    #   dataTable()
    # } #, options = list(bFilter = FALSE, iDisplayLength = 50)
    # )
    #
    # output$setsByYear <- renderChart({
    #   plotSetsCountByYear(dataTableBySetYear())
    # })
    #
    # output$themesByYear <- renderChart({
    #   plotThemesCountByYear(dataTableByYear())
    # })
    #
    # output$piecesByYear <- renderChart({
    #   plotPiecesByYear(dataTableByPiece())
    # })
    #
    # output$piecesByYearAvg <- renderChart({
    #   plotPiecesByYearAvg(dataTableByPieceAvg())
    # })
    #
    # output$piecesByThemeAvg <- renderChart({
    #   plotPiecesByThemeAvg(dataTableByPieceThemeAvg())
    # })


}
