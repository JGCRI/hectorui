#' Clean up visual elements on change of number of output variables or scenarios
#'
#' @return
#' @export
#'
#' @examples
cleanPlots <- function()
{
  print("in clean plots")
  if(length(hcores) < 1)
  {
    output[["plot1"]] <<- NULL
    output[["plot2"]] <<- NULL
    output[["plot3"]] <<- NULL
    output[["plot4"]] <<- NULL
  }
  else
  {
    if(length(outputVariables) < 4)
      output[["plot4"]] <<- NULL
    if(length(outputVariables) < 3)
      output[["plot3"]] <<- NULL
    if(length(outputVariables) < 2)
      output[["plot2"]] <<- NULL
    if(length(outputVariables) < 1)
      output[["plot1"]] <<- NULL
  }
}

#' Load graph by proxy
#'
#' @return
#' @export
#'
#' @examples
loadGraphProxy <- function()
{
  loadGraph()
}

#' Main output function that generates the graphs
#'
#' Observer function designed to handle the loading/creation of the output graphs from the Hector model.
#' @return no return value
#' @export
#'
#' @examples
loadGraph <- function()
{
  print("in load graph")
  hdata <- data.frame()
  df_total <- data.frame()
  scale_colors <<- vector()
  if(length(hcores) > 0)
  {
    if(length(outputVariables) < 5)
    {
      tryCatch(
        {
          if(length(outputVariables) >= 1)
          {
            withProgress(message = 'Loading Output Graphs...\n', value = 0,
            {
              for (i in 1:length(outputVariables))
              {
                # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances.
                local(
                {
                    my_i <- i
                    plotname <- paste("plot", i, sep="")
                    seriesname <- ""
                    for(j in 1:length(hcores))
                    {
                      hdata <- hector::fetchvars(core = hcores[[j]], dates = globalVars[['startDate']]:globalVars[['endDate']], vars = outputVariables[i], "\n")
                      if(names(hcores[j])=="Custom")
                        seriesname <- input$input_ScenarioName
                      else
                        seriesname <- paste("RCP", names(hcores[j]))
                      hdata <- dplyr::mutate(hdata, Scenario = seriesname, Year = year)
                      df_total <- rbind(df_total,hdata)

                    }
                    x <- dplyr::distinct(hdata, units)
                    ggplotGraph <- ggplot2::ggplot(data=df_total, ggplot2::aes(x=Year, y=value, group=variable, color=Scenario)) + ggplot2::geom_line() +
                       ggplot2::labs(y=Hmisc::capitalize(x[[1]]), title =  attr(outputVariables[[i]], 'longName')) #+  ggplot2::scale_color_manual(values = globalColorScales)

                    #+ ggplot2::scale_color_manual(values=globalScenarioColors) + ggplot2::geom_ribbon(alpha=0.5)
                    # +  ggplot2::guides(color = ggplot2::guide_colorbar(title  = expression(beta)))
                    # +  ggplot2::scale_color_viridis_c()

                    localPlot <- plotly::ggplotly(p = ggplotGraph)
                    plotly::layout(p=localPlot, xaxis = a, yaxis = a, legend = list(orientation = 'h'))

                    output[[plotname]] <- plotly::renderPlotly(localPlot)
                    # output[[plottitle]] <- renderText({paste("1:", my_i, ".  n is ", 4, sep = "")})
                    # output[[tablename]] <- renderTable({table(x = 1:my_i, y = 1:my_i)})
                })
                incProgress(1/length(hcores), detail = paste(attr(outputVariables[[i]], 'longName'), " loaded."))
                Sys.sleep(0.25)
              }

              #browser()
            })
            if(length(outputVariables) < 4)
            {
              cleanPlots()
            }
          }
          else
          {
            shinyalert::shinyalert("Invalid Input:", "Please choose at least 1 output variables.", type = "warning")
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
  else
  {
    shinyalert::shinyalert("No active Hector cores", "Please set at least one of the RCP scenarios to active or upload a custom emissions scenario.", type = "warning")
  }
}

loadMap <- function()
{
  tryCatch(
  {#browser()
    local(
      {
        for(i in 1:length(hcores))
        {
          results <- hector::fetchvars(hcores[[i]], 2000:2100)
          #results <- hector::fetchvars(core = hcores[[j]], dates = globalVars[['startDate']]:globalVars[['endDate']], vars = outputVariables[i], "\n")
          tgav_hector <- filter(results, variable == "Tgav")
        }

        pattern <- readRDS("www/maps/tas_Amon_MIROC-ESM_esmrcp85_r1i1p1_200601-210012_pattern.rds")
        coordinates <- pattern$coordinate_map

        # Get the annual TAS in each grid cell as predicted by the annual pattern for the Hector tgav
        for(i in 1:length(hcores))
        {
          mapname <- paste("map", i, sep="")
          hector_annual_gridded <- pscl_apply(pattern$annual_pattern, as.vector(tgav_hector$value+15))
          hector_annual_gridded_t <- t(hector_annual_gridded)
          #browser()
          temp <- hector_annual_gridded_t
          combined_data <- mutate(coordinates, value = temp[, as.numeric(input$mapYear)-1999])

          world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
          ggplotMap <- ggplot(data=world) + geom_sf() +

            geom_raster(data = combined_data, aes(x=lon, y = lat, fill=value)) +
            coord_fixed(ratio = 1) +
            scale_fill_viridis(direction = -1) +
            theme_bw() +  coord_map("ortho", orientation = c(41, -74, 0))

          localPlot <- plotly::ggplotly(p = ggplotMap)
          # plotly::layout(p=localPlot, xaxis = a, yaxis = a, legend = list(orientation = 'h'))

          output[[mapname]] <- plotly::renderPlotly(localPlot)

        }
      }
    )
  },
  # warning = function(war)
  # {
  #   # warning handler picks up where error was generated
  #   showModal(modalDialog(
  #     title = "Important message",
  #     paste("MY_WARNING:  ",war)
  #   ))
  #
  # },
  error = function(err)
  {
    # error handler picks up where error was generated
    shinyalert::shinyalert("Error Detected:",print(paste('There was an error when attempting to load the graph:',err)), type = "error")
  })

  # coordinates <- coordinates %>% mutate(value = hector_annual_gridded_t[,101])



}


# Download handler for downloading the raw data output from a Hector run. This is activated upon button click.
output$downloadData <- downloadHandler(

  filename = function()
  {
    paste('Hector-data-', Sys.Date(), '.csv', sep='')
  },

  content = function(file)
  {
    if(length (hcores) > 0)
    {
      #browser()
      dataList <- list()
      df <- data.frame()
      seriesname <- ""
      for(i in 1:length(hcores))
      {
        hdata <- hector::fetchvars(core = hcores[[i]], dates = 1800:globalVars[['endDate']], vars = outputVariables, "\n")
        hdata <- dplyr::mutate(hdata)
        if(names(hcores[i])=="Custom")
          seriesname <- input$input_ScenarioName
        else
          seriesname <- paste("RCP ", names(hcores[i]))
        hdata <- dplyr::mutate(hdata, scenario=seriesname)
        df <- data.frame(hdata)
        dataList[[i]] <- df
        # browser()
      }
    }
    header_text <- paste("File created with Hector UI - https://github.com/JGCRI/hector-ui\n" ,
                         "Model Parameters: " , input$input_paramToggle , "\n",
                         "Alpha:,", input$input_aero, ",Beta:,", input$input_beta, ",Diff:,", input$input_diff,
                         ",ECS:,", input$input_ecs, ",CO2:,", input$input_pco2, ",Q10:,", input$input_q10, ",Volc:,", input$input_volc,
                         "\n")

    cat(header_text, file = file)
    lapply(dataList, function(x) write.table( data.frame(x), file  , append= T, sep=',', row.names = F,  ))
    #write.csv(df, file, row.names = FALSE)
  }
)
