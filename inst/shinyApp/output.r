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
                {#browser()
                    my_i <- i
                    plotname <- paste("plot", i, sep="")
                    seriesname <- ""
                    for(j in 1:length(hcores))
                    {
                      hdata <- hector::fetchvars(core = hcores[[j]], dates = globalVars[['startDate']]:globalVars[['endDate']], vars = outputVariables[i], "\n")
                      if(hcores[[j]]$name=="custom")
                        seriesname <- input$input_custom_scenarioName
                      else
                        seriesname <- paste("RCP", names(hcores[j]))
                      hdata <- dplyr::mutate(hdata, Scenario = seriesname, Year = year, Value = round(value, 2))
                      df_total <- rbind(df_total,hdata)

                    }
                    x <- dplyr::distinct(hdata, units)
                    ggplotGraph <- ggplot2::ggplot(data=df_total, ggplot2::aes(x=Year, y=Value, group=variable, color=Scenario)) + ggplot2::geom_line() +
                       ggplot2::labs(y=Hmisc::capitalize(x[[1]]), title =  attr(outputVariables[[i]], 'longName')) #+  ggplot2::scale_color_manual(values = globalColorScales)

                    #+ ggplot2::scale_color_manual(values=globalScenarioColors) + ggplot2::geom_ribbon(alpha=0.5)
                    # +  ggplot2::guides(color = ggplot2::guide_colorbar(title  = expression(beta)))
                    # +  ggplot2::scale_color_viridis_c()

                    localPlot <- plotly::ggplotly(p = ggplotGraph)
                    plotly::layout(p=localPlot, xaxis = a, yaxis = a, legend = list(orientation = 'h'))

                    output[[plotname]] <- plotly::renderPlotly(localPlot)


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

#' Main output function that generates the downscaled maps
#'
#' Observer function designed to handle the loading/creation of downscaled world maps from the Hector model output.
#' @return no return value
#' @export
#'
#' @examples
loadMap <- function()
{
  tryCatch(
  {
    local(
      {
        withProgress(message = 'Generating Map Data...\n', value = 0,
        {
          for(i in 1:length(hcores))
          {
            results <- hector::fetchvars(hcores[[i]], 2000:2100)
            #results <- hector::fetchvars(core = hcores[[j]], dates = globalVars[['startDate']]:globalVars[['endDate']], vars = outputVariables[i], "\n")
            tgav_hector <- dplyr::filter(results, variable == "Tgav")
          }

          pattern <- readRDS(input$mapPattern)
          coordinates <- pattern$coordinate_map
          incProgress(1/length(hcores), detail = paste("Loading pattern, downscaling"))
          Sys.sleep(0.15)
          for(i in 1:length(coordinates$lon))
          {
            #browser()
            if(coordinates$lon[i] > 180)
            coordinates$lon[i] <- coordinates$lon[i] - 360
          }


          # Get the annual TAS in each grid cell as predicted by the annual pattern for the Hector tgav
          for(i in 1:length(hcores))
          {
            mapname <- paste("map", i, sep="")
            hector_annual_gridded <- fldgen::pscl_apply(pattern$annual_pattern, as.vector(tgav_hector$value+15))
            hector_annual_gridded_t <- t(hector_annual_gridded)
            #browser()
            temp <- hector_annual_gridded_t
            combined_data <- dplyr::mutate(coordinates, Temp = round(temp[, as.numeric(input$mapYear)-1999], 2), Lon=round(lon, 2), Lat=round(lat,2))
            combined_data <- dplyr::select(combined_data, -c(lat, lon, colnum))
            mapWorld <- ggplot2::borders("world",  ylim=c(-90, 90), xlim=c(-180, 180),exact=TRUE) #  colour="black", col="white",, fill="gray100"

            ggplotMap <- ggplot2::ggplot() +
              mapWorld +
              ggplot2::geom_tile(data = combined_data, ggplot2::aes(x=Lon, y = Lat, fill=Temp)) +
              ggplot2::coord_fixed(ratio = 1) +
              viridis::scale_fill_viridis(direction = -1) +
              ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = "Plot Title", fill = "Local Temp \u00B0C") +
              ggplot2::scale_y_continuous(limits=c(-93, 93), expand = c(0, 0), breaks=seq(-90,90,30))+
              ggplot2::scale_x_continuous(limits=c(-183, 180), expand = c(0, 0), breaks=seq(-180,180,30))

            localPlot <- plotly::ggplotly(p = ggplotMap)
            plotly::layout(p=localPlot, yaxis = list(tickformat = "\u00B0C", dtick = 10))

            output[[mapname]] <- plotly::renderPlotly(localPlot)

          }
        })
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
