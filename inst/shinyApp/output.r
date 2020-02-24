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
{#browser()
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
                      scenarioName <- names(hcores)[j]
                      hdata <- hector::fetchvars(core = hcores[[j]], dates = globalVars[['startDate']]:globalVars[['endDate']], vars = outputVariables[i], "\n")
                      if(substr(scenarioName, 1, 8) =="Standard")
                        seriesname <- paste0("RCP", substr(scenarioName, nchar(scenarioName)-3, nchar(scenarioName)))
                      else
                        seriesname <- names(hcores)[j]
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
    if(length(hcores) < 1)
    {
      shinyalert::shinyalert("No active Hector cores", "Please set at least one of the RCP scenarios to active or upload a custom emissions scenario before mapping.", type = "warning")
    }
    else
    {

      local(
      {
        withProgress(message = 'Generating Map Data...\n', value = 0,
        {
          #browser()
          if(input$mapVar == "tas")
            patternFile <- globalTempPatterns[[input$mapPattern]]
          else
            patternFile <- globalPrecipPatterns[[input$mapPattern]]
          results <- hector::fetchvars(hcores[[input$mapCore]], 1900:2100)
          tgav_hector <- dplyr::filter(results, variable == "Tgav")
          pattern <- readRDS(patternFile)
          coordinates <- pattern$coordinate_map
          incProgress(1/2, detail = paste("Loading pattern, downscaling"))
          for(i in 1:length(coordinates$lon))
          {
            if(coordinates$lon[i] > 180)
            coordinates$lon[i] <- coordinates$lon[i] - 360
          }

          mapname <- paste("map", 1, sep="")
          hector_annual_gridded <- fldgen::pscl_apply(pattern$annual_pattern, as.vector(tgav_hector$value+15))
          hector_annual_gridded_t <- t(hector_annual_gridded)

          if(input$mapVar == "tas")
          {
            if(input$input_map_compare)
            {
              mapFill <- "\u0394 Temperature \u00B0C"
              mapVar <- "deltaTemp"
            }
            else
            {
              mapFill <- "Temperature \u00B0C"
              mapVar <- "Temp"
            }
            mapPalette <- "RdYlBu"
            mapDirection <- -1
           # mapVar <- "Temp"
           # if(input$input_map_compare)
              combined_data <- dplyr::mutate(coordinates, Temp = round(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899], 2),
                                             deltaTemp = round(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899] - hector_annual_gridded_t[, 1], 2), Lon=round(lon, 2), Lat=round(lat,2),
                                             Neg = ifelse(deltaTemp < 0, TRUE, FALSE))
           # else
            #  combined_data <- dplyr::mutate(combined_data, deltaTemp = round(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899], 2), Lon=round(lon, 2), Lat=round(lat,2))
           # combined_data$Neg <- ifelse(combined_data$Temp < 0, TRUE, FALSE)
           # browser()
          }
          else
          {
            if(input$input_map_compare)
            {
              mapFill <- "\u0394 Precip. - g/m2/s"
              mapVar <- "deltaPrecip"
            }
            else
            {
              mapFill <- "Precip. - g/m2/s"
              mapVar <- "Precip"
            }
            mapDirection <- 1
            mapPalette <- "Purples"
           # mapVar <- "Precip"
           # if(input$input_map_compare)
              combined_data <- dplyr::mutate(coordinates, Precip = round(1000*hector_annual_gridded_t[, as.numeric(input$mapYear)-1899], 4),
                                             deltaPrecip = round(1000*(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899] - hector_annual_gridded_t[, 1]), 4),
                                             Lon=round(lon, 2), Lat=round(lat,2), Neg = ifelse(deltaPrecip < 0, TRUE, FALSE))
          #  else
            #  combined_data <- dplyr::mutate(coordinates, Precip = round(1000*hector_annual_gridded_t[, as.numeric(input$mapYear)-1899], 4), Lon=round(lon, 2), Lat=round(lat,2))
           # combined_data$Neg <- ifelse(combined_data$Precip < 0, FALSE, TRUE)
          }

          combined_data <- dplyr::select(combined_data, -c(lat, lon, colnum))

          lat_min <- -90
          lat_max <- 90
          lon_min <- -180
          lon_max <- 180

          if(input$input_map_filter)
          {
            validate(need(as.numeric(input$input_lat_min) >= -90 && (as.numeric(input$input_lat_min)) <= 90 &&
                            (as.numeric(input$input_lat_min)) < (as.numeric(input$input_lat_max)), "Please enter a valid lat min"))
            validate(need(as.numeric(input$input_lat_max) >= -90 && (as.numeric(input$input_lat_max)) <= 90 &&
                            (as.numeric(input$input_lat_max)) > (as.numeric(input$input_lat_min)), "Please enter a valid lat max"))
            validate(need(as.numeric(input$input_lon_min) >= -180 && (as.numeric(input$input_lon_min)) <= 180 &&
                            (as.numeric(input$input_lon_min)) < (as.numeric(input$input_lon_max)), "Please enter a valid lon min"))
            validate(need(as.numeric(input$input_lon_max) >= -180 && (as.numeric(input$input_lon_max)) <= 180 &&
                            (as.numeric(input$input_lon_max)) > (as.numeric(input$input_lon_min)), "Please enter a valid lon max"))

            lat_min <- as.numeric(input$input_lat_min)
            lat_max <- as.numeric(input$input_lat_max)
            lon_min <- as.numeric(input$input_lon_min)
            lon_max <- as.numeric(input$input_lon_max)

            combined_data <- dplyr::filter(combined_data, Lat >= lat_min, Lat <= lat_max, Lon >= lon_min, Lon <= lon_max)

          }
          mapWorld <- ggplot2::borders("world",  ylim=c(lat_min, lat_max), xlim=c(lon_min, lon_max)) #  colour="black", col="white",, fill="gray100"

          ggplotMap <- ggplot2::ggplot() +
            mapWorld +
            ggplot2::geom_tile(data = combined_data, ggplot2::aes_string(x="Lon", y = "Lat", fill=mapVar)) +
            # ggplot2::geom_point(data = combined_data, ggplot2::aes(x = Lon, y = Lat, color = Neg, alpha = 0.5)) +
            ggplot2::coord_fixed(ratio = 1) +
            ggplot2::scale_fill_distiller(palette = mapPalette,type = "div", direction = mapDirection, na.value = "Gray" ) +
            #viridis::scale_fill_viridis(direction = 1, option = "E" ) +
            ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = paste0(input$mapCore, " - ", input$mapYear), fill = mapFill) +
            ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))+
            ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))

          localPlot <- plotly::ggplotly(p = ggplotMap)
          plotly::layout(p=localPlot, yaxis = list(tickformat = "\u00B0C", dtick = 10))

          output[[mapname]] <- plotly::renderPlotly(localPlot)
          incProgress(1/1, detail = "Map loaded.")
          Sys.sleep(0.25)
          shinyjs::show(id = 'map-div')

        })
      })
    }
  },
  warning = function(war)
  {
    # warning handler picks up where error was generated
    showModal(modalDialog(
      title = "Important message",
      paste("Warning:  ",war)
    ))

  },
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

      header_text <- paste("File created with Hector UI - https://github.com/JGCRI/hector-ui\n" ,
                           "Model Parameters: " , input$input_paramToggle , "\n",
                           "Alpha:,", input$input_aero, ",Beta:,", input$input_beta, ",Diff:,", input$input_diff,
                           ",ECS:,", input$input_ecs, ",CO2:,", input$input_pco2, ",Q10:,", input$input_q10, ",Volc:,", input$input_volc,
                           "\n")

      cat(header_text, file = file)
      lapply(dataList, function(x) write.table( data.frame(x), file  , append= T, sep=',', row.names = F,  ))
      #write.csv(df, file, row.names = FALSE)
    }
    else
    {
      shinyalert::shinyalert("No active Hector cores", "Please set at least one of the RCP scenarios to active or upload a custom emissions scenario before downloading.", type = "warning")
    }
  }
)
