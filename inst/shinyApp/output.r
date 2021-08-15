# This file handles anything output related (screen, file, etc)

#' Internal function used to clean up visual elements when the number of number of graph output variables changes
#'
#' @return Function does not return a value
#'
#' @examples
cleanPlots <- function()
{
  print("in clean plots")

  # Clean all plots if there's no active Hector cores
  if(length(hcores) < 1)
  {
    output[["plot1"]] <<- NULL
    output[["plot2"]] <<- NULL
    output[["plot3"]] <<- NULL
    output[["plot4"]] <<- NULL
  }
  else
  {
    # Start in reverse and clear out (make NULL) any plots that exceed the number of output variables
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

#' Output function that generates the graphs
#'
#' Observer function designed to handle the loading/creation of the output graphs
#' @return no return value
#' @export
#'
#' @examples
loadGraph <- function()
{
  print("in load graph")
  # Set up local variables for dealing with output data frames
  hdata <- data.frame()
  df_total <- data.frame()

  # Main loop that handles graph output based on number of scenarios and number of output variables
  if(length(hcores) > 0)
  {
    # If length is 5 or more than they've chosen too many variables
    if(length(outputVariables) < 5)
    {
      tryCatch(
        {
          if(length(outputVariables) >= 1)
          {
            withProgress(message = 'Loading Output Graphs...\n', value = 0,
            {
              # Create a new graph for each output variable
              for (i in 1:length(outputVariables))
              {
                # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances.
                local(
                {
                    my_i <- i
                    plotname <- paste("plot", i, sep="")
                    seriesname <- ""
                    # For each hector core create an output data set and bind all to df_total
                    for(j in 1:length(hcores))
                    {
                      scenarioName <- names(hcores)[j]
                      hdata <- hector::fetchvars(core = hcores[[j]], dates = globalVars[['startDate']]:globalVars[['endDate']], vars = outputVariables[i], "\n")
                      if(substr(scenarioName, 1, 8) =="Standard")
                        seriesname <- paste("RCP", substr(scenarioName, nchar(scenarioName)-2, nchar(scenarioName)))
                      else
                        seriesname <- names(hcores)[j]
                      hdata <- dplyr::mutate(hdata, Scenario = seriesname, Year = year, Value = round(value, 2))
                      df_total <- rbind(df_total,hdata)

                    }
                    # Get the units for graph axis
                    x <- dplyr::distinct(hdata, units)
                    ggplotGraph <- ggplot2::ggplot(data=df_total, ggplot2::aes(x=Year, y=Value, group=variable, color=Scenario)) +
                      ggplot2::geom_line() +
                      ggplot2::labs(y=Hmisc::capitalize(x[[1]]), title =  attr(outputVariables[[i]], 'longName')) +
                      ggplot2::scale_color_manual(values = globalColorScales)

                    #+ ggplot2::scale_color_manual(values=globalScenarioColors) + ggplot2::geom_ribbon(alpha=0.5)
                    # +  ggplot2::guides(color = ggplot2::guide_colorbar(title  = expression(beta)))
                    # +  ggplot2::scale_color_viridis_c()

                    # Construct the plots and add to the shiny output variable
                    localPlot <- plotly::ggplotly(p = ggplotGraph)
                    plotly::layout(p=localPlot, xaxis = a, yaxis = a, legend = list(orientation = 'h'))
                    output[[plotname]] <- plotly::renderPlotly(localPlot)
                })
                incProgress(1/length(hcores), detail = paste(attr(outputVariables[[i]], 'longName'), " loaded."))
                Sys.sleep(0.25)
              }
            })
            # Check if any output variables have been removed and clear any extra plots
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
#' Observer function designed to handle the loading/creation of downscaled world maps from the Hector model output and pre-generated pattern files
#' @return no return value
#' @export
#'
#' @examples
loadMap <- function()
{
  tryCatch(
  {
    # First check for existing cores
    if(length(hcores) < 1)
    {
      shinyalert::shinyalert("No active Hector cores", "Please set at least one of the RCP scenarios to active or upload a custom emissions scenario before mapping.", type = "warning")
    }
    else
    {
      # Need local so that each item gets its own number. Without it, the value of i in the renderPlot() will be the same across all instances.
      local(
      {
        withProgress(message = 'Generating Map Data...\n', value = 0,
        {
          # Choose pattern file based on user choice of temperature or precipitation
          if(input$mapVar == "tas")
            patternFile <- globalTempPatterns[[input$mapPattern]]
          else
            patternFile <- globalPrecipPatterns[[input$mapPattern]]

          # Fetch needed data from Hector cores
          results <- hector::fetchvars(hcores[[input$mapCore]], 1900:2100)
          # Use temperature data for downscaling both temp and precip
          tgav_hector <- dplyr::filter(results, variable == "Tgav")
          pattern <- readRDS(patternFile)
          coordinates <- pattern$coordinate_map
          incProgress(1/2, detail = paste("Loading pattern, downscaling"))
          # Construct coordinates based on -180 to 180 for longitude
          for(i in 1:length(coordinates$lon))
          {
            if(coordinates$lon[i] > 180)
            coordinates$lon[i] <- coordinates$lon[i] - 360
          }

          # Apply patterns to temp data and transform for mapping
          mapname <- paste("map", 1, sep="")
          hector_annual_gridded <- fldgen::pscl_apply(pattern$annual_pattern, as.vector(tgav_hector$value+15))
          hector_annual_gridded_t <- t(hector_annual_gridded)

          # Build aesthetics based on if the compare to 1900 was checked
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
              combined_data <- dplyr::mutate(coordinates, Temp = round(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899], 2),
                                             deltaTemp = round(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899] - hector_annual_gridded_t[, 1], 2),
                                             Lon=round(lon, 2), Lat=round(lat,2), Neg = ifelse(deltaTemp < 0, TRUE, FALSE))
        }
        else
        {
          # Build aesthetics based on if the compare to 1900 was checked
          if(input$input_map_compare)
          {
            mapFill <- "\u0394 Precip. - mm/day"
            mapVar <- "deltaPrecip"
          }
          else
          {
            mapFill <- "Precip. - mm/day"
            mapVar <- "Precip"
          }
          mapDirection <- 1
          mapPalette <- "Purples"
          browser()
          combined_data <- dplyr::mutate(coordinates, Precip = round(86400*hector_annual_gridded_t[, as.numeric(input$mapYear)-1899], 4),
                                           deltaPrecip = round(86400*(hector_annual_gridded_t[, as.numeric(input$mapYear)-1899] - hector_annual_gridded_t[, 1]), 4),
                                           Lon=round(lon, 2), Lat=round(lat,2), Neg = ifelse(deltaPrecip < 0, TRUE, FALSE))
        }

        combined_data <- dplyr::select(combined_data, -c(lat, lon, colnum))

        lat_min <- -90
        lat_max <- 90
        lon_min <- -180
        lon_max <- 180

        # Validate lat/lon min/max input fields if the filter by lat/lon was checked
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

          # Assign local variables to input fields
          lat_min <- as.numeric(input$input_lat_min)
          lat_max <- as.numeric(input$input_lat_max)
          lon_min <- as.numeric(input$input_lon_min)
          lon_max <- as.numeric(input$input_lon_max)

          # Filter data set by lat/lon
          combined_data <- dplyr::filter(combined_data, Lat >= lat_min, Lat <= lat_max, Lon >= lon_min, Lon <= lon_max)

        }

        # Create world map borders
        mapWorld <- ggplot2::borders("world", fill = NA)

        # Construct ggplot map object
        ggplotMap <- ggplot2::ggplot() +
          mapWorld +
          ggplot2::geom_raster(data = combined_data, ggplot2::aes_string(x="Lon", y = "Lat", fill=mapVar),interpolate = TRUE ) +
          ggplot2::coord_fixed() +
          ggplot2::scale_fill_distiller(palette = mapPalette,type = "div", direction = mapDirection, na.value = "Gray") + #, limits = c(-1,1)*max(abs(combined_data[[mapVar]]))) +
          ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = paste0(input$mapCore, " - ", input$mapYear), fill = mapFill) +
          ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))+
          ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))

        # Separate save plot fixes missing shape layer when saving
        ggplotSave <<- ggplot2::ggplot() +
          ggplot2::geom_raster(data = combined_data, ggplot2::aes_string(x="Lon", y = "Lat", fill=mapVar),interpolate = TRUE ) +
          mapWorld +
          ggplot2::coord_equal(clip = "off",expand = FALSE) +
          ggplot2::scale_fill_distiller(palette = mapPalette,type = "div", direction = mapDirection, na.value = "Gray") + #, limits = c(-1,1)*max(abs(combined_data[[mapVar]]))) +
          ggplot2::labs(x="\u00B0Longitude", y="\u00B0Latitude", title = paste0(input$mapCore, " - ", input$mapYear), fill = mapFill) +
          ggplot2::scale_y_continuous(limits=c(lat_min, lat_max), expand = c(0, 0), breaks=seq(-90,90,30))+
          ggplot2::scale_x_continuous(limits=c(lon_min, lon_max), expand = c(0, 0), breaks=seq(-180,180,30))

        localPlot <- plotly::ggplotly(p = ggplotMap  )
        plotly::layout(p=localPlot, yaxis = list(tickformat = "\u00B0C", dtick = 10, showgrid=FALSE))
        plotly::layout(p=localPlot,  xaxis = list(showgrid = FALSE))

        output[[mapname]] <- plotly::renderPlotly(localPlot)
        incProgress(1/1, detail = "Map loaded.")
        Sys.sleep(0.25)
        shinyjs::show(id = 'map-div')

        })
      })
    }
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
      }

      header_text <- paste("File created with Hector UI - https://github.com/JGCRI/hector-ui\n" ,
                           "Model Parameters: " , input$input_paramToggle , "\n",
                           "Alpha:,", input$input_aero, ",Beta:,", input$input_beta, ",Diff:,", input$input_diff,
                           ",ECS:,", input$input_ecs, ",CO2:,", input$input_pco2, ",Q10:,", input$input_q10, ",Volc:,", input$input_volc,
                           "\n")

      cat(header_text, file = file)
      lapply(dataList, function(x) write.table( data.frame(x), file  , append= T, sep=',', row.names = F,  ))
    }
    else
    {
      shinyalert::shinyalert("No active Hector cores", "Please set at least one of the RCP scenarios to active or upload a custom emissions scenario before downloading.", type = "warning")
    }
  }
)

# Download handler for higher resolution maps then the default plotly save
 output$downloadMap <- downloadHandler(
   filename = function()
     {
        paste("HectorMap",'.png',sep='')
     },
   content = function(file)
     {
       # browser()
        ggplot2::ggsave(filename = file, plot = ggplotSave, device = "png", dpi = 150, limitsize = TRUE, width = 15, height = 10)
    }
 )
