library(R6)
library(shiny)
library(hector)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(plotly)
library(DT)
library(shinyWidgets)
library(gifski)
library(gganimate)
library(tidyverse)
library(shinyBS)
library(zip)
library(svglite)

source("components/modules/mod_run.R", local = TRUE)
source("components/modules/mod_custom.r", local = TRUE)
source("components/modules/mod_summary.r", local = TRUE)
source("components/modules/mod_download.R", local = TRUE)
source("components/modules/mod_tracking.R", local = TRUE)
source("components/functions/func_graph_plots.R", local = TRUE)
source("components/functions/func_custom_emissions.R", local = TRUE)

theme_set(theme_minimal())

# Define R6 class
HectorInputs <- R6Class(
  classname = "HectorInputs",
  public = list(
    ini_file = NULL,
    time = NA,
    output = NULL,
    run_name = NA,
    permafrost = NULL,
    run_mode = NULL,
    inputs = NULL,
    core = NULL,
    selected_var = NULL,
    initialize = function(ini_file = system.file("input/hector_ssp245.ini",
                                                 package = "hector")) {
      self$ini_file <- ini_file
      self$time <- time
      self$output <- list()
      self$run_name <- 1
      self$inputs <- list()
      self$selected_var <- "CO2_concentration"
    }
  )
)

#' Global scenario list with Hector input file
#' @export
get_scenarios <- function() {

    scenarios <- list("SSP 1-1.9"="input/hector_ssp119.ini",
                      "SSP 1-2.6"="input/hector_ssp126.ini",
                      "SSP 2-4.5"="input/hector_ssp245.ini",
                      "SSP 3-7.0"="input/hector_ssp370.ini",
                      "SSP 4-3.4"="input/hector_ssp434.ini",
                      "SSP 4-6.0"="input/hector_ssp460.ini",
                      "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                      "SSP 5-8.5"="input/hector_ssp585.ini")

    return(scenarios)
}

#' Global list of variable titles
#' @export
get_titles <- function() {

    title <- list("CO2_concentration" = "Atmospheric CO2",
                  "atmos_co2" = "Atmospheric Carbon Pool",
                  "ffi_emissions" = "FFI Emissions",
                  "luc_emissions" = "LUC Emissions",
                  "N2O_concentration" = "N2O Concentration",
                  "BC_emissions" = "Black Carbon Emissions",
                  "OC_emissions" = "Organic Carbon Emissions",
                  "RF_tot" = "Total Radiative Forcing",
                  "RF_albedo" = "Albedo Radiative Forcing",
                  "RF_CO2" = "CO2 Radiative Forcing",
                  "RF_N2O" = "N2O Radiative Forcing",
                  "RF_BC" = "Black Carbon Radiative Forcing",
                  "RF_OC" = "Organic Carbon Radiative Forcing",
                  "RF_SO2" = "Total SO2 Radiative Forcing",
                  "RF_vol" = "Volcanic Activity Radiative Forcing",
                  "FCH4" = "CH4 Radiative Forcing",
                  "RF_CF4" = "CF4 Radiative Forcing",
                  "RF_C2F6" = "C2F6 Radiative Forcing",
                  "RF_HFC23" = "HFC-23 Radiative Forcing",
                  "RF_HFC4310" = "HFC-4310 Radiative Forcing",
                  "RF_HFC125" = "HFC-125 Radiative Forcing",
                  "RF_HFC143a" = "HFC-143a Radiative Forcing",
                  "RF_HFC245fa" = "HFC-245fa Radiative Forcing",
                  "RF_SF6" = "SF6 Radiative Forcing",
                  "RF_CFC11" = "CFC-11 Radiative Forcing",
                  "RF_CFC12" = "CFC-12 Radiative Forcing",
                  "RF_CFC113" = "CFC-113 Radiative Forcing",
                  "RF_CFC114" = "CFC-114 Radiative Forcing",
                  "RF_CFC115" = "CFC-115 Radiative Forcing",
                  "RF_CCl4" = "CCl4 Radiative Forcing",
                  "RF_CH3CCl3" = "CH3CCl3 Radiative Forcing",
                  "RF_halon1211" = "Halon-1211 Radiative Forcing",
                  "RF_halon1301" = "Halon-1301 Radiative Forcing",
                  "RF_halon2402" = "Halon-2402 Radiative Forcing",
                  "RF_CH3Cl" = "CH3Cl Radiative Forcing",
                  "RF_CH3Br" = "CH3Br Radiative Forcing",
                  "CH4_concentration" = "Atmospheric CH4",
                  "CH4_emissions" = "CH4 Emissions",
                  "SO2_emissions" = "Anthropogenic SO2",
                  "SV" = "Volcanic SO2",
                  "global_tas" = "Global Mean Temperature",
                  "gmst" = "Equilibrium Global Temperature",
                  "sst" = "Ocean Surface Temperature",
                  "ocean_tas" = "Ocean Air Temperature",
                  "heatflux_mixed" = "Heat Flux - Mixed Layer Ocean",
                  "heatflux_interior" = "Heat Flux - Interior Layer Ocean",
                  "heatflux" = "Total Heat Flux - Ocean")

    return(title)
}

scenarios <- get_scenarios()
title <- get_titles()
