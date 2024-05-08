library(R6)
library(shiny)
library(hector)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(plotly)
library(DT)
library(shinyWidgets)
library(gganimate)
library(tidyverse)
library(shinyBS)
library(zip)
library(svglite)

source("./components/modules/mod_graph.r")
source("./components/modules/mod_run.r")
source("./components/modules/mod_custom.r")
source("./components/modules/mod_summary.r")
source("./components/modules/mod_download.r")
source("./components/modules/mod_tracking.r")
source("./components/functions/func_graph_plots.R")
source("./components/functions/func_custom_emissions.R")

theme_set(theme_minimal())

# Define R6 class
HectorInputs <- R6Class(
  classname = "HectorInputs",
  public = list(
    ini_file = NULL,
    time = NA,
    output = NULL,
    no_save_output = NULL,
    no_save = NULL,
    run_name = NA,
    permafrost = NULL,
    save = NULL,
    inputs = NULL,
    selected_var = NULL,
    initialize = function(ini_file = system.file("input/hector_ssp245.ini",
                                                 package = "hector")) {
      self$ini_file <- ini_file
      self$time <- time
      self$output <- list()
      self$no_save <- NULL
      self$run_name <- 1
      self$inputs <- list()
      self$selected_var <- "CO2_concentration"
    }
  )
)

scenarios <- list("SSP 1-1.9"="input/hector_ssp119.ini",
                  "SSP 1-2.6"="input/hector_ssp126.ini",
                  "SSP 2-4.5"="input/hector_ssp245.ini",
                  "SSP 3-7.0"="input/hector_ssp370.ini",
                  "SSP 4-3.4"="input/hector_ssp434.ini",
                  "SSP 4-6.0"="input/hector_ssp460.ini",
                  "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                  "SSP 5-8.5"="input/hector_ssp585.ini")

title <- list("CO2_concentration" = "Atmospheric CO2",
              "atmos_co2" = "Atmospheric Carbon Pool",
              "ffi_emissions" = "FFI Emissions",
              "luc_emissions" = "LUC Emissions",
              "N2O_concentration" = "N2O Concentration",
              "BC_emissions" = "Black Carbon Emissions",
              "OC_emissions" = "Organic Carbon Emissions",
              "RF_tot" = "RF - Total",
              "RF_albedo" = "RF - Albedo",
              "RF_CO2" = "RF - CO2",
              "RF_N2O" = "RF - N2O",
              "RF_BC" = "RF - Black Carbon",
              "RF_OC" = "RF - Organic Carbon",
              "RF_SO2" = "RF - Total SO2",
              "RF_VOL" = "RF - Volcanic Activity",
              "RF_CH4" = "RF - CH4",
              "RF_CF4" = "CF4 Forcing",
              "RF_C2F6" = "C2F6 Forcing",
              "RF_HFC23" = "HFC-23 Forcing",
              "RF_HFC4310" = "HFC-4310 Forcing",
              "RF_HFC125" = "HFC-125 Forcing",
              "RF_HFC143a" = "HFC-143a Forcing",
              "RF_HFC245fa" = "HFC-245fa Forcing",
              "RF_SF6" = "SF6 Forcing",
              "RF_CFC11" = "CFC-11 Forcing",
              "RF_CFC12" = "CFC-12 Forcing",
              "RF_CFC113" = "CFC-113 Forcing",
              "RF_CFC114" = "CFC-114 Forcing",
              "RF_CFC115" = "CFC-115 Forcing",
              "RF_CCl4" = "CCl4 Forcing",
              "RF_CH3CCl3" = "CH3CCl3 Forcing",
              "RF_halon1211" = "Halon-1211 Forcing",
              "RF_halon1301" = "Halon-1301 Forcing",
              "RF_halon2402" = "Halon-2402 Forcing",
              "RF_CH3Cl" = "CH3Cl Forcing",
              "RF_CH3Br" = "CH3Br Forcing",
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
              "heatflux" = "Total Heat Flux - Ocean"
)

units <- list("CO2_concentration" = "DegC",
              "atmos_co2" = "Pg C",
              "ffi_emissions" = "Pg C/yr",
              "luc_emissions" = "Pg C/yr",
              "N2O_concentration" = "ppbv N2O",
              "BC_emissions" = "Tg",
              "OC_emissions" = "Tg",
              "RF_tot" = "RF - Total",
              "RF_albedo" = "W/m2",
              "RF_CO2" = "W/m2",
              "RF_N2O" = "W/m2",
              "RF_BC" = "W/m2",
              "RF_OC" = "W/m2",
              "RF_SO2" = "W/m2",
              "RF_VOL" = "W/m2",
              "RF_CH4" = "W/m2",
              "RF_CF4" = "W/m2",
              "RF_C2F6" = "W/m2",
              "RF_HFC23" = "W/m2",
              "RF_HFC4310" = "W/m2",
              "RF_HFC125" = "W/m2",
              "RF_HFC143a" = "W/m2",
              "RF_HFC245fa" = "W/m2",
              "RF_SF6" = "W/m2",
              "RF_CFC11" = "W/m2",
              "RF_CFC12" = "W/m2",
              "RF_CFC113" = "W/m2",
              "RF_CFC114" = "W/m2",
              "RF_CFC115" = "W/m2",
              "RF_CCl4" = "W/m2",
              "RF_CH3CCl3" = "W/m2",
              "RF_halon1211" = "W/m2",
              "RF_halon1301" = "W/m2",
              "RF_halon2402" = "W/m2",
              "RF_CH3Cl" = "W/m2",
              "RF_CH3Br" = "W/m2",
              "CH4_concentration" = "ppbv CH4",
              "CH4_emissions" = "Tg CH4",
              "SO2_emissions" = "Gg SO2",
              "SV" = "W/m2",
              "global_tas" = "DegC",
              "gmst" = "DegC",
              "sst" = "DegC",
              "ocean_tas" = "DegC",
              "heatflux_mixed" = "W/m2",
              "heatflux_interior" = "W/m2",
              "heatflux" = "W/m2"
)

