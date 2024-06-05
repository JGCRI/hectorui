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

scenarios <- get_scenarios()
units <- get_units()
title <- get_titles()

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
    inputs = NULL,
    core = NULL,
    selected_var = NULL#,
    # initialize = function(ini_file = system.file("input/hector_ssp245.ini",
    #                                              package = "hector")) {
    #   self$ini_file <- ini_file
    #   self$time <- time
    #   self$output <- list()
    #   self$run_name <- 1
    #   self$inputs <- list()
    #   self$selected_var <- "CO2_concentration"
    # }
  )
)
