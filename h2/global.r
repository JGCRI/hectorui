library(R6)
library(shiny)
library(hector)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(plotly)
library(shinyalert) # don't need if we have shinyWidgets?
library(DT)
library(shinyWidgets)

#setwd("~/GitHub/hectorui/h2")

source("./components/modules/mod_graph.r")
source("./components/modules/mod_run.r")
source("./components/modules/mod_summary.r")
source("./components/modules/mod_download.r")
source("./components/functions/func_graph_plots.R")

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
    save = NULL,
    inputs = NULL,
    selected_var = NULL,
    initialize = function(ini_file = system.file("input/hector_ssp245.ini",
                                                 package = "hector")) {
      self$ini_file <- ini_file
      self$time <- time
      # self$time[2] <- time
      self$output <- list()
      self$no_save <- NULL
      self$run_name <- 1
      self$inputs <- list()
      self$selected_var <- "CO2_concentration"
      #stopifnot(time[2] > time[1]) #gotta have the start year before the end year
    }
  )
)
