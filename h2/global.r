library(R6)
library(shiny)
library(hector)
library(dplyr)
library(ggplot2)
library(shinycssloaders)
library(plotly)
#library(shinyalert) # don't need if we have shinyWidgets?
library(DT)
library(shinyWidgets)

source("./components/modules/mod_graph.r")
source("./components/modules/mod_run.r")
source("./components/modules/mod_summary.r")
source("./components/modules/mod_download.r")

# Define R6 class
HectorInputs <- R6Class(
  classname = "HectorInputs",
  public = list(
    ini_file = NULL,
    start = NA,
    end = NA,
    output = NULL,
    no_save = NULL,
    i = NA,
    save = NULL,
    initialize = function(ini_file = system.file("input/hector_ssp245.ini",
                                                 package = "hector"),
                          start = 2000,
                          end = 2300) {
      self$ini_file <- ini_file
      self$start <- start
      self$end <- end
      self$output <- list()
      #self$no_save <- NULL
      self$i <- 1
      #self$savetoggle <- FALSE
      stopifnot(end > start) #gotta have the start year before the end year
    }
  )
)