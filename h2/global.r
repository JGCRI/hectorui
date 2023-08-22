library(R6)
library(shiny)
library(hector)
library(dplyr)
library(ggplot2)
library(shinycssloaders)

source("./components/modules/mod_graph.r")
source("./components/modules/mod_run.r")
source("./components/modules/mod_summary.r")

# Define R6 class
HectorInputs <- R6Class(
  classname = "HectorInputs",
  public = list(
    ini_file = NULL,
    start = NA,
    end = NA,
    output = NULL,
    initialize = function(ini_file=system.file("input/hector_ssp245.ini",
                                          package="hector"),
                          start=2000,end=2300) {
      self$ini_file <- ini_file
      self$start <- start
      self$end <- end
      stopifnot(end>start) #gotta have the start year before the end year
    }
  )
)
