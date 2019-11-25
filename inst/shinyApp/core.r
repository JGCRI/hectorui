

#' Loads one of the preset RCP scenarios
#'
#' Main function that loads/starts the Hector Core and runs the specified scenario
#'
#' @param scenario
#'
#' @return The Hector core object created from the scenario
#' @export
#'
#' @examples
loadScenario <- function(scenario)
{
  print("in load scenario")
  tryCatch(
    {
      inifile <<- system.file(globalScenarios[paste("RCP", scenario)], package='hector', mustWork=TRUE)
      hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP", scenario)]))
      hector::run(hcore, globalVars['endDate'])
    },
    error = function(err)
    {
      shinyalert::shinyalert("Initalization Error",print(paste('Error starting Hector: ',err)), type = "error")
    })
  return(hcore)
}

#' Reset the active Hector cores and run the spinup
#'
#' Function to reset (not restart via shutdown) a Hector core. A core reset should only be called when input parameters have changed.
#' @return no return value
#' @export
#'
#' @examples
resetCore <- function()
{
  print("in reset core")
  for(i in 1:length(hcores))
  {
    hector::reset(hcores[[i]])
    hector::run(hcores[[i]], globalVars['endDate'])
  }

  #loadGraph()
}

#' Restart the active Hector cores
#'
#' Function to shutdown and restart active Hector cores. A core restart is called when the scenario has changed/been loaded or emissions changes
#' @return
#' @export
#'
#' @examples
restartCore <- function()
{
  print("in restart core")
  if(length(hcores) > 0)
  {
    withProgress(message = 'Restarting Hector Cores...\n', value = 0,
    {
      for(i in 1:length(hcores))
      {

          hcores[[i]] <<- hector::shutdown(core = hcores[[i]])
          inifile <<- system.file(globalScenarios[paste("RCP", names(hcores[i]))], package='hector', mustWork=TRUE)
          hcores[[i]] <<- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP",  names(hcores[i]))]))
          hector::run(hcores[[i]], globalVars['endDate'])
          incProgress(1/length(hcores), detail = paste0("Core ", names(hcores[i]), " Restart Successful."))
          Sys.sleep(0.1)
      }
    })
    # hcore <<- hector::shutdown(hcore)
    # startHector()
    loadGraph()
  }
  else
  {
    shinyalert::shinyalert("Warning:", "There are no active cores to reset emissions", type = "warning")
  }
}
