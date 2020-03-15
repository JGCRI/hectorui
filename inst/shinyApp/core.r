
#' Loads one of the preset RCP scenarios
#'
#' Main function that loads/starts the Hector Core and runs the specified scenario
#'
#' @param scenario RCP Scenario name to load into new Hector core
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
      hcore <- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP", scenario)]))
      setCoreParameters(hcore)
      hector::run(hcore, globalVars[['endDate']])
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
    hector::run(hcores[[i]], globalVars[['endDate']])
  }
}

#' Restart the active Hector cores
#'
#' Function to shutdown and restart active Hector cores. A core restart is required when the scenario has changed/been loaded or emissions changes.
#' @return
#' @export
#'
#' @examples
restartCore <- function()
{
  print("in restart core")
  tryCatch(
  {
    if(length(hcores) > 0)
    {
      withProgress(message = 'Restarting Hector Cores...\n', value = 0,
      {
        for(i in 1:length(hcores))
        {
          scenarioName <- names(hcores)[i]
          if(substr(scenarioName, 1, 8) =="Standard")
          {
            core <- hcores[[i]]
            inifile <- system.file(core$name, package='hector', mustWork=TRUE)
            hector::shutdown(core = hcores[[i]])
            hcores[[scenarioName]] <<- loadScenario(substr(scenarioName, nchar(scenarioName)-2, nchar(scenarioName))) #  hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP",  scenario)]))
            hector::run(hcores[[i]], globalVars[['endDate']])
            incProgress(1/length(hcores), detail = paste0("Core ", names(hcores)[i], " Restart Successful."))
            Sys.sleep(0.1)
          }
        }
      })
      # hcore <<- hector::shutdown(hcore)
      # startHector()
      loadGraph()
    }
    else
    {
      shinyalert::shinyalert("Warning:", "There are no active cores to reset emissions", type = "warning")
    }},
  error = function(err)
  {
    shinyalert::shinyalert("Core Error",print(paste('Error restarting Hector: ',err)), type = "error")
  })
}
