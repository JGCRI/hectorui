
#' Loads one of the preset RCP scenarios
#'
#' Main function that loads/starts the Hector Core and runs the specified scenario
#'
#' @param scenario (Character) - RCP Scenario name to load into new Hector core
#'
#' @return (Hector Core) - The Hector core object created from the scenario
#' @export
#'
#' @examples
loadScenario <- function(scenario)
{
  print("in load scenario")
  tryCatch(
    {browser()
      inifile <<- system.file(globalScenarios[paste0("RCP-", scenario)], package='hector', mustWork=TRUE)
      hcore <- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP", scenario)]))
      setCoreParameters(hcore)
      hector::run(hcore, globalVars[['endDate']])
    },
    error = function(err)
    {
      shinyalert::shinyalert("Initalization Error", print(paste('Error starting Hector: ', err, ' scen= ', scenario, ' gs=',globalScenarios[paste("RCP", scenario)])), type = "error")
      print(scenario)
    })
  return(hcore)
}

#' Reset the active Hector cores and run the Hector core spinup
#'
#' Function to reset (not restart via shutdown) all active Hector cores. A core reset should only be called when input parameters have changed.
#' @return no return value
#' @export
#'
#' @examples
resetCore <- function()
{
  print("in reset core")
  # Call reset on each core that the user has created
  for(i in 1:length(hcores))
  {
    hector::reset(hcores[[i]])
    hector::run(hcores[[i]], globalVars[['endDate']])
  }
}

#' Restart the active Hector cores
#'
#' Function to shutdown and restart active Hector cores. A core restart is required when the scenario has changed/been loaded or the user has made emissions changes.
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
        # For each core, shutdown and restart
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
      # Call loadGraph to refresh output graph after data change
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
