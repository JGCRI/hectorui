
#----- CORE RELATED FUNCTIONS

# Main function that loads/starts the Hector Core and runs the default scenario
loadScenario <- function(scenario)
{
  print("in load scenario")
  tryCatch(
    { #browser()
      inifile <<- system.file(globalScenarios[paste("RCP", scenario)], package='hector', mustWork=TRUE)
      hcore <<- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP", scenario)]))
      # updateTextInput(session=session, "input_ScenarioName", value=paste(globalScenarios[paste("RCP", scenario)]))
      hector::run(hcore, globalVars['endDate'])

    },
    error = function(err)
    {
      shinyalert::shinyalert("Initalization Error",print(paste('Error starting Hector: ',err)), type = "error")
    })
  hcore
}

# Function to reset (not restart via shutdown) a Hector core. A core reset should only be called when input parameters have changed.
resetCore <- function()
{
  print("in reset core")
  for(i in 1:length(hcores))
  {
    hector::reset(hcores[[i]])
    hector::run(hcores[[i]], globalVars['endDate'])
  }

  loadGraph()
}

# Function to shutdown and create a new Hector core. A core restart should only be called when the scenario (RCP or custom) has changed/been loaded
restartCore <- function()
{
  print("in restart core")
  for(i in 1:length(hcores))
  {
    hcores[[i]] <<- hector::shutdown(core = hcores[[i]])
    inifile <<- system.file(globalScenarios[paste("RCP", names(hcores[i]))], package='hector', mustWork=TRUE)
    hcores[[i]] <<- hector::newcore(inifile, suppresslogging=TRUE, name=paste(globalScenarios[paste("RCP",  names(hcores[i]))]))
    hector::run(hcores[[i]], globalVars['endDate'])
  }
  # hcore <<- hector::shutdown(hcore)
  # startHector()
  loadGraph()
}
#----- END CORE FUNCTIONS

