

#' Assign Parameters
#'
#' S
#'
#' @return no return value
#' @export
#'
#' @examples
assignParameters <- function()
{
  print('in assign params')
  updateNumericInput(session, "input_aero", value=paramsList[[1]])
  updateNumericInput(session, "input_beta", value=paramsList[[2]])
  updateNumericInput(session, "input_diff", value=paramsList[[3]])
  updateNumericInput(session, "input_ecs",  value=paramsList[[4]])
  updateNumericInput(session, "input_pco2", value=paramsList[[5]])
  updateNumericInput(session, "input_q10",  value=paramsList[[6]])
  updateNumericInput(session, "input_volc", value=paramsList[[7]])
}

#' Restore Parameters after changing model state
#'
#' Function that maintains persistence after the user has changed parameter values to the Hector core (after scenario change)
#'
#' @return no return value
#' @export
#'
#' @examples
restoreParameters <- function()
{
  print('in restore params')
  setParameters()
}

#' Load an alternative model's parameters for emulation
#'
#' Observer function that responds to changes in input from the drop down in the model parameters section and loads a set of model params
#'
#' @return no return value
#' @export
#'
#' @examples
loadModelParameters <- function()
{
  print("in load model params")
  paramsGroup <- vector()
  if(input$input_paramToggle == "default")
    paramsGroup <- globalParamsDefault
  else if(input$input_paramToggle == "canesm2")
    paramsGroup <- globalParamsCanESM2
  else if(input$input_paramToggle == "cesm1-bgc")
    paramsGroup <- globalParamsCESM1BGC
  else if(input$input_paramToggle == "gfdl-esm2g")
    paramsGroup <- globalParamsGFDLESM2G
  else if(input$input_paramToggle == "miroc-esm")
    paramsGroup <- globalParamsMIROCESM
  else if(input$input_paramToggle == "mpi-esm-lr")
    paramsGroup <- globalParamsMPIESMLR
  else if(input$input_paramToggle == "mri-esm1")
    paramsGroup <- globalParamsMRIESM1

  # Update the on screen input components for parameters with the associated values from the chosen parameter group
  # Note - with the current code the parameters need to be in correct order or would have to switch to named calls
  updateNumericInput(session, "input_aero", value=paramsGroup[[1]])
  updateNumericInput(session, "input_beta", value=paramsGroup[[2]])
  updateNumericInput(session, "input_diff", value=paramsGroup[[3]])
  updateNumericInput(session, "input_ecs",  value=paramsGroup[[4]])
  updateNumericInput(session, "input_pco2", value=paramsGroup[[5]])
  updateNumericInput(session, "input_q10",  value=paramsGroup[[6]])
  updateNumericInput(session, "input_volc", value=paramsGroup[[7]])

  if(length(hcores) > 0)
  {
    for(i in 1:length(hcores))
    {
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['aero'], values = c(as.double(paramsGroup[[1]])), unit = "unitless")
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['beta'], values = c(as.double(paramsGroup[[2]])), unit = "unitless")
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['diff'], values = c(as.double(paramsGroup[[3]])), unit = "cm2/s")
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['ecs'],  values = c(as.double(paramsGroup[[4]])), unit = "degC")
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['pco2'], values = c(as.double(paramsGroup[[5]])), unit = "ppmv CO2")
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['q10'],  values = c(as.double(paramsGroup[[6]])), unit = "unitless")
      hector::setvar(hcores[[i]], dates = NA, var = globalParameters['volc'], values = c(as.double(paramsGroup[[7]])), unit = "unitless")
    }

    resetCore()
  }
  if(!firstLoad)
    setParamsChanged(toggle = TRUE)
  if(length(hcores) > 0)
    loadGraph()
}

#' Reset parameter values to current model's default
#'
#' Observer function to handle the user input on the reset parameters button - reset hector parameters to model defaults
#'
#' @return no return value
#' @export
#'
#' @examples
resetParams <- function()
{
  print("in reset params")
  loadModelParameters()
  setParamsChanged(FALSE)
  # if(length(hcores) >= 1)
  # {
  #
  #   updateSelectInput(session = session, inputId = "input_paramToggle", selected = "default")
  #   # restartCore()
  #   # loadParameters()
  # }
}


#' Map Hector parameters to field values
#'
#' Function that gets the input parameters from the hector core and maps them to the input fields. This would normally be called on first load or when parameters are reset.
#'
#' @return no return value
#' @export
#'
#' @examples
loadParameters <- function()
{
  print("in load params")

  # Fetch hector parameters from core
  for(i in 1:length(hcores))
  {
    hdata <- hector::fetchvars(core = hcores[[i]], dates = NA, vars = globalParameters, "\n")
  }

  # Update the on screen input components for parameters with the associated values stored in hector core
  updateNumericInput(session, "input_aero", value=round(hdata[which(hdata$variable == "alpha"), 4], 2))
  updateNumericInput(session, "input_beta", value=round(hdata[which(hdata$variable == "beta"), 4], 2))
  updateNumericInput(session, "input_diff", value=round(hdata[which(hdata$variable == "diff"), 4], 2))
  updateNumericInput(session, "input_ecs",  value=round(hdata[which(hdata$variable == "S"), 4], 2))
  updateNumericInput(session, "input_pco2", value=round(hdata[which(hdata$variable == "C0"), 4], 2))
  updateNumericInput(session, "input_q10",  value=round(hdata[which(hdata$variable == "q10_rh"), 4], 2))
  updateNumericInput(session, "input_volc", value=round(hdata[which(hdata$variable == "volscl"), 4], 2))

  # Store params in the top level variable paramsList for persistence
  paramsList['alpha']   <<- hdata[which(hdata$variable == "alpha"), 4]
  paramsList['beta']    <<- hdata[which(hdata$variable == "beta"), 4]
  paramsList['diff']    <<- hdata[which(hdata$variable == "diff"), 4]
  paramsList['S']       <<- hdata[which(hdata$variable == "S"), 4]
  paramsList['C']       <<- hdata[which(hdata$variable == "C0"), 4]
  paramsList['q10_rh']  <<- hdata[which(hdata$variable == "q10_rh"), 4]
  paramsList['volscl']  <<- hdata[which(hdata$variable == "volscl"), 4]
}


#' Set parameters to active Hector cores after user change
#'
#' Observer function to handle user click on the set parameters button.
#'
#' @return no return value
#' @export
#'
#' @examples
setParameters <- function()
{
  print("in set parameters")
  if(length(hcores) >= 1)
  {
    newVals <- vector()
    # Run through variables and make sure none are left empty and update the top level scope paramsList variable
    # and the hector core with any changed values
    tryCatch(
      {
        for(i in 1:length(hcores))
        {
          if(!is.na(input$input_aero))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['aero'], values = c(as.double(input$input_aero)), unit = "unitless")
            paramsList['alpha'] <<- as.double(input$input_aero)
          }
          if(!is.na(input$input_beta))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['beta'], values = c(as.double(input$input_beta)), unit = "unitless")
            paramsList['beta'] <<- as.double(input$input_aero)
          }
          if(!is.na(input$input_diff))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['diff'], values = c(as.double(input$input_diff)), unit = "cm2/s")
            paramsList['diff'] <<- as.double(input$input_aero)
          }
          if(!is.na(input$input_ecs))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['ecs'],  values = c(as.double(input$input_ecs)), unit = "degC")
            paramsList['S'] <<- as.double(input$input_aero)
          }
          if(!is.na(input$input_pco2))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['pco2'], values = c(as.double(input$input_pco2)), unit = "ppmv CO2")
            paramsList['C'] <<- as.double(input$input_aero)
          }
          if(!is.na(input$input_q10))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['q10'],  values = c(as.double(input$input_q10)), unit = "unitless")
            paramsList['q10_rh'] <<- as.double(input$input_aero)
          }
          if(!is.na(input$input_volc))
          {
            hector::setvar(hcores[[i]], dates = NA, var = globalParameters['volc'], values = c(as.double(input$input_volc)), unit = "unitless")
            paramsList['volscl'] <<- as.double(input$input_aero)
          }
        }
        resetCore()
        if(length(hcores) > 0)
          loadGraph()
      },
      warning = function(war)
      {
        # warning handler picks up where error was generated
        showModal(modalDialog(
          title = "Important message",
          paste("Details:  ",war)
        ))
      },
      error = function(err)
      {
        # error handler picks up where error was generated
        print(paste("\n ERROR: Set Params - ", as.character(err[1]), sep=" "))
        shinyalert::shinyalert("Oops!",print(paste('Error:',err)), type = "error")

      })
  }
}


#' Notifies system that parameters have changed state
#'
#' This function is used to set the corresponding flag so that they system knows the parameters have been changed
#'
#' @param toggle
#'
#' @return no return value
#' @export
#'
#' @examples
setParamsChanged <- function(toggle)
{
  print("in set Params Changed")
  # Check input validation here

  if(toggle == TRUE & !firstLoad)
  {
    paramsChanged <<- TRUE
    # shinyjs::enable("set_Params")
    # shinyjs::toggleClass(id = "set_Params", class = "changedParamsTrue")
    # shinyjs::runjs(paste0('$("#set_Params").css({"border": "1px #AC7023 solid"})'))
  }
  else
  {

    paramsChanged <<- FALSE
    # shinyjs::toggleClass(id = "set_Params", class = "changedParamsFalse")
  }

}
