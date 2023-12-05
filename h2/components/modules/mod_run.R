# Run Hector using R6 module

run_ui <- function(id) {
  ns <- NS(id)

  tagList(
    chooseSliderSkin(skin = "Flat", color = "#375a7f"),
    textInput(ns("core_name"), "Input name for core:", placeholder="Unnamed Hector core"),
    prettyRadioButtons(ns("ssp_path"), label="Select SSP:",
                choices = list("SSP 1-1.9"="input/hector_ssp119.ini",
                               "SSP 1-2.6"="input/hector_ssp126.ini",
                               "SSP 2-4.5"="input/hector_ssp245.ini",
                               "SSP 3-7.0"="input/hector_ssp370.ini",
                               "SSP 4-3.4"="input/hector_ssp434.ini",
                               "SSP 4-6.0"="input/hector_ssp460.ini",
                               "SSP 5-3.4OS"="input/hector_ssp534-over.ini",
                               "SSP 5-8.5"="input/hector_ssp585.ini"),
                selected = "input/hector_ssp245.ini", inline=TRUE,
                shape = "square", width = "80%"),
    sliderInput(ns("start"), label="Select dates:",
                min = 1750, max = 2300, value = c(1900,2100), sep="", width = "90%", step=5),
    br(),
    h5("Model Parameters"),
    sliderInput(ns("alpha"), label="Aerosol forcing scaling factor", # AERO_SCALE()
                min = 0.01, max = 1, value = 1, width = "90%"),
    sliderInput(ns("beta"), label="CO2 fertilization factor", # BETA()
                min = 0.01, max = 4, value = 0.36, step=0.01, width = "90%"),
    sliderInput(ns("diff"), label="Ocean heat diffusivity", # DIFFUSIVITY()
                min = 0, max = 5, value = 2.3, step=0.1, post = " cm2/s", width = "90%"),
    sliderInput(ns("S"), label="Equilibrium climate sensitivity", # ECS()
                min = 1, max = 6, value = 3, step=0.1, post = " °C", width = "90%"),
    sliderInput(ns("q10_rh"), label="Heterotrophic temperature sensitivity", # Q10_RH()
                min = 1, max = 5, value = 2, step=0.1, width = "90%"),
    sliderInput(ns("volscl"), label="Volcanic forcing scaling factor", # VOLCANIC_SCALE()
                min = 0, max = 1, value = 1, width = "90%"),
    materialSwitch(ns("savetoggle"),"Save Run", value = FALSE),
    actionButton(ns("run"),"Run")
  )
}

run_server <- function(id, r6) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    observe({
    
      toggle <- reactive({input$savetoggle})
      
      if (toggle() == TRUE) {
        # shinyalert(
        #   text = tagList(
        #     textInput("run_name,","Input run name:",placeholder="run name"),
        #     actionButton("OK","OK")
        #   ),
        #   html = TRUE,
        #   #type = "input",
        #   #inputType = "text",
        #   #inputPlaceholder = "run name",
        #   showConfirmButton = TRUE,
        #   confirmButtonText = "OK",
        #   showCancelButton = TRUE,
        #   cancelButtonText = "Cancel",
        #   closeOnClickOutside = FALSE
        # )
        
        observe({
          showModal(modalDialog(
            textInput(ns("run_name"),"Enter run name:",value="run name"),
            easyClose=TRUE,
            footer=tagList(
              actionButton(ns("ok"), "OK"),
              modalButton("Cancel")
            )
          ))
        }) %>%
          bindEvent(input$run, ignoreNULL = TRUE, ignoreInit = TRUE)
        
        observe({
          # Close input modal
          removeModal()
          
          #browser()
          
          r6$i <- reactive({input$run_name})
          
          r6$ini_file <- reactive({system.file(input$ssp_path,package="hector")})
          r6$start <- reactive({input$start[1]})
          r6$end <- reactive({input$start[2]})
          
          print("Running...") # in command line
          
          # Create core
          core <- reactive({newcore(r6$ini_file(),name=input$core_name)})
          
          # Set parameters using inputs (function to only call setvar once in final version)
          setvar(core(),NA,AERO_SCALE(),input$alpha,"(unitless)")
          setvar(core(),NA,BETA(),input$beta,"(unitless)")
          setvar(core(),NA,DIFFUSIVITY(),input$diff,"cm2/s")
          setvar(core(),NA,ECS(),input$S,"degC")
          setvar(core(),NA,Q10_RH(),input$q10_rh,"(unitless)")
          setvar(core(),NA,VOLCANIC_SCALE(),input$volscl,"(unitless)")
          
          # Run core
          reset(core())
          run(core())
          
          r6$output[[r6$i()]] <- fetchvars(core(),r6$start():r6$end(),vars=
                                             list(CONCENTRATIONS_CO2(),FFI_EMISSIONS(),
                                                  LUC_EMISSIONS(),CONCENTRATIONS_N2O(),
                                                  EMISSIONS_BC(),EMISSIONS_OC(),RF_TOTAL(),
                                                  RF_ALBEDO(),RF_N2O(),RF_CO2(),RF_BC(),
                                                  RF_OC(),RF_SO2(),RF_CH4(),RF_VOL(),
                                                  RF_CF4(),RF_C2F6(),RF_HFC23(), 
                                                  RF_HFC4310(),RF_HFC125(),RF_HFC143A(),
                                                  RF_HFC245FA(),RF_SF6(),RF_CFC11(), 
                                                  RF_CFC12(),RF_CFC113(),RF_CFC114(),
                                                  RF_CFC115(),RF_CCL4(),RF_CH3CCL3(),
                                                  RF_HALON1211(),RF_HALON1301(), 
                                                  RF_HALON2402(),RF_CH3CL(),RF_CH3BR(),
                                                  CONCENTRATIONS_CH4(),EMISSIONS_CH4(),
                                                  EMISSIONS_SO2(),VOLCANIC_SO2(),
                                                  GLOBAL_TAS(),SST(),OCEAN_TAS(),
                                                  FLUX_MIXED(),FLUX_INTERIOR(),HEAT_FLUX(),
                                                  ATMOSPHERIC_CO2(),GMST()
                                             )) %>% 
            mutate(run=r6$i())
          
          #output$done <- renderPrint({r6$i()})
          r6$save <- TRUE
          
          print("Done") # in console
          
        }) %>%
          bindEvent(input$ok)
        
      } else if (toggle() == FALSE) {
        
        observe({
          browser()
          r6$i <- reactive({input$run_name})

          r6$ini_file <- reactive({system.file(input$ssp_path,package="hector")})
          r6$start <- reactive({input$start[1]})
          r6$end <- reactive({input$start[2]})

          print("Running...") # in console

          # Create core
          core <- reactive({newcore(r6$ini_file(),name=input$core_name)})

          # Set parameters using inputs (function to only call setvar once in final version)
          setvar(core(),NA,AERO_SCALE(),input$alpha,"(unitless)")
          setvar(core(),NA,BETA(),input$beta,"(unitless)")
          setvar(core(),NA,DIFFUSIVITY(),input$diff,"cm2/s")
          setvar(core(),NA,ECS(),input$S,"degC")
          setvar(core(),NA,Q10_RH(),input$q10_rh,"(unitless)")
          setvar(core(),NA,VOLCANIC_SCALE(),input$volscl,"(unitless)")

          # Run core
          reset(core())
          run(core())

          # Output results
          r6$no_save <- fetchvars(core(),r6$start():r6$end(),vars=
                                    list(CONCENTRATIONS_CO2(),FFI_EMISSIONS(),
                                         LUC_EMISSIONS(),CONCENTRATIONS_N2O(),
                                         EMISSIONS_BC(),EMISSIONS_OC(),RF_TOTAL(),
                                         RF_ALBEDO(),RF_N2O(),RF_CO2(),RF_BC(),
                                         RF_OC(),RF_SO2(),RF_CH4(),RF_VOL(),
                                         RF_CF4(),RF_C2F6(),RF_HFC23(),
                                         RF_HFC4310(),RF_HFC125(),RF_HFC143A(),
                                         RF_HFC245FA(),RF_SF6(),RF_CFC11(),
                                         RF_CFC12(),RF_CFC113(),RF_CFC114(),
                                         RF_CFC115(),RF_CCL4(),RF_CH3CCL3(),
                                         RF_HALON1211(),RF_HALON1301(),
                                         RF_HALON2402(),RF_CH3CL(),RF_CH3BR(),
                                         CONCENTRATIONS_CH4(),EMISSIONS_CH4(),
                                         EMISSIONS_SO2(),VOLCANIC_SO2(),
                                         GLOBAL_TAS(),SST(),OCEAN_TAS(),
                                         FLUX_MIXED(),FLUX_INTERIOR(),HEAT_FLUX(),
                                         ATMOSPHERIC_CO2(),GMST()
                                    ))
          r6$save <- FALSE

          print("Done") # in console
        }) %>%
          bindEvent(input$run, ignoreNULL = TRUE, ignoreInit = FALSE)
        
      }
      
    })
  })
}