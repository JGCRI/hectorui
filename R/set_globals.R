#' Global vars for misc items such as the run date end year (2100)
#' @export
get_globalVars <- function() {

    globalVars <- list()
    globalVars['startDate'] <- 1900
    globalVars['endDate'] <- 2100
    globalVars['writeDirectory'] <- "temp"

    return(globalVars)
}


#' Global vars for scale colors
#' @export
get_globalColorScales <- function() {

    globalColorScales <- vector()
    globalColorScales <- c("RCP 2.6" = "#5DBFDE", "RCP 4.5" = "#5CB95C", "RCP 6.0" = "#FBAB33", "RCP 8.5" ="#D7534E")


    return(globalColorScales)
}


#' RCP list by number
#' @export
get_rcps <- function() {

    return(c(26,45,60,85))
}


#' Global file paths vector
#' @export
get_globalScenarios <- function() {

    rcps <- get_rcps()

    globalScenarios <- list()
    globalScenarios[['RCP-2.6']] <-  file.path('input',paste0('hector_rcp',rcps[1],'.ini'))
    globalScenarios[['RCP-4.5']] <-  file.path('input',paste0('hector_rcp',rcps[2],'.ini'))
    globalScenarios[['RCP-6.0']] <-  file.path('input',paste0('hector_rcp',rcps[3],'.ini'))
    globalScenarios[['RCP-8.5']] <-  file.path('input',paste0('hector_rcp',rcps[4],'.ini'))

    return(globalScenarios)
}


#' Global scenario color schemes
#' @export
get_globalScenarioColors <- function() {

    return(c("RCP 2.6" = "#5DBFDE", "RCP 4.5" = "#5CB95C", "RCP 6.0" = "#FBAB33", "RCP 8.5" = "#D7534E"))
}


#' Global temperature patterns
#' @export
get_globalTempPatterns <- function() {

    return(c("CanESM2" = "www/maps/tas_Amon_CanESM2_esmrcp85_r1i1p1_200601-210012_pattern.rds",
             "CESM1-BGC" = "www/maps/tas_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
             "GFDL-ESM2G" = "www/maps/tas_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
             "MIROC-ESM" = "www/maps/tas_Amon_MIROC-ESM_esmrcp85_r1i1p1_200601-210012_pattern.rds",
             "MPI-ESM-LR" = "www/maps/tas_Amon_MPI-ESM-LR_esmrcp85_r1i1p1_200601-210012_pattern.rds",
             "MRI-ESM1" = "www/maps/tas_Amon_MRI-ESM1_esmrcp85_r1i1p1_200601-210012_pattern.rds"))
}


#' Global precipitation patterns list
#' @export
get_globalPrecipPatterns <- function() {

    return(c("CanESM2" = "www/maps/pr_Amon_CanESM2_rcp85_r1i1p1_200601-210012_pattern.rds",
             "CESM1-BGC" = "www/maps/pr_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
             "GFDL-ESM2G" = "www/maps/pr_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
             "MIROC-ESM" = "www/maps/pr_Amon_MIROC-ESM_rcp85_r1i1p1_200601-210012_pattern.rds",
             "MPI-ESM-LR" = "www/maps/pr_Amon_MPI-ESM-LR_rcp85_r1i1p1_200601-210012_pattern.rds",
             "MRI-ESM1" = "www/maps/pr_Amon_MRI-ESM1_rcp85_r1i1p1_200601-210012_pattern.rds"))
}


#' Create master list of parameter lookup strings
#' @export
get_globalParameters <- function() {

    globalParameters <- vector()
    globalParameters['pco2'] <- hector::PREINDUSTRIAL_CO2()
    globalParameters['q10'] <- hector::Q10_RH() #2.0
    globalParameters['beta'] <- hector::BETA() #0.36
    globalParameters['ecs'] <- hector::ECS() #3.0
    globalParameters['aero'] <- hector::AERO_SCALE() #1.0
    globalParameters['volc'] <- hector::VOLCANIC_SCALE() #1.0
    globalParameters['diff'] <- hector::DIFFUSIVITY() #2.3

    return(globalParameters)
}



#' Default Hector parameters
#' @export
get_globalParamsDefault <- function() {

    return(c('alpha' = 1, 'beta' = 0.36, 'diff' = 2.3, 'S' = 3, 'C' = 276.09, 'q10_rh' = 2, 'volscl' = 1))

}

#' CanESM2 Parameter Sets
#' @export
get_globalParamsCanESM2 <- function() {

    return(c('alpha' = 1.87, 'beta' = 0.08, 'diff' = 0.98, 'S' = 3.88, 'C' = 282.35, 'q10_rh' = 1.75, 'volscl' = 1.81))

}


#' CESM1-BGC Parameter Set
#' @export
get_globalParamsCESM1BGC <- function() {

    return(c('alpha' = -0.43, 'beta' = 0.0, 'diff' = 8, 'S' = 2.4, 'C' = 280.31, 'q10_rh' = 1.78, 'volscl' = 3.94))

}



#' GFDL-ESM2G Parameter Set
#' @export
get_globalParamsGFDLESM2G <- function() {

    return(c('alpha' = 0.46, 'beta' = 0.07, 'diff' = 12.01, 'S' = 2.03, 'C' = 289.24, 'q10_rh' = 1.76, 'volscl' = 2.12))

}



#' MIROC-ESM Parameter Set
#' @export
get_globalParamsMIROCESM <- function() {

    return(c('alpha' = 1.05, 'beta' = 0.02, 'diff' = 6.39, 'S' = 5.83, 'C' = 283.31, 'q10_rh' = 1.77, 'volscl' = 2.02))

}



#' MPI-ESM-LR Parameter Set
#' @export
get_globalParamsMPIESMLR <- function() {

    return(c('alpha' = 1.22, 'beta' = 0.28, 'diff' = 2.93, 'S' = 3.66, 'C' = 289.13, 'q10_rh' = 1.75, 'volscl' = 0.70))

}



#' MRI-ESM1 Parameter Set
#' @export
get_globalParamsMRIESM1 <- function() {

    return(c('alpha' = 1.42, 'beta' = 0.66, 'diff' = 4.21, 'S' = 2.04, 'C' = 289.49, 'q10_rh' = 1.76, 'volscl' = 0.27))

}


#' Create master list of variable lookups for "capabilities" (output variables for graphing)
#' @export
get_globalCapabilities <- function() {

    globalCapabilities <- list()

    # CARBON CYCLE
    # "Land Carbon Flux"
    globalCapabilities[['cc_lcf']] <- hector::LAND_CFLUX()
    attr(globalCapabilities[['cc_lcf']], 'longName') <- "Land Carbon Flux"

    # "Atmospheric CO2"
    globalCapabilities[['cc_co2']] <- hector::ATMOSPHERIC_CO2()
    attr(globalCapabilities[['cc_co2']], 'longName') <- "Atmospheric CO2"

    # "Atmospheric Carbon Pool"
    globalCapabilities[['cc_acp']] <- hector::ATMOSPHERIC_C()
    attr(globalCapabilities[['cc_acp']], 'longName') <- "Atmospheric Carbon Pool"

    # "Fossil Fuel and Industrial Emissions"
    globalCapabilities[['cc_ffi']] <- hector::FFI_EMISSIONS()
    attr(globalCapabilities[['cc_ffi']], 'longName') <- "FFI Emissions"
    attr(globalCapabilities[['cc_ffi']], 'unit') <- "GtC/yr"

    # "Land Use Change Emissions"
    globalCapabilities[['cc_luc']] <- hector::LUC_EMISSIONS()
    attr(globalCapabilities[['cc_luc']], 'longName') <- "LUC Emissions"

    # CONCENTRATIONS
    # "Amospheric N2O"
    globalCapabilities[['c_an20']] <- hector::ATMOSPHERIC_N2O()
    attr(globalCapabilities[['c_an20']], 'longName') <- "Amospheric N2O"

    # "Preindustrial Atmospheric CO2"
    globalCapabilities[['c_paco2']] <- hector::PREINDUSTRIAL_CO2()
    attr(globalCapabilities[['c_paco2']], 'longName') <- "Preindustrial Atmospheric CO2"

    # "Preindustrial Ozone Concentration"
    globalCapabilities[['c_po']] <- hector::PREINDUSTRIAL_O3()
    attr(globalCapabilities[['c_po']], 'longName') <- "Preindustrial Ozone Concentration"


    # EMISSIONS
    # "Black Carbon Emissions"
    globalCapabilities[['e_bc']] <- hector::EMISSIONS_BC()
    attr(globalCapabilities[['e_bc']], 'longName') <- "Black Carbon Emissions"
    attr(globalCapabilities[['e_bc']], 'unit') <- "Tg"

    # "N20 Emissions"
    globalCapabilities[['e_n2o']] <- hector::EMISSIONS_N2O()
    attr(globalCapabilities[['e_n2o']], 'longName') <- "N20 Emissions"

    # "NOx Emissions"
    globalCapabilities[['e_nox']] <- hector::EMISSIONS_NOX()
    attr(globalCapabilities[['e_nox']], 'longName') <- "NOx Emissions"

    # "CO Emissions"
    globalCapabilities[['e_co']] <- hector::EMISSIONS_CO()
    attr(globalCapabilities[['e_co']], 'longName') <- "CO Emissions"

    # "NMVOC Emissions"
    globalCapabilities[['e_nmvoc']] <- hector::EMISSIONS_NMVOC()
    attr(globalCapabilities[['e_nmvoc']], 'longName') <- "NMVOC Emissions"

    # "Organic Carbon Emissions"
    globalCapabilities[['e_oc']] <- hector::EMISSIONS_OC()
    attr(globalCapabilities[['e_oc']], 'longName') <- "Organic Carbon Emissions"
    attr(globalCapabilities[['e_oc']], 'unit') <- "Tg"

    # FORCINGS
    # "RF - Total"
    globalCapabilities[['f_rft']] <- hector::RF_TOTAL()
    attr(globalCapabilities[['f_rft']], 'longName') <- "RF - Total"

    # "RF - Albedo"
    globalCapabilities[['f_alb']] <- hector::RF_T_ALBEDO()
    attr(globalCapabilities[['f_alb']], 'longName') <- "RF - Albedo"

    # "RF - CO2"
    globalCapabilities[['f_co2']] <- hector::RF_CO2()
    attr(globalCapabilities[['f_co2']], 'longName') <- "RF - CO2"

    # "RF - N2O"
    globalCapabilities[['f_n2o']] <- hector::RF_N2O()
    attr(globalCapabilities[['f_n2o']], 'longName') <- "RF - N2O"

    # "RF - H2O"
    # globalCapabilities[['f_h2o']] <- hector::RF_H2O()
    # attr(globalCapabilities[['f_h2o']], 'longName') <- "RF - H2O"

    # "RF - Ozone"
    # globalCapabilities[['f_oz']] <- hector::RF_O3()
    # attr(globalCapabilities[['f_oz']], 'longName') <- "RF - Ozone"

    # "RF - Black Carbon"
    globalCapabilities[['f_bc']] <- hector::RF_BC()
    attr(globalCapabilities[['f_bc']], 'longName') <- "RF - Black Carbon"

    # "RF - Organic Carbon"
    globalCapabilities[['f_oc']] <- hector::RF_OC()
    attr(globalCapabilities[['f_oc']], 'longName') <- "RF - Organic Carbon"

    # "RF - SO2 Direct"
    globalCapabilities[['f_so2d']] <- hector::RF_SO2D()
    attr(globalCapabilities[['f_so2d']], 'longName') <- "RF - SO2 Direct"

    # "RF - SO2 Indirect"
    globalCapabilities[['f_so2i']] <- hector::RF_SO2I()
    attr(globalCapabilities[['f_so2i']], 'longName') <- "RF - SO2 Indirect"

    # "RF - SO2 Total"
    globalCapabilities[['f_so2t']] <- hector::RF_SO2()
    attr(globalCapabilities[['f_so2t']], 'longName') <- "RF - SO2 Total"

    # "RF - Volcanic Activity"
    globalCapabilities[['f_va']] <- hector::RF_VOL()
    attr(globalCapabilities[['f_va']], 'longName') <- "RF - Volcanic Activity"

    # "RF - CH4"
    globalCapabilities[['f_ch4']] <- hector::RF_CH4()
    attr(globalCapabilities[['f_ch4']], 'longName') <- "RF - CH4"

    # HALOCARBON EMISSIONS
    # "CF4 Emissions"
    globalCapabilities[['he_cf4']] <- hector::EMISSIONS_CF4()
    attr(globalCapabilities[['he_cf4']], 'longName') <- "CF4 Emissions"

    # "C2F6 Emissions"
    globalCapabilities[['he_c2f6']] <- hector::EMISSIONS_C2F6()
    attr(globalCapabilities[['he_c2f6']], 'longName') <- "C2F6 Emissions"

    # "HFC-23 Emissions"
    globalCapabilities[['he_hfc23']] <- hector::EMISSIONS_HFC23()
    attr(globalCapabilities[['he_hfc23']], 'longName') <- "HFC-23 Emissions"

    # "HFC-32 Emissions"
    globalCapabilities[['he_hfc32']] <- hector::EMISSIONS_HFC32()
    attr(globalCapabilities[['he_hfc32']], 'longName') <- "HFC-32 Emissions"

    # "HFC-4310 Emissions"
    globalCapabilities[['he_hfc4310']] <- hector::EMISSIONS_HFC4310()
    attr(globalCapabilities[['he_hfc4310']], 'longName') <- "HFC-4310 Emissions"

    # "HFC-125 Emissions"
    globalCapabilities[['he_hfc125']] <- hector::EMISSIONS_HFC125()
    attr(globalCapabilities[['he_hfc125']], 'longName') <- "HFC-125 Emissions"

    # "HFC-134a Emissions"
    globalCapabilities[['he_hfc134a']] <- hector::EMISSIONS_HFC134A()
    attr(globalCapabilities[['he_hfc134a']], 'longName') <- "HFC-134a Emissions"

    # "HFC-143a Emissions"
    globalCapabilities[['he_hfc143a']] <- hector::EMISSIONS_HFC143A()
    attr(globalCapabilities[['he_hfc143a']], 'longName') <- "HFC-143a Emissions"

    # "HFC-227ea Emissions"
    globalCapabilities[['he_hfc227ea']] <- hector::EMISSIONS_HFC227EA()
    attr(globalCapabilities[['he_hfc227ea']], 'longName') <- "HFC-227ea Emissions"

    # "HFC-254fa Emissions"
    globalCapabilities[['he_245fa']] <- hector::EMISSIONS_HFC245FA()
    attr(globalCapabilities[['he_245fa']], 'longName') <-  "HFC-254fa Emissions"

    # "SF6 Emissions"'
    globalCapabilities[['he_sf6']] <- hector::EMISSIONS_SF6()
    attr(globalCapabilities[['he_sf6']], 'longName') <- "SF6 Emissions"

    # "CFC-11 Emissions"
    globalCapabilities[['he_cfc11']] <- hector::EMISSIONS_CFC11()
    attr(globalCapabilities[['he_cfc11']], 'longName') <- "CFC-11 Emissions"

    # "CFC-12 Emissions"
    globalCapabilities[['he_cfc12']] <- hector::EMISSIONS_CFC12()
    attr(globalCapabilities[['he_cfc12']], 'longName') <-"CFC-12 Emissions"

    # "CFC-113 Emissions"
    globalCapabilities[['he_cfc113']] <- hector::EMISSIONS_CFC113()
    attr(globalCapabilities[['he_cfc113']], 'longName') <-"CFC-113 Emissions"

    # "CFC-114 Emissions"
    globalCapabilities[['he_cfc114']] <- hector::EMISSIONS_CFC114()
    attr(globalCapabilities[['he_cfc114']], 'longName') <-"CFC-114 Emissions"

    # "CFC-115 Emissions"
    globalCapabilities[['he_cfc115']] <- hector::EMISSIONS_CFC115()
    attr(globalCapabilities[['he_cfc115']], 'longName') <-"CFC-115 Emissions"

    # "CCl4 Emissions"
    globalCapabilities[['he_ccl4']] <- hector::EMISSIONS_CCL4()
    attr(globalCapabilities[['he_ccl4']], 'longName') <-"CCl4 Emissions"

    # "CH3CCl3 Emissions"
    globalCapabilities[['he_ch3ccl3']] <- hector::EMISSIONS_CH3CCL3()
    attr(globalCapabilities[['he_ch3ccl3']], 'longName') <- "CH3CCl3 Emissions"

    # "HCFC-22 Emissions"
    # globalCapabilities[['he_hcfc22']] <- hector::EMISSIONS_HCF22()
    # attr(globalCapabilities[['he_hcfc22']], 'longName') <- "HCFC-22 Emissions"

    # "HCFC-141b Emissions"
    # globalCapabilities[['he_hcfc141b']] <- hector::EMISSIONS_HCF141B()
    # attr(globalCapabilities[['he_hcfc141b']], 'longName') <- "HCFC-141b Emissions"

    # "HCFC-142b Emissions"
    # globalCapabilities[['he_hcfc142b']] <- hector::EMISSIONS_HCF142B()
    # attr(globalCapabilities[['he_hcfc142b']], 'longName') <- "HCFC-142b Emissions"

    # "Halon-1211 Emissions"
    globalCapabilities[['he_halon1211']] <- hector::EMISSIONS_HALON1211()
    attr(globalCapabilities[['he_halon1211']], 'longName') <- "Halon-1211 Emissions"

    # "Halon-1301 Emissions"
    globalCapabilities[['he_halon1301']] <- hector::EMISSIONS_HALON1301()
    attr(globalCapabilities[['he_halon1301']], 'longName') <- "Halon-1301 Emissions"

    # "Halon-2402 Emissions"
    globalCapabilities[['he_halon2402']] <- hector::EMISSIONS_HALON2402()
    attr(globalCapabilities[['he_halon2402']], 'longName') <- "Halon-2402 Emissions"

    # "CH3Cl Emissions"
    globalCapabilities[['he_ch3cl']] <- hector::EMISSIONS_CH3CL()
    attr(globalCapabilities[['he_ch3cl']], 'longName') <- "CH3Cl Emissions"

    # "CH3Br Emissions"
    globalCapabilities[['he_ch3br']] <- hector::EMISSIONS_CH3BR()
    attr(globalCapabilities[['he_ch3br']], 'longName') <- "CH3Br Emissions"

    # HALOCARBON FORCINGS
    # "CF4 Forcing"
    globalCapabilities[['hf_cf4']] <- hector::RF_CF4()
    attr(globalCapabilities[['hf_cf4']], 'longName') <- "CF4 Forcing"

    # "C2F6 Forcing"
    globalCapabilities[['hf_c2f6']] <- hector::RF_C2F6()
    attr(globalCapabilities[['hf_c2f6']], 'longName') <-  "C2F6 Forcing"

    # "HFC-23 Forcing"
    globalCapabilities[['hf_hfc23']] <- hector::RF_HFC23()
    attr(globalCapabilities[['hf_hfc23']], 'longName') <- "HFC-23 Forcing"

    # "HFC-32 Forcing"
    globalCapabilities[['hf_hfc32']] <- hector::RF_HFC32()
    attr(globalCapabilities[['hf_hfc32']], 'longName') <- "HFC-32 Forcing"

    # "HFC-4310 Forcing"
    globalCapabilities[['hf_hfc4310']] <- hector::RF_HFC4310()
    attr(globalCapabilities[['hf_hfc4310']], 'longName') <- "HFC-4310 Forcing"

    # "HFC-125 Forcing"
    globalCapabilities[['hf_hfc125']] <- hector::RF_HFC125()
    attr(globalCapabilities[['hf_hfc125']], 'longName') <-  "HFC-125 Forcing"

    # "HFC-134a Forcing"
    globalCapabilities[['hf_hfc134a']] <- hector::RF_HFC134A()
    attr(globalCapabilities[['hf_hfc134a']], 'longName') <- "HFC-134a Forcing"

    # "HFC-143a Forcing"
    globalCapabilities[['hf_hfc143a']] <- hector::RF_HFC143A()
    attr(globalCapabilities[['hf_hfc143a']], 'longName') <- "HFC-143a Forcing"

    # "HFC-227ea Forcing"
    globalCapabilities[['hf_hfc227ea']] <- hector::RF_HFC227EA()
    attr(globalCapabilities[['hf_hfc227ea']], 'longName') <- "HFC-227ea Forcing"

    # "HFC-245fa Forcing"
    globalCapabilities[['hf_hfc245fa']] <- hector::RF_HFC245FA()
    attr(globalCapabilities[['hf_hfc245fa']], 'longName') <- "HFC-245fa Forcing"

    # "SF6 Forcing"
    globalCapabilities[['hf_hfcsf6']] <- hector::RF_SF6()
    attr(globalCapabilities[['hf_hfcsf6']], 'longName') <- "SF6 Forcing"

    # "CFC-11 Forcing"
    globalCapabilities[['hf_cfc11']] <- hector::RF_CFC11()
    attr(globalCapabilities[['hf_cfc11']], 'longName') <- "CFC-11 Forcing"

    # "CFC-12 Forcing"
    globalCapabilities[['hf_cfc12']] <- hector::RF_CFC12()
    attr(globalCapabilities[['hf_cfc12']], 'longName') <- "CFC-12 Forcing"

    # "CFC-113 Forcing"
    globalCapabilities[['hf_cfc113']] <- hector::RF_CFC113()
    attr(globalCapabilities[['hf_cfc113']], 'longName') <- "CFC-113 Forcing"

    # "CFC-114 Forcing"
    globalCapabilities[['hf_cfc114']] <- hector::RF_CFC114()
    attr(globalCapabilities[['hf_cfc114']], 'longName') <- "CFC-114 Forcing"

    # "CFC-115 Forcing"
    globalCapabilities[['hf_cfc115']] <- hector::RF_CFC115()
    attr(globalCapabilities[['hf_cfc115']], 'longName') <- "CFC-115 Forcing"

    # "CCl4 Forcing"
    globalCapabilities[['hf_ccl4']] <- hector::RF_CCL4()
    attr(globalCapabilities[['hf_ccl4']], 'longName') <- "CCl4 Forcing"

    # "CH3CCl3 Forcing"
    globalCapabilities[['hf_ch3ccl3']] <- hector::RF_CH3CCL3()
    attr(globalCapabilities[['hf_ch3ccl3']], 'longName') <- "CH3CCl3 Forcing"

    # "HCFC-22 Forcing"
    # globalCapabilities[['hf_hcfc22']] <- hector::RF_HCF22()
    # attr(globalCapabilities[['hf_hcfc22']], 'longName') <- "HCFC-22 Forcing"

    # "HCFC-141b Forcing"
    # globalCapabilities[['hf_hcfc141b']] <- hector::RF_HCFC141B()
    # attr(globalCapabilities[['hf_hcfc141b']], 'longName') <- "HCFC-141b Forcing"

    # "HCFC-142b Forcing"
    # globalCapabilities[['hf_hcfc142b']] <- hector::RF_HCFC142B()
    # attr(globalCapabilities[['hf_hcfc142b']], 'longName') <- "HCFC-142b Forcing"

    # "Halon-1211 Forcing"
    globalCapabilities[['hf_halon1211']] <- hector::RF_HALON1211()
    attr(globalCapabilities[['hf_halon1211']], 'longName') <- "Halon-1211 Forcing"

    # "Halon-1301 Forcing"
    globalCapabilities[['hf_halon1301']] <- hector::RF_HALON1301()
    attr(globalCapabilities[['hf_halon1301']], 'longName') <- "Halon-1301 Forcing"

    # "Halon-2402 Forcing"
    globalCapabilities[['hf_halon2402']] <- hector::RF_HALON2402()
    attr(globalCapabilities[['hf_halon2402']], 'longName') <- "Halon-2402 Forcing"

    # "CH3Cl Forcing"
    globalCapabilities[['hf_ch3cl']] <- hector::RF_CH3CL()
    attr(globalCapabilities[['hf_ch3cl']], 'longName') <- "CH3Cl Forcing"

    # "CH3Br Forcing"
    globalCapabilities[['hf_ch3br']] <- hector::RF_CH3BR()
    attr(globalCapabilities[['hf_ch3br']], 'longName') <- "CH3Br Forcing"

    # METHANE
    # "Atmospheric CH4"
    globalCapabilities[['m_a_ch4']] <- hector::ATMOSPHERIC_CH4()
    attr(globalCapabilities[['m_a_ch4']], 'longName') <- "Atmospheric CH4"

    # "Preindustrial Atmospheric CH4"
    globalCapabilities[['m_pa_ch4']] <- hector::PREINDUSTRIAL_CH4()
    attr(globalCapabilities[['m_pa_ch4']], 'longName') <- "Preindustrial Atmospheric CH4"

    # "Emissions CH4"
    globalCapabilities[['m_e_ch4']] <- hector::EMISSIONS_CH4()
    attr(globalCapabilities[['m_e_ch4']], 'longName') <- "Emissions CH4"

    # "Natural CH4 Emissions"
    globalCapabilities[['m_n_ch4']] <- hector::NATURAL_CH4()
    attr(globalCapabilities[['m_n_ch4']], 'longName') <- "Natural CH4 Emissions"

    # "Methane Loss - Soil"
    globalCapabilities[['m_soil_loss']] <- hector::LIFETIME_SOIL()
    attr(globalCapabilities[['m_soil_loss']], 'longName') <- "Methane Loss - Soil"

    # "Methane Loss - Straosphere"
    globalCapabilities[['m_strat_loss']] <- hector::LIFETIME_STRAT()
    attr(globalCapabilities[['m_strat_loss']], 'longName') <- "Methane Loss - Straosphere"

    # OCEAN
    # "Ocean Carbon Flux"
    globalCapabilities[['o_cf']] <- hector::OCEAN_CFLUX()
    attr(globalCapabilities[['o_cf']], 'longName') <- "Ocean Carbon Flux"

    # "Ocean Total Carbon"
    globalCapabilities[['o_tc']] <- hector::OCEAN_C()
    attr(globalCapabilities[['o_tc']], 'longName') <- "Ocean Total Carbon"

    # "Ocean Surface High-Lat Carbon"
    globalCapabilities[['o_os_hlc']] <- hector::OCEAN_C_HL()
    attr(globalCapabilities[['o_os_hlc']], 'longName') <- "Ocean Surface High-Lat Carbon"

    # "Ocean Surface Low-Lat Carbon"
    globalCapabilities[['o_os_llc']] <- hector::OCEAN_C_LL()
    attr(globalCapabilities[['o_os_llc']], 'longName') <- "Ocean Surface Low-Lat Carbon"

    # "Ocean Intermediate Carbon"
    globalCapabilities[['o_ic']] <- hector::OCEAN_C_IO()
    attr(globalCapabilities[['o_ic']], 'longName') <- "Ocean Intermediate Carbon"

    # "Ocean Deep Carbon"
    globalCapabilities[['o_dc']] <- hector::OCEAN_C_DO()
    attr(globalCapabilities[['o_dc']], 'longName') <- "Ocean Deep Carbon"

    # "Thermohaline Overturning"
    globalCapabilities[['o_to']] <- hector::TT()
    attr(globalCapabilities[['o_to']], 'longName') <- "Thermohaline Overturning"

    # "High-Lat Overturning"
    globalCapabilities[['o_hl_o']] <- hector::TU()
    attr(globalCapabilities[['o_hl_o']], 'longName') <- "High-Lat Overturning"

    # "Warm-Intermediate Exchange"
    globalCapabilities[['o_wie']] <- hector::TWI()
    attr(globalCapabilities[['o_wie']], 'longName') <- "Warm-Intermediate Exchange"

    # "Intermediate-Deep Exchange"
    globalCapabilities[['o_ide']] <- hector::TID()
    attr(globalCapabilities[['o_ide']], 'longName') <- "Intermediate-Deep Exchange"

    # "High Latitude Ph"
    globalCapabilities[['o_hl_ph']] <- hector::PH_HL()
    attr(globalCapabilities[['o_hl_ph']], 'longName') <- "High Latitude Ph"

    # "Low Latitude Ph"
    globalCapabilities[['o_ll_ph']] <- hector::PH_LL()
    attr(globalCapabilities[['o_ll_ph']], 'longName') <- "Low Latitude Ph"

    # "Atmosphere-Ocean Flux - High Lat"
    globalCapabilities[['o_hl_aof']] <- hector::ATM_OCEAN_FLUX_HL()
    attr(globalCapabilities[['o_hl_aof']], 'longName') <- "Atmosphere-Ocean Flux - High Lat"

    # "Atmosphere-Ocean Flux - Low Lat"
    globalCapabilities[['o_ll_aof']] <- hector::ATM_OCEAN_FLUX_LL()
    attr(globalCapabilities[['o_ll_aof']], 'longName') <- "Atmosphere-Ocean Flux - Low Lat"

    # "Partial Pressure CO2 - High Lat"
    globalCapabilities[['o_hl_pp_co2']] <- hector::PCO2_HL()
    attr(globalCapabilities[['o_hl_pp_co2']], 'longName') <- "Partial Pressure CO2 - High Lat"

    # "Partial Pressure CO2 - Low Lat"
    globalCapabilities[['o_ll_pp_co2']] <- hector::PCO2_LL()
    attr(globalCapabilities[['o_ll_pp_co2']], 'longName') <- "Partial Pressure CO2 - Low Lat"

    # "Dissolved Inorganic C - High Lat"
    globalCapabilities[['o_hl_dic']] <- hector::DIC_HL()
    attr(globalCapabilities[['o_hl_dic']], 'longName') <- "Dissolved Inorganic C - High Lat"

    # "Dissolved Inorganic C - Low Lat"'
    globalCapabilities[['o_ll_dic']] <- hector::DIC_LL()
    attr(globalCapabilities[['o_ll_dic']], 'longName') <- "Dissolved Inorganic C - Low Lat"

    # "Ocean Temperature - High Lat"
    globalCapabilities[['o_hl_t']] <- hector::TEMP_HL()
    attr(globalCapabilities[['o_hl_t']], 'longName') <- "Ocean Temperature - High Lat"

    # "Ocean Temperature - Low Lat"
    globalCapabilities[['o_ll_t']] <- hector::TEMP_LL()
    attr(globalCapabilities[['o_ll_t']], 'longName') <- "Ocean Temperature - Low Lat"

    # "Carbonate Concentration - High Lat"
    globalCapabilities[['o_hl_cc']] <- hector::CO3_HL()
    attr(globalCapabilities[['o_hl_cc']], 'longName') <- "Carbonate Concentration - High Lat"

    # "Carbonate Concentration - Low Lat"
    globalCapabilities[['o_ll_cc']] <- hector::CO3_LL()
    attr(globalCapabilities[['o_ll_cc']], 'longName') <- "Carbonate Concentration - Low Lat"

    # SO2
    # "Natural SO2"
    globalCapabilities[['so2_n']] <- hector::NATURAL_SO2()
    attr(globalCapabilities[['so2_n']], 'longName') <- "Natural SO2"

    # "Year 2000 SO2"
    globalCapabilities[['so2_y2k']] <- hector::Y2000_SO2()
    attr(globalCapabilities[['so2_y2k']], 'longName') <- "Year 2000 SO2"

    # "Anthropogenic SO2"
    globalCapabilities[['so2_a']] <- hector::EMISSIONS_SO2()
    attr(globalCapabilities[['so2_a']], 'longName') <- "Anthropogenic SO2"

    # "Volcanic SO2"
    globalCapabilities[['so2_v']] <- hector::VOLCANIC_SO2()
    attr(globalCapabilities[['so2_v']], 'longName') <- "Volcanic SO2"

    # TEMPERATURE
    # "Global Mean Temp"
    globalCapabilities[['t_gmt']] <- hector::GLOBAL_TEMP()
    attr(globalCapabilities[['t_gmt']], 'longName') <- "Global Mean Temperature"

    # "Equilibrium Global Temp"
    globalCapabilities[['t_egt']] <- hector::GLOBAL_TEMPEQ()
    attr(globalCapabilities[['t_egt']], 'longName') <- "Equilibrium Global Temperature"

    # "Ocean Surface Temp"
    globalCapabilities[['t_ost']] <- hector::OCEAN_SURFACE_TEMP()
    attr(globalCapabilities[['t_ost']], 'longName') <- "Ocean Surface Temperature"

    # "Ocean Air Temp"
    globalCapabilities[['t_oat']] <- hector::OCEAN_AIR_TEMP()
    attr(globalCapabilities[['t_oat']], 'longName') <- "Ocean Air Temperature"

    # "Land Temp Anomaly"
    # globalCapabilities[['t_lta']] <- hector::LAND_TEMP()
    # attr(globalCapabilities[['t_lta']], 'longName') <- "Land Temperature Anomaly"

    # "Heat Flux - Mixed Layer Ocean"
    globalCapabilities[['t_hf_mlo']] <- hector::FLUX_MIXED()
    attr(globalCapabilities[['t_hf_mlo']], 'longName') <- "Heat Flux - Mixed Layer Ocean"

    # "Heat Flux - Interior Layer Ocean"
    globalCapabilities[['t_hf_ilo']] <- hector::FLUX_INTERIOR()
    attr(globalCapabilities[['t_hf_ilo']], 'longName') <- "Heat Flux - Interior Layer Ocean"

    # "Total Heat Flux - Ocean"
    globalCapabilities[['t_hf_t']] <- hector::HEAT_FLUX()
    attr(globalCapabilities[['t_hf_t']], 'longName') <- "Total Heat Flux - Ocean"


    return(globalCapabilities)
}
