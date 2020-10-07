library(hector)


context("globals")


test_that("globals:  miscellaneous", {

  globalVars <- get_globalVars()

  expect_equal(globalVars[['startDate']][1], 1900)
  expect_equal(globalVars[['endDate']], 2100)
  expect_equal(globalVars[['writeDirectory']], "temp")

})


test_that("globals:  color scale", {

  globalColorScales <- get_globalColorScales()

  expect_equal(globalColorScales, c("RCP 2.6"="#db735c", "RCP 4.5"="#EFA86E", "RCP 6.0"="#9A8A76", "RCP 8.5"="#7A6752"))

})


test_that("globals:  rcps", {

  rcps <- get_rcps()

  expect_equal(rcps, c(26,45,60,85))

})


test_that("globals:  file paths", {


  globalScenarios <- get_globalScenarios()

  expect_equal(globalScenarios[['RCP-2.6']], file.path('input',paste0('hector_rcp',26,'.ini')))
  expect_equal(globalScenarios[['RCP-4.5']], file.path('input',paste0('hector_rcp',45,'.ini')))
  expect_equal(globalScenarios[['RCP-6.0']], file.path('input',paste0('hector_rcp',60,'.ini')))
  expect_equal(globalScenarios[['RCP-8.5']], file.path('input',paste0('hector_rcp',85,'.ini')))

})


test_that("globals:  scenario color scheme", {

  globalScenarioColors <- get_globalScenarioColors()

  expect_equal(globalScenarioColors, c("RCP 2.6" = "#99cc33", "RCP 4.5" = "#FFFF00", "RCP 6.0" = "#ff9900", "RCP 8.5" = "#ff3333"))

})


test_that("globals:  temperature patterns", {

  globalTempPatterns <- get_globalTempPatterns()

  expect_equal(globalTempPatterns, c("CanESM2" = "www/maps/tas_Amon_CanESM2_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                     "CESM1-BGC" = "www/maps/tas_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
                                     "GFDL-ESM2G" = "www/maps/tas_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
                                     "MIROC-ESM" = "www/maps/tas_Amon_MIROC-ESM_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                     "MPI-ESM-LR" = "www/maps/tas_Amon_MPI-ESM-LR_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                     "MRI-ESM1" = "www/maps/tas_Amon_MRI-ESM1_esmrcp85_r1i1p1_200601-210012_pattern.rds"))

})


test_that("globals:  precipitation patterns", {

  globalPrecipPatterns <- get_globalPrecipPatterns()

  expect_equal(globalPrecipPatterns, c("CanESM2" = "www/maps/pr_Amon_CanESM2_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "CESM1-BGC" = "www/maps/pr_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "GFDL-ESM2G" = "www/maps/pr_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "MIROC-ESM" = "www/maps/pr_Amon_MIROC-ESM_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "MPI-ESM-LR" = "www/maps/pr_Amon_MPI-ESM-LR_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "MRI-ESM1" = "www/maps/pr_Amon_MRI-ESM1_rcp85_r1i1p1_200601-210012_pattern.rds"))

})


test_that("globals:  parameters", {

  globalParameters <- get_globalParameters()

  # Main Model Input Parameters
  expect_equal(globalParameters[['pco2']], hector::PREINDUSTRIAL_CO2())
  expect_equal(globalParameters[['q10']], hector::Q10_RH())
  expect_equal(globalParameters[['beta']], hector::BETA())
  expect_equal(globalParameters[['ecs']], hector::ECS())
  expect_equal(globalParameters[['aero']], hector::AERO_SCALE())
  expect_equal(globalParameters[['volc']], hector::VOLCANIC_SCALE())
  expect_equal(globalParameters[['diff']], hector::DIFFUSIVITY())

})


test_that("globals:  default params", {

  globalParamsDefault <- get_globalParamsDefault()
  globalParamsCanESM2 <- get_globalParamsCanESM2()
  globalParamsCESM1BGC <- get_globalParamsCESM1BGC()
  globalParamsGFDLESM2G <- get_globalParamsGFDLESM2G()
  globalParamsMIROCESM <- get_globalParamsMIROCESM()
  globalParamsMPIESMLR <- get_globalParamsMPIESMLR()
  globalParamsMRIESM1 <- get_globalParamsMRIESM1()

  # Default Hector parameters
  expect_equal(globalParamsDefault, c('alpha' = 1, 'beta' = 0.36, 'diff' = 2.3, 'S' = 3, 'C' = 276.09, 'q10_rh' = 2, 'volscl' = 1))

  # CanESM2 Parameter Sets
  expect_equal(globalParamsCanESM2, c('alpha' = 1.87, 'beta' = 0.08, 'diff' = 0.98, 'S' = 3.88, 'C' = 282.35, 'q10_rh' = 1.75, 'volscl' = 1.81))

  # CESM1-BGC Parameter Set
  expect_equal(globalParamsCESM1BGC, c('alpha' = -0.43, 'beta' = 0.0, 'diff' = 8, 'S' = 2.4, 'C' = 280.31, 'q10_rh' = 1.78, 'volscl' = 3.94))

  # GFDL-ESM2G Parameter Set
  expect_equal(globalParamsGFDLESM2G, c('alpha' = 0.46, 'beta' = 0.07, 'diff' = 12.01, 'S' = 2.03, 'C' = 289.24, 'q10_rh' = 1.76, 'volscl' = 2.12))

  # MIROC-ESM Parameter Set
  expect_equal(globalParamsMIROCESM, c('alpha' = 1.05, 'beta' = 0.02, 'diff' = 6.39, 'S' = 5.83, 'C' = 283.31, 'q10_rh' = 1.77, 'volscl' = 2.02))

  # MPI-ESM-LR Parameter Set
  expect_equal(globalParamsMPIESMLR, c('alpha' = 1.22, 'beta' = 0.28, 'diff' = 2.93, 'S' = 3.66, 'C' = 289.13, 'q10_rh' = 1.75, 'volscl' = 0.70))

  # MRI-ESM1 Parameter Set
  expect_equal(globalParamsMRIESM1, c('alpha' = 1.42, 'beta' = 0.66, 'diff' = 4.21, 'S' = 2.04, 'C' = 289.49, 'q10_rh' = 1.76, 'volscl' = 0.27))

})


test_that("globals:  output parameters", {

  globalCapabilities <- get_globalCapabilities()

  expect_equal(globalCapabilities[['cc_lcf']][1] , hector::LAND_CFLUX())
  # "Atmospheric CO2"
  expect_equal(globalCapabilities[['cc_co2']][1] , hector::ATMOSPHERIC_CO2())
  # "Atmospheric Carbon Pool"
  expect_equal(globalCapabilities[['cc_acp']][1] , hector::ATMOSPHERIC_C())
  # "Fossil Fuel and Industrial Emissions"
  expect_equal(globalCapabilities[['cc_ffi']][1] , hector::FFI_EMISSIONS())
  # "Land Use Change Emissions"
  expect_equal(globalCapabilities[['cc_luc']][1] , hector::LUC_EMISSIONS())

  # CONCENTRATIONS
  # "Amospheric N2O"
  expect_equal(globalCapabilities[['c_an20']][1] , hector::ATMOSPHERIC_N2O())
  # "Preindustrial Atmospheric CO2"
  expect_equal(globalCapabilities[['c_paco2']][1] , hector::PREINDUSTRIAL_CO2())
  # "Preindustrial Ozone Concentration"
  expect_equal(globalCapabilities[['c_po']][1] , hector::PREINDUSTRIAL_O3())

  # EMISSIONS
  # "Black Carbon Emissions"
  expect_equal(globalCapabilities[['e_bc']][1] , hector::EMISSIONS_BC())
  # "N20 Emissions"
  expect_equal(globalCapabilities[['e_n2o']][1] , hector::EMISSIONS_N2O())
  # "NOx Emissions"
  expect_equal(globalCapabilities[['e_nox']][1] , hector::EMISSIONS_NOX())
  # "CO Emissions"
  expect_equal(globalCapabilities[['e_co']][1] , hector::EMISSIONS_CO())
  # "NMVOC Emissions"
  expect_equal(globalCapabilities[['e_nmvoc']][1] , hector::EMISSIONS_NMVOC())
  # "Organic Carbon Emissions"
  expect_equal(globalCapabilities[['e_oc']][1] , hector::EMISSIONS_OC())

  # FORCINGS
  # "RF - Total"
  expect_equal(globalCapabilities[['f_rft']][1] , hector::RF_TOTAL())
  # "RF - Albedo"
  expect_equal(globalCapabilities[['f_alb']][1] , hector::RF_T_ALBEDO())
  # "RF - CO2"
  expect_equal(globalCapabilities[['f_co2']][1] , hector::RF_CO2())
  # "RF - N2O"
  expect_equal(globalCapabilities[['f_n2o']][1] , hector::RF_N2O())

  # "RF - Black Carbon"
  expect_equal(globalCapabilities[['f_bc']][1] , hector::RF_BC())
  # "RF - Organic Carbon"
  expect_equal(globalCapabilities[['f_oc']][1] , hector::RF_OC())
  # "RF - SO2 Direct"
  expect_equal(globalCapabilities[['f_so2d']][1] , hector::RF_SO2D())
  # "RF - SO2 Indirect"
  expect_equal(globalCapabilities[['f_so2i']][1] , hector::RF_SO2I())
  # "RF - SO2 Total"
  expect_equal(globalCapabilities[['f_so2t']][1] , hector::RF_SO2())
  # "RF - Volcanic Activity"
  expect_equal(globalCapabilities[['f_va']][1] , hector::RF_VOL())
  # "RF - CH4"
  expect_equal(globalCapabilities[['f_ch4']][1] , hector::RF_CH4())

  # HALOCARBON EMISSIONS
  # "CF4 Emissions"
  expect_equal(globalCapabilities[['he_cf4']][1] , hector::EMISSIONS_CF4())
  # "C2F6 Emissions"
  expect_equal(globalCapabilities[['he_c2f6']][1] , hector::EMISSIONS_C2F6())
  # "HFC-23 Emissions"
  expect_equal(globalCapabilities[['he_hfc23']][1] , hector::EMISSIONS_HFC23())
  # "HFC-32 Emissions"
  expect_equal(globalCapabilities[['he_hfc32']][1] , hector::EMISSIONS_HFC32())
  # "HFC-4310 Emissions"
  expect_equal(globalCapabilities[['he_hfc4310']][1] , hector::EMISSIONS_HFC4310())
  # "HFC-125 Emissions"
  expect_equal(globalCapabilities[['he_hfc125']][1] , hector::EMISSIONS_HFC125())
  # "HFC-134a Emissions"
  expect_equal(globalCapabilities[['he_hfc134a']][1] , hector::EMISSIONS_HFC134A())
  # "HFC-143a Emissions"
  expect_equal(globalCapabilities[['he_hfc143a']][1] , hector::EMISSIONS_HFC143A())
  # "HFC-227ea Emissions"
  expect_equal(globalCapabilities[['he_hfc227ea']][1] , hector::EMISSIONS_HFC227EA())
  # "HFC-254fa Emissions"
  expect_equal(globalCapabilities[['he_245fa']][1] , hector::EMISSIONS_HFC245FA())
  # "SF6 Emissions"'
  expect_equal(globalCapabilities[['he_sf6']][1] , hector::EMISSIONS_SF6())
  # "CFC-11 Emissions"
  expect_equal(globalCapabilities[['he_cfc11']][1] , hector::EMISSIONS_CFC11())
  # "CFC-12 Emissions"
  expect_equal(globalCapabilities[['he_cfc12']][1] , hector::EMISSIONS_CFC12())
  # "CFC-113 Emissions"
  expect_equal(globalCapabilities[['he_cfc113']][1] , hector::EMISSIONS_CFC113())
  # "CFC-114 Emissions"
  expect_equal(globalCapabilities[['he_cfc114']][1] , hector::EMISSIONS_CFC114())
  # "CFC-115 Emissions"
  expect_equal(globalCapabilities[['he_cfc115']][1] , hector::EMISSIONS_CFC115())
  # "CCl4 Emissions"
  expect_equal(globalCapabilities[['he_ccl4']][1] , hector::EMISSIONS_CCL4())
  # "CH3CCl3 Emissions"
  expect_equal(globalCapabilities[['he_ch3ccl3']][1] , hector::EMISSIONS_CH3CCL3())
  # "Halon-1211 Emissions"
  expect_equal(globalCapabilities[['he_halon1211']][1] , hector::EMISSIONS_HALON1211())
  # "Halon-1301 Emissions"
  expect_equal(globalCapabilities[['he_halon1301']][1] , hector::EMISSIONS_HALON1301())
  # "Halon-2402 Emissions"
  expect_equal(globalCapabilities[['he_halon2402']][1] , hector::EMISSIONS_HALON2402())
  # "CH3Cl Emissions"
  expect_equal(globalCapabilities[['he_ch3cl']][1] , hector::EMISSIONS_CH3CL())
  # "CH3Br Emissions"
  expect_equal(globalCapabilities[['he_ch3br']][1] , hector::EMISSIONS_CH3BR())

  # HALOCARBON FORCINGS
  # "CF4 Forcing"
  expect_equal(globalCapabilities[['hf_cf4']][1] , hector::RF_CF4())
  # "C2F6 Forcing"
  expect_equal(globalCapabilities[['hf_c2f6']][1] , hector::RF_C2F6())
  # "HFC-23 Forcing"
  expect_equal(globalCapabilities[['hf_hfc23']][1] , hector::RF_HFC23())
  # "HFC-32 Forcing"
  expect_equal(globalCapabilities[['hf_hfc32']][1] , hector::RF_HFC32())
  # "HFC-4310 Forcing"
  expect_equal(globalCapabilities[['hf_hfc4310']][1] , hector::RF_HFC4310())
  # "HFC-125 Forcing"
  expect_equal(globalCapabilities[['hf_hfc125']][1] , hector::RF_HFC125())
  # "HFC-134a Forcing"
  expect_equal(globalCapabilities[['hf_hfc134a']][1] , hector::RF_HFC134A())
  # "HFC-143a Forcing"
  expect_equal(globalCapabilities[['hf_hfc143a']][1] , hector::RF_HFC143A())
  # "HFC-227ea Forcing"
  expect_equal(globalCapabilities[['hf_hfc227ea']][1] , hector::RF_HFC227EA())
  # "HFC-245fa Forcing"
  expect_equal(globalCapabilities[['hf_hfc245fa']][1] , hector::RF_HFC245FA())
  # "SF6 Forcing"
  expect_equal(globalCapabilities[['hf_hfcsf6']][1] , hector::RF_SF6())
  # "CFC-11 Forcing"
  expect_equal(globalCapabilities[['hf_cfc11']][1] , hector::RF_CFC11())
  # "CFC-12 Forcing"
  expect_equal(globalCapabilities[['hf_cfc12']][1] , hector::RF_CFC12())
  # "CFC-113 Forcing"
  expect_equal(globalCapabilities[['hf_cfc113']][1] , hector::RF_CFC113())
  # "CFC-114 Forcing"
  expect_equal(globalCapabilities[['hf_cfc114']][1] , hector::RF_CFC114())
  # "CFC-115 Forcing"
  expect_equal(globalCapabilities[['hf_cfc115']][1] , hector::RF_CFC115())
  # "CCl4 Forcing"
  expect_equal(globalCapabilities[['hf_ccl4']][1] , hector::RF_CCL4())
  # "CH3CCl3 Forcing"
  expect_equal(globalCapabilities[['hf_ch3ccl3']][1] , hector::RF_CH3CCL3())

  # "Halon-1211 Forcing"
  expect_equal(globalCapabilities[['hf_halon1211']][1] , hector::RF_HALON1211())
  # "Halon-1301 Forcing"
  expect_equal(globalCapabilities[['hf_halon1301']][1] , hector::RF_HALON1301())
  # "Halon-2402 Forcing"
  expect_equal(globalCapabilities[['hf_halon2402']][1] , hector::RF_HALON2402())
  # "CH3Cl Forcing"
  expect_equal(globalCapabilities[['hf_ch3cl']][1] , hector::RF_CH3CL())
  # "CH3Br Forcing"
  expect_equal(globalCapabilities[['hf_ch3br']][1] , hector::RF_CH3BR())

  # METHANE
  # "Atmospheric CH4"
  expect_equal(globalCapabilities[['m_a_ch4']][1] , hector::ATMOSPHERIC_CH4())
  # "Preindustrial Atmospheric CH4"
  expect_equal(globalCapabilities[['m_pa_ch4']][1] , hector::PREINDUSTRIAL_CH4())
  # "Emissions CH4"
  expect_equal(globalCapabilities[['m_e_ch4']][1] , hector::EMISSIONS_CH4())
  # "Natural CH4 Emissions"
  expect_equal(globalCapabilities[['m_n_ch4']][1] , hector::NATURAL_CH4())
  # "Methane Loss - Soil"
  expect_equal(globalCapabilities[['m_soil_loss']][1] , hector::LIFETIME_SOIL())
  # "Methane Loss - Straosphere"
  expect_equal(globalCapabilities[['m_strat_loss']][1] , hector::LIFETIME_STRAT())

  # OCEAN
  # "Ocean Carbon Flux"
  expect_equal(globalCapabilities[['o_cf']][1] , hector::OCEAN_CFLUX())
  # "Ocean Total Carbon"
  expect_equal(globalCapabilities[['o_tc']][1] , hector::OCEAN_C())
  # "Ocean Surface High-Lat Carbon"
  expect_equal(globalCapabilities[['o_os_hlc']][1] , hector::OCEAN_C_HL())
  # "Ocean Surface Low-Lat Carbon"
  expect_equal(globalCapabilities[['o_os_llc']][1] , hector::OCEAN_C_LL())
  # "Ocean Intermediate Carbon"
  expect_equal(globalCapabilities[['o_ic']][1] , hector::OCEAN_C_IO())
  # "Ocean Deep Carbon"
  expect_equal(globalCapabilities[['o_dc']][1] , hector::OCEAN_C_DO())
  # "Thermohaline Overturning"
  expect_equal(globalCapabilities[['o_to']][1] , hector::TT())
  # "High-Lat Overturning"
  expect_equal(globalCapabilities[['o_hl_o']][1] , hector::TU())
  # "Warm-Intermediate Exchange"
  expect_equal(globalCapabilities[['o_wie']][1] , hector::TWI())
  # "Intermediate-Deep Exchange"
  expect_equal(globalCapabilities[['o_ide']][1] , hector::TID())
  # "High Latitude Ph"
  expect_equal(globalCapabilities[['o_hl_ph']][1] , hector::PH_HL())
  # "Low Latitude Ph"
  expect_equal(globalCapabilities[['o_ll_ph']][1] , hector::PH_LL())
  # "Atmosphere-Ocean Flux - High Lat"
  expect_equal(globalCapabilities[['o_hl_aof']][1] , hector::ATM_OCEAN_FLUX_HL())
  # "Atmosphere-Ocean Flux - Low Lat"
  expect_equal(globalCapabilities[['o_ll_aof']][1] , hector::ATM_OCEAN_FLUX_LL())
  # "Partial Pressure CO2 - High Lat"
  expect_equal(globalCapabilities[['o_hl_pp_co2']][1] , hector::PCO2_HL())
  # "Partial Pressure CO2 - Low Lat"
  expect_equal(globalCapabilities[['o_ll_pp_co2']][1] , hector::PCO2_LL())
  # "Dissolved Inorganic C - High Lat"
  expect_equal(globalCapabilities[['o_hl_dic']][1] , hector::DIC_HL())
  # "Dissolved Inorganic C - Low Lat"'
  expect_equal(globalCapabilities[['o_ll_dic']][1] , hector::DIC_LL())
  # "Ocean Temperature - High Lat"
  expect_equal(globalCapabilities[['o_hl_t']][1] , hector::TEMP_HL())
  # "Ocean Temperature - Low Lat"
  expect_equal(globalCapabilities[['o_ll_t']][1] , hector::TEMP_LL())
  # "Carbonate Concentration - High Lat"
  expect_equal(globalCapabilities[['o_hl_cc']][1] , hector::CO3_HL())
  # "Carbonate Concentration - Low Lat"
  expect_equal(globalCapabilities[['o_ll_cc']][1] , hector::CO3_LL())

  # SO2
  # "Natural SO2"
  expect_equal(globalCapabilities[['so2_n']][1] , hector::NATURAL_SO2())
  # "Year 2000 SO2"
  expect_equal(globalCapabilities[['so2_y2k']][1] , hector::Y2000_SO2())
  # "Anthropogenic SO2"
  expect_equal(globalCapabilities[['so2_a']][1] , hector::EMISSIONS_SO2())
  # "Natural CH4 Emissions"
  expect_equal(globalCapabilities[['so2_n_ch4']][1] , hector::EMISSIONS_CH4())
  # "Volcanic SO2"
  expect_equal(globalCapabilities[['so2_v']][1] , hector::VOLCANIC_SO2())

  # TEMPERATURE
  # "Global Mean Temp"
  expect_equal(globalCapabilities[['t_gmt']][1] , hector::GLOBAL_TEMP())
  # "Equilibrium Global Temp"
  expect_equal(globalCapabilities[['t_egt']][1] , hector::GLOBAL_TEMPEQ())
  # "Ocean Surface Temp"
  expect_equal(globalCapabilities[['t_ost']][1] , hector::OCEAN_SURFACE_TEMP())
  # "Ocean Air Temp"
  expect_equal(globalCapabilities[['t_oat']][1] , hector::OCEAN_AIR_TEMP())

  # "Heat Flux - Mixed Layer Ocean"
  expect_equal(globalCapabilities[['t_hf_mlo']][1] , hector::FLUX_MIXED())
  # "Heat Flux - Interior Layer Ocean"
  expect_equal(globalCapabilities[['t_hf_ilo']][1] , hector::FLUX_INTERIOR())
  # "Total Heat Flux - Ocean"
  expect_equal(globalCapabilities[['t_hf_t']][1] , hector::HEAT_FLUX())

})



