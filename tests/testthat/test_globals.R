library(testthat)
library(hector)

context("globals")

test_that("globals:  miscellaneous", {

  expect_equal(globalVars[['startDate']], 1900)
  expect_equal(globalVars[['endDate']], 2100)
  expect_equal(globalVars[['writeDirectory']], "temp")

})


test_that("globals:  color scale", {

  expect_equal(globalColorScales, c("RCP 2.6"="#db735c", "RCP 4.5"="#EFA86E", "RCP 6.0"="#9A8A76", "RCP 8.5"="#7A6752"))

})


test_that("globals:  file paths", {

  expect_equal(rcps, c(26,45,60,85))
  expect_equal(globalScenarios[['RCP-2.6']], file.path('input',paste0('hector_rcp',26,'.ini')))
  expect_equal(globalScenarios[['RCP-4.5']], file.path('input',paste0('hector_rcp',45,'.ini')))
  expect_equal(globalScenarios[['RCP-6.0']], file.path('input',paste0('hector_rcp',60,'.ini')))
  expect_equal(globalScenarios[['RCP-8.5']], file.path('input',paste0('hector_rcp',85,'.ini')))
  expect_equal(globalScenarioColors, c("RCP 2.6" = "#99cc33", "RCP 4.5" = "#FFFF00", "RCP 6.0" = "#ff9900", "RCP 8.5" = "#ff3333"))

})


test_that("globals:  temperature patterns", {

  expect_equal(globalTempPatterns, c("CanESM2" = "www/maps/tas_Amon_CanESM2_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                     "CESM1-BGC" = "www/maps/tas_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
                                     "GFDL-ESM2G" = "www/maps/tas_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
                                     "MIROC-ESM" = "www/maps/tas_Amon_MIROC-ESM_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                     "MPI-ESM-LR" = "www/maps/tas_Amon_MPI-ESM-LR_esmrcp85_r1i1p1_200601-210012_pattern.rds",
                                     "MRI-ESM1" = "www/maps/tas_Amon_MRI-ESM1_esmrcp85_r1i1p1_200601-210012_pattern.rds"))

})


test_that("globals:  precipitation patterns", {

  expect_equal(globalPrecipPatterns, c("CanESM2" = "www/maps/pr_Amon_CanESM2_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "CESM1-BGC" = "www/maps/pr_Amon_CESM1-BGC_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "GFDL-ESM2G" = "www/maps/pr_Amon_GFDL-ESM2G_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "MIROC-ESM" = "www/maps/pr_Amon_MIROC-ESM_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "MPI-ESM-LR" = "www/maps/pr_Amon_MPI-ESM-LR_rcp85_r1i1p1_200601-210012_pattern.rds",
                                       "MRI-ESM1" = "www/maps/pr_Amon_MRI-ESM1_rcp85_r1i1p1_200601-210012_pattern.rds"))

})


test_that("globals:  parameters", {

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





