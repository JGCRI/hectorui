# Build Global variables for Hector capabilities

#' Global constants for the Hector-Shiny UI
#' @import hector
#' @name constants
NULL

# Global vars for misc items such as run date end (2100)
#' @details \code{globalVars}: Miscellaneous global variables
#' @rdname constants
#' @export
globalVars <- vector()
globalVars['endDate'] <- 2100

# Create global file input vector
#' @details \code{globalScenarios} Scenario input files
#' @rdname constants
#' @export
globalScenarios <- NULL
rcps <- c(26,45,60,85)
globalScenarios <- file.path('input',paste0('hector_rcp',rcps,'.ini'))
names(globalScenarios) <- paste('RCP', c('2.6', '4.5', '6.0', '8.5'))

# Create master list of parameter lookups
#' @details \code{globalParameters}: Capability strings for Hector parameters
#' @rdname constants
#' @export
globalParameters <- vector()
# Load parameter strings into global variable (should match UI component ids)
globalParameters['pco2'] <- hector::PREINDUSTRIAL_CO2()
globalParameters['q10'] <- hector::Q10_RH() #2.0
globalParameters['beta'] <- hector::BETA() #0.36
globalParameters['ecs'] <- hector::ECS() #3.0
globalParameters['aero'] <- hector::AERO_SCALE() #1.0
globalParameters['volc'] <- hector::VOLCANIC_SCALE() #1.0
globalParameters['diff'] <- hector::DIFFUSIVITY() #2.3
# UNUSED Parameters as of now
# globalParameters['fnppv'] <- hector::F_NPPV()
# globalParameters['fnppd'] <- hector::F_NPPD()
# globalParameters['flitter'] <- hector::F_LITTERD()
# globalParameters['flucv'] <- hector::F_LUCV()
# globalParameters['flucd'] <- hector::F_LUCD()


# Output Variables: This section maps output variables (should match drop down item ids)

# Create master list of variable lookups for "capabilities" (output variables for graphing)
#' @details \code{globalCapabilities} Capability strings for Hector variables
#' @rdname constants
#' @export
globalCapabilities <- list()

# CARBON CYCLE
# "Land Carbon Flux"
globalCapabilities[['cc_lcf']] <- hector::LAND_CFLUX()
attr(globalCapabilities[['cc_lcf']], "name") <- "Land Carbon Flux"
# "Atmospheric CO2"
globalCapabilities[['cc_co2']] <- hector::ATMOSPHERIC_CO2()
attr(globalCapabilities[['cc_co2']], "name") <- "Atmospheric CO2"
# "Atmospheric Carbon Pool"
globalCapabilities[['cc_acp']] <- hector::ATMOSPHERIC_C()
attr(globalCapabilities[['cc_acp']], "name") <- "Atmospheric Carbon Pool"

# CONCENTRATIONS
# "Amospheric N2O"
globalCapabilities[['c_an20']] <- hector::ATMOSPHERIC_N2O()
attr(globalCapabilities[['c_an20']], "name") <- "Amospheric N2O"
# "Preindustrial Atmospheric CO2"
globalCapabilities[['c_paco2']] <- hector::PREINDUSTRIAL_CO2()
attr(globalCapabilities[['c_paco2']], "name") <- "Preindustrial Atmospheric CO2"
# "Preindustrial Ozone Concentration"
globalCapabilities[['c_po']] <- hector::PREINDUSTRIAL_O3()
attr(globalCapabilities[['c_po']], "name") <- "Preindustrial Ozone Concentration"

# EMISSIONS
# "Black Carbon Emissions"
globalCapabilities[['e_bc']] <- hector::EMISSIONS_BC()
attr(globalCapabilities[['e_bc']], "name") <- "Black Carbon Emissions"
# "N20 Emissions"
globalCapabilities[['e_n2o']] <- hector::EMISSIONS_N2O()
attr(globalCapabilities[['e_n2o']], "name") <- "N20 Emissions"
# "NOx Emissions"
globalCapabilities[['e_nox']] <- hector::EMISSIONS_NOX()
attr(globalCapabilities[['e_nox']], "name") <- "NOx Emissions"
# "CO Emissions"
globalCapabilities[['e_co']] <- hector::EMISSIONS_CO()
attr(globalCapabilities[['e_co']], "name") <- "CO Emissions"
# "NMVOC Emissions"
globalCapabilities[['e_nmvoc']] <- hector::EMISSIONS_NMVOC()
attr(globalCapabilities[['e_nmvoc']], "name") <- "NMVOC Emissions"
# "Organic Carbon Emissions"
globalCapabilities[['e_oc']] <- hector::EMISSIONS_OC()
attr(globalCapabilities[['e_oc']], "name") <- "Organic Carbon Emissions"

# FORCINGS
# "RF - Total"
globalCapabilities[['f_rft']] <- hector::RF_TOTAL()
attr(globalCapabilities[['f_rft']], "name") <- "RF - Total"
# "RF - Albedo"
globalCapabilities[['f_alb']] <- hector::RF_T_ALBEDO()
attr(globalCapabilities[['f_alb']], "name") <- "RF - Albedo"
# "RF - CO2"
globalCapabilities[['f_co2']] <- hector::RF_CO2()
attr(globalCapabilities[['f_co2']], "name") <- "RF - CO2"
# "RF - N2O"
globalCapabilities[['f_n2o']] <- hector::RF_N2O()
attr(globalCapabilities[['f_n2o']], "name") <- "RF - N2O"
# "RF - H2O"
globalCapabilities[['f_h2o']] <- hector::RF_H2O()
attr(globalCapabilities[['f_h2o']], "name") <- "RF - H2O"
# "RF - Ozone"
globalCapabilities[['f_oz']] <- hector::RF_O3()
attr(globalCapabilities[['f_oz']], "name") <- "RF - Ozone"
# "RF - Black Carbon"
globalCapabilities[['f_bc']] <- hector::RF_BC()
attr(globalCapabilities[['f_bc']], "name") <- "RF - Black Carbon"
# "RF - Organic Carbon"
globalCapabilities[['f_oc']] <- hector::RF_OC()
attr(globalCapabilities[['f_oc']], "name") <- "RF - Organic Carbon"
# "RF - SO2 Direct"
globalCapabilities[['f_so2d']] <- hector::RF_SO2D()
attr(globalCapabilities[['f_so2d']], "name") <- "RF - SO2 Direct"
# "RF - SO2 Indirect"
globalCapabilities[['f_so2i']] <- hector::RF_SO2I()
attr(globalCapabilities[['f_so2i']], "name") <- "RF - SO2 Indirect"
# "RF - SO2 Total"
globalCapabilities[['f_so2t']] <- hector::RF_SO2()
attr(globalCapabilities[['f_so2t']], "name") <- "RF - SO2 Total"
# "RF - Volcanic Activity"
globalCapabilities[['f_va']] <- hector::RF_VOL()
attr(globalCapabilities[['f_va']], "name") <- "RF - Volcanic Activity"
# "RF - CH4"
globalCapabilities[['f_ch4']] <- hector::RF_CH4()
attr(globalCapabilities[['f_ch4']], "name") <- "RF - CH4"

# HALOCARBON EMISSIONS
 # "CF4 Emissions"
globalCapabilities[['he_cf4']] <- hector::EMISSIONS_CF4()
attr(globalCapabilities[['he_cf4']], "name") <- "CF4 Emissions"
# "C2F6 Emissions"
globalCapabilities[['he_c2f6']] <- hector::EMISSIONS_C2F6()
attr(globalCapabilities[['he_c2f6']], "name") <- "C2F6 Emissions"
# "HFC-23 Emissions"
globalCapabilities[['he_hfc23']] <- hector::EMISSIONS_HFC23()
attr(globalCapabilities[['he_hfc23']], "name") <- "HFC-23 Emissions"
# "HFC-32 Emissions"
globalCapabilities[['he_hfc32']] <- hector::EMISSIONS_HFC32()
attr(globalCapabilities[['he_hfc32']], "name") <- "HFC-32 Emissions"
# "HFC-4310 Emissions"
globalCapabilities[['he_hfc4310']] <- hector::EMISSIONS_HFC4310()
attr(globalCapabilities[['he_hfc4310']], "name") <- "HFC-4310 Emissions"
# "HFC-125 Emissions"
globalCapabilities[['he_hfc125']] <- hector::EMISSIONS_HFC125()
attr(globalCapabilities[['he_hfc125']], "name") <- "HFC-125 Emissions"
# "HFC-134a Emissions"
globalCapabilities[['he_hfc134a']] <- hector::EMISSIONS_HFC134A()
attr(globalCapabilities[['he_hfc134a']], "name") <- "HFC-134a Emissions"
# "HFC-143a Emissions"
globalCapabilities[['he_hfc143a']] <- hector::EMISSIONS_HFC143A()
attr(globalCapabilities[['he_hfc143a']], "name") <- "HFC-143a Emissions"
# "HFC-227ea Emissions"
globalCapabilities[['he_hfc227ea']] <- hector::EMISSIONS_HFC227EA()
attr(globalCapabilities[['he_hfc227ea']], "name") <- "HFC-227ea Emissions"
# "HFC-254fa Emissions"
globalCapabilities[['he_245fa']] <- hector::EMISSIONS_HFC245FA()
attr(globalCapabilities[['he_245fa']], "name") <-  "HFC-254fa Emissions"
# "SF6 Emissions"'
globalCapabilities[['he_sf6']] <- hector::EMISSIONS_SF6()
attr(globalCapabilities[['he_sf6']], "name") <- "SF6 Emissions"
# "CFC-11 Emissions"
globalCapabilities[['he_cfc11']] <- hector::EMISSIONS_CFC11()
attr(globalCapabilities[['he_cfc11']], "name") <- "CFC-11 Emissions"
# "CFC-12 Emissions"
globalCapabilities[['he_cfc12']] <- hector::EMISSIONS_CFC12()
attr(globalCapabilities[['he_cfc12']], "name") <-"CFC-12 Emissions"
# "CFC-113 Emissions"
globalCapabilities[['he_cfc113']] <- hector::EMISSIONS_CFC113()
attr(globalCapabilities[['he_cfc113']], "name") <-"CFC-113 Emissions"
# "CFC-114 Emissions"
globalCapabilities[['he_cfc114']] <- hector::EMISSIONS_CFC114()
attr(globalCapabilities[['he_cfc114']], "name") <-"CFC-114 Emissions"
# "CFC-115 Emissions"
globalCapabilities[['he_cfc115']] <- hector::EMISSIONS_CFC115()
attr(globalCapabilities[['he_cfc115']], "name") <-"CFC-115 Emissions"
# "CCl4 Emissions"
globalCapabilities[['he_ccl4']] <- hector::EMISSIONS_CCL4()
attr(globalCapabilities[['he_ccl4']], "name") <-"CCl4 Emissions"
# "CH3CCl3 Emissions"
globalCapabilities[['he_ch3ccl3']] <- hector::EMISSIONS_CH3CCL3()
attr(globalCapabilities[['he_ch3ccl3']], "name") <- "CH3CCl3 Emissions"
# "HCFC-22 Emissions"
globalCapabilities[['he_hcfc22']] <- hector::EMISSIONS_HCF22()
attr(globalCapabilities[['he_hcfc22']], "name") <- "HCFC-22 Emissions"
# "HCFC-141b Emissions"
globalCapabilities[['he_hcfc141b']] <- hector::EMISSIONS_HCF141B()
attr(globalCapabilities[['he_hcfc141b']], "name") <- "HCFC-141b Emissions"
# "HCFC-142b Emissions"
globalCapabilities[['he_hcfc142b']] <- hector::EMISSIONS_HCF142B()
attr(globalCapabilities[['he_hcfc142b']], "name") <- "HCFC-142b Emissions"
# "Halon-1211 Emissions"
globalCapabilities[['he_halon1211']] <- hector::EMISSIONS_HALON1211()
attr(globalCapabilities[['he_halon1211']], "name") <- "Halon-1211 Emissions"
# "Halon-1301 Emissions"
globalCapabilities[['he_halon1301']] <- hector::EMISSIONS_HALON1301()
attr(globalCapabilities[['he_halon1301']], "name") <- "Halon-1301 Emissions"
# "Halon-2402 Emissions"
globalCapabilities[['he_halon2402']] <- hector::EMISSIONS_HALON2402()
attr(globalCapabilities[['he_halon2402']], "name") <- "Halon-2402 Emissions"
# "CH3Cl Emissions"
globalCapabilities[['he_ch3cl']] <- hector::EMISSIONS_CH3CL()
attr(globalCapabilities[['he_ch3cl']], "name") <- "CH3Cl Emissions"
# "CH3Br Emissions"
globalCapabilities[['he_ch3br']] <- hector::EMISSIONS_CH3BR()
attr(globalCapabilities[['he_ch3br']], "name") <- "CH3Br Emissions"

# HALOCARBON FORCINGS
# "CF4 Forcing"
globalCapabilities[['hf_cf4']] <- hector::RF_CF4()
attr(globalCapabilities[['hf_cf4']], "name") <- "CF4 Forcing"
# "C2F6 Forcing"
globalCapabilities[['hf_c2f6']] <- hector::RF_C2F6()
attr(globalCapabilities[['hf_c2f6']], "name") <-  "C2F6 Forcing"
# "HFC-23 Forcing"
globalCapabilities[['hf_hfc23']] <- hector::RF_HFC23()
attr(globalCapabilities[['hf_hfc23']], "name") <- "HFC-23 Forcing"
# "HFC-32 Forcing"
globalCapabilities[['hf_hfc32']] <- hector::RF_HFC32()
attr(globalCapabilities[['hf_hfc32']], "name") <- "HFC-32 Forcing"
# "HFC-4310 Forcing"
globalCapabilities[['hf_hfc4310']] <- hector::RF_HFC4310()
attr(globalCapabilities[['hf_hfc4310']], "name") <- "HFC-4310 Forcing"
# "HFC-125 Forcing"
globalCapabilities[['hf_hfc125']] <- hector::RF_HFC125()
attr(globalCapabilities[['hf_hfc125']], "name") <-  "HFC-125 Forcing"
# "HFC-134a Forcing"
globalCapabilities[['hf_hfc134a']] <- hector::RF_HFC134A()
attr(globalCapabilities[['hf_hfc134a']], "name") <- "HFC-134a Forcing"
# "HFC-143a Forcing"
globalCapabilities[['hf_hfc143a']] <- hector::RF_HFC143A()
attr(globalCapabilities[['hf_hfc143a']], "name") <- "HFC-143a Forcing"
# "HFC-227ea Forcing"
globalCapabilities[['hf_hfc227ea']] <- hector::RF_HFC227EA()
attr(globalCapabilities[['hf_hfc227ea']], "name") <- "HFC-227ea Forcing"
# "HFC-245fa Forcing"
globalCapabilities[['hf_hfc245fa']] <- hector::RF_HFC245FA()
attr(globalCapabilities[['hf_hfc245fa']], "name") <- "HFC-245fa Forcing"
# "SF6 Forcing"
globalCapabilities[['hf_hfcsf6']] <- hector::RF_SF6()
attr(globalCapabilities[['hf_hfcsf6']], "name") <- "SF6 Forcing"
# "CFC-11 Forcing"
globalCapabilities[['hf_cfc11']] <- hector::RF_CFC11()
attr(globalCapabilities[['hf_cfc11']], "name") <- "CFC-11 Forcing"
# "CFC-12 Forcing"
globalCapabilities[['hf_cfc12']] <- hector::RF_CFC12()
attr(globalCapabilities[['hf_cfc12']], "name") <- "CFC-12 Forcing"
# "CFC-113 Forcing"
globalCapabilities[['hf_cfc113']] <- hector::RF_CFC113()
attr(globalCapabilities[['hf_cfc113']], "name") <- "CFC-113 Forcing"
# "CFC-114 Forcing"
globalCapabilities[['hf_cfc114']] <- hector::RF_CFC114()
attr(globalCapabilities[['hf_cfc114']], "name") <- "CFC-114 Forcing"
# "CFC-115 Forcing"
globalCapabilities[['hf_cfc115']] <- hector::RF_CFC115()
attr(globalCapabilities[['hf_cfc115']], "name") <- "CFC-115 Forcing"
# "CCl4 Forcing"
globalCapabilities[['hf_ccl4']] <- hector::RF_CCL4()
attr(globalCapabilities[['hf_ccl4']], "name") <- "CCl4 Forcing"
# "CH3CCl3 Forcing"
globalCapabilities[['hf_ch3ccl3']] <- hector::RF_CH3CCL3()
attr(globalCapabilities[['hf_ch3ccl3']], "name") <- "CH3CCl3 Forcing"
# "HCFC-22 Forcing"
globalCapabilities[['hf_hcfc22']] <- hector::RF_HCF22()
attr(globalCapabilities[['hf_hcfc22']], "name") <- "HCFC-22 Forcing"
# "HCFC-141b Forcing"
globalCapabilities[['hf_hcfc141b']] <- hector::RF_HCF141B()
attr(globalCapabilities[['hf_hcfc141b']], "name") <- "HCFC-141b Forcing"
# "HCFC-142b Forcing"
globalCapabilities[['hf_hcfc142b']] <- hector::RF_HCF142B()
attr(globalCapabilities[['hf_hcfc142b']], "name") <- "HCFC-142b Forcing"
# "Halon-1211 Forcing"
globalCapabilities[['hf_halon1211']] <- hector::RF_HALON1211()
attr(globalCapabilities[['hf_halon1211']], "name") <- "Halon-1211 Forcing"
# "Halon-1301 Forcing"
globalCapabilities[['hf_halon1301']] <- hector::RF_HALON1301()
attr(globalCapabilities[['hf_halon1301']], "name") <- "Halon-1301 Forcing"
# "Halon-2402 Forcing"
globalCapabilities[['hf_halon2402']] <- hector::RF_HALON2402()
attr(globalCapabilities[['hf_halon2402']], "name") <- "Halon-2402 Forcing"
# "CH3Cl Forcing"
globalCapabilities[['hf_ch3cl']] <- hector::RF_CH3CL()
attr(globalCapabilities[['hf_ch3cl']], "name") <- "CH3Cl Forcing"
# "CH3Br Forcing"
globalCapabilities[['hf_ch3br']] <- hector::RF_CH3BR()
attr(globalCapabilities[['hf_ch3br']], "name") <- "CH3Br Forcing"

# METHANE
# "Atmospheric CH4"
globalCapabilities[['m_a_ch4']] <- hector::ATMOSPHERIC_CH4()
attr(globalCapabilities[['m_a_ch4']], "name") <- "Atmospheric CH4"
# "Preindustrial Atmospheric CH4"
globalCapabilities[['m_pa_ch4']] <- hector::PREINDUSTRIAL_CH4()
attr(globalCapabilities[['m_pa_ch4']], "name") <- "Preindustrial Atmospheric CH4"
# "Emissions CH4"
globalCapabilities[['m_e_ch4']] <- hector::EMISSIONS_CH4()
attr(globalCapabilities[['m_e_ch4']], "name") <- "Emissions CH4"
# "Natural CH4 Emissions"
globalCapabilities[['m_n_ch4']] <- hector::NATURAL_CH4()
attr(globalCapabilities[['m_n_ch4']], "name") <- "Natural CH4 Emissions"
# "Methane Loss - Soil"
globalCapabilities[['m_soil_loss']] <- hector::LIFETIME_SOIL()
attr(globalCapabilities[['m_soil_loss']], "name") <- "Methane Loss - Soil"
# "Methane Loss - Straosphere"
globalCapabilities[['m_strat_loss']] <- hector::LIFETIME_STRAT
attr(globalCapabilities[['m_strat_loss']], "name") <- "Methane Loss - Straosphere"

# OCEAN
# "Ocean Carbon Flux"
globalCapabilities[['o_cf']] <- hector::OCEAN_CFLUX()
attr(globalCapabilities[['o_cf']], "name") <- "Ocean Carbon Flux"
# "Ocean Total Carbon"
globalCapabilities[['o_tc']] <- hector::OCEAN_C()
attr(globalCapabilities[['o_tc']], "name") <- "Ocean Total Carbon"
# "Ocean Surface High-Lat Carbon"
globalCapabilities[['o_os_hlc']] <- hector::OCEAN_C_HL()
attr(globalCapabilities[['o_os_hlc']], "name") <- "Ocean Surface High-Lat Carbon"
# "Ocean Surface Low-Lat Carbon"
globalCapabilities[['o_os_llc']] <- hector::OCEAN_C_LL()
attr(globalCapabilities[['o_os_llc']], "name") <- "Ocean Surface Low-Lat Carbon"
# "Ocean Intermediate Carbon"
globalCapabilities[['o_ic']] <- hector::OCEAN_C_IO()
attr(globalCapabilities[['o_ic']], "name") <- "Ocean Intermediate Carbon"
# "Ocean Deep Carbon"
globalCapabilities[['o_dc']] <- hector::OCEAN_C_DO()
attr(globalCapabilities[['o_dc']], "name") <- "Ocean Deep Carbon"
# "Thermohaline Overturning"
globalCapabilities[['o_to']] <- hector::TT()
attr(globalCapabilities[['o_to']], "name") <- "Thermohaline Overturning"
# "High-Lat Overturning"
globalCapabilities[['o_hl_o']] <- hector::TU()
attr(globalCapabilities[['o_hl_o']], "name") <- "High-Lat Overturning"
# "Warm-Intermediate Exchange"
globalCapabilities[['o_wie']] <- hector::TWI()
attr(globalCapabilities[['o_wie']], "name") <- "Warm-Intermediate Exchange"
# "Intermediate-Deep Exchange"
globalCapabilities[['o_ide']] <- hector::TID()
attr(globalCapabilities[['o_ide']], "name") <- "Intermediate-Deep Exchange"
# "High Latitude Ph"
globalCapabilities[['o_hl_ph']] <- hector::PH_HL()
attr(globalCapabilities[['o_hl_ph']], "name") <- "High Latitude Ph"
# "Low Latitude Ph"
globalCapabilities[['o_ll_ph']] <- hector::PH_LL()
attr(globalCapabilities[['o_ll_ph']], "name") <- "Low Latitude Ph"
# "Atmosphere-Ocean Flux - High Lat"
globalCapabilities[['o_hl_aof']] <- hector::ATM_OCEAN_FLUX_HL()
attr(globalCapabilities[['o_hl_aof']], "name") <- "Atmosphere-Ocean Flux - High Lat"
# "Atmosphere-Ocean Flux - Low Lat"
globalCapabilities[['o_ll_aof']] <- hector::ATM_OCEAN_FLUX_LL()
attr(globalCapabilities[['o_ll_aof']], "name") <- "Atmosphere-Ocean Flux - Low Lat"
# "Partial Pressure CO2 - High Lat"
globalCapabilities[['o_hl_pp_co2']] <- hector::PCO2_HL()
attr(globalCapabilities[['o_hl_pp_co2']], "name") <- "Partial Pressure CO2 - High Lat"
# "Partial Pressure CO2 - Low Lat"
globalCapabilities[['o_ll_pp_co2']] <- hector::PCO2_LL()
attr(globalCapabilities[['o_ll_pp_co2']], "name") <- "Partial Pressure CO2 - Low Lat"
# "Dissolved Inorganic C - High Lat"
globalCapabilities[['o_hl_dic']] <- hector::DIC_HL()
attr(globalCapabilities[['o_hl_dic']], "name") <- "Dissolved Inorganic C - High Lat"
# "Dissolved Inorganic C - Low Lat"'
globalCapabilities[['o_ll_dic']] <- hector::DIC_LL()
attr(globalCapabilities[['o_ll_dic']], "name") <- "Dissolved Inorganic C - Low Lat"
# "Ocean Temperature - High Lat"
globalCapabilities[['o_hl_t']] <- hector::TEMP_HL()
attr(globalCapabilities[['o_hl_t']], "name") <- "Ocean Temperature - High Lat"
# "Ocean Temperature - Low Lat"
globalCapabilities[['o_ll_t']] <- hector::TEMP_LL()
attr(globalCapabilities[['o_ll_t']], "name") <- "Ocean Temperature - Low Lat"
# "Carbonate Concentration - High Lat"
globalCapabilities[['o_hl_cc']] <- hector::CO3_HL()
attr(globalCapabilities[['o_hl_cc']], "name") <- "Carbonate Concentration - High Lat"
# "Carbonate Concentration - Low Lat"
globalCapabilities[['o_ll_cc']] <- hector::CO3_LL()
attr(globalCapabilities[['o_ll_cc']], "name") <- "Carbonate Concentration - Low Lat"

# SO2
# "Natural SO2"
globalCapabilities[['so2_n']] <- hector::NATURAL_SO2()
attr(globalCapabilities[['so2_n']], "name") <- "Natural SO2"
# "Year 2000 SO2"
globalCapabilities[['so2_y2k']] <- hector::Y2000_SO2()
attr(globalCapabilities[['so2_y2k']], "name") <- "Year 2000 SO2"
# "Anthropogenic SO2"
globalCapabilities[['so2_a']] <- hector::EMISSIONS_SO2()
attr(globalCapabilities[['so2_a']], "name") <- "Anthropogenic SO2"
# "Natural CH4 Emissions"
globalCapabilities[['so2_n_ch4']] <- hector::EMISSIONS_CH4()
attr(globalCapabilities[['so2_n_ch4']], "name") <- "Natural CH4 Emissions"
# "Volcanic SO2"
globalCapabilities[['so2_v']] <- hector::VOLCANIC_SO2()
attr(globalCapabilities[['so2_v']], "name") <- "Volcanic SO2"

# TEMPERATURE
# "Global Mean Temp"
globalCapabilities[['t_gmt']] <- hector::GLOBAL_TEMP()
attr(globalCapabilities[['t_gmt']], "name") <- "Global Mean Temp"
# "Equilibrium Global Temp"
globalCapabilities[['t_egt']] <- hector::GLOBAL_TEMPEQ()
attr(globalCapabilities[['t_egt']], "name") <- "Equilibrium Global Temp"
# "Ocean Surface Temp"
globalCapabilities[['t_ost']] <- hector::OCEAN_SURFACE_TEMP()
attr(globalCapabilities[['t_ost']], "name") <- "Ocean Surface Temp"
# "Ocean Air Temp"
globalCapabilities[['t_oat']] <- hector::OCEAN_AIR_TEMP()
attr(globalCapabilities[['t_oat']], "name") <- "Ocean Air Temp"
# "Land Temp Anomaly"
globalCapabilities[['t_lta']] <- hector::LAND_TEMP()
attr(globalCapabilities[['t_lta']], "name") <- "Land Temp Anomaly"
# "Heat Flux - Mixed Layer Ocean"
globalCapabilities[['t_hf_mlo']] <- hector::FLUX_MIXED()
attr(globalCapabilities[['t_hf_mlo']], "name") <- "Heat Flux - Mixed Layer Ocean"
# "Heat Flux - Interior Layer Ocean"
globalCapabilities[['t_hf_ilo']] <- hector::FLUX_INTERIOR()
attr(globalCapabilities[['t_hf_ilo']], "name") <- "Heat Flux - Interior Layer Ocean"
# "Total Heat Flux - Ocean"
globalCapabilities[['t_hf_t']] <- hector::HEAT_FLUX()
attr(globalCapabilities[['t_hf_t']], "name") <- "Total Heat Flux - Ocean"
