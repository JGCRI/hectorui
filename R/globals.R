# Build Global variables for Hector capabilities
library(hector)

# Create global file input vector
globalScenarios <- vector()

# Create master list of variable lookups
globalCapabilities <- vector()

# Create master list of parameter lookups
globalParameters <- vector()

# Load file input paths into global variable
globalScenarios['RCP 2.6'] <- "input/hector_rcp26.ini"
globalScenarios['RCP 4.5'] <- "input/hector_rcp45.ini"
globalScenarios['RCP 6.0'] <- "input/hector_rcp60.ini"
globalScenarios['RCP 8.5'] <- "input/hector_rcp85.ini"

# Load parameter strings into global variable (should match UI component ids)
globalParameters['pco2'] <- hector::PREINDUSTRIAL_CO2()
globalParameters['q10'] <- hector::Q10_RH()
globalParameters['beta'] <- hector::BETA()
globalParameters['ecs'] <- hector::ECS()
globalParameters['aero'] <- hector::AERO_SCALE()
globalParameters['volc'] <- hector::VOLCANIC_SCALE()
globalParameters['diff'] <- hector::DIFFUSIVITY()
# UNUSED Parameters as of now
globalParameters['fnppv'] <- hector::F_NPPV()
globalParameters['fnppd'] <- hector::F_NPPD()
globalParameters['flitter'] <- hector::F_LITTERD()
globalParameters['flucv'] <- hector::F_LUCV()
globalParameters['flucd'] <- hector::F_LUCD()


# Output Variables: This section maps output variables (should match drop down item ids)

# CARBON CYCLE
# "Land Carbon Flux"
globalCapabilities['cc_lcf'] <- hector::LAND_CFLUX()
# "Atmospheric CO2"
globalCapabilities['cc_co2'] <- hector::ATMOSPHERIC_CO2()
# "Atmospheric Carbon Pool"
globalCapabilities['cc_acp'] <- hector::ATMOSPHERIC_C()

# CONCENTRATIONS
# "Amospheric N2O"
globalCapabilities['c_an20'] = hector::ATMOSPHERIC_N2O()
# "Preindustrial Atmospheric CO2"
globalCapabilities['c_paco2'] = hector::PREINDUSTRIAL_CO2()
# "Preindustrial Ozone Concentration"
globalCapabilities['c_po'] = hector::PREINDUSTRIAL_O3()

# EMISSIONS
# "Black Carbon Emissions"
globalCapabilities['e_bc'] = hector::EMISSIONS_BC()
# "N20 Emissions"
globalCapabilities['e_n2o'] = hector::EMISSIONS_N2O()
# "NOx Emissions"
globalCapabilities['e_nox'] = hector::EMISSIONS_NOX()
# "CO Emissions"
globalCapabilities['e_co'] = hector::EMISSIONS_CO()
# "NMVOC Emissions"
globalCapabilities['e_nmvoc'] = hector::EMISSIONS_NMVOC()
# "Organic Carbon Emissions"
globalCapabilities['e_oc'] = hector::EMISSIONS_OC()

# FORCINGS
# "RF - Total"
globalCapabilities['f_rft'] = hector::RF_TOTAL()
# "RF - Albedo"
globalCapabilities['f_alb'] = hector::RF_T_ALBEDO()
# "RF - CO2"
globalCapabilities['f_co2'] = hector::RF_CO2()
# "RF - N2O"
globalCapabilities['f_n2o'] = hector::RF_N2O()
# "RF - H2O"
globalCapabilities['f_h2o'] = hector::RF_H2O()
# "RF - Ozone"
globalCapabilities['f_oz'] = hector::RF_O3()
# "RF - Black Carbon"
globalCapabilities['f_bc'] = hector::RF_BC()
# "RF - Organic Carbon"
globalCapabilities['f_oc'] = hector::RF_OC()
# "RF - SO2 Direct"
globalCapabilities['f_so2d'] = hector::RF_SO2D()
# "RF - SO2 Indirect"
globalCapabilities['f_so2i'] = hector::RF_SO2I()
# "RF - SO2 Total"
globalCapabilities['f_so2t'] = hector::RF_SO2()
# "RF - Volcanic Activity"
globalCapabilities['f_va'] = hector::RF_VOL()
# "RF - CH4"
globalCapabilities['f_ch4'] = hector::RF_CH4()

# HALOCARBON EMISSIONS
 # "CF4 Emissions"
globalCapabilities['he_cf4'] = hector::EMISSIONS_CF4()
# "C2F6 Emissions"
globalCapabilities['he_c2f6'] = hector::EMISSIONS_C2F6()
# "HFC-23 Emissions"
globalCapabilities['he_hfc23'] = hector::EMISSIONS_HFC23()
# "HFC-32 Emissions"
globalCapabilities['he_hfc32'] = hector::EMISSIONS_HFC32()
# "HFC-4310 Emissions"
globalCapabilities['he_hfc4310'] = hector::EMISSIONS_HFC4310()
# "HFC-125 Emissions"
globalCapabilities['he_hfc125'] = hector::EMISSIONS_HFC125()
# "HFC-134a Emissions"
globalCapabilities['he_hfc134a'] = hector::EMISSIONS_HFC134A()
# "HFC-143a Emissions"
globalCapabilities['he_hfc143a'] = hector::EMISSIONS_HFC143A()
# "HFC-227ea Emissions"
globalCapabilities['he_hfc227ea'] = hector::EMISSIONS_HFC227EA()
# "HFC-254fa Emissions"
globalCapabilities['he_245fa'] = hector::EMISSIONS_HFC245FA()
# "SF6 Emissions"'
globalCapabilities['he_sf6'] = hector::EMISSIONS_SF6()
# "CFC-11 Emissions"
globalCapabilities['he_cfc11'] = hector::EMISSIONS_CFC11()
# "CFC-12 Emissions"
globalCapabilities['he_cfc12'] = hector::EMISSIONS_CFC12()
# "CFC-113 Emissions"
globalCapabilities['he_cfc113'] = hector::EMISSIONS_CFC113()
# "CFC-114 Emissions"
globalCapabilities['he_cfc114'] = hector::EMISSIONS_CFC114()
# "CFC-115 Emissions"
globalCapabilities['he_cfc115'] = hector::EMISSIONS_CFC115()
# "CCl4 Emissions"
globalCapabilities['he_ccl4'] = hector::EMISSIONS_CCL4()
# "CH3CCl3 Emissions"
globalCapabilities['he_ch3ccl3'] = hector::EMISSIONS_CH3CCL3()
# "HCFC-22 Emissions"
globalCapabilities['he_hcfc22'] = hector::EMISSIONS_HCF22()
# "HCFC-141b Emissions"
globalCapabilities['he_hcfc141b'] = hector::EMISSIONS_HCF141B()
# "HCFC-142b Emissions"
globalCapabilities['he_hcfc142b'] = hector::EMISSIONS_HCF142B()
# "Halon-1211 Emissions"
globalCapabilities['he_halon1211'] = hector::EMISSIONS_HALON1211()
# "Halon-1301 Emissions"
globalCapabilities['he_halon1301'] = hector::EMISSIONS_HALON1301()
# "Halon-2402 Emissions"
globalCapabilities['he_halon2402'] = hector::EMISSIONS_HALON2402()
# "CH3Cl Emissions"
globalCapabilities['he_ch3cl'] = hector::EMISSIONS_CH3CL()
# "CH3Br Emissions"
globalCapabilities['he_ch3br'] = hector::EMISSIONS_CH3BR()

# HALOCARBON FORCINGS
# "CF4 Forcing"
globalCapabilities['hf_cf4'] = hector::RF_CF4()
# "C2F6 Forcing"
globalCapabilities['hf_c2f6'] = hector::RF_C2F6()
# "HFC-23 Forcing"
globalCapabilities['hf_hfc23'] = hector::RF_HFC23()
# "HFC-32 Forcing"
globalCapabilities['hf_hfc32'] = hector::RF_HFC32()
# "HFC-4310 Forcing"
globalCapabilities['hf_hfc4310'] = hector::RF_HFC4310()
# "HFC-125 Forcing"
globalCapabilities['hf_hfc125'] = hector::RF_HFC125()
# "HFC-134a Forcing"
globalCapabilities['hf_hfc134a'] = hector::RF_HFC134A()
# "HFC-143a Forcing"
globalCapabilities['hf_hfc143a'] = hector::RF_HFC143A()
# "HFC-227ea Forcing"
globalCapabilities['hf_hfc227ea'] = hector::RF_HFC227EA()
# "HFC-245fa Forcing"
globalCapabilities['hf_hfc245fa'] = hector::RF_HFC245FA()
# "SF6 Forcing"
globalCapabilities['hf_hfcsf6'] = hector::RF_SF6()
# "CFC-11 Forcing"
globalCapabilities['hf_cfc11'] = hector::RF_CFC11()
# "CFC-12 Forcing"
globalCapabilities['hf_cfc12'] = hector::RF_CFC12()
# "CFC-113 Forcing"
globalCapabilities['hf_cfc113'] = hector::RF_CFC113()
# "CFC-114 Forcing"
globalCapabilities['hf_cfc114'] = hector::RF_CFC114()
# "CFC-115 Forcing"
globalCapabilities['hf_cfc115'] = hector::RF_CFC115()
# "CCl4 Forcing"
globalCapabilities['hf_ccl4'] = hector::RF_CCL4()
# "CH3CCl3 Forcing"
globalCapabilities['hf_ch3ccl3'] = hector::RF_CH3CCL3()
# "HCFC-22 Forcing"
globalCapabilities['hf_hcfc22'] = hector::RF_HCF22()
# "HCFC-141b Forcing"
globalCapabilities['hf_hcfc141b'] = hector::RF_HCF141B()
# "HCFC-142b Forcing"
globalCapabilities['hf_hcfc142b'] = hector::RF_HCF142B()
# "Halon-1211 Forcing"
globalCapabilities['hf_halon1211'] = hector::RF_HALON1211()
# "Halon-1301 Forcing"
globalCapabilities['hf_halon1301'] = hector::RF_HALON1301()
# "Halon-2402 Forcing"
globalCapabilities['hf_halon2402'] = hector::RF_HALON2402()
# "CH3Cl Forcing"
globalCapabilities['hf_ch3cl'] = hector::RF_CH3CL()
# "CH3Br Forcing"
globalCapabilities['hf_ch3br'] = hector::RF_CH3BR()

# METHANE
# "Atmospheric CH4"
globalCapabilities['m_a_ch4'] = hector::ATMOSPHERIC_CH4()
# "Preindustrial Atmospheric CH4"
globalCapabilities['m_pa_ch4'] = hector::PREINDUSTRIAL_CH4()
# "Emissions CH4"
globalCapabilities['m_e_ch4'] = hector::EMISSIONS_CH4()
# "Natural CH4 Emissions"
globalCapabilities['m_n_ch4'] = hector::NATURAL_CH4()
# "Methane Loss - Soil"
globalCapabilities['m_soil_loss'] = hector::LIFETIME_SOIL()
# "Methane Loss - Straosphere"
globalCapabilities['m_strat_loss'] = hector::LIFETIME_STRAT()

# OCEAN
# "Ocean Carbon Flux"
globalCapabilities['o_cf'] = hector::OCEAN_CFLUX()
# "Ocean Total Carbon"
globalCapabilities['o_tc'] = hector::OCEAN_C()
# "Ocean Surface High-Lat Carbon"
globalCapabilities['o_os_hlc'] = hector::OCEAN_C_HL()
# "Ocean Surface Low-Lat Carbon"
globalCapabilities['o_os_llc'] = hector::OCEAN_C_LL()
# "Ocean Intermediate Carbon"
globalCapabilities['o_ic'] = hector::OCEAN_C_IO()
# "Ocean Deep Carbon"
globalCapabilities['o_dc'] = hector::OCEAN_C_DO()
# "Thermohaline Overturning"
globalCapabilities['o_to'] = hector::TT()
# "High-Lat Overturning"
globalCapabilities['o_hl_o'] = hector::TU()
# "Warm-Intermediate Exchange"
globalCapabilities['o_wie'] = hector::TWI()
# "Intermediate-Deep Exchange"
globalCapabilities['o_ide'] = hector::TID()
# "High Latitude Ph"
globalCapabilities['o_hl_ph'] = hector::PH_HL()
# "Low Latitude Ph"
globalCapabilities['o_ll_ph'] = hector::PH_LL()
# "Atmosphere-Ocean Flux - High Lat"
globalCapabilities['o_hl_aof'] = hector::ATM_OCEAN_FLUX_HL()
# "Atmosphere-Ocean Flux - Low Lat"
globalCapabilities['o_ll_aof'] = hector::ATM_OCEAN_FLUX_LL()
# "Partial Pressure CO2 - High Lat"
globalCapabilities['o_hl_pp_co2'] = hector::PCO2_HL()
# "Partial Pressure CO2 - Low Lat"
globalCapabilities['o_ll_pp_co2'] = hector::PCO2_LL()
# "Dissolved Inorganic C - High Lat"
globalCapabilities['o_hl_dic'] = hector::DIC_HL()
# "Dissolved Inorganic C - Low Lat"'
globalCapabilities['o_ll_dic'] = hector::DIC_LL()
# "Ocean Temperature - High Lat"
globalCapabilities['o_hl_t'] = hector::TEMP_HL()
# "Ocean Temperature - Low Lat"
globalCapabilities['o_ll_t'] = hector::TEMP_LL()
# "Carbonate Concentration - High Lat"
globalCapabilities['o_hl_cc'] = hector::CO3_HL()
# "Carbonate Concentration - Low Lat"
globalCapabilities['o_ll_cc'] = hector::CO3_LL()

# SO2
# "Natural SO2"
globalCapabilities['so2_n'] = hector::NATURAL_SO2()
# "Year 2000 SO2"
globalCapabilities['so2_y2k'] = hector::Y2000_SO2()
# "Anthropogenic SO2"
globalCapabilities['so2_a'] = hector::EMISSIONS_SO2()
# "Natural CH4 Emissions"
globalCapabilities['so2_n_ch4'] = hector::EMISSIONS_CH4()
# "Volcanic SO2"
globalCapabilities['so2_v'] = hector::VOLCANIC_SO2()

# TEMPERATURE
# "Global Mean Temp"
globalCapabilities['t_gmt'] = hector::GLOBAL_TEMP()
# "Equilibrium Global Temp"
globalCapabilities['t_egt'] = hector::GLOBAL_TEMPEQ()
# # "Ocean Surface Temp"
globalCapabilities['t_ost'] = hector::OCEAN_SURFACE_TEMP()
# "Ocean Air Temp"
globalCapabilities['t_oat'] = hector::OCEAN_AIR_TEMP()
# "Land Temp Anomaly"
globalCapabilities['t_lta'] = hector::LAND_TEMP()
# "Heat Flux - Mixed Layer Ocean"
globalCapabilities['t_hf_mlo'] = hector::FLUX_MIXED()
# "Heat Flux - Interior Layer Ocean"
globalCapabilities['t_hf_ilo'] = hector::FLUX_INTERIOR()
# "Total Heat Flux - Ocean"
globalCapabilities['t_hf_t'] = hector::HEAT_FLUX()
