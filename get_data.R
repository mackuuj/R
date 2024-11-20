here::i_am("R/get_data.R")
library(data.table)
library(WDI)
library(dplyr)
library(tidyr)
library(eurostat)
library(here)
library(countrycode)
source(here("R/country_classification.R"))

get_wdi <- FALSE
download_eurostat_balance <- FALSE # If Eurostat data should be downloaded
tidy_eurostat_energy_balance <- TRUE # If raw data downloaded from Eurostat
# should be prepared into a tidy data set from scratch

# Get population data from World Bank
# Get energy balances from Eurostat
# Get footprints from EXIOBASE -> import from other project

# Green products? -> product codes
# Employment by sector and environmental footprint of sectors

# Population data from World Bank----------------
if (get_wdi){
  pop_data_raw <- WDI::WDI(
    country = countrycode(base_countries, "country.name", "iso2c"), 
    start = 1990, indicator = c(
      "population" = "SP.POP.TOTL"
      )
    )
  pop_data <- pop_data_raw %>% 
    select(iso3c, year, population) %>% 
    mutate(population = population / 1000)
  fwrite(pop_data, file = here("data/tidy/wdi_population_1000s.csv"))
} else{
  pop_data <- as_tibble(fread(here("data/tidy/wdi_population_1000s.csv")))
}

# Energy balances from Eurostat------------------

if (download_eurostat_balance){
  energy_balance_raw <- get_eurostat(
    id = "nrg_bal_c", time_format = "num", 
    stringsAsFactors = FALSE)
  setDT(energy_balance_raw)
  energy_balance_raw[,c("freq"):=NULL]
  saveRDS(
    object = energy_balance_raw, 
    file = here("data/raw/eurostat_energy-balance_nrg_bal_c.Rds"))
} 

if (tidy_eurostat_energy_balance){
  energy_balance_raw <- readRDS(here("data/raw/eurostat_energy-balance_nrg_bal_c.Rds"))
  
  energy_balance_raw <- energy_balance_raw[
    nrg_bal %in% c("PPRD", "IMP", "EXP", "FC_E") &
      unit %in% c("GWH") & 
      geo %in% countrycode(base_countries, "country.name", "eurostat") &
      siec %in% c("TOTAL") # CHECK
  ]
  
  energy_balance_raw <- energy_balance_raw[
    , .(nrg_bal, country=geo, year=TIME_PERIOD, values)]
  
  energy_balance_tidy <- as_tibble(energy_balance_raw) %>% 
    pivot_wider(names_from = "nrg_bal", values_from = "values") %>% 
    mutate(country=countrycode(country, "eurostat", "iso3c")) %>% 
    rename(
      Exports=EXP, Imports=IMP, 
      FinalEnergyConsumption=FC_E, 
      PrimaryEnergyProduction=PPRD
    ) %>% 
    mutate(NetTrade=Exports - Imports) %>% 
    select(country, year, PrimaryEnergyProduction, FinalEnergyConsumption, 
           Exports, Imports, NetTrade) %>% 
    mutate(
      validityTest = (PrimaryEnergyProduction-NetTrade)-(FinalEnergyConsumption))
  
  if (anyDuplicated(select(energy_balance_tidy, country, year)) == 0){
    print("No duplicates in energy balances")
  } else{
    warning("DUPLICATES IN ENERGY BALANCES!")
  }
  
  if(nrow(filter(energy_balance_tidy, validityTest<0)) == 0){
    print("Okay: Production - NetTrade larger than total consumption!")
  } else{
    warning("Production - NetTrade SMALLER than total consumption -> implausible!")
  }
  
  fwrite(
    x = select(energy_balance_tidy, -validityTest), 
    file = here("data/tidy/eurostat_energy-balance.csv"))
  energy_balance <- energy_balance_tidy
} else{
  energy_balance <- as_tibble(fread(here("data/tidy/eurostat_energy-balance.csv")))
}

# Data from EXIOBASE --------
# This data is prepared from the raw IO tables using the script:
#  TXNY-paper_gwp_balance.py
exiobase_base <- fread(here("data/tidy/TXNY_GWP_Trade.csv")) %>% 
  as_tibble(.) %>% 
  select(-V1) %>% 
  rename(country=Country) %>% 
  filter(!country %in% c("WA", "WE", "WL", "WM")) %>% # Remove RoW regions
  mutate(country=countrycode(country, "iso2c", "iso3c"))

# Green patents --------
# Definition of green patents: EPO tag for green patents
# SQL query can be found in: sql/get_green_patents.sql
green_patents <- fread(here("data/tidy/patstat_green-patents.csv")) %>%
  mutate(
    country=countrycode(country, "iso2c", "iso3c")) %>%
  rename(GreenPatents_n=n_patents) %>%
  as_tibble(.)

# TODO: Green products

# Merge data------

full_data_1 <- full_join(
  x = exiobase_base, y = energy_balance, 
  by = c("country", "year"))

full_data_2 <- left_join(
  x = full_data_1, y = pop_data, 
  by = c("country"="iso3c", "year")) %>% 
  rename(
    EnergyImports=Imports, 
    EnergyExports=Exports, 
    EnergyNetTrade=NetTrade
  ) %>%
  left_join(., y = green_patents, by = c("country", "year")) %>%
  mutate(GreenPatents_n = ifelse(is.na(GreenPatents_n), 0, GreenPatents_n)) %>%
  filter(country %in% countrycode(base_countries, "country.name", "iso3c"))

fwrite(full_data_2, file = here("data/tidy/full_taxonomy_data.csv"))
min(full_data_2$year)
max(full_data_2$year)
unique(full_data_2$country)
cat(countrycode(unique(full_data_2$country), "iso3c", "iso2c"), sep = "', '")
# Air emissions: env_ac_ainah_r2
