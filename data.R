library(tidyverse)
library(magrittr)

## read data, clear variable names ####
gdp <- read_csv("data_raw/national-gdp-wb.csv") %>% 
  rename(GDP_2017_Dollar = `GDP, PPP (constant 2017 international $)`)

debt <- read_csv("data_raw/gross-public-sector-debt-as-a-proportion-of-gdp.csv") %>% 
  rename(Debt_Percent_GDP = `17.13.1 - Gross public sector debt, Central Government, as a proportion of GDP (%) - DP_DOD_DLD2_CR_CG_Z1`) %>% 
  mutate(Debt_Percent_GDP = Debt_Percent_GDP/100)

co2 <- read_csv("data_raw/annual-co2-emissions-per-country.csv") %>% 
  rename(Annual_CO2 = `Annual CO₂ emissions`)

energy <- read_csv("data_raw/electricity-prod-source-stacked.csv") %>% 
  rename_with(~str_remove(., "Electricity from")) %>% 
  rename_with(~str_remove(., " - TWh")) %>% 
  rename_with(~str_remove(., "\\(")) %>% 
  rename_with(~str_remove(., "\\)")) %>% 
  rename_with(~str_remove(., "adapted for visualization of chart electricity-prod-source-stacked")) %>% 
  rename_with(~str_squish(.)) %>% 
  rename_with(~tools::toTitleCase(.)) %>% 
  rename_with(~str_replace_all(., " ", "_")) %>% 
  pivot_longer(4:ncol(.)) %>%
  ### calculate energy mix percentages ###
  group_by(Entity,
           Year) %>% 
  mutate(percent_fuel = value/sum(value,
                                  na.rm = T),
         percent_fuel= percent_fuel*100) %>% 
  ungroup() %>% 
  select(-value) %>% 
  pivot_wider(names_from = name,
              values_from = percent_fuel) %>% 
  ### combine carbon heavy fuels ###
  rowwise() %>% 
  mutate(carbon_heavy = sum(Oil,
                            Gas,
                            Coal,
                            na.rm = T)) %>% 
  ungroup()


## combine ####
### create combined table ####
gdp_debt_co2 <- gdp %>% 
  left_join(debt) %>% 
  left_join(co2) %>% 
  ### make nas 0 ####
mutate(Debt_Percent_GDP = na_if(Debt_Percent_GDP, 0)) %>% 
  ### calculate actual debt ####
mutate(Debt_Real = GDP_2017_Dollar * Debt_Percent_GDP) %>% 
  ### calculate annual percentage changes per measure per country ####
group_by(Code) %>% 
  mutate(percent_change_gdp = ((GDP_2017_Dollar - dplyr::lag(GDP_2017_Dollar,1))/dplyr::lag(GDP_2017_Dollar,1))*100,
         percent_change_debt_prop = ((Debt_Percent_GDP - dplyr::lag(Debt_Percent_GDP,1))/dplyr::lag(Debt_Percent_GDP,1))*100,
         percent_change_real_debt = ((Debt_Real - dplyr::lag(Debt_Real,1))/dplyr::lag(Debt_Real,1))*100,
         percent_change_co2 = ((Annual_CO2 - dplyr::lag(Annual_CO2,1))/dplyr::lag(Annual_CO2,1))*100) %>% 
  ungroup() %>% 
  left_join(energy)

### get 2012 data for base (first year of danish data) ####
data_2012 <- gdp_debt_co2 %>% 
  filter(Year == "2012") %>% 
  select(-contains("percent_change"),
         -Year) %>% 
  rename_with(~ paste(., "2012", sep = "_"),
              .cols = 3:ncol(.))

### calculate index(2012 = 100) for measures ####
gdp_debt_co2_2012_index <- gdp_debt_co2 %>% 
  left_join(data_2012) %>% 
  mutate(GDP_2017_Dollar_2012_index = GDP_2017_Dollar/GDP_2017_Dollar_2012,
         Debt_Percent_GDP_2012_index = Debt_Percent_GDP/Debt_Percent_GDP_2012,
         Annual_CO2_2012_index = Annual_CO2/Annual_CO2_2012,
         Debt_Real_2012_index = Debt_Real/Debt_Real_2012) %>% 
  select(-c(GDP_2017_Dollar_2012,
            Debt_Percent_GDP_2012,
            Annual_CO2_2012,
            Debt_Real_2012))

### finnish peat prices ####

peat_price_raw <- read_csv("data_raw/001_12gb_2023q3_20240117-164233.csv",
                           skip = 2) %>% 
  separate(`Domestic fuel`,
           into = c("first",
                    "second"),
           sep = ",") %>% 
  mutate(fuel = str_squish(first)) %>% 
  select(Quarter,
         fuel,
         price = `Price (eur/MWh)`,
         price_change_percent = `Price, year-on-year change (%)`)

peat_price <- peat_price_raw %>% 
  select(-price_change_percent) %>% 
  pivot_wider(names_from = fuel,
              values_from = price)

peat_price_annual_percent_change <- peat_price_raw %>% 
  select(-price) %>% 
  pivot_wider(names_from = fuel,
              values_from = price_change_percent)

rm(co2,
   debt,
   gdp,
   energy,
   data_2012,
   peat_price_raw)



