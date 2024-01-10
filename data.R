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
  ungroup()

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
