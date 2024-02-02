source("data.R")

# filter to nordic + ireland for regression ####
for_model <- gdp_debt_co2_2012_index %>% 
  filter(Code %in% c("FIN",
                     "SWE",
                     "NOR",   
                     "DNK",
                     "ISL",
                     "IRL")) %>% 
  filter(!is.na(Debt_Real)) %>% 
  mutate(is_finland = if_else(Code == "FIN",
                              1,
                              0))

## models ####
### lm annual c0 to carbon heavy fuel ####
model_output_co2_carbon_h_fuel <- for_model %>%  
  # filter(!Code == "IRL") %>% 
  # filter(Code == "FIN") %$% 
  filter() %$%
  lm(Annual_CO2 ~ carbon_heavy)

summary(model_output_co2_carbon_h_fuel)

summary_table <- summary(model_output_co2_carbon_h_fuel)

lm_summary_table_function(summary_table)

### lm annual CO2 to Real Debt (indexed 2012 = 100) ####
model_output_co2_debt <- for_model %>%  
  # filter(!Code == "IRL") %>% 
  # filter(Code == "FIN") %$% 
  filter() %$%
  lm(Annual_CO2_2012_index ~ Debt_Real_2012_index)

summary(model_output_co2_debt)

### percentage change co2 to others ####
model_output_percentage_debt_change <- for_model %>%  
  # filter(!Code == "IRL") %>% 
  # filter(Code == "FIN") %$% 
  filter() %$%
  lm(percent_change_co2 ~ percent_change_debt_prop + percent_change_gdp + percent_change_carbon_heavy + is_finland)

summary(model_output_percentage_debt_change)

summary_table <- summary(model_output_percentage_debt_change)

lm_summary_table_function(summary_table)

## plots ####
### Annual Co2 emissions indexed (2012 = 100) ####
for_model %>% 
  # filter(Code == "FIN") %>%
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = Year, y = Annual_CO2_2012_index, color = Code)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  # guides(color = FALSE) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "Annual CO2 Emissions Index (2012 = 100)",
       y = "",
       x = "")

### debt as % of GDP ####
for_model %>% 
  filter(Code == "FIN") %>%
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = Year, y = Debt_Percent_GDP, color = Code)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  # guides(color = FALSE) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "Debt % of GDP",
       y = "",
       x = "")

### percentage change debt ####
for_model %>% 
  filter(!Code == "IRL") %>%
  filter(Code == "FIN") %>%
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = Year, y = percent_change_debt_prop, color = Code)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  # guides(color = FALSE) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "Annual Debt % Change",
       y = "",
       x = "")

### percentage high carbon fuels ####
for_model %>% 
  filter(Code == "FIN") %>%
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = Year, y = carbon_heavy, color = Code)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  # guides(color = FALSE) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "Carbon Heavy Fuels (% of all fue use)",
       y = "",
       x = "")

### percentage change in price of peat ####
peat_price %>%
  # filter(Code == "FIN") %>%
  # mutate(values = as.factor(values)) %>% 
  ggplot(aes(x = Quarter, y = `Milled peat delivered (excl. excise duty)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  # guides(color = FALSE) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "Peat Price including excise duty",
       y = "",
       x = "")

#####################

