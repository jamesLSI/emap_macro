source("data.R")

## filter to nordic + ireland for regression ####
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
  lm(percent_change_co2 ~ percent_change_debt_prop + percent_change_gdp + is_finland)

summary(model_output_percentage_debt_change)


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

### devt as % of GDP ####
for_model %>% 
  # filter(Code == "FIN") %>% 
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

### perccentage change debt ####
for_model %>% 
  filter(!Code == "IRL") %>%
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


