
for_model_no_covid <- for_model %>% 
  filter(!Year == 2020)

rdd_co2 <- model_output_co2_debt <- for_model %>%  
  # filter(!Code == "IRL") %>% 
  filter(Code == "FIN") %$%
  # filter() %$%
  # lm(Annual_CO2_2012_index ~ Debt_Real_2012_index) %$%
  lm(Annual_CO2_2012_index ~ threshold + I(time - dmy("01-01-2019")) + threshold:I(time - dmy("01-01-2019")))

summary(rdd_co2)     

for_model %>%  
  # filter(!Code == "IRL") %>% 
  filter(Code == "FIN") %>%
  mutate(threshold = as.factor(threshold)) %>% 
  ggplot(aes(x = time, y = Annual_CO2_2012_index, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(title = "Annual CO2 indexed at 2012",
       subtitle = "RDD from 2019 policy launch",
       y = "",
       x = "")


###############


rdd_debt <- model_output_co2_debt <- for_model %>%  
  # filter(!Code == "IRL") %>% 
  filter(Code == "FIN") %$%
  # filter() %$%
  # lm(Annual_CO2_2012_index ~ Debt_Real_2012_index) %$%
  lm(Debt_Real_2012_index ~ threshold + I(time - dmy("01-01-2019")) + threshold:I(time - dmy("01-01-2019")))

summary(rdd_debt)     

for_model %>%  
  filter(Year > 2007) %>% 
  # filter(!Code == "IRL") %>% 
  filter(Code == "FIN") %>%
  mutate(threshold = as.factor(threshold)) %>% 
  ggplot(aes(x = time, y = percent_change_real_debt, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 21, color = "red",
             size = 1, linetype = "dashed") +
  labs(title = "Annual % change of real debt",
       subtitle = "RDD from 2019 policy launch",
       y = "",
       x = "")

###############

rdd_co2_debt <- model_output_co2_debt <- for_model %>%  
  # filter(!Code == "IRL") %>% 
  filter(Code == "FIN") %$%
  # filter() %$%
  # lm(Annual_CO2_2012_index ~ Debt_Real_2012_index) %$%
  lm(Annual_CO2_2012_index ~ Debt_Real_2012_index + threshold + I(time - dmy("01-01-2019")) + threshold:I(time - dmy("01-01-2019")))

summary(rdd_co2_debt)     

plot <- for_model_no_covid %>%  
  filter(Year > 2007) %>% 
  # filter(!Code == "IRL") %>% 
  filter(Code == "FIN") %>%
  mutate(threshold = as.factor(threshold)) %>% 
  ggplot(aes(x = Annual_CO2_2012_index, y = Debt_Real_2012_index, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  # geom_vline(xintercept = 21, color = "red",
  #            size = 1, linetype = "dashed") +
  labs(title = "Annual CO2 and Debt Indexed at 2012",
       subtitle = "RDD from 2019 policy launch",
       y = "",
       x = "")


plotly::ggplotly(plot)
