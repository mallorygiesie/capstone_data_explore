# packages: nortest

# CalAdapt data: Name: Model_Scenario
# 4 models, 3 scenarios, 12 files


# Group by month to summarise variable data
monthlytemp <- CanESM2_rcp45 %>% 
  # Month and Year
  group_by(month = lubridate::month(time),
           year = lubridate::year(time)) %>% 
  # LOOP through each column, summarize min/max/avg, sum of rain?
  summarise_all(list(min, max, mean))


# Group by month to view total precip
monthlyprecip <- CanESM2_rcp45 %>% 
  # Month and Year
  group_by(month = lubridate::month(time),
           year = lubridate::year(time)) %>% 
  # LOOP through each column, summarize min/max/avg, sum of rain?
  summarise(total_precip = sum(precip_1))

# Function to view a select month (all Januarys) of a select climate model
monthlytempfunc <- function(pick_model, month_number) {
# Group by month for temperature range
monthlytemp <- pick_model %>% 
  # Month and Year
  group_by(month = lubridate::month(time),
           year = lubridate::year(time)) %>% 
  select(min_1, max_temp_1) %>% 
  # LOOP through each column, summarize min/max/avg, sum of rain?
  summarise(min = min(min_1),
            max = max(max_temp_1)) %>% 
  filter(month == month_number)
# view monthly graph, automate as function
ggplot(monthlytemp, aes(x = year)) +
  geom_line(aes(y = min)) +
  geom_line(aes(y = max)) +
  labs(y = "temperature") +
  theme_minimal()
}

# View options
# Table of max/min/average
# Display as graph
# Select which January

# Save in new dataframe