sedgwick_all_years <- read_csv("/capstone/firefutures/capstone_data/sedgwick_all.csv")

# Cleaning the data 
sedgwick_all_years_temp <- sedgwick_all %>%
  mutate(Date_Time = str_sub(Date_Time, 1, 10)) %>% # Getting rid of the time from the date column
  mutate(Date_Time = lubridate::mdy(Date_Time)) %>% # lubricating the date column
  select(Date_Time, air_temp_set_1) %>% # selecting only air temperature
  group_by(Date_Time) %>% # grouping by day 
  summarize(air_temp_max = max(air_temp_set_1),
            air_temp_min= min(air_temp_set_1)) %>% # Finding the maximum/minimum temperature per day
  rename(time = Date_Time) # renaming the air/time column so it can be joined with calAdapt easier


sum(is.na(sedgwick_all_years$relative_humidity_set_1))
