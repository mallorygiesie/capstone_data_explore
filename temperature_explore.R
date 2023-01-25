library(stringr)
library(tidyverse)
library(strex)
library(reshape2)

# DATA EXPLORATION 2005 Cal-Adapt 4 priority models (2 grid cells) with Sedgwick station data

# Finding all .csv files
files <-
  dir(
    "/capstone/firefutures/cal_adapt_hist",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )
as.list(files)


# Function to read in all files and name them by their model name (after the 5th dash in the filepath
#and excluding .csv)
read_in <- function(filepath) {
  name <- str_after_nth(filepath, '/', 5)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE)
  assign(name, file,  envir = parent.frame())
}
# Reading through list of file paths and applying the function defined above
for (i in files) {
  read_in(i)
}

# Reading in Sedgwick 2005 data
sedgwick_2005 <- read_csv("/capstone/firefutures/sedgwick_data/sedgwick_2005.csv")

# Cleaning the data 
sedgwick_2005_temp <- sedgwick_2005 %>%
  mutate(Date_Time = str_sub(Date_Time, 1, 10)) %>% # Getting rid of the time from the date column
  mutate(Date_Time = lubridate::mdy(Date_Time)) %>% # lubricating the date column
  select(Date_Time, air_temp_set_1) %>% # selecting only air temperature
  group_by(Date_Time) %>% # grouping by day 
  summarize(air_temp_set_1 = mean(air_temp_set_1)) %>% # taking the mean of the air temps each day
  rename(sedg_temp = air_temp_set_1, time = Date_Time) # renaming the air/time column so it can be joined with calAdapt easier

temp_MIROC5 <- tasmax_day_MIROC5_historical %>% 
  rename('max_1' = '0', 'max_2' = '1') %>% # renaming to max_temp 1 and 2 (which represent grid cells of SB)
  left_join(tasmin_day_MIROC5_historical, by = 'time') %>% # joining min temp data with max temp
  rename('min_1' = '0', 'min_2' = '1') %>% # renaming min temp to follow max temp convention 
  mutate(max_1 = (max_1 - 273) * 9/5 + 32,
         max_2 = (max_2 - 273) * 9/5 + 32,
         min_1 = (min_1 - 273) * 9/5 + 32,
         min_2 = (min_2 -273) * 9/5 + 32) %>% # converting kelvin into Fahrenheit
  filter(time >= as.Date("2004-12-31") & time <= as.Date(	
    "2005-12-30")) %>% # choosing only 2005 (same dates as Sedgwick data)
  mutate(temp_mean_1_MIROC5 = ((max_1 + min_1) / 2), 
         temp_mean_2_MIROC5 = ((max_2 + min_2)/ 2)) %>% # getting the mean temp of min and max
  # not sure if taking the mean of the min and max is best to compare with the mean averages of 
  # Sedgwick which is data collected every 5 minutes and I averaged above
  select(time, temp_mean_1_MIROC5, temp_mean_2_MIROC5) # selecting only the avg temp for the two grid cells 


# CLEANING THE REMAINING 3 PRIORITY MODELS (SAME PROCESS AS ABOVE. Create function next time)  
#############################################################################################
temp_HadGEM2ES <- `tasmax_day_HadGEM2-ES_historical` %>% 
  rename('max_1' = '0', 'max_2' = '1') %>%
  left_join(`tasmin_day_HadGEM2-ES_historical`, by = 'time') %>% 
  rename('min_1' = '0', 'min_2' = '1') %>% 
  mutate(max_1 = (max_1 - 273) * 9/5 + 32,
         max_2 = (max_2 - 273) * 9/5 + 32,
         min_1 = (min_1 - 273) * 9/5 + 32,
         min_2 = (min_2 -273) * 9/5 + 32) %>% 
  filter(time >= as.Date("2004-12-31") & time <= as.Date(	
    "2005-12-30")) %>% 
  mutate(temp_mean_1_GEM2ES = ((max_1 + min_1) / 2), temp_mean_2_GEM2ES = ((max_2 + min_2)/ 2)) %>% 
  select(time, temp_mean_1_GEM2ES, temp_mean_2_GEM2ES)

temp_CNRCM5 <- `tasmax_day_CNRM-CM5_historical` %>% 
  rename('max_1' = '0', 'max_2' = '1') %>%
  left_join(`tasmin_day_CNRM-CM5_historical`, by = 'time') %>% 
  rename('min_1' = '0', 'min_2' = '1') %>% 
  mutate(max_1 = (max_1 - 273) * 9/5 + 32,
         max_2 = (max_2 - 273) * 9/5 + 32,
         min_1 = (min_1 - 273) * 9/5 + 32,
         min_2 = (min_2 -273) * 9/5 + 32) %>% 
  filter(time >= as.Date("2004-12-31") & time <= as.Date(	
    "2005-12-30")) %>% 
  mutate(temp_mean_1_CNRCM5 = ((max_1 + min_1) / 2), temp_mean_2_CNRCM5 = ((max_2 + min_2)/ 2))  %>% 
  select(time, temp_mean_1_CNRCM5, temp_mean_2_CNRCM5)

temp_CanESM2 <- tasmax_day_CanESM2_historical %>% 
  rename('max_1' = '0', 'max_2' = '1') %>%
  left_join(tasmin_day_CanESM2_historical, by = 'time') %>% 
  rename('min_1' = '0', 'min_2' = '1') %>% 
  mutate(max_1 = (max_1 - 273) * 9/5 + 32,
         max_2 = (max_2 - 273) * 9/5 + 32,
         min_1 = (min_1 - 273) * 9/5 + 32,
         min_2 = (min_2 -273) * 9/5 + 32) %>% 
  filter(time >= as.Date("2004-12-31") & time <= as.Date(	
    "2005-12-30")) %>% 
  mutate(temp_mean_1_ESM2 = ((max_1 + min_1) / 2), temp_mean_2_ESM2 = ((max_2 + min_2)/ 2)) %>% 
  select(time, temp_mean_1_ESM2, temp_mean_2_ESM2)
#########################################################
#Joining the 4 priority models with Sedgwick station data 
all_temp_mods_2005 <- temp_CanESM2 %>% left_join(temp_CNRCM5, by = 'time') %>% 
  left_join(temp_HadGEM2ES, by = 'time') %>%
  left_join(temp_MIROC5, by = 'time') %>% 
  left_join(sedgwick_2005_temp, by = 'time')

ggplot(all_temp_mods_2005, aes(x = time, y = seq(45:75, by = 1))) +
  geom_line(mapping = aes(y = sedg_temp, color = "SEDGWICK")) +
  geom_line(mapping = aes(y = temp_mean_1_CNRCM5, color = "CNRCM5")) 


