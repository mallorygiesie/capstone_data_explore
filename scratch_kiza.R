#This script reads in station data and cleans it 
kiza_files <-
  dir(
    "/capstone/firefutures/capstone_data/KIZA_data",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )
as.list(kiza_files)

read_in_station_data <- function(filepath) {
  name <- str_after_nth(filepath, '/', 5)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% as.data.frame() %>% 
    mutate(precip = coalesce(precip_accum_one_hour_set_1, 0), #Making precip NAs 0
           date = lubridate::mdy_hm(str_sub(Date_Time, 1, -4))) %>%  #Making date column with correct format
    group_by(date = lubridate::date(date)) %>% # grouping by year
    summarise(min_temp = min(air_temp_set_1), # making a min column with the coldest record of the day
              max_temp = max(air_temp_set_1), # and a min column with the warmest record of the day
              min_humidity = min(relative_humidity_set_1), # finding min humidity
              max_humidity = max(relative_humidity_set_1), # finding max humidity
              precip = sum(precip), # summing precipitation by day 
              wind_speed = mean(wind_speed_set_1)) # taking the wind speed from all the records of the day 
  assign(name, file,  envir = parent.frame())
} 

for (i in kiza_files) {
  read_in_station_data(i)
}

  
  


  