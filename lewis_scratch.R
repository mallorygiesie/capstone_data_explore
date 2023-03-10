library(stringr)
library(tidyverse)
library(strex)
library(reshape2)
library(TSdist)
library(dtw)


####################################################################
min_temp_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/min_temp_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )
as.list(min_temp_files)

read_in_min <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('min_1' = '0', 'min_2' = '1') %>% 
    mutate(min_1 = (min_1 - 273) * 9/5 + 32,
           min_2 = (min_2 - 273) * 9/5 + 32)
  assign(name, file,  envir = parent.frame())
} 

for (i in min_temp_files) {
  read_in_min(i)
}
#####################################################################
max_temp_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/max_temp_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )
as.list(max_temp_files)

read_in_max <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('max_temp_1' = '0', 'max_temp_2' = '1') %>% 
    mutate(max_temp_1 = (max_temp_1 - 273) * 9/5 + 32,
           max_temp_2 = (max_temp_2 - 273) * 9/5 + 32)
  assign(name, file,  envir = parent.frame())
} 

for (i in max_temp_files) {
  read_in_max(i)
}
##############################################################
min_humidity_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/min_humidity_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(min_humidity_files)

read_in_min_humidity <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('min_humidity_1' = '0', 'min_humidity_2' = '1') 
  assign(name, file,  envir = parent.frame())
} 

for (i in min_humidity_files) {
  read_in_min_humidity(i)
}
########################################################################
max_humidity_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/max_humidity_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(max_humidity_files)

read_in_max_humidity <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('max_humidity_1' = '0', 'max_humidity_2' = '1') 
  assign(name, file,  envir = parent.frame())
} 

for (i in max_humidity_files) {
  read_in_max_humidity(i)
}
########################################################################
precip_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/precip_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(precip_files)

read_in_precip <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('precip_1' = '0', 'precip_2' = '1') 
  assign(name, file,  envir = parent.frame())
} 

for (i in precip_files) {
  read_in_precip(i)
}

########################################################################
wind_files <-
  dir(
    "/capstone/firefutures/capstone_data/cal_adapt_all/wind_all",
    recursive = TRUE,
    full.names = TRUE,
    pattern = "\\.csv$"
  )

as.list(wind_files)

read_in_wind <- function(filepath) {
  name <- str_after_nth(filepath, '/', 6)
  name <- str_before_last(name, '.csv')
  file <- read_csv(filepath, show_col_types = FALSE) %>% 
    rename('wind_1' = '0', 'wind_2' = '1') 
  assign(name, file,  envir = parent.frame())
} 

for (i in wind_files) {
  read_in_wind(i)
}

########################################################
pattern_vals <- grep('MIROC5_rcp4',names(.GlobalEnv),value=TRUE)

pattern_list <-do.call("list",mget(pattern_vals))

test <- pattern_list %>% 
  reduce(left_join, by = "time")

model_scenario_join <- function(pattern_string) {
  pattern_vals <- grep(pattern_string ,names(.GlobalEnv),value=TRUE)
  pattern_list <- do.call("list",mget(pattern_vals, inherits = TRUE))
  test <- pattern_list %>% 
    reduce(left_join, by = "time")
}

test_lewis <- model_scenario_join('CNRM-CM5_historical')
