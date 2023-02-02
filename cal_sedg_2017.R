source("cleaning_cal_adapt.R")
sedgwick_2017_today <- read_csv("/capstone/firefutures/capstone_data/sbco_raws_daily_070117_073122.csv")

MIROC5_rcp45_2017 <- MIROC5_rcp45 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)
MIROC5_rcp85_2017 <- MIROC5_rcp85 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)


CanESM2_rcp85_2017 <- CanESM2_rcp85 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)
CanESM2_rcp45_2017 <- CanESM2_rcp45 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)


HadGEM2ES_rcp45_2017 <- HadGEM2ES_rcp45 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)
HadGEM2ES_rcp85_2017 <- HadGEM2ES_rcp45 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)


CNRM_CM5_rcp85_2017 <- CNRM_CM5_rcp85 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)
CNRM_CM5_rcp45_2017 <- CNRM_CM5_rcp45 %>% filter(lubridate::year(time) >= 2017 & lubridate::year(time) < 2023)

