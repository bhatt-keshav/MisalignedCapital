library(tidyverse)
library(lubridate)
library(vroom)

saudi_cds_10y_raw <- read.csv("./data/SAGV10YUSAC.csv")

saudi_cds_10y <- saudi_cds_10y_raw[, c("Date", "Change")]

saudi_cds_10y$Date <- dmy(saudi_cds_10y$Date)

saudi_cds_10y$Change <- as.numeric(gsub("%", "", saudi_cds_10y$Change))


## Day when swap agreement with China was announced
event <- ymd("2023-11-20")

## Create estimation data set (-1 year, 30 days)
cds_est_data <- saudi_cds_10y %>%
  filter(Date >= ymd('2022-11-20') &
           Date <= (event - 30))

## And observation data
### Core: [-1,1], we have to do -3 days because the announcement was on a Monday
obs_data_1 <- saudi_cds_10y %>%
  filter(Date >= event - days(3) & 
           Date <= event + days(1)) 

### Leak: [-3, 3]
obs_data_3 <- saudi_cds_10y %>%
  filter(Date >= event - days(5) & 
           Date <= event + days(3)) 

### Sanity Check: [-5, 5]
obs_data_5 <- saudi_cds_10y %>%
  filter(Date >= event - days(8) & 
           Date <= event + days(7)) 


# US 10 year yields
us_10y_raw <- read.csv("./data/United-States-10-YearBondYieldHistoricalData.csv", 
                       header=TRUE, 
                       colClasses = "character")


us_10y_raw$Change <- as.numeric(gsub("%", "", us_10y_raw$Change))
                                

us_10y_raw$Date <- mdy(us_10y_raw$Date) 

## Create estimation data set (-1 year, 30 days)
us_10y_est_data <- us_10y_raw %>%
  filter(Date >= ymd('2022-11-20') &
           Date <= (event - 30))
