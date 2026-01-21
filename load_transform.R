library(tidyverse); library(lubridate)

saudi_cds_10y_raw <- read.csv("./data/SAGV10YUSAC.csv")

saudi_cds_10y <- saudi_cds_10y[, c("Date", "Change..")]

saudi_cds_10y$Date <- dmy(saudi_cds_10y$Date)

names(saudi_cds_10y)[2] <- "Change"

saudi_cds_10y$Change <- as.numeric(gsub("%", "", saudi_cds_10y$Change))


# Day when swap agreement with China was announced
event <- ymd("2023-11-20")

# Create estimation data set (-1 year, 30 days)
est_data <- saudi_cds_10y %>%
  filter(Date >= ymd('2022-11-20') &
           Date <= (event - 30))

# And observation data
# Core: [-1,1]
obs_data_1 <- saudi_cds_10y %>%
  filter(Date >= event - days(1) & 
           Date <= event + days(1)) 

# Leak: [-3, 3]
obs_data_1 <- saudi_cds_10y %>%
  filter(Date >= event - days(3) & 
           Date <= event + days(3)) 

# Sanity Check: [-5, 5]
obs_data_1 <- saudi_cds_10y %>%
  filter(Date >= event - days(5) & 
           Date <= event + days(5)) 

m <- lm(
  d_cds ~ d_lvix + d_lbrent,
  data = estimation_sample
)

summary(m)



