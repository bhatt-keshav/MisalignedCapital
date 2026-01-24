library(MASS)
library(lmtest)
library(sandwich)
library(tidyverse)
library(lubridate)
library(conflicted)
library(patchwork)


# Force R to use dplyr's version of these common conflicting functions
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source("R/custom_functions.R")

saudi_cds_10y <- calc_cds_delta(saudi_cds_10y, "Date", "Price")


# Read Raw Data
saudi_cds_10y_raw <- read.csv("./data/SAGV10YUSAC.csv")

us_10y_raw <- read.csv(
  "./data/United-States-10-YearBondYieldHistoricalData.csv",
  header = TRUE
)

brent <- readxl::read_xls("./data/eia_brent_spot.xls")
vix <- read.csv("./data/cboe_vix.csv")


# Process data
## CDS
saudi_cds_10y <- saudi_cds_10y_raw[, c("Date", "Price")]
saudi_cds_10y$Date <- dmy(saudi_cds_10y$Date)

saudi_cds_10y <- calc_price_delta(
  data = saudi_cds_10y,
  date_col = "Date",
  price_col = "Price",
  output_col = "d_cds"
)


## US 10 year yields
us_10y_raw$Date <- mdy(us_10y_raw$Date)
us_10y <- us_10y_raw[, c("Date", "Price")]

us_10y <- calc_price_delta(
  data = us_10y,
  date_col = "Date",
  price_col = "Price",
  output_col = "d_10y"
)
us_10y <- us_10y %>% select(Date, d_10y)

## Brent
brent <- calc_log_delta(brent, "Date", "Brent_Spot", "d_lbrent")
brent <- brent %>% select(Date, d_lbrent)

## VIX
vix <- vix[, c("Date", "Price")]
vix$Date <- mdy(vix$Date)

vix <- calc_log_delta(vix, "Date", "Price", "d_lvix")
vix <- vix %>% select(Date, d_lvix)
