library(MASS)
library(lmtest)
library(ggplot2)
library(sandwich)
library(lubridate)
library(tidyverse)
library(patchwork)
library(conflicted)


# Force R to use dplyr's version of these common conflicting functions
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

source("./R/custom_functions.R")


# Read Raw Data
saudi_cds_10y_raw <- read.csv("./data/SAGV10YUSAC.csv")
saudi_cds_10y_raw$Date <- dmy(saudi_cds_10y_raw$Date)
saudi_cds_10y_raw <- saudi_cds_10y_raw[, c("Date", "Price")]

aus_cds_10y_raw <- read.csv("./data/AUGV10YUSAR.csv")
aus_cds_10y_raw$Date <- mdy(aus_cds_10y_raw$Date)
aus_cds_10y <- aus_cds_10y_raw %>% select(Date, Price)


us_10y_raw <- read.csv(
  "./data/United-States-10-YearBondYieldHistoricalData.csv",
  header = TRUE
)
us_10y_raw$Date <- mdy(us_10y_raw$Date)
us_10y_raw <- us_10y_raw[, c("Date", "Price")]

brent <- readxl::read_xls("./data/eia_brent_spot.xls")
brent$Date <- ymd(brent$Date)
brent <- brent[, c("Date", "Brent_Spot")]

vix <- read.csv("./data/cboe_vix.csv")
vix <- vix[, c("Date", "Price")]
vix$Date <- mdy(vix$Date)


iron_ore <- read.csv("./data/iron_ore.csv")
iron_ore$Date <- mdy(iron_ore$Date)

# Process data
## CDS

saudi_cds_10y <- calc_price_delta(
  data = saudi_cds_10y_raw,
  date_col = "Date",
  price_col = "Price",
  output_col = "d_cds"
)

aus_cds_10y <- calc_price_delta(
  data = aus_cds_10y_raw,
  date_col = "Date",
  price_col = "Price",
  output_col = "d_cds"
)

aus_cds_10y <- aus_cds_10y %>% select(Date, d_cds)

## US 10 year yields
us_10y <- calc_price_delta(
  data = us_10y_raw,
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

vix <- calc_log_delta(vix, "Date", "Price", "d_lvix")
vix <- vix %>% select(Date, d_lvix)

## Iron ore
iron_ore <- calc_log_delta(iron_ore, "Date", "Price", "d_iron")
iron_ore <- iron_ore %>% select(Date, d_iron)
