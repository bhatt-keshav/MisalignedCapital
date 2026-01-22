library(tidyverse)
library(lubridate)
library(vroom)

# Functions
# TODO: Move outside
calc_price_delta <- function(data, date_col, price_col) {
  # 1. Sort the data frame by the date column
  # We use data[[date_col]] to extract the column as a vector
  data <- data[order(data[[date_col]]), ]
    
  # 2. Calculate the difference (Current - Previous)
  # diff() calculates the change; we pad with NA to maintain   row length
  delta_values <- c(NA, diff(data[[price_col]]))
    
  # 3. Assign the result to a new column
  data[["d_cds"]] <- delta_values
    
  return(data)
}

# Usage:
# Note: Column names must be passed as strings in quotes
saudi_cds_10y <- calc_cds_delta(saudi_cds_10y, "Date", "Price")


# Usage:
saudi_cds_10y <- calc_cds_delta(saudi_cds_10y)

# Read Raw Data
saudi_cds_10y_raw <- read.csv("./data/SAGV10YUSAC.csv")
us_10y_raw <- read.csv("./data/United-States-10-YearBondYieldHistoricalData.csv", 
                       header=TRUE)


## CDS
saudi_cds_10y <- saudi_cds_10y_raw[, c("Date", "Price")]

saudi_cds_10y$Date <- dmy(saudi_cds_10y$Date)

saudi_cds_10y <- calc_price_delta(data = saudi_cds_10y, date_col = "Date", price_col = "Price")



## US 10 year yields

us_10y_raw$Date <- mdy(us_10y_raw$Date) 
us_10y <- us_10y_raw[, c("Date", "Price")]


us_10y <- calc_price_delta(data = us_10y, date_col = "Date", price_col = "Price")






