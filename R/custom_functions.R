# Functions

## Calculates delta of a column, can have price as levels
## Note: Column names must be passed as strings in quotes


calc_price_delta <- function(data, date_col, price_col, output_col) {
  # 1. Sort the data frame by the date column
  # We use data[[date_col]] to extract the column as a vector
  data <- data[order(data[[date_col]]), ]
  
  # 2. Calculate the difference (Current - Previous)
  # diff() calculates the change; we pad with NA to maintain row length
  delta_values <- c(NA, diff(data[[price_col]]))
  
  # 3. Assign the result to a new column
  data[[output_col]] <- delta_values
  
  return(data)
}

## Calculates log delta of a column

calc_log_delta <- function(data, date_col, price_col, output_col) {
  # 1. Ensure data is sorted by date
  data <- data[order(data[[date_col]]), ]
  
  # 2. Calculate log differences
  # diff() returns a vector of length n-1; prepend NA to keep dimensions consistent
  log_diffs <- c(NA, diff(log(data[[price_col]])))
  
  # 3. Assign to output column
  data[[output_col]] <- log_diffs
  
  return(data)
}

