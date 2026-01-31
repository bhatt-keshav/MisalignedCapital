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

plot_model_comparison <- function(data, ols_model, robust_model) {
  # 1. Basic Scatter Plot
  # We use the column names directly to keep it consistent with your models
  plot(
    data$d_lbrent,
    data$d_cds,
    pch = 16,
    col = "gray",
    main = "Brent Returns vs. CDS Spread Change",
    xlab = "Brent Log Returns (d_lbrent)",
    ylab = "CDS Change (d_cds)"
  )

  # 2. Add the OLS Regression Line (Blue)
  # Using indexing to ensure we grab the correct coefficients
  abline(
    a = coef(ols_model)[1],
    b = coef(ols_model)["d_lbrent"],
    col = "blue",
    lwd = 2
  )

  # 3. Add the Robust Regression Line (Red)
  abline(
    a = coef(robust_model)[1],
    b = coef(robust_model)["d_lbrent"],
    col = "red",
    lwd = 2
  )

  # 4. Add a Legend
  legend(
    "topright",
    legend = c("OLS Line", "Robust (RLM) Line"),
    col = c("blue", "red"),
    lwd = 2,
    bg = "white" # Added background to make it readable over the dots
  )
}

calculate_ar <- function(data, model) {
  data %>%
    mutate(
      risk_predict = predict(model, newdata = .), # '.' is the data passed to the function
      AR_risk = d_cds - risk_predict
    )
}

# Calculates Abnormal Return for the chosen observation time window
## Assumes column names Date and d_cds
calculate_ar_window <- function(data, model, start_date, end_date) {
  data %>%
    # 1. Filter the data for the specific window
    filter(Date >= start_date & Date <= end_date) %>%
    # 2. Calculate predictions and AR
    mutate(
      risk_predict = predict(model, newdata = .),
      AR_risk = d_cds - risk_predict
    )
}

t_stat_event <- function(data, event_date, sd_benchmark) {
  # 1. Subset the AR_risk for the specific date using [row, col]
  # We use drop = TRUE to ensure it returns a single number, not a data frame
  ar_value <- data[data$Date == event_date, "AR_risk", drop = TRUE]

  # 2. Calculate the t-statistic
  t_stat <- ar_value / sd_benchmark

  return(t_stat)
}

calc_t_stat <- function(ar_vector, sd_benchmark) {
  return(abs(ar_vector / sd_benchmark))
}
