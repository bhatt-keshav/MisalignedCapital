# Day when swap agreement with China was announced
event <- ymd("2023-11-20")
event_real <- ymd("2023-11-15")

# Filtering data based on needed dates
## Has both estimation and observation dataset
saudi_cds_10y <- saudi_cds_10y %>%
  filter(
    Date >= ymd('2022-11-20') &
      Date <= (event + days(7))
  ) %>%
  dplyr::select(Date, d_cds)

## Join all data
saudi_cds_10y <- saudi_cds_10y %>%
  left_join(us_10y, by = "Date") %>%
  left_join(vix, by = "Date") %>%
  left_join(brent, by = "Date")


# Create estimation data set (-1 year, 30 days)
cds_est_data <- saudi_cds_10y %>%
  filter(
    Date <= (event - 30)
  )


# Visualization shows, that the ΔCDS is practically flat
plot(cds_est_data$d_cds)

## Which can also be seen by regular harmonic motion, no real trend
plot(saudi_cds_10y$Date, saudi_cds_10y$Price)

# Do linear regression
m <- lm(
  d_cds ~ d_10y + d_lvix + d_lbrent,
  data = cds_est_data
)

summary(m)
plot(m)

## Checking the coefficients for
bptest(m)
coeftest(m, vcov = vcovHC(m, type = "HC1"))

# Do robust linear regression (RLM)
m_robust <- rlm(d_cds ~ d_10y + d_lvix + d_lbrent, data = cds_est_data)
summary(m_robust)

# OLS Plot vs Robust Linear Regression
plot_model_comparison(cds_est_data, m, m_robust)

#And observation data
## Core: [-1,1], we have to do -3 days because the announcement was on a Monday
obs_data_1 <- saudi_cds_10y %>%
  filter(
    Date >= event - days(3) &
      Date <= event + days(1)
  )

## Abnormal Returns plus minus 1 day
ar_1d_window <- calculate_ar_window(
  data = saudi_cds_10y,
  model = m_robust,
  start_date = event_real - days(1),
  end_date = event_real + days(1)
)


## Leak: [-3, 3]
ar_3d_window <- calculate_ar_window(
  data = saudi_cds_10y,
  model = m_robust,
  start_date = event_real - days(5),
  end_date = event_real + days(5)
)


## Sanity Check: [-5, 5]
ar_5d_window <- calculate_ar_window(
  data = saudi_cds_10y,
  model = m_robust,
  start_date = event_real - days(7),
  end_date = event_real + days(7)
)


# Graph the results
ggplot(obs_data_5, aes(x = Date, y = AR_risk)) +
  geom_line() +
  annotate(
    "rect",
    xmin = event - days(1),
    xmax = event + days(1),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = event - days(3),
    xmax = event + days(3),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2,
    fill = "purple"
  ) +
  geom_vline(aes(xintercept = event), linetype = 'dashed') +
  geom_hline(aes(yintercept = 0))
