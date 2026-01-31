# Set dates
## Day when swap agreement between China and Saudi was announced in the press
event <- ymd("2023-11-20")

# Filtering data based on needed dates one year ago from the event and plus 7 days
## Has both estimation and observation dataset
saudi_cds_10y <- saudi_cds_10y %>%
  filter(
    Date >= ymd('2022-11-20') &
      Date <= (event + days(7))
  ) %>%
  dplyr::select(Date, d_cds)

## Join all x variables needed for estimating CDS by a linear model
saudi_cds_10y <- saudi_cds_10y %>%
  left_join(us_10y, by = "Date") %>%
  left_join(vix, by = "Date") %>%
  left_join(brent, by = "Date")


# Create estimation data set (-1 year, 30 days)
saudi_cds_est_data <- saudi_cds_10y %>%
  filter(
    Date <= (event - days(30))
  )

# Visualization shows, that at its baseline ΔCDS is practically flat with some jumps
plot(saudi_cds_est_data$d_cds)

## Which can also be seen by regular harmonic motion, no real trend
plot(saudi_cds_10y_raw$Date, saudi_cds_10y_raw$Price)

# Do linear regression
m <- lm(
  d_cds ~ d_10y + d_lvix + d_lbrent,
  data = saudi_cds_est_data
)

summary(m)
plot(m)

## Test for heteroskedasticity
bptest(m) #none present

# Regression results with adjusted standard errors
## Report these!
coeftest(m, vcov = vcovHC(m, type = "HC1"))

# Do not do robust linear regression (RLM), we have results from BP test
# and HC1 test
# m_robust <- rlm(d_cds ~ d_10y + d_lvix + d_lbrent, data = saudi_cds_est_data)
# summary(m_robust)

# OLS Plot vs Robust Linear Regression
# plot_model_comparison(saudi_cds_est_data, m, m_robust)

########################AUSTRALIA######################
event_aus <- ymd("2012-03-22")

# Filtering data based on needed dates
## Has both estimation and observation dataset
aus_cds_10y <- aus_cds_10y %>%
  filter(
    Date >= (event_aus - years(1)) &
      Date <= (event_aus + days(7))
  )

## Join all data
aus_cds_10y <- aus_cds_10y %>%
  left_join(us_10y, by = "Date") %>%
  left_join(vix, by = "Date") %>%
  left_join(iron_ore, by = "Date")

# Inspecting
aus_cds_10y_ins <- aus_cds_10y_raw %>%
  filter(Date <= (event_aus + days(10)) & Date >= (event_aus - days(10)))
plot(aus_cds_10y_ins$Date, aus_cds_10y_ins$Price)


# Create estimation data set (event -1 year, event - 30 days)
cds_est_data_aus <- aus_cds_10y %>%
  filter(
    Date <= (event - 30)
  )

cds_est_data_aus$Price <- NULL #don't need this col

# Visualization shows, that the ΔCDS is practically flat
plot(cds_est_data_aus$d_cds)

## Which can also be seen by regular harmonic motion, no real trend
plot(aus_cds_10y_raw$Date, aus_cds_10y_raw$Price)

# Do linear regression
m_aus <- lm(
  d_cds ~ d_10y + d_lvix + d_iron,
  data = cds_est_data_aus
)

summary(m_aus)
plot(m_aus)

## Checking the coefficients for
bptest(m_aus)
coeftest(m_aus, vcov = vcovHC(m, type = "HC1"))

# Do robust linear regression (RLM)
m_robust_aus <- rlm(
  d_cds ~ d_10y + d_lvix + d_iron,
  data = cds_est_data_aus
)
summary(m_robust_aus)


#And observation data
## Core: [-1,1], we have to do -3 days because the announcement was on a Monday
aus_obs_data_5 <- aus_cds_10y %>%
  filter(
    Date >= event_aus - days(7) &
      Date <= event_aus + days(1)
  )

## Abnormal Returns plus minus 1 day
aus_ar_1d_window <- calculate_ar_window(
  data = aus_cds_10y,
  model = m_robust_aus,
  start_date = event_aus - days(1),
  end_date = event_aus + days(1)
)


## Leak: [-3, 3]
aus_ar_3d_window <- calculate_ar_window(
  data = aus_cds_10y,
  model = m_robust_aus,
  start_date = event_aus - days(3),
  end_date = event_aus + days(5)
)


## Sanity Check: [-5, 5]
aus_ar_5d_window <- calculate_ar_window(
  data = aus_cds_10y,
  model = m_robust_aus,
  start_date = event_aus - days(7),
  end_date = event_aus + days(7)
)


# Graph the results
ggplot(aus_obs_data_5, aes(x = Date, y = AR_risk)) +
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
