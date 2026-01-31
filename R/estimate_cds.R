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
## Report these not summary(m)!
coeftest(m, vcov = vcovHC(m, type = "HC1"))

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

# Create estimation data set (event -1 year [have], event - 30 days[done])
cds_est_data_aus <- aus_cds_10y %>%
  filter(
    Date <= (event - 30)
  )

cds_est_data_aus$Price <- NULL #don't need this col

# Visualization shows, that the ΔCDS is practically flat
plot(cds_est_data_aus$d_cds)

## Which can also be seen by regular harmonic motion
plot(aus_cds_10y_raw$Date, aus_cds_10y_raw$Price)

# Do linear regression
m_aus <- lm(
  d_cds ~ d_10y + d_lvix + d_iron,
  data = cds_est_data_aus
)

summary(m_aus)
plot(m_aus)

## Checking the coefficients for
bptest(m_aus) # have heteroskedasticity
coeftest(m_aus, vcov = vcovHC(m_aus, type = "HC1"))

# Do robust linear regression (RLM)
m_robust_aus <- rlm(
  d_cds ~ d_10y + d_lvix + d_iron,
  data = cds_est_data_aus
)
summary(m_robust_aus)


## Sanity Check: [-5, 5]
aus_ar_5d_window <- calculate_ar_window(
  data = aus_cds_10y,
  model = m_robust_aus,
  start_date = event_aus - days(7),
  end_date = event_aus + days(7)
)
