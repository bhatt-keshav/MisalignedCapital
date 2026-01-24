# Day when swap agreement with China was announced
event <- ymd("2023-11-20")

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


# Visualisation shows, that the ΔCDS is practically flat
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


# 1. Basic Scatter Plot
plot(
  cds_est_data$d_lbrent,
  cds_est_data$d_cds,
  pch = 16,
  col = "gray",
  main = "Brent Returns vs. CDS Spread Change",
  xlab = "Brent Log Returns (d_lbrent)",
  ylab = "CDS Change (d_cds)"
)

# 2. Add the OLS Regression Line (Blue)
# We use coefficients 1 (Intercept) and 4 (d_lbrent) from your model
abline(a = coef(m)[1], b = coef(m)["d_lbrent"], col = "blue", lwd = 2)

# 3. Add the Robust Regression Line (Red)
abline(
  a = coef(m_robust)[1],
  b = coef(m_robust)["d_lbrent"],
  col = "red",
  lwd = 2
)

# 4. Add a Legend to distinguish the two
legend(
  "topright",
  legend = c("OLS Line", "Robust (RLM) Line"),
  col = c("blue", "red"),
  lwd = 2
)


#And observation data
## Core: [-1,1], we have to do -3 days because the announcement was on a Monday
obs_data_1 <- saudi_cds_10y %>%
  filter(
    Date >= event - days(3) &
      Date <= event + days(1)
  )

## Abnormal Returns
obs_data_1 <- calculate_ar(obs_data_1, m_robust)


## Leak: [-3, 3]
obs_data_3 <- saudi_cds_10y %>%
  filter(
    Date >= event - days(5) &
      Date <= event + days(3)
  )

obs_data_3 <- calculate_ar(obs_data_3, m_robust)


## Sanity Check: [-5, 5]
obs_data_5 <- saudi_cds_10y %>%
  filter(
    Date >= event - days(8) &
      Date <= event + days(7)
  )

obs_data_5 <- calculate_ar(obs_data_5, m_robust)
# obs_data_5 %>% select(Date, AR_risk)

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
