# Performing significance tests on AR
ar_all <- calculate_ar(saudi_cds_10y, m_robust)
sd_ar_all <- ar_all$AR_risk %>% na.omit() %>% sd()

# Compute the t-statistic for the days of interest
# t_event <- t_stat_event(obs_data_5, event_date = event, sd_ar_all)

t_event_real <- t_stat_event(ar_5d_window, event_date = event_real, sd_ar_all)
t_event_real

## Biggest positive spike
t_event_nov17 <- t_stat_event(
  obs_data_5,
  event_date = event - days(3),
  sd_ar_all
)
t_event_nov17

## Biggest negative spike
t_event_nov15 <- t_stat_event(
  obs_data_5,
  event_date = event - days(5),
  sd_ar_all
)
t_event_nov15

t_event_nov21 <- t_stat_event(
  obs_data_5,
  event_date = event + days(1),
  sd_ar_all
)
t_event_nov21

# Test cumulative daily returns
cumulative_ar_3d <- sum(ar_3d_window$AR_risk)
cumulative_ar_3d / (sqrt(dim(ar_3d_window)[1]) * sd_ar_all)

cumulative_ar_5d <- sum(ar_5d_window$AR_risk)
cumulative_ar_5d / (sqrt(dim(ar_5d_window)[1]) * sd_ar_all)
