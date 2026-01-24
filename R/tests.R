# Performing significance tests on AR
ar_all <- calculate_ar(saudi_cds_10y, m_robust)
sd_ar_all <- ar_all$AR_risk %>% na.omit() %>% sd()

# Compute the t-statistic for the day of interest
t_event <- t_stat_event(obs_data_5, event_date = event, sd_ar_all)

# Biggest positive spike
t_event_nov17 <- t_stat_event(
  obs_data_5,
  event_date = event - days(3),
  sd_ar_all
)

t_event_nov17

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
