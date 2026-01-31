########## SAUDI ARABIA ###########
# Performing significance tests on AR
ar_all <- calculate_ar(saudi_cds_10y, m)
sd_ar_all <- ar_all$AR_risk %>% na.omit() %>% sd()

# Sanity Check window: [-5, 5]
ar_5d_window <- calculate_ar_window(
  data = saudi_cds_10y,
  model = m,
  start_date = event_real - days(7),
  end_date = event_real + days(7)
)

# Calculating t-stat for Abnormal Returns in this ±5 day window
ar_5d_window <- ar_5d_window %>%
  mutate(t_stat = calc_t_stat(AR_risk, sd_benchmark = sd_ar_all))

# It shows maximum t-stat is at 15 November = real event actually!
## This is the event of real significance
## Segmented regression will be based on this date
event_real <- ymd("2023-11-15")

# T-Test cumulative daily returns
cumulative_ar_3d <- sum(ar_3d_window$AR_risk)
cumulative_ar_3d / (sqrt(dim(ar_3d_window)[1]) * sd_ar_all)

cumulative_ar_5d <- sum(ar_5d_window$AR_risk)
cumulative_ar_5d / (sqrt(dim(ar_5d_window)[1]) * sd_ar_all) #significant!


# Graph the of AR around the real event
ggplot(ar_5d_window, aes(x = Date, y = AR_risk)) +
  geom_line() +
  annotate(
    "rect",
    xmin = event_real - days(1),
    xmax = event_real + days(1),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = event_real - days(3),
    xmax = event_real + days(3),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2,
    fill = "purple"
  ) +
  geom_vline(aes(xintercept = event_real), linetype = 'dashed') +
  geom_vline(aes(xintercept = event), linetype = 1) +
  geom_hline(aes(yintercept = 0))


################################# AUSTRALIA #######################
# Performing significance tests on AR
ar_all_aus <- calculate_ar(aus_cds_10y, m_robust_aus)
sd_ar_all_aus <- ar_all_aus$AR_risk %>% na.omit() %>% sd()

# Compute the t-statistic for 5 days around announcement date

for (i in seq_along(aus_ar_5d_window$Date)) {
  print(aus_ar_5d_window$Date[i])
  t_stat_event(
    aus_ar_5d_window,
    event_date = aus_ar_5d_window$Date[i],
    sd_ar_all_aus
  ) %>%
    abs %>%
    print()
}

event_real_aus <- ymd("2012-03-23")
