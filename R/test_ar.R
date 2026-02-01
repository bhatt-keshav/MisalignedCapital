########## SAUDI ARABIA ###########
# Performing significance tests on AR
ar_est <- calculate_ar(saudi_cds_est_data, m)
sd_ar_est <- ar_est$AR_risk %>% na.omit() %>% sd()

# Sanity Check window: [-5, 5]
ar_5d_window <- calculate_ar_window(
  data = saudi_cds_10y,
  model = m,
  start_date = event - days(7),
  end_date = event + days(7)
)

# Calculating t-stat for Abnormal Returns in this ±5 day window
ar_5d_window <- ar_5d_window %>%
  mutate(t_stat = calc_t_stat(AR_risk, sd_benchmark = sd_ar_est))

# It shows maximum t-stat is at 15 November = real event actually!
## This is the event of real significance
## Segmented regression will be based on this date
event_real <- ymd("2023-11-15")

# T-Test cumulative daily returns
cumulative_ar_3d <- sum(ar_3d_window$AR_risk)
cumulative_ar_3d
cumulative_ar_3d / (sqrt(dim(ar_3d_window)[1]) * sd_ar_est)

cumulative_ar_5d <- sum(ar_5d_window$AR_risk)
cumulative_ar_5d
cumulative_ar_5d / (sqrt(dim(ar_5d_window)[1]) * sd_ar_est)


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
  #   This is the statistically significant event
  geom_vline(aes(xintercept = event_real), linetype = 'dashed') +
  #   This is announced
  geom_vline(aes(xintercept = event), linetype = 1) +
  geom_hline(aes(yintercept = 0))


################################# AUSTRALIA #######################
# Performing significance tests on AR
ar_all_aus <- calculate_ar(aus_cds_10y, m_robust_aus)
sd_ar_all_aus <- ar_all_aus$AR_risk %>% na.omit() %>% sd()

# Compute the t-statistic for 5 days around announcement date
## None of the dates have a significant t-stat
aus_ar_5d_window <- aus_ar_5d_window %>%
  mutate(t_stat = calc_t_stat(AR_risk, sd_benchmark = sd_ar_all_aus))

# Graph the results
## Shows: Smooth movement, No isolated spike, No significant AR and No level or trend shift
ggplot(aus_ar_5d_window, aes(x = Date, y = AR_risk)) +
  geom_line() +
  annotate(
    "rect",
    xmin = event_aus - days(1),
    xmax = event_aus + days(1),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2,
    fill = "orange"
  ) +
  annotate(
    "rect",
    xmin = event_aus - days(3),
    xmax = event_aus + days(3),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.2,
    fill = "purple"
  ) +
  geom_vline(aes(xintercept = event_aus), linetype = 'dashed') +
  geom_hline(aes(yintercept = 0))
