# Filter ± 3 months
saudi_segmented_reg <- saudi_cds_10y_raw %>%
  filter(
    Date >= event_real %m-% months(3) &
      Date <= (event_real %m+% months(3))
  )

saudi_segmented_reg <- saudi_segmented_reg %>%
  arrange(Date) %>%
  rename(cds = Price)

saudi_segmented_reg <- saudi_segmented_reg %>%
  left_join(us_10y_raw, by = "Date") %>%
  rename(yield = Price)

saudi_segmented_reg <- saudi_segmented_reg %>%
  left_join(vix, by = "Date") %>%
  rename(vix = Price)

saudi_segmented_reg <- saudi_segmented_reg %>%
  left_join(brent, by = "Date")


# Prepare data for regression
## Mutations: Time centered at the event (0 = day of event)
## 1 if date is on or after the event, else 0
## Interaction term, a flag that turns on after the event
saudi_segmented_reg <- saudi_segmented_reg %>%
  mutate(
    time_center = Date - event_real,
    post_event = ifelse(Date >= event_real, 1, 0),
    interaction_term = time_center * post_event
  )

# Do regression
m_segmented <- lm(
  cds ~ time_center + post_event + interaction_term,
  data = saudi_segmented_reg
)

summary(m_segmented)

# dw_results <- dwtest(m_segmented)
# print(dw_results)

robust_results <- coeftest(m_segmented, vcov = NeweyWest(m_segmented))
print(robust_results)
