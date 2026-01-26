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

# Residuals show a "snake" pattern = not white noise :(
plot(
  residuals(m_segmented),
  type = "l",
  main = "Residuals showing Autocorrelation",
  ylab = "Error",
  xlab = "Time Index"
)
abline(h = 0, col = "red")

# Performing the NW test
robust_results <- coeftest(m_segmented, vcov = NeweyWest(m_segmented))
print(robust_results)


# Plotting the regression line
# 1. Adding predictions from the model
saudi_segmented_reg$y_hat <- predict(m_segmented)

# 2. Creating the plot
ggplot(saudi_segmented_reg, aes(x = Date, y = cds)) +
  # Actual data points (faded to keep focus on the trend)
  geom_point(alpha = 0.3, color = "gray40") +

  # The Segmented Lines
  geom_line(aes(y = y_hat), color = "darkred", linewidth = 1.2) +

  # Vertical line at the event (Nov 15, 2023)
  geom_vline(
    xintercept = event_real,
    linetype = "dashed",
    color = "black"
  ) +

  # Annotate the Jump
  annotate(
    "text",
    x = as.Date(event_real),
    y = 110,
    label = "Event: -19.74 bps Jump",
    color = "darkred",
    hjust = -0.1,
    fontface = "bold"
  ) +

  labs(
    title = "Structural Break in Saudi 10Y CDS",
    subtitle = "Segmented Regression: Level Shift vs. Trend Change",
    x = "Date",
    y = "CDS Level (bps)"
  ) +
  theme_minimal()

# Controlling for other variables (yield + vix + Brent_Spot) ***
m_controlled <- lm(
  cds ~ time_center + post_event + interaction_term + yield + vix + Brent_Spot,
  data = saudi_segmented_reg
)
summary(m_controlled)

# Then apply Newey-West again
coeftest(m_controlled, vcov = NeweyWest(m_controlled))
