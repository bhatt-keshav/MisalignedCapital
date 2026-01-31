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
  rename(us_10y_yield = Price)

saudi_segmented_reg <- saudi_segmented_reg %>%
  left_join(vix_raw, by = "Date") %>%
  rename(vix = Price)

saudi_segmented_reg <- saudi_segmented_reg %>%
  left_join(brent_raw, by = "Date")


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
# Add predicted cds (y_hat) to the df
saudi_segmented_reg$y_hat <- predict(m_segmented)


# Residuals show a "snake" pattern = not white noise :(
plot(
  residuals(m_segmented),
  type = "l",
  main = "Residuals showing Autocorrelation",
  ylab = "Error",
  xlab = "Time Index"
)
abline(h = 0, col = "red")

# Performing the NW test to account for potential auto-correlation
## NW Test Assumes that both heteroskedasticity AND autocorrelation exist.
## Therefore this test protects against BOTH problems
robust_results <- coeftest(m_segmented, vcov = NeweyWest(m_segmented))
print(robust_results)


# Controlling for other variables (us_10y_yield + vix + Brent_Spot) ***
m_controlled <- lm(
  cds ~ time_center +
    post_event +
    interaction_term +
    us_10y_yield +
    vix +
    Brent_Spot,
  data = saudi_segmented_reg
)
summary(m_controlled)

# Then apply Newey-West again
coeftest(m_controlled, vcov = NeweyWest(m_controlled))

# Adding predictions from the model
## But there are some rows dropped, so adjustments are needed
saudi_segmented_reg$y_hat_cont <- NA # set all rows to NA

## Get rows used in the controlled model
model_rows <- as.numeric(names(fitted(m_controlled)))

# Assign predictions only to those rows
saudi_segmented_reg$y_hat_cont[model_rows] <- predict(m_controlled)


# Plotting the regression line

# 2. Creating the plot
ggplot(saudi_segmented_reg, aes(x = Date, y = cds)) +
  # Actual data points (faded to keep focus on the trend)
  geom_point(alpha = 0.3, color = "gray40") +

  # The Segmented Lines
  geom_line(aes(y = y_hat), color = "darkred", linewidth = 1.2) +

  # Vertical line at the real event (Nov 15, 2023)
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
    label = "-19.74 bps Fall in CDS",
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


### Stacked plot
# Plot 1: Baseline (clean structural break)
p1 <- ggplot(saudi_segmented_reg, aes(x = Date, y = cds)) +
  geom_point(alpha = 0.3, color = "gray40") +
  geom_line(aes(y = y_hat), color = "darkred", linewidth = 1.2) +
  geom_vline(xintercept = event_real, linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = event_real,
    y = 110,
    label = "Baseline: -19.74 bp",
    color = "darkred",
    hjust = -0.1,
    fontface = "bold"
  ) +
  labs(
    title = "Model 1: Baseline Segmented Regression",
    subtitle = "Pure structural break (no controls)",
    x = "Date",
    y = "CDS Level (bps)"
  ) +
  theme_minimal()

# Plot 2: Controlled (shows control variables matter)
p2 <- ggplot(saudi_segmented_reg, aes(x = Date, y = cds)) +
  geom_point(alpha = 0.3, color = "gray40") +
  geom_line(aes(y = y_hat_cont), color = "violet", linewidth = 1.2) +
  geom_vline(xintercept = event_real, linetype = "dashed", color = "black") +
  annotate(
    "text",
    x = event_real,
    y = 110,
    label = "Controlled: -11.75 bp",
    color = "violet",
    hjust = -0.1,
    fontface = "bold"
  ) +
  labs(
    title = "Model 2: With Market Controls",
    subtitle = "Controlling for VIX, oil, yields",
    x = "Date",
    y = "CDS Level (bps)"
  ) +
  theme_minimal()

# Combine
p1 / p2 # Stack vertically
