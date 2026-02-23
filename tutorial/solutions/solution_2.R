# Exercise 2: Temporal analysis
# Calculate average visit duration by hour of day

hourly_duration <- wtr_data %>%
  mutate(hour = as.numeric(format(timestamp, "%H"))) %>%
  filter(!is.na(duration)) %>%
  group_by(hour) %>%
  summarise(
    mean_duration = mean(duration, na.rm = TRUE),
    median_duration = median(duration, na.rm = TRUE),
    n_visits = n(),
    .groups = "drop"
  )

print(hourly_duration)

# Visualize mean duration by hour
ggplot(hourly_duration, aes(x = hour, y = mean_duration)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Visit Duration by Hour of Day",
    x = "Hour of Day (0-23)",
    y = "Mean Duration (seconds)"
  ) +
  theme_minimal()

# Visualize number of visits by hour (activity level)
ggplot(hourly_duration, aes(x = hour, y = n_visits)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(
    title = "Number of Visits by Hour of Day",
    x = "Hour of Day (0-23)",
    y = "Number of Visits"
  ) +
  theme_minimal()
