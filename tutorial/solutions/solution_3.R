# Exercise 3: User segmentation
# Identify browsing profiles based on category proportions

# Calculate share of visits per category per panelist
panelist_profiles <- wtr_data %>%
  group_by(panelist_id, category_final) %>%
  summarise(visits = n(), .groups = "drop") %>%
  group_by(panelist_id) %>%
  mutate(share = visits / sum(visits)) %>%
  ungroup()

# Identify dominant category per panelist
dominant_category <- panelist_profiles %>%
  group_by(panelist_id) %>%
  slice_max(share, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(panelist_id, dominant_category = category_final, dominant_share = share)

# How many panelists per dominant category?
cat("=== Browsing Profile Distribution ===\n")
dominant_category %>%
  count(dominant_category, sort = TRUE) %>%
  mutate(percentage = paste0(round(n / sum(n) * 100, 1), "%")) %>%
  print()

# Visualize
ggplot(dominant_category, aes(x = reorder(dominant_category, dominant_category, length))) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Panelist Browsing Profiles (Dominant Category)",
    x = "Dominant Category",
    y = "Number of Panelists"
  ) +
  theme_minimal()

# How concentrated are profiles? (distribution of dominant share)
ggplot(dominant_category, aes(x = dominant_share)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "How Dominant Is the Top Category?",
    x = "Share of Visits in Dominant Category",
    y = "Number of Panelists"
  ) +
  theme_minimal()
