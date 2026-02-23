# Exercise 5: Survey correlation
# Test if browsing behavior correlates with demographic variables

# analysis_data is already available from the tutorial (merged wt + survey)
# It contains: n_visits, News_visits, da119a_sex, da120a_ybrth, da127a_seduc,
#              political_interest, etc.

# Boxplot: News visits by education level
analysis_data %>%
  filter(!is.na(da127a_seduc)) %>%
  ggplot(aes(x = da127a_seduc, y = News_visits)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  labs(
    title = "News Visits by Education Level",
    x = "Education",
    y = "Number of News Visits"
  ) +
  theme_minimal()

# Boxplot: Total visits by age group
analysis_data %>%
  filter(!is.na(da120a_ybrth)) %>%
  ggplot(aes(x = da120a_ybrth, y = n_visits)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",")) +
  labs(
    title = "Total Visits by Age Group",
    x = "Age Group",
    y = "Number of Visits"
  ) +
  theme_minimal()
