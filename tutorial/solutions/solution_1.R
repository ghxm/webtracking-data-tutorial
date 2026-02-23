# Exercise 1: Custom classification
# Create a classification scheme for German news outlets

# Build a granular classification where each outlet gets its own label
classification_news <- data.table::data.table(
  domain = c(
    "spiegel.de", "zeit.de", "tagesschau.de", "sueddeutsche.de",
    "faz.net", "welt.de", "t-online.de", "heise.de", "n-tv.de",
    "bild.de", "stern.de", "focus.de"
  ),
  outlet = c(
    "Spiegel", "Zeit", "Tagesschau", "Sueddeutsche",
    "FAZ", "Welt", "t-online", "Heise", "n-tv",
    "Bild", "Stern", "Focus"
  )
)

# Apply classification using domain matching
wtr_data <- classify_visits(wtr_data, classes = classification_news, match_by = "domain")

# Show visit counts per outlet
wtr_data %>%
  filter(!is.na(outlet)) %>%
  count(outlet, sort = TRUE) %>%
  print()

# Visualize outlet distribution
wtr_data %>%
  filter(!is.na(outlet)) %>%
  count(outlet, sort = TRUE) %>%
  ggplot(aes(x = reorder(outlet, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Visits by News Outlet",
    x = "Outlet",
    y = "Number of Visits"
  ) +
  theme_minimal()
