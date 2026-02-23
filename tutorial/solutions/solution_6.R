# Exercise 6: Sub-divide paths
# Subdivide news visits into subcategories using the path column

news_subcategories <- wtr_data %>%
  filter(is_news_gond == 1) %>%
  mutate(
    news_topic = case_when(
      str_detect(path, "politik|political|bundestag|regierung") ~ "Politics",
      str_detect(path, "sport|bundesliga|fussball|olympia") ~ "Sports",
      str_detect(path, "kultur|feuilleton|music|kunst") ~ "Culture",
      str_detect(path, "wirtschaft|finanzen|boerse|economy") ~ "Business",
      str_detect(path, "wissenschaft|forschung|science|wissen") ~ "Science",
      str_detect(path, "panorama|vermischtes|lifestyle") ~ "Panorama",
      TRUE ~ "Other/Unclassified"
    )
  )

cat("=== News Subcategory Distribution ===\n")
news_subcategories %>%
  count(news_topic, sort = TRUE) %>%
  mutate(percentage = paste0(round(n / sum(n) * 100, 1), "%")) %>%
  print()

# Visualize
news_subcategories %>%
  count(news_topic, sort = TRUE) %>%
  ggplot(aes(x = reorder(news_topic, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "News Visits by Subcategory (Path-Based)",
    x = "News Topic",
    y = "Number of Visits"
  ) +
  theme_minimal()

# Per-panelist breakdown
news_per_panelist <- news_subcategories %>%
  filter(news_topic != "Other/Unclassified") %>%
  group_by(panelist_id, news_topic) %>%
  summarise(visits = n(), .groups = "drop") %>%
  group_by(panelist_id) %>%
  mutate(share = visits / sum(visits)) %>%
  ungroup()

# Average share per topic across panelists
cat("\n=== Average Share of News Topics per Panelist ===\n")
news_per_panelist %>%
  group_by(news_topic) %>%
  summarise(mean_share = round(mean(share), 3), .groups = "drop") %>%
  arrange(desc(mean_share)) %>%
  print()
