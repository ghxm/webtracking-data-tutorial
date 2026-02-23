# Exercise 4: Path analysis
# Extract YouTube video IDs and identify most-watched videos

# Filter to YouTube watch URLs and extract video ID from query string
youtube_videos <- wtr_data %>%
  filter(str_detect(domain, "youtube")) %>%
  mutate(
    search_params = ada_get_search(url),
    # Extract the v= parameter from the query string
    video_id = str_extract(search_params, "(?<=v=)[^&]+")
  ) %>%
  filter(!is.na(video_id))

cat("YouTube watch visits:", nrow(youtube_videos), "\n")
cat("Unique video IDs:", n_distinct(youtube_videos$video_id), "\n\n")

# Most-watched videos (by number of visits across all panelists)
cat("=== Top 20 Most-Watched YouTube Videos ===\n")
top_videos <- youtube_videos %>%
  group_by(video_id) %>%
  summarise(
    total_views = n(),
    unique_viewers = n_distinct(panelist_id),
    .groups = "drop"
  ) %>%
  arrange(desc(total_views)) %>%
  head(20)

print(top_videos)
