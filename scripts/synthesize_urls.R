# URL Synthesis Script for Web Tracking Tutorial
# Creates enhanced dataset with realistic full URLs

library(tidyverse)
library(lubridate)

set.seed(42) # Reproducibility

# Read original data
original_data <- read_delim(
  "data/ZA5670_webtracking_data.csv",
  delim = ";",
  col_types = cols(
    panelist_id = col_character(),
    start_time = col_character(),
    host = col_character()
  )
)

# Remove quotes from character columns
original_data <- original_data %>%
  mutate(across(where(is.character), ~ str_remove_all(.x, '\"')))

# URL Templates for different domain categories
# Each returns a vector of possible paths for that domain

news_paths <- function(n) {
  categories <- c("politik", "wirtschaft", "sport", "kultur", "panorama", "wissenschaft")
  topics <- c(
    "bundesregierung", "klimawandel", "bundestagswahl", "ukraine-konflikt",
    "inflation", "energiekrise", "bundeskanzler", "europa", "migration",
    "fussball", "bundesliga", "olympia", "kultur-debatte"
  )

  # Calculate exact counts
  n_home <- round(n * 0.15)
  n_category <- round(n * 0.25)
  n_article <- max(0, n - n_home - n_category)

  paths <- c()
  if (n_home > 0) paths <- c(paths, rep("/", n_home))
  if (n_category > 0) paths <- c(paths, paste0("/", sample(categories, n_category, replace = TRUE), "/"))
  if (n_article > 0) {
    base_paths <- paste0(
      "/", sample(categories, n_article, replace = TRUE),
      "/", sample(topics, n_article, replace = TRUE),
      "-", sample(c("analyse", "kommentar", "bericht", "interview"), n_article, replace = TRUE),
      "-a-", sample(100000:999999, n_article, replace = TRUE), ".html"
    )

    # Add query parameters to some articles (tracking, sharing, etc.)
    add_params <- runif(n_article) < 0.3
    base_paths[add_params] <- paste0(
      base_paths[add_params],
      "?", sample(c("utm_source=google", "utm_source=facebook", "ref=twitter",
                    "share=email", "print=true"), sum(add_params), replace = TRUE)
    )

    paths <- c(paths, base_paths)
  }

  sample(paths, n)
}

wiki_paths <- function(n) {
  topics <- c(
    "Bundestagswahl", "Klimawandel", "Künstliche_Intelligenz", "COVID-19-Pandemie",
    "Europäische_Union", "Deutschland", "Berlin", "Bundeskanzler",
    "Photosynthese", "Quantenphysik", "Geschichte_Deutschlands", "Bundesliga",
    "Python_(Programmiersprache)", "Maschinelles_Lernen", "Volkswirtschaft"
  )

  paths <- paste0("/wiki/", sample(topics, n, replace = TRUE))
  paths
}

ecommerce_paths <- function(n, domain) {
  search_terms <- c(
    "laptop", "smartphone", "kopfhörer", "bücher", "kaffeemaschine",
    "fernseher", "fahrrad", "schuhe", "kleidung", "spielzeug"
  )

  if (str_detect(domain, "^(www\\.)?amazon\\.(de|com)$")) {
    n_home <- round(n * 0.1)
    n_search <- round(n * 0.4)
    n_product <- round(n * 0.3)
    n_cart <- round(n * 0.1)
    n_gp <- max(0, n - n_home - n_search - n_product - n_cart)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) {
      # Add sorting/filtering params to searches
      search_paths <- paste0("/s?k=", sample(search_terms, n_search, replace = TRUE))
      add_sort <- runif(n_search) < 0.4
      search_paths[add_sort] <- paste0(
        search_paths[add_sort],
        "&sort=", sample(c("price-asc-rank", "price-desc-rank", "review-rank", "date-desc-rank"), sum(add_sort), replace = TRUE)
      )
      paths <- c(paths, search_paths)
    }
    if (n_product > 0) paths <- c(paths, sapply(1:n_product, function(i) paste0("/dp/", paste0(sample(c(LETTERS, 0:9), 10, replace = TRUE), collapse = ""))))
    if (n_cart > 0) paths <- c(paths, rep("/gp/cart/view.html", n_cart))
    if (n_gp > 0) paths <- c(paths, sapply(1:n_gp, function(i) paste0("/gp/product/", paste0(sample(c(LETTERS, 0:9), 10, replace = TRUE), collapse = ""))))
  } else if (str_detect(domain, "^(www\\.)?ebay\\.(de|com)$")) {
    n_home <- round(n * 0.1)
    n_search <- round(n * 0.5)
    n_item <- round(n * 0.3)
    n_mys <- max(0, n - n_home - n_search - n_item)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) {
      # Add sorting params to searches
      search_paths <- paste0("/sch/i.html?_nkw=", sample(search_terms, n_search, replace = TRUE))
      add_sort <- runif(n_search) < 0.3
      search_paths[add_sort] <- paste0(
        search_paths[add_sort],
        "&_sop=", sample(c("10", "12", "15", "16"), sum(add_sort), replace = TRUE)
      )
      paths <- c(paths, search_paths)
    }
    if (n_item > 0) paths <- c(paths, paste0("/itm/", sample(100000000:999999999, n_item, replace = TRUE)))
    if (n_mys > 0) paths <- c(paths, rep("/mys/home", n_mys))
  } else {
    n_home <- round(n * 0.2)
    n_search <- round(n * 0.6)
    n_anzeige <- max(0, n - n_home - n_search)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) paths <- c(paths, paste0("/s-", sample(search_terms, n_search, replace = TRUE), "/k0"))
    if (n_anzeige > 0) paths <- c(paths, paste0("/anzeige/", sample(100000000:999999999, n_anzeige, replace = TRUE)))
  }

  sample(paths, n)
}

video_social_paths <- function(n, domain) {
  if (str_detect(domain, "youtube")) {
    n_home <- round(n * 0.1)
    n_watch <- round(n * 0.7)
    n_search <- max(0, n - n_home - n_watch)

    search_terms <- c("tutorial", "musik", "nachrichten", "sport", "comedy", "rezept", "diy", "gaming")

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_watch > 0) {
      # Pool of reusable video IDs (mix of "popular" and less common)
      popular_ids <- c(
        "dQw4w9WgXcQ", "1mR6zmSTDNE", "s-6MjhDDPLU", "x93gseHQLyo",
        "kJQP7kiw5Fk", "9bZkp7q19f0", "RgKAFK5djSk", "JGwWNGJdvx8",
        "OPf0YbXqDm0", "fJ9rUzIMcZQ", "hT_nvWreIhg", "YQHsXMglC9A",
        "CevxZvSJLk8", "LsoLEjrDogU", "pRpeEdMmmQ0", "e-ORhEE9VVg"
      )
      # 10% from the popular pool, 90% random unique IDs
      n_popular <- round(n_watch * 0.10)
      n_random <- n_watch - n_popular
      ids <- c(
        sample(popular_ids, n_popular, replace = TRUE),
        replicate(n_random, paste0(sample(c(letters, LETTERS, 0:9), 11, replace = TRUE), collapse = ""))
      )
      watch_paths <- paste0("/watch?v=", sample(ids))
      add_params <- runif(n_watch) < 0.3
      watch_paths[add_params] <- paste0(
        watch_paths[add_params],
        "&", sample(c("list=PLxxx", "t=120s", "feature=share", "ab_channel=ChannelName"), sum(add_params), replace = TRUE)
      )
      paths <- c(paths, watch_paths)
    }
    if (n_search > 0) paths <- c(paths, paste0("/results?search_query=", sample(search_terms, n_search, replace = TRUE)))
  } else if (str_detect(domain, "facebook")) {
    n_home <- round(n * 0.3)
    n_section <- round(n * 0.4)
    n_video <- max(0, n - n_home - n_section)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_section > 0) paths <- c(paths, paste0("/", sample(c("watch", "marketplace", "groups", "events"), n_section, replace = TRUE)))
    if (n_video > 0) paths <- c(paths, paste0("/video/", sample(100000000000:999999999999, n_video, replace = TRUE)))
  } else if (str_detect(domain, "instagram")) {
    n_home <- round(n * 0.2)
    n_post <- round(n * 0.5)
    n_reel <- max(0, n - n_home - n_post)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_post > 0) {
      # Add sharing params to some posts
      post_paths <- sapply(1:n_post, function(i) paste0("/p/", paste0(sample(c(letters, LETTERS, 0:9), 11, replace = TRUE), collapse = "")))
      add_params <- runif(n_post) < 0.2
      post_paths[add_params] <- paste0(post_paths[add_params], "/?igshid=", paste0(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
      paths <- c(paths, post_paths)
    }
    if (n_reel > 0) paths <- c(paths, sapply(1:n_reel, function(i) paste0("/reel/", paste0(sample(c(letters, LETTERS, 0:9), 11, replace = TRUE), collapse = ""))))
  } else {
    paths <- rep("/", n)
  }

  sample(paths, n)
}

search_paths <- function(n, domain) {
  queries <- c(
    "klimawandel", "bundestagswahl", "rezepte", "wetter", "nachrichten",
    "coronavirus", "laptop+kaufen", "urlaub+2024", "fußball+ergebnisse",
    "künstliche+intelligenz", "steuererklärung", "gesunde+ernährung"
  )

  if (str_detect(domain, "^(www\\.)?google\\.(com|de)$")) {
    n_home <- round(n * 0.1)
    n_search <- round(n * 0.8)
    n_paginated <- max(0, n - n_home - n_search)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) paths <- c(paths, paste0("/search?q=", sample(queries, n_search, replace = TRUE)))
    if (n_paginated > 0) paths <- c(paths, paste0("/search?q=", sample(queries, n_paginated, replace = TRUE), "&start=", sample(c(10, 20, 30), n_paginated, replace = TRUE)))
  } else if (str_detect(domain, "^(www\\.)?bing\\.com$")) {
    n_home <- round(n * 0.1)
    n_search <- max(0, n - n_home)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) paths <- c(paths, paste0("/search?q=", sample(queries, n_search, replace = TRUE)))
  } else if (str_detect(domain, "^(www\\.)?duckduckgo\\.com$")) {
    n_home <- round(n * 0.1)
    n_search <- max(0, n - n_home)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) paths <- c(paths, paste0("/?q=", sample(queries, n_search, replace = TRUE)))
  } else if (str_detect(domain, "^(www\\.)?ecosia\\.org$")) {
    n_home <- round(n * 0.1)
    n_search <- max(0, n - n_home)

    paths <- c()
    if (n_home > 0) paths <- c(paths, rep("/", n_home))
    if (n_search > 0) paths <- c(paths, paste0("/search?q=", sample(queries, n_search, replace = TRUE)))
  } else {
    paths <- rep("/", n)
  }

  sample(paths, n)
}

# Domain categorization function
categorize_domain <- function(domain) {
  case_when(
    str_detect(domain, "blacked_out|full_deny") ~ "privacy",
    str_detect(domain, "spiegel|zeit|tagesschau|sueddeutsche|faz|welt|tagesschau|t-online|msn|heise") ~ "news",
    str_detect(domain, "wikipedia") ~ "wikipedia",
    str_detect(domain, "^(www\\.)?amazon\\.(de|com)|^(www\\.)?ebay\\.(de|com)|^(www\\.)?kleinanzeigen\\.de|^(www\\.)?zalando\\.de|^(www\\.)?otto\\.de") ~ "ecommerce",
    str_detect(domain, "youtube|facebook|instagram|twitter|tiktok|reddit") ~ "social_video",
    str_detect(domain, "^(www\\.)?google\\.(com|de)|^(www\\.)?bing\\.com|^(www\\.)?duckduckgo\\.com|^(www\\.)?ecosia\\.org|^(www\\.)?search\\.yahoo") ~ "search",
    TRUE ~ "other"
  )
}

# Generate URL for a given domain
generate_url <- function(domain, category, make_full_url) {
  # Privacy placeholders stay as is
  if (category == "privacy") {
    return(domain)
  }

  # Special browser URLs stay as is (don't add paths)
  # These include chrome://, about:, extensions, file://, etc.
  if (str_detect(domain, "^(chrome|about|chrome-extension|moz-extension|edge|opera|file|data|javascript):")) {
    return(domain)
  }

  # Some entries stay domain-only
  if (!make_full_url) {
    return(domain)
  }

  # Add protocol prefix if not present
  if (!str_detect(domain, "^https?://")) {
    protocol <- "https://"
    domain_clean <- domain
  } else {
    protocol <- ""
    domain_clean <- domain
  }

  # Generate path based on category
  path <- case_when(
    category == "news" ~ news_paths(1),
    category == "wikipedia" ~ wiki_paths(1),
    category == "ecommerce" ~ ecommerce_paths(1, domain),
    category == "social_video" ~ video_social_paths(1, domain),
    category == "search" ~ search_paths(1, domain),
    TRUE ~ "/"
  )

  paste0(protocol, domain_clean, path)
}

# Define domains that should ALWAYS stay as domain-only (never get full URLs)
# These include email, banking, and survey platforms
always_domain_only <- function(domain) {
  str_detect(domain, regex(
    "mail\\.|webmail\\.|web\\.de|gmx\\.net|puls\\.gesis\\.org|sparkasse|bank|online-banking|login\\.|auth\\.|sso\\.",
    ignore_case = TRUE
  ))
}

# Main synthesis logic
cat("Synthesizing URLs...\n")

enhanced_data <- original_data %>%
  mutate(
    category = categorize_domain(host),
    # Determine if this entry should get a full URL
    # Privacy placeholders: always stay as is
    # Email/banking/survey platforms: always domain-only
    # Other sites: 70% full URLs, 30% domain-only
    make_full_url = case_when(
      category == "privacy" ~ FALSE,
      always_domain_only(host) ~ FALSE,
      TRUE ~ runif(n()) < 0.70
    )
  ) %>%
  rowwise() %>%
  mutate(
    url = generate_url(host, category, make_full_url)
  ) %>%
  ungroup() %>%
  select(panelist_id, start_time, url)

# ============================================================================
# Inject duplicate artifacts (consecutive visits to same URL within 1-2 sec)
# ============================================================================
# This simulates real tracking artifacts: browser refreshes, redirects, and
# duplicate logging. We identify visits that are close together in time
# (within 2 seconds) for the same panelist, and copy the URL from the
# previous visit to create realistic duplicates that students can clean.

cat("Injecting duplicate artifacts...\n")

enhanced_data <- enhanced_data %>%
  group_by(panelist_id) %>%
  mutate(
    timestamp = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
    time_diff = as.numeric(difftime(timestamp, lag(timestamp), units = "secs")),
    prev_url = lag(url),
    # ~5% of visits within 1 second become duplicates of the previous visit
    make_duplicate = !is.na(time_diff) & time_diff <= 1 & runif(n()) < 0.05,
    url = if_else(make_duplicate, prev_url, url)
  ) %>%
  ungroup() %>%
  select(panelist_id, start_time, url)

n_duplicates <- sum(enhanced_data %>%
  group_by(panelist_id) %>%
  mutate(is_dup = url == lag(url) & !is.na(lag(url))) %>%
  pull(is_dup), na.rm = TRUE)

cat(sprintf("  Consecutive duplicate URLs in data: %d (%.1f%%)\n",
            n_duplicates, n_duplicates / nrow(enhanced_data) * 100))

# Check distribution (3 categories matching the tutorial)
cat("\nURL type distribution:\n")
enhanced_data %>%
  mutate(
    url_type = case_when(
      url %in% c("blacked_out", "full_deny") ~ "Privacy placeholder",
      str_detect(url, "^https?://") | str_detect(url, "^[a-z][a-z0-9+.-]*:") ~ "Full URL",
      TRUE ~ "Domain only"
    )
  ) %>%
  count(url_type) %>%
  mutate(percentage = scales::percent(n / sum(n))) %>%
  print()

# Sample URLs from each category
cat("\nSample URLs by category:\n")
enhanced_data %>%
  mutate(category = case_when(
    str_detect(url, "blacked_out|full_deny") ~ "privacy",
    str_detect(url, "spiegel|zeit|tagesschau|t-online") ~ "news",
    str_detect(url, "wikipedia") ~ "wikipedia",
    str_detect(url, "amazon|ebay|kleinanzeigen") ~ "ecommerce",
    str_detect(url, "youtube|facebook|instagram") ~ "social_video",
    str_detect(url, "google|bing|duckduckgo|ecosia") ~ "search",
    TRUE ~ "other"
  )) %>%
  filter(category != "privacy") %>%
  group_by(category) %>%
  slice_sample(n = 2) %>%
  select(category, url) %>%
  print(n = 20)

# Write to CSV
cat("\nWriting enhanced dataset...\n")
write_csv(enhanced_data, "data/ZA5670_webtracking_data_enhanced.csv")

cat("\nDone! Enhanced dataset saved to data/ZA5670_webtracking_data_enhanced.csv\n")
cat(sprintf("Total records: %d\n", nrow(enhanced_data)))
