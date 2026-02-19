# Data loading functions for "Working with Web Tracking Data" workshop

library(tidyverse)

# Data Paths
DATA_DIR <- here::here("data")
SURVEY_DATA_PATH <- file.path(DATA_DIR, "ZA5670_survey_data.csv")
WEBTRACKING_DATA_PATH <- file.path(DATA_DIR, "ZA5670_webtracking_data.csv")
ENHANCED_DATA_PATH <- file.path(DATA_DIR, "ZA5670_webtracking_data_enhanced.csv")

#' Create Enhanced Dataset with Synthetic URLs
#'
#' This function generates the enhanced web tracking dataset by running the
#' synthesis script. It adds realistic full URLs to the original host-only data.
#' This process takes about 1-2 minutes.
#'
#' Note: This dataset is synthetic and created for demonstration purposes only.
#' The original ZA5670 dataset contains only host-level information.
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise
#' @export
create_enhanced_dataset <- function() {
  cat("Creating enhanced dataset with synthetic URLs...\n")
  cat("This will take about 1-2 minutes. Please wait.\n\n")

  # Check if synthesis script exists
  script_path <- here::here("scripts", "synthesize_urls.R")
  if (!file.exists(script_path)) {
    stop("Synthesis script not found at: ", script_path)
  }

  # Run synthesis script
  result <- tryCatch({
    source(script_path)
    cat("\n✓ Enhanced dataset created successfully!\n")
    cat("  Location:", ENHANCED_DATA_PATH, "\n\n")
    TRUE
  }, error = function(e) {
    cat("\n✗ Error creating enhanced dataset:\n")
    cat("  ", conditionMessage(e), "\n\n")
    FALSE
  })

  invisible(result)
}

#' Load Survey Data
#'
#' Loads the GESIS panel survey data with panelist demographics and responses.
#'
#' @return A tibble with survey data
#' @export
load_survey_data <- function() {
  cat("Loading survey data...\n")

  if (!file.exists(SURVEY_DATA_PATH)) {
    stop("Survey data file not found at: ", SURVEY_DATA_PATH)
  }

  survey_data <- read_delim(
    SURVEY_DATA_PATH,
    delim = ";",
    col_types = cols(.default = col_character())
  ) %>%
    mutate(across(where(is.character), ~ str_remove_all(.x, '\"')))

  cat("✓ Loaded", nrow(survey_data), "survey responses\n\n")

  return(survey_data)
}

#' Load Original Web Tracking Data
#'
#' Loads the original GESIS panel web tracking data with host-only information.
#'
#' @return A tibble with original web tracking data (panelist_id, start_time, host)
#' @export
load_webtracking_data <- function() {
  cat("Loading original web tracking data...\n")

  if (!file.exists(WEBTRACKING_DATA_PATH)) {
    stop("Web tracking data file not found at: ", WEBTRACKING_DATA_PATH)
  }

  webtracking_data <- read_delim(
    WEBTRACKING_DATA_PATH,
    delim = ";",
    col_types = cols(
      panelist_id = col_character(),
      start_time = col_character(),
      host = col_character()
    )
  ) %>%
    mutate(across(where(is.character), ~ str_remove_all(.x, '\"')))

  cat("✓ Loaded", format(nrow(webtracking_data), big.mark = ","), "web tracking records\n")
  cat("  Panelists:", length(unique(webtracking_data$panelist_id)), "\n\n")

  return(webtracking_data)
}

#' Load Enhanced Web Tracking Data
#'
#' Loads the enhanced web tracking data with synthetic full URLs.
#' If the enhanced dataset doesn't exist, prompts to create it.
#'
#' @return A tibble with enhanced web tracking data (panelist_id, start_time, url)
#' @export
load_enhanced_webtracking_data <- function() {
  cat("Loading enhanced web tracking data...\n")

  # Check if enhanced dataset exists
  if (!file.exists(ENHANCED_DATA_PATH)) {
    cat("Enhanced dataset not found.\n")
    cat("Creating it now (this will take 1-2 minutes)...\n\n")

    success <- create_enhanced_dataset()
    if (!success) {
      stop("Failed to create enhanced dataset. Please check the error message above.")
    }
  }

  enhanced_data <- read_csv(
    ENHANCED_DATA_PATH,
    col_types = cols(
      panelist_id = col_character(),
      start_time = col_character(),
      url = col_character()
    )
  )

  cat("✓ Loaded", format(nrow(enhanced_data), big.mark = ","), "enhanced web tracking records\n")
  cat("  Panelists:", length(unique(enhanced_data$panelist_id)), "\n\n")

  return(enhanced_data)
}

#' Load All Data
#'
#' Convenience function to load all datasets at once.
#' Returns a named list with survey_data, webtracking_data, and enhanced_data.
#'
#' @return A named list with three elements: survey_data, webtracking_data, enhanced_data
#' @export
load_all_data <- function() {
  cat("========================================\n")
  cat("Loading all datasets...\n")
  cat("========================================\n\n")

  data_list <- list(
    survey_data = load_survey_data(),
    webtracking_data = load_webtracking_data(),
    enhanced_data = load_enhanced_webtracking_data()
  )

  cat("========================================\n")
  cat("All datasets loaded successfully!\n")
  cat("========================================\n\n")

  return(data_list)
}

# ============================================================================
# Create enhanced dataset on first load
# ============================================================================
# This ensures the enhanced dataset is created before the tutorial starts,
# since it takes 1-2 minutes and users need to generate it themselves.

if (!file.exists(ENHANCED_DATA_PATH)) {
  cat("\n")
  cat("========================================\n")
  cat("FIRST-TIME SETUP\n")
  cat("========================================\n\n")
  cat("The enhanced dataset doesn't exist yet.\n")
  cat("Creating it now (this will take 1-2 minutes)...\n\n")

  create_enhanced_dataset()

  cat("========================================\n")
  cat("Setup complete! You're ready to start.\n")
  cat("========================================\n\n")
}

# ============================================================================
# Load enhanced dataset into wt_data for tutorial use
# ============================================================================
# This loads the enhanced web tracking data into a dataframe called wt_data
# with columns: panelist_id, start_time, url

wt_data <- load_enhanced_webtracking_data()

cat("Enhanced web tracking data loaded into 'wt_data' dataframe\n")
cat("Columns: panelist_id, start_time, url\n\n")