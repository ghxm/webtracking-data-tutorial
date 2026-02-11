# Working with Web Tracking Data

Workshop session materials for an interactive code-along.

## Structure

- `slides/` - Presentation slides (using gesis-pandoc-beamer template)
- `tutorial/` - Interactive tutorial session with integrated exercises
- `data/` - Data files (see setup instructions below)
- `helpers.R` - Shared helper functions sourced by all documents
- `scripts/` - Data processing scripts (URL synthesis, etc.)

## Setup

### Important: Open Project from Root Folder

**⚠️ You must open this project from the repository root folder in RStudio or your IDE for all paths to work correctly.**

- ✓ Correct: Open the project from `/path/to/webtracking-data-tutorial/`
- ✗ Incorrect: Opening individual files or subdirectories

This ensures all relative paths to data files, helper scripts, and other resources work properly.

### Data Download

Download the Web Tracking data (Campusfile) from https://search.gesis.org/research_data/ZA5670 and place the following files in the `data/` folder:

- `ZA5670_webtracking_data.csv` - Original web tracking data (host-level)
- `ZA5670_survey_data.csv` - Survey responses and demographics

**Note:** The enhanced dataset with synthetic full URLs will be automatically generated when you first run the tutorial.
