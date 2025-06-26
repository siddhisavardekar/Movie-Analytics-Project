library(dplyr)
library(stringr)
library(stringi)
library(lubridate)
library(purrr)
library(readr)

# Load raw data
tmdb_data <- read_csv("TMDB Movies Dataset-3.csv")

# Clean column names if needed
colnames(tmdb_data) <- make.names(colnames(tmdb_data))

# Preprocess data
tmdb_clean <- tmdb_data %>%
  mutate(
    # Parse release date
    release_date = mdy(release_date),
    
    # Create profit column
    profit = revenue - budget,
    
    # Convert genres from string to list and extract main genre
    genres = str_split(genres, "\\|"),
    main_genre = map_chr(genres, 1),
    
    # Clean up popularity level as ordered factor
    popularity_level = factor(popularity_level,
                              levels = c("Low", "Medium", "High"),
                              ordered = TRUE)
  ) %>%
  # Filter invalid values
  filter(!is.na(budget), budget > 0,
         !is.na(revenue), revenue > 0,
         !is.na(runtime), runtime > 0,
         !is.na(release_date))

# Remove unneeded columns
tmdb_clean <- tmdb_clean %>%
  select(id, imdb_id, original_title, release_date, release_year,
         director, cast, runtime, budget, revenue, profit,
         main_genre, vote_average, vote_count, popularity, popularity_level)

# Save cleaned data to CSV in data/ folder or wherever you prefer
write_csv(tmdb_clean, "D:/clean_tmdb.csv")

