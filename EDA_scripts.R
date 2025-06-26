library(tidyverse)
library(ggplot2)
library(dplyr)

# Load raw dataset
tmdb_data <- read_csv("clean_tmdb.csv")

# 1. Profitability by Genre
tmdb_data %>%
  group_by(main_genre) %>%
  summarise(mean_profit = mean(profit, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(main_genre, mean_profit), y = mean_profit)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Average Profit by Genre", x = "Genre", y = "Profit ($)")

# 2. Popularity vs. Rating
ggplot(tmdb_data, aes(x = popularity, y = vote_average)) +
  geom_point(alpha = 0.4, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Popularity vs Rating", x = "Popularity", y = "Rating")

# 3. Budget vs Revenue
ggplot(tmdb_data, aes(x = budget, y = revenue)) +
  geom_point(alpha = 0.3, color = "purple") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Budget vs Revenue", x = "Budget", y = "Revenue")

# 4. Top Directors by Average Profit
tmdb_data %>%
  group_by(director) %>%
  summarise(mean_profit = mean(profit, na.rm = TRUE), count = n()) %>%
  filter(count > 3) %>%
  top_n(10, mean_profit) %>%
  ggplot(aes(x = reorder(director, mean_profit), y = mean_profit)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(title = "Top 10 Directors by Average Profit", x = "Director", y = "Profit")

# 5. Genre Distribution Over Time
tmdb_data %>%
  mutate(decade = floor(release_year / 10) * 10) %>%
  group_by(decade, main_genre) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = decade, y = count, fill = main_genre)) +
  geom_bar(stat = "identity") +
  labs(title = "Genre Popularity Over Decades", x = "Decade", y = "Movie Count")

# Check distribution of popularity_level
popularity_distribution <- tmdb_data %>%
  count(popularity_level) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# 6. Bar plot for popularity_level distribution
ggplot(popularity_distribution, aes(x = popularity_level, y = n, fill = popularity_level)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 4.5) +
  labs(
    title = "Distribution of Popularity Levels in Movies",
    x = "Popularity Level",
    y = "Number of Movies"
  ) +
  scale_fill_manual(values = c("Low" = "#FDB863", "Medium" = "#80B1D3", "High" = "#BEBADA")) +
  theme_minimal(base_size = 14)

