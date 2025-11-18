#Lab_Sea_2304

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Read CSV
data <- read.csv("country_wise_latest.csv")

# Select top 10 by Confirmed cases
top10 <- data %>%
  arrange(desc(Confirmed)) %>%
  head(10)

# Clean bar chart
ggplot(top10, aes(x = reorder(Country.Region, Confirmed),
                  y = Confirmed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries by Confirmed COVID-19 Cases",
       x = "Country",
       y = "Total Confirmed Cases") +
  theme_minimal(base_size = 14)

library(ggplot2)
library(dplyr)

# Read CSV
data <- read.csv("country_wise_latest.csv")

# Group by WHO Region
region_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(TotalConfirmed = sum(Confirmed, na.rm = TRUE)) %>%
  arrange(desc(TotalConfirmed))

# Plot
ggplot(region_data, aes(x = reorder(WHO.Region, TotalConfirmed),
                        y = TotalConfirmed)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Confirmed COVID-19 Cases by WHO Region",
       x = "WHO Region",
       y = "Total Confirmed Cases") +
  theme_minimal(base_size = 14)

#pie chart

# Aggregate by region & compute percentages
region_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(TotalConfirmed = sum(Confirmed, na.rm = TRUE)) %>%
  mutate(Percent = TotalConfirmed / sum(TotalConfirmed),
         LegendLabel = paste0(WHO.Region,
                              " — ",
                              percent(Percent)))

# Pie chart with clean look
ggplot(region_data, aes(x = "", y = TotalConfirmed, fill = LegendLabel)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y") +
  labs(title = "Share of Confirmed COVID-19 Cases by WHO Region") +
  theme_void(base_size = 14) +
  guides(fill = guide_legend(title = "WHO Region (with % share)"))

#stacked bar
# Summarise metrics by region
region_stack <- data %>%
  group_by(WHO.Region) %>%
  summarise(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE)
  )

# Convert to long format for stacking
region_long <- region_stack %>%
  pivot_longer(cols = c(Confirmed, Deaths, Recovered),
               names_to = "Metric",
               values_to = "Value")

# Stacked bar chart
ggplot(region_long, aes(x = WHO.Region, y = Value, fill = Metric)) +
  geom_bar(stat = "identity") +
  labs(title = "NO OF PEOPLE AFFECTED BY COVID-19 (Stacked Bar)",
       x = "WHO Region",
       y = "Total Count",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


