#Lab_Sea_2304

# Load libraries
library(ggplot2)
library(dplyr)

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


#Bar chart on region
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
# Aggregate by region
region_data <- data %>%
  group_by(WHO.Region) %>%
  summarise(TotalConfirmed = sum(Confirmed, na.rm = TRUE))

# Pie chart
ggplot(region_data, aes(x = "", y = TotalConfirmed, fill = WHO.Region)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Share of Confirmed Cases by WHO Region",
       fill = "WHO Region") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank())



