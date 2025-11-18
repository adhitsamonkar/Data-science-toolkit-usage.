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

