library(afcharts)
library(dplyr)
library(tidyverse)
library(ggplot2)
use_afcharts()

# SETUP ------------------------------------------------------------------------

# Load in survey data
data <- read.csv("FinalSurveyData.csv")

# CREATE TABLES ----------------------------------------------------------------

# Create a frequency table for Q24
frequency_table <- as.data.frame(table(data[["q24_have_shared"]]))
colnames(frequency_table) <- c("Response", "Frequency")

# Filter out blank responses
frequency_table <- frequency_table %>%
  filter(!row_number() %in% 1)

proportion_table <- frequency_table %>%
  mutate(`Total responses` = ave(Frequency, FUN = sum)) %>%
  mutate(Proportion = (Frequency/`Total responses`))

# PLOT GRAPHS ------------------------------------------------------------------

# Plot count graph
ggplot(frequency_table, 
       aes(x = reorder(Response, Frequency), y = Frequency)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 500) +
  scale_x_discrete(labels = label_wrap(15)) + 
  geom_text(aes(label = Frequency, hjust = -0.3)) +
  labs(title = "Number of FFRF beneficiaries sharing advice with other farmers", 
       x = "Response", y = "Count") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_sharing_advice.jpg", width = 11, height = 8)

# Plot percentage graph
ggplot(proportion_table, 
       aes(x = reorder(Response, Proportion), y = Proportion)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 0.6) +
  scale_x_discrete(labels = label_wrap(15)) + scale_y_continuous(labels = percent) +
  geom_text(aes(label = sprintf("%d%%", round(Proportion * 100))), hjust = -0.3) +
  labs(title = "Number of FFRF beneficiaries sharing advice with other farmers", 
       x = "Response", y = "Percentage") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_sharing_advice_percent.jpg", width = 11, height = 8)

# CREATE TABLES ----------------------------------------------------------------

current_question <- data %>%
  select(starts_with("q25_"))
