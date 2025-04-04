library(afcharts)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
use_afcharts()

# SETUP ------------------------------------------------------------------------

# Load in survey data
data <- read.csv("FinalSurveyData.csv")

# Create a frequency table for Q22
frequency_table <- as.data.frame(table(data[["q22_willing_to_pay"]]))
colnames(frequency_table) <- c("Response", "Frequency")

# Filter out blank responses
frequency_table <- frequency_table %>%
  filter(!row_number() %in% 1)

# Group all "willing" responses
combined_frequency_table <- frequency_table %>%
  mutate(Response = 
           ifelse(Response == "£0 - I would not have been willing to pay for it", 
                  "Unwilling to pay", 
                  "Willing to pay")) %>%
  group_by(Response) %>%
  summarise(Frequency = sum(Frequency), .groups = "drop")

# Calculate proportions for combined table
combined_proportion_table <- combined_frequency_table %>%
  mutate(`Total responses` = ave(Frequency, FUN = sum)) %>%
  mutate(Proportion = (Frequency/`Total responses`))

# PLOT GRAPHS ------------------------------------------------------------------

# Plot combined count graph
ggplot(combined_frequency_table, 
       aes(x = Response, y = Frequency)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 600) +
  scale_x_discrete(labels = label_wrap(15)) + 
  geom_text(aes(label = Frequency, hjust = -0.3)) +
  labs(title = "Beneficiaries' willingness to pay for support received", 
       x = "Response", y = "Count") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_combined_willingness_to_pay.jpg", width = 11, height = 8)

# Plot combined percentage graph
ggplot(combined_proportion_table, 
       aes(x = Response, y = Proportion)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 0.7) +
  scale_x_discrete(labels = label_wrap(15)) + scale_y_continuous(labels = percent) +
  geom_text(aes(label = sprintf("%d%%", round(Proportion * 100))), hjust = -0.3) +
  labs(title = "Beneficiaries' willingness to pay for support received", 
       x = "Response", y = "Percentage") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_combined_ willingness_to_pay_percent.jpg", width = 11, height = 8)

# CREATE TABLES ----------------------------------------------------------------

# Reorder dataframe by amount willing to pay
desired_order <- 
  c("£0 - I would not have been willing to pay for it", "Up to £100", 
    "Between £100 and £500", "Between £500 and £1000", 
    "Between £1000 and £2000", "Over £2000")

frequency_table <- 
  frequency_table[order(match(frequency_table$Response, desired_order)), ]

frequency_table$Response <- 
  factor(frequency_table$Response, levels = desired_order)
rownames(frequency_table) <- NULL

# REMOVE THESE FOLLOWING 2 LINES IF GRAPHS NEED £0 RESPONSES
frequency_table <- frequency_table %>%
  filter(!row_number() %in% 1)

# Calculate proportions
proportion_table <- frequency_table %>%
  mutate(`Total responses` = ave(Frequency, FUN = sum)) %>%
  mutate(Proportion = (Frequency/`Total responses`))


# PLOT GRAPHS ------------------------------------------------------------------

# Plot count graph
ggplot(frequency_table, 
       aes(x = fct_rev(Response), y = Frequency)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 320) +
  scale_x_discrete(labels = label_wrap(15)) + 
  geom_text(aes(label = Frequency, hjust = -0.3)) +
  labs(title = str_wrap("Amount beneficiaries would have been willing to pay for support received through FFRF", 50), 
       x = "Response", y = "Count") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_willingness_to_pay1.jpg", width = 11, height = 8)

# Plot percentage graph
ggplot(proportion_table, 
       aes(x = fct_rev(Response), y = Proportion)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 0.6) +
  scale_x_discrete(labels = label_wrap(15)) + scale_y_continuous(labels = percent) +
  geom_text(aes(label = sprintf("%d%%", round(Proportion * 100))), hjust = -0.3) +
  labs(title = str_wrap("Amount beneficiaries would have been willing to pay for support received through FFRF", 50), 
       x = "Response", y = "Percentage") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_willingness_to_pay_percent1.jpg", width = 11, height = 8)

# CREATE TABLES ----------------------------------------------------------------
