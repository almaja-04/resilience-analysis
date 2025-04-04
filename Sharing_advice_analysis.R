library(afcharts)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
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

# Calculate proportions
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

method_names <- 
  c("Informal discussion", "Formal discussion", "Social media/online forums",
    "Farm demonstration/walk", "Other")

# Filter for responses to Q25
current_question <- data %>%
  select(starts_with("q25_")) %>%
  filter(if_all(everything(), ~ !is.na(.)))

# Append totals for each method of sharing to dataframe
new_current_question <- rownames_to_column(current_question, var = "Method")
new_current_question <- 
  adorn_totals(new_current_question, where = "row", name = "Total")
rownames(new_current_question) <- new_current_question$Method
new_current_question$Method <- NULL
new_current_question <- new_current_question[-c(1:463),]
colnames(new_current_question) <- method_names
# Remove "Other" responses as count is negligible
new_current_question$Other <- NULL

# Turn dataframe into a pivot table
current_question_pivot <- 
  data.frame(as.table(as.matrix(new_current_question, row.names = 1)))
current_question_pivot$Var1 <- NULL
colnames(current_question_pivot) <- c("Method of Sharing", "Frequency")

# Calculate proportions
proportion_table <- current_question_pivot %>%
  mutate(`Total responses` = 463) %>% # Added due to a response rate of 463
  mutate(Proportion = (Frequency/`Total responses`))

# PLOT GRAPHS ------------------------------------------------------------------

# Plot count graph
ggplot(current_question_pivot, 
       aes(x = reorder(`Method of Sharing`, Frequency), y = Frequency)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 500) +
  scale_x_discrete(labels = label_wrap(20)) + 
  geom_text(aes(label = Frequency, hjust = -0.3)) +
  labs(title = "Methods beneficiaries shared advice by", 
       x = "Method", y = "Count") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_method_sharing_advice.jpg", width = 11, height = 8)

# Plot percentage graph
ggplot(proportion_table, 
       aes(x = reorder(`Method of Sharing`, Proportion), y = Proportion)) + 
  geom_col(width = 0.7, fill = "#00A33B") + coord_flip() + expand_limits(y = 1.05) + 
  scale_x_discrete(labels = label_wrap(15)) + scale_y_continuous(labels = percent) +
  geom_text(aes(label = sprintf("%d%%", round(Proportion * 100))), hjust = -0.3) +
  labs(title = "Methods beneficiaries shared advice by", 
       x = "Method", y = "Percentage") +
  theme(
    axis.title.y = element_text(angle = 90, vjust = 1), 
    plot.title = element_text(face = "bold", vjust = -2),
    panel.grid.major.y = element_line(colour = "white"),
    panel.grid.major.x = element_line(colour = "grey")
  )

ggsave("graph_method_sharing_advice_percent.jpg", width = 11, height = 8)
