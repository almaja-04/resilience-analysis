library(afcharts)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(forcats)
library(rstatix)
library(pheatmap)
use_afcharts()

# SETUP ------------------------------------------------------------------------

# Load in survey data
data <- read.csv("FinalSurveyData.csv")

change_dataframe <- data.frame(
  q9 = c("q9_grow_business", "q9_reduce_business", "q9_diversify", 
         "q9_increase_productivity", "q9_change_core_enterprise", "q9_retire", 
         "q9_leave_other", "q9_other_change"),
  q10 = c("q10_grow_business", "q10_reduce_business", "q10_diversify", 
          "q10_increase_productivity", "q10_change_core_enterprise", "q10_retire", 
          "q10_leave_sooner", "q10_other_change")
)

# Filter for question 10
current_question <- data %>%
  select(starts_with(c("q9_grow_business", "q10_grow_business"))) %>%
  filter(q9_grow_business_influence == "low influence") %>%
  select(-"q9_grow_business_influence")

# Remove Q number from column headings
colnames(current_question) <- 
  str_remove(colnames(current_question), "q10_")

# Add columns to count each time frame
current_question <- as.data.frame(t(current_question))
current_question <- current_question %>%
  mutate("Up to 6 months sooner" = NA, "6 months-1 year sooner" = NA, 
         "1-3 years sooner" = NA, "More than 3 years sooner" = NA)

# Add counts for each level of influence for each response
current_question[] <- lapply(current_question, as.character)

# Count the number of each time frame for each response
current_question$`Up to 6 months sooner` <- 
  rowSums(current_question == "Up to 6 months sooner", na.rm = TRUE)
current_question$`6 months-1 year sooner` <- 
  rowSums(current_question == "6 months-1 year sooner", na.rm = TRUE)
current_question$`1-3 years sooner` <- 
  rowSums(current_question == "1-3 years sooner", na.rm = TRUE)
current_question$`More than 3 years sooner` <- 
  rowSums(current_question == "More than 3 years sooner", na.rm = TRUE)

# Keeping columns with only counts
current_question <- current_question %>%
  select(c(34, 35, 36, 37))

# Renaming question responses for clarity
row.names(current_question) <- "Grow the business"

# Create a pivot table
current_question_pivot <- 
  data.frame(as.table(as.matrix(current_question, row.names = 1)))
colnames(current_question_pivot) <- c("Change Made", "Timeframe", "Freq")

# Append proportions onto the end of the pivot table
current_question_pivot_props <- current_question_pivot %>%
  mutate(`Total per change` = ave(Freq, `Change Made`, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per change`))

---------------------------------------------------------------------------------------------------------------------
  
# Plot count graphs, in the order of total count
ggplot(current_question_pivot, aes(x = `Change Made`, Freq, 
                                   y = Freq, fill = `Timeframe`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + 
  labs(
    title = str_wrap(
      "How much sooner changes made by beneficiaires were brought forward because of support received through the Resilience Fund", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  geom_text(aes(label = Freq), position = position_stack(vjust=0.5)) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )
