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
  q9 = c("q9_grow_business_influence", "q9_reduce_business_influence", 
         "q9_diversify_influence", "q9_increase_productivity_influence", 
         "q9_change_core_enterprise_influence", "q9_retire_influence", 
         "q9_leave_other_influence", "q9_other_change_influence"),
  q10 = c("q10_grow_business_sooner", "q10_reduce_business_sooner", 
          "q10_diversify_sooner", "q10_increase_productivity_sooner", 
          "q10_change_core_enterprise_sooner", "q10_retire_sooner", 
          "q10_leave_other_sooner", "q10_other_change_sooner"),
  renamed = c("Grow the business", "Reduce size of the business", 
              "Diversify into non-farming areas", "Increase productivity", 
              "Change core agricultural enterprises", 
              "Leave farming (reture or pass onto next generation", 
              "Leave farming (other reasons)", "Another change")
)

# Create master dataframe to bind individual change data to
df_time <- data.frame(
)

# Iteration to address each change made
for (i in 1:nrow(change_dataframe)) {
  col_name <- sym(change_dataframe[i,1])
  
  # Filtering for Q10
  current_question <- data %>%
    select(c(change_dataframe[i,1], change_dataframe[i,2])) %>%
    filter(!!col_name == "low influence") %>%
    select(-starts_with("q9"))
  
  # Remove question number from column headings
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
  
  # Keeping columns only with counts
  current_question <- current_question[, tail(colnames(current_question), 4)]
  
  # Renaming question responses for clarity
  row.names(current_question) <- change_dataframe[i,3]
  
  # Create a pivot table
  current_question_pivot <- 
    data.frame(as.table(as.matrix(current_question, row.names = 1)))
  colnames(current_question_pivot) <- c("Change Made", "Timeframe", "Freq")
  
  # Append proportions onto the end of the pivot table
  current_question_pivot_props <- current_question_pivot %>%
    mutate(`Total per change` = ave(Freq, `Change Made`, FUN = sum)) %>%
    mutate(Proportion = (Freq/`Total per change`))
  
  # Bind the data onto master dataframe
  df_time <- bind_rows(df_time, current_question_pivot_props)
}


# Plot count graphs, in the order of total count
ggplot(df_time, aes(x = `Change Made`, Freq, y = Freq, fill = `Timeframe`)) + 
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
