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
              "Leave farming (retire or pass onto next generation", 
              "Leave farming (other reasons)", "Other")
)

schemes_dataframe <- data.frame(
  q20 = c("cq20_sfi", "cq20_cs", "cq20_fetf", "cq20_ahw", "cq20_fipl", 
          "cq20_ftf", "cq20_lr", "cq20_slurry", "cq20_trees", "cq20_woodland"),
  q21 = c("cq21_sfi", "cq21_cs", "cq21_fetf", "cq21_ahw", "cq21_fipl", 
          "cq21_ftf", "cq21_lr", "cq21_slurry", "cq21_trees", "cq21_woodland"),
  renamed = c("Sustainable Farming Incentive", "Countryside Stewardship", 
              "Farming Equipment and Technology Fund", "Animal Health and Welfare", 
              "Farming in Protected Landscapes", "Farming Transformation Fund", 
              "Landscape Recovery", "Slurry Infrastructure Grants", 
              "Tree Health Pilot", "Woodland Creation Offer")
)

# Create master dataframes to bind individual data to
df_time <- data.frame()
df_schemes <- data.frame()

# CREATE TABLES FOR PLOTTING (CHANGES) -----------------------------------------

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

# Remove rows where count = 0 for plotting
df_time_plot <- df_time %>%
  filter(Freq != 0)

# PLOT GRAPH -------------------------------------------------------------------

# Plot count graphs, in the order of total count
ggplot(df_time_plot, aes(x = reorder(`Change Made`, `Total per change`), 
                    y = Freq, fill = `Timeframe`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + expand_limits(y = 65) + 
  labs(
    title = str_wrap(
      "How much sooner changes made by beneficiaires were brought forward because of support received through the Resilience Fund", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  geom_text(aes(label = Freq), position = position_stack(vjust=0.5), colour = "white") + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

ggsave("Changes_brought_forward.jpg", width = 11, height = 8)

# PLOT PERCENTAGE GRAPH --------------------------------------------------------

#Create a dataframe that orders changes in terms of timeframe
# and appends this onto main pivot table
reordered_timeframe_data <- df_time %>%
  filter(Timeframe %in% c("1-3 years sooner", "More than 3 years sooner")) %>%
  group_by(`Change Made`) %>%
  summarise(Total_1plus = sum(Proportion, na.rm = TRUE)) %>%
  arrange(Total_1plus)  # Sort in descending order
df_time <- df_time %>%
  mutate(`Change Made` = factor(`Change Made`, levels =
                                  reordered_timeframe_data$`Change Made`))

# Remove rows where count = 0 for plotting
df_time_plot <- df_time %>%
  filter(Freq != 0)

# Plot count graphs, in the order of total count
ggplot(df_time_plot, aes(x = `Change Made`, y = Proportion, fill = `Timeframe`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + expand_limits(y = 1) + 
  labs(
    title = str_wrap(
      "How much sooner changes made by beneficiaires were brought forward because of support received through the Resilience Fund", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
            position = position_stack(vjust=0.5), colour = "white") + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

ggsave("Changes_brought_forward_percent.jpg", width = 11, height = 8)

# CREATE TABLES FOR PLOTTING (SCHEMES) -----------------------------------------

# Iteration to address each change made
for (i in 1:nrow(schemes_dataframe)) {
  col_name <- sym(schemes_dataframe[i,1])
  
  # Filtering for Q20
  current_question <- data %>%
    select(c(schemes_dataframe[i,1], schemes_dataframe[i,2])) %>%
    filter(!!col_name == "low influence") %>%
    select(-starts_with("cq20"))
  
  # Remove question number from column headings
  colnames(current_question) <-
    str_remove(colnames(current_question), "cq21_")
  
  # Add columns to count each time frame
  current_question <- as.data.frame(t(current_question))
  current_question <- current_question %>%
    mutate("Up to 6 months later" = NA, "6 months-1 year later" = NA, 
           "1-2 years later" = NA, "More than 2 years later" = NA)
  
  # Add counts for each level of influence for each response
  current_question[] <- lapply(current_question, as.character)
  
  # Count the number of each timeframe for each response
  current_question <- current_question %>%
    mutate(`Up to 6 months later` = 
             rowSums(across(everything(), ~ . %in% c(
               "up to 6 months later", "Up to 6 months later")), na.rm = TRUE),
           `6 months-1 year later` = 
             rowSums(across(everything(), ~ . %in% c(
               "6 months - 1 year later", "Between 6 months and 1 year later")), 
               na.rm = TRUE),
           `1-2 years later` = 
             rowSums(across(everything(), ~ . == "1 - 2 years later"), 
                     na.rm = TRUE),
           `More than 2 years later` = 
             rowSums(across(everything(), ~ . == "More than 2 years later"), 
                     na.rm = TRUE)
    )
  
  # Keeping columns only with counts
  current_question <- current_question[, tail(colnames(current_question), 4)]
  
  # Renaming question responses for clarity
  row.names(current_question) <- schemes_dataframe[i,3]
  
  # Create a pivot table
  current_question_pivot <- 
    data.frame(as.table(as.matrix(current_question, row.names = 1)))
  colnames(current_question_pivot) <- c("Scheme", "Timeframe", "Freq")
  
  # Append proportions onto the end of the pivot table
  current_question_pivot_props <- current_question_pivot %>%
    mutate(`Total per scheme` = ave(Freq, Scheme, FUN = sum)) %>%
    mutate(Proportion = (Freq/`Total per scheme`))
  
  # Bind the data onto master dataframe
  df_schemes <- bind_rows(df_schemes, current_question_pivot_props)
}

# Remove rows where count = 0 for plotting
df_schemes_plot <- df_schemes %>%
  filter(Freq != 0)

# PLOT GRAPH (SCHEMES) ---------------------------------------------------------

# Plot count graphs, in the order of total count
ggplot(df_schemes_plot, aes(x = reorder(Scheme, `Total per scheme`), 
                         y = Freq, fill = `Timeframe`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + expand_limits(y = 65) + 
  labs(
    title = str_wrap(
      "How much later applications (or plans to apply) to schemes would have happened, if not for support recieved through the Resilience Fund", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  geom_text(aes(label = Freq), position = position_stack(vjust=0.5), colour = "white") + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

ggsave("Schemes_brought_forward.jpg", width = 11, height = 8)

# PLOT PERCENTAGE GRAPH (SCHEMES) ----------------------------------------------

#Create a dataframe that orders changes in terms of timeframe
# and appends this onto main pivot table
reordered_scheme_data <- df_schemes %>%
  filter(Timeframe %in% c("1-2 years later", "More than 2 years later")) %>%
  group_by(Scheme) %>%
  summarise(Total_1plus = sum(Proportion, na.rm = TRUE)) %>%
  arrange(Total_1plus)  # Sort in descending order
df_schemes <- df_schemes %>%
  mutate(Scheme = factor(Scheme, levels = reordered_scheme_data$Scheme))

# Remove rows where count = 0 for plotting
df_schemes_plot <- df_schemes %>%
  filter(Freq != 0)

# Plot count graphs, in the order of total count
ggplot(df_schemes_plot, aes(x = Scheme, y = Proportion, fill = `Timeframe`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + expand_limits(y = 1) + 
  labs(
    title = str_wrap(
      "How much later applications (or plans to apply) to schemes would have happened, if not for support recieved through the Resilience Fund", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
            position = position_stack(vjust=0.5), colour = "white") + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

ggsave("Schemes_brought_forward_percent.jpg", width = 11, height = 8)
