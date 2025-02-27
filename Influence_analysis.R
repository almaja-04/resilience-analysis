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

# Lists to rename headings
q9_NewHeadings <- 
  c("Stay farming - grow the business", "Stay farming - reduce business size", 
    "Stay farming - diversify into non-farming areas", 
    "Stay farming - increase productivity", 
    "Stay farming - change core agricultural enterprises", 
    "Leave farming - retire or pass on", "Leave farming - other reasons", "Other")

# PREPARING QUESTION 9 FOR PLOTTING --------------------------------------------

# Filter for question
current_question <- data %>%
  select(starts_with("q9_"))

# Transpose the dataframe and add extra columns
current_question <- as.data.frame(t(current_question))
current_question <- current_question %>%
  mutate("High Influence" = NA, "Medium Influence" = NA, "Low Influence" = NA, 
         "No Influence" = NA, "Unsure" = NA)

# Add counts for each level of influence for each response
current_question[] <- lapply(current_question, as.character)

# Count the number of each level of influence for each response
current_question$`High Influence` <- 
  rowSums(current_question == "high influence", na.rm = TRUE)
current_question$`Medium Influence` <- 
  rowSums(current_question == "medium influence", na.rm = TRUE)
current_question$`Low Influence` <- 
  rowSums(current_question == "low influence", na.rm = TRUE)
current_question$`No Influence` <- 
  rowSums(current_question == "no influence", na.rm = TRUE)
current_question$`Unsure` <- 
  rowSums(current_question == "unsure", na.rm = TRUE)

# Keeping columns with only counts
current_question <- current_question %>%
  select(c(913, 914, 915, 916, 917))

# Renaming question responses for clarity
row.names(current_question) <- q9_NewHeadings

# Filter out the "Unsure" option
current_question_no_unsure <- current_question %>%
  select(-`Unsure`)

# Create a pivot table
current_question_pivot <- 
  data.frame(as.table(as.matrix(current_question_no_unsure, row.names = 1)))
colnames(current_question_pivot) <- c("Change Made", "Influence Level", "Freq")

# Append proportions onto the end of the pivot table
current_question_pivot1 <- current_question_pivot %>%
  mutate(`Total per change` = ave(Freq, `Change Made`, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per change`))

#Create a dataframe that orders changes in terms of level of influence
# and appends this onto main pivot table
reordered_change_data <- current_question_pivot1 %>%
  filter(`Influence Level` %in% c("High Influence", "Medium Influence")) %>%
  group_by(`Change Made`) %>%
  summarise(Total_High_Medium = sum(Proportion, na.rm = TRUE)) %>%
  arrange(Total_High_Medium)  # Sort in descending order
current_question_pivot1 <- current_question_pivot1 %>%
  mutate(`Change Made` = factor(`Change Made`, 
                                levels = reordered_change_data$`Change Made`))

# PLOT QUESTION 9 COUNT GRAPHS -------------------------------------------------

# Plot count graphs, in the order of total count
ggplot(current_question_pivot, aes(x = reorder(`Change Made`, Freq), 
                                   y = Freq, fill = `Influence Level`)) + 
  geom_bar(stat = "identity", position = "stack") + expand_limits(y = 300) + 
  coord_flip() + 
  labs(
    title = str_wrap(
      "Change made (or planned), and the level of influence that support received through the Resilience Fund had on them", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  geom_text(aes(label = Freq), position = position_stack(vjust=0.5)) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

# Save to .jpg
ggsave("Influence_on_change_count.jpg", width = 11, height = 8)

# Plot count graphs, in the order of % high influence
ggplot(current_question_pivot1, aes(x = `Change Made`, 
                                    y = Freq, fill = `Influence Level`)) + 
  geom_bar(stat = "identity", position = "stack") + expand_limits(y = 300) + 
  coord_flip() + 
  labs(
    title = str_wrap(
      "Change made (or planned), and the level of influence that support received through the Resilience Fund had on them", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  geom_text(aes(label = Freq), position = position_stack(vjust=0.5)) + 
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
  )

# Save to .jpg
ggsave("Influence_on_change_count1.jpg", width = 11, height = 8)

# PLOT QUESTION 9 PERCENTAGE GRAPHS --------------------------------------------

# Plot percentage graphs
ggplot(current_question_pivot1, aes(x = `Change Made`, y = Proportion, 
                                    fill = `Influence Level`)) + 
  geom_bar(stat = "identity", position = "stack") + expand_limits(y = 1) + 
  coord_flip() + 
  labs(
    title = str_wrap(
      "Change made (or planned), and the level of influence that support received through the Resilience Fund had on them", 50),
    x = "Change made", y = "Percentage") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
  )

# Save to .jpg
ggsave("Influence_on_change_percent.jpg", width = 11, height = 8)

# THIS SECTION ADDS AN EXTRA COLUMN FOR AVERAGE INFLUENCE ACROSS ALL CHANGES

# Append totals for each level of influence onto dataframe
new_current_question <- rownames_to_column(current_question_no_unsure, var = "Change")
new_current_question <- 
  adorn_totals(new_current_question, where = "row", name = "All changes")
rownames(new_current_question) <- new_current_question$Change
new_current_question$Change <- NULL
new_current_question <- new_current_question[-c(1:8),]

# Create a pivot table for these totals
new_question_pivot <- 
  data.frame(as.table(as.matrix(new_current_question, row.names = 1)))
colnames(new_question_pivot) <- c("Change Made", "Influence Level", "Freq")

# Append proportions onto the end of the pivot table
new_question_pivot1 <- new_question_pivot %>%
  mutate(`Total per change` = ave(Freq, `Change Made`, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per change`))

# Bind the totals pivot together with the original pivot table
# (The pivot table with changes separated)
combined_change_pivot <- bind_rows(current_question_pivot1, new_question_pivot1)

# Plot percentage graphs, with an extra column showing influence on all changes
ggplot(combined_change_pivot, aes(x = `Change Made`, y = Proportion, fill = `Influence Level`)) + 
  geom_bar(stat = "identity", position = "stack") + expand_limits(y = 1) + 
  coord_flip() + 
  labs(
    title = str_wrap(
      "Change made (or planned), and the level of influence that support received through the Resilience Fund had on them", 50),
    x = "Change made", y = "Percentage") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
            position = position_stack(vjust = 0.5)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
  )

# Save to .jpg
ggsave("Combined_influence_on_change.jpg", width = 11, height = 8)

# QUESTION 9 STATISTICAL TESTING -----------------------------------------------

# Chi squared and residuals
chisq <- chisq.test(current_question)
print("Chi-square result for Q8")
print(chisq)
fishers <- chisq.test(current_question, simulate.p.value = TRUE)
print("Fishers exact result for Q8")
print(fishers)

current_residuals <- as.data.frame(chisq$stdres) %>%
  select(-Unsure)

current_heatmap <- 
  pheatmap(current_residuals, cluster_rows = FALSE, cluster_cols = FALSE, 
           display_numbers = TRUE, main = "Standardised Residuals", 
           fontsize = 14, fontsize_number = 12)

# Save to .jpg
ggsave("q9_heatmap.jpg", current_heatmap, width = 11, height = 8)

#############################################################
# PLOTTING WITH "I AM A PART OF THE SCHEME" STILL IN THE DATA
#############################################################



# PREPARING QUESTION 20 FOR PLOTTING -------------------------------------------

# Filter for question
current_question <- data %>% 
  select(starts_with("q20_"))

# Remove "_influence" from each column heading
colnames(current_question) <- 
  str_remove(colnames(current_question), "_influence$")

group_names <- unique(str_extract(colnames(current_question), "[^_]+$"))

# Split data into separate data frames based on scheme
split_dfs <- setNames(lapply(group_names, function(group) {
  current_question %>% select(matches(paste0("_", group, "$")))
}), group_names)

names(split_dfs) <- paste0("influence_", group_names)
list2env(split_dfs, env = .GlobalEnv)

# Data frame for totaling level of influence for each scheme
df_iterate <- data.frame(
  df_name = c("influence_ahw", "influence_cs", "influence_fetf", "influence_fipl", 
              "influence_ftf", "influence_lr", "influence_sfi", "influence_slurry", 
              "influence_trees", "influence_woodland"),
  scheme_name = c("Animal Health and Welfare", "Countryside Stewardship", 
                  "Farming in Protected Landscapes",
                  "Farming Equipment and Technology Fund", 
                  "Farming Transformation Fund","Landscape Recovery", 
                  "Slurry Infrastructure Grants", "Sustainable Farming Incentive",
                  "Tree Health Pilot", "Woodland Creation Offer")
)

# Create master dataframe to bind individual scheme data to
df_schemes <- data.frame(
)

for (i in 1:nrow(df_iterate)) {
  current_scheme <- get(df_iterate[i,1])
  
  current_scheme <- as.data.frame(t(current_scheme))
  
  current_scheme <- current_scheme %>%
    mutate("High Influence" = NA, "Low Influence" = NA, "No Influence" = NA)
  
  # Add counts for each level of influence for each response
  current_scheme[] <- lapply(current_scheme, as.character)
  
  # Count the number of each level of influence for each response
  current_scheme$`High Influence` <- 
    rowSums(current_scheme == "high influence", na.rm = TRUE)
  current_scheme$`Low Influence` <- 
    rowSums(current_scheme == "low influence", na.rm = TRUE)
  current_scheme$`No Influence` <- 
    rowSums(current_scheme == "no influence", na.rm = TRUE)
  
  # Keeping columns with totals
  current_scheme <- current_scheme %>%
    select(c(913, 914, 915))
  
  # Add up the total levels of influence
  current_scheme <- rownames_to_column(current_scheme, var = "Scheme")
  current_scheme <- current_scheme %>%
    adorn_totals(current_scheme, where = "row", name = paste0(df_iterate[i,2]))
  
  # Keep only these totals
  rownames(current_scheme) <- current_scheme$Scheme
  current_scheme$Scheme <- NULL
  current_scheme <- current_scheme[-c(1:4),]
  
  df_schemes <- bind_rows(df_schemes, current_scheme)
}

# Convert to pivot table
schemes_pivot <- 
  data.frame(as.table(as.matrix(df_schemes, row.names = 1)))
colnames(schemes_pivot) <- c("Scheme", "Influence Level", "Freq")

# Append proportions onto the end of the pivot table
schemes_pivot1 <- schemes_pivot %>%
  mutate(`Total per scheme` = ave(Freq, Scheme, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per scheme`))

#Create a dataframe that orders changes in terms of level of influence
# and appends this onto main pivot table
reordered_scheme_data <- schemes_pivot1 %>%
  filter(`Influence Level` %in% c("High Influence")) %>%
  group_by(Scheme) %>%
  summarise(Total_High = sum(Proportion, na.rm = TRUE)) %>%
  arrange(Total_High)  # Sort in descending order
schemes_pivot1 <- schemes_pivot1 %>%
  mutate(Scheme = factor(Scheme, levels = reordered_scheme_data$Scheme))

# THIS SECTION ADDS AN EXTRA COLUMN FOR AVERAGE INFLUENCE ACROSS ALL SCHEMES

# Append totals for each level of influence onto dataframe
new_scheme_df <- rownames_to_column(df_schemes, var = "Scheme")
new_scheme_df <- 
  adorn_totals(new_scheme_df, where = "row", name = "All schemes")
rownames(new_scheme_df) <- new_scheme_df$Scheme
new_scheme_df$Scheme <- NULL
new_scheme_df <- new_scheme_df[-c(1:10),]

# Create a pivot table for these totals
new_scheme_pivot <- 
  data.frame(as.table(as.matrix(new_scheme_df, row.names = 1)))
colnames(new_scheme_pivot) <- c("Scheme", "Influence Level", "Freq")

# Append proportions onto the end of the pivot table
new_scheme_pivot1 <- new_scheme_pivot %>%
  mutate(`Total per scheme` = ave(Freq, Scheme, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per scheme`))

# Bind the totals pivot together with the original pivot table
# (The pivot table with changes separated)
combined_scheme_pivot <- bind_rows(schemes_pivot1, new_scheme_pivot1)

#################################################
# REMOVE "I AM A PART OF THE SCHEME" RESPONSES
#################################################


# Filter for question
current_question <- data %>% 
  select(starts_with("q20_")) %>%
  select(-c(1:10))

# Remove "_influence" from each column heading
colnames(current_question) <- 
  str_remove(colnames(current_question), "_influence$")

# Split data into separate data frames based on scheme
split_dfs <- setNames(lapply(group_names, function(group) {
  current_question %>% select(matches(paste0("_", group, "$")))
}), group_names)

names(split_dfs) <- paste0("influence_", group_names)
list2env(split_dfs, env = .GlobalEnv)

# Create master dataframe to bind individual scheme data to
df_schemes <- data.frame(
)

for (i in 1:nrow(df_iterate)) {
  current_scheme <- get(df_iterate[i,1])
  
  current_scheme <- as.data.frame(t(current_scheme))
  
  current_scheme <- current_scheme %>%
    mutate("High Influence" = NA, "Low Influence" = NA, "No Influence" = NA)
  
  # Add counts for each level of influence for each response
  current_scheme[] <- lapply(current_scheme, as.character)
  
  # Count the number of each level of influence for each response
  current_scheme$`High Influence` <- 
    rowSums(current_scheme == "high influence", na.rm = TRUE)
  current_scheme$`Low Influence` <- 
    rowSums(current_scheme == "low influence", na.rm = TRUE)
  current_scheme$`No Influence` <- 
    rowSums(current_scheme == "no influence", na.rm = TRUE)
  
  # Keeping columns with totals
  current_scheme <- current_scheme %>%
    select(c(913, 914, 915))
  
  # Add up the total levels of influence
  current_scheme <- rownames_to_column(current_scheme, var = "Scheme")
  current_scheme <- current_scheme %>%
    adorn_totals(current_scheme, where = "row", name = paste0(df_iterate[i,2]))
  
  # Keep only these totals
  rownames(current_scheme) <- current_scheme$Scheme
  current_scheme$Scheme <- NULL
  current_scheme <- current_scheme[-c(1:3),]
  
  df_schemes <- bind_rows(df_schemes, current_scheme)
}

# Convert to pivot table
schemes_pivot <- 
  data.frame(as.table(as.matrix(df_schemes, row.names = 1)))
colnames(schemes_pivot) <- c("Scheme", "Influence Level", "Freq")

# Append proportions onto the end of the pivot table
schemes_pivot1 <- schemes_pivot %>%
  mutate(`Total per scheme` = ave(Freq, Scheme, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per scheme`))

#Create a dataframe that orders changes in terms of level of influence
# and appends this onto main pivot table
reordered_scheme_data <- schemes_pivot1 %>%
  filter(`Influence Level` %in% c("High Influence")) %>%
  group_by(Scheme) %>%
  summarise(Total_High = sum(Proportion, na.rm = TRUE)) %>%
  arrange(Total_High)  # Sort in descending order
schemes_pivot1 <- schemes_pivot1 %>%
  mutate(Scheme = factor(Scheme, levels = reordered_scheme_data$Scheme))

# THIS SECTION ADDS AN EXTRA COLUMN FOR AVERAGE INFLUENCE ACROSS ALL SCHEMES

# Append totals for each level of influence onto dataframe
new_scheme_df <- rownames_to_column(df_schemes, var = "Scheme")
new_scheme_df <- 
  adorn_totals(new_scheme_df, where = "row", name = "All schemes")
rownames(new_scheme_df) <- new_scheme_df$Scheme
new_scheme_df$Scheme <- NULL
new_scheme_df <- new_scheme_df[-c(1:10),]

# Create a pivot table for these totals
new_scheme_pivot <- 
  data.frame(as.table(as.matrix(new_scheme_df, row.names = 1)))
colnames(new_scheme_pivot) <- c("Scheme", "Influence Level", "Freq")

# Append proportions onto the end of the pivot table
new_scheme_pivot1 <- new_scheme_pivot %>%
  mutate(`Total per scheme` = ave(Freq, Scheme, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per scheme`))

# Bind the totals pivot together with the original pivot table
# (The pivot table with changes separated)
combined_scheme_pivot_only_applied <- bind_rows(schemes_pivot1, new_scheme_pivot1)

ggplot(combined_scheme_pivot_only_applied, aes(x = Scheme, y = Proportion, fill = `Influence Level`)) + 
  geom_bar(stat = "identity", position = "stack") + expand_limits(y = 1) + 
  coord_flip() + 
  labs(
    title = str_wrap(
      "Schemes where respondents had applied, or were planning to apply, and the level of influence support received through the resilience fund had on that decision", 50),
    x = "Change made", y = "Percentage") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
            position = position_stack(vjust = 0.5), colour = "white") +
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
  )
# Save to .jpg
ggsave("combined_influence_on_schemes_only_applied.jpg", width = 11, height = 8)


# ITERATION TO PLOT INFLUENCE ON SCHEME APPLICATION GRAPHS ---------------------

scheme_plot_iterate <- data.frame(
  scheme_data = c("combined_scheme_pivot", "combined_scheme_pivot_only_applied"),
  file_name = c("combined_influence_on_schemes.jpg", 
                "combined_influence_on_schemes_only_applied.jpg"),
  graph_name = 
    c("Schemes beneficiaries were a part of, had applied or were planning to apply to, and the level of influence support received through the Resilience fund had on that decision",
      "Schemes where respondents had applied, or were planning to apply, and the level of influence support received through the Resilience fund had on that decision")
)

for (i in 1:nrow(scheme_plot_iterate)) {
  to_plot <- get(scheme_plot_iterate[i,1])
  
  # Plot
  ggplot(to_plot, aes(x = Scheme, y = Proportion, fill = `Influence Level`)) + 
    geom_bar(stat = "identity", position = "stack") + expand_limits(y = 1) + 
    coord_flip() + 
    labs(
      title = str_wrap(paste0(scheme_plot_iterate[i,3]), 50),
      x = "Scheme", y = "Percentage") + 
    scale_x_discrete(labels = label_wrap(30)) + 
    scale_y_continuous(labels = scales::percent) + 
    geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
              position = position_stack(vjust = 0.5), colour = "white") +
    theme(axis.title.y = element_text(angle = 90, vjust = 2),
          panel.grid.major.y = element_line(colour = "white"),
          panel.grid.major.x = element_line(colour = "grey")
    )
  # Save to .jpg
  ggsave(paste0(scheme_plot_iterate[i,2]), width = 11, height = 8)
}
