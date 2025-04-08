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

support_dataframe <- data.frame(
  q2 = c("q2_webinars", "q2_discussion_group", "q2_one_farm_visit", 
         "q2_multiple_farm_visit", "q2_business_plan", "q2_report", 
         "q2_carbon_audit", "q2_scheme_advice", "q2_scheme_application"),
  rename = c("Attended a Webinar", "Attended a Discussion Group/Presentation", 
             "Received One Farm Visit", "Received Multiple Farm Visits", 
             "Received Help Developing a Business Plan", "Received a Report", 
             "Received a Carbon Audit", "Received Scheme Advice", 
             "Received Help With a Scheme Application"),
  new_df = c("influence_webinars", "influence_discussion_group", 
             "influence_one_farm_visit", "influence_multiple_farm_visit", 
             "influence_business_plan", "influence_report", 
             "influence_carbon_audit", "influence_scheme_advice", 
             "influence_scheme_application"),
  rename_no = c("Didn't attend a Webinar", "No Discussion Group/Presentation", 
                "Didn't receive One Farm Visit", 
                "Didn't receive Multiple Farm Visits", 
                "Didn't receive Help Developing a Business Plan", 
                "Didn't Receive a Report", "No Carbon Audit", 
                "Didn't receive Scheme Advice", 
                "Didn't receive Help With a Scheme Application"),
  new_df_stats = c("count_influence_webinars", "count_influence_discussion_group", 
                   "count_influence_one_farm_visit", 
                   "count_influence_multiple_farm_visit", 
                   "count_influence_business_plan", "count_influence_report", 
                   "count_influence_carbon_audit", "count_influence_scheme_advice", 
                   "count_influence_scheme_application")
)

col_names <- 
  c("Support type", "Influence", "Freq", "Total responses", "Proportion")

influence_webinars <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_webinars) <- col_names
influence_discussion_group <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_discussion_group) <- col_names
influence_one_farm_visit <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_one_farm_visit) <- col_names
influence_multiple_farm_visit <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_multiple_farm_visit) <- col_names
influence_business_plan <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_business_plan) <- col_names
influence_report <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_report) <- col_names
influence_carbon_audit <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_carbon_audit) <- col_names
influence_scheme_advice <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_scheme_advice) <- col_names
influence_scheme_application <- data.frame(matrix(nrow = 4, ncol = 5))
colnames(influence_scheme_application) <- col_names

# CREATE A TABLE ---------------------------------------------------------------

# Iteration to calculate influence from people who did receive the support
for (i in 1:nrow(support_dataframe)) {
  col_name <- sym(support_dataframe[i,1])
  
  current_support_yes <- data %>%
    select(support_dataframe[i,1], "influence_level_q9") %>%
    filter(!!col_name == 1) %>%
    filter(influence_level_q9 != "") %>%
    mutate(across(1, ~ support_dataframe[i,2])) %>%
    select(-paste0(support_dataframe[i,1]))
  
  # Add columns to count each time frame
  current_support_yes <- as.data.frame(t(current_support_yes))
  current_support_yes <- current_support_yes %>%
    mutate("High influence" = NA, "Medium influence" = NA, 
           "Low influence" = NA, "No influence" = NA)
  
  # Add counts for each level of influence for each response
  current_support_yes[] <- lapply(current_support_yes, as.character)
  
  # Count the number of each influence
  current_support_yes$`High influence` <- 
    rowSums(current_support_yes == "high influence", na.rm = TRUE)
  current_support_yes$`Medium influence` <- 
    rowSums(current_support_yes == "medium influence", na.rm = TRUE)
  current_support_yes$`Low influence` <- 
    rowSums(current_support_yes == "low influence", na.rm = TRUE)
  current_support_yes$`No influence` <- 
    rowSums(current_support_yes == "no influence", na.rm = TRUE)
  
  # Keeping columns only with counts
  current_support_yes <- 
    current_support_yes[, tail(colnames(current_support_yes), 4)]
  
  # Change row index for clarity
  rownames(current_support_yes) <- paste0(support_dataframe[i,2])
  
  # Create a pivot table
  current_support_yes_pivot <- 
    data.frame(as.table(as.matrix(current_support_yes, row.names = 1)))
  colnames(current_support_yes_pivot) <- c("Support type", "Influence", "Freq")
  
  # Append proportions onto the end of the pivot table
  current_support_yes_pivot_props <- current_support_yes_pivot %>%
    mutate(`Total responses` = ave(Freq, `Support type`, FUN = sum)) %>%
    mutate(Proportion = (Freq/`Total responses`))
  
  # Bind this first dataframe onto master frame
  assign(support_dataframe[i,3], bind_rows(get(support_dataframe[i,3]), current_support_yes_pivot_props))
  
  current_support_no <- data %>%
    select(support_dataframe[i,1], "influence_level_q9") %>%
    filter(!!col_name == 0) %>%
    filter(influence_level_q9 != "") %>%
    mutate(across(1, ~ support_dataframe[i,2])) %>%
    select(-paste0(support_dataframe[i,1]))
  
  # Add columns to count each time frame
  current_support_no <- as.data.frame(t(current_support_no))
  current_support_no <- current_support_no %>%
    mutate("High influence" = NA, "Medium influence" = NA, 
           "Low influence" = NA, "No influence" = NA)
  
  # Add counts for each level of influence for each response
  current_support_no[] <- lapply(current_support_no, as.character)
  
  # Count the number of each influence
  current_support_no$`High influence` <- 
    rowSums(current_support_no == "high influence", na.rm = TRUE)
  current_support_no$`Medium influence` <- 
    rowSums(current_support_no == "medium influence", na.rm = TRUE)
  current_support_no$`Low influence` <- 
    rowSums(current_support_no == "low influence", na.rm = TRUE)
  current_support_no$`No influence` <- 
    rowSums(current_support_no == "no influence", na.rm = TRUE)
  
  # Keeping columns only with counts
  current_support_no <- 
    current_support_no[, tail(colnames(current_support_no), 4)]
  
  # Change row index for clarity
  rownames(current_support_no) <- paste0(support_dataframe[i,4])
  
  # Create a pivot table
  current_support_no_pivot <- 
    data.frame(as.table(as.matrix(current_support_no, row.names = 1)))
  colnames(current_support_no_pivot) <- c("Support type", "Influence", "Freq")
  
  # Append proportions onto the end of the pivot table
  current_support_no_pivot_props <- current_support_no_pivot %>%
    mutate(`Total responses` = ave(Freq, `Support type`, FUN = sum)) %>%
    mutate(Proportion = (Freq/`Total responses`))
  
  # Bind the two dataframes together for plotting
  assign(support_dataframe[i,3], bind_rows(get(support_dataframe[i,3]), current_support_no_pivot_props))
  assign(support_dataframe[i,3], get(support_dataframe[i,3])[-c(1:4), ])
  
  # Bind two dataframes together for statistical testing
  assign(support_dataframe[i,5], rbind(current_support_yes, current_support_no))
}

# PLOT GRAPHS ------------------------------------------------------------------

for (i in 1:nrow(support_dataframe)) {
  to_plot <- get(support_dataframe[i,3])
  
  current_graph <- ggplot(to_plot, 
         aes(x = fct_rev(`Support type`), y = Proportion, fill = `Influence`)) + 
    geom_bar(stat = "identity", position = "stack") + coord_flip() + 
    scale_y_continuous(labels = percent) +  scale_x_discrete(labels = label_wrap(15)) + 
    geom_text(aes(label = sprintf("%d%%", round(Proportion * 100))), 
              position = position_stack(vjust = 0.5), colour = "white") + 
    labs(title = str_wrap("How did receiving/not receiving this support type affect the overall influence of the Resilience fund on changes made?", 50), 
         x = "Support type", y = "Percentage") +
    theme(
      axis.title.y = element_text(angle = 90, vjust = 1), 
      plot.title = element_text(face = "bold", vjust = -2),
      panel.grid.major.y = element_line(colour = "white"),
      panel.grid.major.x = element_line(colour = "grey")
    )
  
  # Save to .jpg
  ggsave(paste0(support_dataframe[i,3], ".jpg"), width = 11, height = 8)
}

# STATS TESTING ----------------------------------------------------------------

for (i in 1:nrow(support_dataframe)) {
  chisq <- chisq.test(get(support_dataframe[i,5]))
  print(paste0("Chi-squared test: ", support_dataframe[i,2]))
  print(chisq)
  
  current_residuals <- as.data.frame(chisq$stdres)
  
  current_heatmap <- 
    pheatmap(current_residuals, cluster_rows = FALSE, cluster_cols = FALSE, 
             display_numbers = TRUE, main = "Standardised Residuals", 
             fontsize = 14, fontsize_number = 12)
  
  # Save to .jpg
  ggsave(paste0(support_dataframe[i,3], "_heatmap.jpg"), current_heatmap, width = 11, height = 8)
}













#  
#  names(current_support)[1] <- "Support Type"
#  
#  current_support <-  
#    adorn_totals(current_support_yes, where = "row", name = support_dataframe[i,2])
#  current_support <- tail(current_support, 1)
#  rownames(current_support) <- support_dataframe[i,2]
#  current_support <- current_support[,-1]
#  colnames(current_support) <- 
#    c("Grow the business", "Reduce size of the business", 
#      "Diversify into non-farming areas", "Increase productivity", 
#      "Change core agricultural enterprises", 
#      "Leave farming (retire or pass onto next generation)", 
#      "Leave farming (other reasons)")
#  
#  df_support <- bind_rows(df_support, current_support)
#  
#  support_pivot <- data.frame(as.table(as.matrix(current_support, row.names = 1)))
#  colnames(support_pivot) <- c("Support Type", "Change Made", "Freq")
#  
#  support_pivot_props <- support_pivot %>%
#    mutate(`Total per support` = ave(Freq, `Support Type`, FUN = sum)) %>%
#    mutate(Proportion = (Freq/`Total per support`))
#  
#  
#  support_pivot_props_all <- 
#    bind_rows(support_pivot_props_all, support_pivot_props)
#  
#}

# THIS SECTION ADDS AN EXTRA COLUMN FOR CHANGES ACROSS ALL SUPPORT TYPES

# Append totals onto dataframe
#new_support_dataframe <- rownames_to_column(df_support, var = "Support Type")
#new_support_dataframe <- 
#  adorn_totals(new_support_dataframe, where = "row", name = "All Support Types")
#rownames(new_support_dataframe) <- new_support_dataframe$Support
#new_support_dataframe$`Support Type` <- NULL
#new_support_dataframe <- tail(new_support_dataframe, 1)
#
# Create a pivot table for these totals
#new_support_pivot <- 
#  data.frame(as.table(as.matrix(new_support_dataframe, row.names = 1)))
#colnames(new_support_pivot) <- c("Support Type", "Change Made", "Freq")
#
# Append proportions onto the end of the pivot table
#new_support_pivot_proportion <- new_support_pivot %>%
#  mutate(`Total per support` = ave(Freq, `Support Type`, FUN = sum)) %>%
#  mutate(Proportion = (Freq/`Total per support`))

# Bind the totals pivot together with the original pivot table
# (The pivot table with changes separated)
#combined_support_pivot <- bind_rows(new_support_pivot_proportion, 
#                                    support_pivot_props_all)

# GRAPHS FOR LIKELIHOOD OF MAKING SPECIFIC CHANGES -----------------------------

# Count graph
#ggplot(support_pivot_props_all, aes(x = reorder(`Support Type`, `Total per support`), 
#                         y = Freq, fill = `Change Made`)) + 
#  geom_bar(stat = "identity", position = "stack") + 
#  coord_flip() + expand_limits(y = 630) + 
#  labs(
#    title = str_wrap(
#      "Types of support beneficiaries received through the Resilience Fund, and the changes they went on to make", 50), 
#    x = "Change made", y = "Count") + 
#  scale_x_discrete(labels = label_wrap(30)) + 
#  geom_text(aes(label = Freq), position = position_stack(vjust=0.5), colour = "white") + 
#  scale_fill_manual(values = 
#                      c("#C8C8C8", "#F0C571", "#59A89C", "#0B81A2", "#E25759", 
#                                 "#9D2C00", "#7E4794", "#36B700")) +
#  theme(axis.title.y = element_text(angle = 90, vjust = 2),
#        panel.grid.major.y = element_line(colour = "white"),
#        panel.grid.major.x = element_line(colour = "grey")
#        
#  )

#ggsave("Support_and_changes.jpg", width = 11, height = 8)

# Percentage graph
#ggplot(combined_support_pivot, aes(x = fct_rev(`Support Type`), y = Proportion, fill = `Change Made`)) + 
#  geom_bar(stat = "identity", position = "stack") + 
#  coord_flip() + expand_limits(y = 1) + 
#  labs(
#    title = str_wrap(
#      "Types of support beneficiaries received through the Resilience Fund, and the changes they went on to make", 50), 
#    x = "Support Type Received", y = "Percentage") + 
#  scale_x_discrete(labels = label_wrap(30)) + 
#  scale_y_continuous(labels = scales::percent) + 
#  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
#            position = position_stack(vjust=0.5), colour = "white") + 
#  scale_fill_manual(values = 
#                      c("#C8C8C8", "#F0C571", "#59A89C", "#0B81A2", "#E25759", 
#                                 "#9D2C00", "#7E4794", "#36B700")) +
#  theme(axis.title.y = element_text(angle = 90, vjust = 2),
#        panel.grid.major.y = element_line(colour = "white"),
#        panel.grid.major.x = element_line(colour = "grey")
        
#  )

#ggsave("Support_and_changes_percent.jpg", width = 15, height = 8)



# TABLE FOR LIKELIHOOD OF INFLUENCE ON SPECIFIC CHANGES ------------------------

#current_support <- data %>%
#  select("q2_webinars", starts_with("q9_")) %>%
#  filter(q2_webinars == 1)
