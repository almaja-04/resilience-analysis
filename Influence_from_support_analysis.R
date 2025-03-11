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
  rename = c("Webinar", "Discussion Group/Presentation", "One Farm Visit", 
             "Multiple Farm Visits", "Help Developing a Business Plan", 
             "Received a Report", "Carbon Audit", "Scheme Advice", 
             "Help With a Scheme Application")
)

df_support <- data.frame()
support_pivot_props_all <- data.frame()

# TABLE FOR LIKELIHOOD OF MAKING SPECIFIC CHANGES ------------------------------

for (i in 1:nrow(support_dataframe)) {
  col_name <- sym(support_dataframe[i,1])
  
  current_support <- data %>%
    select(support_dataframe[i,1], starts_with("q8_")) %>%
    filter(!!col_name == 1) %>%
    select(-"q8_other")
  
  names(current_support)[1] <- "Support Type"
  
  current_support <-  
    adorn_totals(current_support, where = "row", name = support_dataframe[i,2])
  current_support <- tail(current_support, 1)
  rownames(current_support) <- support_dataframe[i,2]
  current_support <- current_support[,-1]
  colnames(current_support) <- 
    c("Grow the business", "Reduce size of the business", 
      "Diversify into non-farming areas", "Increase productivity", 
      "Change core agricultural enterprises", 
      "Leave farming (retire or pass onto next generation)", 
      "Leave farming (other reasons)")
  
  df_support <- bind_rows(df_support, current_support)
  
  support_pivot <- data.frame(as.table(as.matrix(current_support, row.names = 1)))
  colnames(support_pivot) <- c("Support Type", "Change Made", "Freq")
  
  support_pivot_props <- support_pivot %>%
    mutate(`Total per support` = ave(Freq, `Support Type`, FUN = sum)) %>%
    mutate(Proportion = (Freq/`Total per support`))
  
  
  support_pivot_props_all <- 
    bind_rows(support_pivot_props_all, support_pivot_props)
  
}

# THIS SECTION ADDS AN EXTRA COLUMN FOR CHANGES ACROSS ALL SUPPORT TYPES

# Append totals onto dataframe
new_support_dataframe <- rownames_to_column(df_support, var = "Support Type")
new_support_dataframe <- 
  adorn_totals(new_support_dataframe, where = "row", name = "All Support Types")
rownames(new_support_dataframe) <- new_support_dataframe$Support
new_support_dataframe$`Support Type` <- NULL
new_support_dataframe <- tail(new_support_dataframe, 1)

# Create a pivot table for these totals
new_support_pivot <- 
  data.frame(as.table(as.matrix(new_support_dataframe, row.names = 1)))
colnames(new_support_pivot) <- c("Support Type", "Change Made", "Freq")

# Append proportions onto the end of the pivot table
new_support_pivot_proportion <- new_support_pivot %>%
  mutate(`Total per support` = ave(Freq, `Support Type`, FUN = sum)) %>%
  mutate(Proportion = (Freq/`Total per support`))

# Bind the totals pivot together with the original pivot table
# (The pivot table with changes separated)
combined_support_pivot <- bind_rows(new_support_pivot_proportion, 
                                    support_pivot_props_all)

# GRAPHS FOR LIKELIHOOD OF MAKING SPECIFIC CHANGES -----------------------------

# Count graph
ggplot(support_pivot_props_all, aes(x = reorder(`Support Type`, `Total per support`), 
                         y = Freq, fill = `Change Made`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + expand_limits(y = 630) + 
  labs(
    title = str_wrap(
      "Types of support beneficiaries received through the Resilience Fund, and the changes they went on to make", 50), 
    x = "Change made", y = "Count") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  geom_text(aes(label = Freq), position = position_stack(vjust=0.5), colour = "white") + 
  scale_fill_manual(values = 
                      c("#C8C8C8", "#F0C571", "#59A89C", "#0B81A2", "#E25759", 
                                 "#9D2C00", "#7E4794", "#36B700")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

ggsave("Support_and_changes.jpg", width = 11, height = 8)

# Percentage graph
ggplot(combined_support_pivot, aes(x = fct_rev(`Support Type`), y = Proportion, fill = `Change Made`)) + 
  geom_bar(stat = "identity", position = "stack") + 
  coord_flip() + expand_limits(y = 1) + 
  labs(
    title = str_wrap(
      "Types of support beneficiaries received through the Resilience Fund, and the changes they went on to make", 50), 
    x = "Support Type Received", y = "Percentage") + 
  scale_x_discrete(labels = label_wrap(30)) + 
  scale_y_continuous(labels = scales::percent) + 
  geom_text(aes(label = sprintf("%d%%", round(Proportion*100))), 
            position = position_stack(vjust=0.5), colour = "white") + 
  scale_fill_manual(values = 
                      c("#C8C8C8", "#F0C571", "#59A89C", "#0B81A2", "#E25759", 
                                 "#9D2C00", "#7E4794", "#36B700")) +
  theme(axis.title.y = element_text(angle = 90, vjust = 2),
        panel.grid.major.y = element_line(colour = "white"),
        panel.grid.major.x = element_line(colour = "grey")
        
  )

ggsave("Support_and_changes_percent.jpg", width = 15, height = 8)



# TABLE FOR LIKELIHOOD OF INFLUENCE ON SPECIFIC CHANGES ------------------------

#current_support <- data %>%
#  select("q2_webinars", starts_with("q9_")) %>%
#  filter(q2_webinars == 1)
