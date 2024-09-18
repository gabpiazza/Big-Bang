#' ---
#' title: 02_analysis_A
#' author: Gabriele Piazza
#' date: 2024-06-20
#' Description: This scripts does some descriptive analysis
#' The data was created using scripts 01_dataprep_A,B,C,D
#' 
# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign","xtable" ,"knitr","kableExtra","haven", "fuzzyjoin", "data.table","panelView", "visdat", "beepr", "lubridate", "readxl") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)
## 1.2 Create functions ----------------------------------------------------
`%notin%` <- Negate(`%in%`)




## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data folders 
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"

full_panel_file <- "full_panel"
#Load the data
# top_1_percent_threshold <- quantile(full_panel$number_applications, 0.99)
# top_1_percent_companies <- full_panel %>%
#   filter(number_applications > top_1_percent_threshold) %>%
#   select(bvd_id_number) %>% distinct() %>% 
#   pull(bvd_id_number)
# full_panel<-full_panel %>% filter(bvd_id_number %notin% top_1_percent_companies)
full_panel<- readRDS(paste0(data_proc_dir, full_panel_file))
fr_it_es_uk_orders<- readRDS(paste0(data_proc_dir,"fr_it_es_uk_orders"))
# ## 3. Descriptive -------------------------------------------------------

number_cohorts <- full_panel %>%select(bvd_id_number, first_order) %>%
  distinct() %>% 
  group_by(first_order) %>% 
  dplyr::summarize(number = n()) %>% 
  distinct() %>% 
  mutate(cohort = paste0("Cohort", " ", first_order)) %>% 
  select(-first_order)

number_cohorts$cohort[number_cohorts$cohort=="Cohort 0"]<- "Never treated"

# Adding broader cohorts for grouping (e.g., 5-year periods)
number_cohorts <-number_cohorts %>%
  mutate(broader_cohort = case_when(
    cohort == "Never treated" ~ "Never treated",
    cohort %in% c("Cohort 1995", "Cohort 1996", "Cohort 1997", "Cohort 1998", "Cohort 1999") ~ "1995-1999",
    cohort %in% c("Cohort 2000", "Cohort 2001", "Cohort 2002", "Cohort 2003", "Cohort 2004") ~ "2000-2004",
    cohort %in% c("Cohort 2005", "Cohort 2006", "Cohort 2007", "Cohort 2008", "Cohort 2009") ~ "2005-2009",
    cohort %in% c("Cohort 2010", "Cohort 2011", "Cohort 2012", "Cohort 2013", "Cohort 2014") ~ "2010-2014",
    cohort %in% c("Cohort 2015", "Cohort 2016", "Cohort 2017", "Cohort 2018", "Cohort 2019", "Cohort 2020") ~ "2015-2020"
  ))

# Summarize data by broader cohorts
collapsed_data <- number_cohorts %>%
  group_by(broader_cohort) %>%
  summarise(number_of_firms = sum(number)) %>%
  mutate(percent_of_firms_in_cohort = (number_of_firms/sum(number_of_firms)) * 100)

# Convert percentage to string with percentage symbol
collapsed_data$percent_of_firms_in_cohort <- paste0(round(as.numeric(collapsed_data$percent_of_firms_in_cohort), 2), "%")

# Convert to xtable
xtable_data <- xtable(collapsed_data, caption = "Table 1: Number of firms by treatment cohort timing group (Collapsed)")


# Save the LaTeX code to a file
latex_code <- capture.output(print(xtable_data, include.rownames = FALSE, booktabs = TRUE, caption.placement = "top", 
                                   sanitize.text.function = function(x) {x}, add.to.row = list(pos = list(nrow(collapsed_data)), 
                                                                                               command = "\\hline \n \\multicolumn{3}{l}{\\textit{Note: Years 2008, 1995, 2020 have fewer than 20 observations.}} \n")))

# Write the LaTeX code to a .tex file
writeLines(latex_code, "table_1.tex")



%

# Balance table -----------------------------------------------------------

# Assuming your data frame is called full_panel
mean_results <- full_panel %>%
  group_by(supplier_status) %>%
  summarise(
    mean_pre_log_fixed_assets = mean(pre_log_fixed_assets, na.rm = TRUE),
    mean_pre_log_operating_revenue_turnover = mean(pre_log_operating_revenue_turnover, na.rm = TRUE),
    mean_pre_log_application_stock = mean(pre_log_application_stock, na.rm = TRUE)
  )

# Perform t-tests
t_test_fixed_assets <- t.test(pre_log_fixed_assets ~ supplier_status, data = full_panel)
t_test_operating_revenue <- t.test(pre_log_operating_revenue_turnover ~ supplier_status, data = full_panel)
t_test_patent_stock <- t.test(pre_log_application_stock ~ supplier_status, data = full_panel)


# Extract means from the summary for both groups (assuming binary groups: 0 and 1)
means_0 <- mean_results %>% filter(supplier_status == 0)
means_1 <- mean_results %>% filter(supplier_status == 1)
# Function to add asterisks based on significance
add_significance <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}
# Create a combined table with p-values and means
combined_results <- data.frame(
  Variable = c("Pre-Log Fixed Assets", "Pre-Log Operating Revenue Turnover", "Pre-Log Application Stock"),
  Mean_Group_0 = c(means_0$mean_pre_log_fixed_assets, means_0$mean_pre_log_operating_revenue_turnover, means_0$mean_pre_log_application_stock),
  Mean_Group_1 = c(means_1$mean_pre_log_fixed_assets, means_1$mean_pre_log_operating_revenue_turnover, means_1$mean_pre_log_application_stock),
  P_Value = c(
    round(t_test_fixed_assets$p.value, 2),
    round(t_test_operating_revenue$p.value, 2),
    round(t_test_patent_stock$p.value, 2)
  ),
  Significance = c(
    add_significance(t_test_fixed_assets$p.value),
    add_significance(t_test_operating_revenue$p.value),
    add_significance(t_test_patent_stock$p.value)
  )
)

# Combine P_Value and Significance
combined_results$P_Value <- paste0(combined_results$P_Value, combined_results$Significance)

# Remove the Significance column as it is now merged with P_Value
combined_results <- combined_results %>% select(-Significance)

# Print the table
print(combined_results)
# Convert to LaTeX table
latex_table <- xtable(combined_results, caption = "Means and P-Values from T-Tests by Supplier Status", label = "table:means_pvalues")
print(latex_table, type = "latex", include.rownames = FALSE, caption.placement = "top")


# Treatment panel ---------------------------------------------------------


treatment_panel <- full_panel %>% filter(supplier_status ==1)%>% 
  mutate(treatment_timing = case_when(
    first_order < 2009 ~ "Early treated",
    first_order >= 2009 ~ "Late treated",
    TRUE ~ NA_character_  # Handle any other unexpected cases
  ))


# Calculate means by treatment timing
mean_results_treatment <- treatment_panel %>%
  group_by(treatment_timing) %>%
  summarise(
    mean_pre_log_fixed_assets = round(mean(pre_log_fixed_assets, na.rm = TRUE), 2),
    mean_pre_log_operating_revenue_turnover = round(mean(pre_log_operating_revenue_turnover, na.rm = TRUE), 2),
    mean_pre_log_patent_stock = round(mean(pre_log_application_stock, na.rm = TRUE), 2),
    mean_log_application_stock = round(mean(log_application_stock, na.rm = TRUE), 2)
  )
# Perform t-tests
t_test_fixed_assets_treatment <- t.test(pre_log_fixed_assets ~ treatment_timing, data = treatment_panel)
t_test_operating_revenue_treatment <- t.test(pre_log_operating_revenue_turnover ~ treatment_timing, data = treatment_panel)
t_test_patent_stock_treatment <- t.test(pre_log_application_stock ~ treatment_timing, data = treatment_panel)
t_test_application_stock_treatment <- t.test(log_application_stock ~ treatment_timing, data = treatment_panel)


# Extract means from the summary for both groups (assuming binary groups: Early treated and Late treated)
means_early_treatment <- mean_results_treatment %>% filter(treatment_timing == "Early treated")
means_late_treatment <- mean_results_treatment %>% filter(treatment_timing == "Late treated")

# Function to add asterisks based on significance
add_significance <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Create a combined table with p-values and means
combined_results_treatment <- data.frame(
  Variable = c("Pre-Log Fixed Assets", "Pre-Log Operating Revenue Turnover", "Pre-Log Patent Stock", "Log Application Stock"),
  Mean_Early_Treated = c(means_early_treatment$mean_pre_log_fixed_assets, means_early_treatment$mean_pre_log_operating_revenue_turnover, means_early_treatment$mean_pre_log_patent_stock, means_early_treatment$mean_log_application_stock),
  Mean_Late_Treated = c(means_late_treatment$mean_pre_log_fixed_assets, means_late_treatment$mean_pre_log_operating_revenue_turnover, means_late_treatment$mean_pre_log_patent_stock, means_late_treatment$mean_log_application_stock),
  P_Value = c(
    round(t_test_fixed_assets_treatment$p.value, 2),
    round(t_test_operating_revenue_treatment$p.value, 2),
    round(t_test_patent_stock_treatment$p.value, 2),
    round(t_test_application_stock_treatment$p.value, 2)
  ),
  Significance = c(
    add_significance(t_test_fixed_assets_treatment$p.value),
    add_significance(t_test_operating_revenue_treatment$p.value),
    add_significance(t_test_patent_stock_treatment$p.value),
    add_significance(t_test_application_stock_treatment$p.value)
  )
)

# Combine P_Value and Significance
combined_results_treatment$P_Value <- paste0(combined_results_treatment$P_Value, combined_results_treatment$Significance)

# Print the table
print(combined_results_treatment)
# Convert to LaTeX table
latex_table_treatment <- xtable(combined_results_treatment, caption = "Means and P-Values from T-Tests by Treatment Timing", label = "table:means_pvalues_treatment_timing")
print(latex_table_treatment, type = "latex", include.rownames = FALSE, caption.placement = "top")

##3.1. Just Retour Orders ------------------------------------------------------
#Calculate the orders by country 

orders_by_country_tech<- fr_it_es_uk_orders %>% 
  group_by(country, order_date, tech_level) %>% 
  summarise(number_orders = n_distinct(order_number),
            total_amount = sum(chf_amount))
orders_by_country_tech_wide <- orders_by_country_tech %>%
  pivot_wider(names_from = tech_level, 
              values_from = c(number_orders, total_amount),
              names_sep = "_") %>% 
  rename(number_orders_low_tech = number_orders_0, 
         number_orders_high_tech = number_orders_1, 
         total_amount_low_tech = total_amount_0, 
         total_amount_high_tech = total_amount_1) %>% 
  replace_na(list(number_orders_low_tech = 0, number_orders_high_tech = 0,
                  total_amount_low_tech=0,total_amount_high_tech=0)) %>% 
  mutate(number_orders = number_orders_low_tech+number_orders_high_tech, 
         total_amount = total_amount_low_tech+total_amount_high_tech)




return_by_year_and_country<- fr_it_es_uk_orders %>% 
  select(country, order_date, coefficient_return_supplies) %>% 
  distinct()

just_retour_panel<- orders_by_country_tech_wide %>% left_join(return_by_year_and_country)
just_retour_panel<- just_retour_panel %>% drop_na()
just_retour_panel<-just_retour_panel %>% 
arrange(country, order_date) %>%  # Ensure data is sorted by country and order_date
  group_by(country) %>%             # Group by country to calculate lag within each country
  mutate(lag_coefficient = lag(coefficient_return_supplies)) %>%
  ungroup()

scatter_plot_orders <- ggplot(just_retour_panel, aes(x = coefficient_return_supplies, y = number_orders)) +
  geom_point() +geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_vline(xintercept = 0.9, linetype = "dotted", color = "red", size = 1) +  # Adding the dotted line
  labs(title = "Scatter Plot of Coefficient vs Total Number of Orders",
       x = "Return coefficient",
       y = "Total Number of Orders") +
  theme_minimal()


# Larger contracts --------------------------------------------------------


orders_by_country_tech_large<- fr_it_es_uk_orders %>% 
  group_by(country, order_date, tech_level) %>% 
  filter(sum(chf_amount) >=200000) %>% 
  summarise(number_orders = n_distinct(order_number),
            total_amount = sum(chf_amount))
orders_by_country_tech_large_wide <- orders_by_country_tech_large %>%
  pivot_wider(names_from = tech_level, 
              values_from = c(number_orders, total_amount),
              names_sep = "_") %>% 
  rename(number_orders_low_tech = number_orders_0, 
         number_orders_high_tech = number_orders_1, 
         total_amount_low_tech = total_amount_0, 
         total_amount_high_tech = total_amount_1) %>% 
  replace_na(list(number_orders_low_tech = 0, number_orders_high_tech = 0,
                  total_amount_low_tech=0,total_amount_high_tech=0)) %>% 
  mutate(number_orders = number_orders_low_tech+number_orders_high_tech, 
         total_amount = total_amount_low_tech+total_amount_high_tech)




return_by_year_and_country<- fr_it_es_uk_orders %>% 
  select(country, order_date, coefficient_return_supplies) %>% 
  distinct()

just_retour_panel_large<- orders_by_country_tech_large_wide %>% left_join(return_by_year_and_country)
just_retour_panel_large<- just_retour_panel_large %>% drop_na()
just_retour_panel_large<-just_retour_panel_large %>% 
  arrange(country, order_date) %>%  # Ensure data is sorted by country and order_date
  group_by(country) %>%             # Group by country to calculate lag within each country
  mutate(lag_coefficient = lag(coefficient_return_supplies)) %>%
  ungroup()

scatter_plot_orders <- ggplot(just_retour_panel_large, aes(x = coefficient_return_supplies, y = number_orders)) +
  geom_point() +geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_vline(xintercept = 0.9, linetype = "dotted", color = "red", size = 1) +  # Adding the dotted line
  labs(title = "Scatter Plot of Coefficient vs Total Number of Orders",
       x = "Return coefficient",
       y = "Total Number of Orders") +
  theme_minimal()


# amount total orders -----------------------------------------------------

order_first_order_amount<- full_panel %>% filter(supplier_status ==1) %>% 
  select(bvd_id_number, first_order_amount, total_orders, total_orders_amount, first_order_tech, SME_status) %>% distinct() %>% 
  mutate(log_first_order_amount = log(first_order_amount), log_total_orders= log(total_orders), log_total_orders_amount = log(total_orders_amount))
order_first_order_amount$first_order_tech<- as.factor(order_first_order_amount$first_order_tech)
order_first_order_amount$SME_status<- as.factor(order_first_order_amount$SME_status)
order_first_order_amount <- order_first_order_amount %>%
  mutate(tech_category = case_when(
    first_order_tech == 0 ~ "low-tech",
    first_order_tech == 1 ~ "high-tech",
    TRUE ~ NA_character_  # Handle any other unexpected cases
  ))

post_log_patent_stock <- full_panel %>% filter(supplier_status == 1 & treat ==1) %>% 
  select(bvd_id_number, log_application_stock, year)
  

aes(x = log_application_stock)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Density Plot of Log Application Stock") +
  labs(x = "Log Application Stock", y = "Density") +
  theme_minimal()

ggplot(post_log_patent_stock, aes(x = log_application_stock)) +
  geom_density(fill = "blue", alpha = 0.5) +
  ggtitle("Density Plot of Log Application Stock") +
  labs(x = "Log Application Stock", y = "Density") +
  theme_minimal()

# Creating the density plot
# Adjusted density plot with explicit group mapping
density_plot_by technology <- ggplot(order_first_order_amount, aes(x = log_total_orders, fill = tech_category, color = tech_category, group = tech_category)) +
  geom_density(alpha = 0.5, adjust = 1.5) +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  ggtitle("First Order Amount by Technology") +
  labs(x = "First Order Amount", y = "Density") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Technology"), color = guide_legend(title = "Technology"))+
  annotate("text", x = Inf, y = Inf, label = "
                ", hjust = 1.2, vjust = 1.5, size = 3.5, color = "black")+ annotate("text", x = Inf, y = Inf, label = "
                Average High-tech Order: 322,918.5 CHF
                Average Low-tech Order: 180,334.7 CHF
                ", hjust = 1.2, vjust = 1.5, size = 3.5, color = "black")


average_order <- order_first_order_amount %>%
  mutate(first_order_tech = case_when(
    first_order_tech == 0 ~ "low-tech order",
    first_order_tech == 1 ~ "high-tech order",
    TRUE ~ as.character(first_order_tech)  # This line handles any unexpected values
  )) %>%
  group_by(first_order_tech) %>%
  summarise(
    average = mean(first_order_amount, na.rm = TRUE),  # na.rm = TRUE ensures that missing values are ignored
    minimum = min(first_order_amount, na.rm = TRUE),
    max_amount = max(first_order_amount, na.rm = TRUE)
  )

  




  scatter_plot_first_order_amount <-ggplot(order_first_order_amount, aes(x = log_first_order_amount, y = log_total_orders_amount)) +
  geom_point() +geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Coefficient vs Total Number of Orders",
       x = "Return coefficient",
       y = "Total Number of Orders") +
  theme_minimal()
  
industry_summary <- full_panel %>% group_by(nace_rev_2_main_section, supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number))

industry_summary <- industry_summary %>%
  mutate(supplier = case_when(
    supplier_status == 0 ~ "Potential Supplier",
    supplier_status == 1 ~ "Supplier",
    TRUE ~ NA_character_  # Handle any other unexpected cases
  ))
total_potential = sum(industry_summary$number_companies[industry_summary$supplier == "Potential Supplier"])
total_supplier = sum(industry_summary$number_companies[industry_summary$supplier == "Supplier"])

industry_summary<- industry_summary %>% select(-supplier_status) %>% distinct()
industry_summary<-industry_summary %>%  pivot_wider(names_from = supplier, values_from = number_companies, values_fill = list(number_companies = 0)) %>%
  mutate(
    Percent_Potential = round((`Potential Supplier` / total_potential) * 100,1),
    Percent_Supplier = round((`Supplier` / total_supplier)* 100,1)
  )
industry_summary<- industry_summary %>% select(-`Potential Supplier`, -Supplier)
latex_table <- xtable(industry_summary, caption = "Percentage of Firms by Supplier Category", label = "tab:supplier_percentages")
# Printing the table with the small text size command
print(latex_table, type = "latex", include.rownames = FALSE,
      sanitize.text.function = function(x) {paste("\\scriptsize", x)})  # Applies \scriptsize to all text elementsplies \small to all text elements