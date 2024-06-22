#' ---
#' title: 01_dataprep_d
#' author: Gabriele Piazza
#' date: 2024-06-01
#' Description: This script merged the suppliers and potential dataset and makes the last changes before the analysis

# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign", "haven", "fuzzyjoin", "data.table", "visdat", "lubridate", "readxl") # list packages needed
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
data_proc_dir<<- "~/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
file.exists(paste0(data_proc_dir, suppliers_file))
suppliers_file <- "full_panel_suppliers"
potential_suppliers_file <- "full_panel_potential_suppliers"
## 2.2 Load the data -------------------------------------------------------

full_panel_suppliers<- readRDS(paste0(data_proc_dir, suppliers_file))
full_panel_potential_suppliers<- readRDS(paste0(data_proc_dir, potential_suppliers_file))
# Check that there are no duplicates 
list_potential_suppiers<- unique(full_panel_potential_suppliers$bvd_id_number)
list_suppliers <- unique(full_panel_suppliers$bvd_id_number)

full_panel_suppliers<- full_panel_suppliers %>% filter(bvd_id_number %notin% list_potential_suppiers)
full_panel_potential_suppliers<- full_panel_potential_suppliers %>% filter(bvd_id_number %notin% list_suppliers)

number_suppliers<- unique(full_panel_suppliers$bvd_id_number) # this gives 1311 suppliers
number_potential_suppliers<- unique(full_panel_potential_suppliers$bvd_id_number) # this gives 761 potential suppliers





# 3. Data Manipulation  ---------------------------------------------------


## 3.1 Create the same variables for the two dataframes-----------------------------------------

# Function to compare two dataframes
compare_dataframes <- function(df1, df2) {
  # Compare column names
  cols_only_in_df1 <- setdiff(names(df1), names(df2))
  cols_only_in_df2 <- setdiff(names(df2), names(df1))
  common_cols <- intersect(names(df1), names(df2))
  
  # Compare data types
  type_diffs <- data.frame(Column = character(), Type_df1 = character(), Type_df2 = character(), stringsAsFactors = FALSE)
  for (col in common_cols) {
    type_df1 <- typeof(df1[[col]])
    type_df2 <- typeof(df2[[col]])
    if (type_df1 != type_df2) {
      type_diffs <- rbind(type_diffs, data.frame(Column = col, Type_df1 = type_df1, Type_df2 = type_df2, stringsAsFactors = FALSE))
    }
  }
  
  # Compare contents
  content_diffs <- data.frame(Column = character(), stringsAsFactors = FALSE)
  for (col in common_cols) {
    if (!all(na.omit(df1[[col]]) == na.omit(df2[[col]]))) {
      content_diffs <- rbind(content_diffs, data.frame(Column = col, stringsAsFactors = FALSE))
    }
  }
  
  # Print results
  if (length(cols_only_in_df1) > 0) {
    cat("Columns only in df1:\n")
    print(cols_only_in_df1)
  } else {
    cat("No columns are exclusive to df1.\n")
  }
  
  if (length(cols_only_in_df2) > 0) {
    cat("Columns only in df2:\n")
    print(cols_only_in_df2)
  } else {
    cat("No columns are exclusive to df2.\n")
  }
  
  if (nrow(type_diffs) > 0) {
    cat("Columns with different data types:\n")
    print(type_diffs)
  } else {
    cat("No columns have different data types.\n")
  }
  
  if (nrow(content_diffs) > 0) {
    cat("Columns with different contents:\n")
    print(content_diffs)
  } else {
    cat("No columns have different contents.\n")
  }
}

compare_dataframes(full_panel_suppliers, full_panel_potential_suppliers)

# Columns only in df1:
#   [1] "company_name"             "country"                  "order_date"               "total_chf_amount_year"    "max_tech"                
# [6] "first_order"              "last_order"               "total_orders"             "total_orders_amount"      "first_order_amount"      
# [11] "first_order_tech"         "registration_first_order" "code_2_digits"            "subproject_first_year"    "subproject_1_first_year" 
# [16] "year_after_order"         "year_before_order"       
# Columns only in df2:
#   [1] "country.x"                     "country.y"                     "well_balanced"                 "tech_level"                   
# [5] "supplies_ms_status"            "year_after_registration"       "year_before_registration"      "registration_after_last_orbis"
# Columns with different data types:
#   Column  Type_df1 Type_df2
# 1 registration_year character   double

full_panel_potential_suppliers<- full_panel_potential_suppliers %>% 
  mutate(order_date = 0, last_order = 0, total_orders =0, total_chf_amount_year = 0,first_order = 0, total_orders =0, 
         total_orders_amount =0, first_order_amount = 0, first_order_tech = tech_level, max_tech = tech_level, code_2_digits = NA, 
         year_after_order = NA, year_before_order= NA, code_2_digits = NA, registration_first_order = NA, subproject_first_year = NA, 
         subproject_1_first_year = NA, year_after_order = NA, year_before_order = NA, supplier_status =0) %>% 
  rename(country= country.x) %>% 
  select(-country.y, -well_balanced, -tech_level, -supplies_ms_status, -year_after_registration, -year_before_registration, -registration_after_last_orbis) %>% distinct()

full_panel_suppliers<- full_panel_suppliers %>% select(-company_name) %>%
  mutate(supplier_status=1) %>% distinct()
full_panel<- rbind(full_panel_suppliers, full_panel_potential_suppliers)


##3.2 Create the treatment variables  -----------------------------------------

# Create a treatment variable
# Creating the treat variable based on the condition
full_panel <- full_panel %>%
  group_by(bvd_id_number) %>%
  mutate(treat = ifelse(first_order <= year, 1, 0)) %>%
  ungroup()  # Ungroup after the operation
# Creating the relative time 
full_panel<- full_panel %>% 
  mutate(time_to_treat = case_when(
    treat ==1 ~ year - first_order, 
    treat ==0 ~0
# Create a different treatment variable
  ))
full_panel<- full_panel %>% 
  mutate(first_order_2 = case_when(first_order >0 ~ first_order,
                                   first_order ==0 ~3000))# this is advised in other scripts for never treated



##3.3 Create the control variables ---------------------------------------
full_panel <- full_panel %>%
  mutate(
    registration_year = as.numeric(registration_year),
    incorporation_year = as.numeric(incorporation_year),
    first_order = as.numeric(first_order),
    first_year = as.numeric(first_year)
  )

# I have some missing values for incorporation year and therefore I use the first_year in the Oris data
full_panel <- full_panel %>%
  mutate(
    incorporation_year_age = if_else(is.na(incorporation_year) | incorporation_year > 2022, first_year, incorporation_year)
  )


full_panel<- full_panel %>%
  mutate(
    age = case_when(
      supplier_status == 1 ~ first_order - incorporation_year_age,
      supplier_status == 0 ~ registration_year - incorporation_year_age,
      TRUE ~ NA_real_   # default case to handle any other unexpected values
    )
  )

no_age_companies <- full_panel %>% filter(is.na(age)) %>% 
  dplyr::group_by(bvd_id_number) %>% distinct() %>% pull(bvd_id_number) # this is empty 

full_panel<- full_panel %>% 
  mutate(SME_status= case_when(
           size_classification %in% c("Small company","Medium sized company") ~1,
           size_classification %in% c("Large company","Very large company") ~0,
         ))

full_panel <- full_panel %>%
  group_by(bvd_id_number) %>% 
  mutate(year_diff_supplier_1 = year - first_order,# this is for suppliers
         year_diff_supplier_0 = year - registration_year,# this is for potential suppliers
         pre_log_operating_revenue_turnover = case_when(
           supplier_status == 1 & year_diff_supplier_1 == 0 ~ log_operating_revenue_turnover_,
           supplier_status == 0 & year_diff_supplier_0 == 0 ~ log_operating_revenue_turnover_,
           TRUE ~ NA_real_
         ),
         pre_log_ebitda = case_when(
           supplier_status == 1 & year_diff_supplier_1 == 0 ~ log_ebitda,
           supplier_status == 0 & year_diff_supplier_0 == 0 ~ log_ebitda,
           TRUE ~ NA_real_),
         pre_log_application_stock = case_when(
           supplier_status==1 & year_diff_supplier_1 == 0 ~ log_application_stock,
           supplier_status==0 & year_diff_supplier_0 == 0 ~ log_application_stock
         ),
         pre_log_fixed_assets = case_when(
           supplier_status == 1 & year_diff_supplier_1 == 0 ~ log_fixed_assets,
           supplier_status == 0 & year_diff_supplier_0 == 0 ~ log_fixed_assets,
           TRUE ~ NA_real_))%>%
  ungroup() %>% 
  arrange(bvd_id_number, year) %>%
  group_by(bvd_id_number) %>%
  fill(pre_log_operating_revenue_turnover, pre_log_ebitda, pre_log_application_stock,pre_log_fixed_assets, .direction = "downup") %>% # this fills up the entire colum
  ungroup()  # Remove the temporary columns

##3.4 Drop those observations with no post-treatment that are tre --------


# Step 1: Filter the dataset to include only rows where supplier_status is equal to 1
filtered_data <- full_panel %>%
  filter(supplier_status == 1)


# Step 2: Group by bvd_id_number and count the years where treat is equal to 0
treat_zero_counts <- filtered_data %>%
  group_by(bvd_id_number) %>%
  dplyr::summarize(count_treat_zero = sum(treat == 0, na.rm = TRUE))

# Step 3: Group by bvd_id_number and count the years where treat is equal to 1
treat_one_counts <- filtered_data %>%
  group_by(bvd_id_number) %>%
  dplyr::summarize(count_treat_one = sum(treat == 1, na.rm = TRUE))
# I want to exclude those treated that only have no treated years - the first order was received after they were inactive
# somethign wrong with matching or data
no_post_treatment <- treat_one_counts %>% filter(count_treat_one==0) %>% select(bvd_id_number) %>% 
  distinct() %>% 
  pull(bvd_id_number) 

grouped_data<- filtered_data %>% select(bvd_id_number, first_order) %>% left_join(treat_zero_counts) %>% 
 left_join(treat_one_counts) %>% distinct()
few_years <- grouped_data %>% filter(count_treat_zero==1 & count_treat_one==1) %>% 
  select(bvd_id_number) %>% distinct() %>% pull(bvd_id_number)

full_panel<- full_panel %>% filter(bvd_id_number %notin% c(no_post_treatment, few_years))


saveRDS(full_panel, paste0(data_proc_dir, "full_panel"))
