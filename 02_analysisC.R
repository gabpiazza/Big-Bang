#Info --------------------------------------------------------------------
  ##
  ##Script name: 02_analysis 
  ##
  ##Purpose of script: Preparing the procurement data for analaysis##
  ##Author: Gabriele Piazza
  ##
  ##Date Created: 2023-07-03
  ##
  ##Copyright (c) Gabriele Piazza, 2023
  ##Email: g.piazza@lse.ac.uk 
  ##

##
## Notes:
##   
##

#### 1. Load packages and functions----------------------------------------------------------

#####1.1 Packages --------------------------------------------------------

rm(list= ls())

install.packages("weights")
install.packages("cobalt")
install.packages("twang")
install.packages("gtable")
install.packages("scales")
install.packages("progress")
library(bacondecomp)
library(Matching)
library(panelView)
library(tjbal)
library(MatchIt)
library(WeightIt)
library(tidyverse)
library(rstatix)
library(bacondecomp)
library(ggpubr)
library(gsynth)
library(did)
library(modelsummary)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(janitor)
library(tmap)    # for static and interactive maps
library(leaflet)
library(sf)
library(terra)
library(cobalt)
library(here)
library(dplyr)
library(spData)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(tabulator)
library(Hmisc)
library(skimr)
library(tjbal)
library(fuzzyjoin)
library (tidyverse)
library(janitor)
library(reshape2)
library(easycsv)
library(Synth)
library(readxl)
library(plm)
library(progress)
library(lfe)
library(fixest)
library(did)
library(stringi)
if(!require(SCtools)) devtools::install_github("bcastanho/SCtools")
library(SCtools)
library(tidysynth)
library(modelsummary)
library(panelView)
library(eeptools)
library(here)
library(rdd)
library (haven)
library(sp)
library(spdep)
library(tidyverse)
library(forcats)
library(sf)
library(tmap)
library(gridExtra)
library(xtable)
library(panelView)
library(weights)
library(twang)
library(scales)
library(did)
library(fixest)
library(dplyr)
library(beepr)
library(naniar)

options(scipen=999) # to get rid of scientific notation

##### 1.2 Functions ------------------------------------------------------

'%notin%'<- Negate('%in%')
save_results <- function(object, filename) {
  filepath <- here("results","output", paste0(filename, ".rds"))
  saveRDS(object, filepath)
  cat(paste("Saved", filename, "to", filepath, "\n"))
}

# data <- data.frame(
#   ATT = "0.1101**",
#   Std.Error = "0.0491",
#   "Confidence Interval" = "0.0138 to 0.2064"
# )

# Start the PDF device
pdf("table_output.pdf", width = 8, height = 4)

# Create the table without row names
grid.table(data, rows = NULL)

# Turn off the device
dev.off()


#### 2. Load data  ----------------------------------------------------------
matched_potential_suppliers_orbis_final<- read_csv(here( "data_proc", "matched_potential_suppliers_final_.csv"))
matched_potential_suppliers_orbis_final<- matched_potential_suppliers_orbis_final %>% 
select(-'...1')
# 
matched_potential_suppliers_orbis_final$country_2<- substr(matched_potential_suppliers_orbis_final$bvd_id_number, start = 1, stop =2 ) # for some reason there are some zeros. I need to go back and understand why
matched_potential_suppliers_orbis_final$country<- matched_potential_suppliers_orbis_final$country_2
matched_potential_suppliers_orbis_final<-matched_potential_suppliers_orbis_final %>% distinct()
# is.pbalanced(matched_potential_suppliers_orbis_final)
# test<-matched_potential_suppliers_orbis_final
# test$unique_id <- paste(test$bvd_id_number, test$year)
# test$duplicate<- duplicated(test$unique_id)
# test <- subset(test, duplicate =="TRUE")
# test<- test %>% select(bvd_id_number, year, country, duplicate)






duplicates_test <- matched_potential_suppliers_orbis_final %>% filter(bvd_id_number=="ESA01011840")
# There are some duplicates becuase some firms have multiple  industry codes

# write.csv(matched_potential_suppliers_orbis_selected_age_20, here("data_proc", "matched_potential_suppliers_orbis_selected_age_20.csv")) 
# number_suppliers_potential_matched_orbis <- matched_potential_suppliers_orbis_final %>% 
#   select(bvd_id_number, supplier_status) %>% distinct() %>% 
#   dplyr::group_by(supplier_status) %>% 
#   dplyr::summarise(number_companies = n()) # 1,047 suppliers and 477 companies

 matched_potential_suppliers_orbis_selected<- read_csv(here("data_proc", "matched_potential_suppliers_orbis_selected_age_20.csv"))# Where does this data come from?
 matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
   select(-'...1')
matched_potential_suppliers_orbis_selected$country_2<- substr(matched_potential_suppliers_orbis_selected$bvd_id_number, start = 1, stop =2 ) # for some reason there are some zeros. I need to go back and understand why
matched_potential_suppliers_orbis_selected$country <- matched_potential_suppliers_orbis_selected$country_2
matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% select(-country_2)
matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% distinct()
# 
# # matched_potential_suppliers_orbis_final$country<- matched_potential_suppliers_orbis_final$country_2
# # matched_potential_suppliers_orbis_final<-matched_potential_suppliers_orbis_final %>% distinct()
# 
# unique(matched_potential_suppliers_orbis_selected_age_20$country)

# # These are just some summary statistics that show that there is an increase in patents after the treatment
# number_of_patents_pre_treatment<- matched_potential_suppliers_orbis_final %>% 
#   dplyr::group_by(postTreated, high_tech) %>% 
#   dplyr::summarize(number_applications = mean(num_applications))


## 3. Preparing the data for analysis --------------------------------------


# Create variables for the matching - pre-treatment covariates - this is only needed for the 
# There are a few companies that have more registration. I only consider the first one.
matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>%
  group_by(bvd_id_number) %>%
  filter(registration_year == min(registration_year, na.rm = TRUE)) %>%
  ungroup() %>% distinct()

matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>%
  group_by(bvd_id_number) %>% 
  mutate(year_diff_supplier_1 = year - first_order,# this is for suppliers
         year_diff_supplier_0 = year - registration_year,# this is for potential suppliers
         pre_operating_revenue_turnover = case_when(
           supplier_status == 1 & year_diff_supplier_1 == 0 ~ operating_revenue_turnover_,
           supplier_status == 0 & year_diff_supplier_0 == 0 ~ operating_revenue_turnover_,
           TRUE ~ NA_real_
         ),
         pre_ebitda = case_when(
           supplier_status == 1 & year_diff_supplier_1 == 0 ~ ebitda,
           supplier_status == 0 & year_diff_supplier_0 == 0 ~ ebitda,
           TRUE ~ NA_real_),
         pre_patent_stock = case_when(
           supplier_status==1 & year_diff_supplier_1 == 0 ~ patent_stock,
           supplier_status==0 & year_diff_supplier_0 == 0 ~ patent_stock
         ),
         pre_fixed_assets = case_when(
           supplier_status == 1 & year_diff_supplier_1 == 0 ~ fixed_assets,
           supplier_status == 0 & year_diff_supplier_0 == 0 ~ fixed_assets,
           TRUE ~ NA_real_))%>%
  ungroup() %>% 
  arrange(bvd_id_number, year) %>%
  group_by(bvd_id_number) %>%
  fill(pre_operating_revenue_turnover, pre_ebitda, pre_patent_stock,pre_fixed_assets, .direction = "downup") %>% # this fills up the entire colum
  ungroup()  # Remove the temporary columns



# These are just some checks to see the number of companies that I have
number_companies<- matched_potential_suppliers_orbis_selected %>% select(bvd_id_number) %>% distinct()# this gives 1,524 companies
check<- matched_potential_suppliers_orbis_selected %>% filter(is.na(pre_fixed_assets)) %>% 
  select(bvd_id_number) %>% distinct() # 141 companies do not have data: Do I get rid of these?


  # this gives me 141 companies
companies_no_pre_value <- unique(check$bvd_id_number) # this is 141 companies

# I exclude the companies that do not have a value at the time of the registration or treatment 
matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
  filter(bvd_id_number %notin% companies_no_pre_value) 


# I want to assign the high-tech variable to the order 
high_tech_order <- c(3,4,5,7)# I use the information provided by CSIL
low_tech_order<- c(1,2,6,0,9,8, NA) #


# I am doing this because I have duplicates - bvd_id_number with multiple three_digit_codes - I have duplicates
# matched_potential_suppliers_orbis_selected$one_digit_code <- str_sub(matched_potential_suppliers_orbis_selected$code_2_digits, start=1, end=1)# This gets the first digit
# matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>%
#   mutate(
#     tech_registration = case_when(
#       supplier_status == 1 ~ as.character(first_order_tech),# this is based on the first order they receive
#       supplier_status == 0 ~ as.character(one_digit_code), # this is based on the registration code
#       TRUE ~ NA_character_
#     )
#   )


# # I assign the variable tech low-teh
# matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
#   mutate(high_tech = case_when(tech_registration %in% high_tech_order ~1,
#                                       tech_registration %in% low_tech_order ~ 0))

# Here the issue is that I have registerd suppliers that have both high-tech and low-tech. I assign the high-tech for mostof them
matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>%
  group_by(bvd_id_number) %>%
  filter(high_tech == max(high_tech, na.rm = TRUE)) %>%
  ungroup()



# matched_potential_suppliers_orbis_selected$code_2_digits<- as.factor(matched_potential_suppliers_orbis_selected$code_2_digits) # This might be redundant
# I remove these variables as I have duplicates
# matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>% 
#   select(-one_digit_code, -code_2_digits) %>% distinct()
# 
# matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
#  mutate(postTreated = (first_order!=0) & year >= first_order)# Here I create the treatment variable





years_before_after<- matched_potential_suppliers_orbis_selected %>%
  mutate(
    # Assuming you have a column 'reference_date' to compare against
    year_difference = case_when(
      supplier_status == 0 ~ year- registration_year,
      supplier_status == 1 ~ year - first_order,
      TRUE ~ NA_integer_  # Default case to handle other situation
    ),
    period = case_when(
      year_difference < 0 ~ "Before",
      year_difference >= 0 ~ "After",
      TRUE ~ NA_character_  # Default case for period
    )
  ) %>%
  group_by(bvd_id_number, period) %>%
  summarise(count = n(), .groups = 'drop')

years_before_after_wide<- years_before_after %>% 
  pivot_wider(
    names_from = period, 
    values_from = count, 
    values_fill = list(count = 0)
  )

years_before_minimum <- years_before_after_wide %>% 
  filter(Before >4 ) %>% 
  select(bvd_id_number) %>% 
  distinct() %>% 
  pull(bvd_id_number)


matched_potential_suppliers_orbis_selected_5 <- matched_potential_suppliers_orbis_selected%>% 
  filter(bvd_id_number %in% years_before_minimum) %>% 
distinct()



check<- matched_potential_suppliers_orbis_selected_5 %>% dplyr::group_by(bvd_id_number) %>% # 1,206 comapnies
  dplyr::summarize(number_companies = n())
# # Update the dataframe
# matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>%
#   mutate(high_tech = ifelse(bvd_id_number %in% ids_with_both, 1, high_tech)) %>% distinct()

# # there are some issues with this. Some companies have different levels of patent stock. I go for thei highest number
# matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
#   group_by(bvd_id_number,year) %>%
#   filter(lag_patent_stock == max(lag_patent_stock, na.rm = TRUE),
#          patent_stock ==max(patent_stock, na.rm = TRUE) ) %>%
#   ungroup() %>% distinct()





## Create new variables
matched_potential_suppliers_orbis_selected_5$first_date<- pmin(matched_potential_suppliers_orbis_selected_5$incorporation_year, matched_potential_suppliers_orbis_selected_5$registration_year)
matched_potential_suppliers_orbis_selected_5<- matched_potential_suppliers_orbis_selected_5 %>% 
  mutate(age= case_when(
    supplier_status ==1~  first_order - first_date, 
    supplier_status ==0 ~ registration_year - first_date
  ))



check<- matched_potential_suppliers_orbis_selected_5 %>% select(bvd_id_number, year,first_date, age,num_applications,fixed_assets, first_year, registration_year,country, incorporation_year, first_order, treatment) %>% filter(country =="IT")
# matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
#   mutate(age = registration_year - incorporation_year,
#          treatment = if_else(year >= first_order, 1, 0),
#          log_patent_stock =log(patent_stock+0.00001),
#          log_pre_patent_stock = log(pre_patent_stock+0.00001))

no_age_companies <- matched_potential_suppliers_orbis_selected_5 %>% filter(is.na(age)) %>% 
  dplyr::group_by(bvd_id_number) %>% distinct() %>% pull(bvd_id_number) # this is 93 companies 





# # Extracting individual datasets from the list
# matched_potential_suppliers_1 <- list_of_datasets$`Group 1`
# matched_potential_suppliers_2 <- list_of_datasets$`Group 2`
# matched_potential_suppliers_3 <- list_of_datasets$`Group 3`
# matched_potential_suppliers_4 <- list_of_datasets$`Group 4`
# matched_potential_suppliers_5 <- list_of_datasets$`Group 5`
# 
# m <- dataset %>%
#   mutate(registration_group = cut(registration_year, 
#                                   breaks = 5, 
#                                   labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"),
#                                   include.lowest = TRUE))

# matched_potential_suppliers_orbis_selected<- matched_potential_suppliers_orbis_selected %>% 
#   filter(bvd_id_number %notin% no_age_companies)

# matched_potential_suppliers_orbis_selected <- matched_potential_suppliers_orbis_selected %>% 
#   mutate(log_patent_stock =log(patent_stock+0.00001),
#          log_pre_patent_stock = log(pre_patent_stock+0.00001))


check<- matched_potential_suppliers_orbis_selected_5%>% dplyr::group_by(bvd_id_number, year) %>% 
  dplyr::summarize(number = n())

check<- matched_potential_suppliers_orbis_selected_5%>% group_by(bvd_id_number) %>% 
  select(bvd_id_number) %>% distinct() %>% pull(bvd_id_number) # this is 1206 companies
matched_potential_suppliers_orbis_selected_5$bvd_id_numeric <- as.numeric(as.factor(matched_potential_suppliers_orbis_selected_5$bvd_id_number))

matched_potential_suppliers_orbis_selected_5<- matched_potential_suppliers_orbis_selected_5 %>% 
  mutate(first_order_2 = case_when(first_order >0 ~ first_order,
                                   first_order ==0 ~3000))# this is advised in other scripts for never treated


matched_potential_suppliers_orbis_selected_5<- matched_potential_suppliers_orbis_selected_5 %>% 
  select( -log_patent_stock,-pre_patent_stock) %>% distinct()

duplicates_check<- matched_potential_suppliers_orbis_selected_5 %>% dplyr::group_by(bvd_id_number, supplier_status) %>% 
  dplyr::summarize(number_observations= n()) %>% 
  select(bvd_id_number, supplier_status) %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarize(supplier = n_distinct(supplier_status))

check<- matched_potential_suppliers_orbis_selected_5 %>% filter(bvd_id_number=="GB00262938" & year==2005)
  


firms_by_size <- matched_potential_suppliers_orbis_selected_5 %>% 
  select(bvd_id_number, supplier_status, size_classification) %>% distinct() %>% 
  dplyr::group_by(size_classification) %>% 
  dplyr::summarize(number = n())


# I realized that the dependent variable, pantent stock, was not correctly calculated. Here I upload it and fix the dateset
panel_data_patents_1990_2019<- read_csv(here("data_proc", "panel_data_patents_1990_2019.csv"))
panel_data_patents_1990_2019<- panel_data_patents_1990_2019 %>% 
  select(-...1) %>% rename(year = year_orbis)

check_duplicates_year_orbis_patent<- panel_data_patents_1990_2019 %>% 
  dplyr::group_by(bvd_id_number, year) %>% 
  dplyr::summarize(number_observations = n())

matched_potential_suppliers_orbis_selected_5<- matched_potential_suppliers_orbis_selected_5 %>% 
  select( -num_applications, -num_publications, -cumulative_patents,-lag_cum_applications, -patent_stock) %>% distinct()

# patent_panel_data_1990_2019<- patent_panel_data_1990_2019 %>% 
# mutate(
#   num_applications = case_when(
#     is.na(num_applications) ~ 0,
#     TRUE ~ num_applications
#   ),
#   patent_stock = case_when(
#     is.na(patent_stock) ~ 0,
#     TRUE ~ patent_stock
#   )
# ) %>% select(-...1)

#There are a few issues with the the country code

full_panel<- matched_potential_suppliers_orbis_selected_5%>% 
  left_join(panel_data_patents_1990_2019, by = c("year", "bvd_id_number")) 

bvd_id_cern<- matched_potential_suppliers_orbis_selected_5 %>% 
  select(bvd_id_number) %>% distinct() %>% pull(bvd_id_number)

full_panel<- full_panel %>% filter(bvd_id_number %in% bvd_id_cern) %>% distinct() 

full_panel_first_date<- full_panel %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::filter(year>=first_date) %>% 
  ungroup()


is.pbalanced(full_panel_first_date
             )


# full_panel$country_2 <- substr(full_panel$bvd_id_number, start = 1, stop =2 )
# full_panel$country <- full_panel$country_2
# 
# balanced_check<- limited_panel %>% 
#   group_by(bvd_id_number, year) %>% 
#   summarise(n= n(), .groups = "drop") %>% 
#   ungroup() %>% 
#   complete(bvd_id_number, year, fill = list(n = 0)) %>%  # Fills in missing 'bvd_id_number'-'year' combinations with 0
#   count(bvd_id_number, name = "total_observations") %>%  # Counts total observations for each entity
#   mutate(is_balanced = total_observations == max(total_observations))
# 
# balanced_count <- sum(balanced_check$is_balanced)  # Sums up the number of balanced entities
# 
# 
# full_panel_test <- full_panel
# full_panel_test <- full_panel %>% 
#   select(-country) %>% distinct()
# 
# 
# check_duplicates<- full_panel %>% 
#   dplyr::group_by(bvd_id_number, year) %>% 
#   dplyr::summarize(number = n()) 


full_panel_first_date<- full_panel_first_date %>% 
  mutate(
    num_applications = case_when(
      is.na(num_applications) ~ 0,
      TRUE ~ num_applications
    ),
    patent_stock = case_when(
      is.na(patent_stock) ~ 0,
      TRUE ~ patent_stock
    )
  ) %>% distinct()



full_panel_first_date<- full_panel_first_date %>% group_by(bvd_id_number, year) %>% 
  mutate(log_patent_stock = asinh(patent_stock),
         log_pat_applications = log(num_applications+1),
         log_sine_application= asinh(num_applications),
         patented = ifelse(num_applications > 0, 1, 0)) %>% 
distinct() %>% 
    ungroup()



# Calculate the 1st and 99th percentiles for patent_stock
p1 <- quantile(full_panel_first_date$patent_stock, 0.01)
p99 <- quantile(full_panel_first_date$patent_stock, 0.99)

# Identify bvd_id_number for the top 1% and bottom 1% of patent_stock
outlier_ids <- full_panel_first_date %>%
  filter(patent_stock >= p99) %>%
  pull(bvd_id_number) %>%
  unique()

# Exclude these bvd_id_number from the dataset
full_panel_filtered <- full_panel_first_date %>%
  filter(bvd_id_number %notin% outlier_ids)


number_suppliers_potential_matched_orbis <- full_panel_first_date %>% 
  select(bvd_id_number, supplier_status) %>% distinct() %>% 
  dplyr::group_by(supplier_status) %>% 
  dplyr::summarise(number_companies = n()) # 1,047 suppliers and 477 companies

 SME_suppliers_potential_matched_orbis <- full_panel %>% 
  select(bvd_id_number, SME_status) %>% distinct() %>% 
  dplyr::group_by(SME_status) %>% 
  dplyr::summarise(number_companies = n()) # 1,047 suppliers and 477 companies

##Check data --------------------------------------------------------------

# Define the columns you want to check for missing observations




##4.  Callaway and Sant'anna estimators ------------------------------------------------

# change the variable to numeric for the did package
full_panel$bvd_id_numeric <-as.numeric(as.factor(full_panel$bvd_id_number))

years_few_observations <- c(1995)#c(2014,1997,1999)# this are years with fewer observations

full_panel<- full_panel %>%
  mutate(
    age = case_when(
      supplier_status == 1 ~ first_order - incorporation_year,
      supplier_status == 0 ~ registration_year - incorporation_year,
      TRUE ~ NA_real_   # default case to handle any other unexpected values
    )
  )

full_panel<- full_panel%>% 
  mutate(log_pre_turnover = asinh(pre_operating_revenue_turnover),
         log_pre_fixed_assets = asinh(pre_fixed_assets))


na_pre_turnover <- full_panel %>% filter(is.na(pre_operating_revenue_turnover)) %>% 
  select(bvd_id_number) %>% distinct() %>% pull(bvd_id_number)

full_panel<- full_panel %>% 
  filter(bvd_id_number %notin% na_pre_turnover)



full_panel<- full_panel %>% 
  mutate(log_age = asinh(age),
         SME_status= case_when(
           size_classification %in% c("Small company","Medium sized company") ~1,
           size_classification %in% c("Large company","Very large company") ~0,
         ))




# ### Create grouped data -------------------------------------------------

# Function to create a two-year period variable
get_two_year_period <- function(year) {
  return((year - 1990) %/% 2 * 2 + 1990)
}

# Function to group years by two
group_by_two_years <- function(year) {
  return(year - (year %% 2))
}

# Load the dataset
full_panel_grouped <- full_panel

full_panel_grouped$first_order_grouped<- sapply(full_panel_grouped$first_order, group_by_two_years)

# Applying the two-year period function to year and first_order
full_panel_grouped$year_period <- sapply(full_panel_grouped$year, get_two_year_period)
full_panel_grouped$first_order_period <- sapply(full_panel_grouped$first_order, get_two_year_period)


# Safe mean function to handle missing values and relog the mean
# Safe mean function to handle missing values and add a small number
safe_mean_small_number <- function(x, small_number = 1e-6) {
  x <- x + small_number # Adding a small number
  if (sum(!is.na(x)) == 1) {
    mean_val <- x[!is.na(x)]
  } else {
    mean_val <- mean(x, na.rm = TRUE)
  }
  return(mean_val)
}

# Safe mean function for non-logged variables
safe_mean <- function(x) {
  if (sum(!is.na(x)) == 1) {
    return(x[!is.na(x)])
  } else {
    return(mean(x, na.rm = TRUE))
  }
}

# Grouping by the two-year period and calculating averages for the specified variables
averages <- full_panel_grouped %>%
  group_by(year_period) %>%
  summarise(
    log_patent_stock_avg = log(safe_mean_small_number(patent_stock)), # Delogging, calculating mean, and relogging
    fixed_assets_avg = safe_mean(fixed_assets), # Calculating mean without delogging
    log_age_avg = log(safe_mean(age)), # Delogging, calculating mean, and relogging
    log_pat_applications_avg = log(safe_mean(log_pat_applications)+1), # Delogging, calculating mean, and relogging
    patented_max = max(patented, na.rm = TRUE)
  )


# Selecting non-time-varying variables and merging with the aggregated data
full_panel_grouped_final <- full_panel_grouped %>%
  select(bvd_id_numeric,high_tech, SME_status, size_classification, first_order_amount,first_order, country, total_orders, first_order_period,year_period) %>%
  distinct() %>%
  left_join(averages, by = c("year_period"))


full_panel_grouped_final <- full_panel_grouped_final %>%
  select(-year_grouped_1, - first_order_period_1)

# Load the dplyr package
library(dplyr)

# Get a vector of unique, sorted, non-zero year_period values
unique_years <- sort(unique(full_panel_grouped_final$year_period[full_panel_grouped_final$year_period > 0]))

# Create a mapping from unique year values to a sequence starting from 1
year_mapping <- setNames(seq_along(unique_years), unique_years)

# Apply this mapping to your dataset
full_panel_grouped_final <- full_panel_grouped_final %>%
  mutate(
    year_period_1 = ifelse(year_period > 0, year_mapping[year_period], 0),
    first_order_period_1 = ifelse(first_order_period > 0, year_mapping[first_order_period], 0)
  )
full_panel_grouped_final<- full_panel_grouped_final %>% distinct() #select(-first_order)

# Get a vector of unique, sorted, non-zero year_period values
unique_years <- sort(unique(full_panel_grouped_final$year_period[full_panel_grouped_final$year_period > 0]))

# Create a mapping from unique year values to a sequence starting from 1
year_mapping <- setNames(seq_along(unique_years), unique_years)

# Apply this mapping to your dataset
full_panel_grouped_final <- full_panel_grouped_final %>%
  mutate(
    year_period_1 = ifelse(year_period > 0, year_mapping[year_period], 0),
    first_order_period_1 = ifelse(first_order_period > 0, year_mapping[first_order_period], 0)
  )

y_vars<- c("log_patent_stock", "log_pat_applications","log_sine_application", "patented")

y_vars_2<- c("log_patent_stock_avg", "log_pat_applications_avg", "patented_max")

set.seed(123456789)

perform_analysis_2 <- function(y_var, control_group_type, dataset, covariates = NULL) {
  data_filtered <- dataset %>%
    filter(first_order %notin% years_few_observations)
  
  if (!is.null(covariates) && length(covariates) > 0) {
    xformla <- as.formula(paste( "~", paste(covariates, collapse = " + ")))
  } else {
    xformla <- as.formula(paste( "~ 1"))
    
  }
  
  # Expected column names
  expected_columns <- c("bvd_id_numeric", "year", y_var, "first_order_grouped")
  
  # Check for missing columns
  missing_columns <- expected_columns[!expected_columns %in% colnames(data_filtered)]
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  cs_results <- att_gt(
    yname = y_var,
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "first_order_grouped",
    xformla = xformla,
    data = data_filtered,
    control_group = control_group_type,
    est_method = "ipw",
    clustervars = "bvd_id_numeric", 
    pl=TRUE, 
    cores=8,
    panel = TRUE,
    biters = 3000,
    bstrap = T,
    allow_unbalanced_panel = TRUE
  )
  
  # You can add further calculations if needed.
  
  
  # Compute the different results
  cs_results.dyn <- aggte(cs_results, type = "dynamic", na.rm = TRUE, max_e = 15, min_e = -15)
  cs_results.sim <- aggte(cs_results, type = "simple", na.rm = TRUE)
  cs_results.grp <- aggte(cs_results, type = "group", na.rm = TRUE)
  cs_results.cal <- aggte(cs_results, type = "calendar", na.rm = TRUE)
  
  sim_ratio <- cs_results.sim$overall.att / cs_results.sim$overall.se
  
  # Visualization and confidence intervals for dynamic results
  cs_results_plot_dyn <- ggdid(cs_results.dyn)
  cs_results_data_dyn <- cs_results_plot_dyn$data
  cs_results_data_dyn$lower <- cs_results_data_dyn$att - cs_results_data_dyn$c * cs_results_data_dyn$att.se
  cs_results_data_dyn$upper <- cs_results_data_dyn$att + cs_results_data_dyn$c * cs_results_data_dyn$att.se
  
  # Create a list to store all the results
  results_list <- list(
    main = cs_results,
    dynamic = cs_results.dyn,
    simple = cs_results.sim,
    group = cs_results.grp,
    calendar = cs_results.cal,
    sim_ratio = sim_ratio,
    dynamic_plot_data = cs_results_data_dyn
  )
  
  return(results_list)
}




perform_analysis <- function(y_var, control_group_type, dataset, covariates = NULL) {
  data_filtered <- dataset %>%
filter(first_order %notin% years_few_observations)
   
  if (!is.null(covariates) && length(covariates) > 0) {
    xformla <- as.formula(paste( "~", paste(covariates, collapse = " + ")))
  } else {
    xformla <- as.formula(paste( "~ 1"))
    
  }
  
  # Expected column names
  expected_columns <- c("bvd_id_numeric", "year", y_var, "first_order")
  
  # Check for missing columns
  missing_columns <- expected_columns[!expected_columns %in% colnames(data_filtered)]
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  cs_results <- att_gt(
    yname = y_var,
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "first_order_2",
    xformla = xformla,
    data = data_filtered,
    control_group = control_group_type,
    est_method = "ipw",
    clustervars = "bvd_id_numeric", 
    pl=TRUE, 
    cores=8,
    bstrap = T,
    panel = TRUE,
    biters = 3000,
    allow_unbalanced_panel = TRUE,
  )
 
  # You can add further calculations if needed.
  
  
  # Compute the different results
  cs_results.dyn <- aggte(cs_results, type = "dynamic", na.rm = TRUE, max_e = 10, min_e = -10, cband = F)
  cs_results.sim <- aggte(cs_results, type = "simple", na.rm = TRUE)
  cs_results.grp <- aggte(cs_results, type = "group", na.rm = TRUE)
  cs_results.cal <- aggte(cs_results, type = "calendar", na.rm = TRUE)
  
  sim_ratio <- cs_results.sim$overall.att / cs_results.sim$overall.se
  
  # Visualization and confidence intervals for dynamic results
  cs_results_plot_dyn <- ggdid(cs_results.dyn)
  cs_results_data_dyn <- cs_results_plot_dyn$data
  cs_results_data_dyn$lower <- cs_results_data_dyn$att - cs_results_data_dyn$c * cs_results_data_dyn$att.se
  cs_results_data_dyn$upper <- cs_results_data_dyn$att + cs_results_data_dyn$c * cs_results_data_dyn$att.se
  
  # Create a list to store all the results
  results_list <- list(
    main = cs_results,
    dynamic = cs_results.dyn,
    simple = cs_results.sim,
    group = cs_results.grp,
    calendar = cs_results.cal,
    sim_ratio = sim_ratio,
    dynamic_plot_data = cs_results_data_dyn
  )
  
  return(results_list)
}

perform_analysis_placebo <- function(y_var, control_group_type, dataset, covariates = NULL) {
  data_filtered <- dataset %>%
    filter(first_order %notin% years_few_observations)
  
  if (!is.null(covariates) && length(covariates) > 0) {
    xformla <- as.formula(paste( "~", paste(covariates, collapse = " + ")))
  } else {
    xformla <- as.formula(paste( "~ 1"))
    
  }
  
  # Expected column names
  expected_columns <- c("bvd_id_numeric", "year", y_var, "first_order_2")
  
  # Check for missing columns
  missing_columns <- expected_columns[!expected_columns %in% colnames(data_filtered)]
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  cs_results <- att_gt(
    yname = y_var,
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "placebo_treat",
    xformla = xformla,
    data = data_filtered,
    control_group = control_group_type,
    est_method = "ipw",
    clustervars = "bvd_id_numeric", 
    pl=TRUE, 
    cores=8,
    bstrap = T,
    panel = TRUE,
    biters = 3000,
    allow_unbalanced_panel = TRUE,
  )
  
  # You can add further calculations if needed.
  
  
  # Compute the different results
  cs_results.dyn <- aggte(cs_results, type = "dynamic", na.rm = TRUE, max_e = 10, min_e = -10, cband = F)
  cs_results.sim <- aggte(cs_results, type = "simple", na.rm = TRUE)
  cs_results.grp <- aggte(cs_results, type = "group", na.rm = TRUE)
  cs_results.cal <- aggte(cs_results, type = "calendar", na.rm = TRUE)
  
  sim_ratio <- cs_results.sim$overall.att / cs_results.sim$overall.se
  
  # Visualization and confidence intervals for dynamic results
  cs_results_plot_dyn <- ggdid(cs_results.dyn)
  cs_results_data_dyn <- cs_results_plot_dyn$data
  cs_results_data_dyn$lower <- cs_results_data_dyn$att - cs_results_data_dyn$c * cs_results_data_dyn$att.se
  cs_results_data_dyn$upper <- cs_results_data_dyn$att + cs_results_data_dyn$c * cs_results_data_dyn$att.se
  
  # Create a list to store all the results
  results_list <- list(
    main = cs_results,
    dynamic = cs_results.dyn,
    simple = cs_results.sim,
    group = cs_results.grp,
    calendar = cs_results.cal,
    sim_ratio = sim_ratio,
    dynamic_plot_data = cs_results_data_dyn
  )
  
  return(results_list)
}



run_all_analyses_multiple_combinations <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c("pre_fixed_assets","log_pre_fixed_assets","log_age","log_pre_turnover",  "fixed_assets")
  
  covariate_combinations <- list("none" = NULL)
  
  # Loop through all sizes of combinations
  for (i in 1:length(all_covariates)) {
    combinations <- combn(all_covariates, i)
    for (j in 1:ncol(combinations)) {
      combination_name <- paste(combinations[,j], collapse="_")
      covariate_combinations[[combination_name]] <- combinations[,j]
    }
  }
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each combination
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}

run_all_analyses <- function(y_vars_2, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Define covariates for the "all" and "none" scenarios
  all_covariates <- c("ebitda","fixed_assets","log_age","operating_revenue_turnover_")
  covariate_combinations <- list("all" = all_covariates, "none" = NULL)
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each scenario
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}

run_all_analyses_no_multi <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Define covariates for the "all" and "none" scenarios
  all_covariates <- c("pre_fixed_assets","log_age")
  covariate_combinations <- list("all" = all_covariates, "none" = NULL)
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each scenario
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}


run_all_analyses_no_multi <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Define covariates for the "all" and "none" scenarios
  all_covariates <- c("fixed_assets","log_age")
  covariate_combinations <- list("all" = all_covariates, "none" = NULL)
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each scenario
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}
run_all_analyses_no_multi_2 <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Define covariates for the "all" and "none" scenarios
  all_covariates <- c("fixed_assets","log_age")
  covariate_combinations <- list("all" = all_covariates, "none" = NULL)
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each scenario
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis_2(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}



run_not_yet_analyses <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c("pre_ebitda","log_pre_fixed_assets","log_age","log_pre_turnover",  "fixed_assets")
  
  covariate_combinations <- list("none" = NULL)
  
  # Loop through all sizes of combinations
  for (i in 1:length(all_covariates)) {
    combinations <- combn(all_covariates, i)
    for (j in 1:ncol(combinations)) {
      combination_name <- paste(combinations[,j], collapse="_")
      covariate_combinations[[combination_name]] <- combinations[,j]
    }
  }
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each combination
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
 
  
  return(results)
}


run_not_yet_analyses_placebo <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c("pre_ebitda","log_pre_fixed_assets","log_age","log_pre_turnover",  "fixed_assets")
  
  covariate_combinations <- list("none" = NULL)
  
  # Loop through all sizes of combinations
  for (i in 1:length(all_covariates)) {
    combinations <- combn(all_covariates, i)
    for (j in 1:ncol(combinations)) {
      combination_name <- paste(combinations[,j], collapse="_")
      covariate_combinations[[combination_name]] <- combinations[,j]
    }
  }
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each combination
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis_placebo(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  
  
  return(results)
}


run_not_yet_analyses_no_multi <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c( 'notyettreated')
  
  # Define covariates for the "all" and "none" scenarios
  all_covariates <- c("fixed_assets","log_age","operating_revenue_turnover_")
  covariate_combinations <- list("all" = all_covariates, "none" = NULL)
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each scenario
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}
# Usage:


save_all_results <- function(all_results, save_path) {
  # Loop through the results and save them
  for (analysis_name in names(all_results)) {
    analysis_result <- all_results[[analysis_name]]
    
    # Check if analysis_result is a list or dataframe
    if (!is.list(analysis_result) && !is.data.frame(analysis_result)) {
      stop(paste("Expected a list or dataframe for analysis_name:", analysis_name, "but got:", class(analysis_result)))
    }
    
    for (y_var in names(analysis_result)) {
      result_for_var <- analysis_result[[y_var]]
      
      # Handle the numeric type specifically for sim_ratio
      if (y_var == "sim_ratio" && is.numeric(result_for_var)) {
        saveRDS(result_for_var, file = paste0(save_path, analysis_name, "_", y_var, "_results.rds"))
        next
      }
      
      # If not sim_ratio, check if result_for_var is a list or dataframe
      if (!is.list(result_for_var) && !is.data.frame(result_for_var)) {
        next # Just skip non-list, non-dataframe items
      }
      
      # Save the main results
      saveRDS(result_for_var, file = paste0(save_path, analysis_name, "_", y_var, "_results.rds"))
      
      # Save dynamic visualization data if it exists
      if (!is.null(result_for_var$dynamic_data)) {
        saveRDS(result_for_var$dynamic_data, file = paste0(save_path, analysis_name, "_", y_var, "_dynamic_data.rds"))
      }
    }
  }
}


print_simple_results <- function(results) {
  for (result_name in names(results)) {
    cat("\nResults for:", result_name, "\n")
    for (y_var in names(results[[result_name]])) {
      cat("For y_var:", y_var, "\n")
      if(is.list(results[[result_name]][[y_var]]$simple)) {
        simple_result <- results[[result_name]][[y_var]]$simple$overall.att
        if (!is.null(simple_result)) {
          cat(simple_result, "\n")
        } else {
          cat("NULL\n")
        }
      } else {
        cat("Error: Unexpected structure in results\n")
      }
    }
  }
}

save_all_results <- function(all_results, save_path) {
  # Calculate total number of items to process
  total_items <- sum(sapply(all_results, function(x) length(unlist(x))))
  
  # Create a progress bar
  pb <- txtProgressBar(min = 0, max = total_items, style = 3)
  
  # Initialize counter
  progress_counter <- 0
  
  # Loop through the results and save them
  for (analysis_name in names(all_results)) {
    analysis_result <- all_results[[analysis_name]]
    
    # Check if analysis_result is a list or dataframe
    if (!is.list(analysis_result) && !is.data.frame(analysis_result)) {
      stop(paste("Expected a list or dataframe for analysis_name:", analysis_name, "but got:", class(analysis_result)))
    }
    
    for (y_var in names(analysis_result)) {
      result_for_var <- analysis_result[[y_var]]
      
      # Handle the numeric type specifically for sim_ratio
      if (y_var == "sim_ratio" && is.numeric(result_for_var)) {
        saveRDS(result_for_var, file = paste0(save_path, analysis_name, "_", y_var, "_results.rds"))
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
        next
      }
      
      # If not sim_ratio, check if result_for_var is a list or dataframe
      if (!is.list(result_for_var) && !is.data.frame(result_for_var)) {
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
        next # Just skip non-list, non-dataframe items
      }
      
      # Save the main results
      saveRDS(result_for_var, file = paste0(save_path, analysis_name, "_", y_var, "_results.rds"))
      progress_counter <- progress_counter + 1
      setTxtProgressBar(pb, progress_counter)
      
      # Save dynamic visualization data if it exists
      if (!is.null(result_for_var$dynamic_data)) {
        saveRDS(result_for_var$dynamic_data, file = paste0(save_path, analysis_name, "_", y_var, "_dynamic_data.rds"))
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
      }
    }
  }
  # Close the progress bar
  close(pb)
}




# Assuming you've run this already:
# cs_all_results <- run_all_analyses(y_vars, matched_potential_suppliers_orbis_selected_age_20)

# Now, you can print the results using:
print_simple_results(cs_all_results)


#####4.1 All suppliers ----------------------------------------------------
all_suppliers<- full_panel %>% filter(supplier_status==1) %>% select(bvd_id_number) %>% distinct() %>% pull(bvd_id_number) 

full_panel_all_suppliers<- full_panel %>% filter(bvd_id_number%in% all_suppliers)
path_to_save <-here("results","output", "cs_all_results_")
cs_all_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_filtered)
save_all_results(cs_all_results, path_to_save)
beep()


sim_ratios_all <- lapply(cs_all_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})


data_frame_sim_ratio_all_2<- as.data.frame(sim_ratios_all_2)
sim_ratios_all_long_2 <- gather(data_frame_sim_ratio_all_2, key = "variable", value = "value")

data_frame_sim_ratio_all<- as.data.frame(sim_ratios_all)
sim_ratios_all_long <- gather(data_frame_sim_ratio_all, key = "variable", value = "value")

cs_results_statistically_significant<- sim_ratios_all_long %>% filter(abs(value) >=1.95)
cs_results_statistically_significant_2<- sim_ratios_all_long_2 %>% filter(abs(value) >=1.95)


#####4.2 By technology  suppliers ----------------------------------------------------
##### *4.21 High-tech ----------------------------------------------------

#Create the dataset for only high-tech orders
high_tech_data <- full_panel %>% filter(high_tech==1)

number_high_tech_suppliers <- high_tech_data %>% select(bvd_id_number, supplier_status)
all_high_tech_firms <- number_high_tech_suppliers %>% select(bvd_id_number) %>% distinct(bvd_id_number)
all_high_tech_suppliers<- number_high_tech_suppliers %>% filter(supplier_status==1) %>% distinct(bvd_id_number)

      all_high_tech_pot_suppliers <-   number_high_tech_suppliers %>% filter(supplier_status==0) %>% distinct(bvd_id_number)                                                                                               
high_tech_data_2 <- full_panel_filtered %>% filter(high_tech==1)
path_to_save <-here("results","output", "cs_ht_results_")
cs_ht_results<- run_all_analyses_multiple_combinations(y_vars, high_tech_data)
cs_ht_results_2 <- run_all_analyses_multiple_combinations(y_vars, high_tech_data_2)

models<- feols(log_patent_stock ~ postTreated +log_age| bvd_id_numeric + year, data=full_panel_filtered)

high_tech_data$event_time<- high_tech_data$year - high_tech_data$first_order
high_tech_data$event_treated <- high_tech_data$event_time * high_tech_data$postTreated
model <- feols(log_patent_stock ~ event_treated +fixed_assets +log_age| bvd_id_numeric + year, data = full_panel)

save_all_results(cs_ht_results, path_to_save)

final_data<- read_csv(here("data_proc", "bi_fina_ldata.csv"))
matched_potential_suppliers_orbis_selected_age_20$first_order_period
final_data$first_order_period_2 <- ifelse(final_data$first_order_period == 0, 0, (final_data$first_order_period - 1990) / 2 + 1)
final_data$year_period_2<- as.numeric(((final_data$year_period-1990)/2)+1)

final_data$first_order_period_2<- as.numeric(final_data$first_order_period_2)
final_data<- as.data.frame(final_data)

first_year <- min(matched_potential_suppliers_orbis_selected_age_20$first_order)
last_year <- max(matched_potential_suppliers_orbis_selected_age_20$first_order)
bins <- seq(first_year, last_year, by = 2)

matched_potential_suppliers_orbis_selected_age_20$first_order_grouped <- cut(matched_potential_suppliers_orbis_selected_age_20$first_order, bins, labels = FALSE)

cs_results_ht_test <- att_gt(
  yname = "log_patent_stock",
  tname = "year",
  idname = "bvd_id_numeric",
  gname = "first_order",
  xformla = ~ fixed_assets +log_age,
  data =full_panel,  
  est_method = "ipw",
  clustervars = "bvd_id_numeric", 
  control_group = "notyettreated",
  allow_unbalanced_panel = T,
 # biters = 10000,
  base_period = "universal",
  bstrap = F
)

cs_results_high_tech_test.sim <- aggte( cs_results_ht_test, type ="simple", na.rm = TRUE)
cs_results_high_tech_test.dyn <- aggte( cs_results_ht_test, type ="dynamic", max_e = 12, min_e = -12,  cband=F, na.rm=T, bstrap =F)
ggdid(cs_results_high_tech_test.dyn, type = "dynamic")

full

full_panel_grouped_final_selected <- full_panel_grouped_final %>% 
  select(bvd_id_numeric, year_mapping, first_order_period_1, fixed_assets_avg, log_patent_stock_avg) %>% distinct()
full_panel_grouped_final_selected<- as.data.frame(full_panel_grouped_final_selected)
full_panel_grouped_final<- full_panel_grouped_final %>% 
  select(-first_order_period_1)
  select(-year_grouped_1, -first_order_grouped_1, -year,-year_period_1)

years <- c(1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)

# Create a sequence starting from 1
sequence <- seq_along(years)

# Create a data frame
year_df <- data.frame(year_period = years, year_period_1 = sequence)
order_df<- data.frame(first_order_period = years, first_order_period_1 = sequence)

full_panel_grouped_final<- full_panel_grouped_final %>% 
  left_join(year_df)
full_panel_grouped_final<- full_panel_grouped_final %>% 
  left_join(order_df)

full_panel_grouped_final<-full_panel_grouped_final %>% 
  mutate(first_order_period_1 = replace(first_order_period_1, is.na(first_order_period_1), 0))


# Replace values in first_order_period_1
full_panel_grouped_final <- full_panel_grouped_final%>%
  mutate(first_order_period_2 = ifelse(first_order_period_1 == 0, 3000, first_order_period_1))


# Define the years
years <- c(1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020)

# Create a sequence starting from 1
sequence <- seq_along(years)

# Create a data frame
year_df <- data.frame(year_period = years, year_period_1 = sequence)
full_panel_grouped_final$first_order_period_1<- as.numeric(full_panel_grouped_final$first_order_period_1)
full_panel_grouped_final$log_pat_applications_avg
post_pre_firms <- unique(test$bvd_id_numeric)

full_panel_grouped_final$first_order_period_2_corrected <- ifelse(full_panel_grouped_final$first_order_period_2 == 3000, 0, full_panel_grouped_final$first_order_period_2)

cs_results_test <- att_gt(
  yname = "log_patent_stock_avg",
  tname = "year_period_1",
  idname = "bvd_id_numeric",
  gname = "first_order_period_1",
  #xformla = ~ fixed_assets_avg,
  data = full_panel_grouped_final, #%>% filter(bvd_id_numeric %in% post_pre_firms),  
  #est_method = "ipw",
  clustervars = "bvd_id_numeric", 
  control_group = "notyettreated",
  allow_unbalanced_panel = T,
  biters = 3000,
  panel= T,
  bstra
)


# Check summary of 'first_order_period_1' and 'year_period_1'
summary(full_panel_grouped_final$first_order_period_1)
summary(full_panel_grouped_final$year_period_1)

# Visualize the distribution of treatment over time
library(ggplot2)
ggplot(full_panel_grouped_final, aes(x = year_period_1, fill = as.factor(first_order_period_1))) +
  geom_histogram(binwidth = 1, position = "dodge")

# Check for pre-treatment observations
pre_treatment_check <- full_panel_grouped_final %>% 
  filter(year_period_1 < min(full_panel_grouped_final$first_order_period_1[full_panel_grouped_final$first_order_period_1 > 0]))

# Display a summary of pre-treatment check
summary(pre_treatment_check)


pre_treated_units <- full_panel_grouped_final%>% 
  filter(year_period_1 <= first_order_period_2) %>%
  pull(bvd_id_numeric) %>% 
  unique()

# Counting pre-treated units
pre_treated_count <- length(pre_treated_units)
print(paste("Number of pre-treated units:", pre_treated_count))

full_panel_grouped_final<- full_panel_grouped_final %>% 
  filter(!bvd_id_numeric %in% pre_treated_units)
  

# Counting pre-treated units
pre_treated_count <- length(pre_treated_units)

summary <- full_panel_grouped_final %>% dplyr::group_by(bvd_id_numeric) %>% 
  dplyr::summarise(number_years = n())

test<- full_panel_grouped_final %>% 
  select(bvd_id_numeric, year_period, log_patent_stock_avg, first_order_period
         )

never_treated <- full_panel_grouped_final %>% 
  filter(!first_order_period_2 %in% unique(full_panel_grouped_final$year_period_1))

# Checking for pre-treatment observations
pre_treatment_data <-full_panel_grouped_final%>% 
  filter(year_period_1 < min(full_panel_grouped_final$first_order_period_2[full_panel_grouped_final$first_order_period_2 > 0]))

test<- full_panel_grouped_final %>%
  dplyr::group_by(bvd_id_numeric) %>%
  dplyr::summarize(pre_treatment = any(year_period_1 < first_order_period_2),
            post_treatment = any(year_period_1 >= first_order_period_2)) %>%
  filter(pre_treatment & post_treatment)

# Display some of the pre-treatment data
head(pre_treatment_data)


aggte(cs_results_test, type = "simple")

str(full_panel_grouped_final$first_order_grouped_1)

here()

aggte(cs_results_high_tech_test.dyn, type ="dynamic", na.rm = TRUE)
min(final_data$year_period)
max(final_data$year_period)
unique(final_data$first_order_period)

any(final_data$year_period < final_data$first_order_period)
summary(final_data$first_order_period)
summary(final_data$year_period)
length(unique(final_data$first_order_period))
length(unique(final_data$year_period))


sim_ratios_ht <- lapply(cs_ht_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_ht<- as.data.frame(sim_ratios_ht)
sim_ratios_ht_long <- gather(data_frame_sim_ratio_ht, key = "variable", value = "value")

sim_ratios_ht_2 <- lapply(cs_ht_results_2, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_ht_2<- as.data.frame(sim_ratios_ht)
sim_ratios_ht_long <- gather(data_frame_sim_ratio_ht, key = "variable", value = "value")


###### *4.22 Low-tech ----------------------------------------------------

#Create the dataset for only low-tech orders

low_tech_data <- full_panel %>% filter(high_tech==0)
low_tech_data_2<- full_panel_filtered %>% filter(high_tech==0)
path_to_save <-here("results","output", "cs_lt_results_")
cs_lt_results <- run_all_analyses_multiple_combinations(y_vars, low_tech_data)
cs_lt_results_2 <- run_all_analyses_multiple_combinations(y_vars, low_tech_data_2)

number_low_tech_suppliers <- low_tech_data %>% select(bvd_id_number, supplier_status)
all_low_tech_firms <- number_low_tech_suppliers %>% select(bvd_id_number) %>% distinct(bvd_id_number)
all_low_tech_suppliers<- number_low_tech_suppliers %>% filter(supplier_status==1) %>% distinct(bvd_id_number)

save_all_results(cs_lt_results, path_to_save)
sim_ratios_lt <- lapply(cs_lt_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_lt<- as.data.frame(sim_ratios_lt)
sim_ratios_lt_long <- gather(data_frame_sim_ratio_lt, key = "variable", value = "value")

sim_ratios_lt_2 <- lapply(cs_lt_results_2, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_lt_2<- as.data.frame(sim_ratios_lt_2)
sim_ratios_lt_long_2 <- gather(data_frame_sim_ratio_lt_2, key = "variable", value = "value")


cs_results_lt_statistically_significant<- sim_ratios_lt_long %>% filter(abs(value) >=1.95)
cs_results_lt_statistically_significant_2<- sim_ratios_lt_long_2 %>% filter(abs(value) >=1.95)
#####4.4 Number of orders ----------------------------------------------------
######4.41 Single orders----------------------------------------------------

#Create the dataset for only one orders

one_order_data <- full_panel %>% filter(total_orders<2)
number_one_order_suppliers <- one_order_data %>% select(bvd_id_number, supplier_status)
all_one_order_firms <- number_one_order_suppliers %>% select(bvd_id_number) %>% distinct(bvd_id_number)
all_one_suppliers<- number_one_order_suppliers %>% filter(supplier_status==1) %>% distinct(bvd_id_number)

path_to_save <-here("results","output", "cs_one_results_")
cs_one_results <- run_all_analyses_multiple_combinations(y_vars, one_order_data)
save_all_results(cs_one_results, path_to_save)
sim_ratios_one <- lapply(cs_one_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_one<- as.data.frame(sim_ratios_one)
sim_ratios_one_long <- gather(data_frame_sim_ratio_one, key = "variable", value = "value")

######4.42 Multiple orders----------------------------------------------------
multi_order_data<- full_panel %>% filter(total_orders>1)


number_multiple_order_suppliers <- multi_order_data %>% select(bvd_id_number, supplier_status)
all_multi_order_firms <- number_multiple_order_suppliers %>% select(bvd_id_number) %>% distinct(bvd_id_number)
all_multi_suppliers<- number_multiple_order_suppliers %>% filter(supplier_status==1) %>% distinct(bvd_id_number)

path_to_save <-here("results","output", "cs_multiple_results_")
cs_multiple_results <- run_not_yet_analyses(y_vars, multi_order_data)
save_all_results(cs_multiple_results, path_to_save)
sim_ratios_multiple <- lapply(cs_multiple_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_multiple<- as.data.frame(sim_ratios_multiple)
sim_ratios_multiple_long <- gather(data_frame_sim_ratio_multiple, key = "variable", value = "value")
#####4.5 Post/Pre008 ----------------------------------------------------

###### 4.51 Post2008 ----------------------------------------------------

post_2008_data <- full_panel%>% filter(first_order>2008)
path_to_save <-here("results","output", "cs_post_2008_results_")
cs_post_2008_results <- run_not_yet_analyses(y_vars, post_2008_data)
save_all_results(cs_post_2008_results, path_to_save)

sim_ratios_post_2008 <- lapply(cs_post_2008_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_post_2008<- as.data.frame(sim_ratios_post_2008)
sim_ratios_post_2008_long <- gather(data_frame_sim_ratio_post_2008, key = "variable", value = "value")


###### 4.52 Pre2008 ----------------------------------------------------

pre_2008_data <- full_panel %>% filter(first_order<2009 )
path_to_save <-here("results","output", "cs_pre_2008_results_")
cs_pre_2008_results <- run_not_yet_analyses(y_vars, pre_2008_data)
save_all_results(cs_pre_2008_results, path_to_save)
sim_ratios_pre_2008 <- lapply(cs_pre_2008_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_pre_2008<- as.data.frame(sim_ratios_pre_2008)
sim_ratios_pre_2008_long <- gather(data_frame_sim_ratio_pre_2008, key = "variable", value = "value")

beep()

#####4.6 Firms Size ----------------------------------------------------
###### 4.61 SME ----------------------------------------------------
SME_data <- full_panel %>% filter(SME_status==1)

path_to_save <-here("results","output", "cs_SME_results_")
cs_SME_results <- run_all_analyses_multiple_combinations(y_vars, SME_data)
save_all_results(cs_SME_results, path_to_save)
beep()
sim_ratios_SME <- lapply(cs_SME_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_SME<- as.data.frame(sim_ratios_SME)
sim_ratios_SME_long <- gather(data_frame_sim_ratio_SME, key = "variable", value = "value")




###### 4.62 Large ----------------------------------------------------

large_data <- full_panel %>% filter(SME_status==0)
path_to_save <-here("results","output", "cs_large_results_")
cs_large_results <- run_all_analyses_multiple_combinations(y_vars, large_data)
beep()
save_all_results(cs_large_results, path_to_save)

sim_ratios_large <- lapply(cs_large_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
beep()
data_frame_sim_ratio_large<- as.data.frame(sim_ratios_large)
sim_ratios_large_long <- gather(data_frame_sim_ratio_large, key = "variable", value = "value")

#####4.7 Amount ----------------------------------------------------

###### 4.71 100k ----------------------------------------------------

data_100k <- full_panel %>% filter(first_order_amount<100001)
path_to_save <-here("results","output", "cs_100k_results_")
cs_100k_results <- run_all_analyses_multiple_combinations(y_vars, data_100k)
save_all_results(cs_100k_results, path_to_save)
sim_ratios_100k <- lapply(cs_100k_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_100k<- as.data.frame(sim_ratios_100k)
sim_ratios_100k_long <- gather(data_frame_sim_ratio_100k, key = "variable", value = "value")

###### 4.72 200k ----------------------------------------------------

data_200k <- full_panel %>% filter(first_order_amount>100000)
path_to_save <-here("results","output", "cs_200k_results_")
cs_200k_results <- run_not_yet_analyses(y_vars, data_200k)
beep()
save_all_results(cs_200k_results, path_to_save)
beep()
sim_ratios_200k <- lapply(cs_200k_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_200k<- as.data.frame(sim_ratios_200k)
sim_ratios_200k_long <- gather(data_frame_sim_ratio_200k, key = "variable", value = "value")

###### 4.73 >100k ----------------------------------------------------

data_more_100k <- full_panel %>% filter(first_order_amount>100000)
path_to_save <-here("results","output", "cs_more_100k_results_")
cs_more_100k_results <- run_not_yet_analyses(y_vars, data_more_100k)
beep()

sim_ratios_more_100k <- lapply(cs_more_100k_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_more_100k<- as.data.frame(sim_ratios_more_100k)
sim_ratios_more_100k_long <- gather(data_frame_sim_ratio_more_100k, key = "variable", value = "value")


save_all_results(cs_more_100k_results, path_to_save)
beep()

#sim_ratios_more_200k <- lapply(cs_more_200k_results, function(x) {
 # if ("sim_ratio" %in% names(x)) {
  #  return(x$sim_ratio)
  #} else {
   # return(NULL)
 # }
#})





##### 4.8 Countries ----------------------------------------------------
######## *4.81 Italy ----------------------------------------------------


IT_data <- full_panel %>% filter(country=="IT")
IT_ht_data<- matched_potential_suppliers_orbis_selected_age_20 %>% filter(country=="IT" & high_tech==1)
path_to_save <-here("results","output", "cs_IT_results_")
cs_IT_results <- run_not_yet_analyses(y_vars, IT_data)

save_all_results(cs_IT_results, path_to_save)
sim_ratios_it <- lapply(cs_IT_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_it<- as.data.frame(sim_ratios_it)
sim_ratios_it_long <- gather(data_frame_sim_ratio_it, key = "variable", value = "value")


####### *4.82 France ----------------------------------------------------
FR_data <- full_panel %>% filter(country=="FR")

path_to_save <-here("results","output", "cs_FR_results_")
cs_FR_results <- run_not_yet_analyses(y_vars, FR_data)

save_all_results(cs_FR_results, path_to_save)
sim_ratios_fr <- lapply(cs_FR_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_fr<- as.data.frame(sim_ratios_fr)
sim_ratios_fr_long <- gather(data_frame_sim_ratio_fr, key = "variable", value = "value")


####### *4.83 Spain ----------------------------------------------------
ES_data <- full_panel %>% filter(country=="ES")
path_to_save <-here("results","output", "cs_ES_results_")
cs_ES_results <- run_not_yet_analyses(y_vars, ES_data)
save_all_results(cs_ES_results, path_to_save)
sim_ratios_es <- lapply(cs_ES_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_es<- as.data.frame(sim_ratios_es)
sim_ratios_es_long <- gather(data_frame_sim_ratio_es, key = "variable", value = "value")


####### *4.84 UK ----------------------------------------------------
UK_data <-full_panel %>% filter(country=="GB")
path_to_save <-here("results","output", "cs_UK_results_")
cs_UK_results <- run_not_yet_analyses(y_vars, UK_data)
save_all_results(cs_UK_results, path_to_save)

sim_ratios_uk <- lapply(cs_UK_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_uk<- as.data.frame(sim_ratios_uk)
sim_ratios_uk_long <- gather(data_frame_sim_ratio_uk, key = "variable", value = "value")


##### 4.9 Firm Age ----------------------------------------------------

######## *4.91 Start-Up  ----------------------------------------------------
start_up_data<- full_panel %>% filter(age<6)
path_to_save <-here("results","output", "cs_start_up_results_")
cs_start_up_results <- run_not_yet_analyses(y_vars, start_up_data)
save_all_results(cs_start_up_results, path_to_save)
sim_ratios_start_up <- lapply(cs_start_up_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_start_up<- as.data.frame(sim_ratios_start_up)
sim_ratios_start_up_long <- gather(data_frame_sim_ratio_start_up, key = "variable", value = "value")


######## *4.92 Incumbent  ---------------------------------------------------

incumbent_data<- full_panel%>% filter(age>5)
path_to_save <-here("results","output", "cs_incumbent_results_")
cs_incumbent_results <- run_not_yet_analyses(y_vars, incumbent_data)
save_all_results(cs_incumbent_results, path_to_save)

sim_ratios_incumbent <- lapply(cs_incumbent_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_incumbent<- as.data.frame(sim_ratios_incumbent)
sim_ratios_incumbent_long <- gather(data_frame_sim_ratio_incumbent, key = "variable", value = "value")

beep()
######## *4.91 Incumbent  ----------------------------------------------------





all_lists <- list(sim_ratios_all, sim_ratios_ht, sim_ratios_lt, sim_ratios_one, sim_ratios_SME, 
                  sim_ratios_large, sim_ratios_100k )

list_48<- list(sim_ratios_post_2008, sim_ratios_pre_2008, sim_ratios_es, sim_ratios_fr, sim_ratios_it, sim_ratios_uk,
               sim_ratios_incumbent, sim_ratios_start_up,sim_ratios_200k)
list_of_dfs <- lapply(all_lists, function(x) as.data.frame(t(unlist(x))))
list_of_dfs_48<- lapply(list_48,function(x) as.data.frame(t(unlist(x))))

names(list_of_dfs) <- c("sim_ratios_all", "sim_ratios_ht", "sim_ratios_lt", "sim_ratios_one", "sim_ratios_SME", "sim_ratios_large",
                        "sim_ratios_100k")
names(list_of_dfs_48)<- c("sim_ratios_post_2008", "sim_ratios_pre_2008", "sim_ratios_es", "sim_ratios_fr", "sim_ratios_it", "sim_ratios_uk",
                          "sim_ratios_incumbent", "sim_ratios_start_up","sim_ratios_200k")
# Add an identifier column to each dataframe
list_of_dfs <- lapply(1:length(list_of_dfs), function(i) {
  df <- list_of_dfs[[i]]
  df$source <- names(list_of_dfs)[i]
  return(df)
})


list_of_dfs_48 <- lapply(1:length(list_of_dfs_48), function(i) {
  df <- list_of_dfs_48[[i]]
  df$source <- names(list_of_dfs_48)[i]
  return(df)
})



combined_df <- do.call(rbind, list_of_dfs)
combined_df_48 <- do.call(rbind, list_of_dfs_48)


# Combine all dataframes into one
combined_df<- combined_df %>% select(source, everything())
combined_df_48<- combined_df_48 %>% select(source, everything())


df_long <- combined_df %>%
  gather(key = "variable", value = "value", -source)

df_long_48 <- combined_df_48 %>%
  gather(key = "variable", value = "value", -source)

# Spread the source column into separate columns with values from the value column
df_wide <- df_long %>%
  spread(key = source, value = value)

df_wide_48 <- df_long_48 %>%
  spread(key = source, value = value)

write.csv(df_wide, here("results","sim_ratios_1.csv"))
write.csv(df_wide_48, here("results","sim_ratios_2_2017.csv"))




all_lists_2018 <- list(sim_ratios_all, sim_ratios_ht, sim_ratios_lt, sim_ratios_one, sim_ratios_SME, 
                  sim_ratios_large, sim_ratios_100k, sim_ratios_200k)

list_48_2018<- list(sim_ratios_post_2008, sim_ratios_pre_2008, sim_ratios_es, sim_ratios_fr, sim_ratios_it, sim_ratios_uk,
               sim_ratios_incumbent, sim_ratios_start_up)
list_of_dfs_2018 <- lapply(all_lists_2018, function(x) as.data.frame(t(unlist(x))))
list_of_dfs_48_2018<- lapply(list_48_2018,function(x) as.data.frame(t(unlist(x))))

names(list_of_dfs_2018) <- c("sim_ratios_all", "sim_ratios_ht", "sim_ratios_lt", "sim_ratios_one", "sim_ratios_SME", "sim_ratios_large",
                        "sim_ratios_100k", "sim_ratios_200k")
names(list_of_dfs_48_2018)<- c("sim_ratios_post_2008", "sim_ratios_pre_2008", "sim_ratios_es", "sim_ratios_fr", "sim_ratios_it", "sim_ratios_uk",
                          "sim_ratios_incumbent", "sim_ratios_start_up")
# Add an identifier column to each dataframe
list_of_dfs_2018 <- lapply(1:length(list_of_dfs_2018), function(i) {
  df <- list_of_dfs_2018[[i]]
  df$source <- names(list_of_dfs_2018)[i]
  return(df)
})


list_of_dfs_48_2018 <- lapply(1:length(list_of_dfs_48_2018), function(i) {
  df <- list_of_dfs_48_2018[[i]]
  df$source <- names(list_of_dfs_48_2018)[i]
  return(df)
})



combined_df_2018 <- do.call(rbind, list_of_dfs_2018)
combined_df_48_2018 <- do.call(rbind, list_of_dfs_48_2018)


# Combine all dataframes into one
combined_df_2018<- combined_df_2018 %>% select(source, everything())
combined_df_48_2018<- combined_df_48_2018 %>% select(source, everything())


df_long_2018 <- combined_df_2018 %>%
  gather(key = "variable", value = "value", -source)

df_long_48_2018 <- combined_df_48_2018 %>%
  gather(key = "variable", value = "value", -source)

# Spread the source column into separate columns with values from the value column
df_wide_2018 <- df_long_2018 %>%
  spread(key = source, value = value)

df_wide_48_2018 <- df_long_48_2018 %>%
  spread(key = source, value = value)

write.csv(df_wide_2018, here("results","sim_ratios_1_2018.csv"))
write.csv(df_wide_48_2018, here("results","sim_ratios_2_2018.csv"))



all_lists_2015 <- list(sim_ratios_all, sim_ratios_ht, sim_ratios_lt, sim_ratios_one, sim_ratios_SME, 
                       sim_ratios_large, sim_ratios_100k, sim_ratios_200k)

list_48_2015<- list(sim_ratios_post_2008, sim_ratios_pre_2008, sim_ratios_es, sim_ratios_fr, sim_ratios_it, sim_ratios_uk,
                    sim_ratios_incumbent, sim_ratios_start_up)
list_of_dfs_2015 <- lapply(all_lists_2015, function(x) as.data.frame(t(unlist(x))))
list_of_dfs_48_2015<- lapply(list_48_2015,function(x) as.data.frame(t(unlist(x))))

names(list_of_dfs_2015) <- c("sim_ratios_all", "sim_ratios_ht", "sim_ratios_lt", "sim_ratios_one", "sim_ratios_SME", "sim_ratios_large",
                             "sim_ratios_100k", "sim_ratios_200k")
names(list_of_dfs_48_2015)<- c("sim_ratios_post_2008", "sim_ratios_pre_2008", "sim_ratios_es", "sim_ratios_fr", "sim_ratios_it", "sim_ratios_uk",
                               "sim_ratios_incumbent", "sim_ratios_start_up")
# Add an identifier column to each dataframe
list_of_dfs_2015 <- lapply(1:length(list_of_dfs_2015), function(i) {
  df <- list_of_dfs_2015[[i]]
  df$source <- names(list_of_dfs_2015)[i]
  return(df)
})


list_of_dfs_48_2015 <- lapply(1:length(list_of_dfs_48_2015), function(i) {
  df <- list_of_dfs_48_2015[[i]]
  df$source <- names(list_of_dfs_48_2015)[i]
  return(df)
})



combined_df_2015 <- do.call(rbind, list_of_dfs_2015)
combined_df_48_2015 <- do.call(rbind, list_of_dfs_48_2015)


# Combine all dataframes into one
combined_df_2015<- combined_df_2015 %>% select(source, everything())
combined_df_48_2015<- combined_df_48_2015 %>% select(source, everything())


df_long_2015 <- combined_df_2015 %>%
  gather(key = "variable", value = "value", -source)

df_long_48_2015 <- combined_df_48_2015 %>%
  gather(key = "variable", value = "value", -source)

# Spread the source column into separate columns with values from the value column
df_wide_2015 <- df_long_2015 %>%
  spread(key = source, value = value)

df_wide_48_2015 <- df_long_48_2015 %>%
  spread(key = source, value = value)

write.csv(df_wide_2015, here("results","sim_ratios_1_2015.csv"))
write.csv(df_wide_48_2015, here("results","sim_ratios_2_2015.csv"))


# Display the reshaped data
head(long_df)



df<- as.data.frame(all_lists)

df$dataset <- c("all", "ht", "lt", "one", "SME", "Large")


all_dfs <- lapply(all_lists, parse_list)
final_df <- do.call(rbind, all_dfs)
##5.1 Placebo effect ------------------------------------------------------------------

  #####Create the fake treatment year ---------------------------------------
full_panel_placebo<- full_panel %>%
  dplyr::filter(supplier_status == 0) %>%
  mutate(
    placebo_treat = case_when(
      high_tech == 1 ~ registration_year,
      high_tech == 0 ~ 0
    ),
    first_order = placebo_treat
  )

cs_placebo_results <- run_not_yet_analyses_placebo(y_vars, full_panel_placebo)

sim_ratios_placebo <- lapply(cs_placebo_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

path_to_save <-here("results","output", "cs_placebo_results_")
save_all_results(cs_placebo_results, path_to_save)




  ######5.11 Placebo no covariates Never treated----------------------------------------------------
  ## Never treated
  cs_results_placebo <- att_gt(
    yname = "log_patent_stock",
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "placebo_treat",
    #xformla = ~ operating_revenue_turnover_,
    data = matched_potential_suppliers_orbis_selected_age_20_placebo_fake %>% filter(year>1999 & year <2018 & placebo_treat !=2007),
    control_group =  'nevertreated',
    est_method = "ipw",
    clustervars = "bvd_id_numeric", 
    base_period = "universal",
    panel = T,
    biters =3000,
    allow_unbalanced_panel = T
  )
  
  
  cs_results_placebo.dyn <- aggte( cs_results_placebo, type ="dynamic",na.rm = TRUE, max_e = 10, min_e = -10)
  cs_results_placebo.sim <- aggte( cs_results_placebo, type ="simple", na.rm = TRUE)
  cs_results_placebo.grp <- aggte( cs_results_placebo, type ="group", na.rm = TRUE)
  cs_results_placebo.cal <- aggte( cs_results_placebo, type ="calendar")
  cs_results_placebo.sim$overall.att/ cs_results_placebo.sim$overall.se
  cs_results_placebo.plot<-ggdid(cs_results_placebo.dyn)
  ggdid(cs_results_placebo.grp)
  
  
  # The effects are negative but statistically non-significant
  
  
  
  cs_results_placebo_data<- cs_results_placebo.plot$data
  cs_results_placebo_data$lower <- cs_results_placebo_data$att - cs_results_placebo_data$c * cs_results_placebo_data$att.se
  cs_results_placebo_data$upper <- cs_results_placebo_data$att + cs_results_placebo_data$c * cs_results_placebo_data$att.se
  
  
  # #5.12 All suppliers no covariates not yet treated ------------------------
  cs_results_placebo_not_yet <- att_gt(
    yname = "log_patent_stock",
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "placebo_treat",
    #xformla = ~ operating_revenue_turnover_,
    data = matched_potential_suppliers_orbis_selected_age_20_placebo_fake %>% filter(year>1999 & year <2018  & placebo_treat !=2007),
    control_group =  'notyettreated',
    est_method = "ipw",
    clustervars = "bvd_id_numeric", 
    base_period = "universal",
    panel = T,
    biters =3000,
    allow_unbalanced_panel = T
  )
  
  
  cs_results_placebo_not_yet.dyn <- aggte(cs_results_placebo_not_yet, type ="dynamic",na.rm = TRUE, max_e = 10, min_e = -10)
  cs_results_placebo_not_yet.sim <- aggte(cs_results_placebo_not_yet, type ="simple", na.rm = TRUE)
  cs_results_placebo_not_yet.grp <- aggte(cs_results_placebo_not_yet, type ="group", na.rm = TRUE)
  cs_results_placebo_not_yet.cal <- aggte(cs_results_placebo_not_yet, type ="calendar")
  cs_results_placebo_not_yet.sim$overall.att/cs_results_placebo_not_yet.sim$overall.se
  cs_results_placebo_not_yet.plot<-ggdid(cs_results_placebo_not_yet.dyn)
  ggdid(cs_results_placebo_not_yet.grp)
  
  
  # The effects are negative but statistically non-significant
  
  
  
  cs_results_placebo_not_yet_data<- cs_results_placebo_not_yet.plot$data
  cs_results_placebo_not_yet_data$lower <- cs_results_placebo_not_yet_data$att - cs_results_placebo_not_yet_data$c * cs_results_placebo_not_yet_data$att.se
  cs_results_placebo_not_yet_data$upper <- cs_results_placebo_not_yet_data$att + cs_results_placebo_not_yet_data$c * cs_results_placebo_not_yet_data$att.se
  
  
  
  ###### 5.13 All suppliers with covariates never treated----------------------------------------------------
  
  cov_cs_placebo_results<- att_gt(
    yname = "log_patent_stock",
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "placebo_treat",
    xformla = ~ operating_revenue_turnover_ + ebitda,
    data = matched_potential_suppliers_orbis_selected_age_20_placebo_fake %>% filter(year>1999 & year<2018 & placebo_treat !=2007),
    control_group =  'nevertreated',
    est_method = "ipw",
    clustervars = "bvd_id_numeric",
    base_period = "universal",
    bstrap =T,
    panel = T,
    biters =3000,
    allow_unbalanced_panel = T
  )
  cov_cs_placebo_results.dyn <- aggte(cov_cs_placebo_results, type ="dynamic", na.rm = TRUE, min_e =-10, max_e = 10)
  cov_cs_placebo_results.sim <- aggte(cov_cs_placebo_results, type ="simple", na.rm = TRUE)
  cov_cs_placebo_results.grp <- aggte(cov_cs_placebo_results, type ="group", na.rm = TRUE)
  cov_cs_placebo_results.cal <- aggte(cov_cs_placebo_results, type ="calendar", na.rm = TRUE)
  cov_cs_placebo_results.sim$overall.att/cov_cs_placebo_results.sim$overall.se
  cov_cs_placebo_results.plot<- ggdid(cov_cs_placebo_results.dyn, xgap =2)
  ggdid(cov_cs_placebo_results.grp, xgap = 5)
  
  
  cov_cs_placebo_results_data<- cov_cs_placebo_results.plot$data
  
  # Compute confidence intervals
  cov_cs_placebo_results_data$lower <- cov_cs_placebo_results_data$att - cov_cs_placebo_results_data$c * cov_cs_placebo_results_data$att.se
  cov_cs_placebo_results_data$upper <- cov_cs_placebo_results_data$att + cov_cs_placebo_results_data$c * cov_cs_placebo_results_data$att.se
  
  
  #### #5.14 All suppliers with covariates not yet treated --------------------------------------------------------------------
  
  
  ## Not yet treated 
  
  cov_cs_placebo_results_not_yet<- att_gt(
    yname = "log_patent_stock",
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "placebo_treat",
    xformla = ~ operating_revenue_turnover_ + ebitda,
    data = matched_potential_suppliers_orbis_selected_age_20_placebo_fake %>% filter(year>1999 & year<2018 & placebo_treat !=2007),
    control_group =  'notyettreated',
    est_method = "ipw",
    clustervars = "bvd_id_numeric",
    base_period = "universal",
    bstrap =T,
    panel = T,
    biters =3000,
    allow_unbalanced_panel = T
  )
  cov_cs_placebo_results_not_yet.dyn <- aggte(cov_cs_placebo_results_not_yet, type ="dynamic", na.rm = TRUE, min_e =-10, max_e = 10)
  cov_cs_placebo_results_not_yet.sim <- aggte(cov_cs_placebo_results_not_yet, type ="simple", na.rm = TRUE)
  cov_cs_placebo_results_not_yet.grp <- aggte(cov_cs_placebo_results_not_yet, type ="group", na.rm = TRUE)
  cov_cs_placebo_results_not_yet.cal <- aggte(cov_cs_placebo_results_not_yet, type ="calendar", na.rm = TRUE)
  cov_cs_placebo_results_not_yet.sim$overall.att/cov_cs_placebo_results_not_yet.sim$overall.se
  cov_cs_placebo_results_not_yet.plot<- ggdid(cov_cs_placebo_results_not_yet.dyn, xgap =2)
  ggdid(cov_cs_placebo_results_not_yet.grp, xgap = 5)
  
  
  cov_cs_placebo_results_not_yet_data<- cov_cs_placebo_results_not_yet.plot$data
  
  # Compute confidence intervals
  cov_cs_placebo_results_not_yet_data$lower <- cov_cs_placebo_results_not_yet_data$att - cov_cs_placebo_results_not_yet_data$c * cov_cs_placebo_results_not_yet_data$att.se
  cov_cs_placebo_results_not_yet_data$upper <- cov_cs_placebo_results_not_yet_data$att + cov_cs_placebo_results_not_yet_data$c * cov_cs_placebo_results_not_yet_data$att.se
  
  
  
  
  # The effects are positive and statistically non-significant.
  
  
  
  ###### 5.15 Prepare and save data for Graphs and Tables---------------------------------------------------------------
  cov_cs_placebo_results_data$source <- "Callaway & Sant'Anna with covariates"
  cs_results_placebo_data$source <- "Callaway & Sant'Anna"
  all_cs_placebo_data<- rbind(cov_cs_placebo_results_data, cs_results_placebo_data)
  
  cov_cs_placebo_results_not_yet_data<- "Callaway & Sant'Anna with covariates"
  cs_results_placebo_not_yet_data<- "Callaway & Sant'Anna"
  all_cs_placebo_data_not_yet<- rbind(cov_cs_placebo_results_not_yet, cs_results_placebo_not_yet_data)
  ### save the data 
  saveRDS(all_cs_placebo_data, here("results","output", "all_cs_placebo_data_graph.rds"))
  saveRDS(all_cs_placebo_data_not_yet, here("results","output", "all_cs_placebo_not_yet_data_graph.rds"))
  
  
  # Usage
  save_results(cs_results_placebo.sim, "cs_results_placebo.sim")
  save_results(cs_results_placebo_not_yet.sim, "cs_results_placebo_not_yet.sim")
  save_results(cov_cs_placebo_results.sim, "cov_cs_results_placebo.sim")
  save_results(cov_cs_placebo_results_not_yet.sim, "cov_cs_not_yet_results_placebo.sim")
  
  


  
  
  


# Two-way FEs -------------------------------------------------------------
#### In this section here, I want to show that by using two-way fixed effects, the estimates might be driven by the wrong comparison. So what I do here is the following:
  # First, I create the treat variable (this should not be so different from the suppliers status that I have). 
  # Second, I create a balanced panel (after removing the outliers). I am doing this because the bacon decomposition package only works with a balanced panel
  # Third, I use the TWFE and  bacon decomposition: this shows how much weight is given to the variables
  ### When doing the TWFE with covariates, I basically follow the approach of Burgess et al. (2015)  here:https://www.aeaweb.org/articles?id=10.1257/aer.20131031 
  #where the year is interacted with the time-invariant covariate (If I don't do this, you have the issue of collinearity)

# Preparing the data
# Create a treatment variable
full_panel_first_date<- full_panel_first_date %>% # 
    mutate(treat = case_when(
      first_order >0 ~1,
      first_order ==0 ~0
    ))
  # Creating the relative time 
  full_panel_first_date<- full_panel_first_date %>% 
    mutate(time_to_treat = case_when(
      treat ==1 ~ year - first_order, 
      treat ==0 ~0
      
    ))
  
  ### Create the filtered dataset  (removin the outliers)
  
  full_panel_filtered <- full_panel_first_date %>%
    filter(bvd_id_number %notin% outlier_ids)
  
  # this is to use the bacon decomposition 
  balanced_data <- make.pbalanced(full_panel_filtered, balance.type = "shared.times")

# full_panel_first_date<- full_panel_first_date %>% 
#   mutate(log_pre_fixed_assets = log(pre_fixed_assets+1))
#   
# full_panel_first_date <- full_panel_first_date %>% # not sure whether this is an issue
#     mutate(
#       first_order_dummy = case_when(
#         supplier_status == 1 & first_order == year ~ 1,     # When supplier_status is 1 and first_order equals year
#         supplier_status == 0 & registration_year == year ~ 1, # When supplier_status is 0 and registration_year equals year
#         TRUE ~ 0                                            # Otherwise
#       )
#     )



  

  
# Create a static TWFE 

  #### All data ------------------------------------------------------------

model_twfe_balanced <- feols(log_sine_application ~ postTreated| ## Our key interaction: time × treatment status
                       ## Other controls
                       year+bvd_id_number,                             ## FEs
                     cluster = ~bvd_id_number,                          ## Clustered SEs
                     data = balanced_data)

bacon_decomposition <- bacon(log_sine_application ~ postTreated,
      data = balanced_data %>% filter(high_tech==1),
      id_var = "bvd_id_number", 
      time_var = "year"
)




  #### Only treated ------------------------------------------------------------

model_twfe_balanced_treated <- feols(log_patent_stock ~ I(postTreated*first_order_amount) +I(year*log_age) +I(log_pre_fixed_assets*year) | ## Our key interaction: time × treatment status
                               ## Other controls
                               year+bvd_id_number,                             ## FEs
                             cluster = ~bvd_id_number,                          ## Clustered SEs
                             data = balanced_data %>% filter( high_tech ==1 & supplier_status==1) %>% filter(first_order %notin% c(2008,2009)))

bacon_decomposition_treated <- bacon(log_patent_stock~ postTreated,
                             data = balanced_data %>% filter(supplier_status==1 & high_tech ==1 & first_order %notin% c(1995,2018,2019)) ,
                             id_var = "bvd_id_number", 
                             time_var = "year")


cs_results <- att_gt(
  yname = "log_patent_stock",
  tname = "year",
  xformla = ~ log_age + log_pre_fixed_assets, 
  idname = "bvd_id_numeric",
  gname = "first_order_2",
  data = full_panel_filtered%>% filter(high_tech==1 & first_order_2 %notin% c(2008,2009, 2004,2010)) ,
  control_group = "notyettreated",
  est_method = "ipw",
  clustervars = "bvd_id_numeric", 
  pl=TRUE, 
  cores=8,
  panel = TRUE,
  biters = 3000,
  bstrap = T,
  base_period="universal",
  allow_unbalanced_panel = TRUE
)

aggte(cs_results, type = "simple", na.rm =TRUE)

### Create a Dynamic TWFE



bacon(log_pat_applications ~i(time_to_treat, treat, ref = -1),
      data = balanced_data %>% filter(high_tech ==1),
      id_var = "bvd_id_number", 
      time_var ="year")                           ## FEs
                          ## Clustered SEs
      

  
  
model_twfe = feols(log_patent_stock~i(time_to_treat, treat, ref = -1)+## Our key interaction: time × treatment status
                       I(year*log_age)+I(year*log_pre_fixed_assets)|       ## Other controls
                      bvd_id_number + as.factor(year),                             ## FEs
                     cluster = ~bvd_id_number,                          ## Clustered SEs
                     data = full_panel_first_date %>% filter(high_tech ==1))
# check what the SUNAB is
model_sunab = feols(log_patent_stock~sunab(first_order_2, year)+## Our key interaction: time × treatment status
                      I(year*log_age)+I(year*log_pre_fixed_assets)  |   ## Other controls
                      bvd_id_number + as.factor(year),                             ## FEs
                     cluster = ~bvd_id_number,                          ## Clustered SEs
                     data = full_panel_first_date%>% filter(high_tech ==1))
  

### Create a Balanced Panel  
  
balanced_data <- make.pbalanced(full_panel_filtered, balance.type = "shared.times")

bacon_check<-bacon(log_patent_stock ~ postTreated,
                   data = balanced_data %>% filter(supplier_status==1),
                   id_var = "bvd_id_number",
                   time_var = "year")


  
  iplot(list(model_twfe, model_sunab), sep = 0.5, ref.line = -1,
        xlab = 'Time to treatment',
        main = 'Event study: Staggered treatment')
  legend("bottomleft", col = c(1, 2), pch = c(20, 17), 
         legend = c("TWFE", "Sun & Abraham (2020)"))
  

  balanced_data$supp
  
  
  bacon(log_pat_applications ~ postTreated,
        data = balanced_data %>%
          filter(supplier_status==1) %>%
          
          mutate(relativeTime = year - yexp2) %>%
          mutate(dins2 = dins + pmax(relativeTime, 0) * 0.01),
        id_var = "stfips",
        time_var = "year"
  )
  
  iplot(model_twfe,  xlab = 'Time to treatment', main = 'Event study: Staggered treatment (TWFE)')
  
  
check <- full_panel_first_date %>% select(bvd_id_number, year, first_order, first_order_dummy, age, log_pre_fixed_assets, pre_fixed_assets)
  
  model_twfe = felm(log_pat_applications ~   (postTreated*first_order_amount)| ## Our key interaction: time × treatment status
                                   ## Other controls
                      bvd_id_number + year,                             ## FEs
                     cluster = ~bvd_id_number,                          ## Clustered SEs
                     data = full_panel_first_date)
  
  full_panel_first_date_high_tech = full_panel_first_date %>% filter(high_tech==1)
  model_twfe = feols(log_sine_application ~ postTreated+I(year*log_age)+I(year*log_pre_fixed_assets)| ## Our key interaction: time × treatment status
                       ## Other controls
                       year+bvd_id_number,                             ## FEs
                     cluster = ~bvd_id_number,                          ## Clustered SEs
                     data = full_panel_filtered %>% filter(high_tech ==1 & supplier_status==1 ))
  
  
  bacon(log_pat_applications ~ postTreated,
          data = full_panel_first_date,
          id_var = "bvd_id_number", 
          time_var = "year"
        )
  
  
  limited_panel_1990<- limited_panel %>% filter(incorporation_year>1989)
  
  balance
  full_panel <- full_panel %>% mutate(age = first_order - incorporation_year)
  limited_panel<- full_panel %>% select(bvd_id_number, year,incorporation_year,fixed_assets, registration_year, registration_first_order, first_order, num_applications, pre_fixed_assets, age, pre_ebitda, treat, postTreated)
  vis_miss(limited_panel, warn_large_data = F)
  model.twfe.0 <- feols(patent_applications~treatment, index()
                        data=full_panel , cluster = "bvd_id_number") #use the clustered standard error  

  # Get robust standard errors
  robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
  
  # Update the summary of the model with robust standard errors
  coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  matlibrary(car)
  
  # Assuming 'dataset' is your dataset and 'y_vars' is a vector of outcome variables
  
  # Define your covariates
  all_covariates <- c("fixed_assets", "log_age", "operating_revenue_turnover_")
  
  # Placeholder for VIF results
  vif_results <- list()
  
  # Loop through each outcome variable
  for (y_var in y_vars) {
    # Construct the formula
    formula <- as.formula(paste(y_var, "~", paste(all_covariates, collapse = "+")))
    # Fit the model
    model <- lm(formula, data = matched_potential_suppliers_orbis_selected_age_20)
    # Calculate VIF and store results
    vif_results[[y_var]] <- vif(model)
  }
  
  # Print VIF results
  print(vif_results) 



# ### Descriptive statistics   ---------------------------------------------------------
vars_to_compute <- c("operating_revenue_turnover_", "ebitda","p_l_before_tax","fixed_assets", "age", "patent_stock", "patented")  # Replace with your variables

# Function to calculate means and t-test
# Modify the compute_stats function to return p-value
  compute_stats <- function(var_names, data) {
    results_df <- data.frame(variable = character(),
                             supplier_mean = numeric(),
                             potential_supplier_mean = numeric(),
                             p_value = numeric(),
                             stringsAsFactors = FALSE)
    
    for (name in var_names) {
      if(!name %in% names(data)) {
        stop(paste("Variable", name, "is not a column in the data"))
      }
      
      if(length(unique(data$supplier_status)) <= 1) {
        stop("supplier_status must have more than one level")
      }
      
      mean_results <- data %>%
        group_by(supplier_status) %>%
        summarise(mean_value = mean(get(name), na.rm = TRUE))
      
      p_value <- tidy(t.test(get(name) ~ supplier_status, data = data))$p.value
      
      temp_df <- data.frame(variable = name, 
                            supplier_mean = mean_results$mean_value[1], 
                            potential_supplier_mean = mean_results$mean_value[2], 
                            p_value = p_value,
                            stringsAsFactors = FALSE)
      
      results_df <- rbind(results_df, temp_df)
    }
    
    return(results_df)
  }
  
  

# Apply the function to each variable
results <- compute_stats(vars_to_compute, full_panel)
results<- as.data.frame(results)
# Convert to dataframe and set column names
df_results <- as.data.frame(results, stringsAsFactors = FALSE)

colnames(df_results) <- c("Variable", "Supplier", "Potential Supplier", "P-value")
df_results$Supplier <- format(round(as.numeric(df_results$Supplier), 2), big.mark = ",", nsmall = 2)
df_results$`Potential Supplier` <- format(round(as.numeric(df_results$`Potential Supplier`), 2), big.mark = ",", nsmall = 2)
df_results$`P-value` <- format(round(as.numeric(df_results$`P-value`), 2), big.mark = ",", nsmall = 2)


df_results$Variable <- c("Turnover", "EBITDA", "P&L before taxes", "Fixed assets", "Age", "Patent stock", "Prob. of patenting")

write.csv(df_results, here("results", "output", "descriptive_statistics_pot_supplier.csv"))

table_ <- xtable(df_results, digits=2, caption = "Summary statistics for Suppliers and potential")
table_3 <- print(table_3, include.rownames = TRUE)

# Save LaTeX table code to a file
write.table(table_3, here("results", "tables", "table_3.tex"), sep = "", row.names = FALSE, col.names = TRUE, quote = FALSE)


# Filter companies with num_applications = 0 for all years
full_panel$cumulative_patents[is.na(full_panel$cumulative_patents)] <- 0

companies_with_zero_applications <- full_panel %>%
  group_by(bvd_id_number, supplier_status) %>%
  filter(all(cumulative_patents == 0)) %>%
  summarise()

# Count the number of such companies
num_companies_with_zero_applications <- nrow(companies_with_zero_applications)

num_companies_with_zero_applications

# Filter for years 2003-2018 and compute patent applications per firm
# Compute average for pre-procurement (before first_order)
patent_summary<- full_panel %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarize(total_applications = sum(num_applications, na.rm = TRUE))

avg_pre_procurement <- matched_potential_suppliers_orbis_selected_age_20 %>%
  filter(year <= first_order) %>%
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarise(total_applications = sum(num_applications, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::summarise(mean_pre = mean(total_applications, na.rm = TRUE)) %>%
  pull(mean_pre)

avg_pre_stock_procurement<- full_panel %>%
  filter(year <= first_order) %>%
  filter(supplier_status==1) %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarise(patent_stock = sum(patent_stock, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::summarise(mean_pre = mean(patent_stock, na.rm = TRUE)) %>%
  pull(mean_pre)
# Compute average for post-procurement (after first_order)
avg_post_procurement <- full_panel %>%
  filter(year > first_order) %>%
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarise(total_applications = sum(num_applications, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::summarise(mean_post = mean(total_applications, na.rm = TRUE)) %>%
  pull(mean_post)

avg_post_stock_procurement <- full_panel %>%
  filter(year > first_order) %>%
  filter(supplier_status==1) %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarise(patent_stock = sum(patent_stock, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::summarise(mean_post = mean(patent_stock, na.rm = TRUE)) %>%
  pull(mean_post)


avg_patent_applications <- matched_potential_suppliers_orbis_selected_age_20 %>%
  dplyr::group_by(bvd_id_number) %>%
  filter(year<2018 & year>1994) %>% 
  dplyr::summarise(total_applications = sum(num_applications, na.rm = TRUE)) %>% 
  ungroup() %>% 
  dplyr::summarise(mean_patent = mean(total_applications, na.rm = TRUE)) %>%
  pull(mean_patent)



# Create table
result_table <- tibble(
  Period = c("Avg. pre-procurement", "Avg. post-procurement"),
  Mean = c(avg_pre_stock_procurement, avg_post_stock_procurement)
)

result_table

result_table



patent_summary <- full_panel %>%
  dplyr::group_by(bvd_id_number) %>%
  dplyr::summarise(
    total_applications = sum(num_applications, na.rm = TRUE))



# Classify firms based on total applications
patent_summary <- patent_summary %>%
  mutate(category = case_when(
    total_applications == 0 ~ "0",
    total_applications >= 1 & total_applications <= 50 ~ "1-50",
    total_applications >= 51 & total_applications <= 100 ~ "51-100",
    total_applications > 100 ~ ">100"
  ))

# Create the table
table_summary <- patent_summary %>%
  group_by(category) %>%
  summarise(
    number_of_firms = n(),
    percentage = (number_of_firms / nrow(patent_summary)) * 100
  )

# Compute average statistics for 2003-2018
avg_2003_2018 <- mean(patent_summary$total_applications)


# Dummy spacer row
spacer <- tibble(
  category = "SPACER",
  number_of_firms = NA,
  percentage = NA
)

# Combine the tables
combined_table <- bind_rows(
  table_summary,
  spacer,
  result_table
)
write.csv(combined_table, here("results", "output", "descriptive_table_3.csv"))
# Convert to LaTeX and replace the dummy row with vspace
combined_table
latex_code <- xtable(combined_table, align = c("l", "l", "r", "r", "l", "l"))
latex_output <- capture.output(print(latex_code, type = "latex", floating = FALSE))
latex_output[grepl("SPACER", latex_output)] <- "\\vspace{1cm}"
file_path <- here("results",  "tables", "table_3.tex")
writeLines(latex_output, file_path)

cat(latex_output, sep = "\n")
writeLines(cat(latex_output, sep = "\n"), here("output", "tables", "table_3.tex"))


graph_order_by_year <- full_panel %>% 
  filter(supplier_status ==1) %>% 
  select(first_order, bvd_id_number, year) %>% distinct()

saveRDS(graph_order_by_year, here("results","output", "graph_order_by_year.rds"))
proportions_data <- graph_order_by_year %>%
  group_by(year) %>%
  mutate(status = ifelse(year >= first_order, "Supplier", "Not Yet Supplier")) %>%
  group_by(year, status) %>%
  summarise(count = n_distinct(bvd_id_number)) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = count / total * 100)
# Plot
ggplot(proportions_data, aes(x = as.factor(year), y = percentage, fill = status)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_fill_manual(values = c("Supplier" = "blue", "Not Yet Supplier" = "grey"))+
  theme(legend.position = "bottom", legend.title = element_blank()) 

## Share of high-tech orders during the LHC

high_tech_orders <- full_panel %>% select(first_order,high_tech, bvd_id_number, LHC) %>% distinct()
orders_before_2009<- sum(high_tech_orders$LHC)
high_tech_orders_before_2009<- sum(high_tech_orders$high_tech & high_tech_orders$LHC)
share_of_high_tech_orders <- high_tech_orders_before_2009/orders_before_2009
print(share_of_high_tech_orders)

## Share of high-tech orders before the LHC

high_tech_orders_after_LHC <- full_panel %>% select(first_order, high_tech, bvd_id_number, LHC) %>% 
  distinct() %>% 
  filter(LHC==0)

orders_after_2008<- nrow(high_tech_orders_after_LHC)
high_tech_orders_after_LHC_number<- sum(high_tech_orders_after_LHC$high_tech)
share_of_high_tech_orders<- high_tech_orders_after_LHC_number/orders_after_2008

# Visualize missing data

panelview(log_patent_stock~ treatment + operating_revenue_turnover_, 
          data = matched_potential_suppliers_orbis_selected_age_20, index = c("bvd_id_number","year"), 
          pre.post = TRUE)
