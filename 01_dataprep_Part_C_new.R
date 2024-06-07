#' ---
#' title: ORBIS-CERN procurement database
#' author: Gabriele Piazza
#' date: 2023-06-27
#' ---



# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign", "haven", "fuzzyjoin", "data.table", "visdat", "beepr", "lubridate") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)

## 1.2 Create functions ----------------------------------------------------
`%notin%` <- Negate(`%in%`)

# Define the function to split the data frame into custom parts and write to CSV
split_and_write_csv <- function(data, split_size, output_dir) {
  # Calculate the number of rows in each subset
  total_rows <- nrow(data)
  num_splits <- ceiling(total_rows / split_size)
  
  # Loop through to create subsets and write to CSV
  for (i in 1:num_splits) {
    start <- (i - 1) * split_size + 1
    end <- min(i * split_size, total_rows)
    
    # Subset the data
    subset_data <- data[start:end, ]
    
    # Define the file name
    file_name <- paste0("patent_lookup_subset_", i, ".csv")
    file_path <- here(output_dir, file_name)
    
    # Write the subset to a CSV file
    write.csv(subset_data, file_path, row.names = FALSE)
  }
}

# 2. Load the data --------------------------------------------------------

## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"

## file names for CERN suppliers and potential
matched_suppliers_orbis_file <- "matched_suppliers_orbis_data"# file for matched suppliers
matched_potential_suppliers_orbis_file <- "matched_potential_suppliers_orbis_data" #file for matched potential suppliers
all_orders_tech_balance_file <- "all_orders_tech_balance" #all orders with with tech and balance matched
potential_suppliers_registration_file <- "22_10_31_potential_suppliers.csv"
incorporation_suppliers_file <- "Export 11_07_2023 13_43_suppliers_incorporation_date.xlsx"
supplier_patent_lookup_dir <- paste0(data_proc_dir, "suppliers_patent_lookup")
# suppliers_registration_file <- "suppliers_registration_year.csv"



## 2.2 Loading the data  --------------------------------


### Registered
potential_registration <- read_csv(paste0(data_raw_dir, potential_suppliers_registration_file)) %>% 
  clean_names() %>% 
  rename(company_name = suppliername)
### All Orders with tech lookup and balanc e
all_orders_tech_balance<- readRDS(paste0(data_proc_dir, all_orders_tech_balance_file))

## Matched Orbis Data
### Suppliers
matched_suppliers_orbis_data<- readRDS(paste0(data_proc_dir, matched_suppliers_orbis_file))
### Registered
matched_potential_suppliers_orbis_data<- readRDS(paste0(data_proc_dir, matched_potential_suppliers_orbis_file))

incorporation_date<-  read_excel(paste0(data_raw_dir,"Export 11_07_2023 13_43_suppliers_incorporation_date.xlsx"),  sheet = "Results", 
                                col_types = c("text","text", "date", "text", "text", "numeric"))

incorporation_date<- incorporation_date %>% select(-'Column1',-"Company name Latin alphabet",-'Branch indicator') %>% clean_names() %>% 
  rename(bvd_id_number = bv_d_id_number) %>% 
  mutate(incorporation_year = year(date_of_incorporation)) %>% 
  select(-date_of_incorporation)



# 3. Data Manipulation  ---------------------------------------------------


## 3.1 Matched suppliers -------------------------------------------------------

#Matke changes to the year variable
# This follows the convention explained in the Kalemli-Ozcan paper
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)

matched_suppliers_orbis_data<- matched_suppliers_orbis_data %>% 
  mutate(closing_date_format = str_remove(closing_date,"T00:00:00.000Z" ),
        date_closing = as.Date(closing_date_format),
        year_orbis = year(closing_date_format),
        month_orbis = month(closing_date_format), 
        year= case_when (month_orbis <6 ~ year_orbis -1, 
                         month_orbis >5 ~ year_orbis),
        matching_year = year)

## Create BVD ID lookup 
bvd_id_lookup <- matched_suppliers_orbis_data %>% 
  select(bvd_id_number, company_name) %>% distinct()# I need this to fill the missing year registration

# I merge the new file with the bvd id lookup
all_orders_tech_balance<- all_orders_tech_balance %>% 
  left_join(bvd_id_lookup)

# There are some NAs for registration year 
all_orders_tech_balance <- all_orders_tech_balance %>%
  group_by(bvd_id_number) %>% # Group the data by 'bvd_id_number'
  # Create or modify variables within each group
  mutate(
    # Create a logical variable 'has_non_missing' that checks if there's at least one non-missing 'registration_year'
    has_non_missing = !all(is.na(registration_year)),
    # Modify 'registration_year' based on the condition of 'has_non_missing'
    registration_year = ifelse(
      has_non_missing, 
      # If 'has_non_missing' is TRUE, replace NA values or values greater than the minimum 'registration_year' in the group
      replace(registration_year, 
        is.na(registration_year) | registration_year > min(registration_year, na.rm = TRUE), 
        min(registration_year, na.rm = TRUE)
      ), # If 'has_non_missing' is FALSE (all values are NA), set 'registration_year' to NA
      NA)
  ) %>% # Ungroup the data to remove the grouping structure
  ungroup() %>% # Select all columns except 'has_non_missing' to remove the temporary variable
  select(-has_non_missing)

# I create a matching year variable before matching to the orbis data
all_orders_tech_balance$matching_year <- all_orders_tech_balance$order_date 
all_orders_tech_balance_selected_countries<- all_orders_tech_balance %>% 
  filter(country %in% c("ES", "FR", "IT", "GB")) %>% 
  filter(!is.na(bvd_id_number))
check_orders<- all_orders_tech_balance_selected_countries %>% select(bvd_id_number, matching_year, order_date) %>% 
distinct()


all_orders_tech_balance_selected_countries_vars<- all_orders_tech_balance_selected_countries %>% 
  select(bvd_id_number, country, order_date, chf_amount, tech_intensity, tech_level, registration_year, order_number, code_2_digits, subroject, subproject_1) %>% 
  distinct() %>% 
  filter(!is.na(order_date)) %>% 
  group_by(bvd_id_number, order_date) %>% 
  mutate(
    total_chf_amount_year = sum(chf_amount, na.rm = TRUE),
    max_tech = max(tech_level, na.rm = TRUE),
    number_orders = n_distinct(order_number),
    code_2_digits = ifelse(length(code_2_digits) == 0, 0, code_2_digits[which.max(chf_amount)]),
    subroject_max = ifelse(all(is.na(subroject)), NA, subroject[which.max(chf_amount)]),
    subroject_max_1 = ifelse(all(is.na(subroject)), NA, subproject_1[which.max(chf_amount)]),
  ) %>%
  distinct() %>% 
  drop_na(total_chf_amount_year) %>% 
  select(-tech_intensity, -chf_amount, -order_number) %>% 
  distinct() %>% ungroup() %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_order= min(order_date),last_order = max(order_date), total_orders = sum(number_orders),
         total_orders_amount = sum(total_chf_amount_year),
         first_order_amount = total_chf_amount_year[which.min(order_date)],
         first_order_tech = max_tech[which.min(order_date)],
         code_2_digits = code_2_digits[which.min(order_date)],
         subproject_first_year = subroject_max[which.min(order_date)], 
         subproject_1_first_year = subroject_max_1[which.min(order_date)],
         registration_first_order = first_order -as.numeric(registration_year),
         matching_year = order_date)%>% 
  select(bvd_id_number, order_date, matching_year, registration_year, total_chf_amount_year, max_tech, first_order, last_order, total_orders, total_orders_amount, 
         first_order_amount, first_order_tech, registration_first_order, code_2_digits, subproject_first_year, subproject_1_first_year)%>% 
  distinct() %>% 
  ungroup()
  
## Merge orders and orbis data
matched_suppliers_orbis_data$matching_year <- matched_suppliers_orbis_data$year
matched_suppliers_orbis_data<- matched_suppliers_orbis_data %>% 
  left_join(all_orders_tech_balance_selected_countries_vars, by =c("bvd_id_number", "matching_year")) 




matched_suppliers_orbis_data<- matched_suppliers_orbis_data %>% select( -identifier, -score, -nacerev2mainsection, 
                                                                         -nacerev2mainsection, -nacerev2corecode4digits,-country_iso_code, 
                                                                        -nacerev2secondarycodes,  -fax_number, -region_in_country,
                                                                        -type_of_region_in_country,- street_no_building_etc_line_1,-street_no_building_etc_line_1_native, -street_no_building_etc_line_2, 
                                                                        -street_no_building_etc_line_2_native, -street_no_building_etc_line_3, -street_no_building_etc_line_3_native) %>% distinct()




matched_suppliers_orbis_data_vars<- matched_suppliers_orbis_data %>% 
  group_by(bvd_id_number) %>% fill(nacerev2primarycodes, registration_year, registration_first_order,
                                   total_chf_amount_year, max_tech, first_order, last_order, total_orders, total_orders_amount, 
                                   first_order_amount, first_order_tech, registration_first_order,  subproject_first_year, subproject_1_first_year) %>% 
  ungroup() %>% 
  distinct()





saveRDS(matched_suppliers_orbis_data_vars, paste0(data_proc_dir, "matched_suppliers_orbis_data_vars"))



# I create a new variables for the consolidations codes. Before doing this, I have U1, U2, C1, and C2. I want just the initial letters. 

matched_suppliers_orbis_data_vars$consolidation_l <- substr(matched_suppliers_orbis_data_vars$consolidation_code,1,1)
matched_suppliers_orbis_data_vars_unconsolidated <- matched_suppliers_orbis_data_vars %>% 
  filter(consolidation_l =="U") %>% distinct()

matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>% 
  mutate(first_year = min(year),
         last_year = max(year),
         order_after_last_orbis = last_year<first_order)

matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated%>% 
  group_by(bvd_id_number, year) %>% 
  filter( ebitda == max(ebitda))%>% # if multiple ebitda per year, I get the maximum
  select( -consolidation_code, -matched_company_name, -nacerev2primarycodes) %>% 
  ungroup() %>% 
  distinct()


matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>% 
  mutate(year_after_order = last_year - first_order,
         year_before_order = first_order -first_year) %>% 
  ungroup() %>% 
  distinct()


matched_suppliers_orbis_data_vars_unconsolidated <- matched_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(first_order = ifelse(!is.na(first_order), first_order, last(na.omit(first_order))),
         first_order_amount = ifelse(!is.na(first_order_amount), first_order_amount, last(na.omit(first_order_amount))),
         first_order_tech = ifelse(!is.na(first_order_tech), first_order_tech, last(na.omit(first_order_tech))),
         total_orders_amount = ifelse(!is.na(total_orders_amount), total_orders_amount, last(na.omit(total_orders_amount))),
         total_orders = ifelse(!is.na(total_orders), total_orders, last(na.omit(total_orders))),
         last_order = ifelse(!is.na(last_order), last_order, last(na.omit(last_order))),
         subproject_first_year= ifelse(!is.na(subproject_first_year), subproject_first_year, last(na.omit(subproject_first_year))), 
         subproject_1_first_year = ifelse(!is.na(subproject_1_first_year), subproject_1_first_year, last(na.omit(subproject_1_first_year))),
         registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         registration_first_order = ifelse(!is.na(registration_first_order), registration_first_order, last(na.omit(registration_first_order))),
         year_after_order = ifelse(!is.na(year_after_order), year_after_order, last(na.omit(year_after_order))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year))),
         year_before_order = ifelse(!is.na(year_before_order), year_before_order, last(na.omit(year_before_order)))) %>%
  ungroup()

number_companies <- unique(matched_suppliers_orbis_data_vars_unconsolidated$bvd_id_number)
# I now create some variables for years before and after order - I want to see whether there is data  pre and post treatment 
matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>% 
  mutate(year_after_order = last_year - first_order,
         year_before_order = first_order -first_year) %>% 
  ungroup() %>% 
  distinct()


matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>% 
  select(-city, - closing_date, -street_no_building_etc_line_4, -street_no_building_etc_line_4_native, -postcode, 
         -city_native, -telephone_number, -address_type, -date_closing, -closing_date_format) %>% distinct()

saveRDS(matched_suppliers_orbis_data_vars_unconsolidated, paste0(data_proc_dir, "matched_suppliers_orbis_data_vars_unconsolidated"))


## Add the incorporation year
matched_suppliers_orbis_data_vars_unconsolidated_inc <- left_join(matched_suppliers_orbis_data_vars_unconsolidated, incorporation_date)

companies_number_suppliers <- unique(matched_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number)

#Create the lookup for patents

patent_bvd_lookup <- matched_suppliers_orbis_data_vars_unconsolidated_inc %>% 
  select(bvd_id_number) %>% distinct()

split_and_write_csv(patent_bvd_lookup, 15, supplier_patent_lookup_dir)



# ### 2.53 I create the date variable -------------------------------------
all_matched_potential_suppliers<-all_matched_potential_suppliers %>% 
  mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z"),
         date_closing = as.Date(closing_date_format))

# I then extract month and year 
all_matched_potential_suppliers_clean<-all_matched_potential_suppliers %>% 
  mutate(year_orbis = year(closing_date_format),
         month_orbis = month(closing_date_format))


# This follows the convention explained in the Kalemli-Ozcan paper
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)

all_matched_potential_suppliers_clean<- all_matched_potential_suppliers_clean %>% mutate(year = case_when (month_orbis <6 ~ year_orbis -1, 
                                                                                                     month_orbis >5 ~ year_orbis))
all_matched_potential_suppliers_clean<- all_matched_potential_suppliers_clean %>% select( -identifier, -score, -nacerev2mainsection, -nationalindustryclassificationus, -primarycodesinthisclassification, -primarycodeinnationalindustrycla, -secondarycodesinthisclassificati,
                                                                       -secondarycodeinnationalindustryc, -nacerev2mainsection, -nacerev2corecode4digits,-nacerev2corecodetextdescription,
                                                                       -nacerev2secondarycodes,-nacerev2secondarycodetextdescrip, -postcode, -city.y) %>% distinct()

# I create a new variables for the consolidations codes. Before doing this, I have U1, U2, C1, and C2. I want just the initial letters. 

all_matched_potential_suppliers_clean$consolidation_l <- substr(all_matched_potential_suppliers_clean$consolidation_code,1,1)


all_matched_potential_suppliers_clean<-all_matched_potential_suppliers_clean %>% 
  group_by(bvd_id_number) %>% fill(year_supplier_registration) %>% ungroup()

all_matched_potential_suppliers_clean<- all_matched_potential_suppliers_clean %>% 
  rename(registration_year = year_supplier_registration) 
all_matched_potential_suppliers_clean<- all_matched_potential_suppliers_clean %>% 
  select(-city,-city.x, -nacerev2primarycodes, -contact, - nacerev2primarycodes, -email, 
         -email_y_n, -x1_digit, -matched_company_name, -company_name, -suppliercode) %>% distinct()
  
unconsolidated_accounts_potential<- all_matched_potential_suppliers_clean %>% 
  filter(consolidation_l =="U")

unconsolidated_accounts_potential<- unconsolidated_accounts_potential %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_year = min(year),
         last_year = max(year)) %>% 
  ungroup() %>% 
  distinct()

unconsolidated_accounts_potential<- unconsolidated_accounts_potential %>% 
  group_by(bvd_id_number, year) %>% 
  filter( ebitda == max(ebitda))%>%
  select(-filing_type) %>% 
  ungroup() %>% 
  distinct() %>% 
  ungroup() %>% 
  filter(!is.na(registration_year))


check<- unconsolidated_accounts_potential %>% filter(!is.na(registration_year))
number_companies_with_code <- unique(check$bvd_id_number)

number_observations_pot_variables<- unconsolidated_accounts_potential %>% 
  dplyr::filter(year<ΩΩ & year>=1995) %>% 
  dplyr::group_by(bvd_id_number,year) %>% 
  dplyr::filter(ebitda == max(ebitda)) %>% # I have notices some discrepancies when multiple ebitda are filed each year. I select the largest one
  dplyr::select(bvd_id_number, year, operating_revenue_turnover_, p_l_before_tax, p_l_after_tax, ebitda) %>% 
  distinct() %>% 
  ungroup() %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarize(observations_turnover = sum(!is.na(operating_revenue_turnover_)),
            observations_p_l_b_tax = sum(!is.na(p_l_before_tax)),
            observations_p_l_a_tax = sum(!is.na(p_l_after_tax)),
            observations_ebitda = sum(!is.na(ebitda)))

unconsolidated_accounts_potential<- unconsolidated_accounts_potential %>% 
  filter(year >=1989 & year <= 2020 )
number_companies_with_code <- unique(unconsolidated_accounts_potential$bvd_id_number)


# Create a panel data
year<- seq(1990, 2020) # I am including all the data between 1995 and 2019 but no strong reasons to to this 
year <- rep(1990:2020, each = 484) # this is the number of companies that I have 
bvd_id_number <- unique(unconsolidated_accounts_potential$bvd_id_number) # This gives you the unique companies
bvd_id_number<-(rep(bvd_id_number, each = 31)) # this repeats times 25, the number of years 
merged_data_pot <- expand.grid(year = unique(year), bvd_id_number = unique(bvd_id_number))# this creates a grid
merged_data_pot <- merged_data_pot[order(merged_data_pot$bvd_id_number, merged_data_pot$year), ]
merged_data_pot$year <- as.numeric(merged_data_pot$year)
# I am now joining this new grid with the data that I have
full_panel_pot_suppliers_unconsolidated<- left_join(merged_data_pot, unconsolidated_accounts_potential)

#-- This might be redundant as companies might be assigned different codes
CERN_orders_techlevel <- read_excel("~/Dropbox/PhD/procurement_cern/data/raw/CERN_techlevel.xlsx")
CERN_orders_techlevel<-clean_names(CERN_orders_techlevel)
CERN_orders_techlevel<-CERN_orders_techlevel %>% rename(activitycode=x3_digits )# columns have different names
tech_level<-CERN_orders_techlevel %>% select(activitycode,tech_intensity) # don't need the other columns so  am dropping them 
full_panel_pot_suppliers_unconsolidated<- left_join(full_panel_pot_suppliers_unconsolidated, tech_level)
# Create change variables -------------------------------------------------

full_panel_pot_suppliers_unconsolidated <- full_panel_pot_suppliers_unconsolidated %>% 
  group_by(bvd_id_number) %>%
  arrange(year) %>% 
  mutate(
    operating_revenue_turnover_rate_change = (operating_revenue_turnover_ - lag(operating_revenue_turnover_)) / lag(operating_revenue_turnover_),
    ebitda_rate_change = ((ebitda - lag(ebitda)) / (abs(lag(ebitda))*100)),
    p_l_before_tax_rate_change = ((p_l_before_tax - lag(p_l_before_tax)) / (abs(lag(p_l_before_tax))*100)),
    p_l_after_tax_rate_change = ((p_l_after_tax - lag(p_l_after_tax))/(abs(lag(p_l_after_tax))*100)), 
    operating_revenue_turnover_change = operating_revenue_turnover_ - lag(operating_revenue_turnover_), 
    ebitda_change = ebitda - lag(ebitda), 
    p_l_before_tax_change = p_l_before_tax - lag(p_l_before_tax),
    p_l_after_tax_change = p_l_after_tax - lag(p_l_after_tax)
  )

# I now have many NAs for variables that do not change over time, so I try to fill them with existing values. t
full_panel_pot_suppliers_unconsolidated<- full_panel_pot_suppliers_unconsolidated %>% 
  group_by(bvd_id_number) %>% 
  fill(registration_year, first_year, last_year, )

# The issue is that the fill function only works when you have data prior to NAs. If the first year is NA, you might have some issues 
# This script should address the issue 

full_panel_pot_suppliers_unconsolidated <- full_panel_pot_suppliers_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         x2_digit = ifelse(!is.na(x2_digit), x2_digit, last(na.omit(x2_digit))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year)))) %>%  
  rename(code_2_digits =x2_digit) %>%  ungroup()

full_panel_pot_suppliers_unconsolidated <- full_panel_pot_suppliers_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(exit_year = ifelse(year > last_year & last_year < 2018, 1, 0),
         entry_year = ifelse(year >= first_year | year >= registration_year, 1, 0),
         entry_year = ifelse(year < first_year, 0, entry_year)) %>%
  ungroup()


full_panel_pot_suppliers_unconsolidated<-left_join(full_panel_pot_suppliers_unconsolidated, number_observations_pot_variables)
full_panel_pot_suppliers_unconsolidated<- full_panel_pot_suppliers_unconsolidated %>% filter(year>1994)

full_panel_pot_suppliers_unconsolidated_nona<- full_panel_pot_suppliers_unconsolidated %>% 
  filter(!is.na(observations_turnover) & !is.na(observations_ebitda) & !is.na(observations_p_l_a_tax) & !is.na(observations_p_l_b_tax))


# #### Consolidation year -------------------------------------------------



list_full_panel_pot_suppliers_unconsolidated_nona <-full_panel_pot_suppliers_unconsolidated_nona %>% select(bvd_id_number) %>% distinct()
write.csv(list_full_panel_pot_suppliers_unconsolidated_nona, here("Analysis","data_proc", "bvd_incorporation_pot_lookup.csv"), row.names = F)


incorporation_date_pot_suppliers<-  read_excel(here("Analysis","data_raw","Export 11_07_2023 18_02_pot_suppliers_incorporation.xlsx"), 
                                               sheet = "Results", col_types = c("text", 
                                                                                "text", "numeric", "text", "text", "date"))

incorporation_date_pot_suppliers<- incorporation_date_pot_suppliers %>% select(-'Column1',-"Company name Latin alphabet")
incorporation_date_pot_suppliers<- clean_names(incorporation_date_pot_suppliers)
incorporation_date_pot_suppliers<- incorporation_date_pot_suppliers %>% rename(bvd_id_number = bv_d_id_number)
incorporation_date_pot_suppliers <- incorporation_date_pot_suppliers %>% mutate(incorporation_year = year(date_of_incorporation)) %>% select(-date_of_incorporation)
incorporation_date_pot_suppliers$last_avail_year<- as.numeric(incorporation_date_pot_suppliers$last_avail_year)

full_panel_pot_suppliers_unconsolidated_nona <- left_join(full_panel_pot_suppliers_unconsolidated_nona, incorporation_date_pot_suppliers)

full_panel_pot_suppliers_unconsolidated_nona <- full_panel_pot_suppliers_unconsolidated_nona %>%
  group_by(bvd_id_number) %>%
  mutate(exit_year = ifelse(year > last_avail_year & last_avail_year < 2018, 1, 0),
         entry_year = ifelse(year >= incorporation_year | year >= registration_year, 1, 0),
         entry_year = ifelse(year < incorporation_year, 0, entry_year)) %>%
  ungroup()



full_panel_pot_suppliers_unconsolidated_25_all_outcomes_nona<-full_panel_pot_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover==25 & observations_ebitda ==25 & observations_p_l_b_tax==25 & observations_p_l_a_tax ==25)
pot_suppliers_full_data_25<- full_panel_pot_suppliers_unconsolidated_25_all_outcomes_nona %>% select(bvd_id_number) %>% distinct()

full_panel_pot_suppliers_unconsolidated_24_all_outcomes_nona<- full_panel_pot_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover==24 & observations_ebitda ==24 & observations_p_l_b_tax==24 & observations_p_l_a_tax==24)
pot_suppliers_full_data_24<- full_panel_pot_suppliers_unconsolidated_24_all_outcomes_nona %>% select(bvd_id_number) %>% distinct()

full_panel_pot_suppliers_unconsolidated_nona_random<- full_panel_pot_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover<24 & observations_ebitda <24 & observations_p_l_b_tax<24 & observations_p_l_a_tax<24)

missing_random_years<- full_panel_pot_suppliers_unconsolidated_nona_random %>% group_by(bvd_id_number) %>% 
  summarize(entry_years = sum(entry_year),
            exit_years = sum(exit_year)) %>% 
  filter(exit_years ==0 & entry_years==25)


  filter(entry_year==1 & exit_year==0) %>%
  select(bvd_id_number) %>% distinct()

exited_companies<- full_panel_pot_suppliers_unconsolidated_nona %>% 
  filter(exit_year ==1) %>% group_by(bvd_id_number) %>% 
  mutate(number = n()) %>% 
  select(bvd_id_number) %>% 
  distinct()

exited_companies_list <- unique(exited_companies$bvd_id_number)
exited_companies<- full_panel_suppliers_unconsolidated_nona %>% 
  filter(bvd_id_number %in% exited_companies_list)

entered_companies_pot<- full_panel_pot_suppliers_unconsolidated_nona %>% 
  filter(entry_year==0)  
enter_pot_companies_list <- unique(entered_companies_pot$bvd_id_number)
entered_companies_pot<- full_panel_pot_suppliers_unconsolidated_nona %>% 
  filter(bvd_id_number %in% enter_companies_list)

entered_exited_companies_pot <- entered_companies_pot %>% filter(exit_year==1) %>% 
  select(bvd_id_number) %>% distinct()

# Patent Lookup-------------------------------------------------------------------------

#  
patent_pot_bvd_lookup <- full_panel_pot_suppliers_unconsolidated %>% 
  select(bvd_id_number) %>% distinct()
# Determine the total number of rows in the data
total_rows <- nrow(patent_pot_bvd_lookup)

# Set the desired subset size
subset_size <- 100

# Calculate the number of subsets needed
num_subsets <- ceiling(total_rows / subset_size)

# Loop over subsets and write to separate CSV files
for (i in 1:num_subsets) {
  # Calculate the start and end row indices for the subset
  start_row <- (i - 1) * subset_size + 1
  end_row <- min(i * subset_size, total_rows)
  
  # Extract the subset of rows
  subset <- patent_pot_bvd_lookup[start_row:end_row, ]
  
  # Generate the filename for the subset
  filename <- paste0("patent_pot_lookup_subset", i, ".csv")
  
  # Write the subset to CSV
  write.csv(subset, here("data_proc","pot_suppliers_patent_lookup", filename), row.names = FALSE)
}



# Merge with patent data  -------------------------------------------------


patent_data_pot_suppliers<- read_csv(here("Analysis","data_proc", "patents_pot_suppliers_1995_2019.csv"))
patent_data_pot_suppliers<- patent_data_pot_suppliers %>% select(-'...1')
full_panel_pot_suppliers<- left_join(full_panel_pot_suppliers_unconsolidated_nona, patent_data_pot_suppliers)

full_panel_pot_suppliers <- full_panel_pot_suppliers %>%
  mutate(
    num_applications = replace(num_applications, is.na(num_applications), 0),
    num_publications = replace(num_publications, is.na(num_publications), 0),
    cumulative_patents = replace(cumulative_patents, is.na(cumulative_patents), 0),
    lag_cum_applications = replace(lag_cum_applications, is.na(lag_cum_applications), 0),
    stock_patent = replace(stock_patent, is.na(stock_patent), 0),
    lag_patent_stock = replace(lag_patent_stock, is.na(lag_patent_stock), 0),
    patent_stock = replace(patent_stock, is.na(patent_stock), 0)
  )
full_panel_pot_suppliers<- full_panel_pot_suppliers %>% distinct()
full_panel_pot_suppliers<- full_panel_pot_suppliers %>% mutate(supplier_status =0)

full_panel_pot_suppliers <- full_panel_pot_suppliers %>%
  group_by(bvd_id_number) %>%
  mutate(tech_intensity = ifelse(!is.na(tech_intensity), tech_intensity, last(na.omit(tech_intensity))),
         activitycode = ifelse(!is.na(activitycode), activitycode, last(na.omit(activitycode))),
         country = ifelse(!is.na(country), country, last(na.omit(country))),
         code_2_digits = ifelse(!is.na(code_2_digits), code_2_digits, last(na.omit(code_2_digits))),
         
        incorporation_year = ifelse(!is.na(incorporation_year), incorporation_year, last(na.omit(incorporation_year)))) %>%   ungroup()

bvd_id_lookup_potential_suppliers_size <- full_panel_pot_suppliers %>% select(bvd_id_number) %>% distinct()

write.csv(full_panel_pot_suppliers, here("Analysis","data_proc", "full_panel_pot_suppliers.csv"))
write.csv(bvd_id_lookup_potential_suppliers_size, here("Analysis", "data_proc", "bvd_id_lookup_pot_suppliers_size.csv"))



# Load size postcode lookup -----------------------------------------------
# load the files 
size_nace_postcode_pot_suppliers <- read_excel("Analysis/data_proc/size_nace_postcode_pot_suppliers.xlsx", sheet ="Results")

size_nace_postcode_pot_suppliers <- size_nace_postcode_pot_suppliers  %>% 
  select(-'...1') %>% 
  rename(company_name ='Company name Latin alphabet',bvd_id_number = 'BvD ID number',
         nace_main_section = 'NACE Rev. 2 main section', size_classification = 'Size classification',
         nace_4_digits = 'NACE Rev. 2, core code (4 digits)', bvd_sectors = 'BvD sectors', 
         postcode ='Postcode\nLatin Alphabet')

full_panel_pot_suppliers<- left_join(full_panel_pot_suppliers, size_nace_postcode_pot_suppliers)

# this does not work 

full_panel_pot_suppliers<- full_panel_pot_suppliers %>%
  select(-date_closing, -consolidation_l, -closing_date_format) %>% 
  group_by(bvd_id_number) %>%
  mutate(first_order = 0,
         first_order_amount = 0,
         first_order_tech = 0,
         total_orders_amount = 0,
         total_orders = 0,
         last_order = 0,
         first_year = 0,
         last_year =0,
         country = 0) %>%
  ungroup()

write.csv(full_panel_pot_suppliers, here("Analysis","data_proc", "full_panel_pot_suppliers.csv"))


