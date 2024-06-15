#' ---
#' title: 01_dataprep_C
#' author: Gabriele Piazza
#' date: 2024-06-01
#' Description: This script creates the potential suppliers dataset that includes information from Orbis
#' including financials, patents and other characteristics. 

# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign", "haven", "fuzzyjoin", "data.table", "visdat", "beepr", "lubridate", "readxl") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)

## 1.2 Create functions ----------------------------------------------------
`%notin%` <- Negate(`%in%`)

# Function to read the "results" sheet from an Excel file, used for the orbis data
read_results_sheet <- function(file) {
  read_excel(file, sheet = "Results",col_types = c(rep("guess", 8), "text", rep("guess", 5)))
}

# Function to handle different date formats and serial numbers
convert_to_year <- function(x) {
  # If it's a year, return as is
  if (nchar(x) == 4 && grepl("^[0-9]{4}$", x)) {
    return(as.numeric(x))
  }
  # If it's a numeric value but not a year, attempt to convert to date
  if (grepl("^[0-9]+$", x)) {
    # Try converting to date from origin (Excel's origin is "1899-12-30")
    date_value <- as.Date(as.numeric(x), origin = "1899-12-30")
    # Check if the conversion resulted in a reasonable year
    if (year(date_value) > 1800 && year(date_value) < 2100) {
      return(year(date_value))
    } else {
      # If not a reasonable date, return NA
      return(NA)
    }
  }
  # If it's a date in day-month-year format, convert it
  if (grepl("/", x)) {
    parsed_date <- dmy(x)
    if (!is.na(parsed_date)) {
      return(year(parsed_date))
    }
  }
  # Default case for other formats
  return(NA)
}

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
    file_name <- paste0("lookup_subset_", i, ".csv")
    file_path <- here(output_dir, file_name)
    
    # Write the subset to a CSV file
    write.csv(subset_data, file_path, row.names = FALSE)
  }
}


# 2. Load the data --------------------------------------------------------

## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data folders 
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"

## file names for CERN  potential
matched_potential_suppliers_orbis_file <- "matched_potential_suppliers_orbis_data" #file for matched potential suppliers
potential_suppliers_registration_file <- "22_10_31_potential_suppliers.csv"


pot_supplier_patent_lookup_dir <- paste0(data_proc_dir, "pot_suppliers_patent_lookup")

potential_suppliers_tech_balance_file <-"potential_suppliers_tech_balance"
# suppliers_registration_file <- "suppliers_registration_year.csv"

## 2.2 Loading the data  --------------------------------


### Registration
potential_suppliers_tech_balance <- readRDS(paste0(data_proc_dir, potential_suppliers_tech_balance_file)) %>% 
  clean_names() %>% 
  rename(company_name = suppliername)

#Orbis Data
matched_potential_suppliers_orbis_data<- readRDS(paste0(data_proc_dir, matched_potential_suppliers_orbis_file))

#Incorporation
#incorporation_nace_size_list <- lapply(incorporation_suppliers_file,read_results_sheet)
#incorporation_nace_size_data<- do.call(rbind, incorporation_nace_size_list)
#incorporation_nace_size_data<- incorporation_nace_size_data %>% select(-'...1') %>%
#  clean_names() %>% 
 # rename(bvd_id_number = bv_d_id_number) %>% 
  # Identify and process year-only entries
  #mutate(
   # date_of_incorporation = as.character(date_of_incorporation),
    #incorporation_year = sapply(date_of_incorporation, convert_to_year))

# 3. Data Manipulation  ---------------------------------------------------


## 3.1 Matched  potential suppliers -------------------------------------------------------

#Make changes to the year variable
# This follows the convention explained in the Kalemli-Ozcan paper
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)

matched_potential_suppliers_orbis_data<- matched_potential_suppliers_orbis_data %>% 
  mutate(closing_date_format = str_remove(closing_date,"T00:00:00.000Z" ),
         date_closing = as.Date(closing_date_format),
         year_orbis = year(closing_date_format),
         month_orbis = month(closing_date_format), 
         year= case_when (month_orbis <6 ~ year_orbis -1, 
                          month_orbis >5 ~ year_orbis),
         matching_year = year)

## Create BVD ID lookup 
bvd_id_lookup <- matched_potential_suppliers_orbis_data %>% 
  select(bvd_id_number, company_name) %>% distinct()# I need this to fill the missing year registration

# I merge the new file with the bvd id lookup !
 potential_suppliers_tech_balance<- potential_suppliers_tech_balance %>%
   left_join(bvd_id_lookup)

# There are some NAs for registration year 
potential_suppliers_tech_balance <- potential_suppliers_tech_balance %>%
  filter(!is.na(bvd_id_number)) %>% 
  group_by(bvd_id_number) %>%   # Group the data by 'bvd_id_number'
  rename(registration_year = year_supplier_registration) %>% 
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
# potential_suppliers_tech_balance$matching_year<- potential_suppliers_tech_balance$registration_year  
potential_suppliers_tech_balance_selected_countries<- potential_suppliers_tech_balance %>% 
  filter(country %in% c("ES", "FR", "IT", "GB")) %>% 
  filter(!is.na(bvd_id_number))


potential_suppliers_tech_balance_selected_countries_vars<- potential_suppliers_tech_balance_selected_countries %>% 
  select(bvd_id_number, country, registration_year, tech_intensity, tech_level, well_balanced, supplies_ms_status, x2_digit) %>% 
  distinct() %>% 
  filter(!is.na(registration_year)) %>% 
  group_by(bvd_id_number, registration_year) %>% 
  mutate(max_tech = max(tech_level, na.rm = TRUE),
         matching_year = registration_year) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(bvd_id_number, country, registration_year, well_balanced,max_tech,  supplies_ms_status) %>% #I drop the tech level because I still have multiples 
  # I am dropping the x2_digit because I have multiple for the same company and year
  distinct() %>% 
  rename(tech_level = max_tech)
  
# I checked and all the bvds only have one year of registration


## Merge orders and orbis data
matched_potential_suppliers_orbis_data$matching_year <- matched_potential_suppliers_orbis_data$year# this creates the matching year
matched_potential_suppliers_orbis_data<- matched_potential_suppliers_orbis_data %>% 
  left_join(potential_suppliers_tech_balance_selected_countries_vars, by =c("bvd_id_number")) %>% 
  filter(!is.na(registration_year)) # it seems that I have some companies that are potential suppliers for which I don't have the registration year

matched_potential_suppliers_orbis_data<- matched_potential_suppliers_orbis_data %>% filter(!is.na(registration_year))

# getting rid of all the variables that are not needed
matched_potential_suppliers_orbis_data_vars<- matched_potential_suppliers_orbis_data %>% select( -identifier, -score, -nacerev2mainsection, 
                                                                        -nacerev2mainsection, -nacerev2corecode4digits,-country_iso_code, 
                                                                        -nacerev2secondarycodes,  -fax_number, -region_in_country,
                                                                        -type_of_region_in_country,- street_no_building_etc_line_1,-street_no_building_etc_line_1_native, -street_no_building_etc_line_2, 
                                                                        -street_no_building_etc_line_2_native, -street_no_building_etc_line_3, -street_no_building_etc_line_3_native) %>% distinct()




saveRDS(matched_potential_suppliers_orbis_data_vars, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars"))

# I create a new variables for the consolidations codes. Before doing this, I have U1, U2, C1, and C2. I want just the initial letters. 
# Add the difference between consolidated and unconsolidated from the paper 
matched_potential_suppliers_orbis_data_vars$consolidation_l <- substr(matched_potential_suppliers_orbis_data_vars$consolidation_code,1,1)
matched_potential_suppliers_orbis_data_vars_unconsolidated <- matched_potential_suppliers_orbis_data_vars %>% 
  filter(consolidation_l =="U") %>% distinct()

matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_year = min(year),# this gets the first year for which you have orbis data
         registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         tech_level = ifelse(!is.na(tech_level), tech_level, last(na.omit(tech_level))),
         last_year = max(year), # this gets the last year for which you have orbis data
         registration_after_last_orbis = last_year<registration_year)# this says whether the last year for which I have data, is before the first order. 

matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated%>% 
  group_by(bvd_id_number, year) %>% 
  filter(ebitda == max(ebitda))%>% # if multiple ebitda per year, I get the maximum - I need to explain why I do this. 
  select(-consolidation_code, -matched_company_name, -nacerev2primarycodes) %>% 
  ungroup() %>% 
  distinct()


matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>% 
  mutate(year_after_order = last_year - registration_year, # This calculates how many years you have after the registration
         year_before_order = registration_year -first_year) %>%  # This calculates how many years before the registration. 
  ungroup() %>% 
  distinct() 

matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  select(-city, - closing_date, -street_no_building_etc_line_4, -street_no_building_etc_line_4_native, -postcode, 
         -city_native, -telephone_number, -address_type, -date_closing, -closing_date_format) %>% distinct() # I Am getting rid of the variables that I don't need

saveRDS(matched_potential_suppliers_orbis_data_vars_unconsolidated, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars_unconsolidated"))

# Create lookup for incorporation, size, nace and activity 
## this is then used to create the incorporation_nace_size_data

incorporation_size_lookup <- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  select(bvd_id_number) %>% distinct()
split_and_write_csv(incorporation_size_lookup, 725, incorporation_size_nace_lookup_pot_dir)

# directories and files
incorporation_size_nace_lookup_pot_dir <- paste0(data_proc_dir, "potential_incorporation_size_nace_activity_lookup")
incorporation_pot_suppliers_orbis_dir <- paste0(data_raw_dir, "Orbis_size_classification_incorp_pot_suppliers/")
incorporation_pot_suppliers_file <- list.files(incorporation_pot_suppliers_orbis_dir, pattern = "xlsx", full.names = TRUE)
incorporation_pot_nace_size_list <- lapply(incorporation_pot_suppliers_file,read_results_sheet)
incorporation_pot_nace_size_data<- do.call(rbind, incorporation_pot_nace_size_list)
incorporation_pot_nace_size_data<- incorporation_pot_nace_size_data %>% select(-'...1') %>%
  clean_names() %>% 
  rename(bvd_id_number = bv_d_id_number) %>% 
  # Identify and process year-only entries
  mutate(
    date_of_incorporation = as.character(date_of_incorporation),
    incorporation_year = sapply(date_of_incorporation, convert_to_year))


## Add the incorporation year, nace, size and actviity
matched_potential_suppliers_orbis_data_vars_unconsolidated_inc <- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  left_join(incorporation_pot_nace_size_data)

saveRDS(matched_potential_suppliers_orbis_data_vars_unconsolidated_inc, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars_unconsolidated_inc"))


# I then extract the data from the Orbis Platform 

#Create the lookup for patents

patent_bvd_lookup <-matched_potential_suppliers_orbis_data_vars_unconsolidated  %>% 
  select(bvd_id_number) %>% distinct()

split_and_write_csv(patent_bvd_lookup, 100, pot_supplier_patent_lookup_dir)



all_orders_tech_balance_selected_countries_vars <- all_orders_tech_balance_selected_countries %>% 
  select(bvd_id_number, country, order_date, chf_amount, tech_intensity, tech_level, registration_year, order_number, code_2_digits, subroject, subproject_1) %>% 
  distinct() %>% 
  filter(!is.na(order_date)) %>% 
  mutate(
    subroject = replace_na(subroject, "OTHERS"),
    subproject_1 = replace_na(subproject_1, "OTHERS")
  ) %>% 
  group_by(bvd_id_number, order_date) %>% 
  mutate(
    total_chf_amount_year = sum(chf_amount, na.rm = TRUE), # this sums up all the orders by year and bvdid
    max_tech = max(tech_level, na.rm = TRUE), # this peaks the highest tech-level - (1 = high-tech, 0 = low-tech)
    number_orders = n_distinct(order_number), # this is the number of orders per year
    code_2_digits = ifelse(length(code_2_digits) == 0, 0, code_2_digits[which.max(chf_amount)]), # this picks the code for the largest order in a year
    subroject_max = ifelse(all(is.na(subroject)), NA, subroject[which.max(chf_amount)]), # this does the same thing as above for subroject
    subroject_max_1 = ifelse(all(is.na(subproject_1)), NA, subproject_1[which.max(chf_amount)]) # this does the same thing as above for subproject_1
  ) %>%
  distinct() %>% 
  drop_na(total_chf_amount_year) %>% 
  select(-tech_intensity, -chf_amount, -order_number) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(bvd_id_number) %>% # the following code creates the variable by bvd_id_number
  mutate(
    first_order = min(order_date),
    last_order = max(order_date),
    total_orders = sum(number_orders), # creating the first order_date, the last_order, and the total number of orders
    total_orders_amount = sum(total_chf_amount_year), # this creates the amount for all the orders
    first_order_amount = total_chf_amount_year[which.min(order_date)], # this creates the amount for the first order
    first_order_tech = max_tech[which.min(order_date)], # this creates the tech_eve for the first order
    code_2_digits = code_2_digits[which.min(order_date)], # this creates the code_2 digits for the first_order 
    subproject_first_year = subroject_max[which.min(order_date)], # this creates the subproject for the first year/order
    subproject_1_first_year = subroject_max_1[which.min(order_date)], # this creates the subproject for year 1
    registration_first_order = first_order - as.numeric(registration_year), # this creates the registration for the first order
    matching_year = order_date
  ) %>% 
  select(bvd_id_number, order_date, matching_year, registration_year, total_chf_amount_year, max_tech, first_order, last_order, total_orders, total_orders_amount, 
         first_order_amount, first_order_tech, registration_first_order, code_2_digits, subproject_first_year, subproject_1_first_year) %>% 
  distinct() %>% 
  ungroup()
