#' ---
#' title: 01_dataprep_B
#' author: Gabriele Piazza
#' date: 2024-06-01
#' Description: This script creates the suppliers dataset that includes information from Orbis
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
# Function to read the "results" sheet from an Excel file, used for the orbis data
read_results_sheet <- function(file) {
  read_excel(file, sheet = "Results",col_types = c(rep("guess", 8), "text", rep("guess", 6)))
}

read_results_sheet_patent <- function(file) {
  read_excel(file, sheet = "Results")
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

## file names for CERN suppliers and potential
matched_suppliers_orbis_file <- "matched_suppliers_orbis_data"# file for matched suppliers
#matched_potential_suppliers_orbis_file <- "matched_potential_suppliers_orbis_data" #file for matched potential suppliers
all_orders_tech_balance_file <- "all_orders_tech_balance" #all orders with with tech and balance matched
potential_suppliers_registration_file <- "22_10_31_potential_suppliers.csv"


supplier_patent_lookup_dir <- paste0(data_proc_dir, "suppliers_patent_lookup")
incoporation_size_nace_dir <- paste0(data_proc_dir, "Incorporation_size_nace_activity_lookup")
incorporation_suppliers_orbis_dir <- paste0(data_raw_dir, "Orbis_size_classification_incorp_suppliers/")
incorporation_suppliers_file <- list.files(incorporation_suppliers_orbis_dir, pattern = "xlsx", full.names = TRUE)
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
#matched_potential_suppliers_orbis_data<- readRDS(paste0(data_proc_dir, matched_potential_suppliers_orbis_file))

incorporation_nace_size_list <- lapply(incorporation_suppliers_file,read_results_sheet)
incorporation_nace_size_data<- do.call(rbind, incorporation_nace_size_list)
incorporation_nace_size_data<- incorporation_nace_size_data %>% select(-'...1') %>%
  clean_names() %>% 
  rename(bvd_id_number = bv_d_id_number) %>% 
  # Identify and process year-only entries
  mutate(
    date_of_incorporation = as.character(date_of_incorporation),
    incorporation_year = sapply(date_of_incorporation, convert_to_year))
# incorporation_date<-  read_excel(paste0(data_raw_dir,"Export 11_07_2023 13_43_suppliers_incorporation_date.xlsx"),  sheet = "Results", 
#                                 col_types = c("text","text", "date", "text", "text", "numeric"))
# 
# incorporation_date<- incorporation_date %>% select(-'Column1',-"Company name Latin alphabet",-'Branch indicator') %>% clean_names() %>% 
#   rename(bvd_id_number = bv_d_id_number) %>% 
#   mutate(incorporation_year = year(date_of_incorporation)) %>% 
#   select(-date_of_incorporation)



# 3. Data Manipulation  ---------------------------------------------------


## 3.1 Manipulation of the matched orbis dataset for suppliers and all orders -------------------------------------------------------

#Make changes to the year variable
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
  distinct() %>% 
  ungroup() %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_order= min(order_date),last_order = max(order_date), total_orders = sum(number_orders),
         total_orders_amount = sum(total_chf_amount_year),
         first_order_amount = total_chf_amount_year[which.min(order_date)],
         first_order_tech = max_tech[which.min(order_date)],
         code_2_digits = code_2_digits[which.min(order_date)],
         registration_first_order = first_order -as.numeric(registration_year),
         subproject_first_year = subroject_max[which.min(order_date)], 
         subproject_1_first_year = subroject_max_1[which.min(order_date)],
         registration_first_order = first_order -as.numeric(registration_year),
         matching_year = order_date)%>% 
  select(bvd_id_number, order_date, matching_year, registration_year, total_chf_amount_year, max_tech, first_order, last_order, total_orders, total_orders_amount, 
         first_order_amount, first_order_tech, registration_first_order, code_2_digits, subproject_first_year, subproject_1_first_year)%>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(matching_year = order_date)


 
## Merge orders and orbis data
matched_suppliers_orbis_data$matching_year <- matched_suppliers_orbis_data$year# this creates the matching year
matched_suppliers_orbis_data<- matched_suppliers_orbis_data %>% 
  left_join(all_orders_tech_balance_selected_countries_vars) 






# getting rid of all the variables that are not needed
matched_suppliers_orbis_data<- matched_suppliers_orbis_data %>% select( -identifier, -score, -nacerev2mainsection, 
                                                                         -nacerev2mainsection, -nacerev2corecode4digits,-country_iso_code, 
                                                                        -nacerev2secondarycodes,  -fax_number, -region_in_country,
                                                                        -type_of_region_in_country,- street_no_building_etc_line_1,-street_no_building_etc_line_1_native, -street_no_building_etc_line_2, 
                                                                        -street_no_building_etc_line_2_native, -street_no_building_etc_line_3, -street_no_building_etc_line_3_native) %>% distinct()




matched_suppliers_orbis_data_vars<- matched_suppliers_orbis_data %>% # As there are NAs for all the years, I fill all the missing years but I am not successful (Please revisit this) 
  group_by(bvd_id_number) %>% fill(nacerev2primarycodes, registration_year, 
                                   total_chf_amount_year, max_tech, first_order, last_order, total_orders, total_orders_amount, 
                                   first_order_amount, first_order_tech, registration_first_order,  subproject_first_year, subproject_1_first_year) %>% 
  ungroup() %>% 
  distinct()




saveRDS(matched_suppliers_orbis_data_vars, paste0(data_proc_dir, "matched_suppliers_orbis_data_vars"))

# I create a new variables for the consolidations codes. Before doing this, I have U1, U2, C1, and C2 and LF. I want just the initial letters. 
matched_suppliers_orbis_data_vars$consolidation_l <- substr(matched_suppliers_orbis_data_vars$consolidation_code,1,1)
#   
#   When firms report different financial values under different consolidation codes, priority is given to those with consolidated accounts.
# Filters for Case 2 Duplicates:
#   
#   For firms continuously reporting unconsolidated accounts, priority is given to unconsolidated accounts.
# For firms continuously reporting consolidated accounts, priority is given to consolidated accounts.
# Inconsistent Reporting for Case 2 Duplicates:
#   
# #   For firms reporting both consolidated and unconsolidated accounts inconsistently but with the same sales value, 
# the sales volume over time is checked and verified. Priority is given to the consolidated code, and the time series is reclassified as consolidated.



# Add the difference between consolidated and unconsolidated from the paper 
# Priority for Case 1 Duplicates:
# Step 1: Case 1 Handling (different financial values)
# Filter duplicates where firms report different financial values under different consolidation codes
# Priority is given to consolidated accounts ('C')
matched_suppliers_orbis_data_vars_case_1 <- matched_suppliers_orbis_data_vars %>%
  group_by(bvd_id_number, year_orbis) %>%
  filter(n() > 1 & length(unique(operating_revenue_turnover_)) > 1) %>%
  arrange(bvd_id_number, year_orbis, desc(consolidation_l)) %>% # to prioritize the C accounts 
  slice(1) %>%
  ungroup()

# Step 2: Case 2 Handling (same financial values)
# Define a function to prioritize duplicates based on consolidation code
# Function to prioritize based on rules
prioritize_duplicates <- function(df) {
  df %>%
    group_by(bvd_id_number) %>%
    mutate(
      priority = case_when(
        all(consolidation_l == 'U') ~ ifelse(consolidation_l == 'U', 1, 2),
        all(consolidation_l == 'C') ~ ifelse(consolidation_l == 'C', 1, 2),
        TRUE ~ ifelse(consolidation_l == 'C', 1, 2)
      )
    ) %>%
    arrange(priority) %>%
    slice(1) %>%
    ungroup() %>%
    select(-priority)
}

# Apply the function to handle duplicates where firms report the same financial values under different consolidation codes
matched_suppliers_orbis_data_vars_case_2  <- matched_suppliers_orbis_data_vars %>%
  group_by(bvd_id_number, year_orbis) %>%
  filter(n() > 1 & length(unique(operating_revenue_turnover_)) == 1) %>%
  prioritize_duplicates()

# Step 3: Combine Results
# Combine the cleaned data from Case 1 and Case 2 into a final dataframe
# Keeping only the highest priority rows
matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars %>% 
  anti_join(matched_suppliers_orbis_data_vars_case_1, by = c("bvd_id_number", "year_orbis", "consolidation_l", "operating_revenue_turnover_")) %>%
  anti_join(matched_suppliers_orbis_data_vars_case_2, by = c("bvd_id_number", "year_orbis", "consolidation_l", "operating_revenue_turnover_")) %>%
  bind_rows(matched_suppliers_orbis_data_vars_case_1, matched_suppliers_orbis_data_vars_case_2) %>%
  arrange(bvd_id_number, year_orbis) #49748



matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_year = min(year),# this gets the first year for which you have orbis data
         last_year = max(year), # this gets the last year for which you have orbis data
         order_after_last_orbis = last_year<first_order) # this says whether the last year for which I have data, is before the first order. 

matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated%>% 
  group_by(bvd_id_number, year) %>% 
  filter(operating_revenue_turnover_ == max(operating_revenue_turnover_))%>% # if multiple ebitda per year, I get the maximum - I need to explain why I do this. 
  select(-consolidation_code, -matched_company_name, -nacerev2primarycodes) %>% 
  ungroup() %>% 
  distinct()


matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>% 
  mutate(year_after_order = last_year - first_order, # This calculates how many years you have after the first order
         year_before_order = first_order -first_year) %>%  # This calculates how many years before the order. 
  ungroup() %>% 
  distinct() 

# Here I fill all the rows for the time invariant variables

matched_suppliers_orbis_data_vars_unconsolidated <- matched_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(first_order = ifelse(!is.na(first_order), first_order, last(na.omit(first_order))),
         first_order_amount = ifelse(!is.na(first_order_amount), first_order_amount, last(na.omit(first_order_amount))),
         first_order_tech = ifelse(!is.na(first_order_tech), first_order_tech, last(na.omit(first_order_tech))),
         total_orders_amount = ifelse(!is.na(total_orders_amount), total_orders_amount, last(na.omit(total_orders_amount))),
         total_orders = ifelse(!is.na(total_orders), total_orders, last(na.omit(total_orders))),
         last_order = ifelse(!is.na(last_order), last_order, last(na.omit(last_order))),
         subproject_first_year = ifelse(!is.na(subproject_first_year), subproject_first_year, last(na.omit(subproject_first_year))), 
         subproject_1_first_year = ifelse(!is.na(subproject_1_first_year), subproject_1_first_year, last(na.omit(subproject_1_first_year))),
         registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         registration_first_order = ifelse(!is.na(registration_first_order), registration_first_order, last(na.omit(registration_first_order))),
         year_after_order = ifelse(!is.na(year_after_order), year_after_order, last(na.omit(year_after_order))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year))),
         year_before_order = ifelse(!is.na(year_before_order), year_before_order, last(na.omit(year_before_order)))) %>%
  ungroup() %>%
  fill(first_order, first_order_amount, first_order_tech, total_orders_amount, total_orders,
       last_order, subproject_first_year, subproject_1_first_year, registration_year,
       registration_first_order, year_after_order, first_year, last_year, year_before_order, 
       .direction = "downup") # This fills missing values forward and then backward



number_companies <- unique(matched_suppliers_orbis_data_vars_unconsolidated$bvd_id_number) # this gives 1592 companies. 
# I now create some variables for years before and after order - I want to see whether there is data  pre and post treatment 
matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>% # this might be a repetition
  group_by(bvd_id_number) %>% 
  mutate(year_after_order = last_year - first_order,
         year_before_order = first_order -first_year) %>% 
  ungroup() %>% 
  distinct()

# here I am getting rid of all the variables that I don't need
matched_suppliers_orbis_data_vars_unconsolidated<- matched_suppliers_orbis_data_vars_unconsolidated %>% 
  select(-city, - closing_date, -street_no_building_etc_line_4, -street_no_building_etc_line_4_native, -postcode, 
         -city_native, -telephone_number, -address_type, -date_closing, -closing_date_format) %>% distinct() # I Am getting rid of the variables that I don't need

saveRDS(matched_suppliers_orbis_data_vars_unconsolidated, paste0(data_proc_dir, "matched_suppliers_orbis_data_vars_unconsolidated"))

## 3.2 Incorporation, Size, NACE, Active data ------------------------------



incoporation_size_nace_dir <- paste0(data_proc_dir, "supplier_incorporation_size_nace_activity_lookup")# create the directory where the files is then saved
incorporation_suppliers_orbis_dir <- paste0(data_raw_dir, "Orbis_size_classification_incorp_suppliers/") # create the directory where the matched files are saved 
# Create lookup for incorporation, size, nace and activity 
## this is then used to create the incorporation_nace_size_data matched on orbis 
incorporation_size_lookup <- matched_suppliers_orbis_data_vars_unconsolidated %>% 
  select(bvd_id_number) %>% distinct()
split_and_write_csv(incorporation_size_lookup, 750, incoporation_size_nace_dir)

#  Load the matched data

incorporation_suppliers_file <- list.files(incorporation_suppliers_orbis_dir, pattern = "xlsx", full.names = TRUE) #load the data as list

incorporation_nace_size_list <- lapply(incorporation_suppliers_file,read_results_sheet)# save it into diffent files
incorporation_nace_size_data<- do.call(rbind, incorporation_nace_size_list)# bind the different datasets together
incorporation_nace_size_data<- incorporation_nace_size_data %>% select(-'...1') %>% # rename the identifier
  clean_names() %>% 
  rename(bvd_id_number = bv_d_id_number) %>% 
  # Identify and process year-only entries
  mutate(
    date_of_incorporation = as.character(date_of_incorporation),# create the year variabes
    incorporation_year = sapply(date_of_incorporation, convert_to_year),
    last_avail_year = as.numeric(last_avail_year))

## Add the incorporation year, nace, size and activity to the initial dataset
matched_suppliers_orbis_data_vars_unconsolidated_inc <- left_join(matched_suppliers_orbis_data_vars_unconsolidated, incorporation_nace_size_data)
matched_suppliers_orbis_data_vars_unconsolidated_inc<- matched_suppliers_orbis_data_vars_unconsolidated_inc %>% 
  select(-date_of_incorporation, -original_currency, -exchange_rate_from_original_curr) %>% 
  mutate(status_simple = case_when(
  status %in% c("Active", 
                "Active (insolvency proceedings)", 
                "Active (reorganization)", 
                "Active (rescue plan)", 
                "Active (dormant)") ~ "Active",
  status %in% c("Dissolved", 
                "Dissolved (merger or take-over)", 
                "Inactive (no precision)", 
                "Dissolved (bankruptcy)", 
                "Status unknown", 
                "Bankruptcy", 
                "In liquidation", 
                "Dissolved (liquidation)") ~ "Inactive",
  TRUE ~ "Unknown" # Default value for any other status not listed
))
saveRDS(matched_suppliers_orbis_data_vars_unconsolidated_inc, paste0(data_proc_dir, "matched_suppliers_orbis_data_vars_unconsolidated_inc"))


#Create the lookup for patents

## 3.3.Patent data --------------------------------------------------------

patent_bvd_lookup <- matched_suppliers_orbis_data_vars_unconsolidated_inc %>% 
  select(bvd_id_number) %>% distinct()# create the lookup

split_and_write_csv(patent_bvd_lookup, 50, supplier_patent_lookup_dir) # create the lookups to use to retrieve informaton 
# I can only download data for 32,358 rows at a time for the variables that I need

### I retrieve the data from the Orbis Intellectual property database and then load it here 
patent_matched_suppliers_dir<- paste0(data_raw_dir, "patent_matched_suppliers")
patent_matched_suppliers_list_files <- list.files(patent_matched_suppliers_dir, pattern = "xlsx", full.names = TRUE)
patent_matched_suppliers_list <- lapply(patent_matched_suppliers_list_files, read_results_sheet_patent)
patent_matched_suppliers_data <- do.call(rbind, patent_matched_suppliers_list)
patent_matched_suppliers_data<- patent_matched_suppliers_data %>% select(-'...1')

# There is an issue here and I have to repeat the matching as I might have mised out a number of companies. Redo 487 to 491 and redownload and match 

patent_matched_suppliers_data<- patent_matched_suppliers_data %>% clean_names()
# List of columns to forward fill
# I do this because of the way I saved the files. There are multiple rows for the same patents
columns_to_fill <- c("publication_number", "priority_date_5", "current_direct_owner_s_name_s", 
                     "current_direct_owner_s_country_code_s", "current_direct_owner_s_bv_d_id_number_s", 
                     "ipc_code_main_9", "ipc_code_label_main", "publication_date", "patent_office", 
                     "application_filing_date", "application_number", "wipo_code", 
                     "number_of_forward_citations", "number_of_backward_citations", 
                     "number_of_family_members")

patent_matched_suppliers_data<- patent_matched_suppliers_data%>% ## There are many NAs rows because each value for some of the variables are saved in different rows
  fill(all_of(columns_to_fill), .direction = "down")


# Extract the year variable
patent_matched_suppliers_data$application_year<-format(patent_matched_suppliers_data$application_filing_date, "%Y")
patent_matched_suppliers_data$publication_year<-format(patent_matched_suppliers_data$publication_date, "%Y")

# I create here a set of variables based on applications and publications (how different are these?)
# Applications 

patent_matched_suppliers_data_apps <- patent_matched_suppliers_data %>%
  group_by(application_number) %>%
  mutate(number_of_forward_citations = replace_na(number_of_forward_citations, 0),
         weighted_patent = (1 + number_of_forward_citations)) %>%
  summarize(number_inventors = n_distinct(inventor_s_name_s),
            number_bvd_ids = n_distinct(applicant_s_bv_d_id_number_s_18),
            number_patent_offices = n_distinct(patent_office),
            weighted_patent = sum(weighted_patent)) %>%
  ungroup()%>%
  mutate(collaborations = case_when(number_bvd_ids > 1 ~ 1, 
                                    TRUE ~ 0),
         multiple_inventors =case_when(number_inventors > 1 ~ 1, 
                                       TRUE ~ 0),
         multiple_offices = case_when(number_patent_offices>1~1, 
                                      TRUE ~0)) %>%
  distinct()
patent_matched_apps_suppliers_summary<- patent_matched_suppliers_data %>%
  left_join(patent_matched_suppliers_data_apps, by = "application_number")

patent_matched_apps_suppliers_summary <- patent_matched_apps_suppliers_summary %>% 
  select(applicant_s_bv_d_id_number_s_18, application_year, application_number,
         wipo_code, weighted_patent, multiple_inventors, collaborations, multiple_offices) %>% 
  distinct() %>% 
  filter(!is.na(applicant_s_bv_d_id_number_s_18))%>% 
  group_by(applicant_s_bv_d_id_number_s_18, application_year) %>% 
  summarize(number_applications = n_distinct(application_number),
            number_WIPO_code_apps = n_distinct(wipo_code),
            number_weighted_patent_apps = sum(weighted_patent),
            number_multiple_inventors_apps = sum(multiple_inventors),
            number_collaborations_apps = sum(collaborations),
            number_multiple_patent_offices_apps = sum(multiple_offices), 
            )  %>% ungroup() %>% 
  rename(year= application_year)

# Publications

patent_matched_suppliers_data_pubs <- patent_matched_suppliers_data %>%
  group_by(publication_number) %>%
  mutate(number_of_forward_citations = replace_na(number_of_forward_citations, 0),
         weighted_patent = (1 + number_of_forward_citations)) %>% # this is from the 1990's paper 
  summarize(number_inventors = n_distinct(inventor_s_name_s),
            number_bvd_ids = n_distinct(applicant_s_bv_d_id_number_s_18),
            number_patent_offices = n_distinct(patent_office),
            weighted_patent = sum(weighted_patent)) %>%
  ungroup()%>%
  mutate(collaborations = case_when(number_bvd_ids > 1 ~ 1, 
                                    TRUE ~ 0),
         multiple_inventors =case_when(number_inventors > 1 ~ 1, 
                                       TRUE ~ 0),
         multiple_offices = case_when(number_patent_offices>1~1, 
                                      TRUE ~0)) %>%
  distinct()

patent_matched_pubs_suppliers_summary<- patent_matched_suppliers_data %>%
  left_join(patent_matched_suppliers_data_pubs, by = "publication_number")

patent_matched_pubs_suppliers_summary <- patent_matched_pubs_suppliers_summary %>% 
  select(applicant_s_bv_d_id_number_s_18, publication_year, publication_number,
         wipo_code, weighted_patent, multiple_inventors, collaborations, multiple_offices) %>% 
  distinct() %>% 
  filter(!is.na(applicant_s_bv_d_id_number_s_18))%>% 
  group_by(applicant_s_bv_d_id_number_s_18, publication_year) %>% 
  summarize(number_publications = n_distinct(publication_number),
            number_WIPO_code_pubs = n_distinct(wipo_code),
            number_weighted_patent_pubs = sum(weighted_patent),
            number_multiple_inventors_pubs = sum(multiple_inventors),
            number_collaborations_pubs = sum(collaborations),
            number_multiple_patent_offices_pubs = sum(multiple_offices), 
  )  %>% ungroup() %>% 
  rename(year = publication_year)


patent_suppliers_summary<- patent_matched_apps_suppliers_summary %>% 
  left_join(patent_matched_pubs_suppliers_summary)

# Now I want to include only the bvd_ids that are suppliers
patent_suppliers_summary_selected<- patent_suppliers_summary %>% 
  rename(bvd_id_number = applicant_s_bv_d_id_number_s_18,
         year_patent= year) %>% 
  filter(bvd_id_number %in% bvd_id_lookup$bvd_id_number)

patent_suppliers_summary_selected$year<- as.numeric(patent_suppliers_summary_selected$year_patent)
patent_suppliers_summary_selected<- patent_suppliers_summary_selected %>% distinct()
patent_suppliers_summary_selected[is.na(patent_suppliers_summary_selected)] <- 0
#I create the probability of patenting/collaborating in a given year by bvd_id
# List of variables to process
variables <- c("number_applications", 
               "number_WIPO_code_apps", 
               "number_weighted_patent_apps", 
               "number_multiple_inventors_apps", 
               "number_collaborations_apps", 
               "number_multiple_patent_offices_apps", 
               "number_publications", 
               "number_WIPO_code_pubs", 
               "number_weighted_patent_pubs", 
               "number_multiple_inventors_pubs", 
               "number_collaborations_pubs", 
               "number_multiple_patent_offices_pubs")

# Define the processing function
process_data <- function(df, variables) {
  # Function to create binary variable and calculate probability
  calculate_probability <- function(df, variable) {
    new_var_name <- gsub("number_", "", paste0("probability_", variable))
    
    df %>%
      mutate(!!sym(variable) := ifelse(!!sym(variable) > 0, 1, 0)) %>%
      group_by(year, bvd_id_number) %>%
      summarise(!!sym(new_var_name) := mean(!!sym(variable))) %>%
      ungroup()
  }
  
  # Apply the function to all variables and combine the results
  result_list <- lapply(variables, function(var) calculate_probability(df, var))
  result_df <- Reduce(function(x, y) full_join(x, y, by = c("year", "bvd_id_number")), result_list)
  
  # Calculate additional variables for all relevant variables
  for (variable in variables) {
    short_var <- gsub("number_", "", variable)
    result_df <- result_df %>%
      left_join(df %>% dplyr::select(year, bvd_id_number, !!sym(variable)), by = c("year", "bvd_id_number")) %>%
      mutate(!!paste0("log_", short_var) := log(!!sym(variable) + 1),
             !!paste0("asinh_", short_var) := asinh(!!sym(variable))) %>%
      dplyr::select(-!!sym(variable))  # Drop the original columns if not needed
  }
  
  return(result_df)
}

probability_log_patents_panel<- process_data(panel_data_suppliers_patents, variables)
patent_suppliers_summary_selected<- patent_suppliers_summary_selected %>% left_join(probability_log_patents_panel) %>% 
  select(-probability_publication_stock, -probability_application_stock)
# Display the result


number_companies <- unique(patent_suppliers_summary_selected$bvd_id_number)

# Create a panel dataset: I have to do this to calculate the patent stock for application and publications 
panel_data_suppliers_patents<-expand.grid(year = 1900:2022, bvd_id_number = unique(matched_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number))
panel_data_suppliers_patents <- panel_data_suppliers_patents %>% 
  left_join(patent_suppliers_summary_selected)
  
# Calculate the patent stock for publications and applications 

# This is the depreciation of knowledge taken from the Castelnovo's paper (2023)

rho <- 0.15 # Example decay factor, adjust as needed
# Ensure the dataframe is sorted and initialize stock columns
panel_data_suppliers_patents <- panel_data_suppliers_patents %>%
  mutate_at(vars(-year_patent), ~replace_na(., 0))

panel_data_suppliers_patents <- panel_data_suppliers_patents %>%
  arrange(bvd_id_number, year) %>%
  mutate(application_stock = 0, publication_stock = 0)

# Calculate patent stock using dplyr for efficiency
panel_data_suppliers_patents <- panel_data_suppliers_patents %>%
  group_by(bvd_id_number) %>%
  mutate(application_stock = {
    application_stock_temp <- 0
    sapply(number_applications, function(x) {
      application_stock_temp <<- application_stock_temp * (1 - rho) + x
      application_stock_temp
    })
  }) %>%
  ungroup()

# Calculate publication stock using dplyr for efficiency
panel_data_suppliers_patents <- panel_data_suppliers_patents %>%
  group_by(bvd_id_number) %>%
  mutate(publication_stock = {
    publication_stock_temp <- 0
    sapply(number_publications, function(x) {
      publication_stock_temp <<- publication_stock_temp * (1 - rho) + x
      publication_stock_temp
    })
  }) %>%
  ungroup()

head(panel_data_suppliers_patents
     )

panel_data_suppliers_patents_1990_2022<- panel_data_suppliers_patents %>% filter(year>1989 & year<2023) %>% distinct()
  

saveRDS(panel_data_suppliers_patents_1990_2022, paste0(data_proc_dir, "panel_data_suppliers_patents_1990_2022"))
number_companies<- unique(panel_data_suppliers_patents_1990_2022$bvd_id_number)


## 3.4 Merge all the data together -----------------------------------------


### Putting all the data together
year<- seq(1990, 2022) # I am including all the data between 1990 and 2019 but no strong reasons to to this 
year <- rep(1990:2022, each = length(unique(matched_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number))) # this is the number of companies that I have 
bvd_id_number <- unique(matched_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number) # This gives you the unique companies
bvd_id_number<-(rep(bvd_id_number, each = 31)) # this repeats times 25, the number of years 
merged_data_suppliers <- expand.grid(year = unique(year), bvd_id_number = unique(bvd_id_number))# this creates a grid
merged_data_suppliers <- merged_data_suppliers[order(merged_data_suppliers$bvd_id_number, merged_data_suppliers$year), ]
merged_data_suppliers$year <- as.numeric(merged_data_suppliers$year)




full_panel_suppliers<- left_join(merged_data_suppliers, matched_suppliers_orbis_data_vars_unconsolidated_inc) %>% 
  left_join(panel_data_suppliers_patents_1990_2022)

### Last cleaning steps 
missing_financial_bvd_ids <- full_panel_suppliers %>%
  filter(year == first_order) %>%
  filter(is.na(fixed_assets) | is.na(operating_revenue_turnover_) | is.na(ebitda)) %>%
  pull(bvd_id_number)

always_treated_bvd_ids <- full_panel_suppliers %>%
  filter(first_year >= first_order | incorporation_year >= first_order) %>%
  pull(bvd_id_number)



#### I am excluding all the years after the last available year for those inactive
full_panel_suppliers<- full_panel_suppliers %>% 
  filter(!bvd_id_number %in% c(missing_financial_bvd_ids, always_treated_bvd_ids)) %>% 
  group_by(bvd_id_number) %>% 
  filter(!(status_simple == "Inactive" & year > last_avail_year)) %>% 
  ungroup()  # Ungroup after filtering


## I am also excluding those bvd_id_ that do not have data for the covariates that I use in the analysis at the time of the order



# Extract the year variable
p
names(test)
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
year <- rep(1990:2020, each = length(unique(matched_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number))) # this is the number of companies that I have 
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





