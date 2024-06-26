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
  read_excel(file, sheet = "Results",col_types = c(rep("guess", 8), "text", rep("guess", 6)))
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


# getting rid of all the variables that are not needed
matched_potential_suppliers_orbis_data_vars<- matched_potential_suppliers_orbis_data %>% select( -identifier, -score, -nacerev2mainsection, 
                                                                        -nacerev2mainsection, -nacerev2corecode4digits,-country_iso_code, 
                                                                        -nacerev2secondarycodes,  -fax_number, -region_in_country,
                                                                        -type_of_region_in_country,- street_no_building_etc_line_1,-street_no_building_etc_line_1_native, -street_no_building_etc_line_2, 
                                                                        -street_no_building_etc_line_2_native, -street_no_building_etc_line_3, -street_no_building_etc_line_3_native) %>% distinct()




saveRDS(matched_potential_suppliers_orbis_data_vars, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars"))
matched_potential_suppliers_orbis_data_vars<-readRDS(paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars"))


# I create a new variables for the consolidations codes. Before doing this, I have U1, U2, C1, and C2. I want just the initial letters. 
# Add the difference between consolidated and unconsolidated from the paper 
matched_potential_suppliers_orbis_data_vars$consolidation_l <- substr(matched_potential_suppliers_orbis_data_vars$consolidation_code,1,1) 
matched_potential_suppliers_orbis_data_vars<- matched_potential_suppliers_orbis_data_vars %>%
  select(-nacerev2primarycodes, -city_native, -address_type, -consolidation_code, -postcode) %>% 
         distinct()
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
matched_potential_suppliers_orbis_data_vars_case_1 <- matched_potential_suppliers_orbis_data_vars %>%
  group_by(bvd_id_number, year_orbis) %>%
  filter(n() > 1 & length(unique(operating_revenue_turnover_)) > 1) %>%
  arrange(bvd_id_number, year_orbis, desc(consolidation_l)) %>% # to prioritize the C accounts 
  slice(1) %>%
  ungroup()

# Step 2: Case 2 Handling (same financial values)
# Define a function to prioritize duplicates based on majority rule
prioritize_duplicates_majority <- function(df) {
  df %>%
    group_by(bvd_id_number) %>%
    mutate(
      majority_consolidation = ifelse(sum(consolidation_l == 'U') > sum(consolidation_l == 'C'), 'U', 'C'),
      priority = ifelse(consolidation_l == majority_consolidation, 1, 2)
    ) %>%
    arrange(priority) %>%
    slice(1) %>%
    ungroup() %>%
    select(-priority, -majority_consolidation)
}

# Apply the function to handle duplicates where firms report the same financial values under different consolidation codes
matched_potential_suppliers_orbis_data_vars_case_2  <- matched_potential_suppliers_orbis_data_vars %>%
  group_by(bvd_id_number, year_orbis) %>%
  filter(n() > 1 & length(unique(operating_revenue_turnover_)) == 1) %>%
  prioritize_duplicates_majority()

# Step 3: Combine Results
# Combine the cleaned data from Case 1 and Case 2 into a final dataframe
# Keeping only the highest priority rows
matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars %>% 
  anti_join(matched_potential_suppliers_orbis_data_vars_case_1, by = c("bvd_id_number", "year_orbis", "consolidation_l", "operating_revenue_turnover_")) %>%
  anti_join(matched_potential_suppliers_orbis_data_vars_case_2, by = c("bvd_id_number", "year_orbis", "consolidation_l", "operating_revenue_turnover_")) %>%
  bind_rows(matched_potential_suppliers_orbis_data_vars_case_1, matched_potential_suppliers_orbis_data_vars_case_2) %>%
  arrange(bvd_id_number, year_orbis) #49748

matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  group_by(bvd_id_number, year) %>%
  filter(operating_revenue_turnover_ == max(operating_revenue_turnover_)) %>%
  ungroup()

matched_potential_suppliers_orbis_data_vars_unconsolidated <- matched_potential_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number, year) %>%
  arrange(desc(consolidation_l)) %>% # Prioritize 'C' over 'U'
  slice(1) %>%
  ungroup()

check_number<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% group_by(bvd_id_number, year) %>% 
  summarize(number = n())

matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_year = min(year),# this gets the first year for which you have orbis data
         last_year = max(year), # this gets the last year for which you have orbis data
        registration_after_last_orbis = last_year<registration_year) # this says whether the last year for which I have data, is before the first order. 

matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated%>% 
  group_by(bvd_id_number, year) %>% 
  filter(operating_revenue_turnover_ == max(operating_revenue_turnover_))%>% # if multiple ebitda per year, I get the maximum - I need to explain why I do this. 
  select( -matched_company_name) %>% 
  ungroup() %>% 
  distinct()


matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>% 
  mutate(year_after_registration = last_year - registration_year, # This calculates how many years you have after the first order
         year_before_registration = registration_year -first_year) %>%  # This calculates how many years before the order. 
  ungroup() %>% 
  distinct() 

# Here I fill all the rows for the time invariant variables

matched_potential_suppliers_orbis_data_vars_unconsolidated <- matched_potential_suppliers_orbis_data_vars_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         year_after_registration = ifelse(!is.na(year_after_registration), year_after_registration, last(na.omit(year_after_registration))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year))),
         year_before_registration = ifelse(!is.na(year_before_registration), year_before_registration, last(na.omit(year_before_registration)))) %>%
  ungroup() %>%
  fill(registration_year,
        year_after_registration, first_year, last_year, year_before_registration, 
       .direction = "downup") # This fills missing values forward and then backward


number_companies <- unique(matched_potential_suppliers_orbis_data_vars_unconsolidated$bvd_id_number) # this gives 778 companies.

# here I am getting rid of all the variables that I don't need
matched_potential_suppliers_orbis_data_vars_unconsolidated<- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  select(-city, - closing_date, -street_no_building_etc_line_4, -street_no_building_etc_line_4_native, 
         -telephone_number,  -date_closing, -closing_date_format) %>% distinct() # I Am getting rid of the variables that I don't need

saveRDS(matched_potential_suppliers_orbis_data_vars_unconsolidated, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars_unconsolidated"))


## 3.2 Incorporation, Size, NACE, Active data ------------------------------



incoporation_size_nace_dir <- paste0(data_proc_dir, "potential_incorporation_size_nace_activity_lookup")# create the directory where the files is then saved
incorporation_pot_suppliers_orbis_dir <- paste0(data_raw_dir, "Orbis_size_classification_incorp_pot_suppliers/") # create the directory where the matched files are saved 
# Create lookup for incorporation, size, nace and activity 
## this is then used to create the incorporation_nace_size_data matched on orbis 
incorporation_size_lookup <- matched_potential_suppliers_orbis_data_vars_unconsolidated %>% 
  select(bvd_id_number) %>% distinct()
split_and_write_csv(incorporation_size_lookup, 750, incoporation_size_nace_dir)

#  Load the matched data

incorporation_pot_suppliers_file <- list.files(incorporation_pot_suppliers_orbis_dir, pattern = "xlsx", full.names = TRUE) #load the data as list

incorporation_nace_size_list <- lapply(incorporation_pot_suppliers_file,read_results_sheet)# save it into diffent files
incorporation_nace_size_data<- do.call(rbind, incorporation_nace_size_list)# bind the different datasets together
incorporation_nace_size_data<- incorporation_nace_size_data %>% select(-'...1') %>% # rename the identifier
  clean_names() %>% 
  rename(bvd_id_number = bv_d_id_number) %>% 
  # Identify and process year-only entries
  mutate(
    date_of_incorporation = as.character(date_of_incorporation),# create the year variabes
    incorporation_year = sapply(date_of_incorporation, convert_to_year),
    last_avail_year = as.numeric(last_avail_year)) %>% 
  distinct()

## Add the incorporation year, nace, size and activity to the initial dataset
matched_potential_suppliers_orbis_data_vars_unconsolidated_inc <- left_join(matched_potential_suppliers_orbis_data_vars_unconsolidated, incorporation_nace_size_data)
matched_potential_suppliers_orbis_data_vars_unconsolidated_inc<- matched_potential_suppliers_orbis_data_vars_unconsolidated_inc %>% 
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
saveRDS(matched_potential_suppliers_orbis_data_vars_unconsolidated_inc, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars_unconsolidated_inc"))
matched_potential_suppliers_orbis_data_vars_unconsolidated_inc<- readRDS(paste0(data_proc_dir, "matched_potential_suppliers_orbis_data_vars_unconsolidated_inc"))

#Create the lookup for patents

## 3.3.Patent data --------------------------------------------------------

patent_bvd_lookup <- matched_potential_suppliers_orbis_data_vars_unconsolidated_inc %>% 
  select(bvd_id_number) %>% distinct()# create the lookup

split_and_write_csv(patent_bvd_lookup, 150, pot_supplier_patent_lookup_dir) # create the lookups to use to retrieve informaton 
# I can only download data for 32,358 rows at a time for the variables that I need

### I retrieve the data from the Orbis Intellectual property database and then load it here 
patent_matched_pot_suppliers_dir<- paste0(data_raw_dir, "patent_matched_pot_suppliers")
patent_matched_pot_suppliers_list_files <- list.files(patent_matched_pot_suppliers_dir, pattern = "xlsx", full.names = TRUE)
patent_matched_pot_suppliers_list <- lapply(patent_matched_pot_suppliers_list_files, read_results_sheet_patent)

standardize_dates <- function(df) {
  df$`Priority date...5` <- if_else(is.na(df$`Priority date...5`), NA_Date_, as_date(df$`Priority date...5`))
  df$`Publication date` <- if_else(is.na(df$`Publication date`), NA_Date_, as_date(df$`Publication date`))
  df$`Application/filing date` <- if_else(is.na(df$`Application/filing date`), NA_Date_, as_date(df$`Application/filing date`))
  df$`Priority date...15` <- if_else(is.na(df$`Priority date...15`), NA_Date_, as_date(df$`Priority date...15`))
  return(df)
}
patent_matched_pot_suppliers_list <- lapply(patent_matched_pot_suppliers_list, standardize_dates)

patent_matched_pot_suppliers_data <- do.call(rbind, patent_matched_pot_suppliers_list)
patent_matched_pot_suppliers_data<- patent_matched_pot_suppliers_data %>% select(-'...1')


patent_matched_pot_suppliers_data<- patent_matched_pot_suppliers_data %>% clean_names()
# List of columns to forward fill
# I do this because of the way I saved the files. There are multiple rows for the same patents
columns_to_fill <- c("publication_number", "priority_date_5", "current_direct_owner_s_name_s", 
                     "current_direct_owner_s_country_code_s", "current_direct_owner_s_bv_d_id_number_s", 
                     "ipc_code_main_9", "ipc_code_label_main", "publication_date", "patent_office", 
                     "application_filing_date", "application_number", "wipo_code", 
                     "number_of_forward_citations", "number_of_backward_citations", 
                     "number_of_family_members")

patent_matched_pot_suppliers_data<- patent_matched_pot_suppliers_data%>% ## There are many NAs rows because each value for some of the variables are saved in different rows
  fill(all_of(columns_to_fill), .direction = "down")

# Extract the year variable
patent_matched_pot_suppliers_data$application_year<-format(patent_matched_pot_suppliers_data$application_filing_date, "%Y")
patent_matched_pot_suppliers_data$publication_year<-format(patent_matched_pot_suppliers_data$publication_date, "%Y")


# I create here a set of variables based on applications and publications (how different are these?)
# Applications 

patent_matched_pot_suppliers_data_apps <- patent_matched_pot_suppliers_data %>%
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
patent_matched_apps_pot_suppliers_summary<- patent_matched_pot_suppliers_data %>%
  left_join(patent_matched_pot_suppliers_data_apps, by = "application_number")

patent_matched_apps_pot_suppliers_summary <- patent_matched_apps_pot_suppliers_summary %>% 
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

patent_matched_pot_suppliers_data_pubs <- patent_matched_pot_suppliers_data %>%
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

patent_matched_pubs_pot_suppliers_summary<- patent_matched_pot_suppliers_data %>%
  left_join(patent_matched_pot_suppliers_data_pubs, by = "publication_number")

patent_matched_pubs_pot_suppliers_summary <- patent_matched_pubs_pot_suppliers_summary %>% 
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


patent_pot_suppliers_summary<- patent_matched_apps_pot_suppliers_summary %>% 
  left_join(patent_matched_pubs_pot_suppliers_summary)

# Now I want to include only the bvd_ids that are suppliers
patent_pot_suppliers_summary_selected<- patent_pot_suppliers_summary %>% 
  rename(bvd_id_number =applicant_s_bv_d_id_number_s_18,
         year_patent= year) %>% 
  filter(bvd_id_number %in% bvd_id_lookup$bvd_id_number)



patent_pot_suppliers_summary_selected$year<- as.numeric(patent_pot_suppliers_summary_selected$year_patent)
patent_pot_suppliers_summary_selected<- patent_pot_suppliers_summary_selected %>% distinct()
patent_pot_suppliers_summary_selected[is.na(patent_pot_suppliers_summary_selected)] <- 0


number_companies <- unique(patent_pot_suppliers_summary_selected$bvd_id_number) # this is only 217



# Create a panel dataset: I have to do this to calculate the patent stock for application and publications 
panel_data_pot_suppliers_patents<-expand.grid(year = 1900:2022, bvd_id_number = unique(matched_potential_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number))
panel_data_pot_suppliers_patents <- panel_data_pot_suppliers_patents %>% 
  left_join(patent_pot_suppliers_summary_selected)
panel_data_pot_suppliers_patents[is.na(panel_data_pot_suppliers_patents)] <- 0


# Calculate the patent stock for publications and applications 

# This is the depreciation of knowledge taken from the Castelnovo's paper (2023)

rho <- 0.15 # Example decay factor, adjust as needed
# Ensure the dataframe is sorted and initialize stock columns
panel_data_pot_suppliers_patents <- panel_data_pot_suppliers_patents %>%
  mutate_at(vars(-year_patent), ~replace_na(., 0))

panel_data_pot_suppliers_patents <- panel_data_pot_suppliers_patents %>%
  arrange(bvd_id_number, year) %>%
  mutate(application_stock = 0, publication_stock = 0)

# Calculate patent stock using dplyr for efficiency
panel_data_pot_suppliers_patents <- panel_data_pot_suppliers_patents %>%
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
panel_data_pot_suppliers_patents <- panel_data_pot_suppliers_patents %>%
  group_by(bvd_id_number) %>%
  mutate(publication_stock = {
    publication_stock_temp <- 0
    sapply(number_publications, function(x) {
      publication_stock_temp <<- publication_stock_temp * (1 - rho) + x
      publication_stock_temp
    })
  }) %>%
  ungroup()
panel_data_pot_suppliers_patents_1990_2022<- panel_data_pot_suppliers_patents %>% filter(year>1989 & year<2023) %>% distinct()

# Define the processing function
# Function to process the variables list
process_variables <- function(df, variables) {
  # Function to create binary variable and calculate probability
  calculate_probability <- function(df, variable) {
    new_var_name <- gsub("number_", "", paste0("probability_", variable))
    
    df %>%
      mutate(!!sym(variable) := ifelse(!!sym(variable) > 0, 1, 0)) %>%
      group_by(year, bvd_id_number) %>%
      summarise(!!sym(new_var_name) := mean(!!sym(variable)), .groups = "drop")
  }
  
  # Apply the function to all variables and combine the results
  result_list <- lapply(variables, function(var) calculate_probability(df, var))
  result_df <- Reduce(function(x, y) full_join(x, y, by = c("year", "bvd_id_number")), result_list)
  
  # Calculate additional variables for all relevant variables
  for (variable in variables) {
    short_var <- gsub("number_", "", variable)
    result_df <- result_df %>%
      left_join(df %>% dplyr::select(year, bvd_id_number, !!sym(variable)), by = c("year", "bvd_id_number")) %>%
      mutate(
        !!paste0("log_", short_var) := ifelse(!!sym(variable) == 0, 0, log(!!sym(variable) + 1)),  # Ensure log(variable + 1)
        !!paste0("asinh_", short_var) := asinh(!!sym(variable))
      ) %>%
      dplyr::select(-!!sym(variable))  # Drop the original columns if not needed
  }
  
  return(result_df)
}



process_stock_variables <- function(df, stock_vars) {
  result_df <- df %>% dplyr::select(year, bvd_id_number)
  
  # Calculate transformations for stock variables
  for (stock_var in stock_vars) {
    result_df <- result_df %>%
      left_join(df %>% dplyr::select(year, bvd_id_number, !!sym(stock_var)), by = c("year", "bvd_id_number")) %>%
      mutate(
        !!paste0("log_", stock_var) := ifelse(!!sym(stock_var) == 0, 0, log(!!sym(stock_var) + 1)),  # Ensure log(stock_var + 1)
        !!paste0("asinh_", stock_var) := asinh(!!sym(stock_var))
      ) %>%
      dplyr::select(-!!sym(stock_var))  # Drop the original columns if not needed
  }
  
  return(result_df)
}

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

# List of stock variables
stock_vars <- c("application_stock", "publication_stock")

# create probability and log for the two sets of variables
result_variables <- process_variables(panel_data_pot_suppliers_patents_1990_2022,variables)
result_stock_variables <- process_stock_variables(panel_data_pot_suppliers_patents_1990_2022, stock_vars)
# Putting the two datasets together
probability_log_variables<- left_join(result_variables, result_stock_variables)


panel_data_pot_suppliers_patents_1990_2022<- panel_data_pot_suppliers_patents_1990_2022 %>% 
  left_join(probability_log_variables)
saveRDS(panel_data_pot_suppliers_patents_1990_2022, paste0(data_proc_dir, "panel_data_pot_suppliers_patents_1990_2022"))
number_companies<- unique(panel_data_pot_suppliers_patents_1990_2022$bvd_id_number)
panel_data_pot_suppliers_patents_1990_2022<- readRDS(paste0(data_proc_dir, "panel_data_pot_suppliers_patents_1990_2022"))

## 3.4 Merge all the data together -----------------------------------------


### Putting all the data together
year<- seq(1990, 2022) # I am including all the data between 1990 and 2019 but no strong reasons to to this 
year <- rep(1990:2022, each = length(unique(matched_potential_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number))) # this is the number of companies that I have 
bvd_id_number <- unique(matched_potential_suppliers_orbis_data_vars_unconsolidated_inc$bvd_id_number) # This gives you the unique companies
bvd_id_number<-(rep(bvd_id_number, each = 31)) # this repeats times 25, the number of years 
merged_data_potential_suppliers <- expand.grid(year = unique(year), bvd_id_number = unique(bvd_id_number))# this creates a grid
merged_data_potential_suppliers <- merged_data_potential_suppliers[order(merged_data_potential_suppliers$bvd_id_number, merged_data_potential_suppliers$year), ]
merged_data_potential_suppliers$year <- as.numeric(merged_data_potential_suppliers$year)




full_panel_potential_suppliers<- left_join(merged_data_potential_suppliers, matched_potential_suppliers_orbis_data_vars_unconsolidated_inc) %>% 
  left_join(panel_data_pot_suppliers_patents_1990_2022)

### Last cleaning steps 
missing_financial_bvd_ids <- full_panel_potential_suppliers %>%
  filter(year == registration_year) %>%
  filter(is.na(fixed_assets) | is.na(operating_revenue_turnover_) | is.na(ebitda)) %>%
  pull(bvd_id_number)

# always_treated_bvd_ids <- full_panel_potential_suppliers %>%
#   filter(first_year >= registration_year | incorporation_year >= registration_year) %>%
#   pull(bvd_id_number)



#### I am excluding all the years after the last available year for those inactive
## I am also excluding those bvd_id_ that do not have data for the covariates that I use in the analysis at the time of the order

full_panel_potential_suppliers<- full_panel_potential_suppliers %>% 
  filter(!bvd_id_number %in% missing_financial_bvd_ids) %>% 
  group_by(bvd_id_number) %>% 
  filter(!(status_simple == "Inactive" & year > last_avail_year)) %>% 
  ungroup()  # Ungroup after filtering

annual_report_combinations <- full_panel_potential_suppliers%>%
  group_by(bvd_id_number, year) %>%
  filter(any(filing_type == 'Annual report')) %>%
  ungroup() %>%
  select(bvd_id_number, year) %>%
  distinct()

# Filter to keep only 'Annual Report' for combinations with both types
filtered_df_annual <-full_panel_potential_suppliers%>%
  semi_join(annual_report_combinations, by = c("bvd_id_number", "year")) %>%
  filter(filing_type == 'Annual report') %>% distinct()

# Identify combinations where only 'Local Registry Filing' is present
local_registry_only_combinations <- full_panel_potential_suppliers %>%
  group_by(bvd_id_number, year) %>%
  filter(all(filing_type == 'Local registry filing')) %>%
  ungroup() %>%
  select(bvd_id_number, year) %>%
  distinct()

# Filter to keep only 'Local Registry Filing' for combinations with only this type
filtered_df_local <- full_panel_potential_suppliers %>%
  semi_join(local_registry_only_combinations, by = c("bvd_id_number", "year"))

# Combine both filtered dataframes
final_filtered_df <- bind_rows(filtered_df_annual, filtered_df_local)
full_panel_potential_suppliers<- final_filtered_df



## Create the logs for financial variables 
# Function to process the new variables, handling negative values
process_financial_variables <-  function(df, new_vars) {
  for (new_var in new_vars) {
    # Find the minimum value
    min_value <- min(df[[new_var]], na.rm = TRUE)
    # Calculate the shift value if the minimum value is negative
    shift_value <- ifelse(min_value < 0, abs(min_value) + 1, 0)
    
    df <- df %>%
      mutate(!!paste0("log_", new_var) := ifelse(is.na(!!sym(new_var)), NA, log(pmax(!!sym(new_var) + shift_value, 0) + 1)))
  }
  return(df)
}

# List of new variables to process
financial_vars <- c("fixed_assets", 
                    "intangible_fixed_assets", 
                    "tangible_fixed_assets", 
                    "current_assets", 
                    "total_assets", 
                    "number_of_employees", 
                    "operating_revenue_turnover_", 
                    "p_l_before_tax", 
                    "p_l_after_tax", 
                    "research_development_expenses", 
                    "ebitda")



# Use the function to process the new variables
logged_financial_vars <- process_financial_variables(full_panel_potential_suppliers, financial_vars)
# Select only the bvd_id_number, year, and the new log variables
logged_financial_names <- paste0("log_", financial_vars)
selected_columns <- c("bvd_id_number", "year",logged_financial_names)
logged_financial_vars <- logged_financial_vars %>% select(all_of(selected_columns))

## Merging to the full_suppliers_panel
full_panel_potential_suppliers<- full_panel_potential_suppliers %>% left_join(logged_financial_vars) %>% distinct()
full_panel_potential_suppliers<- readRDS(paste0(data_proc_dir, "full_panel_potential_suppliers"))
full_panel_potential_suppliers<- full_panel_potential_suppliers %>% 
  select(-company_name) %>% distinct()

full_panel_potential_suppliers <- full_panel_potential_suppliers %>%
  group_by(bvd_id_number, year) %>%
  arrange(desc(filing_type == 'Annual Report'), desc(operating_revenue_turnover_)) %>% # Prioritize Annual Report and then by highest operating revenue
  slice(1) %>%
  ungroup()


saveRDS(full_panel_potential_suppliers, paste0(data_proc_dir, "full_panel_potential_suppliers"))





