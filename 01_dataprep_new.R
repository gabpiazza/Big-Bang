# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_v2_selected countries
##
##Purpose of script: Preparing the procurement data for analaysis##
##Author: Gabriele Piazza
##
##Date Created: 2023-06-19
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes: I am now selecting a few countries only  
##   
##
# #1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign", "haven") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)


  ##1.2 Create Functions ----------------------------------------------------
# This is a function to match the bvd_id that I manually retrieved from Orbis with the suppliers 
matching_bvd_id_function <- function(matched_orbis, suppliers_data, country){
  matched_orbis<- matched_orbis %>% 
    rename(supplier_name = company_name)
  suppliers_country_matched<- left_join(suppliers_data, matched_orbis, by = "supplier_name") 
  assign(paste0("suppliers_", country, "_matched"), suppliers_country_matched, envir = .GlobalEnv)
}

matching_orbis_financial <- function(country_suppliers, orbis_country){
  new_dataset <- inner_join(country_suppliers, orbis_country, by = "bvd_id_number") %>%
    mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>%
    filter(!is.na(closing_date_format)) %>%
    select(-research_development_expenses) %>%
    drop_na()
  return(new_dataset)
}


matching_orbis_nace <- function(data, matched_cern_firms){
  data <- data %>%
    rename(bvd_id_number = bvdidnumber)
  new_dataset <- inner_join(matched_cern_firms, data, by = "bvd_id_number") %>%
    select(1:11) # Why am I doing this?
  return(new_dataset)
}

matching_orbis_addresses<- function(data, matched_cern_firms){
  data <- data %>%
    clean_names() %>%
    rename(bvd_id_number = "bv_d_id_number") %>%
    select(bvd_id_number, postcode, city)
  new_dataset <- inner_join(matched_cern_firms, data, by = "bvd_id_number")
  return(new_dataset)
}

# General function to load and process data for a given country
process_country_data <- function(country, country_suppliers, orbis_path, nace_path, address_path){
  orbis_data <- read_dta(orbis_path, encoding = 'latin1')
  matched_suppliers <- matching_orbis_financial(country_suppliers, orbis_data)
  rm(orbis_data)
  
  nace_data <- read_csv(nace_path)
  matched_suppliers <- matching_orbis_nace(nace_data, matched_suppliers)
  rm(nace_data)
  
  address_data <- read_csv(address_path, skip = 1)
  matched_suppliers <- matching_orbis_addresses(address_data, matched_suppliers)
  rm(address_data)
  
  matched_suppliers <- matched_suppliers %>% 
    select(-nacerev2primarycodetextdescripti)
  
  number_matched <- unique(matched_suppliers$bvd_id_number)
  
  return(list(data = matched_suppliers, unique_ids = number_matched))
}

# 2.  Load the data for suppliers and registered companies-------------------------------------------------------
## Setting up the directories for the data
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
matched_orbis_suppliers_dir<- paste0(data_proc_dir, "Matched_orbis_suppliers/")
matched_potential_suppliers_dir <- paste0(data_proc_dir, "Matched_potential_suppliers/")
orbis_financial_dir<- paste0(data_raw_dir,"Orbis_Financial/" )
orbis_Nace_dir <- paste0(data_raw_dir, "ORBIS_NACE_2/")
orbis_addresses_dir <- paste0(data_raw_dir, "ORBIS_ADDRESSES/")
### 2.1 Suppliers -------------------------------------------------
suppliers_2016_file <- "21_10_27_Suppliers_cern_2016_nocontacts.xlsx"
suppliers_2021_file <- "2021-06-29 - CERN Orders 2014-2021_clean.xlsx"
suppliers_2016 <- read_excel(paste0(data_raw_dir, suppliers_2016_file))
suppliers_2016<- clean_names(suppliers_2016)
suppliers_2021 <- read_excel(paste0(data_raw_dir, suppliers_2021_file))
suppliers_2021<- clean_names(suppliers_2021)


### 2.2 Registered firm (potential suppliers) ---------------------------
potential_suppliers_file<- "22_03_08_potential_suppliers_procurement.csv"# there is somethign wrong here as it has only GB
potential_suppliers<- read_csv(paste0(data_raw_dir, potential_suppliers_file))
potential_suppliers<- clean_names(potential_suppliers)


# 3. Data Preparation -----------------------------------------------------



### 3.1 Suppliers ------------------------------------------------------------

# I combine the datasets for both periods

suppliers_2016_selected_variables<- suppliers_2016 %>% select(supplier_name, country, city, vat_number, registration_number, supplier_code)
suppliers_2021_selected_variables<- suppliers_2021 %>% select(supplier_name, country, city, vat_number, registration_number, supplier_code)
all_suppliers_selected_variables<- rbind(suppliers_2016_selected_variables, suppliers_2021_selected_variables)
all_suppliers_selected_variables$vat_number<- as.character(all_suppliers_selected_variables$vat_number)
all_suppliers_selected_variables<- all_suppliers_selected_variables %>% distinct()
##### Selected countries -------------------------------------------------
## The reason I do this is that I then match then manually and I can only do it for a few countries
selected_countries <- c("IT", "GB", "ES", "FR")
suppliers_selected_countries <- all_suppliers_selected_variables %>% filter(country %in% selected_countries)

# Italy
  suppliers_italy<- suppliers_selected_countries %>% filter(country =="IT") %>% distinct()

# France
  suppliers_france <- suppliers_selected_countries %>% filter(country =="FR") %>% distinct()
  
# Spain 
  suppliers_spain <- suppliers_selected_countries %>% filter(country == "ES") %>% distinct()
  
# UK
  suppliers_uk <- suppliers_selected_countries %>% filter(country =="GB") %>% distinct()
  

#####  Write csv -----------------------------------------------------------
 #Italy
  
  write.csv(suppliers_italy, file = paste0(data_proc_dir, "suppliers_italy.csv"), row.names = FALSE)
  
 #France
  suppliers_france_1<- suppliers_france[1:999,] # this is because I cannot load more than 1000 rows on Orbis
  suppliers_france_2<- suppliers_france[1000:nrow(suppliers_france),]
  
  write.csv(suppliers_france_1,paste0(data_proc_dir, "suppliers_france_1.csv"), row.names = FALSE)
  write.csv(suppliers_france_2, paste0(data_proc_dir ,"suppliers_france_2.csv"), row.names = FALSE)
  
#Spain
  write.csv(suppliers_spain, paste0(data_proc_dir, "suppliers_spain.csv"), row.names = FALSE)
  
# UK 
  write.csv(suppliers_uk, paste0(data_proc_dir, "suppliers_uk.csv"), row.names = FALSE)

  

### 3.2 Registered companies (potential suppliers) --------------------------
  potential_suppliers_selected_variables<- clean_names(potential_suppliers)
  potential_suppliers_selected_variables<- potential_suppliers_selected_variables %>% 
    select(suppliername, country)
  
  # Define the list of countries and their respective codes
  countries <- c("IT" = "italy", "FR" = "france", "ES" = "spain", "GB" = "uk")
  
  # Loop through each country and write the CSV files
  for (country_code in names(countries)) {
    country_name <- countries[[country_code]]
    potential_suppliers <- potential_suppliers_selected_variables %>% 
      filter(country == country_code) %>% 
      distinct()
    write.csv(potential_suppliers, paste0(data_proc_dir, "potential_suppliers_", country_name, ".csv"), row.names = FALSE)
  }

# 4. Load the data with bvd id number -------------------------------------


# I have matched the suppliers and potential suppliers manually to Orbis and saved the files in the matched_orbis_suppliers  and potential suppliers folder
# I have done this to retrive the bvd_id _numbers

  # Upload all the files

## 4.1 Suppliers ---------------------------------------------------------

  
files <- list( 
italy = "Export_suppliers_italy.xlsx",
spain=  "Export_suppliers_spain.xlsx",
france_1 =  "Export_suppliers_france_1.xlsx",
france_2 =  "Export_suppliers_france_2.xlsx",
uk = "Export_suppliers_uk.xlsx"
)


# Loop through the files and read each one
for (country in names(files)){
  file_path <- paste0(matched_orbis_suppliers_dir, files[[country]])
  matched_suppliers<- read_excel(file_path)
  matched_suppliers <- matched_suppliers[!is.na(matched_suppliers$`Matched BvD ID`), ]
  matched_suppliers <- matched_suppliers %>%
    rename(bvd_id_number = `Matched BvD ID`)
  matched_suppliers<- clean_names(matched_suppliers)
  
  # Create the variable name
  variable_name <- paste0("matched_orbis_", country)
  
  
  # Assign the data frame to a variable with the constructed name
  assign(variable_name,  matched_suppliers)
}
  

## Matching them to the suppliers data to retreive information on supplier code etc
#Italy

matching_bvd_id_function(matched_orbis_italy, suppliers_italy, "italy")

# Match Spain 
matching_bvd_id_function(matched_orbis_spain, suppliers_spain, "spain")

# Match France
matched_orbis_france <- rbind(matched_orbis_france_1, matched_orbis_france_2)
matching_bvd_id_function(matched_orbis_france, suppliers_france, "france")

#Match UK
matching_bvd_id_function(matched_orbis_uk, suppliers_uk, "uk")


## 4.1 Potential Suppliers ---------------------------------------------------------

files <- list( 
  italy = "Export_potential_suppliers_italy.xlsx",
  spain=  "Export_potential_suppliers_spain.xlsx",
  france =  "Export_potential_suppliers_france.xlsx",
  uk = "Export_potential_suppliers_uk.xlsx"
)

# Loop through the files and read each one
for (country in names(files)){
  file_path <- paste0(matched_potential_suppliers_dir, files[[country]])
  matched_potential_suppliers <- read_excel(file_path)
  
  # Drop rows with NA in bvd_id_number
  matched_potential_suppliers <- matched_potential_suppliers[!is.na(matched_potential_suppliers$`Matched BvD ID`), ]
  matched_potential_suppliers <- matched_potential_suppliers %>%
    rename(bvd_id_number =`Matched BvD ID`)
  matched_potentail_suppliers<- clean_names(matched_potential_suppliers)
  # Create the variable name
  variable_name <- paste0("matched_orbis_potential_", country)
  
  # Assign the data frame to a variable with the constructed name
  assign(variable_name, matched_potential_suppliers)
}


# 5. CERN Matching --------------------------------------------------------
# File paths by country
file_paths <- list(
  france = list(
    financial = paste0(orbis_financial_dir, "Gabriele FR.dta"),
    nace = paste0(orbis_Nace_dir, "NACE FR.csv"),
    address = paste0(orbis_addresses_dir,"ADDRESS_FR.csv")
  ),
  spain = list(
    financial = paste0(orbis_financial_dir,"Gabriele ES.dta"),
    nace =  paste0(orbis_Nace_dir,"NACE ES.csv"),
    address = paste0(orbis_addresses_dir,"ADDRESS_ES.csv")
  ),
  italy = list(
    financial = paste0(orbis_financial_dir, "Gabriele IT.dta"),
    nace = paste0(orbis_Nace_dir,"NACE IT.csv"),
    address =paste0(orbis_addresses_dir ,"ADDRESS_IT.csv")
  ),
  uk = list(
   financial = paste0(orbis_financial_dir,"Gabriele GB.dta"),
    nace =  paste0(orbis_Nace_dir,"NACE GB.csv"),
    address = paste0(orbis_addresses_dir ,"ADDRESS_GB.csv")
  )
)
## 5.1 Suppliers -----------------------------------------------------------

france_data <- process_country_data("france", matched_orbis_france, file_paths$france$financial, file_paths$france$nace, file_paths$france$address)
spain_data <- process_country_data("spain", matched_orbis_spain, file_paths$spain$financial, file_paths$spain$nace, file_paths$spain$address)
italy_data <- process_country_data("italy", matched_orbis_italy, file_paths$italy$financial, file_paths$italy$nace, file_paths$italy$address)
uk_data <- process_country_data("uk", matched_orbis_uk, file_paths$uk$financial, file_paths$uk$nace, file_paths$uk$address)


