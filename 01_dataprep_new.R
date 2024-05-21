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
need <- c("tidyverse","stargazer", "janitor", "here","readxl") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)


  ##1.2 Create Functions ----------------------------------------------------
# This is a function to match the bvd_id that I manually retrieved from Orbis with the suppliers 
matching_bvd_id_function <- function(matched_orbis, suppliers_data, country){
  matched_orbis<- matched_orbis %>% 
    rename(supplier_name = "Company name")
  suppliers_country_matched<- left_join(suppliers_data, matched_orbis, by = "supplier_name") %>% 
    drop_na(`Matched BvD ID`) 
  assign(paste0("suppliers_", country, "_matched"), suppliers_country_matched, envir = .GlobalEnv)
}


# 2.  Load the data for suppliers and registered companies-------------------------------------------------------
## Setting up the directories for the data
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
matched_orbis_suppliers_dir<- paste0(data_proc_dir, "Matched_orbis_suppliers/")
matched_potential_suppliers_dir <- paste0(data_proc_dir, "Matched_potential_suppliers/")
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





