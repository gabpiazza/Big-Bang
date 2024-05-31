# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep_v2_selected countries
##
##Purpose of script: Preparing the procurement data: load the procurement data, match it to orbis and save the files
##
##Author: Gabriele Piazza
##
##Date Created: 2024-05-19
##
##Copyright (c) Gabriele Piazza, 2024
##Email: g.piazza@lse.ac.uk 
##

##
## Notes: What I do in this script is the following: 
##   Step 1. I combine the orders data (up to 2016 and post 2016), I match them to the tech lookup
##       and member states balance - this were kindly given to me by CSIL
##   Step 2. I then filter and create a new data set that include only France, Italy, Spain and UK. 
##      I do the same for the potential suppliers.
##   Step 3. I save the files for each country, I do the same for potential suppliers. 
##   Step 4. I then use these files to get the bvd id number from Orbis. 
##.  Step 5. I finally use these bvd id dumber to get the Orbis Financial Data, NACE and Address - these were given to me by the libbrary
##      Please note that there were some issues with loading the NACE data
##    Final product: at the end of the script, you have two dataset with the orbis financial data
##     (i) for the suppliers - matched_suppliers_orbis_data -  and (ii) registered suppliers -  this is called matched_potential_suppliers_orbis_data.
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
    select(research_development_expenses) %>%
  return(new_dataset)
}


matching_orbis_nace <- function(data, matched_cern_firms){
    new_dataset <- inner_join(matched_cern_firms, data, by = "bvd_id_number",relationship = "many-to-many") 
  return(new_dataset)
}

matching_orbis_addresses<- function(data, matched_cern_firms){
  data <- data %>%
    clean_names() %>%
    rename(bvd_id_number = "bv_d_id_number") %>% 
    rename(bvd_id_number = "bvd_id_number")%>%
    select(bvd_id_number, postcode, city)
  new_dataset <- inner_join(matched_cern_firms, data, by = "bvd_id_number")
  return(new_dataset)
}

# General function to load and process data for a given country
#process_country_data <- function(country, country_suppliers, orbis_path, nace_path, address_path){
  process_country_data <- function(country, country_suppliers, orbis_path, nace_path, address_path){
  orbis_data <- read_dta(orbis_path, encoding = 'latin1') %>% 
    clean_names()
  matched_suppliers <- matching_orbis_financial(country_suppliers, orbis_data)
  rm(orbis_data)
  
  nace_data <- read_csv(nace_path) %>%
    clean_names()
  matched_suppliers <- matching_orbis_nace(matched_suppliers,nace_data)
  rm(nace_data)
  
  #address_data <- read_csv(address_path, skip = 1) %>% 
 #   #clean_names()
  #matched_suppliers <- matching_orbis_addresses( matched_suppliers,address_data)
  #rm(address_data)
  
  
  return(data = matched_suppliers)
}
  # Function to process and match Orbis data for a given country
  process_orbis_data <- function(country, file_paths, matched_orbis) {
    # Load the financial data
    financial_data <- read_dta(file_paths[[country]]$financial)
    
    # Perform the joins
    suppliers_orbis <- inner_join(matched_orbis[[country]], financial_data)
    rm(financial_data)
    gc()
    
    # Load and clean the NACE data
    nace_data <- read_csv(file_paths[[country]]$nace) %>%
      rename(bvd_id_number = bvdidnumber)
    suppliers_orbis <- left_join(suppliers_orbis, nace_data)
    rm(nace_data)  # Remove NACE data to free up memory
    gc()  # Run garbage collection
    
    
    # Load and clean the address data, skipping the first row
    address_data <- read_csv(file_paths[[country]]$address, skip = 1) %>%
      clean_names() %>% 
      rename(bvd_id_number = bv_d_id_number)
    
 
   
    suppliers_orbis <- left_join(suppliers_orbis, address_data, by = "bvd_id_number")
    rm(address_data)  # Remove address data to free up memory
    gc()  # Run garbage collection
    
    
    return(suppliers_orbis)
  }
  
# 2.  Load the data for suppliers and registered companies-------------------------------------------------------
## Setting up the directories for the data
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
matched_orbis_suppliers_dir<- paste0(data_proc_dir, "Matched_orbis_suppliers/")
matched_potential_suppliers_dir <- paste0(data_proc_dir, "Matched_potential_suppliers/")
orbis_financial_dir<- paste0(data_raw_dir,"Orbis_Financial/" )
orbis_Nace_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/ORBIS NACE/"
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

number_suppliers_cern_selected_countries <- fr_it_es_uk_orders %>% select(supplier_code) %>% distinct()
number_orders_cern_selected_countries <- fr_it_es_uk_orders %>% select(order_number) %>% distinct()
suppliers_2016_selected_variables<- suppliers_2016 %>% select(supplier_name, country, city, vat_number, registration_number, supplier_code)
suppliers_2021_selected_variables<- suppliers_2021 %>% select(supplier_name, country, city, vat_number, registration_number, supplier_code)
all_suppliers_selected_variables<- rbind(suppliers_2016_selected_variables, suppliers_2021_selected_variables)
all_suppliers_selected_variables$vat_number<- as.character(all_suppliers_selected_variables$vat_number)
all_suppliers_selected_variables<- all_suppliers_selected_variables %>% distinct()

orders_2016 <- suppliers_2016 %>% select(-contact) %>% rename(chf_amount = sum_chf_amount)
orders_2021<- suppliers_2021
orders_2021$subproject_1<- NA
all_orders<-rbind(orders_2016, orders_2021)
all_orders$order_date<-as.numeric(all_orders$order_date)
all_orders<- clean_names(all_orders)
all_orders<- all_orders %>% rename(registration_year = registration_supplier)
number_suppliers_cern_selected_countries <- fr_it_es_uk_orders %>% select(supplier_code) %>% distinct() # 2,284 suppliers
number_orders_cern_selected_countries <- fr_it_es_uk_orders %>% select(order_number) %>% distinct() # 21,261 orders

## 3.2 Loading the tech_lookup given by CSIL -----------------------------------------------------
#-- This might be redundant as companies might be assigned different codes
tech_level_file <- "CERN_techlevel.xlsx"
CERN_orders_techlevel <- read_excel(paste0(data_raw_dir, tech_level_file))
CERN_orders_techlevel<-clean_names(CERN_orders_techlevel)
tech_level<-CERN_orders_techlevel %>% select(x3_digits,tech_intensity) # don't need the other columns so  am dropping

## 3.3 Loading the Balance by member state lookup --------------------------------------------------
#--- The issue here is that many years do not have a code
balance_MS_file <- "balance_MS.csv"
balance_MS <- read_csv(paste0(data_raw_dir, balance_MS_file))
balance_MS <- clean_names(balance_MS)
balance_MS<- balance_MS %>% rename(country=iso_code,order_date=year)

## 3.4 Merging procurement data, tech lookup and balance ----------------------------------------------
# The changes to city names are unnecessary as I end up dropping the city name variable as I have a number of duplicates

all_orders$x3_digits <- str_extract(all_orders$purchase_code, "^\\d{3}")

all_orders_tech<- left_join(all_orders,tech_level)#joining the two datasets
all_orders_tech_balance<- left_join(all_orders_tech, balance_MS)
all_orders_tech_balance<- all_orders_tech_balance %>% rename(company_name=supplier_name)
all_orders_tech_balance$city[all_orders_tech_balance$city=='LABEGE (TOULOUSE)']<-'TOULOUSE'
all_orders_tech_balance$city[all_orders_tech_balance$city=='LABEGE']<-'TOULOUSE'
all_orders_tech_balance$city[all_orders_tech_balance$city=='NEWBURY  BERKSHIRE']<-'NEWBURYBERKSHIRE'
all_orders_tech_balance$city[all_orders_tech_balance$city=='DECINES']<-'RUEIL MALMAISON'
all_orders_tech_balance$city[all_orders_tech_balance$city=='DECINES CHARPIEU CEDEX']<-'RUEIL MALMAISON'
all_orders_tech_balance$city[all_orders_tech_balance$city=='DECINES CHARPIEU']<-'RUEIL MALMAISON'
all_orders_tech_balance$city[all_orders_tech_balance$city=='DECINES CEDEX']<-'RUEIL MALMAISON'
all_orders_tech_balance$city[all_orders_tech_balance$city=='COLCHESTER  ESSEX']<-'COLCHESTER ESSEX'
all_orders_tech_balance$city[all_orders_tech_balance$city=='NEWBURYBERKSHIRE']<-'NEWBURY BERKSHIRE'
all_orders_tech_balance_selected<-all_orders_tech_balance %>% 
  select(company_name, supplier_code, subroject, subproject_1, order_date, city,x3_digits,code_2_digits, code_1_digit, country, chf_amount, tech_intensity, coefficient_return_supplies, supplies_ms_status, well_balanced,
         media_annua, balance_over_time, registration_year, code_1_digit, code_2_digits, order_number)

# The next step is to replace the tech intensity NA and ? with 0 - not making any assumptions on what 0 means
all_orders_tech_balance$tech_intensity[all_orders_tech_balance$tech_intensity=="?"]<-0 
all_orders_tech_balance$tech_intensity[is.na(all_orders_tech$tech_intensity)]<- 0

# I want to assign the high-tech variable to the order 
high_tech_order <- c(3,4,5,7)# I use the information provided by CSIL
low_tech_order<- c(1,2,6,0,9,8, NA) #

all_orders_tech_balance<- all_orders_tech_balance %>% 
  mutate(tech_level= case_when(tech_intensity %in% high_tech_order~1,
                               TRUE ~0))
selected_countries <- c("IT", "GB", "ES", "FR")
fr_it_es_uk_orders<- all_orders_tech_balance %>% filter(country %in% selected_countries)

saveRDS(all_orders_tech_balance,paste0(data_proc_dir, "all_orders_tech_balance"))
saveRDS(fr_it_es_uk_orders, paste0(data_proc_dir, "fr_it_es_uk_orders"))

##### Selected countries -------------------------------------------------
## The reason I do this is that I then match then manually and I can only do it for a few countries

suppliers_selected_countries <- fr_it_es_uk_orders

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

  
### 3.5 Registered companies (potential suppliers) --------------------------
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

  
files_suppliers <- list( 
italy = "Export_suppliers_italy.xlsx",
spain=  "Export_suppliers_spain.xlsx",
france_1 =  "Export_suppliers_france_1.xlsx",
france_2 =  "Export_suppliers_france_2.xlsx",
uk = "Export_suppliers_uk.xlsx"
)


# Loop through the files and read each one
for (country in names(files_suppliers)){
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

files_potential_suppliers <- list( 
  italy = "Export_potential_suppliers_italy.xlsx",
  spain=  "Export_potential_suppliers_spain.xlsx",
  france =  "Export_potential_suppliers_france.xlsx",
  uk = "Export_potential_suppliers_uk.xlsx"
)

# Loop through the files and read each one
for (country in names(files_potential_suppliers)){
  file_path <- paste0(matched_potential_suppliers_dir, files_potential_suppliers[[country]])
  matched_potential_suppliers <- read_excel(file_path)
  
  # Drop rows with NA in bvd_id_number
  matched_potential_suppliers <- matched_potential_suppliers[!is.na(matched_potential_suppliers$`Matched BvD ID`), ]
  matched_potential_suppliers <- matched_potential_suppliers %>%
    rename(bvd_id_number =`Matched BvD ID`)
  matched_potential_suppliers<- clean_names(matched_potential_suppliers)
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
    nace = paste0(orbis_Nace_dir, "Nace FR_selected_variables.csv"),
    address = paste0(orbis_addresses_dir,"ADDRESS_FR.csv")
  ),
  spain = list(
    financial = paste0(orbis_financial_dir,"Gabriele ES.dta"),
    nace =  paste0(orbis_Nace_dir,"Nace ES_selected_variables.csv"),
    address = paste0(orbis_addresses_dir,"ADDRESS_ES.csv")
  ),
  italy = list(
    financial = paste0(orbis_financial_dir, "Gabriele IT.dta"),
    nace = paste0(orbis_Nace_dir,"NACE IT_selected_variables.csv"),
    address =paste0(orbis_addresses_dir ,"ADDRESS_IT.csv")
  ),
  uk = list(
    financial = paste0(orbis_financial_dir,"Gabriele GB.dta"),
    nace =  paste0(orbis_Nace_dir,"Nace GB_selected_variables.csv"),
    address = paste0(orbis_addresses_dir ,"ADDRESS_GB.csv")
  )
)


## 5.1 Suppliers -----------------------------------------------------------

# matched Orbis data
matched_suppliers_orbis <- list(
  france = matched_orbis_france,
  spain = matched_orbis_spain,
  italy = matched_orbis_italy,
  uk = matched_orbis_uk
)

france_suppliers_orbis <- process_orbis_data("france", file_paths, matched_suppliers_orbis)
italy_suppliers_orbis <- process_orbis_data("italy", file_paths, matched_suppliers_orbis)
spain_suppliers_orbis <- process_orbis_data("spain", file_paths, matched_suppliers_orbis)
uk_suppliers_orbis <- process_orbis_data ("uk", file_paths, matched_suppliers_orbis)
# combine all the datasets together
# Ensure the column names are consistent where possible
colnames(france_suppliers_orbis)[which(names(france_suppliers_orbis) == "country.x")] <- "country"
colnames(france_suppliers_orbis)[which(names(france_suppliers_orbis) == "city.x")] <- "city"
colnames(italy_suppliers_orbis)[which(names(italy_suppliers_orbis) == "country")] <- "country"
colnames(italy_suppliers_orbis)[which(names(italy_suppliers_orbis) == "city.x")] <- "city"
colnames(spain_suppliers_orbis)[which(names(spain_suppliers_orbis) == "country")] <- "country"
colnames(spain_suppliers_orbis)[which(names(spain_suppliers_orbis) == "city.x")] <- "city"
colnames(uk_suppliers_orbis)[which(names(uk_suppliers_orbis) == "country")] <- "country"
colnames(uk_suppliers_orbis)[which(names(uk_suppliers_orbis) == "city.x")] <- "city"

# Bind the dataframes together
matched_suppliers_orbis_data <- bind_rows(france_suppliers_orbis, 
                                      italy_suppliers_orbis, 
                                      spain_suppliers_orbis, 
                                      uk_suppliers_orbis) %>% 
  select(-city.y, -country.y)
saveRDS(matched_suppliers_orbis_data, paste0(data_proc_dir, "matched_suppliers_orbis_data"))
rm(matched_suppliers_orbis_data)
## 5.2  Potential suppliers-----------------------------------------------------------

matched_potential_suppliers_orbis <- list(
  france = matched_orbis_potential_france,
  spain = matched_orbis_potential_spain,
  italy = matched_orbis_potential_italy,
  uk = matched_orbis_potential_uk
)

france_potential_suppliers_orbis <- process_orbis_data("france", file_paths, matched_potential_suppliers_orbis)
italy_potential_suppliers_orbis <- process_orbis_data("italy", file_paths, matched_potential_suppliers_orbis)
spain_potential_suppliers_orbis <- process_orbis_data("spain", file_paths, matched_potential_suppliers_orbis)
uk_potential_suppliers_orbis <- process_orbis_data ("uk", file_paths, matched_potential_suppliers_orbis)

# Ensure the column names are consistent where possible
colnames(france_potential_suppliers_orbis)[which(names(france_potential_suppliers_orbis) == "country.x")] <- "country"
colnames(france_potential_suppliers_orbis)[which(names(france_potential_suppliers_orbis) == "city.x")] <- "city"
colnames(italy_potential_suppliers_orbis)[which(names(italy_potential_suppliers_orbis) == "country")] <- "country"
colnames(italy_potential_suppliers_orbis)[which(names(italy_potential_suppliers_orbis) == "city.x")] <- "city"
colnames(spain_potential_suppliers_orbis)[which(names(spain_potential_suppliers_orbis) == "country")] <- "country"
colnames(spain_potential_suppliers_orbis)[which(names(spain_potential_suppliers_orbis) == "city.x")] <- "city"
colnames(uk_potential_suppliers_orbis)[which(names(uk_potential_suppliers_orbis) == "country")] <- "country"
colnames(uk_potential_suppliers_orbis)[which(names(uk_potential_suppliers_orbis) == "city.x")] <- "city"

# Bind the dataframes together
matched_potential_suppliers_orbis_data  <- bind_rows(france_potential_suppliers_orbis, 
                                                italy_potential_suppliers_orbis, 
                                                spain_potential_suppliers_orbis, 
                                                uk_potential_suppliers_orbis) %>% 
  select(-city.y, -country.y)

saveRDS(matched_potential_suppliers_orbis_data, paste0(data_proc_dir, "matched_potential_suppliers_orbis_data"))
rm(matched_potential_suppliers_orbis_data)

# I do the cleaning in another script