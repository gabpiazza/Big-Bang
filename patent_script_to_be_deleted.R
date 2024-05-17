
#' ---
#' title: 
#' author: Gabriele Piazza
#' date: 2023-07-10
#' ---
#This dataset matches the potential suppliers (not-treated) to the data on CERN. The datasets are saved in a separate folder and have not been
#uploaded due to size reasons 

# 1. Install & Load packages --------------------------------------------------------

# * 1.1 Install packages -------------------------------------------------
install.packages("tidyverse")
install.packages("readr")
install.packages("haven")
install.packages("foreign")
install.packages("visdat")
install.packages("haven")
install.packages("data.table")
install.packages("readxl")
install.packages("janitor")
install.packages("stringdist")
install.packages("fuzzyjoin")
options(scipen = 999)
# * 1.2 Load packages ---------------------------------------------------------------
library(visdat)
library(haven)
library(tidyverse)
library(foreign)
library(readr)
library(lubridate)
library(haven)
library(data.table)
library(readxl)
library(janitor)
library(readxl)
library(here)
library(stringdist)
library(fuzzyjoin)


#patents_orbis<- read_excel("data_raw/Export 18_05_2023 21_58 - patents.xlsx",  sheet = "Results")
# Path to the folder containing Excel files
folder_path <- "~/Dropbox/PhD/CERN_procurement/Analysis/data_raw/patent_matched_suppliers/"

# Get the list of files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
df_list <- list()

# Read the "results" sheet from each file and store the data frame in the list
for (file_path in file_list) {
  df <- read_excel(file_path, sheet = "Results")
  df_list <- append(df_list, list(df))
}

# Combine all data frames into a single data frame
patents_orbis_suppliers <- bind_rows(df_list)


# Rename the bvd variable
patents_orbis_suppliers<- patents_orbis_suppliers %>% rename(bvd_id_number = 'Current direct owner(s) BvD ID Number(s)')
patents_orbis_suppliers<- patents_orbis_suppliers %>% filter(!is.na(bvd_id_number))


# Extract the year variable
patents_orbis_suppliers$application_year<-format(patents_orbis_suppliers$`Application/filing date`, "%Y")
patents_orbis_suppliers$publication_year<-format(patents_orbis_suppliers$`Publication date`, "%Y")



# Potential suppliers -----------------------------------------------------


# Path to the folder containing Excel files
folder_path <- "~/Dropbox/PhD/CERN_procurement/Analysis/data_raw/patent_matched_pot_suppliers/"

# Get the list of files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
df_list <- list()

# Read the "results" sheet from each file and store the data frame in the list
for (file_path in file_list) {
  df <- read_excel(file_path, sheet = "Results")
  df_list <- append(df_list, list(df))
}

# Combine all data frames into a single data frame
patents_orbis_pot_suppliers <- bind_rows(df_list)


# Rename the bvd variable
patents_orbis_pot_suppliers<- patents_orbis_pot_suppliers %>% rename(bvd_id_number = 'Current direct owner(s) BvD ID Number(s)')
patents_orbis_pot_suppliers<- patents_orbis_pot_suppliers %>% filter(!is.na(bvd_id_number))


# Extract the year variable
patents_orbis_pot_suppliers$application_year<-format(patents_orbis_pot_suppliers$`Application/filing date`, "%Y")
patents_orbis_pot_suppliers$publication_year<-format(patents_orbis_pot_suppliers$`Publication date`, "%Y")


names_col_suppliers<- names(patents_orbis_suppliers)
names_col_pot_suppliers<- names(patents_orbis_pot_suppliers)

common_columns <- intersect(names_col_suppliers, names_col_pot_suppliers)
patents_orbis_suppliers<- patents_orbis_suppliers %>% 
  select(all_of(common_columns)) %>% select(-...1) %>% distinct()
patents_orbis_pot_suppliers<- patents_orbis_pot_suppliers %>% 
  select(all_of(common_columns)) %>% select(-...1) %>% distinct()

patent_all<- rbind(patents_orbis_suppliers, patents_orbis_pot_suppliers) %>% distinct()




# Calculate the number of observations
patents_application_counts <- patent_all %>%
  dplyr::group_by(bvd_id_number, application_year) %>%
  dplyr::summarize(
    num_applications = n()) %>% 
  rename(year_orbis = application_year) %>% drop_na()


patent_publication_counts<- patent_all %>% 
  dplyr::group_by(bvd_id_number, publication_year) %>% 
  dplyr::summarize(num_publications = n()) %>% 
  rename(year_orbis = publication_year) %>% drop_na()

patents_publications_apps_counts<- left_join(patents_application_counts,patent_publication_counts)
patents_publications_apps_counts<- patents_publications_apps_counts %>% filter(!is.na(year_orbis))

patents_publications_apps_counts$year_orbis<- as.numeric(patents_publications_apps_counts$year_orbis)
panel_data_patents<-expand.grid(year_orbis = 1900:2022, bvd_id_number = unique(patents_publications_apps_counts$bvd_id_number))
panel_data_patents <- panel_data_patents %>% 
  left_join(patents_publications_apps_counts) %>%   
  replace_na(list(num_applications = 0, num_publications = 0))


panel_data_patents <- panel_data_patents %>% dplyr::group_by(bvd_id_number) %>% 
  mutate(cumulative_patents = cumsum(num_applications),
         cumulative_publications = cumsum(num_publications))

rho <- 0.15

# Drop the original patent_stock column if it exists
if("patent_stock" %in% colnames(panel_data_patents)) {
  panel_data_patents <- panel_data_patents %>%
    select(-patent_stock)
}

# Arrange data
panel_data_patents <- panel_data_patents %>%
  arrange(bvd_id_number, year_orbis)

# Calculate patent stock
for (firm in unique(panel_data_patents$bvd_id_number)) {
  indices <- which(panel_data_patents$bvd_id_number == firm)
  patent_stock_temp <- 0 # Initialize the patent stock
  
  for (i in indices) {
    patent_stock_temp <- patent_stock_temp * (1 - rho) + panel_data_patents$num_applications[i]
    panel_data_patents$patent_stock[i] <- patent_stock_temp
  }
}

# Check and remove existing publication_patent_stock column
if("publication_patent_stock" %in% colnames(panel_data_patents)) {
  panel_data_patents <- panel_data_patents %>%
    select(-publication_patent_stock)
}

# Arrange data (if not already arranged)
panel_data_patents <- panel_data_patents %>%
  arrange(bvd_id_number, year_orbis)

# Calculate publication patent stock
rho <- 0.15 # Example decay factor, adjust as needed
for (firm in unique(panel_data_patents$bvd_id_number)) {
  indices <- which(panel_data_patents$bvd_id_number == firm)
  publication_stock_temp <- 0 # Initialize the publication stock
  
  for (i in indices) {
    publication_stock_temp <- publication_stock_temp * (1 - rho) + panel_data_patents$num_publications[i]
    panel_data_patents$publication_patent_stock[i] <- publication_stock_temp
  }
}




panel_data_patents_1990_2019<- panel_data_patents %>% filter(year_orbis>1989 & year_orbis<2020) %>% distinct()


# I now want to expand the grid so that I can match it to the dataset that I have 




panel_data_patents_1990_2019<- panel_data_patents_1990_2019 %>% distinct()

write.csv(panel_data_patents_1990_2019, here("data_proc", "panel_data_patents_1990_2019.csv"))


# Potential suppliers -----------------------------------------------------


# Path to the folder containing Excel files
folder_path <- "~/Dropbox/PhD/CERN_procurement/Analysis/data_raw/patent_matched_pot_suppliers/"

# Get the list of files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store the data frames
df_list <- list()

# Read the "results" sheet from each file and store the data frame in the list
for (file_path in file_list) {
  df <- read_excel(file_path, sheet = "Results")
  df_list <- append(df_list, list(df))
}

# Combine all data frames into a single data frame
patents_orbis_pot_suppliers <- bind_rows(df_list)


# Rename the bvd variable
patents_orbis_pot_suppliers<- patents_orbis_pot_suppliers %>% rename(bvd_id_number = 'Current direct owner(s) BvD ID Number(s)')
patents_orbis_pot_suppliers<- patents_orbis_pot_suppliers %>% filter(!is.na(bvd_id_number))


# Extract the year variable
patents_orbis_pot_suppliers$application_year<-format(patents_orbis_pot_suppliers$`Application/filing date`, "%Y")
patents_orbis_pot_suppliers$publication_year<-format(patents_orbis_pot_suppliers$`Publication date`, "%Y")


# Calculate the number of observations
patents_application_pot_counts <- patents_orbis_pot_suppliers %>%
  dplyr::group_by(bvd_id_number, application_year) %>%
  dplyr::summarize(
    num_applications = n()) %>% 
  rename(year_orbis = application_year) %>% drop_na()


patent_publication_pot_counts<- patents_orbis_pot_suppliers %>% 
  dplyr::group_by(bvd_id_number, publication_year) %>% 
  dplyr::summarize(num_publications = n()) %>% 
  rename(year_orbis = publication_year) %>% drop_na()

patents_publications_pot_apps_counts<- left_join(patents_application_pot_counts,patent_publication_pot_counts)
patents_publications_pot_apps_counts<- patents_publications_pot_apps_counts %>% filter(!is.na(year_orbis)) %>% 
  distinct()

patents_publications_pot_apps_counts$year_orbis<- as.numeric(patents_publications_pot_apps_counts$year_orbis)
panel_data_pot_patents<-expand.grid(year_orbis = 1900:2022, bvd_id_number = unique(patents_publications_pot_apps_counts$bvd_id_number))
panel_data_pot_patents<- panel_data_pot_patents %>% left_join(patents_publications_pot_apps_counts) %>%   
  replace_na(list(num_applications = 0, num_publications =0))


# Arrange data
panel_data_pot_patents <- panel_data_pot_patents %>%
  arrange(bvd_id_number, year_orbis)

# Calculate patent stock
for (firm in unique(panel_data_pot_patents$bvd_id_number)) {
  indices <- which(panel_data_pot_patents$bvd_id_number == firm)
  patent_stock_temp <- 0 # Initialize the patent stock
  
  for (i in indices) {
    patent_stock_temp <- patent_stock_temp * (1 - rho) + panel_data_pot_patents$num_applications[i]
    panel_data_pot_patents$patent_stock[i] <- patent_stock_temp
  }
}

# Ensure the data is arranged
panel_data_pot_patents <- panel_data_pot_patents %>%
  arrange(bvd_id_number, year_orbis)

# Calculate patent stock publication
for (firm in unique(panel_data_pot_patents$bvd_id_number)) {
  indices <- which(panel_data_pot_patents$bvd_id_number == firm)
  publication_stock_temp <- 0 # Initialize the publication stock
  
  for (i in indices) {
    # Assuming num_publications is the column for the number of publications
    publication_stock_temp <- publication_stock_temp * (1 - rho) + panel_data_pot_patents$num_publications[i]
    panel_data_pot_patents$publication_patent_stock[i] <- publication_stock_temp
  }
}


panel_data_pot_patents_1990_2019<- panel_data_pot_patents %>% filter(year_orbis>1989 & year_orbis<2020)


# I now want to expand the grid so that I can match it to the dataset that I have 
# patents_publications_pot_apps_counts_1995_2019<- patents_publications_pot_apps_counts %>% filter(year_orbis>1994 & year_orbis<2020)
# patents_publications_pot_apps_counts_1995_20119 <- patents_publications_pot_apps_counts_1995_2019 %>%
#   mutate(num_publications = ifelse(is.na(num_publications), 0, num_publications))
# panel_data_pot_patents_1995_2019<- panel_data_pot_patents_1995_2019 %>% distinct()

write.csv(panel_data_pot_patents_1990_2019, here("data_proc", "patents_pot_suppliers_1990_2019.csv"))

