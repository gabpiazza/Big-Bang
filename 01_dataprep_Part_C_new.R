#' ---
#' title: ORBIS-CERN procurement database
#' author: Gabriele Piazza
#' date: 2023-06-27
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
library(beepr)
# 2. Data preparation --------------------------------------------------------
## Load all the potential suppliers
# * 2.1 Load the potential CERN suppliers  ------------------------------------------------
files_list <- list.files(path = "~/Dropbox/PhD/CERN_procurement/Analysis/data_proc/Matched_potential_suppliers",
                         pattern = "*.xlsx",
                         full.names = TRUE)


folder_path <- "~/Dropbox/PhD/CERN_procurement/Analysis/data_proc/Matched_potential_suppliers/"
files_list <- list.files(path = folder_path, full.names = TRUE)

all_potential_suppliers_data <- files_list %>%
  map_dfr(read_excel) 

all_potential_suppliers_data<- clean_names(all_potential_suppliers_data)
all_potential_suppliers_data<- all_potential_suppliers_data %>% rename(bvd_id_number = matched_bv_d_id)
matched_orbis_potential_suppliers<- all_potential_suppliers_data %>% drop_na(bvd_id_number)
spain_potential_suppliers<- matched_orbis_potential_suppliers %>% filter(country =="ES")
france_potential_suppliers<- matched_orbis_potential_suppliers %>% filter(country =="FR")
italy_potential_suppliers <- matched_orbis_potential_suppliers %>% filter(country =="IT")
uk_potential_suppliers<- matched_orbis_potential_suppliers %>% filter(country =="GB")


# * 2.4 Load and match country data ---------------------------------------------------------
#I am applying the matching_cern function to the different datasets


####   ** 2.41 France ---------------------------------------------------------


france<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele FR.dta",encoding='latin1')
france_matched_potential_suppliers <- inner_join(france_potential_suppliers, france, by = 'bvd_id_number')# match it b 
rm(france)

france_matched_potential_suppliers<- france_matched_potential_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
  filter(!is.na(closing_date_format))

france_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE FR.csv")
france_nace<- france_nace %>% rename(bvd_id_number = bvdidnumber)
france_matched_potential_suppliers <- left_join(france_matched_potential_suppliers, france_nace, by = 'bvd_id_number')
rm(france_nace)

france_address<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_FR.csv", skip=1)
france_address<- clean_names(france_address)
france_address<- france_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
  select(bvd_id_number, postcode, city)

france_matched_potential_suppliers<- left_join(france_matched_potential_suppliers, france_address, by = 'bvd_id_number')
france_matched_potential_suppliers <- france_matched_potential_suppliers %>% select(-nacerev2primarycodetextdescripti)
rm(france_address)
#write.csv(france_matched, "~/Dropbox/PhD/procurement_cern/data/processed/Orbis_matched_country/france_matched.csv")

number_france_matched <- unique(france_matched_suppliers$bvd_id_number)

###   ** 2.42 Spain ---------------------------------------------------------------

spain<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele ES.dta",encoding='latin1')

spain_matched_potential_suppliers <- inner_join(spain_potential_suppliers, spain, by = 'bvd_id_number')
spain_matched_potential_suppliers<- spain_matched_potential_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
  filter(!is.na(closing_date_format))

rm(spain)

spain_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE ES.csv")
spain_nace<- spain_nace %>% rename(bvd_id_number = bvdidnumber)
spain_matched_potential_suppliers <- left_join(spain_matched_potential_suppliers, spain_nace, by = 'bvd_id_number')
rm(spain_nace)

spain_address <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_ES.csv", skip=1)
spain_address<- clean_names(spain_address)
spain_address<- spain_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
  select(bvd_id_number, postcode, city)

spain_matched_potential_suppliers<- left_join(spain_matched_potential_suppliers, spain_address, by = 'bvd_id_number')
rm(spain_address)


number_spain_matched<- unique(spain_matched_potential_suppliers$bvd_id_number)




### ** 2.43   *Italy ---------------------------------------------------------------

italy<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele IT.dta",encoding='latin1')

italy_matched_potential_suppliers <- inner_join(italy_potential_suppliers, italy, by = 'bvd_id_number')
italy_matched_potential_suppliers<- italy_matched_potential_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
  filter(!is.na(closing_date_format))

rm(italy)

italy_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE IT.csv")
italy_nace<- italy_nace %>% rename(bvd_id_number = bvdidnumber)
italy_matched_potential_suppliers <- left_join(italy_matched_potential_suppliers, italy_nace, by = 'bvd_id_number')
rm(italy_nace)

italy_address <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_IT.csv", skip=1)
italy_address<- clean_names(italy_address)
italy_address<- italy_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
  select(bvd_id_number, postcode, city)

italy_matched_potential_suppliers<- left_join(italy_matched_potential_suppliers, italy_address, by = 'bvd_id_number')
rm(italy_address)

italy_matched_potential_suppliers<- italy_matched_potential_suppliers %>% select(-nacerev2primarycodetextdescripti)

number_italy_matched<- unique(italy_matched_potential_suppliers$bvd_id_number)


### ** 2.44   *UK ---------------------------------------------------------------

UK <- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele GB.dta",encoding='latin1')

uk_matched_potential_suppliers <- inner_join(uk_potential_suppliers, UK, by = 'bvd_id_number')
uk_matched_potential_suppliers<- uk_matched_potential_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
  filter(!is.na(closing_date_format))

rm(UK)

uk_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE GB.csv")
uk_nace<- uk_nace %>% rename(bvd_id_number = bvdidnumber)
uk_matched_potential_suppliers <- left_join(uk_matched_potential_suppliers, uk_nace, by = 'bvd_id_number')
rm(uk_nace)

uk_address <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_GB.csv", skip=1)
uk_address<- clean_names(uk_address)
uk_address<- uk_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
  select(bvd_id_number, postcode, city)

uk_matched_potential_suppliers<- left_join(uk_matched_potential_suppliers, uk_address, by = 'bvd_id_number')
rm(uk_address)
uk_matched_potential_suppliers<- uk_matched_potential_suppliers %>% select(-nacerev2primarycodetextdescripti)
number_uk_matched<- unique(uk_matched_potential_suppliers$bvd_id_number)

beep()

### 2.5 Putting the data togeher  --------------------------------------------


#### 2.51 Put the data for the four countries together --------------------------------------------------------------------

all_matched_potential_suppliers <- rbind(france_matched_potential_suppliers, spain_matched_potential_suppliers, italy_matched_potential_suppliers, uk_matched_suppliers)
all_matched_potential_suppliers<- all_matched_potential_suppliers  %>% distinct()
suppliers_bvd_list <- unique(full_panel_suppliers$bvd_id_number)
`%notin%` <- Negate(`%in%`)
all_matched_potential_suppliers<- all_matched_potential_suppliers %>% filter(bvd_id_number %notin% suppliers_bvd_list)

#all_matched_potential_suppliers<- all_matched_potential_suppliers %>% filter(matched_company_name !="NATIXIS ASSURANCES")
number_matched_potential_suppliers <- unique(all_matched_potential_suppliers$bvd_id_number) # this gives me 761 companies for 4 countries

# #### 2.52 Loading all the registrations  --------------------------------
cern_registration_procurement <- read_csv(here("Analysis","data_raw", "22_10_31_potential_suppliers.csv"))
cern_registration_procurement<- clean_names(cern_registration_procurement)
cern_registration_procurement<- cern_registration_procurement %>% 
  rename(company_name = suppliername)
all_matched_potential_suppliers<- left_join(all_matched_potential_suppliers, cern_registration_procurement)
cern_registration_procurement_suppliers<- cern_registration_procurement %>% 
  select(suppliercode) %>% distinct()
fr_it_es_uk_pot_suppliers<- cern_registration_procurement %>% filter(country %in% c("IT", "ES", "FR", "UK")) %>% select(company_name) %>% distinct()


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


