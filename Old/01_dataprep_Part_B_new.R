#' ---
#' title: ORBIS-CERN procurement database
#' author: Gabriele Piazza
#' date: 2023-06-27
#' ---
#This dataset matches the suppliers (treated) to the data on CERN. The datasets are saved in a separate folder nad have not been
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
## Load all the suppliers
# * 2.1 Load the potebtial CERN suppliers  ------------------------------------------------
files_list <- list.files(path = "~/Dropbox/PhD/CERN_procurement/Analysis/data_proc/Matched_orbis_suppliers",
                         pattern = "*.xlsx",
                         full.names = TRUE)


folder_path <- "~/Dropbox/PhD/CERN_procurement/Analysis/data_proc/Matched_orbis_suppliers/"
files_list <- list.files(path = folder_path, full.names = TRUE)

alldata <- files_list %>%
  map_dfr(read_excel) 

alldata<- clean_names(alldata)
alldata<- alldata %>% rename(bvd_id_number = matched_bv_d_id)
matched_orbis_suppliers<- alldata %>% drop_na(bvd_id_number)
spain_suppliers<- matched_orbis_suppliers %>% filter(country =="ES")
france_suppliers<- matched_orbis_suppliers %>% filter(country =="FR")
france_suppliers$company_name[france_suppliers$company_name=='SAINT-GOBAIN PERFORMANCE PLASTICS ASTI']<-'SAINT-GOBAIN PERFORMANCE PLASTICS  ASTI'
france_suppliers$company_name[france_suppliers$company_name=='E.T.S S.R.A']<-'E.T.S  S.R.A'
italy_suppliers <- matched_orbis_suppliers %>% filter(country =="IT")
uk_suppliers<- matched_orbis_suppliers %>% filter(country =="GB")


number_suppliers<- unique(matched_orbis_suppliers$bvd_id_number) # this gives me 1,874 companies


# share_matched_country <- alldata %>% dplyr::group_by(country) %>% 
#   dplyr::summarize(total_suppliers = n(),
#             number_matched = sum(!is.na(bvd_id_number)))
# share_matched_country<- share_matched_country %>% dplyr::mutate(share = (number_matched/total_suppliers)*100)
# 
# country_plot_matched_orders_share<- ggplot(share_matched_country, aes(x = reorder(as.factor(country),-share), y = share, fill = country)) +
#   geom_bar(stat = "identity", position = "dodge")+labs(y= "Matched as % of total orders", x= "")+
#   theme_classic()+
#   theme(legend.position="none")


# * 2.2 Load the CERN firms  ------------------------------------------------
#matched_cern_firms<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/BVD_codes_CERN.csv")# this loads all the matched cern firms
#matched_cern_firms<- matched_cern_firms %>% rename(bvd_id_number = `Old ID`) # rename the id variable so I can do the matchin
#matched_cern_firms$country<- substr(matched_cern_firms$bvd_id_number, 1, 2) # I am doing this so that I can extract 
#unique(matched_cern_firms$country)# this is to check the list of countries


# * 2.3 Functions ---------------------------------------------------------------
#We re-construct the YEAR variable based on the following convention. 
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)

matching_cern <- function(country_suppliers, orbis_country){
  new_dataset <- inner_join(country_suppliers, orbis_country)
  new_dataset<- mutate(new_dataset,closing_date_format = str_remove(closing_date, "T00:00:00.000Z"))
  new_dataset<- filter(new_dataset, !is.na(closing_date_format))
  new_dataset <-  select(new_dataset,-research_development_expenses) %>% drop_na()
}


matching_cern_nace <- function(data){
  data<- data %>% rename(bvd_id_number = bvdidnumber)
  new_dataset <- inner_join(matched_cern_firms, data)
  new_dataset <- new_dataset %>% select(1:11)
}

matching_cern_addresses<- function(data){
  data<- clean_names(data)
  data<- data %>% rename(bvd_id_number = "bv_d_id_number") %>% 
    select(bvd_id_number, postcode, city)
  new_dataset <- inner_join(matched_cern_firms, data)
}




# * 2.4 Load and match country data ---------------------------------------------------------
#I am applying the matching_cern function to the different datasets


####   ** 2.41 France ---------------------------------------------------------


france<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele FR.dta",encoding='latin1')
france_matched_suppliers <- inner_join(france_suppliers, france, by = 'bvd_id_number')# match it b 
rm(france)

france_matched_suppliers<- france_matched_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
  filter(!is.na(closing_date_format))

france_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE FR.csv")
france_nace<- france_nace %>% rename(bvd_id_number = bvdidnumber)
france_matched_suppliers <- left_join(france_matched_suppliers, france_nace, by = 'bvd_id_number')
rm(france_nace)

france_address<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_FR.csv", skip=1)
france_address<- clean_names(france_address)
france_address<- france_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
  select(bvd_id_number, postcode, city)

france_matched_suppliers<- left_join(france_matched_suppliers, france_address, by = 'bvd_id_number')
france_matched_suppliers <- france_matched_suppliers %>% select(-nacerev2primarycodetextdescripti)
rm(france_address)
#write.csv(france_matched, "~/Dropbox/PhD/procurement_cern/data/processed/Orbis_matched_country/france_matched.csv")

number_france_matched <- unique(france_matched_suppliers$bvd_id_number)

###   ** 2.42 Spain ---------------------------------------------------------------

spain<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele ES.dta",encoding='latin1')

spain_matched_suppliers <- inner_join(spain_suppliers, spain, by = 'bvd_id_number')
spain_matched_suppliers<- spain_matched_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
  filter(!is.na(closing_date_format))

rm(spain)

spain_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE ES.csv")
spain_nace<- spain_nace %>% rename(bvd_id_number = bvdidnumber)
  spain_matched_suppliers <- left_join(spain_matched_suppliers, spain_nace, by = 'bvd_id_number')
  rm(spain_nace)
  
spain_address <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_ES.csv", skip=1)
spain_address<- clean_names(spain_address)
spain_address<- spain_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
   select(bvd_id_number, postcode, city)
 
 spain_matched_suppliers<- left_join(spain_matched_suppliers, spain_address, by = 'bvd_id_number')
 rm(spain_address)
 

 number_spain_matched<- unique(spain_matched_suppliers$bvd_id_number)
 
 
  

 ### ** 2.43   *Italy ---------------------------------------------------------------
 
 italy<- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele IT.dta",encoding='latin1')
 
 italy_matched_suppliers <- inner_join(italy_suppliers, italy, by = 'bvd_id_number')
 italy_matched_suppliers<- italy_matched_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
   filter(!is.na(closing_date_format))
 
 rm(italy)
 
 italy_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE IT.csv")
 italy_nace<- italy_nace %>% rename(bvd_id_number = bvdidnumber)
 italy_matched_suppliers <- left_join(italy_matched_suppliers, italy_nace, by = 'bvd_id_number')
 rm(italy_nace)
 
 italy_address <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_IT.csv", skip=1)
 italy_address<- clean_names(italy_address)
 italy_address<- italy_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
   select(bvd_id_number, postcode, city)
 
 italy_matched_suppliers<- left_join(italy_matched_suppliers, italy_address, by = 'bvd_id_number')
 rm(italy_address)
 
 italy_matched_suppliers<- italy_matched_suppliers %>% select(-nacerev2primarycodetextdescripti)
 
 number_italy_matched<- unique(italy_matched_suppliers$bvd_id_number)

 
 ### ** 2.44   *UK ---------------------------------------------------------------
 
 UK <- read_dta("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_LIBRARY/Gabriele/Gabriele GB.dta",encoding='latin1')
 
 uk_matched_suppliers <- inner_join(uk_suppliers, UK, by = 'bvd_id_number')
 uk_matched_suppliers<- uk_matched_suppliers %>% mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z")) %>% 
   filter(!is.na(closing_date_format))
 
 rm(UK)
 
 uk_nace<- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_NACE_2/NACE GB.csv")
 uk_nace<- uk_nace %>% rename(bvd_id_number = bvdidnumber)
 uk_matched_suppliers <- left_join(uk_matched_suppliers, uk_nace, by = 'bvd_id_number')
 rm(uk_nace)
 
 uk_address <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/ORBIS_ADDRESSES/ADDRESS_GB.csv", skip=1)
 uk_address<- clean_names(uk_address)
 uk_address<- uk_address %>% rename(bvd_id_number = "bv_d_id_number") %>% 
   select(bvd_id_number, postcode, city)
 
 uk_matched_suppliers<- left_join(uk_matched_suppliers, uk_address, by = 'bvd_id_number')
 rm(uk_address)
 uk_matched_suppliers<- uk_matched_suppliers %>% select(-nacerev2primarycodetextdescripti)
 number_uk_matched<- unique(uk_matched_suppliers$bvd_id_number)
 
 beep()

### 2.5 Putting the data togeher  --------------------------------------------


#### 2.51 Put the data for the four countries together --------------------------------------------------------------------

all_matched_suppliers <- rbind(france_matched_suppliers, spain_matched_suppliers, italy_matched_suppliers, uk_matched_suppliers)
all_matched_suppliers<- all_matched_suppliers  %>% distinct()
all_matched_suppliers<- all_matched_suppliers %>% filter(matched_company_name !="NATIXIS ASSURANCES")# Why am I excluding this?
number_matched_suppliers <- unique(all_matched_suppliers$bvd_id_number) # this gives me 1711 companies for 4 countries

#### 2.52  Load the procurement data---------------------------------------------------------------------

orders_2014_2021<- read_excel("~/Dropbox/PhD/procurement_cern/data/raw/2021-06-29 - CERN Orders 2014-2021_clean.xlsx")
orders_pre_2014<- read_excel("~/Dropbox/PhD/procurement_cern/data/raw/21_10_27_Suppliers_cern_2016_nocontacts.xlsx")
orders_pre_2014<- orders_pre_2014 %>% select(-`SUBPROJECT 1`, -CONTACT) %>% rename("CHF_AMOUNT"="SUM(CHF_AMOUNT)")
all_orders<-rbind(orders_2014_2021, orders_pre_2014)
all_orders<-all_orders %>% distinct_all()
all_orders<-clean_names(all_orders)
all_orders$order_date<-as.numeric(all_orders$order_date)
all_orders<- clean_names(all_orders)
fr_it_es_uk_orders<- all_orders %>% filter(country %in% c("FR", "GB","IT", "ES"))

number_suppliers_cern_selected_countries <- fr_it_es_uk_orders %>% select(supplier_code) %>% distinct()
number_orders_cern_selected_countries <- fr_it_es_uk_orders %>% select(order_number) %>% distinct()

#### 2.53 Tech level -----------------------------------------------------
#-- This might be redundant as companies might be assigned different codes
CERN_orders_techlevel <- read_excel("~/Dropbox/PhD/procurement_cern/data/raw/CERN_techlevel.xlsx")
CERN_orders_techlevel<-clean_names(CERN_orders_techlevel)
CERN_orders_techlevel<-CERN_orders_techlevel %>% rename(code_2_digits=x2_digit )# columns have different names
tech_level<-CERN_orders_techlevel %>% select(x3_digits,tech_intensity) # don't need the other columns so  am dropping them 

### Load suppliers this is only up to 2008
#suppliers_registration_year <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/suppliers_registration_year.csv")
#suppliers_registration_year <- clean_names(suppliers_registration_year)
all_orders<- all_orders %>% rename(registration_year = registration_supplier)


#### 2.54  Balance Member states table  --------------------------------------------------
#--- The issue here is that many years do not have a code
balance_MS <- read_csv("~/Dropbox/PhD/procurement_cern/data/raw/balance_MS.csv")
balance_MS <- clean_names(balance_MS)
balance_MS<- balance_MS %>% rename(country=iso_code,order_date=year)


#### 2.55 Merging procurement data, tech lookup and balance ----------------------------------------------
# The changes to city names are unnecessary as I end up dropping the city name variable as I have a number of duplicates

all_orders$x3_digits <- str_extract(all_orders$purchase_code, "^\\d{3}")

all_suppliers_95_21_tech<- left_join(all_orders,tech_level)#joining the two datasets
all_suppliers_95_21_tech_ms<- left_join(all_suppliers_95_21_tech, balance_MS)
all_suppliers_95_21_tech_ms<- all_suppliers_95_21_tech_ms %>% rename(company_name=supplier_name)
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='LABEGE (TOULOUSE)']<-'TOULOUSE'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='LABEGE']<-'TOULOUSE'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='NEWBURY  BERKSHIRE']<-'NEWBURYBERKSHIRE'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='DECINES']<-'RUEIL MALMAISON'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='DECINES CHARPIEU CEDEX']<-'RUEIL MALMAISON'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='DECINES CHARPIEU']<-'RUEIL MALMAISON'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='DECINES CEDEX']<-'RUEIL MALMAISON'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='COLCHESTER  ESSEX']<-'COLCHESTER ESSEX'
all_suppliers_95_21_tech_ms$city[all_suppliers_95_21_tech_ms$city=='NEWBURYBERKSHIRE']<-'NEWBURY BERKSHIRE'
#all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SEF - SOCIETE ETUDE & FABRICATION POUR RECHERCHE & INDUSTRIE"]<-'SEF - SOCIETE ETUDE & FABRICATION'
#all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SEF STE D'ETUDES ET FABRICATIONS"]<-'SEF - SOCIETE ETUDE & FABRICATION'
#all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SOLYRO - SOCIETE LYONNAISE DE ROBINETTERIE"]<-'SOLYRO'
#all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SCHNEIDER ELECTRIC (FRANCE) (CRITICAL POWER & COOLING SERV.)"]<-'SCHNEIDER ELECTRIC (FRANCE)'
#all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=='ELECTRO - MECHANIC EQUIPEMENT (Alias: E.M.E.)']<-'ELECTRO - MECHANIC EQUIPEMENT'



# What I am doing here is to select the variables of interest

all_suppliers_95_21_tech_ms<- as.data.frame(all_suppliers_95_21_tech_ms)# I do this as I had some issues with applying the dplyr functions 

all_orders_95_21_tech_ms_selected<-all_suppliers_95_21_tech_ms %>% 
  select(company_name, supplier_code, order_date, city,x3_digits,code_2_digits, code_1_digit, country, chf_amount, tech_intensity, coefficient_return_supplies, supplies_ms_status, well_balanced,
         media_annua, balance_over_time, registration_year, code_1_digit, code_2_digits, order_number)

# The next step is to replace the tech intensity NA and ? with 0 - not making any assumptions on what 0 means
all_orders_95_21_tech_ms_selected$tech_intensity[all_orders_95_21_tech_ms_selected$tech_intensity =="?"]<- 0 # I need to fix ths
all_orders_95_21_tech_ms_selected$tech_intensity[is.na(all_orders_95_21_tech_ms_selected$tech_intensity)]<- 0

write.csv(all_orders_95_21_tech_ms_selected, here("Analysis", "data_proc", "all_orders_tech_"))


all_orders_tech<- read_csv("data_proc/all_orders_tech.csv") %>% select(-'...1')
# I want to assign the high-tech variable to the order 
high_tech_order <- c(3,4,5,7)# I use the information provided by CSIL
low_tech_order<- c(1,2,6,0,9,8, NA) #

all_orders_tech<- all_orders_tech %>% 
  mutate(tech_level= case_when(tech_intensity %in% high_tech_order~1,
                               TRUE ~0))
write.csv(all_orders_tech, here( "data_proc", "all_orders_tech_level.csv"))

# I now want to calculate the total order by year. The issue here is that companies might receive multiple orders
# in the same year and that these might have a varying technological intensity 

# I realized that this is wrong. Why? It is best to do so by bvd_id ou might have different companies with different supplier codes but the same bvd_id 
# I do this only for the sum of the chf_amount by year
# all_orders_95_21_tech_ms_selected<- all_orders_95_21_tech_ms_selected%>% 
#  select(-city) %>% 
#  distinct() %>% 
#   group_by(company_name, order_date) %>% mutate(chf_amount= sum(chf_amount)) %>% 
#   ungroup() %>% distinct()


# I create variables for the first order received, the last order received, the number of total orders and the amount 
#all_orders_95_21_tech_ms_selected<- all_orders_95_21_tech_ms_selected %>% 
 # group_by(supplier_code) %>% mutate(first_order= min(order_date),last_order = max(order_date), total_orders = n(), total_orders_amount = sum(total_chf_amount_year)) %>% ungroup()

# I create a variable for the first order amount and first order tech

#all_orders_95_21_tech_ms_selected <- all_orders_95_21_tech_ms_selected %>%
 # group_by(supplier_code) %>%
 #mutate(first_order_amount = total_chf_amount_year[which.min(order_date)],
        # first_order_tech = max_tech[which.min(order_date)]) %>%
  #ungroup()



# I calculate the gap between registration and the first order
#all_orders_95_21_tech_ms_selected<- all_orders_95_21_tech_ms_selected %>% mutate(registration_first_order = first_order -registration_year)
#all_orders_95_21_tech_ms_selected<- all_orders_95_21_tech_ms_selected %>%  select(-company_name,supplier_code, country, first_order, last_order, total_orders, total_orders_amount, first_order_amount,
                                                                        #first_order_tech, registration_year, registration_first_order, code_1_digit, code_2_digits) %>% distinct()


#### 2.56 Matching procurement and orbis data-----------------------------------------------
# Firstly, I create the date variable, by removing the time, from the closing date,
cern_orbis_matched_suppliers<-all_matched_suppliers %>% 
  mutate(closing_date_format = str_remove(closing_date, "T00:00:00.000Z"),
         date_closing = as.Date(closing_date_format))

# I then extract month and year 
cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers %>% 
  mutate(year_orbis = year(closing_date_format),
         month_orbis = month(closing_date_format))

# This below might be redudndant
#cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers_clean  %>% 
#  mutate(year_orbis = as.numeric(year(date_closing)),
 #        month_orbis = as.numeric(month(date_closing)))

cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers_clean %>% drop_na(year_orbis)
cern_orbis_matched_suppliers_clean$year_orbis<-as.numeric(cern_orbis_matched_suppliers_clean$year_orbis)
cern_orbis_matched_suppliers_clean$month_orbis<- as.numeric(cern_orbis_matched_suppliers_clean$month_orbis)


# This follows the convention explained in the Kalemli-Ozcan paper
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)

cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers_clean %>% mutate(year = case_when (month_orbis <6 ~ year_orbis -1, 
                                                                                                     month_orbis >5 ~ year_orbis))


cern_orbis_matched_suppliers_clean$matching_year <- cern_orbis_matched_suppliers_clean$year

# I create this variable for the matching 
all_orders_95_21_tech_ms_selected$matching_year <- all_orders_95_21_tech_ms_selected$order_date  
#all_orders_95_17_tech_ms_selected<- all_orders_95_21_tech_ms_selected %>% filter(order_date<2018)

# this bvd id lookup comes from the file analysis B - i have to move things around and tidy up 
bvd_id_lookup <- read_csv(here("Analysis", "data_proc", "bvd_id_lookup_final.csv"))

# I match the procurement data to the bvd_id_lookup 
all_orders_95_17_tech_ms_selected<- left_join(all_orders_95_17_tech_ms_selected, bvd_id_lookup)
all_orders_95_21_tech_ms_selected<- left_join(all_orders_95_21_tech_ms_selected, bvd_id_lookup)


# This is just to populate the registration year
all_orders_95_21_tech_ms_selected <- all_orders_95_21_tech_ms_selected %>%
  group_by(bvd_id_number) %>%
  mutate(
    has_non_missing = !all(is.na(registration_year)),
    registration_year = ifelse(has_non_missing, replace(registration_year, is.na(registration_year) | registration_year > min(registration_year, na.rm = TRUE), min(registration_year, na.rm = TRUE)), NA)
  ) %>%
  ungroup() %>%
  select(-has_non_missing)


cern_orbis_matched_suppliers<-left_join(cern_orbis_matched_suppliers_clean, all_orders_95_21_tech_ms_selected, by =c('bvd_id_number', 'matching_year'))


cern_orbis_matched_suppliers<- cern_orbis_matched_suppliers %>% select(-company_name.x, -city.x, -matched_company_name) %>% distinct()
cern_orbis_matched_suppliers$registration_year<- as.numeric(cern_orbis_matched_suppliers$registration_year)

#I re-construct the YEAR variable based on the following convention. 
#If the closing date is after or on June 1st, the current year is assigned (if CLOSEDATE is 4th of August, 2003, the year is 2003). Otherwise, 
#the previous year is assigned (if CLOSEDATE is 25th of May, 2003, the year is 2002)


#cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers_clean %>% select(-year) %>% 
#  mutate(year_orbis = year(closing_date_format),
#         month_orbis = month(closing_date_format))

#cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers_clean %>% drop_na(year_orbis)
#cern_orbis_matched_suppliers_clean$year_orbis<-as.numeric(cern_orbis_matched_suppliers_clean$year_orbis)
#cern_orbis_matched_suppliers_clean$month_orbis<- as.Date(cern_orbis_matched_suppliers_clean$month_orbis)

#cern_orbis_matched_suppliers_clean$year[cern_orbis_matched_suppliers_clean$month_orbis<6]<-(year(cern_orbis_matched_suppliers_clean$closing_date) -1)
#cern_orbis_matched_suppliers_clean$year[cern_orbis_matched_suppliers_clean$month_orbis>5]<-cern_orbis_matched_suppliers_clean$year_orbis

#cern_orbis_matched_suppliers_clean<- cern_orbis_matched_suppliers_clean %>% mutate(year = case_when (month_orbis <6 ~ year_orbis -1, 
                                            #                         month_orbis >5 ~ year_orbis))

# I now want to calculate the total order by year. The issue here is that companies might receive multiple orders
# in the same year and that these might have a varying technological intensity - I initially did by supplier_code but I had some issues
#

# Why do I have NAs for the total_chf_amount> because I am matching based on the year

cern_orbis_matched_suppliers_selected_variables <- cern_orbis_matched_suppliers %>% 
  select(bvd_id_number, order_date, chf_amount, tech_intensity, registration_year, order_number, code_2_digits) %>% 
  distinct() %>% 
  group_by(bvd_id_number, order_date) %>% 
  mutate(total_chf_amount_year = sum(chf_amount), 
         max_tech = max(tech_intensity),
         number_orders_year = n_distinct(order_number),
         code_2_digits = ifelse(length(code_2_digits) == 0, 0, code_2_digits[which.max(chf_amount)])) %>%
  distinct() %>% 
  drop_na(total_chf_amount_year) %>% 
  select(-tech_intensity, -chf_amount,-order_number) %>% 
  distinct()


# the registration year at the moment is character, I have to change it to numeric
cern_orbis_matched_suppliers_selected_variables$registration_year<-as.numeric(cern_orbis_matched_suppliers_selected_variables$registration_year)


# In this way
cern_orbis_matched_suppliers_selected_variables<- cern_orbis_matched_suppliers_selected_variables %>% 
  select(bvd_id_number, order_date, registration_year, total_chf_amount_year, max_tech, number_orders_year, code_2_digits) %>% 
  distinct() %>% 
  group_by(bvd_id_number) %>% 
  mutate(first_order= min(order_date),last_order = max(order_date), total_orders = sum(number_orders_year), 
         total_orders_amount = sum(total_chf_amount_year),
         first_order_amount = total_chf_amount_year[which.min(order_date)],
         first_order_tech = max_tech[which.min(order_date)],
         code_2_digits = code_2_digits[which.min(order_date)],
         registration_first_order = first_order -registration_year) %>% 
  select(bvd_id_number, order_date, total_chf_amount_year, max_tech, first_order, last_order, total_orders, total_orders_amount, 
         first_order_amount, first_order_tech, registration_first_order, registration_year, code_2_digits)%>% 
  distinct()
  
write.csv(cern_orbis_matched_suppliers_selected_variables, here("Analysis","data_proc", "cern_orbis_matched_selected_variables.csv"))
write.csv(cern_orbis_matched_suppliers_clean, here("Analysis", "data_proc", "cern_orbis_matched_suppliers.csv"))

cern_orbis_matched_suppliers<- read.csv(here("Analysis","data_proc", "cern_orbis_matched_suppliers.csv"))
cern_orbis_matched_suppliers<- cern_orbis_matched_suppliers %>% select( -identifier, -score, -nacerev2mainsection, -nationalindustryclassificationus, -primarycodesinthisclassification, -primarycodeinnationalindustrycla, -secondarycodesinthisclassificati,
       -secondarycodeinnationalindustryc, -nacerev2mainsection, -nacerev2corecode4digits,-nacerev2corecodetextdescription,
       -nacerev2secondarycodes,-nacerev2secondarycodetextdescrip, -postcode, -city.y) %>% distinct()

cern_orbis_matched_suppliers_selected_variables<- read.csv(here("Analysis","data_proc", "cern_orbis_matched_selected_variables.csv"))
cern_orbis_matched_suppliers_selected_variables<- cern_orbis_matched_suppliers_selected_variables %>% select(-X) %>% distinct()
cern_orbis_matched_suppliers_selected_variables$matching_year<- cern_orbis_matched_suppliers_selected_variables$order_date
#cern_orbis_matched_suppliers_france<- cern_orbis_matched_suppliers %>% filter(country.x=="FR")
#cern_orbis_matched_suppliers_france<- left_join(cern_orbis_matched_suppliers_france, cern_orbis_matched_suppliers_selected_variables)
#cern_orbis_matched_suppliers_france<- cern_orbis_matched_suppliers_france %>% distinct()
#cern_orbis_matched_suppliers_france<- cern_orbis_matched_suppliers_france %>% distinct()
#cern_orbis_matched_suppliers_france

#testing<- cern_orbis_matched_suppliers_france %>% filter(bvd_id_number ==	'FR456500537')


# I merge the two datasets 
cern_orbis_matched_suppliers_vars<-left_join(cern_orbis_matched_suppliers,cern_orbis_matched_suppliers_selected_variables)%>%
  distinct()

# I want to fill the columns with variables that do not change by time with the values of other years
cern_orbis_matched_suppliers_vars<- cern_orbis_matched_suppliers_vars %>% 
  group_by(bvd_id_number) %>% fill(nacerev2primarycodes, registration_year, registration_first_order)
cern_orbis_matched_suppliers_vars<- cern_orbis_matched_suppliers_vars %>% distinct() %>% ungroup()

# I create a new variables for the consolidations codes. Before doing this, I have U1, U2, C1, and C2. I want just the initial letters. 

cern_orbis_matched_suppliers_vars$consolidation_l <- substr(cern_orbis_matched_suppliers_vars$consolidation_code,1,1)


# Group the data by bvd_id_number and year, and check for the presence of "U" and "C"
# This might be redundant and you can jump to 524
#consolidated_accounts <- cern_orbis_matched_suppliers_vars %>%
#  group_by(bvd_id_number, year) %>%
 # summarise(
  #  has_U = any(consolidation_l == "U"),
   # has_C = any(consolidation_l == "C"),
    #turnover_U = operating_revenue_turnover_[consolidation_l == "U"],
    #turnover_C = ifelse(any(consolidation_l == "C"), operating_revenue_turnover_[consolidation_l == "C"], NA)
#  ) %>%
 # ungroup()

#consolidated_accounts <- consolidated_accounts %>% 
 # group_by(bvd_id_number) %>%
#  mutate(
 #   priority = ifelse(all(has_U == TRUE) & !all(has_C == TRUE), "U",
  #                    ifelse(all(has_C == TRUE) & !all(has_U == TRUE), "C",
   #                          ifelse(has_U == TRUE & has_C == TRUE & all(turnover_U == turnover_C), "C", ""))),
    #switcher = ifelse(has_U == TRUE & has_C == TRUE & any(turnover_U != turnover_C), TRUE, FALSE)
  #) %>%
  #ungroup()

#consolidated_accounts<- consolidated_accounts %>% 
 # group_by(bvd_id_number) %>% 
  #mutate(number_years = n()) %>% ungroup()

#switchers <- consolidated_accounts %>% filter(switcher ==TRUE)

# I keep only the observations for unconsolidated accounts 
unconsolidated_accounts <- cern_orbis_matched_suppliers_vars %>% 
  mutate(first_year = min(year),
         last_year = max(year),
         first_order = first_order,
         order_after_last_orbis = last_year>first_order) %>% 
  filter(consolidation_l =="U") %>% distinct()

# I now create some variables for years before and after order - I want to see whether there is data  pre and post treatment 
unconsolidated_accounts<- unconsolidated_accounts %>% group_by(bvd_id_number) %>% 
  mutate(year_after_order = last_year - first_order,
            year_before_order = first_order -first_year) %>% 
  ungroup() %>% 
  distinct()
  


unconsolidated_accounts<- unconsolidated_accounts %>% 
  group_by(bvd_id_number, year) %>% 
  filter( ebitda == max(ebitda))%>% # if multiple ebitda per year, I get the maximum
  select( -consolidation_code, -X, company_name, -matched_company_name, -city.x, -nacerev2primarycodes, -filing_type) %>% 
  ungroup() %>% 
  distinct()


# Even in this case, I want to remove duplicates
number_companies<- unique(unconsolidated_accounts$bvd_id_number)# this gives me a total of 1493 companies
number_observations_variables<- unconsolidated_accounts %>% 
  #filter(year<= 2018 & year>=1995) %>% 
  group_by(bvd_id_number,year) %>% 
  filter(ebitda == max(ebitda)) %>% # I have notices some discrepancies when multiple ebitda are filed each year. I select the largest one
  select(bvd_id_number, year, operating_revenue_turnover_, p_l_before_tax, p_l_after_tax, ebitda) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(bvd_id_number) %>% 
  dplyr::summarize(observations_turnover = sum(!is.na(operating_revenue_turnover_)),
            observations_p_l_b_tax = sum(!is.na(p_l_before_tax)),
            observations_p_l_a_tax = sum(!is.na(p_l_after_tax)),
            observations_ebitda = sum(!is.na(ebitda)))

testing<- unconsolidated_accounts %>% 
  filter(bvd_id_number=="IT00811720580") %>% select(year, operating_revenue_turnover_,p_l_before_tax, p_l_after_tax, ebitda)

# Create a panel data
year<- seq(1990, 2020) # I am including all the data between 1995 and 2019 but no strong reasons to to this 
year <- rep(1990:2020, each = 1493) # this is the number of companies that I have 
bvd_id_number <- unique(unconsolidated_accounts$bvd_id_number) # This gives you the unique companies
bvd_id_number<-(rep(bvd_id_number, each = 31)) # this repeats times 25, the number of years 
merged_data <- expand.grid(year = unique(year), bvd_id_number = unique(bvd_id_number))# this creates a grid
merged_data <- merged_data[order(merged_data$bvd_id_number, merged_data$year), ]
# I am now joining this new grid with the data that I have
full_panel_suppliers_unconsolidated<- left_join(merged_data, unconsolidated_accounts)


# Create change variables -------------------------------------------------

full_panel_suppliers_unconsolidated <- full_panel_suppliers_unconsolidated %>% 
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
full_panel_suppliers_unconsolidated<- full_panel_suppliers_unconsolidated %>% 
  group_by(bvd_id_number) %>% 
  fill(first_order, first_order_amount, first_order_tech, total_orders_amount, total_orders, last_order,
       registration_year, registration_first_order, first_year, last_year)

# The issue is that the fill function only works when you have data prior to NAs. If the first year is NA, you might have some issues 
# This script should address the issue 

full_panel_suppliers_unconsolidated <- full_panel_suppliers_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(first_order = ifelse(!is.na(first_order), first_order, last(na.omit(first_order))),
         first_order_amount = ifelse(!is.na(first_order_amount), first_order_amount, last(na.omit(first_order_amount))),
         first_order_tech = ifelse(!is.na(first_order_tech), first_order_tech, last(na.omit(first_order_tech))),
         total_orders_amount = ifelse(!is.na(total_orders_amount), total_orders_amount, last(na.omit(total_orders_amount))),
         total_orders = ifelse(!is.na(total_orders), total_orders, last(na.omit(total_orders))),
         last_order = ifelse(!is.na(last_order), last_order, last(na.omit(last_order))),
         registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         registration_first_order = ifelse(!is.na(registration_first_order), registration_first_order, last(na.omit(registration_first_order))),
         year_after_order = ifelse(!is.na(year_after_order), year_after_order, last(na.omit(year_after_order))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year))),
         year_before_order = ifelse(!is.na(year_before_order), year_before_order, last(na.omit(year_before_order)))) %>%
  ungroup()



list_full_panel_suppliers_unconsolidated <-full_panel_suppliers_unconsolidated %>% select(bvd_id_number) %>% distinct()
write.csv(list_full_panel_suppliers_unconsolidated, here("Analysis","data_proc", "bvd_incorporation_lookup.csv"), row.names = F)


##### Incorporation ------------------------------------------------------

incorporation_date<-  read_excel(here("Analysis", "data_raw","Export 11_07_2023 13_43_suppliers_incorporation_date.xlsx"),  sheet = "Results", 
                                 col_types = c("text","text", "date", "text", "text", "numeric"))

incorporation_date<- incorporation_date %>% select(-'Column1',-"Company name Latin alphabet",-'Branch indicator')
incorporation_date<- clean_names(incorporation_date)
incorporation_date<- incorporation_date %>% rename(bvd_id_number = bv_d_id_number)
incorporation_date <- incorporation_date %>% mutate(incorporation_year = year(date_of_incorporation)) %>% select(-date_of_incorporation)

full_panel_suppliers_unconsolidated <- left_join(full_panel_suppliers_unconsolidated, incorporation_date)

full_panel_suppliers_unconsolidated <- full_panel_suppliers_unconsolidated %>%
  group_by(bvd_id_number) %>%
  mutate(exit_year = ifelse(year > last_avail_year & last_avail_year < 2018, 1, 0),
         entry_year = ifelse(year >= incorporation_year | year >= registration_year, 1, 0),
         entry_year = ifelse(year < incorporation_year, 0, entry_year)) %>%
  ungroup()

full_panel_suppliers_unconsolidated_nona<-left_join(full_panel_suppliers_unconsolidated, number_observations_variables)
full_panel_suppliers_unconsolidated_nona<- full_panel_suppliers_unconsolidated_nona %>% filter(year>1994)

full_panel_suppliers_unconsolidated_nona<- full_panel_suppliers_unconsolidated_nona %>% 
  filter(!is.na(observations_turnover) & !is.na(observations_ebitda) & !is.na(observations_p_l_a_tax) & !is.na(observations_p_l_b_tax))

number_supplier_companies_matched<- full_panel_suppliers_unconsolidated_nona %>% select(bvd_id_number) %>% distinct()

number_exited_companies <- full_panel_suppliers_unconsolidated_nona %>% 
  filter(exit_year==1) %>% select(bvd_id_number) %>% distinct()




full_panel_suppliers_unconsolidated_20_all_outcomes_nona<-full_panel_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover>20 & observations_ebitda >20 & observations_p_l_b_tax >20 & observations_p_l_a_tax >20)



full_panel_suppliers_unconsolidated_25_all_outcomes_nona<-full_panel_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover==25 & observations_ebitda ==25 & observations_p_l_b_tax==25 & observations_p_l_a_tax ==25)
suppliers_full_data_25<- full_panel_suppliers_unconsolidated_25_all_outcomes_nona %>% select(bvd_id_number) %>% distinct()

full_panel_suppliers_unconsolidated_24_all_outcomes_nona<- full_panel_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover==24 & observations_ebitda ==24 & observations_p_l_b_tax>=24 & observations_p_l_a_tax >=24)
suppliers_full_data_24<- full_panel_suppliers_unconsolidated_24_all_outcomes_nona %>% select(bvd_id_number) %>% distinct()


full_panel_suppliers_unconsolidated_23_all_outcomes_nona<- full_panel_suppliers_unconsolidated_nona %>% 
  filter(observations_turnover>=23 & observations_ebitda >=23 & observations_p_l_b_tax>=23 & observations_p_l_a_tax >=23)

suppliers_full_data_23<- full_panel_suppliers_unconsolidated_23_all_outcomes_nona %>% select(bvd_id_number) %>% distinct()

# exited_companies<- full_panel_suppliers_unconsolidated_nona %>% 
#   filter(exit_year ==1) %>% group_by(bvd_id_number) %>% 
#   mutate(number = n()) %>% 
#   select(bvd_id_number) %>% 
#   distinct()
# 
# exited_companies_list <- unique(exited_companies$bvd_id_number)
# exited_companies<- full_panel_suppliers_unconsolidated_nona %>% 
#   filter(bvd_id_number %in% exited_companies_list)
# 
# entered_companies<- full_panel_suppliers_unconsolidated %>% 
#   filter(entry_year==0)  
# enter_companies_list <- unique(entered_companies$bvd_id_number)
# entered_companies<- full_panel_suppliers_unconsolidated_nona %>% 
#   filter(bvd_id_number %in% enter_companies_list)
# 
# entered_exited_companies <- entered_companies %>% filter(exit_year==1) %>% 
#   select(bvd_id_number) %>% distinct()
# 
# select(bvd_id_number) %>% 
#   distinct()
# 
# 
# summary_exited_entered <- full_panel_suppliers_unconsolidated %>% 
#   group_by(bvd_id_number) %>% 
#   summarize(years_exited = sum(exit_year), 
#             years_entered = sum(entry_year)) %>% ungroup()
# 

# -------------------------------------------------------------------------

#  
patent_bvd_lookup <- full_panel_suppliers_unconsolidated_nona %>% 
  select(bvd_id_number) %>% distinct()
# Determine the total number of rows in the data
total_rows <- nrow(patent_bvd_lookup)

# Set the desired subset size
subset_size <- 50

# Calculate the number of subsets needed
num_subsets <- ceiling(total_rows / subset_size)

# Loop over subsets and write to separate CSV files
for (i in 1:num_subsets) {
  # Calculate the start and end row indices for the subset
  start_row <- (i - 1) * subset_size + 1
  end_row <- min(i * subset_size, total_rows)
  
  # Extract the subset of rows
  subset <- patent_bvd_lookup[start_row:end_row, ]
  
  # Generate the filename for the subset
  filename <- paste0("patent_lookup_subset", i, ".csv")
  
  # Write the subset to CSV
  write.csv(subset, here("data_proc", filename), row.names = FALSE)
}

patent_bvd_lookup <- full_panel_suppliers_unconsolidated %>% 
  select(bvd_id_number) %>% distinct()

patent_bvd_lookup_subset_1<- patent_bvd_lookup[1:160,]
patent_bvd_lookup_subset_2<- patent_bvd_lookup[161:320,]
patent_bvd_lookup_subset_3<- patent_bvd_lookup[601:900,]
patent_bvd_lookup_subset_4<- patent_bvd_lookup[901:1276,]



write.csv(patent_bvd_lookup_subset_1, here("data_proc", "patent_lookup_subset_1.csv"), row.names = F)
write.csv(patent_bvd_lookup_subset_2, here("data_proc", "patent_lookup_subset_2.csv"), row.names = F)
write.csv(patent_bvd_lookup_subset_3, here("data_proc", "patent_lookup_subset_3.csv"), row.names = F)
write.csv(patent_bvd_lookup_subset_4, here("data_proc", "patent_lookup_subset_4.csv"), row.names = F)


check<- full_panel_suppliers_unconsolidated %>% 
  select(bvd_id_number, year, ebitda, ebitda_rate_change, p_l_after_tax, p_l_after_tax_rate_change, operating_revenue_turnover_,
         operating_revenue_turnover_rate_change, p_l_before_tax, p_l_before_tax_rate_change)




# Create change variables -------------------------------------------------

full_panel_suppliers_unconsolidated_nona<-full_panel_suppliers_unconsolidated_nona %>% 
  group_by(bvd_id_number) %>%
  arrange(year) %>% 
  mutate(operating_revenue_turnover_change = (operating_revenue_turnover_ - lag(operating_revenue_turnover_)) / lag(operating_revenue_turnover_),
         ebitda_change = (ebitda - lag(ebitda)) / lag(ebitda),
         p_l_before_tax_change = (p_l_before_tax - lag(p_l_before_tax)) / lag(p_l_before_tax),
         p_l_after_tax_change = (p_l_after_tax - lag(p_l_after_tax))/lag(p_l_after_tax))

# Merge with patent data --------------------------------------------------

patent_data_suppliers<- read_csv(here("Analysis","data_proc", "patents_suppliers_1995_2019.csv"))
full_panel_suppliers<- left_join(full_panel_suppliers_unconsolidated_nona, patent_data_suppliers)
full_panel_suppliers <- full_panel_suppliers %>%
  mutate(
    num_applications = replace(num_applications, is.na(num_applications), 0),
    num_publications = replace(num_publications, is.na(num_publications), 0),
    cumulative_patents = replace(cumulative_patents, is.na(cumulative_patents), 0),
    lag_cum_applications = replace(lag_cum_applications, is.na(lag_cum_applications), 0),
    stock_patent = replace(stock_patent, is.na(stock_patent), 0),
    lag_patent_stock = replace(lag_patent_stock, is.na(lag_patent_stock), 0),
    patent_stock = replace(patent_stock, is.na(patent_stock), 0)
  )

full_panel_suppliers<- full_panel_suppliers %>%
  select(-year_orbis, -matching_year, -original_currency, -exchange_rate_from_original_curr, -max_tech) %>% 
  group_by(bvd_id_number) %>%
  mutate(first_order = ifelse(!is.na(first_order), first_order, last(na.omit(first_order))),
         code_2_digits = ifelse(!is.na(code_2_digits), code_2_digits, last(na.omit(code_2_digits))),
         first_order_amount = ifelse(!is.na(first_order_amount), first_order_amount, last(na.omit(first_order_amount))),
         first_order_tech = ifelse(!is.na(first_order_tech), first_order_tech, last(na.omit(first_order_tech))),
         total_orders_amount = ifelse(!is.na(total_orders_amount), total_orders_amount, last(na.omit(total_orders_amount))),
         total_orders = ifelse(!is.na(total_orders), total_orders, last(na.omit(total_orders))),
         last_order = ifelse(!is.na(last_order), last_order, last(na.omit(last_order))),
         registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         registration_first_order = ifelse(!is.na(registration_first_order), registration_first_order, last(na.omit(registration_first_order))),
         year_after_order = ifelse(!is.na(year_after_order), year_after_order, last(na.omit(year_after_order))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year))),
         country = ifelse(!is.na(country), country, last(na.omit(country))) ,
         year_before_order = ifelse(!is.na(year_before_order), year_before_order, last(na.omit(year_before_order)))) %>%
  ungroup()

full_panel_suppliers<- full_panel_suppliers %>% distinct()
full_panel_suppliers<- full_panel_suppliers %>% mutate(supplier_status =1)
full_panel_suppliers<- full_panel_suppliers %>% select(-'...1', -month_orbis, -closing_date, -company_name) %>% distinct()
testing<- full_panel_suppliers %>% filter(bvd_id_number == "FR453618779")

write.csv(full_panel_suppliers, here("Analysis","data_proc", "full_panel_suppliers.csv"))

# Size Nace and postcode --------------------------------------------------


#  I have used this to do the matching on Orbis
bvd_lookup_size <- full_panel_suppliers %>% select(bvd_id_number) %>% distinct()
bvd_lookup_size_1<-bvd_lookup_size[1:999,]
bvd_lookup_size_2<-bvd_lookup_size[1000:1481,]
write.csv(bvd_lookup_size_1, here("Analysis", "data_proc", "bvd_id_lookup_size_1.csv"))
write.csv(bvd_lookup_size_2, here("Analysis", "data_proc", "bvd_id_lookup_size_2.csv"))


# load the files 
size_nace_postcode_suppliers_1 <- read_excel("Analysis/data_proc/size_nace_postcode_suppliers_1.xlsx", sheet = "Results")
size_nace_postcode_suppliers_2 <- read_excel("Analysis/data_proc/size_nace_postcode_suppliers_2.xlsx", sheet = "Results")
# putting the data together 
size_nace_postcode_suppliers <- rbind(size_nace_postcode_suppliers_1,size_nace_postcode_suppliers_2)
size_nace_postcode_suppliers<- size_nace_postcode_suppliers %>% 
  select(-'...1') %>% 
  rename(company_name ='Company name Latin alphabet',bvd_id_number = 'BvD ID number',
         nace_main_section = 'NACE Rev. 2 main section', size_classification = 'Size classification',
         nace_4_digits = 'NACE Rev. 2, core code (4 digits)', bvd_sectors = 'BvD sectors', 
         postcode ='Postcode\nLatin Alphabet')

full_panel_suppliers<- left_join(full_panel_suppliers, size_nace_postcode_suppliers)

# this does not work 

full_panel_suppliers<- full_panel_suppliers %>%
  select(-date_closing, -consolidation_l, -closing_date_format) %>% 
  group_by(bvd_id_number) %>%
  mutate(first_order = ifelse(!is.na(first_order), first_order, last(na.omit(first_order))),
         first_order_amount = ifelse(!is.na(first_order_amount), first_order_amount, last(na.omit(first_order_amount))),
         first_order_tech = ifelse(!is.na(first_order_tech), first_order_tech, last(na.omit(first_order_tech))),
         total_orders_amount = ifelse(!is.na(total_orders_amount), total_orders_amount, last(na.omit(total_orders_amount))),
         total_orders = ifelse(!is.na(total_orders), total_orders, last(na.omit(total_orders))),
         last_order = ifelse(!is.na(last_order), last_order, last(na.omit(last_order))),
         registration_year = ifelse(!is.na(registration_year), registration_year, last(na.omit(registration_year))),
         registration_first_order = ifelse(!is.na(registration_first_order), registration_first_order, last(na.omit(registration_first_order))),
         year_after_order = ifelse(!is.na(year_after_order), year_after_order, last(na.omit(year_after_order))),
         first_year = ifelse(!is.na(first_year), first_year, last(na.omit(first_year))),
         last_year = ifelse(!is.na(last_year), last_year, last(na.omit(last_year))),
         country = ifelse(!is.na(country), country, last(na.omit(country))) ,
         year_before_order = ifelse(!is.na(year_before_order), year_before_order, last(na.omit(year_before_order)))) %>%
  ungroup()


# What I do, I select all these variables
full_panel_suppliers_selected_vars <- full_panel_suppliers %>% 
  select(bvd_id_number, first_order, first_order_amount, first_order_tech, total_orders_amount, total_orders, 
         last_order, registration_year, registration_first_order, year_after_order, last_year, country, year_before_order) %>% 
  distinct() %>% 
  filter(!is.na(first_order)) %>% distinct()


full_panel_suppliers<- full_panel_suppliers %>% 
  select(-first_order, -first_order_amount, -first_order_tech, -total_orders_amount, -total_orders, 
         -last_order, -registration_year, -registration_first_order, -year_after_order, -last_year, -country, -year_before_order)

full_panel_suppliers <- left_join(full_panel_suppliers, full_panel_suppliers_selected_vars)
full_panel_suppliers<- full_panel_suppliers %>% filter(!is.na(first_order))



check<- full_panel_suppliers %>% filter(is.na(first_order)) %>% select(bvd_id_number) %>% distinct()

number_suppliers<- unique(full_panel_suppliers$bvd_id_number
                          )

write.csv(full_panel_suppliers, here("Analysis","data_proc", "full_panel_suppliers.csv"))

#exited_firms
#missing_years<-  unconsolidated_accounts %>%
  #group_by(bvd_id_number) %>%
  #filter(year_orbis > 1995 & year_orbis < 2020) %>%
  #drop_na(operating_revenue_turnover_) %>% 
  #summarize(
    #missing_years_list = toString(setdiff(min(year_orbis):max(year_orbis), year_orbis)),
    #missing_years = length(setdiff(min(year_orbis):max(year_orbis), year_orbis)),
    #first_year = min(year_orbis),
    #last_year = max(year_orbis),
    #first_order = first_order,
    #order_after_last_orbis = last_year>first_order,
   # number_observations = n(), 
  #) %>% distinct()



#disappearing_companies<- unconsolidated_accounts %>%
 # group_by(bvd_id_number) %>%
  #filter(year_orbis > 1993 & year_orbis < 2020) %>%
  #filter(max(year_orbis)>first_order) %>% 
  #filter(max(year_orbis)<2019) %>% 
 # drop_na(operating_revenue_turnover_) %>% 
  #summarize(
   # missing_years_list = toString(setdiff(min(year_orbis):max(year_orbis), year_orbis)),
    #missing_years = length(setdiff(min(year_orbis):max(year_orbis), year_orbis)),
    #first_year = min(year_orbis),
    #last_year = max(year_orbis),
    #first_order = first_order,
    #order_after_last_orbis = last_year<first_order,
    #number_observations = n(), 
    #year_after_order = max(year_orbis - first_order),
    #year_before_order = max(first_order - year_orbis))%>% ungroup() %>% 
  #distinct()


#year_orbis<- seq(1995, 2019)
#year_orbis <- rep(1995:2019, each = 591)
#bvd_id_number <- unique(supplier_potential_matched_no_outliers_no_city_1995$bvd_id_number)
#bvd_id_number<-(rep(bvd_id_number, each = 25))
#merged_data <- expand.grid(year_orbis = unique(year_orbis), bvd_id_number = unique(bvd_id_number))
#merged_data <- merged_data[order(merged_data$bvd_id_number, merged_data$year_orbis), ]


#testing_2<- disappearing_companies %>% filter(year_before_order>2) %>% 
 # filter(first_year<1996, missing_years<6)
#no_missing_years<- missing_years %>% filter(missing_years<2)
#testing_last_orbis <- missing_years %>% filter(order_after_last_orbis==TRUE)

#testing<- unconsolidated_accounts %>% select(bvd_id_number, matching_year, first_order, year_after_order, year_before_order, year_orbis) %>% 
 # group_by(bvd_id_number) %>% mutate(number_observations = n(),
                                     #first_orbis =min(year_orbis),last_orbis = max(year_orbis)) %>% ungroup() %>% distinct()

#testing<- testing %>% filter(year_before_order>2)
#testing<- testing %>% filter(year_after_order>2)

t

#testing_1995<- testing %>% filter(first_orbis<1995)
#testing_1995_2020<- testing_1995 %>% filter(last_orbis>2015)
#number_companies<- unique(testing$bvd_id_number
                          )




#testing_1995_number_of_firms <- unique(testing_1995$bvd_id_number)
#testing_1995_2020_number_of_firms <- unique(testing_1995_2020$bvd_id_number)
#unconsolidated_accounts %>% unconsolidated_accounts %>% 
#mutate(years_after_order = total_chf_amount_year[which.min(order_date)],
 #      first_order_tech = max_tech[which.min(order_date)])

# Print the updated data tibble
#print(data)


#consolidated_accounts_over_time <- cern_orbis_matched_suppliers_vars %>% ungroup() %>% select(year_orbis, consolidation_l)  %>% 
 # group_by(year_orbis, consolidation_l) %>% 
  #mutate(number_consolidations = n()) %>% 
  #ungroup() %>% 
  #distinct() %>% 
  #group_by(year_orbis) %>% 
  #mutate(total_year = sum(number_consolidations)) %>% 
  #ungroup() %>% 
  #distinct()


#consolidated_accounts_over_time <- cern_orbis_matched_suppliers_vars %>%ungroup() %>%  select(year_orbis, consolidation_l)  %>% 
 # group_by(year_orbis, consolidation_l) %>% 
  #mutate(number_consolidations = n()) %>% 
  #ungroup() %>% 
  #distinct() %>% 
  #group_by(year_orbis) %>% 
  #mutate(total_year = sum(number_consolidations)) %>% 
  #ungroup() %>% 
  #distinct()


#consolidated_accounts_firms <- cern_orbis_matched_suppliers_vars %>%ungroup() %>%  select(bvd_id_number, consolidation_l)  %>% 
 # group_by(bvd_id_number, consolidation_l) %>% 
#  mutate(number_consolidations = n()) %>% 
 # ungroup() 

#consolidated_accounts_firms<- as.data.frame(consolidated_accounts_firms)
#consolidated_accounts_firms<- consolidated_accounts_firms %>% distinct()
#consolidated_accounts_firms_wide <- consolidated_accounts_firms %>% 
  #pivot_wider(names_from = consolidation_l, values_from = number_consolidations, values_fill = 0)

#consolidated_accounts_firms <- cern_orbis_matched_suppliers_vars%>%
 # select(bvd_id_number, consolidation_l, year_orbis) %>% 
  #group_by(bvd_id_number, year_orbis) %>%
  #summarise(num_account_types = n_distinct(consolidation_l))

#record_counts <- cern_orbis_matched_suppliers_vars %>% group_by(bvd_id_number, year_orbis) %>%
#summarise(has_U = any(consolidation_l == "U"),
 #         has_C = any(consolidation_l == "C"))


#data <- data %>%
 # arrange(bvd_id_number, year_orbis)

# Create a new variable to indicate priority based on account type
#record_counts<- record_counts %>%
  #group_by(bvd_id_number) %>%
  #mutate(priority = ifelse(all(has_U) & !all(has_C), "U", 
   #                        ifelse(all(has_C) & !all(has_U), "C", ""))) %>%
 # ungroup()

#check<- record_counts %>% filter(priority != "")
#number_companies <- unique(record_counts$bvd_id_number
                           )

# Filter the duplicates based on priority
#filtered_data <- data %>%
  #group_by(bvd_id_number) %>%
  #filter(n() == 1 | priority == first(priority)) %>%
 # ungroup()

# Print the filtered data
#print(filtered_data)

#cern_orbis_matched_suppliers_vars <- cern_orbis_matched_suppliers_vars %>%
 # arrange(bvd_id_number, year_orbis)

# Group the data by bvd_id_number and check for transitions between account types
#account_transitions <- cern_orbis_matched_suppliers_vars %>%
#  group_by(bvd_id_number) %>%
#  mutate(previous_account = lag(consolidation_l),
  #       account_transition = if_else(is.na(previous_account), NA, previous_account != consolidation_l)) %>%
 # ungroup() %>%
#  filter(!is.na(account_transition)) %>%
 # filter(previous_account %in% c("C", "U"), consolidation_l %in% c("C", "U"))


#companies_with_varying_records <- record_counts%>%
#  group_by(bvd_id_number) %>%
 # summarise(unique_record_counts = n_distinct(num_records))

# Print the companies with varying record counts
#print(companies_with_varying_records)



#group_by(year_orbis) %>% 
 # mutate(total_year = sum(number_consolidations)) %>% 
#  ungroup() %>% 
 # distinct()


#cern_orbis_matched_missing_na<- cern_orbis_matched_suppliers_filings %>% 
 # filter(is.na(total_orders_amount)) %>%  
#  select(bvd_id_number) %>% 
 # distinct()


#ggplot(consolidated_accounts_over_time, aes(x = year_orbis, y = number_consolidations/total_year,color = consolidation_l)) +
  #geom_line() +
  #labs(title = "Number of Consolidations over Time by Account Type",
       x = "Year",
 #    y = "Share of Consolidations x",
#      color = "Account Type")


#cern_orbis_nodp_unique_filing<- cern_orbis_nodp %>% filter(number_of_filings==1 | consolidation_l=="C") %>% select(-number_of_filings)

#consolidated_accounts_over_time <- cern_orbis_matched_suppliers_vars

#number_observations <- cern_orbis_matched_suppliers_filings %>% group_by(bvd_id_number) %>% count()
#number_observations_20<- number_observations %>% filter(n>20) %>% distinct()
#number_observations_15<- number_observations %>% filter(n>15) %>% distinct()
#number_observations_10<- number_observations %>% filter(n>10) %>% distinct()
#number_observations <- cern_orbis_matched_suppliers_check %>% group_by(bvd_id_number) %>% count()



#all_orders_95_21_tech_ms_selected<- left_join(all_orders_95_21_tech_ms_selected, bvd_id_lookup) %>% distinct()
#bvd_final_file <- unique(cern_orbis_nodp_unique_filing_4$bvd_id_number)# this should go later 
#all_orders_95_21_orbis_matched<- all_orders_95_21_tech_ms_selected %>% filter(bvd_id_number %in% bvd_final_file)



