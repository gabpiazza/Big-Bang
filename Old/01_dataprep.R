# Info --------------------------------------------------------------------
##
##Script name: 01_dataprep
##
##Purpose of script: Preparing the procurement data for analaysis##
##Author: Gabriele Piazza
##
##Date Created: 2023-04-19
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes: I am missign how soem of these datasets were constructed. For example, how the matchign to orbis was done. Others might ask questions on this. 
##   
##
# 1. Install & Load packages --------------------------------------------------------

##testing 

install.packages(c("terra", "spData",  "tmap", "leaflet","rnaturalearth",
                   "rnaturalearthdata", "hrbrthemes", "viridis", "ggpubr", 
                   "rstatix"))
library(tidyverse)
library(rstatix)
library(ggpubr)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(janitor)
library(tmap)    # for static and interactive maps
library(leaflet)
library(sf)
library(terra)
library(here)
library(dplyr)
library(spData)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(tabulator)
library(Hmisc)
library(skimr)
library(tjbal)
library(fuzzyjoin)
library (tidyverse)
library(janitor)
library(easycsv)
library(Synth)
library(readxl)
library(plm)
library(stringi)
if(!require(SCtools)) devtools::install_github("bcastanho/SCtools")
library(SCtools)
library(tidysynth)
library(modelsummary)
library(panelView)
library(eeptools)
library(here)
library (haven)
library(sp)
library(spdep)
library(tidyverse)
library(sf)
library(tmap)
options(scipen=999) # to get rid of scientific notation

# 2. Load data -----------------------------------------------------------

# orders_2014_2021: all the orders from 2014 to 2021 - given to me from cERN
# orders_pre_2014: all the orders prior to 2014, given to me by CSIL
# all orders: All the orders between 1995 and 2021 as in the orignal dataset

# all_orders_95_21_orbis_matched: all the orders for the suppliers that I was able to match to the Orbis database with at least 4 years worth of data
# before and after treatment 
# CERN_orders_techlevel: tech intensity of the orders  - lookup given by CSIL
# balance_MS: industrial returns for member states (MS)

setwd(here())# this sets the directory as the current folder
## All the orders ----------------------------------------------------------
# this is the data that I received from CERN: all the orders from 2014 to 2021

orders_2014_2021<- read_excel(here("data_raw","2021-06-29 - CERN Orders 2014-2021_clean.xlsx")) 

# this is the data that I received from CSIL, before 2014
orders_pre_2014<- read_excel(here("data_raw","21_10_27_Suppliers_cern_2016_nocontacts.xlsx"))

#drop a few variables before binding
orders_pre_2014<- orders_pre_2014 %>% select(-`SUBPROJECT 1`, -CONTACT) %>% rename("CHF_AMOUNT"="SUM(CHF_AMOUNT)")# not sure why I am excluding these variables

#bind the two datasets togetehr
all_orders<-rbind(orders_2014_2021, orders_pre_2014)# putting together the two datasets for the orders
all_orders<-all_orders %>% distinct_all() #remove duplicates
all_orders<-clean_names(all_orders) #clean the names of the variables using janitor
all_orders$order_date<-as.numeric(all_orders$order_date) # change the date to numeric

write_csv(all_orders, here("data_proc", "all_orders.csv"))

# potential data
potential_suppliers_registration_year<- read_csv(here("data_raw","22_03_08_potential_suppliers_procurement.csv"))
potential_suppliers_registration_year<- clean_names(potential_suppliers_registration_year)
potential_suppliers_registration_year<- potential_suppliers_registration_year %>% select(suppliername, city, country, year_supplier_registration, x1_digit, x2_digit)
potential_suppliers_registration_year<- potential_suppliers_registration_year %>% 
  rename(company_name = suppliername,code_1_digit = x1_digit, code_2_digits = x2_digit)

write.csv(all_orders, here("data_proc", "potential_supplier_registration_year"))


# # Matched orders --------------------------------------------------------
matched_orders<- read_csv(here("data_proc", "matched_orbis_firms_orders.csv")) # these are all the orders that I was able to match 



## Unmatched orders --------------------------------------------------------
unmatched_orders <- read_csv(here("data_proc", "all_orders_unmatched.csv"))# these are all the orders that I was not able to match to Orbis


## Tech Lookup -------------------------------------------------------------

CERN_orders_techlevel <- read_excel(here("data_raw","CERN_techlevel.xlsx")) # load the lookup that CSIL provided
CERN_orders_techlevel<-clean_names(CERN_orders_techlevel) # clean the names
CERN_orders_techlevel<-CERN_orders_techlevel %>% rename(purchase_code=x3_digits )# columns have different names
tech_level<-CERN_orders_techlevel %>% select(purchase_code,tech_intensity) # don't need the other columns so  am dropping them 


## Country coefficient ---------------------------------------------------

balance_MS <- read_csv(here("data_raw","balance_MS.csv")) # this provides information on the whether a country is well-balanced or not. It has data aup to 2016
balance_MS <- clean_names(balance_MS) # this is just to make th names of the variables consistent 
balance_MS <- balance_MS %>% rename(country=iso_code,order_date=year)# renaming

## Suppliers registration ------------------------------------------------

suppliers_registration_year <- read_csv(here("data_raw","suppliers_registration_year.csv"))# this has information for when firms registered. Does this refer to firmstht became supplier or all of them 
# registered but it is not for the full sample
suppliers_registration_year <- clean_names(suppliers_registration_year)


### Matched data ---------------------------------------------------------
### So this should be all the data: how did I match these individually

# I need to investigate how I put this data together - I have to find the script that I have used to put the data together 

cern_orbis_nodp_unique_filing_4<- read_csv(here("data_proc", "cern_orbis_4_years.csv")) # This is all the data for the suppliers for which I have data from orbis
all_pot_suppliers_register_unique_filing_4<- read_csv(here("data_proc", "potential_4_years_clean.csv"))# What is this data? What is the potential suppliers?

## NUTS3 shape file -----------------------------------------------------------------------
#NUTS3_regions <- st_read("/Users/gabrielepiazza/Dropbox/PhD/procurement_cern/data/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp")
#NUTS3_regions <- clean_names(NUTS3_regions) # not necessary for now


# 3. Cleaning and preparing the data for descriptives --------------------------------------


# # Merge the all suppliers dataset with tech and ms lookup ---------------

all_suppliers_95_21_tech<- left_join(all_orders,tech_level) %>% distinct() # remove the duplicates
all_suppliers_95_21_tech<- left_join(all_suppliers_95_21_tech, suppliers_registration_year) # I get the registration year for the matched supplier
all_suppliers_95_21_tech_ms<- left_join(all_suppliers_95_21_tech, balance_MS) #  this  matches  to the coefficients

### This is redundant
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
all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SEF - SOCIETE ETUDE & FABRICATION POUR RECHERCHE & INDUSTRIE"]<-'SEF - SOCIETE ETUDE & FABRICATION'
all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SEF STE D'ETUDES ET FABRICATIONS"]<-'SEF - SOCIETE ETUDE & FABRICATION'
all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SOLYRO - SOCIETE LYONNAISE DE ROBINETTERIE"]<-'SOLYRO'
all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=="SCHNEIDER ELECTRIC (FRANCE) (CRITICAL POWER & COOLING SERV.)"]<-'SCHNEIDER ELECTRIC (FRANCE)'
all_suppliers_95_21_tech_ms$company_name[all_suppliers_95_21_tech_ms$company_name=='ELECTRO - MECHANIC EQUIPEMENT (Alias: E.M.E.)']<-'ELECTRO - MECHANIC EQUIPEMENT'


## Contract size and and tech level variables ----------------------------

all_suppliers_95_21_tech_ms<- all_suppliers_95_21_tech_ms %>% # assign the contract size, this is a bit arbitrary and based on CERN procurement rules, 
  mutate(contract_size= case_when(chf_amount<=100000 ~"small", 
                                  chf_amount>100000 & chf_amount<=200000~"medium",
                                  chf_amount>200000 ~"large"))

high_tech_order <- c(3,4,5,7)# I use the information provided by CSIL
low_tech_order<- c(1,2,6) #
#all_suppliers_95_21_tech_ms<- all_suppliers_95_21_tech_ms %>% 
  #mutate(technology_level = case_when(tech_intensity %in% high_tech_order ~"high-tech", 
                                      #tech_intensity %in% low_tech_order ~ "low-tech"))

#all_orders_95_21_orbis_matched<- all_orders_95_21_orbis_matched %>% 
#mutate(technology_level = case_when(tech_intensity %in% high_tech_order ~"high-tech", 
#tech_intensity %in% low_tech_order ~ "low-tech"))

#all_orders_95_21_orbis_matched<- all_orders_95_21_orbis_matched %>% 
# mutate(contract_size= case_when(chf_amount<=100000 ~"small", 
#chf_amount>100000 & chf_amount<=200000~"medium",
# chf_amount>200000 ~"large"))

### Unmatched suppliers  - Thesese are the orders, whose firm I was not able to find on orbis
### Contract size
unmatched_orders<- unmatched_orders %>% 
  mutate(contract_size= case_when(chf_amount<=100000 ~"small", 
                                  chf_amount>100000 & chf_amount<=200000~"medium",
                                  chf_amount>200000 ~"large"))

unmatched_orders<- unmatched_orders %>% 
  mutate(technology_level = case_when(tech_intensity %in% high_tech_order ~"high-tech", 
                                      tech_intensity %in% low_tech_order ~ "low-tech"))

matched_orders<-matched_orders %>% 
  mutate(contract_size= case_when(chf_amount<=100000 ~"small", 
                                  chf_amount>100000 & chf_amount<=200000~"medium",
                                  chf_amount>200000 ~"large"))

matched_orders<- matched_orders %>% 
  mutate(technology_level = case_when(tech_intensity %in% high_tech_order ~"high-tech", 
                                      tech_intensity %in% low_tech_order ~ "low-tech"))


write.csv(matched_orders, here("data_proc", "matched_orders.csv"))
write.csv(unmatched_orders, here("data_proc", "unmatched_orders.csv"))

# ## Cleaning the matched datasets ----------------------------------------
matched_suppliers_orbis<- cern_orbis_nodp_unique_filing_4 %>% # this is the actual suppliers and I select only the relevant variables
  select(bvd_id_number, code_1_digit, registration_year,status_supplier,  two_digit_nace, year_orbis, 
         first_order, first_order_amount, total_orders_amount, well_balanced, balance_over_time, operating_revenue_turnover_,
         fixed_assets, current_assets, ebitda, total_assets, intangible_fixed_assets, p_l_after_tax, p_l_before_tax, country, city) %>% distinct() # I select only the variables that I need


matched_potential_suppliers<- all_pot_suppliers_register_unique_filing_4 %>%
  rename(year_supplier_registration = registration_year) %>% # this is the potnetial suppliers
  mutate(first_order_amount=0,  total_orders_amount= 0, status_supplier = 0) %>% # I assign 0  to order amount, total order and stuatus supplier
  rename(registration_year = year_supplier_registration) %>% # this is done before merging the two datasets
  select(bvd_id_number, code_1_digit, country, city, registration_year,status_supplier,  two_digit_nace, year_orbis, # I select the variables of interest
         first_order, first_order_amount, total_orders_amount, well_balanced, balance_over_time, operating_revenue_turnover_,
         fixed_assets, current_assets, ebitda, total_assets, intangible_fixed_assets, p_l_after_tax, p_l_before_tax)


supplier_potential_matched <- rbind(matched_suppliers_orbis, matched_potential_suppliers)# this puts the data together
# I am excluding the outliers following the paper by Kalemli-Ozcan
filtered_data <-  supplier_potential_matched[ supplier_potential_matched$operating_revenue_turnover_ < quantile(supplier_potential_matched$operating_revenue_turnover_, 0.99),] 
filtered_companies <- unique(filtered_data$bvd_id_number)
supplier_potential_matched<- supplier_potential_matched %>% filter(bvd_id_number %in% filtered_companies)

# cleaning steps 

# I dropped the companies with missing data on total assets and operating revenue


# 4. Descriptive statistics  ----------------------------------------------------


## Summary statistics ------------------------------------------------------


### Basic summary statistics for all orders  --------------------------------

summary_statistics_all_orders <- summary(all_orders$chf_amount)
summary_statistics_all_orders <- as.vector(summary_statistics_all_orders)

### Basic summary statistics for matched orders  --------------------------------

summary_statistics_matched_suppliers<- matched_suppliers_orders %>% select(-city) %>% distinct()
summary_statistics_matched_suppliers_table <- summary (matched_suppliers_orders$chf_amount) # this is not working for some reason
summary_statistics_matched_suppliers_table<- as.vector(summary_statistics_matched_suppliers_table) # this does not work for some reason

### Basic summary statistics for unmatched orders  --------------------------------
summary_statistics_unmatched_suppliers <-unmatched_suppliers %>% select(-city) %>% distinct()
summary_statistics_unmatched_suppliers_table<-summary(summary_statistics_unmatched_suppliers$chf_amount)
summary_statistics_unmatched_suppliers_table <- as.vector(summary_statistics_unmatched_suppliers_table)


### Putting the statistics together -----------------------------------------


#### Putting the two summary statistics together for all orders and matched -----------------------------

summary_initial_matched <- rbind(summary_statistics_all_orders, summary_statistics_matched_suppliers)
summary_initial_matched<- as.data.frame(summary_initial_matched)
colnames(summary_initial_matched)<- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
summary_initial_matched
row.names(summary_initial_matched)<- c("All orders", "Orbis-matched orders")

#### Putting the summary statistics for matched and unmatched together -------

summary_matched_unmatched<- rbind(summary_statistics_unmatched_suppliers_table, summary_statistics_matched_suppliers_table)
summary_matched_unmatched<- as.data.frame(summary_matched_unmatched)
colnames(summary_matched_unmatched)<- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
row.names(summary_matched_unmatched)<- c("Unmatched orders", "Orbis-Matched orders")


### K density plot -------------------------------------------------------

#### All orders and matched 
amount_all_orders<- all_orders %>% select(supplier_code, chf_amount) %>% 
  mutate(status = "all suppliers")
amount_matched_orders<- all_orders_95_21_orbis_matched %>% select(supplier_code, chf_amount) %>% 
  mutate(status = "matched")

k_density_data<- rbind(amount_all_orders, amount_matched_orders)

k_density_plot<- ggplot(data=k_density_data, aes(x=log(chf_amount), group=status, fill=status)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_classic()


#### Matched and Unmatched 
amount_matched_suppliers_orders<- matched_suppliers_orders %>% 
  select(supplier_code, chf_amount) %>% 
  mutate(status = "matched suppliers")
amount_unmatched_suppliers<- unmatched_suppliers %>% 
  select(supplier_code, chf_amount) %>% 
  mutate(status = "unmatched suppliers")

k_density_data_matched_unmatched<- rbind(amount_matched_suppliers_orders, amount_unmatched_suppliers)

k_density_plot<- ggplot(data=k_density_data_matched_unmatched, aes(x=log(chf_amount), group=status, fill=status)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_classic()


### T test ---------------------------------------------------------------
##### All orders and matched suppliers
t_results_chf_amount<- t.test(all_orders$chf_amount,all_orders_95_21_orbis_matched$chf_amount)
p_value_amount<- t_results_chf_amount$p.value


high_tech_all_suppliers_amount<- all_suppliers_95_21_tech_ms %>% filter(technology_level =="high-tech") %>% select(chf_amount)
low_tech_all_suppliers_amount <- all_suppliers_95_21_tech_ms %>% filter(technology_level =="low-tech") %>% select(chf_amount)

high_tech_matched_suppliers_amount<- all_orders_95_21_orbis_matched %>% filter(technology_level =="high-tech") %>% select(chf_amount)
low_tech_matched_suppliers_amount <- all_orders_95_21_orbis_matched %>% filter(technology_level =="low-tech") %>% select(chf_amount)

t_results_high_tech <- t.test(high_tech_all_suppliers_amount, high_tech_matched_suppliers_amount)
t_results_low_tech <- t.test(low_tech_all_suppliers_amount, low_tech_matched_suppliers_amount)
p_value_high_tech <- t_results_high_tech$p.value
p_value_low_tech <- t_results_low_tech$p.value



##### Matched and unmatched
t_result_matched_unmatched<- t.test(summary_statistics_matched_suppliers$chf_amount, summary_statistics_unmatched_suppliers$chf_amount)
p_value_matched_unmatched <- t_result_matched_unmatched$p.value

high_tech_matched_amount<- summary_statistics_matched_suppliers %>% filter(technology_level== "high-tech") %>% select(chf_amount)
low_tech_matched_amount <- summary_statistics_unmatched_suppliers %>% filter(technology_level =="low-tech") %>% select(chf_amount)

high_tech_unmatched_amount<- unmatched_suppliers %>% filter(technology_level == "high-tech") %>% select(chf_amount)
low_tech_unmatched_amount <- unmatched_suppliers %>% filter(technology_level == "low-tech") %>% select(chf_amount)

t_high_tech_matched_unmatched <- t.test(high_tech_matched_amount, high_tech_unmatched_amount)
t_low_tech_matched_unmatched <- t.test(low_tech_matched_amount, low_tech_matched_amount)
p_value_high_tech_unmatched_matched <- t_high_tech_matched_unmatched$p.value
p_value_low_tech_unmatched_matched <- t_low_tech_matched_unmatched$p.value

### Create a table -------------------------------------------------------
#### All suppliers and matched
list_all_suppliers<- list(all_orders$chf_amount, high_tech_all_suppliers_amount$chf_amount, low_tech_all_suppliers_amount$chf_amount)
list_matched_suppliers<- list(all_orders_95_21_orbis_matched$chf_amount, high_tech_matched_suppliers_amount$chf_amount, low_tech_matched_suppliers_amount$chf_amount)
mean_all_suppliers<-unlist(lapply(list_all_suppliers, mean))
mean_matched_suppliers <-unlist(lapply(list_matched_suppliers, mean))
p_value<- c(p_value_amount, p_value_high_tech, p_value_low_tech)
summary_statistics_df <- data.frame(mean_all_suppliers, mean_matched_suppliers, p_value)
row.names(summary_statistics_df)<- c("All orders", "High-tech", "Low-tech")

#### Matched and unmatched
list_matched<- list(matched_suppliers_orders$chf_amount, high_tech_matched_amount$chf_amount, low_tech_matched_amount$chf_amount)
list_unmatched<- list(unmatched_suppliers$chf_amount, high_tech_unmatched_amount$chf_amount, low_tech_unmatched_amount$chf_amount)
mean_matched<-unlist(lapply(list_matched, mean))
mean_unmatched <-unlist(lapply(list_unmatched, mean))
p_value<- c(p_value_matched_unmatched, p_value_high_tech_unmatched_matched, p_value_low_tech_unmatched_matched)
summary_statistics_df <- data.frame(mean_matched, mean_unmatched, p_value)
row.names(summary_statistics_df)<- c("All orders", "High-tech", "Low-tech")



## Country level statistics ---------------------------------------------

### Summary statistics by country for all orders ---------------------------------------

countries_orders<- all_orders %>% group_by(country) %>% 
  summarize(total_amount = sum(chf_amount), no_orders = n())
countries_orders<- countries_orders %>% mutate(cntr_code = country)
countries_orders$share_amount<- prop.table(countries_orders$total_amount)*100
countries_orders$share_orders<- prop.table(countries_orders$no_orders)*100

### Summary statistics for matched orders ------------------------------

countries_matched_orders<- matched_suppliers_orders %>% group_by(country) %>%
  summarize(total_amount = sum(chf_amount), no_orders = n())
countries_matched_orders$share_amount<- prop.table(countries_matched_orders$total_amount)*100
countries_matched_orders$share_orders<- prop.table(countries_matched_orders$no_orders)*100

### Summary statistics for unmatched orders ------------------------------

countries_unmatched_orders<- unmatched_suppliers %>% group_by(country) %>%
  summarize(total_amount = sum(chf_amount), no_orders = n())
countries_unmatched_orders$share_amount<- prop.table(countries_unmatched_orders$total_amount)*100
countries_unmatched_orders$share_orders<- prop.table(countries_unmatched_orders$no_orders)*100

#### Reshaping data for plot --------------------------------------------

## Matched orders
countries_matched_orders_plotdata<- countries_matched_orders %>% select(country, share_amount) %>% 
  rename(matched=share_amount)
## Unmatched orders
countries_unmatched_orders_plotdata<- countries_unmatched_orders %>% select(country, share_amount) %>% 
  rename(matched=share_amount)
## All orders
country_orders_plotdata<- countries_orders %>% select(country,share_amount) %>% 
  rename(all_data = share_amount)

#### Share of orders matched by country  --------

unmatched_orders_number <- countries_unmatched_orders %>% 
  select(country, no_orders) %>% 
  rename(unmatched_orders = no_orders)
matched_suppliers_orders_number<- countries_matched_orders %>% 
  select(country, no_orders) %>% 
  rename(matched_orders= no_orders)

unmatched_matched_share_orders<- left_join(matched_suppliers_orders_number, unmatched_orders_number)
unmatched_matched_share_orders<- unmatched_matched_share_orders %>% 
  mutate(total_orders = matched_orders + unmatched_orders, 
         matched_share = (matched_orders/total_orders)*100)

#### Share of total amount matched by country  --------

unmatched_orders_amount <- countries_unmatched_orders %>% 
  select(country, total_amount) %>% 
  rename(unmatched_amount = total_amount)
matched_suppliers_amount<- countries_matched_orders %>% 
  select(country, total_amount) %>% 
  rename(matched_amount= total_amount)

unmatched_matched_share_amount<- left_join(matched_suppliers_amount, unmatched_orders_amount)
unmatched_matched_share_amount<- unmatched_matched_share_amount %>% 
  mutate(total_amount = matched_amount + unmatched_amount, 
         matched_amount_share = (matched_amount/total_amount)*100)

## Plot the share of matched orders by country 

country_plot_matched_orders_share<- ggplot(unmatched_matched_share_orders, aes(x = reorder(as.factor(country),-matched_share), y = matched_share, fill= factor(-total_orders))) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Matched as % of total orders", x= "")+
  scale_fill_grey(start = 0.1, end =0.9)+
  theme_classic()+
  theme(legend.position="none")

country_plot_matched_amount_share<- ggplot(unmatched_matched_share_amount, aes(x = reorder(as.factor(country),-matched_amount_share), y = matched_amount_share, fill= factor(-total_amount))) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Matched as % of total amount", x= "")+
  scale_fill_grey(start = 0.1, end =0.9)+
  theme_classic()+
  theme(legend.position="none")






### Plotting the country data -----------------------------------------------
## Combining all orders and matched ones
countries_plot_data <- left_join(country_orders_plotdata, country_matched_orders_plotdata)

## Combining matched orders and unmatched orders 
countries_plot_data_matched_unmatched <- left_join(countries_matched_orders_plotdata, countries_unmatched_orders_plotdata)

#### Plot country all orders and matched -------------------------------------

countries_plot_data_all_matched <- countries_plot_data %>% pivot_longer(cols = 2:3, names_to = "Share", values_to = "percent") %>% 
  filter(!is.na(percent))

country_plot_all_matched<- ggplot(countries_plot_data, aes(x = as.factor(country), y = percent, fill = Share)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total amount", x= "")


#### Plot country unmatched and matched -------------------------------------
countries_plot_data_matched_unmatched <- countries_plot_data_matched_unmatched %>% pivot_longer(cols = 2:3, names_to = "Share", values_to = "percent") %>% 
  filter(!is.na(percent))

country_plot_matched_unmatched<- ggplot(countries_plot_data_matched_unmatched, aes(x = as.factor(country), y = percent, fill = Share)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total amount", x= "")



tech_amount_suppliers_pot<- tech_amount_suppliers_pot %>% rename(data = status_supplier)
tech_plot_side<- ggplot(tech_amount_suppliers_pot, aes(x = as.factor(technology_level), y = share_amount, fill = data)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total amount", x= "")






NUTS3_regions_orders<- NUTS3_regions %>% left_join(countries_orders)
European_countries<- st_read("/Users/gabrielepiazza/Downloads/ref-countries-2020-60m.shp/CNTR_LB_2020_3035.shp/CNTR_LB_2020_3035.shp")
European_countries<- clean_names(European_countries)
European_countries_orders<- European_countries %>% left_join(countries_orders)
ggplot(data = European_countries_orders, mapping = aes(fill = total_amount)) +
  geom_sf()






### Potential suppliers

potential_suppliers_by_country<- potential_suppliers_registration_year %>% group_by(country) %>% summarize(number_registrations = n())
potential_suppliers_by_country$share_registrations<- prop.table(potential_suppliers_by_country$number_registrations)*100
potential_suppliers_by_country_year<- potential_suppliers_registration_year %>% group_by(country, year_supplier_registration) %>% 
  summarize(number_registration = n())


potential_suppliers_by_year <- potential_suppliers_registration_year %>% 
  group_by(year_supplier_registration) %>% 
  summarize(number_registration = n())
share_orders<- prop.table(countries_orders$no_orders)*100

european_union <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                    "Czech Rep.","Denmark","Estonia","Finland","France",
                    "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                    "Lithuania","Luxembourg","Malta","Netherlands","Poland",
                    "Portugal","Romania","Slovakia","Slovenia","Spain",
                    "Sweden","United Kingdom", "Switzerland")

european_union_map <- 
  world_map %>% 
  filter(name %in% european_union)


bbox_europe <- st_bbox(c(xmin = -10, ymin = 20, xmax = 50, ymax = 80), crs = st_crs(european_union_map))
european_union_map_cropped <- st_crop(european_union_map, bbox_europe)
european_union_map_cropped<- european_union_map_cropped %>% rename(country =wb_a2)
european_union_map_cropped_orders<- european_union_map_cropped %>% left_join(countries_orders)
european_union_map_cropped_reg<- european_union_map_cropped %>% left_join(potential_suppliers_by_country)

amount_order_country<-  ggplot(data = european_union_map_cropped_orders) +
  geom_sf(mapping = aes(fill = desc(-total_amount))) +
  scale_fill_gradient(na.value = "grey50", name = "Total amount") +
  labs(title = "Total amount by country") +
  theme(plot.title.position = "plot")

number_orders_country<- ggplot(data = european_union_map_cropped_orders) +
  geom_sf(mapping = aes(fill = no_orders)) +
  scale_fill_gradient(na.value = "grey50") +
  labs(title = "Total orders by country") +
  theme(plot.title.position = "plot")

share_no_orders_country<- ggplot(data = european_union_map_cropped_orders) +
  geom_sf(mapping = aes(fill = share_orders)) +
  scale_fill_gradient(na.value = "grey50", name = "% of totan no.orders" ) +
  labs(title = "Share number orders by country") +
  theme(plot.title.position = "plot")

orders_country_bar <- ggplot(european_union_map_cropped_orders, aes(x = reorder(country, -share_orders), y= share_orders, fill = country)) +
  geom_bar(stat="identity", position=position_dodge())+theme(legend.position = "none",legend.text = element_text(colour="blue", size=4),
                                                             axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y= "Share of total orders", x= "")


share_amount_country<- ggplot(data = european_union_map_cropped_orders) +
  geom_sf(mapping = aes(fill = desc(-share_amount)), legend= "Share of total amount") +
  scale_fill_gradient(na.value = "grey50", name = "% of total amount") +
  labs(title= "Share of total amount, 1995-21") +
  theme(plot.title.position = "plot")

amount_country_bar <- ggplot(european_union_map_cropped_orders, aes(x = reorder(country, -share_amount), y= share_orders, fill = country)) +
  geom_bar(stat="identity", position=position_dodge())+theme(legend.position = "none",legend.text = element_text(colour="blue", size=4),
                                                             axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y= "Share of total amount", x= "")



#---

registration_order_country <- ggplot(data = european_union_map_cropped_reg) +
  geom_sf(mapping = aes(fill = desc(-number_registrations))) +
  scale_fill_gradient(na.value = "grey50", name = "Number of registrations") +
  labs(title= "Number of registrations") +
  theme(plot.title.position = "plot")

registration_country_bar <- ggplot(european_union_map_cropped_reg, aes(x = reorder(country, -share_registrations), y= share_registrations, fill = country)) +
  geom_bar(stat="identity", position=position_dodge())+theme(legend.position = "none",legend.text = element_text(colour="blue", size=4),
                                                             axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y= "Share registrations", x= "")


#------Suppliers registered

suppliers_registered <- all_orders %>% select(supplier_code, registration_supplier, country) %>% distinct()
suppliers_registered<- suppliers_registered %>% 
  group_by(country) %>% 
  summarize(no_orders = n())
suppliers_registered$share_suppliers<- prop.table(suppliers_registered$no_orders)*100


registration_suppliers_bar <- ggplot(suppliers_registered, aes(x = reorder(country, -share_suppliers), y= share_suppliers, fill = country)) +
  geom_bar(stat="identity", position=position_dodge())+theme(legend.position = "none",legend.text = element_text(colour="blue", size=4),
                                                             axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y= "Share suppliers", x= "")

#--------All orders
#plot order by tech and year

### Orders by tech ----------------------------------------------------------
tech_matched_orders<- matched_suppliers_orders %>% group_by(technology_level) %>% 
  summarize(total_technology_orders = n(), total_tech_amount= sum(chf_amount)) %>% 
  mutate(share_tech_orders= prop.table(total_technology_orders)*100,
         data = "matched")

tech_unmatched_orders <- unmatched_suppliers %>% group_by(technology_level) %>% 
  summarize(total_technology_orders = n(), total_tech_amount= sum(chf_amount)) %>% 
  mutate(share_tech_orders= prop.table(total_technology_orders)*100,
         data= "unmatched")

tech_unmatched_matched_orders <- rbind.data.frame(tech_matched_orders, tech_unmatched_orders)

tech_plot_matched_unmatched<- ggplot(tech_unmatched_matched_orders, aes(x = as.factor(technology_level), y = share_tech_orders, fill = data)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total orders", x= "")+
  theme_classic()


# Orders by size ----------------------------------------------------------

size_matched_orders<- matched_suppliers_orders %>% group_by(contract_size) %>% 
  summarize(total_size_orders = n(), size_amount= sum(chf_amount)) %>% 
  mutate(share_size_orders= prop.table(total_size_orders)*100,
         data = "matched")

size_unmatched_orders <- unmatched_suppliers %>% group_by(contract_size) %>% 
  summarize(total_size_orders = n(), size_amount= sum(chf_amount)) %>% 
  mutate(share_size_orders= prop.table(total_size_orders)*100,
         data= "unmatched")

size_unmatched_matched_orders <- rbind.data.frame(size_matched_orders, size_unmatched_orders)

size_plot_matched_unmatched<- ggplot(size_unmatched_matched_orders, aes(x = as.factor(contract_size), y = share_size_orders, fill = data)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total orders", x= "")+
  theme_classic()


tech_amount <- all_suppliers_95_21_tech_ms %>% group_by(technology_level) %>% 
  summarize(total_technology = sum(chf_amount)) %>% 
  mutate(share_technology = prop.table(total_technology)*100)

tech_amount<- tech_amount %>% 
  rename(share_amount = all_suppliers) %>% 
  mutate(status_supplier = "all")

tech_amount_suppliers_pot<- rbind(tech_amount, tech_amount_matched_suppliers)

tech_orders<- all_suppliers_95_21_tech_ms %>% group_by(technology_level) %>% 
  summarize(technology_orders = n()) %>% 
  mutate(share_technology_orders = prop.table(technology_orders)*100)

tech_level_amount_plot <- ggplot(tech_amount, aes(y=share_technology, x = technology_level))+
  geom_col(aes(fill=technology_level))



tech_level_orders_plot <-  ggplot(tech_orders, aes(y=share_technology_orders, x = technology_level))+
  geom_col(aes(fill=technology_level))

tech_amount_country<- all_suppliers_95_21_tech_ms %>% group_by(country, technology_level) %>% 
  summarize(technology_amount = sum(chf_amount)) %>% 
  mutate(share_technology_amount = prop.table(technology_amount)*100)

high_tech_amount<- tech_amount_country %>% filter(technology_level=="high-tech") %>% 
  mutate(country = reorder(country, desc(share_technology_amount))) %>% 
  filter(technology_amount >1000000)
tech_country_plot <- ggplot(high_tech_amount, aes(y=share_technology_amount, x=reorder(country, desc(share_technology_amount))))+
  geom_col()


plot_tech_year<-  ggplot(all_suppliers_95_21_tech_ms, aes(fill=technology_level, y=chf_amount, x=order_date)) +geom_bar(position="stack", stat='identity')

chf_by_country_tech<- all_suppliers_95_21_tech_ms %>% group_by(country, technology_level) %>% summarise(total=sum(chf_amount))#create dataset by country and tech
chf_by_country_tech<- chf_by_country_tech %>% filter(total>50000)
plot_tech_country<- ggplot(chf_by_country_tech, aes(y=log(total), x=country))+
  geom_col(aes(fill=technology_level))

#plot by size 

size_contracts <- all_suppliers_95_21_tech_ms %>% group_by(contract_size) %>% 
  summarize(total_amount = sum(chf_amount), number_orders = n()) %>% 
  mutate(share_amount = prop.table(total_amount)*100, share_orders = prop.table(number_orders)*100)

size_contracts_all<- size_contracts %>% select(contract_size, share_amount) %>% 
  mutate(data = "all")

size_contracts_technology <- all_suppliers_95_21_tech_ms %>% group_by(contract_size, technology_level) %>% 
  summarize(total_amount = sum(chf_amount), number_orders = n()) %>% 
  mutate(share_amount = prop.table(total_amount)*100, share_orders = prop.table(number_orders)*100)

size_contract_tech_average <- 
  
  size_contracts_country <- all_suppliers_95_21_tech_ms %>% group_by(contract_size, country) %>% 
  summarize(total_amount = sum(chf_amount), number_orders = n()) %>% 
  mutate(share_amount = prop.table(total_amount)*100, share_orders = prop.table(number_orders)*100)

size_contracts_high_tech <- size_contracts_technology %>% filter(technology_level =="high-tech")


size_orders_plot <-  ggplot(size_contracts, aes(y=share_amount, x = contract_size))+
  geom_col()

size_orders_tech_plot <-  ggplot(size_contracts_high_tech, aes(y=share_amount, x = contract_size))+
  geom_col()

size_tech_plot<-  ggplot(size_contracts_technology, aes(fill=technology_level, y=share_amount, x=contract_size)) +
  geom_bar(position="stack", stat='identity')

size_country_plot<- ggplot(size_contracts_country, aes(y=share_amount, x= country))+ 
  geom_bar(aes(fill=contract_size), stat= identity)

size_contracts

ggplot(size_contracts_country, aes(x = country, y = share_amount, fill = contract_size)) +
  geom_col(position = "fill", stat= "identity")+
  scale_y_continuous(labels = percent_format())







check_years_potential<- all_pot_suppliers_register_unique_filing_4 %>% 
  drop_na() %>% 
  group_by(bvd_id_number) %>% 
  summarize(year_after_order = length(year_orbis[year_orbis>year_supplier_registration]),
            year_before_order = length(year_orbis[year_orbis<year_supplier_registration]))

check_4_years_pot <- check_years_potential %>% filter(year_after_order>3 & year_before_order>3)
check_4_years_pot<- as.vector(check_4_years_pot$bvd_id_number)


# Matched suppliers -------------------------------------------------------

tech_amount_matched_suppliers <- all_orders_95_21_orbis_matched %>% group_by(technology_level) %>% 
  summarize(total_technology = sum(chf_amount), total_orders_technology = n()) %>% 
  mutate(share_total_amount_technology = prop.table(total_technology)*100, 
         share_total_orders_tech = prop.table(total_orders_technology)*100)

size_contracts_matched_suppliers <- all_orders_95_21_orbis_matched %>% group_by(contract_size) %>% 
  summarize(total_amount = sum(chf_amount), number_orders = n()) %>% 
  mutate(share_amount = prop.table(total_amount)*100, 
         share_orders = prop.table(number_orders)*100)

tech_amount_matched_suppliers<- tech_amount_matched_suppliers %>% 
  select(technology_level, share_total_amount_technology) %>% 
  rename(matched_suppliers = share_total_amount_technology)

tech_amount_matched_suppliers<- tech_amount_matched_suppliers %>% 
  rename(share_amount = matched_suppliers) %>% 
  mutate(status_supplier = "matched")

tech_amount<- tech_amount %>% 
  select(technology_level, share_technology) %>% 
  rename(all_suppliers = share_technology)


countries_matched_orders<- all_orders_95_21_orbis_matched %>% group_by(country) %>%
  summarize(total_amount = sum(chf_amount), no_orders = n())
countries_matched_orders$share_amount<- prop.table(countries_matched_orders$total_amount)*100
countries_matched_orders$share_orders<- prop.table(countries_matched_orders$no_orders)*100

country_matched_orders_plotdata<- countries_matched_orders %>% select(country, share_amount) %>% 
  rename(matched=share_amount)
country_orders_plotdata<- countries_orders %>% select(country,share_amount) %>% 
  rename(all_data = share_amount)

countries_plot_data <- left_join(country_orders_plotdata, country_matched_orders_plotdata)
countries_plot_data <- countries_plot_data %>% pivot_longer(cols = 2:3, names_to = "Share", values_to = "percent") %>% 
  filter(!is.na(percent))

ggplot(countries_plot_data, aes(x = as.factor(country), y = percent, fill = Share)) +
  geom_bar(stat = "identity", position = "dodge")
labs(y= "Share of total orders", x= "")

tech_table<-left_join(tech_amount, tech_amount_matched_suppliers) 

size_contracts_matched_suppliers<- size_contracts_matched_suppliers %>% 
  select(contract_size, share_amount) %>% 
  mutate(data= "matched")

size_contracts_pot<- rbind(size_contracts_all, size_contracts_matched_suppliers)

size_plot_side<- ggplot(size_contracts_pot, aes(x = as.factor(contract_size), y = share_amount, fill = data)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total amount", x= "")


size_contracts<- size_contracts %>% 
  select(contract_size, share_amount) %>% 
  rename(all_suppliers = share_amount)

size_table <- left_join(size_contracts, size_contracts_matched_suppliers)


# ### Summary for the final dataset ---------------------------------------

# Eliminate the outliers
# I am excluding the outliers following the paper by Kalemli-Ozcan
filtered_data <-  supplier_potential_matched[ supplier_potential_matched$operating_revenue_turnover_ < quantile(supplier_potential_matched$operating_revenue_turnover_, 0.99),] 
filtered_companies <- unique(filtered_data$bvd_id_number)
supplier_potential_matched<- supplier_potential_matched %>% filter(bvd_id_number %in% filtered_companies)
supplier_potential_matched<- left_join(supplier_potential_matched, all_addresses)
supplier_potential_matched<- supplier_potential_matched %>% select(-city, -postcode)
supplier_potential_matched<- supplier_potential_matched %>% distinct()

operating_revenue<-t.test(supplier_potential_matched$operating_revenue_turnover_ ~ status_supplier, data = supplier_potential_matched)
ebitda<- t.test(supplier_potential_matched$ebitda ~ status_supplier, data = supplier_potential_matched)
p_l_after_tax<-t.test(supplier_potential_matched$p_l_after_tax ~ status_supplier, data = supplier_potential_matched)
p_l_before_tax<- t.test(supplier_potential_matched$p_l_before_tax ~ status_supplier, data = supplier_potential_matched)
total_assets<- t.test(supplier_potential_matched$total_assets ~ status_supplier, data = supplier_potential_matched)
fixed_assets<- t.test(supplier_potential_matched$fixed_assets ~ status_supplier, data = supplier_potential_matched)
current_assets<- t.test(supplier_potential_matched$current_assets ~ status_supplier, data = supplier_potential_matched)
p_value_matched<- c(operating_revenue$p.value,ebitda$p.value, p_l_after_tax$p.value, p_l_before_tax$p.value,
                    total_assets$p.value, fixed_assets$p.value, current_assets$p.value)

summary_supplier_potential_matched<- supplier_potential_matched %>% group_by(status_supplier) %>% 
  summarize_at(vars(operating_revenue_turnover_,ebitda, p_l_after_tax, p_l_before_tax, total_assets,
                    fixed_assets, current_assets),
               funs(mean = mean(.,na.rm = TRUE))) # calculate the mean of each variable, excluding missing values


summary_suppliers_potential_matched_long<- summary_supplier_potential_matched%>%
  pivot_longer(!status_supplier, names_to = "mean", values_to = "Value") 

summary_suppliers_potential_matched_long$Value<- summary_suppliers_potential_matched_long$Value/1000000

summary_suppliers<- summary_suppliers_potential_matched_long %>% 
  filter(status_supplier ==1) %>%
  select(-status_supplier) %>% 
  rename(Suppliers = Value)

summary_potential<-  summary_suppliers_potential_matched_long %>% 
  filter(status_supplier ==0) %>%
  select(-status_supplier) %>% 
  rename(Potential = Value)

summary_suppliers_potential <- left_join(summary_suppliers, summary_potential) %>% 
  mutate(difference = Suppliers - Potential)

summary_suppliers_potential<- summary_suppliers_potential %>% mutate(p_value = p_value_matched)
summary_suppliers_potential<- summary_suppliers_potential %>% rename (Variable = mean)

xtable(summary_suppliers_potential, digits =2
)

ggplot(supplier_potential_matched,  aes(x = status_supplier,  y =log(operating_revenue_turnover_), fill = status_supplier)) +
  geom_boxplot()  +
  facet_wrap(~ status_supplier)




#### Selected countries --------------------------------------------------
## I select the countries with the biggest coverage as adv
countries_best_covered <- c("IT", "ES", "GR")
supplier_potential_matched_countries_covered <- supplier_potential_matched %>% 
  filter(country %in% countries_best_covered) %>% distinct()
supplier_potential_matched_countries_covered$city[supplier_potential_matched_countries_covered$city=='Muggio']<-"Muggio'"
supplier_potential_matched_countries_covered$city[supplier_potential_matched_countries_covered$city=='San Dona di Piave']<-"San Dona' Di Piave"
supplier_potential_matched_countries_covered$city[supplier_potential_matched_countries_covered$city=='Serra Ricco']<-"Serra Ricco'"
supplier_potential_matched_countries_covered$city[supplier_potential_matched_countries_covered$city=='Cernusco Sul Naviglio']<-"Cernusco sul Naviglio"


summary_country_suppliers<- supplier_potential_matched_countries_covered %>% 
  select(bvd_id_number, country, status_supplier, total_orders_amount) %>% 
  distinct() 

summary_country_suppliers<- left_join(summary_country_suppliers, all_addresses)
summary_country_suppliers<- summary_country_suppliers %>%   
  filter(status_supplier==1) %>% 
  group_by(country, city) %>% 
  summarize(total_amount = sum(total_orders_amount))

summary_suppliers_italy_city_nona <- summary_country_suppliers %>% 
  filter(country == "IT")

summary_potential_city_italy_nona<- left_join(summary_suppliers_italy_city_nona, italy_region_lookup)


summary_country_italy_nona$rank <- (rank(-summary_country_italy_nona$number_orders))

italy_regions<- read_csv("~/Dropbox/PhD/procurement_cern/data/processed/italy_orders_regions.csv")
italy_regions<- clean_names(italy_regions)
italy_region_orders<- italy_regions %>% group_by(region) %>% 
  summarize(total_orders= sum(total_orders)) %>% 
  filter(region != "Missing")

italy_region_orders$share_orders<- prop.table(italy_region_orders$total_orders)*100
italy_regions_plot<- ggplot(italy_region_orders, aes(x = reorder(region, -share_orders), y= share_orders, fill = region)) +
  geom_bar(stat="identity", position=position_dodge())+theme(legend.position = "none",legend.text = element_text(colour="blue", size=4),
                                                             axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y= "% Italy's total orders", x= "")


### Potential supplliers

summary_country_pot_suppliers<- supplier_potential_matched%>% 
  select(bvd_id_number, country, status_supplier, total_orders_amount) %>% 
  distinct() 
summary_country_pot_suppliers<- left_join(summary_country_pot_suppliers, all_addresses)
summary_country_pot_suppliers<- summary_country_pot_suppliers %>%   
  filter(status_supplier==0) %>% 
  group_by(country, city) %>% 
  summarize(total_amount = sum(total_orders_amount))

summary_pot_suppliers_italy_city_nona <- summary_country_pot_suppliers %>% 
  filter(country == "IT")


##### Distribution by industry 
summary_country_nace_orders <- supplier_potential_matched %>% 
  group_by(two_digit_nace) %>% 
  summarize(number_orders = n()) %>% distinct()

summary_country_italy_nace <- summary_country_nace_orders %>% 
  filter(country == "IT") 



## Potential suppliers

summary_potential_city_italy<- matched_potential_suppliers %>% 
  group_by(city, country) %>% 
  summarize(number_companies_registered = n()) %>% 
  filter(country =="IT")

italy_region_lookup <- read_csv("/Users/gabrielepiazza/Dropbox/PhD/procurement_cern/data/processed/italian_city_region_orders - italian_city_region_orders.csv")

summary_potential_city_italy<- left_join(summary_potential_city_italy, italy_region_lookup)



# Patent data -------------------------------------------------------------

patents_orbis<- read_excel("data_raw/Export 18_05_2023 21_58 - patents.xlsx",  sheet = "Results")

patents_orbis<- patents_orbis %>% rename(bvd_id_number = 'Current direct owner(s) BvD ID Number(s)')
patents_orbis<- patents_orbis %>% filter(!is.na(bvd_id_number))

patents_orbis$application_year<-format(patents_orbis$`Application/filing date`, "%Y")
patents_orbis$publication_year<-format(patents_orbis$`Publication date`, "%Y")



patents_application_counts <- patents_orbis %>%
  dplyr::group_by(bvd_id_number, application_year) %>%
  dplyr::summarize(
    num_applications = n_distinct(`Application/filing date`)) %>% 
  rename(year_orbis = application_year)


patent_publication_counts<- patents_orbis %>% 
  dplyr::group_by(bvd_id_number, publication_year) %>% 
  dplyr::summarize(num_publications = n_distinct(`Publication date`)) %>% 
  rename(year_orbis = publication_year)

patents_publications_apps_counts<- left_join(patents_application_counts,patent_publication_counts)
patents_publications_apps_counts<- patents_publications_apps_counts %>% filter(!is.na(year_orbis))
patents_publications_apps_counts$year_orbis<- as.numeric(patents_publications_apps_counts$year_orbis)
patents_publications_apps_counts_1995_2017<- patents_publications_apps_counts %>% filter(year_orbis>1994 & year_orbis<2018)
patents_publications_apps_counts_1995_2017 <- patents_publications_apps_counts_1995_2017 %>%
  mutate(num_publications = ifelse(is.na(num_publications), 0, num_publications))
# Diff-in-diff-estimation -------------------------------------------------

## Create yearly change variables

supplier_potential_matched_change<- supplier_potential_matched %>% 
  group_by(bvd_id_number) %>% 
  arrange(year_orbis) %>% 
  mutate(turnover_change = 100*((operating_revenue_turnover_- lag(operating_revenue_turnover_))/lag(operating_revenue_turnover_)), 
         ebitda_change = 100*((ebitda- lag(ebitda))/lag(ebitda)),
         p_l_after_tax = 100*((p_l_after_tax - lag(p_l_after_tax))/lag(p_l_after_tax)))


supplier_potential_matched_change<- supplier_potential_matched_change %>% 
  filter(status_supplier==1)

supplier_potential_matched_change$bvd_numeric<-  as.numeric(as.factor(supplier_potential_matched_change$bvd_id_number))
supplier_potential_matched_change<- supplier_potential_matched_change %>% 
  select(bvd_numeric, year_orbis, p_l_after_tax, first_order, first_order_amount, country) %>% drop_na() %>% 
  filter(p_l_after_tax!= -Inf, p_l_after_tax !=Inf)
supplier_potential_matched_change$first_order_amount_bin <- discretize(supplier_potential_matched_change$first_order_amount)
supplier_potential_matched_change_it <- supplier_potential_matched_change %>% filter(country=="IT")
cs_results <- att_gt(
  yname = "p_l_after_tax",
  tname = "year_orbis",
  idname = "bvd_numeric",
  gname = "first_order",
  data= supplier_potential_matched_change_it,
  control_group = "notyettreated", 
  xformla = ~1,
  allow_unbalanced_panel = TRUE
)

summary(cs_results)
es <- aggte(cs_results,
            type = "dynamic",
            min_e = -5, max_e = 10, 
            na.rm = TRUE
)

ggdid(es)

aggte(cs_results, type = "simple", na.rm = TRUE)


supplier_potential_matched_change_it <- supplier_potential_matched_change_it %>% mutate(postTreated = !is.na(first_order) & year_orbis >= first_order)

# Run static TWFE, with SEs clustered at the state level
twfe_static <- feols(p_l_after_tax ~ postTreated| bvd_numeric + year_orbis, data = supplier_potential_matched_change_it, cluster = "bvd_numeric")
summary(twfe_static)

