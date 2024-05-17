# Info --------------------------------------------------------------------
##
##Script name: 02_analysis 
##
##Purpose of script: Preparing the procurement data for analaysis##
##Author: Gabriele Piazza
##
##Date Created: 2023-07-03
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes:
##   
##
# 1. Install & Load packages --------------------------------------------------------
install.packages(c("terra", "spData",  "tmap", "leaflet","rnaturalearth",
                   "rnaturalearthdata", "hrbrthemes", "viridis", "ggpubr", 
                   "rstatix"))
install.packages("MatchIt")
install.packages("WeightIt")
install.packages("Matching")
install.packages("cobalt")
library(Matching)
library(MatchIt)
library(WeightIt)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(did)
library(modelsummary)
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
library(reshape2)
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
library(rdd)
library (haven)
library(sp)
library(spdep)
library(tidyverse)
library(forcats)
library(sf)
library(tmap)
library(xtable)
library(panelView)
options(scipen=999) # to get rid of scientific notation


'%notin%' <- Negate('%in%')
# 2. Load the data --------------------------------------------------------


#### 2.1 Load orders -----------------------------------------------------
## I have realized that there were lots of inconsistencies that I have now fixed but it takes from line 87 to line 281
### All orders
all_orders<- read_csv(here("data_proc", "all_orders.csv")) # these are all orders
all_orders_2017<- all_orders %>% filter(order_date<2018)# keep orders before 2018
all_orders_2017 <- all_orders_2017 %>% distinct() # remove duplicates
number_firms_all_orders_2017<- unique(all_orders_2017$supplier_code) # get the number of suppliers:4,685
number_all_orders<- unique(all_orders_2017$order_number) # get the numbe of orders:35678
bvd_id_lookup <- read_csv(here( "data_proc","bvd_id_lookup.csv"))
bvd_id_lookup<- bvd_id_lookup %>% rename(supplier_name = company_name) %>% distinct()

### All matched orders selected countries
all_orders_selected<- all_orders_2017 %>% filter(country %in% c("FR", "IT", "ES", "GB")) 
number_firms_all_orders_2017_selected_countries<- unique(all_orders_selected$supplier_code)#this is 2048
number_firms_names_all_orders_2017<-unique(all_orders_selected$supplier_name)
matched_supplier_orders<- left_join(all_orders_selected, bvd_id_lookup) %>% filter(!is.na(bvd_id_number)) %>%
 filter(!is.na(bvd_id_number)) %>% 
   distinct()
number_matched_supplier_orders<- unique(matched_supplier_orders$supplier_code) # get the number of suppliers that have been matched:1,471
number_matched_supplier_orders_bvd<- unique(matched_supplier_orders$bvd_id_number) # get the number of suppliers that have been matched:1,506
matched_supplier_orders<- as.data.frame(matched_supplier_orders)
number_suppliers_matched_names <- unique(matched_supplier_orders$supplier_name)


# I have noticed that there are some supplier_codes with multiple bvd_id_numbers
# Assuming your dataframe is named 'matched_supplier_orders'

matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ASI-01"] <- "FR444159164"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "SCHN06"] <- NA
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "GTD-50"] <- NA
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "CHN06"] <- "FR542048574"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "THAL02"] <- "FR401432125"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ABS-01"] <- "FR391301991"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "AGRU50"] <- "ESV28664472"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ALTE09"] <- "FR490919123"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "CABL75"] <- "ESB61966420"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ATOS50"] <- "FR350827788"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "C2M-01"] <- "FR338217110"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "CFS-02"] <- "FR323859074"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "DIMA25"] <- "FR325166577"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "EFFA50"] <- "FR312814122"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "FAMY50"] <- "FR764200218"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "GTD-50"] <- "ESA58393778"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "HLP-01"] <- "FR803406271"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "IDC-03"] <- "FR799740238"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "INT157"] <- "FR398288746"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ISMA01"] <- "FR392945424"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ISNA50"] <- "FR309304616"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "JEHI50"] <- "FR064200025"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "LAND50"] <- "FR351559018"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "LV--01"] <- "FR453739732"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "MADA25"] <- "R410746713"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "MBV-01"] <- "FR775660194"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "MULL12"] <- "FR448464677"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "NORC45"] <- "FR479685513"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "NOVA40"] <- "FR827988171"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "PREM11"] <- "FR414979336"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "SGSQ50"] <- "FR552031650"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "SOCO07"] <- "FR834096745"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "SOMA06"] <- "IT00164050429"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "SOTE20"] <- "FR337505259"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "TALL01"] <- "ESB22016208"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "TEC333"] <- "FR384449773"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "THER71"] <- "FR323459925"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ACRO50"] <- "FR500156930"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "SCHN06"] <- "FR421106709"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ABMT50"] <- "FR696780428"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "OXFO75"] <- "GB00775598"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "ATKI01"] <- "GB03215378"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "PANT12"] <- "GB02613750"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code == "RAYD50"] <- "GB03049059"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number == "A48511737"] <- "ESA48511737"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="02436170464"] <- "IT02436170464"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="69409899127"] <- "FR409899127"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="18313987547"] <- "FR313987547"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="81552096281"] <- "FR552096281"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="53425040078"] <- "FR425040078"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="01529600346"] <- "IT01529600346"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="02781830340"] <- "IT02781830340"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="B81254880"] <- "ESB81254880"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="B95795985"] <- "ESB95795985"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="00864500467"] <- "IT00864500467"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="02436170464"] <- "IT02436170464"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="B95795985"] <- "ESB95795985"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="CCD-01"] <- "GB01337532"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="EEV-50"] <- "GB00432014"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="GOOD50"] <- "GB01188162"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="MARC06"] <- "GB00432014"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="MSC-03"] <- "GB03507306"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="NCL-01"] <- "GB01988105"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$supplier_code=="SERC50"] <- "GBSC145051"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$vat_number=="02781830340"] <- "IT02781830340"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="478482383"] <- "FR478482383"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="43395337900024"] <- "FR433953379"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="B83379792"] <- "ESB83379792"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="37835872500018"] <- "FR378358725"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="828 890 178"] <- "FR828890178"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="A48511737"] <- "ESA48511737"
matched_supplier_orders$bvd_id_number[matched_supplier_orders$registration_number=="B83379792"] <- "ESB83379792"

# ### All unmatched orders
unmatched_supplier_orders<- left_join(all_orders_selected, bvd_id_lookup) %>% filter(is.na(bvd_id_number)) %>% 
  distinct()
# unmatched_supplier_orders<- read_csv(here("Analysis","data_proc", "unmatched_orders.csv")) # these are all orders that I was not able to match on Orbis
number_unmatched_supplier_orders<- unique(unmatched_supplier_orders$supplier_code) # get the numbers of orders that have not been matched:596
number_unmatched_supplier_orders<- unmatched_supplier_orders %>% select(supplier_code) %>% distinct()

datacheck<- rbind(matched_supplier_orders, unmatched_supplier_orders)
datacheck$bvd_id_number[datacheck$supplier_code == "ABB-01"] <- "FR335146312"
datacheck$bvd_id_number[datacheck$supplier_code == "VECT12"] <- NA
datacheck$bvd_id_number[datacheck$supplier_code == "AGME50"] <- "GB01494424"
datacheck$bvd_id_number[datacheck$supplier_code == "ALGE75"] <- "FR685550659"
datacheck$bvd_id_number[datacheck$supplier_code == "ANCO02"] <- "FR392319422"
datacheck$bvd_id_number[datacheck$supplier_code == "APAV50"] <- "FR518720925"
datacheck$bvd_id_number[datacheck$supplier_code == "AQUA02"] <- "FR443830120"
datacheck$bvd_id_number[datacheck$supplier_code == "ASAS01"] <- "FR384299285"
datacheck$bvd_id_number[datacheck$supplier_code == "ASI-01"] <- "FR444159164"
datacheck$bvd_id_number[datacheck$supplier_code == "ATKI01"] <- "GB03215378"
datacheck$bvd_id_number[datacheck$supplier_code == "AUBE50"] <- "FR380342808"
datacheck$bvd_id_number[datacheck$supplier_code == "AVNE50"] <- "FR552076010"
datacheck$bvd_id_number[datacheck$supplier_code == "AW--02"] <- "FR428179048"
datacheck$bvd_id_number[datacheck$supplier_code == "BERG87"] <- "FR414997130"
datacheck$bvd_id_number[datacheck$supplier_code == "BILL50"] <- "FR339964462"
datacheck$bvd_id_number[datacheck$supplier_code == "BOMA03"] <- "ESB62903869"
datacheck$bvd_id_number[datacheck$supplier_code == "CABU54"] <- "FR394624712"
datacheck$bvd_id_number[datacheck$supplier_code == "CANB02"] <- "FR512975129"
datacheck$bvd_id_number[datacheck$supplier_code == "CASA05"] <- "ESB83595280"
datacheck$bvd_id_number[datacheck$supplier_code == "CEGE55"] <- "FR537915787"
datacheck$bvd_id_number[datacheck$supplier_code == "CETI50"] <- "FR314257684"
datacheck$bvd_id_number[datacheck$supplier_code == "CNRS50"] <- "FR180089013"
datacheck$bvd_id_number[datacheck$supplier_code == "COFR01"] <- "FR381419076"
datacheck$bvd_id_number[datacheck$supplier_code == "CONV05"] <- "FR481213692"
datacheck$bvd_id_number[datacheck$supplier_code == "CVT-44"] <- "GB02670152"
datacheck$bvd_id_number[datacheck$supplier_code == "DANT01"] <- "GB02806796"
datacheck$bvd_id_number[datacheck$supplier_code == "DESC50"] <- "FR955501036"
datacheck$bvd_id_number[datacheck$supplier_code == "DIAM02"] <- "FR955501036"
datacheck$bvd_id_number[datacheck$supplier_code == "DIMA25"] <- "FR325166577"
datacheck$bvd_id_number[datacheck$supplier_code == "ECRI50"] <- "FR329844187"
datacheck$bvd_id_number[datacheck$supplier_code == "EDWA01"] <- "FR378821771"
datacheck$bvd_id_number[datacheck$supplier_code == "EFOU01"] <- "IT02005300351"
datacheck$bvd_id_number[datacheck$supplier_code == "EMTE02"] <- "ESA64381072"
datacheck$bvd_id_number[datacheck$supplier_code == "ENER30"] <- "FR382861664"
datacheck$bvd_id_number[datacheck$supplier_code == "ENSI25"] <- "FR334148764"
datacheck$bvd_id_number[datacheck$supplier_code == "ESPA75"] <- "FR488000209"
datacheck$bvd_id_number[datacheck$supplier_code == "FARN45"] <- "FR388245029"
datacheck$bvd_id_number[datacheck$supplier_code == "FRAN50"] <- "FR380194019"
datacheck$bvd_id_number[datacheck$supplier_code == "GALV04"] <- "IT09592360151"
datacheck$bvd_id_number[datacheck$supplier_code == "GOOD04"] <- "FR400072997"
datacheck$bvd_id_number[datacheck$supplier_code == "GR2S31"] <- "FR334247459"
datacheck$bvd_id_number[datacheck$supplier_code == "GR3S01"] <- "GB02025661"
datacheck$bvd_id_number[datacheck$supplier_code == "GR3S03"] <- "ESA58393778"
datacheck$bvd_id_number[datacheck$supplier_code == "GROU11"] <- "FR412904773"
datacheck$bvd_id_number[datacheck$supplier_code == "GTM-01"] <- "FR399022177"
datacheck$bvd_id_number[datacheck$supplier_code == "GTM-05"] <- "FR501402234"
datacheck$bvd_id_number[datacheck$supplier_code == "HOLT02"] <- "GB06058999"
datacheck$bvd_id_number[datacheck$supplier_code == "INS-03"] <- "FR352714828"
datacheck$bvd_id_number[datacheck$supplier_code == "ISNA50"] <- "FR309304616"
datacheck$bvd_id_number[datacheck$supplier_code == "LEAF01"] <- "GBNI022726"
datacheck$bvd_id_number[datacheck$supplier_code == "LEO-01"] <- "FR402277404"
datacheck$bvd_id_number[datacheck$supplier_code == "LEYB90"] <- "FR702029976"
datacheck$bvd_id_number[datacheck$supplier_code == "LOCA02"] <- "FR304453160"
datacheck$bvd_id_number[datacheck$supplier_code == "LOXA01"] <- "FR450776968"
datacheck$bvd_id_number[datacheck$supplier_code == "MAC-08"] <- "GB00262938"
datacheck$bvd_id_number[datacheck$supplier_code == "MARC06"] <- "GB00432014"
datacheck$bvd_id_number[datacheck$supplier_code == "META06"] <- "FR399345479"
datacheck$bvd_id_number[datacheck$supplier_code == "META56"] <- "FR301919973"
datacheck$bvd_id_number[datacheck$supplier_code == "MKS-50"] <- "FR328117973"
datacheck$bvd_id_number[datacheck$supplier_code == "MULL12"] <- "FR448464677"
datacheck$bvd_id_number[datacheck$supplier_code == "NALC01"] <- "FR414946681"
datacheck$bvd_id_number[datacheck$supplier_code == "NCT-01"] <- "FR431315159"
datacheck$bvd_id_number[datacheck$supplier_code == "PANT12"] <- "GB02613750"
datacheck$bvd_id_number[datacheck$supplier_code == "PAUC01"] <- "FR353562101"
datacheck$bvd_id_number[datacheck$supplier_code == "PELI50"] <- "FR764200051"
datacheck$bvd_id_number[datacheck$supplier_code == "PLD-01"] <- "FR407666650"
datacheck$bvd_id_number[datacheck$supplier_code == "RAYD50"] <- "GB03049059"
datacheck$bvd_id_number[datacheck$supplier_code == "SAES50"] <- "IT00774910152"
datacheck$bvd_id_number[datacheck$supplier_code == "SALI30"] <- "FR729802744"
datacheck$bvd_id_number[datacheck$supplier_code == "SCHN06"] <- "FR421106709"
datacheck$bvd_id_number[datacheck$supplier_code == "SCT-75"] <- "FR433940483"
datacheck$bvd_id_number[datacheck$supplier_code == "SERV19"] <- "FR408366318"
datacheck$bvd_id_number[datacheck$supplier_code == "SIEM66"] <- "FR562016774"
datacheck$bvd_id_number[datacheck$supplier_code == "SPIE31"] <- "FR303875983"
datacheck$bvd_id_number[datacheck$supplier_code == "SRC-03"] <- "FR431426360"
datacheck$bvd_id_number[datacheck$supplier_code == "STEE09"] <- "GB05269425"
datacheck$bvd_id_number[datacheck$supplier_code == "TEKE75"] <- "FR414776054"
datacheck$bvd_id_number[datacheck$supplier_code == "THOM45"] <- "FR552059024"
datacheck$bvd_id_number[datacheck$supplier_code == "TOST55"] <- "IT01181900539"
datacheck$bvd_id_number[datacheck$supplier_code == "TRA-38"] <- "FR444579205"
datacheck$bvd_id_number[datacheck$supplier_code == "TRAC10"] <- "GB03959572"
datacheck$bvd_id_number[datacheck$supplier_code == "VACU50"] <- "GB05361640"
datacheck$bvd_id_number[datacheck$supplier_code == "VIST01"] <- "FR423191014"
datacheck$bvd_id_number[datacheck$supplier_code == "ZIGL50"] <- "FR452580970"
datacheck$bvd_id_number[datacheck$registration_number == "43395337900024"] <- "FR433953379"


bvd_id_lookup_new<- datacheck %>% select(supplier_code, supplier_name, bvd_id_number) %>% distinct()
write_csv(bvd_id_lookup_new, here( "data_proc", "bvd_id_lookup_final.csv"))


datacheck<- datacheck %>% distinct()

matched_suppliers <- datacheck %>% filter(!is.na(bvd_id_number)) %>% distinct()
unmatched_suppliers<- datacheck %>% filter(is.na(bvd_id_number)) %>% distinct()

number_matched_suppliers <- unique(matched_suppliers$supplier_code)#1,470
number_unmatched_suppliers <- unique(unmatched_suppliers$supplier_code)#578

number_matched_orders <- unique(matched_suppliers$order_number)# 13,043
number_unmatched_orders <- unique(unmatched_suppliers$order_number) #5,337

check<- datacheck%>%group_by(supplier_code) %>% dplyr::summarize(n_distinct(bvd_id_number))

number_unmatched_matched<- rbind(number_matched_supplier_codes, number_unmatched_supplier_orders)
number_unmatched_matched_list <- unique(number_unmatched_matched$supplier_code)
number_unmatched_matched$matched<- "yes"
number_all_orders_supplier_codes_list <- unique(number_all_orders_supplier_codes$supplier_code)
check<- number_all_orders_supplier_codes%>% left_join(number_unmatched_matched)
check<- (number_unmatched_matched)

#### 2.2 Registration suppliers and potential suppliers ------------------

### Registration years of suppliers (it goes up to 2008) - Might not be the right file

suppliers_registration_year<- read_csv(here("data_raw", "suppliers_registration_year.csv"))# this is when the actual suppliers registered 

### Potential suppliers (it goes up to 2014) - 

potential_suppliers_registration_year<- read_csv(here("data_raw", "22_10_31_potential_suppliers.csv"))# this 
number_potential_suppliers<- potential_suppliers_registration_year%>%
  filter(COUNTRY %in% c("IT", "GB", "FR", "ES") & YEAR_SUPPLIER_REGISTRATION<2018) %>% 
  select(SUPPLIERCODE) %>% distinct() %>% 
  pull(SUPPLIERCODE)

# ### 2.3 Load the orbis matched data -------------------------------------


matched_suppliers_orbis<- read_csv(here( "data_proc", "full_panel_suppliers.csv"))
matched_potential_orbis<- read_csv(here( "data_proc", "full_panel_pot_suppliers.csv"))

# Lines 120-124 might be redundant
#list_of_registered_companies<- unique(suppliers_registration_year$SUPPLIER_CODE)# This is not very informative as this only has data up to 2008
#list_of_registered_potential<- unique(potential_suppliers_registration_year$SUPPLIERCODE)
#checking_overlap_registered<-suppliers_registration_year %>%  filter(SUPPLIER_CODE %in% list_of_registered_potential)
potential_suppliers_registration_year<- clean_names(potential_suppliers_registration_year)
potential_suppliers_registration_year_2017<- potential_suppliers_registration_year %>% filter(year_supplier_registration<2018)

# Check number of registrations by supplier: i want to check whether companies register more than once

potential_suppliers_registration_year_2017<- potential_suppliers_registration_year_2017 %>% 
  select(suppliercode, year_supplier_registration) %>% distinct() %>% 
  dplyr::group_by(suppliercode) %>% dplyr::summarize(number = n())

# 3. Descriptive statistics -----------------------------------------------

### 3.1 Basic summary statistics for matched orders and  unmatched orders  --------------------------------

#### Basic summary statistics for unmatched orders  --------------------------------
summary_statistics_matched_supplier<- matched_suppliers %>% select(-city) %>% distinct()
summary_statistics_matched_supplier_table <- summary(matched_suppliers$chf_amount) # this is not working for some reason
summary_statistics_matched_supplier_table<- as.vector(summary_statistics_matched_supplier_table) # this does not work for some reason

#### Basic summary statistics for unmatched orders  --------------------------------
# unmatched_suppliers<- unmatched_suppliers %>% filter(company_name!="ICM S.P.A.")
summary_statistics_unmatched_supplier <-unmatched_suppliers  %>% distinct()
summary_statistics_unmatched_supplier_table<-summary(summary_statistics_unmatched_supplier$chf_amount)
summary_statistics_unmatched_supplier_table <- as.vector(summary_statistics_unmatched_supplier_table)

##### Putting the summary statistics for matched and unmatched together -------

summary_matched_unmatched<- rbind(summary_statistics_unmatched_supplier_table, summary_statistics_matched_supplier_table)
summary_matched_unmatched<- as.data.frame(summary_matched_unmatched)
colnames(summary_matched_unmatched)<- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
row.names(summary_matched_unmatched)<- c("Unmatched orders", "Orbis-Matched orders")
table_1 <- xtable(summary_matched_unmatched, caption = "Summary statistics for mathched and unmatched orders")
table_1 <- print(table_1, include.rownames = FALSE)
write.table(table_1, here("results", "tables", "table_1.tex"), sep = "", row.names = TRUE, col.names = TRUE, quote = FALSE)


### 3.2 Basic summary statistics for matched orders and unmatched orders for selected countries --------------------------------

## technology level 

high_tech_order <- c(3,4,5,7)# I use the information provided by CSIL
low_tech_order<- c(1,2,6, NA) #

matched_suppliers<- matched_suppliers %>% 
  mutate(technology_level = case_when(code_1_digit %in% high_tech_order ~"high-tech", 
                                      code_1_digit %in% low_tech_order ~ "low-tech",
                                      TRUE ~ "low-tech"))

unmatched_suppliers<- unmatched_suppliers %>% 
  mutate(technology_level = case_when(code_1_digit %in% high_tech_order ~"high-tech", 
                                      code_1_digit %in% low_tech_order ~ "low-tech",
                                      TRUE ~ "low-tech"))

selected_countries <- c("IT", "FR", "ES", "GB")# this is the list of countries that I am focusing on 

matched_supplier_orders_selected_countries <- matched_suppliers %>% 
  filter(country %in% selected_countries) # filter in order to have only  suppliers in the selected countries
unmatched_suppliers_selected_countries <- unmatched_suppliers %>% 
  filter(country %in% selected_countries) %>% filter(is.na(bvd_id_number))# filter in order to have only  suppliers in the selected countries


#### Basic summary statistics for matched orders  --------------------------------

summary_statistics_matched_supplier_selected_countries<- matched_supplier_orders_selected_countries %>% select(-city) %>% distinct()
summary_statistics_matched_supplier_table <- summary(matched_supplier_orders_selected_countries$chf_amount) # this is not working for some reason
summary_statistics_matched_supplier_table<- as.vector(summary_statistics_matched_supplier_table) # this does not work for some reason

#### Basic summary statistics for unmatched orders  --------------------------------
summary_statistics_unmatched_supplier_selected_countries <-unmatched_suppliers_selected_countries %>% distinct()
summary_statistics_unmatched_supplier_table<-summary(summary_statistics_unmatched_supplier_selected_countries$chf_amount)
summary_statistics_unmatched_supplier_table <- as.vector(summary_statistics_unmatched_supplier_table)


##### Putting the summary statistics for matched and unmatched together -------

summary_matched_unmatched_selected_countries<- rbind(summary_statistics_unmatched_supplier_table, summary_statistics_matched_supplier_table)
summary_matched_unmatched_selected_countries<- as.data.frame(summary_matched_unmatched_selected_countries)
colnames(summary_matched_unmatched_selected_countries)<- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
row.names(summary_matched_unmatched_selected_countries)<- c("Unmatched orders", "Orbis-Matched orders")
table_1 <- xtable(summary_matched_unmatched_selected_countries, caption = "Summary statistics for mathched and unmatched orders")
table_1 <- print(table_1, include.rownames = FALSE)
write.table(table_1, here( "results", "tables", "table_1.tex"), sep = "", row.names = TRUE, col.names = TRUE, quote = FALSE)




  
#### 3.21 Matched and Unmatched  k density plot for selected countries -----------------------------------


amount_matched_suppliers_orders_fritesgb<- matched_supplier_orders_selected_countries %>% 
  select(supplier_code, chf_amount) %>% # selects the variables of interest
  mutate(status = "matched suppliers") # changed the name of the variables

amount_unmatched_suppliers_orders_fritesgb<- unmatched_supplier_orders_selected_countries %>% 
  select(supplier_code, chf_amount) %>% # selects the variables of interest
  mutate(status = "unmatched suppliers") # changed the name of the variables

k_density_data_matched_unmatched_selected<- rbind(amount_matched_suppliers_orders_fritesgb, amount_unmatched_suppliers_orders_fritesgb) # it binds the two datasets together

k_density_plot_matched_unmatched_selected<- ggplot(data=k_density_data_matched_unmatched_selected, aes(x=log(chf_amount), group=status, fill=status)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_classic()

ggsave(here("results","figures","fig_1.png"), plot = k_density_plot_matched_unmatched_selected, dpi = 300, width = 9, height = 6, limitsize = F)


#### 3.22 T test selected countries ---------------------------------------------------------------
##### All orders and matched suppliers by technology for selected countries 
#####  I want to understand whether the amount for the matched and unmatched is statistically different, bt technology  
##### Matched and unmatched
t_result_matched_unmatched_selected_countries<- t.test(summary_statistics_unmatched_supplier_selected_countries$chf_amount, 
                                                       summary_statistics_matched_supplier_selected_countries$chf_amount) #this does a t test on the amount 
p_value_matched_unmatched_selected_countries <- t_result_matched_unmatched_selected_countries$p.value

high_tech_matched_amount_selected_countries<- summary_statistics_matched_supplier_selected_countries %>% filter(technology_level== "high-tech") %>% dplyr::select(chf_amount)
low_tech_matched_amount_selected_countries <- summary_statistics_matched_supplier_selected_countries %>% filter(technology_level =="low-tech") %>% dplyr::select(chf_amount)

high_tech_unmatched_amount_selected_countries<- summary_statistics_unmatched_supplier_selected_countries %>% filter(technology_level == "high-tech") %>% dplyr::select(chf_amount)
low_tech_unmatched_amount_selected_countries <- summary_statistics_unmatched_supplier_selected_countries %>% filter(technology_level == "low-tech") %>% dplyr:: select(chf_amount)

t_high_tech_matched_unmatched_selected_countries <- t.test(high_tech_matched_amount_selected_countries, high_tech_unmatched_amount_selected_countries)
t_low_tech_matched_unmatched_selected_countries <- t.test(low_tech_matched_amount_selected_countries, low_tech_matched_amount_selected_countries)
p_value_high_tech_unmatched_matched_selected_countries <- t_high_tech_matched_unmatched_selected_countries$p.value
p_value_low_tech_unmatched_matched_selected_countries <- t_low_tech_matched_unmatched_selected_countries$p.value


consortia<- unmatched_supplier_orders %>% dplyr::select(supplier_code, company_name) %>% distinct()

consortia<- subset(consortia, grepl("[-+]", company_name))
consortia <- subset(consortia, grepl("\\(.*\\)", company_name))
#####Create a table -------------------------------------------------------


#### Matched and unmatched
list_matched_selected_countries<- list(matched_supplier_orders_selected_countries$chf_amount, high_tech_matched_amount_selected_countries$chf_amount, low_tech_matched_amount_selected_countries$chf_amount)
list_unmatched_selected_countries<- list(unmatched_suppliers_selected_countries$chf_amount, high_tech_unmatched_amount_selected_countries$chf_amount, low_tech_unmatched_amount_selected_countries$chf_amount)
mean_matched_selected_countries<-unlist(lapply(list_matched_selected_countries, mean))
mean_unmatched_selected_countries <-unlist(lapply(list_unmatched_selected_countries, mean))
p_value_selected_countries<- c(p_value_matched_unmatched_selected_countries, p_value_high_tech_unmatched_matched_selected_countries, p_value_low_tech_unmatched_matched_selected_countries)
summary_statistics_df_selected_countries <- data.frame(mean_matched_selected_countries, mean_unmatched_selected_countries, p_value_selected_countries)
row.names(summary_statistics_df_selected_countries)<- c("All orders", "High-tech", "Low-tech")
table_2 <- xtable(summary_statistics_df_selected_countries, caption = "Summary statistics for matched and unmatched orders")
table_2 <- print(table_2, include.rownames = TRUE)

# Save LaTeX table code to a file
write.table(table_2, here( "results", "tables", "table_2.tex"), sep = "", row.names = TRUE, col.names = TRUE, quote = FALSE)





#### These are the  matched and unmatched orders by country  --------------------------------
# 
# 
# countries_matched_orders<- matched_supplier_orders %>% # What I do here is to group by country and summarize 
#   group_by(country)%>%
#   dplyr::summarize(total_amount = sum(chf_amount), no_orders = n())# and get the total amount and the number of orders
# 
# countries_matched_orders$share_amount<- prop.table(countries_matched_orders$total_amount)*100
# countries_matched_orders$share_orders<- prop.table(countries_matched_orders$no_orders)*100
# 
# #### Summary statistics for unmatched orders ------------------------------
# 
# countries_unmatched_orders<- unmatched_supplier_orders %>% filter(is.na(bvd_id_number)) %>% 
#   group_by(country) %>%
#   dplyr::summarize(total_amount = sum(chf_amount), no_orders = n())
# countries_unmatched_orders$share_amount<- prop.table(countries_unmatched_orders$total_amount)*100
# countries_unmatched_orders$share_orders<- prop.table(countries_unmatched_orders$no_orders)*100
# 
# #### Reshaping data for plot --------------------------------------------
# 
# ### Matched orders
# countries_matched_orders_plotdata<- countries_matched_orders %>% select(country, share_amount) %>% 
#   rename(matched=share_amount)
# ## Unmatched orders
# countries_unmatched_orders_plotdata<- countries_unmatched_orders %>% select(country, share_amount) %>% 
#   rename(unmatched=share_amount)
# 
# 
# unmatched_orders_number <- countries_unmatched_orders %>% 
#   select(country, no_orders) %>% 
#   rename(unmatched_orders = no_orders)
# matched_suppliers_orders_number<- countries_matched_orders %>% 
#   select(country, no_orders) %>% 
#   rename(matched_orders= no_orders)
# 
# unmatched_matched_share_orders<- left_join(matched_suppliers_orders_number, unmatched_orders_number)
# unmatched_matched_share_orders<- unmatched_matched_share_orders %>% 
#   mutate(total_orders = matched_orders + unmatched_orders, 
#          matched_share = (matched_orders/total_orders)*100)
# 
# #### Share of total amount matched by country  --------
# 
# unmatched_orders_amount <- countries_unmatched_orders %>% 
#   select(country, total_amount) %>% 
#   rename(unmatched_amount = total_amount)
# matched_suppliers_amount<- countries_matched_orders %>% 
#   select(country, total_amount) %>% 
#   rename(matched_amount= total_amount)
# 
# unmatched_matched_share_amount<- left_join(matched_suppliers_amount, unmatched_orders_amount)
# unmatched_matched_share_amount<- unmatched_matched_share_amount %>% 
#   mutate(total_amount = matched_amount + unmatched_amount) %>% 
#   mutate(matched_amount_share = (matched_amount/total_amount)*100)
# 
# 
# ## Plot the share of matched orders by country 
# 
# country_plot_matched_orders_share<- ggplot(unmatched_matched_share_orders, aes(x = reorder(as.factor(country),-matched_share), y = matched_share, fill= factor(-total_orders))) +
#   geom_bar(stat = "identity", position = "dodge")+labs(y= "Matched as % of total orders", x= "")+
#   scale_fill_grey(start = 0.1, end =0.9)+
#   theme_classic()+
#   theme(legend.position="none",
#         text = element_text(size = 12))
# 
# country_plot_matched_amount_share <- ggplot(unmatched_matched_share_amount, aes(x = reorder(as.factor(country), -matched_amount_share), y = matched_amount_share, fill = factor(-total_amount))) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(y = "Matched as % of total amount", x = "") +
#   scale_fill_grey(start = 0.1, end = 0.9) +
#   theme_classic() +
#   theme(legend.position = "none",
#         text = element_text(size = 12))  # Increase text size (e.g., size = 12)
# 
# 
# ggsave(here("results","figures","fig_2.png"), plot = country_plot_matched_orders_share, dpi = 300, width = 9, height = 6, limitsize = F)
# ggsave(here("results","figures","fig_3.png"), plot = country_plot_matched_amount_share, dpi = 300, width = 9, height = 6, limitsize = F)
# 
# 
# ### Plotting the country data -----------------------------------------------
# 
# ## Combining matched orders and unmatched orders 
# countries_plot_data_matched_unmatched <- left_join(countries_matched_orders_plotdata, countries_unmatched_orders_plotdata, by = "country")
# 
# countries_plot_data_matched_unmatched <- countries_plot_data_matched_unmatched %>% pivot_longer(cols = 2:3, names_to = "Share", values_to = "percent") %>% 
#   filter(!is.na(percent))
# 
# plot_country_matched_unmatched<- ggplot(countries_plot_data_matched_unmatched, aes(x = country, y = percent, fill = Share)) +
#   geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total amount", x= "")
# 
# 
# ggsave(here("results","figures","fig_4.png"), plot = plot_country_matched_unmatched, dpi = 300, width = 9, height = 6, limitsize = F)
# 
# 
# ## Orders by size ----------------------------------------------------------
# 
# 
# size_matched_orders<- matched_supplier_orders %>% group_by(contract_size) %>% 
#   dplyr::summarize(total_size_orders = n(), size_amount= sum(chf_amount)) %>% 
#   mutate(share_size_orders= prop.table(total_size_orders)*100,
#          data = "matched")
# 
# size_unmatched_orders <- unmatched_supplier_orders %>% group_by(contract_size) %>% 
#   dplyr::summarize(total_size_orders = n(), size_amount= sum(chf_amount)) %>% 
#   mutate(share_size_orders= prop.table(total_size_orders)*100,
#          data= "unmatched")
# 
# size_unmatched_matched_orders <- rbind.data.frame(size_matched_orders, size_unmatched_orders)
# 
# size_plot_matched_unmatched<- ggplot(size_unmatched_matched_orders, aes(x = as.factor(contract_size), y = share_size_orders, fill = data)) +
#   geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total orders", x= "")+
#   theme_classic()
# 
# ggsave(here("results","figures","fig_5.png"), plot = size_plot_matched_unmatched, dpi = 300, width = 9, height = 6, limitsize = F)
# 
# 
# ## Orders by tech ----------------------------------------------------------
# 
# tech_matched_orders <- matched_supplier_orders %>% group_by(technology_level) %>% 
#   dplyr::summarize(total_size_orders = n(), size_amount = sum(chf_amount)) %>% 
#   mutate(share_technology_orders = prop.table(total_size_orders)*100, 
#          data= "matched")
# 
# tech_unmatched_orders<- unmatched_supplier_orders %>% group_by(technology_level) %>% 
#   dplyr::summarize(total_size_orders = n(), size_amount = sum(chf_amount)) %>% 
#   mutate(share_technology_orders = prop.table(total_size_orders)*100,
#          data = "unmatched")
# 
# tech_unmatched_matched_orders <- rbind.data.frame(tech_matched_orders, tech_unmatched_orders)
# 
# 
# tech_plot_matched_unmatched<- ggplot(tech_unmatched_matched_orders, aes(x = as.factor(technology_level), y = share_technology_orders, fill = data)) +
#   geom_bar(stat = "identity", position = "dodge")+labs(y= "Share of total orders", x= "")+
#   theme_classic()
# 
# ggsave(here("results","figures","fig_6.png"), plot = tech_plot_matched_unmatched, dpi = 300, width = 9, height = 6, limitsize = F)
# 


## Registration ------------------------------------------------------------
potential_suppliers_registration_year<- clean_names(potential_suppliers_registration_year)
potential_suppliers_registration_plot_data <- potential_suppliers_registration_year %>% select(suppliercode, year_supplier_registration, city,x1_digit,  country) %>% distinct()
potential_suppliers_registration_plot_data <- potential_suppliers_registration_plot_data %>% 
  group_by(country) %>% 
  dplyr::summarize(no_orders = n())
potential_suppliers_registration_plot_data$share_suppliers<- prop.table(potential_suppliers_registration_plot_data$no_orders)*100


potential_suppliers_registration_bar <- ggplot(potential_suppliers_registration_plot_data, aes(x = reorder(country, -share_suppliers), y= share_suppliers, fill = country)) +
  geom_bar(stat="identity", position=position_dodge())+theme(legend.position = "none",legend.text = element_text(colour="blue", size=4),
                                                             axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y= "Share registered suppliers", x= "")

ggsave(here("results", "figures", "fig_7.png"), plot = potential_suppliers_registration_bar, dpi = 300, width = 9, height = 6, limitsize = F )



## Orbis data for suppliers and potential ----------------------------------
matched_potential_orbis<- read_csv(here("Analysis", "data_proc", "full_panel_pot_suppliers.csv"))
matched_suppliers_orbis<- read_csv(here("Analysis", "data_proc","full_panel_suppliers.csv" ))
matched_suppliers_orbis <- matched_suppliers_orbis %>% select(-'...1')
matched_potential_orbis<- matched_potential_orbis %>% select(-'...1')
list_names_supplier_orbis <- names(matched_suppliers_orbis)
list_names_potential_orbis <- names(matched_potential_orbis)
setdiff(list_names_potential_orbis, list_names_supplier_orbis)

matched_potential_orbis<- matched_potential_orbis %>% select(-consolidation_code, -closing_date, - original_currency,
                                                             -exchange_rate_from_original_curr, -activitycode, - year_orbis)
matched_potential_orbis <- matched_potential_orbis %>% select(-month_orbis, -tech_intensity)


matched_potential_orbis <- matched_potential_orbis %>% 
  mutate(order_date = 0, total_chf_amount_year = 0, order_after_last_orbis = 0, registration_first_order = 0, 
         year_after_order = 0, year_before_order = 0)
  

matched_suppliers_orbis<- matched_suppliers_orbis %>% # this is the actual suppliers and I select only the relevant variables
  select(bvd_id_number, registration_year,supplier_status, year_orbis, 
         first_order, first_order_amount, total_orders_amount,  operating_revenue_turnover_,
         fixed_assets, current_assets, ebitda, total_assets, intangible_fixed_assets, p_l_after_tax, p_l_before_tax) %>% distinct() # I select only the variables that I need


matched_potential_suppliers<- matched_potential_orbis %>%
  rename(year_supplier_registration = registration_year) %>% # this is the potnetial suppliers
  mutate(first_order_amount=0, first_order=0, total_orders_amount= 0, supplier_status = 0) %>% # I assign 0  to order amount, total order and stuatus supplier
  rename(registration_year = year_supplier_registration) %>% # this is done before merging the two datasets
  select(bvd_id_number,  registration_year,supplier_status, year_orbis, # I select the variables of interest
         first_order, first_order_amount, total_orders_amount, operating_revenue_turnover_,
         fixed_assets, current_assets, ebitda, total_assets, intangible_fixed_assets, p_l_after_tax, p_l_before_tax)


matched_potential_suppliers_orbis_final<- rbind(matched_suppliers_orbis, matched_potential_orbis)
write.csv(matched_potential_suppliers_orbis_final, here("Analysis", "data_proc", "matched_potential_suppliers_final_.csv"))
number_observations_variables<- matched_potential_suppliers_orbis_final %>% 
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

supplier_potential_matched <- rbind(matched_suppliers_orbis, matched_potential_orbis)# this puts the data together

supplier_potential_matched <- bind_rows(matched_suppliers_orbis, matched_potential_orbis)
# I am excluding the outliers following the paper by Kalemli-Ozcan

filtered_data <-  supplier_potential_matched[ supplier_potential_matched$operating_revenue_turnover_ < quantile(supplier_potential_matched$operating_revenue_turnover_, 0.99, na.rm = TRUE),] 


filtered_companies <- unique(filtered_data$bvd_id_number)

outliers<- supplier_potential_matched %>% filter(bvd_id_number %notin%  filtered_companies)
supplier_potential_matched_no_outliers<- supplier_potential_matched %>% filter(bvd_id_number %in% filtered_companies)
matched_suppliers_by_country<- supplier_potential_matched_no_outliers_no_city %>% dplyr::filter(status_supplier ==1) %>%  
    dplyr::select(bvd_id_number, country) %>% distinct() %>% dplyr::group_by(country) %>%  dplyr::summarize(number_companies = n())
matched_potential_suppliers_by_country<- supplier_potential_matched_no_outliers_no_city %>% dplyr::filter(status_supplier ==0) %>%  
  dplyr::select(bvd_id_number, country) %>% distinct() %>% dplyr::group_by(country) %>%  dplyr::summarize(number_companies = n())


matched_suppliers_by_country$share<- prop.table(matched_suppliers_by_country$number_companies)*100
matched_potential_suppliers_by_country$share<- prop.table(matched_potential_suppliers_by_country$number_companies)*100


country_plot_matched_suppliers_share<- ggplot(matched_suppliers_by_country, aes(x = reorder(as.factor(country),-share), y = share, fill= country)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "% Total suppliers matched", x= "")+
  theme(legend.position="none",
        text = element_text(size = 12))
country_plot_matched_pot_suppliers_share<- ggplot(matched_potential_suppliers_by_country, aes(x = reorder(as.factor(country),-share), y = share, fill= country)) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "% Total pot. suppliers matched", x= "")+
  theme(legend.position="none",
        text = element_text(size = 12))+
ylim(0, 50)  # Set the y-axis limits (e.g., from 0 to 100)

#supplier_potential_matched<- left_join(supplier_potential_matched, all_addresses)# why am I doing this?
#supplier_potential_matched<- supplier_potential_matched %>% select(-city, -postcode)
supplier_potential_matched_no_outliers<- supplier_potential_matched_no_outliers %>% distinct()

operating_revenue<-t.test(supplier_potential_matched_no_outliers$operating_revenue_turnover_ ~ status_supplier, data = supplier_potential_matched_no_outliers)
ebitda<- t.test(supplier_potential_matched_no_outliers$ebitda ~ status_supplier, data = supplier_potential_matched_no_outliers)
p_l_after_tax<-t.test(supplier_potential_matched_no_outliers$p_l_after_tax ~ status_supplier, data = supplier_potential_matched_no_outliers)
p_l_before_tax<- t.test(supplier_potential_matched_no_outliers$p_l_before_tax ~ status_supplier, data = supplier_potential_matched_no_outliers)
total_assets<- t.test(supplier_potential_matched_no_outliers$total_assets ~ status_supplier, data = supplier_potential_matched_no_outliers)
fixed_assets<- t.test(supplier_potential_matched_no_outliers$fixed_assets ~ status_supplier, data = supplier_potential_matched_no_outliers)
current_assets<- t.test(supplier_potential_matched_no_outliers$current_assets ~ status_supplier, data = supplier_potential_matched_no_outliers)
p_value_matched<- c(operating_revenue$p.value,ebitda$p.value, p_l_after_tax$p.value, p_l_before_tax$p.value)

summary_supplier_potential_matched_no_outliers<- supplier_potential_matched_no_outliers %>% group_by(status_supplier) %>% 
  summarize_at(vars(operating_revenue_turnover_,ebitda, p_l_after_tax, p_l_before_tax),
               funs(mean = mean(.,na.rm = TRUE))) # calculate the mean of each variable, excluding missing values


summary_suppliers_potential_matched_outliers_long<- summary_supplier_potential_matched_no_outliers%>%
  pivot_longer(!status_supplier, names_to = "mean", values_to = "Value") 

summary_suppliers_potential_matched_outliers_long$Value<- summary_suppliers_potential_matched_outliers_long$Value/1000000

summary_suppliers<- summary_suppliers_potential_matched_outliers_long %>% 
  filter(status_supplier ==1) %>%
  dplyr::select(-status_supplier) %>% 
  rename(Suppliers = Value)

summary_potential<-  summary_suppliers_potential_matched_outliers_long %>% 
  filter(status_supplier ==0) %>%
  dplyr::select(-status_supplier) %>% 
  rename(Potential = Value)

summary_suppliers_potential <- left_join(summary_suppliers, summary_potential) %>% 
  mutate(difference = Suppliers - Potential)

summary_suppliers_potential<- summary_suppliers_potential %>% mutate(p_value = p_value_matched)
summary_suppliers_potential<- summary_suppliers_potential %>% rename (Variable = mean)

table_3 <- xtable(summary_suppliers_potential, digits=2, caption = "Summary statistics for potential and suppliers")
table_3 <- print(table_3, include.rownames = TRUE)

# Save LaTeX table code to a file
write.table(table_3, here("results", "tables", "table_3.tex"), sep = "", row.names = TRUE, col.names = TRUE, quote = FALSE)



ggplot(supplier_potential_matched,  aes(x = status_supplier,  y =log(operating_revenue_turnover_), fill = status_supplier)) +
  geom_boxplot()  +
  facet_wrap(~ status_supplier)


count_by_country <- supplier_potential_matched_no_outliers %>% select(country, bvd_id_number) %>% 
  distinct() %>% group_by(country) %>% dplyr::summarise(number_companies = n())
count_by_country$share_amount<- prop.table(count_by_country$number_companies)*100

countries_matched_orders$share_amount<- prop.table(countries_matched_orders$total_amount)*100


# Quantitative analysis ---------------------------------------------------

## Create yearly change


# Specify the variables for which you want to calculate the yearly change
variables_to_calculate <- c("operating_revenue_turnover_", "ebitda", "p_l_after_tax", "p_l_before_tax", "total_assets")

# Calculate the yearly change for the specified variables
supplier_potential_matched_no_outliers<-supplier_potential_matched_no_outliers %>% 
  group_by(bvd_id_number) %>%
  arrange(year_orbis) %>% 
  mutate(operating_revenue_turnover_change = (operating_revenue_turnover_ - lag(operating_revenue_turnover_)) / lag(operating_revenue_turnover_),
         ebitda_change = (ebitda - lag(ebitda)) / lag(ebitda),
         p_l_before_tax_change = (p_l_before_tax - lag(p_l_before_tax)) / lag(p_l_before_tax),
         p_l_after_tax_change = (p_l_after_tax - lag(p_l_after_tax)/lag(p_l_after_tax)))

supplier_potential_matched_no_outliers_no_city <- supplier_potential_matched_no_outliers %>% 
  select(-city) %>% distinct()
  

panelview(ebitda ~ first_order, data= supplier_potential_matched_no_outliers_no_city, index = c("bvd_id_number", "year_orbis"), 
          xlab = "Year", ylab = "bvd")



missing_years <- supplier_potential_matched_no_outliers_no_city%>%
   dplyr::group_by(bvd_id_number) %>%
  dplyr:: summarize(missing_years = toString(setdiff(min(year_orbis):max(year_orbis), year_orbis)))

test<- supplier_potential_matched_no_outliers %>% select(bvd_id_number, year_orbis) %>% 
  group_by(bvd_id_number, year_orbis) %>% 
  dplyr::summarise(number_observation = n())
companies_more_observations <- test %>% filter(number_observation>1)
companies_more_observations_list<- unique(companies_more_observations$bvd_id_number)

supplier_potential_matched_no_outliers_no_city_1995<- supplier_potential_matched_no_outliers_no_city %>% 
  filter(year_orbis >1994 & year_orbis <2020)

check_coverage <- supplier_potential_matched_no_outliers_no_city_1995 %>% 
  dplyr::group_by(bvd_id_number) %>% 
  dplyr::summarize(first_year= min(year_orbis), last_year = max(year_orbis), first_order = min(first_order), number_years = n())


year_orbis<- seq(1995, 2019)
year_orbis <- rep(1995:2019, each = 591)
bvd_id_number <- unique(supplier_potential_matched_no_outliers_no_city_1995$bvd_id_number)
bvd_id_number<-(rep(bvd_id_number, each = 25))
merged_data <- expand.grid(year_orbis = unique(year_orbis), bvd_id_number = unique(bvd_id_number))
merged_data <- merged_data[order(merged_data$bvd_id_number, merged_data$year_orbis), ]

supplier_potential_matched_no_outliers_no_city_1995_full<- left_join(merged_data, supplier_potential_matched_no_outliers_no_city_1995)
supplier_potential_matched_no_outliers_no_city_1995_full<- supplier_potential_matched_no_outliers_no_city_1995_full %>% 
  dplyr::group_by(bvd_id_number) %>% 
  fill(code_1_digit, registration_year, status_supplier, two_digit_nace, first_order, first_order_amount, 
       total_orders_amount, well_balanced, balance_over_time, country)


vis_miss(supplier_potential_matched_no_outliers_no_city_1995_full)

# # Joining with patent data ----------------------------------------------

supplier_potential_matched_no_outliers_no_city_1995_patent<- left_join(supplier_potential_matched_no_outliers_no_city_1995_full, panel_data_patents_1995_2017)
supplier_potential_matched_no_outliers_no_city_1995_patent<- supplier_potential_matched_no_outliers_no_city_1995_patent %>% 
  select(-lag_cum_applications, -stock_patent,-lag_stock_patent, -lag_patent_stock)
supplier_potential_matched_no_outliers_no_city_1995_patent<- supplier_potential_matched_no_outliers_no_city_1995_patent %>% 
  mutate(patent_stock = ifelse(is.na(patent_stock),0,patent_stock))

patent_stock_t.test<-t.test(supplier_potential_matched_no_outliers_no_city_1995_patent$patent_stock ~ status_supplier, data = supplier_potential_matched_no_outliers)


<- data %>%
  mutate(patent_stockit = patent_stockit-1 * (1 - ρ) + patentit)

# Imputing data -----------------------------------------------------------
variables_to_be_imputed<- c("operating_revenue_turnover_", "fixed_assets", "current_assets", "ebitda",
                            "total_assets", "intangible_fixed_assets", "p_l_after_tax", "p_l_before_tax")
mice_data <- futuremice(supplier_potential_matched_no_outliers_no_city_1994_full[,c("bvd_id_number","year_orbis", variables_to_be_imputed)], m = 5, maxit = 50, cluster.seed = 123,cl.type = "FORK")
imputed_data <- complete(mice_data, action = "long", include = TRUE)
data_imputed <- merge(supplier_potential_matched_no_outliers_no_city_1994_full, imputed_data, by = c("bvd_id_number", "year_orbis"), all.x = TRUE)

# Generate imputed data for selected columns
imputed_data <- complete(mice_data, action = "long", include = TRUE)

# Combine the imputed data with the original data
data_imputed <- merge(data, imputed_data, by = intersect(names(data), columns_to_impute))

# missing years by variable and company 

supplier_potential_matched_no_outliers_no_city_1995_2017<- supplier_potential_matched_no_outliers_no_city_1995_full %>% 
  filter(year_orbis<2018)



missing_years <- supplier_potential_matched_no_outliers_no_city_1995_2017%>%
  select("bvd_id_number","year_orbis","operating_revenue_turnover_", "fixed_assets", "current_assets", "ebitda",
         "total_assets", "intangible_fixed_assets", "p_l_after_tax", "p_l_before_tax") %>% 
  pivot_longer(cols = -c(bvd_id_number, year_orbis), names_to = "variable", values_to = "value") %>%
  dplyr::group_by(bvd_id_number, variable) %>%
  dplyr::summarize(missing_years = sum(is.na(value)),
            years_missing = paste0(ifelse(is.na(value), year_orbis, ""), collapse = ", "))

check_firms <- missing_years %>% 
  dplyr::filter(missing_years <6) %>% 
  dplyr::select(bvd_id_number, variable) %>% distinct()

number_companies_least_missing <- unique(check_firms$bvd_id_number)

supplier_potential_matched_no_outliers_no_city_1995_2017_clean<- supplier_potential_matched_no_outliers_no_city_1995_2017 %>% 
  filter(bvd_id_number %in% number_companies_least_missing)


# Check again for number of years                 

# Now I want to understand which variable has the least number of missing years across firms 

variable_with_least_missing_years_by_firms <- missing_years %>%
  filter(missing_years == min(missing_years))

variable_with_least_missing_years <- missing_years %>%
  arrange(missing_years) %>%
  slice(1)

print(variable_with_least_missing_years)


cleal_years<- 

# Attempt to impute data
  
missing_vars <- c("fixed_assets", "current_assets", "ebitda", "total_assets", 
                  "intangible_fixed_assets", "p_l_after_tax", "p_l_before_tax", "operating_revenue_turnover_change", 
                  "ebitda_change", "p_l_before_tax_change", "p_l_after_tax_change")
imputed_data<- supplier_potential_matched_no_outliers_no_city_1995_2017_clean %>% select(bvd_id_number, year_orbis, ebitda, 
                                                                                        p_l_after_tax, 
                                                                                         p_l_before_tax)

imp_formula <- c("p_l_before_tax ~ lead(p_l_before_tax) + lag(p_l_before_tax) + lead(p_l_after_tax) + lag(p_l_after_tax) + lead(ebitda) + lag(ebitda)",
                 "p_l_after_tax ~ lead(p_l_before_tax) + lag(p_l_before_tax) + lead(p_l_after_tax) + lag(p_l_after_tax) + lead(ebitda) + lag(ebitda)",
                 "ebitda ~ lead(p_l_before_tax) + lag(p_l_before_tax) + lead(p_l_after_tax) + lag(p_l_after_tax) + lead(ebitda) + lag(ebitda)")
imputed_data<- amelia(imputed_data, m=5, ts = "year_orbis", cs = "bvd_id_number", formula =  imp_formula, polytime= 3, intercs = TRUE,p2s =2)
imputed_data<- amelia(supplier_potential_matched_no_outliers_no_city_1995_2017_clean)

tscsPlot(imputed_data, cs = "ESB60557659", main = "ebitda", var = "ebitda")


for (i in 1:length(imputed_data$imputations)) {
  imp_data <- imputed_data$imputations[[i]]
  
  # Print a summary of the imputed dataset
  print(summary(imp_data))
  
  # Or perform any other operations or analyses on the imputed dataset
  # For example, you can calculate means, plot histograms, or compute correlations
  
  # Access specific variables in the imputed dataset
  # For example, to access the "age" variable
  ebitda <- imp_data$ebitda
  print(ebitda)
  
  # Or visualize the data
  # For example, to create a histogram of the "income" variable

}

# Attempt with mice
panel_format <- pdata.frame(imputed_data,index = c("bvd_id_number", "year_orbis"), drop.index = F)
panel_format$time <- interaction(panel_format$year_orbis, panel_format$bvd_id_number, drop = T)
mice_obj <- mice(panel_format, method = "cart",  m = 10, include= c("bvd_id_number", "year_orbis"))

# Step 3: Specify the time variable and clustering variable


supplier_panel<- pdata.frame(supplier_potential_matched_no_outliers_no_city_1995_2017_clean, index = c("bvd_id_number", "year_orbis"), drop.index  = F)
supplier_panel<- supplier_panel %>% select(-ebitda, -p_l_after_tax, -p_l_before_tax)
supplier_panel$year_orbis<-as.double(supplier_panel$year_orbis)
# Step 4: Run the imputation process
imputed_data_2 <- mice::complete(mice_obj, "long", include = TRUE)
imputed_data_2$year_orbis<- as.double(imputed_data_2$year_orbis)



supplier_imputed_panel<- left_join(imputed_data_2, supplier_panel)



testing$year <- as.numeric(as.character(testing$year_orbis))
testing$treatment <- ifelse(testing$year >= testing$first_order, 1, 0)
testing_treated<- testing %>% filter(status_supplier ==1)
firms_treated<- unique(testing_treated$bvd_id_number)

# Step 5: Access the imputed data
completed_data <- as.data.frame(imputed_data_2)
completed_data$bvd_id_number <- imputed_data$bvd_id_number[match(completed_data$.imp, completed_data$.time)]

treatment_distribution <- aggregate(treatment ~ year_orbis, data = testing_treated, FUN = function(x) sum(x)/length(x))

treatment_count <- testing_treated %>% filter(.imp ==0)
cumulative_treated <- aggregate(treatment ~ year_orbis, data = testing_treated, FUN = function(x) sum(x)/length(x))
treated_firms <- testing_treated %>%
  filter(.imp ==0) %>% 
  dplyr::group_by(year_orbis) %>%
  dplyr::summarize(num_treated_firms = sum(treatment),
                   num_untreated_firms = 166- sum(treatment))

treated_firms_long <- treated_firms %>%
  pivot_longer(cols = c(num_treated_firms, num_untreated_firms), names_to = "group", values_to = "count")

bar_chart <- ggplot(treated_firms_long, aes(x = year_orbis, y = count, fill = group)) +
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Number of Firms", fill = "Group") +
  scale_fill_manual(values = c("num_treated_firms" = "blue", "num_treated_firms" = "blue")) +
  theme_minimal()


# Display the bar chart
print(bar_chart)
untreated_firms <- testing_treated %>%
  filter(.imp ==0) %>% 
  dplyr::group_by(year_orbis) %>%
  dplyr::summarize(num_untreated_firms =166- sum(treatment))




treated_untreted

bar_chart <- ggplot(treated_firms, aes(x = year_orbis, y = num_treated_firms, fill = factor(treatment))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Number of Firms", fill = "Treatment") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()



cumulative_treated <- treatment_count %>%
  dplyr::group_by(year_orbis) %>%
  dplyr::summarize(cumulative_firms_treated = cumsum(treatment))

treatment_distribution<-table(treatment_count$year_orbis, treatment_count$treatment)
treatment_distribution<- as.data.frame(treatment_distribution)

# Create the bar chart
ggplot(data = as.data.frame(treatment_distribution), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Treatment by Year") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))


# Difference-in-difference ------------------------------------------------


testing_treated <- as.data.frame(testing_treated)
testing_treated_1 <- testing_treated %>% 
  select(-ebitda_change, -p_l_after_tax_change, -p_l_before_tax_change,-operating_revenue_turnover_change)

testing_treated_1$p_l_after_tax<- as.numeric(testing_treated_1$p_l_after_tax)
testing_treated_1$p_l_before_tax<- as.numeric(testing_treated_1$p_l_before_tax)
testing_treated_1$ebitda<- as.numeric(testing_treated_1$ebitda)
testing_treated_change<- testing_treated_1%>% 
  distinct() %>% 
  group_by(bvd_id_number, .imp,) %>% 
  arrange(year_orbis) %>% 
  mutate(p_l_before_tax_lag = lag(p_l_before_tax,1))
           
           (p_l_before_tax/lag(p_l_before_tax)-1)*100),
        ebitda_change = (ebitda/lag(ebitda)-1)*100),
        p_l_after_tax = (p_l_after_tax/lag(p_l_after_tax)-1)*100)

testing_treated_change$lag_ebitda<- lag(testing_treated_change$ebitda)

                                  NA, 
                                  100 * diff(p_l_before_tax)),
         ebitda_change = ifelse(is.na(ebitda) | is.na(lag(ebitda)),
                                NA, 
                                100 * diff(ebitda)),
         p_l_after_tax_change = ifelse(is.na(p_l_after_tax) | is.na(lag(p_l_after_tax)),
                                       NA,  
                                       100 * diff(p_l_after_tax)))




# View the updated dataset with yearly changes
head(testing_treated_change)
testing_treated_change$year_orbis<-as.numeri
testing_treated_change$lag_ebitda<- lag(testing_treated_change$ebitda)
testing_treated_change$id_company<- as.numeric(testing_treated_change$bvd_id_number)
testing_treated_change_panel<-pdata.frame(testing_treated_change, index = c("bvd_id_number", "year_orbis"), drop.index = F)
testing_treated_change_panel$ebitda_change <- lag(testing_treated_change_panel$ebitda)
testing_treated_change_panel$ebitda_change<-testing_treated_change_panel$ebitda/testing_treated_change_panel$ebitda_change
testing_treated_change_panel$ebitda_perc_change<- (testing_treated_change_panel$ebitda_change - 1)*100


testing_treated_change_panel <- testing_treated_change_panel %>% mutate(average_change_ebitda = rollmean(ebitda_perc_change, k = 3, fill = NA))
       average_change_p_l_before_tax = rollmean(year_change_p_l_before_tax, k=3, fill = NA),
       average_change_p_l_after_tax = rollmean(year_change_p_l_after_tax, k=3, fill = NA))


attempt<-att_gt(yname = "average_change_ebitda",
       gname = "first_order",
       idname = "id_company",
       tname = "year",
       xformla = ~1,
       data = testing_treated_change_panel,
       control_group = "notyettreated",
       panel = FALSE
)

attempt.dyn<- aggte(attempt, type = "dynamic")
summary(attempt.dyn)
ggdid(attempt.dyn)


# DiD with patent data ----------------------------------------------------
high_tech_order <- c(3,4,5,7)# I use the information provided by CSIL
low_tech_order<- c(1,2,6) #

matched_orders<- matched_orders %>% 
  mutate(technology_level = case_when(tech_intensity %in% high_tech_order ~"high-tech", 
                                      tech_intensity %in% low_tech_order ~ "low-tech"))


supplier_patent_data <- supplier_potential_matched_no_outliers_no_city_1995_patent %>% filter(status_supplier ==1)
tech_data<- cern_orbis_4_years %>% select(bvd_id_number, first_order_tech, total_orders, first_order_amount)
supplier_patent_data<- supplier_patent_data %>% left_join(tech_data)
supplier_patent_data<-supplier_patent_data %>% 
  mutate(technology_level = case_when(first_order_tech %in% high_tech_order ~"high-tech", 
                                      first_order_tech %in% low_tech_order ~ "low-tech"))

supplier_patent_data_panel<- pdata.frame(supplier_patent_data, index = c("bvd_id_number", "year_orbis"))
supplier_patent_data_panel$id_number<- as.numeric(supplier_patent_data_panel$bvd_id_number)
supplier_patent_data_panel$year<- as.numeric(as.character(supplier_patent_data_panel$year_orbis))
supplier_patent_data_panel$first_order<- as.numeric(supplier_patent_data_panel$first_order)

patent_did_data<- supplier_patent_data_panel %>% select(id_number,year, first_order,patent_stock, technology_level, first_order_amount)
patent_did_data_high_tech<- patent_did_data %>% filter(technology_level =="high-tech")
patent_did_data_high_tech$log_patent_stock<- log(patent_did_data_high_tech$patent_stock +0.00000000001)
patent_did<-att_gt(yname = "log_patent_stock",
                gname = "first_order",
                idname = "id_number",
                tname = "year",
                xformla = ~1,
                data = patent_did_data,
                control_group = "notyettreated",
                panel = FALSE
)



patent.dyn<- aggte(patent_did, type = "dynamic")
summary(patent.dyn)
ggdid(patent.dyn)

patent_did_high_tech<-att_gt(yname = "log_patent_stock",
                   gname = "first_order",
                   idname = "id_number",
                   tname = "year",
                   xformla =~1,
                   est_method = "dr",
                   data = patent_did_data_high_tech,
                   control_group = "notyettreated",
                   panel = FALSE
)

agg_effects_group <- aggte(patent_did_high_tech, type = "group", na.rm = TRUE)
summary(agg_effects_group)           
summary()
agg_effects_es <- aggte(patent_did_high_tech, type = "dynamic", na.rm = TRUE)
ggdid(agg_effects_es)
ggdid(patent_did_high_tech)

patent_high_tech.dyn<- aggte(patent_did_high_tech, type = "dynamic", na.rm = TRUE)
summary(patent_high_tech.dyn)
ggdid(patent_high_tech.dyn)


es <- aggte(patent_did_high_tech,
            type = "dynamic", balance_e = 1)

ggdid(es)

aggte(patent_did_high_tech, type = "simple", na.rm = TRUE)


# Matching ----------------------------------------------------------------
supplier_potential_matched_no_outliers_no_city_1995_patent$log_patent_stock<- log(supplier_potential_matched_no_outliers_no_city_1995_patent$patent_stock +0.00000000001)



t.test(log_patent_stock ~ treatment, data = supplier_potential_matched_no_outliers_no_city_1995_patent)
model_1 <- lm(log_patent_stock ~ treatment + first_order_amount + ebitda +country, data = supplier_potential_matched_no_outliers_no_city_1995_patent)
supplier_potential_matched_no_outliers_no_city_1995_patent$treatment <- ifelse(supplier_potential_matched_no_outliers_no_city_1995_patent$year_orbis >= supplier_potential_matched_no_outliers_no_city_1995_patent$first_order, 1, 0)
supplier_potential_matched_no_outliers_no_city_1995_patent$treatment[is.na(supplier_potential_matched_no_outliers_no_city_1995_patent$treatment)] <- 0

X<- supplier_potential_matched_no_outliers_no_city_1995_patent %>% ungroup() %>% dplyr::select(two_digit_nace, ebitda, country) 
X<- X %>% 
matchmaha1 <- Match(Tr = supplier_potential_matched_no_outliers_no_city_1995_patent$treatment, X = X, estimand = "ATT", M = 1,
                    ties = TRUE, replace = TRUE, Weight = 2)


ps_model<- glm(treatment ~ operating_revenue_turnover_ + two_digit_nace + country +ebitda , data = supplier_potential_matched_no_outliers_no_city_1995_patent, family = "binomial")
propensity_scores <- ps_model$fitted.values
matched_data <- matchit(treatment ~ propensity_scores, method = "nearest", data = supplier_potential_matched_no_outliers_no_city_1995_patent)


## RDD -------------------------------------------------------------------







log_chf_amount <- log(all_orders$chf_amount)
all_orders %>% filter(chf_amount <10000000) %>% pull(log(chf_amount)) %>% 
  rdd
  log_chf_amount %>% 
  rddensity(c = 12.20607) 
  
  ggplot(all_orders, aes(x = log(chf_amount))) +
    geom_vline(xintercept = 12.20607) +
    geom_density() +
    labs(x = "Amount")
  
  test_density <- rdplotdensity(rdd = rddensity(log_chf_amount, c =  12.20607), 
                                X = log_chf_amount, 
                                type = "both")
