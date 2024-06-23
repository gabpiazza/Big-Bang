#' ---
#' title: 02_analysis_B
#' author: Gabriele Piazza
#' date: 2024-06-20
#' Description: This scripts does some descriptive analysis
#' The data was created using scripts 01_dataprep_A,B,C,D
#' 
# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c(
  "Matching", "panelView", "tjbal", "MatchIt", "WeightIt", "tidyverse", 
  "rstatix", "bacondecomp", "ggpubr", "gsynth", "did", "modelsummary", 
  "hrbrthemes", "tidyr", "viridis", "janitor", "tmap", "leaflet", "sf", 
  "terra", "cobalt", "here", "dplyr", "spData", "rnaturalearth", 
  "rnaturalearthdata", "readxl", "tabulator", "Hmisc", "skimr", 
  "tjbal", "fuzzyjoin", "reshape2", "easycsv", "Synth", "plm", 
  "progress", "lfe", "fixest", "did", "stringi", "SCtools", "tidysynth", 
  "modelsummary", "panelView", "eeptools", "rdd", "haven", "sp", 
  "spdep", "forcats", "tmap", "gridExtra", "xtable", "weights", 
  "twang", "scales", "fixest", "beepr", "naniar", "stargazer", 
  "foreign", "knitr", "kableExtra", "data.table", "visdat", "lubridate", "fixest"
) # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)
## 1.2 Create functions ----------------------------------------------------
`%notin%` <- Negate(`%in%`)



# 2. Directories,  loading data and setting up variables-----------------------------------------


## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data folders 
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
full_panel_file <- "full_panel"



## 2.2 Loading the data  --------------------------------------------------------------------
full_panel<- readRDS(paste0(data_proc_dir, full_panel_file))



full_panel



# Two-way FEs -------------------------------------------------------------
#### In this section here, I want to show that by using two-way fixed effects, the estimates might be driven by the wrong comparison. So what I do here is the following:
# First, I create the treat variable (this should not be so different from the suppliers status that I have). 
# Second, I create a balanced panel (after removing the outliers). I am doing this because the bacon decomposition package only works with a balanced panel
# Third, I use the TWFE and  bacon decomposition: this shows how much weight is given to the variables
### When doing the TWFE with covariates, I basically follow the approach of Burgess et al. (2015)  here:https://www.aeaweb.org/articles?id=10.1257/aer.20131031 
#where the year is interacted with the time-invariant covariate (If I don't do this, you have the issue of collinearity)

# 3. TWFE -----------------------------------------------------------------

# Run static TWFE, with SEs clustered at the state level


full_panel <- full_panel %>% mutate(postTreated = first_order>0 & year >= first_order,
                                    time_to_treat = ifelse(supplier_status == 0, -3000, time_to_treat))


model_static_ht<- feols(log_application_stock~  postTreated +(year*pre_log_ebitda) +i(max_tech)| 
                                       year+bvd_id_number, data = full_panel %>% filter(first_order %notin% c(1995,1996, 2008)), cluster= "bvd_id_number" )  
iplot(model_static_ht)
                          summary(fixef(model_static_ht))
res_effect_1 <- feols(log_application_stock ~ (year*pre_log_fixed_assets)
                    + sunab(first_order, year)| year +bvd_id_number, full_panel %>% filter(first_order %notin% c(1995,1996, 2008))
                    
                    )
res_effect_2<-feols(log_application_stock ~ i(max_tech)+
                      + sunab(first_order, year)| year +bvd_id_number, full_panel%>% filter(first_order %notin% c(1995,1996, 2008) & supplier_status ==1))
iplot(res_effect_1)
iplot(res_effect_2)
summary(res_effect_1, agg = "att")
## FEs
twfe_static <- feols(dins ~ postTreated | stfips + year, data = df, cluster = "stfips")

summa

### Create the filtered dataset  (removin the outliers)
