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


p1 <- quantile(full_panel$log_applications, 0.01)
p99 <- quantile(full_panel$log_applications, 0.99)

# Identify bvd_id_number for the top 1% and bottom 1% of patent_stock
outlier_ids <- full_panel %>%
  filter(log_applications >= p99) %>%
  distinct() %>% 
  pull(bvd_id_number) 



# Two-way FEs -------------------------------------------------------------
#### In this section here, I want to show that by using two-way fixed effects, the estimates might be driven by the wrong comparison. So what I do here is the following:
# First, I create the treat variable (this should not be so different from the suppliers status that I have). 
# Second, I create a balanced panel (after removing the outliers). I am doing this because the bacon decomposition package only works with a balanced panel
# Third, I use the TWFE and  bacon decomposition: this shows how much weight is given to the variables
### When doing the TWFE with covariates, I basically follow the approach of Burgess et al. (2015)  here:https://www.aeaweb.org/articles?id=10.1257/aer.20131031 
#where the year is interacted with the time-invariant covariate (If I don't do this, you have the issue of collinearity)

# 3. TWFE -----------------------------------------------------------------

# Run static TWFE, with SEs clustered at the state level

full_panel <- full_panel %>% mutate(postTreated = first_order>0 & year >= first_order)
model_static <- feols(log_applications ~ postTreated | 
                                       year+bvd_id_number, data = full_panel, cluster= "bvd_id_number" )                           ## FEs
twfe_static <- feols(dins ~ postTreated | stfips + year, data = df, cluster = "stfips")
summary(twfe_static)


### Create the filtered dataset  (removin the outliers)
