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


# Two-way FEs -------------------------------------------------------------

#### In this section here, I want to show that by using two-way fixed effects, the estimates might be driven by the wrong comparison. So what I do here is the following:
# First, I create the treat variable (this should not be so different from the suppliers status that I have). 
# Second, I create a balanced panel (after removing the outliers). I am doing this because the bacon decomposition package only works with a balanced panel
# Third, I use the TWFE and  bacon decomposition: this shows how much weight is given to the variables
### When doing the TWFE with covariates, I basically follow the approach of Burgess et al. (2015)  here:https://www.aeaweb.org/articles?id=10.1257/aer.20131031 
#where the year is interacted with the time-invariant covariate (If I don't do this, you have the issue of collinearity)

# 3. TWFE -----------------------------------------------------------------

# Run static TWFE, with SEs clustered at the state level

# Step 1: Load necessary libraries
library(dplyr)

# Step 2: Check the count of missing values for each variable
missing_summary <- sapply(full_panel[, c("pre_log_fixed_assets", "age")], function(x) sum(is.na(x)))
print(missing_summary)

# Step 3: Identify rows with missing values in the specified variables
missing_data <- full_panel %>%
  filter(is.na(pre_log_fixed_assets) | is.na(age) | is.na(country))

# Display the number of rows with missing data
cat("Number of observations with missing data:", nrow(missing_data), "\n")

# Step 4: Create additional columns to indicate which variables are missing
missing_details <- missing_data %>%
  mutate(
    missing_pre_log_fixed_assets = is.na(pre_log_fixed_assets),
    missing_age = is.na(age),
    missing_country = is.na(country)
  )

# Display the first few rows of the missing details to understand the missing pattern
print(head(missing_details))


## 3.1 Create IPW ( a bit of text on how this works) -----------------------------------------------------------------
full_panel <- full_panel %>% mutate(postTreated = first_order>0 & year >= first_order,
                                    time_to_treat = ifelse(supplier_status == 0, -3000, time_to_treat))

full_panel_clean <- full_panel %>%
  filter(!is.na(pre_log_fixed_assets) & !is.na(age) & !is.na(country) & !is.na(nace_rev_2_main_section)) %>% 
  filter(first_order %notin% c(1995,1996, 2008))



c("pre_log_operating_revenue_turnover","pre_log_fixed_assets", "age")
logit_reg <- glm(postTreated ~ pre_log_fixed_assets+pre_log_operating_revenue_turnover,
                 data = full_panel_patenting %>% filter(first_order %notin% c(1995,1996, 2008)),
                 family = binomial(link ='logit'))

# Generate propensity score 
full_panel_patenting$prop_scores<- logit_reg$fitted.values

full_panel_patenting<- full_panel_patenting %>% 
  mutate(ipw = (postTreated/ prop_scores)+ ((1- postTreated)/(1-prop_scores)))
summary(full_panel_patenting$ipw)

# Define the truncation thresholds
upper_threshold <- 10  # Set the maximum acceptable value for IPWs
lower_threshold <- 0.1  # Set the minimum acceptable value (optional, depending on data)

# Apply truncation to the IPWs
full_panel_patenting$ipw_truncated <- pmin(pmax(full_panel_patenting$ipw, lower_threshold), upper_threshold)

# Summary of truncated weights to verify the truncation
summary(full_panel_patenting$ipw_truncated)

model_static <- feols(number_applications ~ postTreated| 
                        year+bvd_id_number, data = full_panel_patenting %>% 
                        filter(first_order %notin% c(1995,1996, 2008)),weights = full_panel_patenting$ipw_truncated,cluster = "bvd_id_number")






model_ols_ipw_log_applications <- feols(
  log_applications ~ postTreated | bvd_id_number + year,  # Specify the model formula
  data = full_panel_patenting %>% filter(first_order %notin% c(1995,1996, 2008)),# The data frame
  weights =  ~ ipw_truncated, 
  cluster= "bvd_id_number"# Include IPW weights
)

model_ols_ipw_asinh_applications <- feols(
  asinh_applications ~ postTreated | bvd_id_number + year,  # Specify the model formula
  data = full_panel_patenting %>% filter(first_order %notin% c(1995,1996, 2008)),# The data frame
  weights =  ~ ipw_truncated, 
  cluster= "bvd_id_number"# Include IPW weights
)

model_ols_ipw_stock <- feols(
 log_application_stock ~ postTreated | bvd_id_number + year,  # Specify the model formula
  data = full_panel_patenting %>% filter(first_order %notin% c(1995,1996, 2008)),# The data frame
  weights =  ~ ipw_truncated, 
  cluster= "bvd_id_number"# Include IPW weights
)

model_ols_ipw_probability <-feols(
  probability_applications~ postTreated | bvd_id_number + year,  # Specify the model formula
  data = full_panel_patenting %>% filter(first_order %notin% c(1995,1996, 2008)),# The data frame
  weights =  ~ ipw_truncated, 
  cluster= "bvd_id_number"# Include IPW weights
)


model_qml_poisson_ipw <- fepois(
  number_applications ~ postTreated | bvd_id_number + year,  # Specify the model formula
  data = full_panel_patenting %>% filter(first_order %notin% c(1995,1996, 2008)),# The data frame
  weights =  ~ ipw_truncated, 
  cluster= "bvd_id_number"# Include IPW weights
)

# Turning the data into balanced panel 

# Step 1: Define the range of years that must be present
required_years <- 1998:2018

# Step 2: Identify entities that have data for all required years
# Group by entity (bvd_id_number) and count the number of unique years
entities_with_complete_data <- full_panel_patenting %>%
  group_by(bvd_id_number) %>%
  summarise(year_count = sum(year %in% required_years)) %>%
  filter(year_count == length(required_years)) %>%
  pull(bvd_id_number)  # Extract the IDs of entities that meet the criteria

# Step 3: Subset the data to keep only the entities with complete data from 1998 to 2018
full_panel_balanced <- full_panel_patenting %>%
  filter(bvd_id_number %in% entities_with_complete_data & year %in% required_years)

check <- full_panel_balanced %>% 
  group_by(bvd_id_number) %>% summarise(number_treat  = sum(treat))

check <- check %>% filter(number_treat ==0) %>% pull(bvd_id_number)

# Step 4: Inspect the resulting balanced panel
summary(full_panel_balanced)
head(full_panel_balanced)



bacon(log_applications ~ treat,
      data = full_panel_balanced %>% filter(bvd_id_number %notin% check),
      id_var = "bvd_id_number",
      time_var = "year"
)






hist(full_panel_clean$ipw, 
     breaks = 50, 
     col = "blue", 
     border = "black", 
     main = "Distribution of IPW", 
     xlab = "IPW", 
     ylab = "Frequency")

full_panel <- full_panel %>% mutate(postTreated = first_order>0 & year >= first_order,
                                    time_to_treat = ifelse(supplier_status == 0, -3000, time_to_treat))




model_static_ht<- feols(log_application_stock~ +(year*first_order_tech)+ postTreated| 
                                       year+bvd_id_number, data = full_panel %>% filter(first_order %notin% c(1995,1996, 2008)), cluster= "bvd_id_number" )  







etable(model_static)
iplot(model_static_ht)
                          summary(fixef(model_static_ht))
res_effect_1 <- feols(log_application_stock ~ i(first_order_tech)
                    + sunab(first_order, year)| year +bvd_id_number, full_panel %>% filter(first_order %notin% c(1995,1996, 2008)))
res_effect_2<-feols(log_applications ~
                      + sunab(first_order, year)| year +bvd_id_number[first_order_tech], full_panel%>% filter(first_order %notin% c(1995,1996, 2008)))
iplot(res_effect_1)
iplot(res_effect_2)
summary(res_effect_2, agg = "att")
## FEs
twfe_static <- feols(dins ~ postTreated | stfips + year, data = df, cluster = "stfips")

summa

### Create the filtered dataset  (removin the outliers)
