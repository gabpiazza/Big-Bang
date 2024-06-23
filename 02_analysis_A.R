#' ---
#' title: 02_analysis_A
#' author: Gabriele Piazza
#' date: 2024-06-20
#' Description: This scripts does some descriptive analysis
#' The data was created using scripts 01_dataprep_A,B,C,D
#' 
# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign","xtable" ,"knitr","kableExtra","haven", "fuzzyjoin", "data.table","panelView", "visdat", "beepr", "lubridate", "readxl") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)
## 1.2 Create functions ----------------------------------------------------
`%notin%` <- Negate(`%in%`)




## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data folders 
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"

full_panel_file <- "full_panel"
#Load the data
full_panel<- readRDS(paste0(data_proc_dir, full_panel_file))
top_1_percent_threshold <- quantile(full_panel$number_applications, 0.99)
top_1_percent_companies <- full_panel %>%
  filter(number_applications > top_1_percent_threshold) %>%
  select(bvd_id_number) %>% distinct() %>% 
  pull(bvd_id_number)
full_panel<-full_panel %>% filter(bvd_id_number %notin% top_1_percent_companies)

# ## 3. Descriptive -------------------------------------------------------

number_cohorts <- full_panel %>%select(bvd_id_number, first_order) %>%
  distinct() %>% 
  group_by(first_order) %>% 
  summarize(number = n()) %>% 
  distinct() %>% 
  mutate(cohort = paste0("Cohort", " ", first_order)) %>% 
  select(-first_order)

number_cohorts$cohort[number_cohorts$cohort=="Cohort 0"]<- "Never treated"

# Adding broader cohorts for grouping (e.g., 5-year periods)
number_cohorts <-number_cohorts %>%
  mutate(broader_cohort = case_when(
    cohort == "Never treated" ~ "Never treated",
    cohort %in% c("Cohort 1995", "Cohort 1996", "Cohort 1997", "Cohort 1998", "Cohort 1999") ~ "1995-1999",
    cohort %in% c("Cohort 2000", "Cohort 2001", "Cohort 2002", "Cohort 2003", "Cohort 2004") ~ "2000-2004",
    cohort %in% c("Cohort 2005", "Cohort 2006", "Cohort 2007", "Cohort 2008", "Cohort 2009") ~ "2005-2009",
    cohort %in% c("Cohort 2010", "Cohort 2011", "Cohort 2012", "Cohort 2013", "Cohort 2014") ~ "2010-2014",
    cohort %in% c("Cohort 2015", "Cohort 2016", "Cohort 2017", "Cohort 2018", "Cohort 2019", "Cohort 2020") ~ "2015-2020"
  ))

# Summarize data by broader cohorts
collapsed_data <- number_cohorts %>%
  group_by(broader_cohort) %>%
  summarise(number_of_firms = sum(number)) %>%
  mutate(percent_of_firms_in_cohort = (number_of_firms/sum(number_of_firms)) * 100)

# Convert percentage to string with percentage symbol
collapsed_data$percent_of_firms_in_cohort <- paste0(round(as.numeric(collapsed_data$percent_of_firms_in_cohort), 2), "%")

# Convert to xtable
xtable_data <- xtable(collapsed_data, caption = "Table 1: Number of firms by treatment cohort timing group (Collapsed)")


# Save the LaTeX code to a file
latex_code <- capture.output(print(xtable_data, include.rownames = FALSE, booktabs = TRUE, caption.placement = "top", 
                                   sanitize.text.function = function(x) {x}, add.to.row = list(pos = list(nrow(collapsed_data)), 
                                                                                               command = "\\hline \n \\multicolumn{3}{l}{\\textit{Note: Years 2008, 1995, 2020 have fewer than 20 observations.}} \n")))

# Write the LaTeX code to a .tex file
writeLines(latex_code, "table_1.tex")



