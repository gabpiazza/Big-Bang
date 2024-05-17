##Script name: 03_tables
##
##Purpose of script: Preparing the data for the paper "A bang for your backs
##Author: Gabriele Piazza
##
##Date Created: 2023-09-04
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes:
##   
##

#1 Load libraries, functions and data  ----------------------------------------------------------

###1.1 Load libraries ----------------------------------------------------

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
library(xtable)

###1.2 Load functions ----------------------------------------------------



# # Example usage
# object_names_table_4 <- c("cs_ht_results.sim", "cs_ht_not_yet_results.sim", "cs_cov_ht_results.sim", "cs_cov_ht_not_yet_results.sim")
# file_path <- "/path/to/your/directory/ResultTable.tex"
# caption_text <- "Summary of Model Parameters and 95% Confidence Intervals Based on the Fable"
# generate_latex_table(object_names_table_4, file_path, caption_text)


### 1.3 Load data -------------------------------------------------------------
folder_path <- here("results", "output")
descriptive_statistics<- read_csv(here("results", "output", "descriptive_statistics_pot_supplier.csv")) %>% 
  select(-'...1') 
combined_table_3 <- read_csv(here("results", "output", "descriptive_table_3.csv")) %>% 
  select(-'...1') 


# Get a list of all .rds files in the folder
rds_files <- list.files(path = folder_path, 
                        pattern = "(treated_log_age_fixed_assets|none)_simple_results.*\\.rds$",
                        full.names = F)

# Loop through the list of .rds files to read each one
for (file in rds_files) {
  # Create an object name based on the file name
  obj_name <- tools::file_path_sans_ext(file)
  
  # Construct the full path of the file
  full_path <- file.path(folder_path, file)
  
  # Load the .rds file into an object
  obj <- readRDS(full_path)
  
  # Assign the object to a variable with the same name as the original file (without extension)
  assign(obj_name, obj, envir = .GlobalEnv)
}

generate_latex_table <- function(object_names, file_path, caption) {
  # Check if xtable is installed and load it
  if (!requireNamespace("xtable", quietly = TRUE)) {
    stop("xtable package is not installed. Please install it before using this function.")
  }
  
  # Initialize an empty list to hold the results
  result_list <- list()
  
  # Z-value for 95% confidence interval
  z_value <- 1.96
  
  # Loop through each object to extract information
  for (obj_name in object_names) {
    if (!exists(obj_name)) {
      cat("Object", obj_name, "does not exist.\\n")
      next
    }
    obj <- get(obj_name)
    
    # Check for the required attributes
    required_attributes <- c("overall.att", "overall.se", "DIDparams")
    if (!all(required_attributes %in% names(obj))) {
      cat("Object", obj_name, "does not have the required attributes.\\n")
      next
    }
    
    # Extract and process data from the object
    overall_att <- obj$overall.att
    overall_se <- obj$overall.se
    lower_bound <- overall_att - (z_value * overall_se)
    upper_bound <- overall_att + (z_value * overall_se)
    
    # Determine the significance stars
    stars <- ifelse(lower_bound > 0 | upper_bound < 0, ifelse(abs(overall_att / overall_se) >= 2.58, "***", ifelse(abs(overall_att / overall_se) >= 1.96, "**", "*")), "")
    
    # Create formatted strings
    att_string <- paste(formatC(overall_att, format = 'f', digits = 2), stars, "(", formatC(overall_se, format = 'f', digits = 2), ")", sep = "")
    number_observations <- formatC(length(obj$DIDparams$data$.rowid), format = "d", big.mark = ",")
    ci_string <- paste("[", formatC(lower_bound, format = 'f', digits = 2), ", ", formatC(upper_bound, format = 'f', digits = 2), "]", sep = "")
    
    # Check if covariates are included and format accordingly
    covariates_included <- ifelse(!is.null(obj$DIDparams$covariates) && length(obj$DIDparams$covariates) > 0, "Yes", "No")
    
    # Add the processed data to the result list
    result_list[[obj_name]] <- c(`ATT (SE)` = att_string, `CI [Lower, Upper]` = ci_string, Observations = number_observations, Covariates = covariates_included)
  }
  
  # Convert the list to a data frame and transpose it
  result_df <- as.data.frame(do.call(rbind, result_list))
  result_df <- t(result_df)
  
  # Define the alignment of the columns
  align <- c("l", rep("c", ncol(result_df)))
  
  # Specify rows to add and their format
  addtorow <- list()
  addtorow$pos <- list(0, nrow(result_df) + 1)  # Positions after the column names and at the end of the table
  addtorow$command <- c(
    "\\hline \\multicolumn{2}{c}{(log) Patent stock} & \\multicolumn{2}{c}{Probability of Patenting} \\\\ \\hline", 
    "(1) & (2) & (3) & (4) \\\\ \\hline"
  )
  
  # Generate LaTeX code
  latex_code <- xtable::xtable(result_df, caption=caption, align = align, add.to.row = addtorow)
  
  # Save the LaTeX code to the specified file
  xtable::print.xtable(latex_code, file=file_path, caption.placement="top", include.rownames=TRUE, booktabs=TRUE, hline.after=c(-1, 0))
}

# Usage:
# generate_latex_table(c("obj1", "obj2"), "output.tex", "Table Caption")



generate_latex_table_2elements <- function(object_names, file_path, caption) {
  # Ensure only two elements are provided
  if (length(object_names) != 2) {
    stop("The function expects exactly two object names in the list.")
  }
  
  # Check if xtable is installed and load it
  if (!requireNamespace("xtable", quietly = TRUE)) {
    stop("xtable package is not installed. Please install it before using this function.")
  }
  
  # Initialize an empty list to hold the results
  result_list <- list()
  
  # Z-value for 95% confidence interval
  z_value <- 1.96
  
  # Loop through each object to extract information
  for (obj_name in object_names) {
    if (!exists(obj_name)) {
      cat("Object", obj_name, "does not exist.\n")
      next
    }
    obj <- get(obj_name)
    
    # Check for the required attributes
    required_attributes <- c("overall.att", "overall.se", "DIDparams")
    if (!all(required_attributes %in% names(obj))) {
      cat("Object", obj_name, "does not have the required attributes.\n")
      next
    }
    
    # Extract and process data from the object
    overall_att <- obj$overall.att
    overall_se <- obj$overall.se
    lower_bound <- overall_att - (z_value * overall_se)
    upper_bound <- overall_att + (z_value * overall_se)
    
    # Determine the significance stars
    stars <- ifelse(lower_bound > 0 | upper_bound < 0, ifelse(abs(overall_att / overall_se) >= 2.58, "***", ifelse(abs(overall_att / overall_se) >= 1.96, "**", "*")), "")
    
    # Create formatted strings
    att_string <- paste(formatC(overall_att, format = 'f', digits = 2), stars, "(", formatC(overall_se, format = 'f', digits = 2), ")", sep = "")
    number_observations <- formatC(length(obj$DIDparams$data$.rowid), format = "d", big.mark = ",")
    ci_string <- paste("[", formatC(lower_bound, format = 'f', digits = 2), ", ", formatC(upper_bound, format = 'f', digits = 2), "]", sep = "")
    
    # Check if covariates are included and format accordingly
    covariates_included <- ifelse(!is.null(obj$DIDparams$covariates) && length(obj$DIDparams$covariates) > 0, "Yes", "No")
    
    # Determine control group based on the object name
    control_group <- ifelse(grepl("notyet", obj_name), "Not yet treated", "Not yet treated & Never treated")
    
    # Add the processed data to the result list
    result_list[[obj_name]] <- c(`ATT (SE)` = att_string, `CI [Lower, Upper]` = ci_string, Observations = number_observations, Covariates = covariates_included, `Control Group` = control_group)
  }
  
  # Convert the list to a data frame and transpose it
  result_df_transposed <- as.data.frame(do.call(rbind, result_list))
  result_df_transposed <- t(result_df_transposed)
  
  # Determine the align parameter based on the number of columns
  num_columns <- ncol(result_df_transposed)
  align_vector <- c("l", rep("c", num_columns))
  
  # Generate LaTeX code
  latex_code <- xtable::xtable(result_df_transposed, caption=caption, align = align_vector)
  
  # Save the LaTeX code to the specified file
  xtable::print.xtable(latex_code, file=file_path, caption.placement="top")
}

# Usage:
# generate_latex_table_2elements(c("obj1", "obj2"), "output.tex", "Table Caption")



# 2. Produce tables ----------------------------------------------------------
#### Table 2 ------------------------------------------------------------
descriptive_statistics$Supplier <- format(round(as.numeric(descriptive_statistics$Supplier), 2), big.mark = ",", nsmall = 2)
descriptive_statistics$`Potential Supplier` <- format(round(as.numeric(descriptive_statistics$`Potential Supplier`), 2), big.mark = ",", nsmall = 2)
descriptive_statistics$`P-value` <- format(round(as.numeric(descriptive_statistics$`P-value`), 2), big.mark = ",", nsmall = 2)
descriptive_statistics<- as.data.frame(descriptive_statistics)
rownames(descriptive_statistics)<- NULL

table_2 <- xtable(descriptive_statistics, digits=2, include.rownames = FALSE, caption = "Summary statistics for potential and suppliers")
table_2 <- print(table_2, include.rownames = FALSE)

# Save LaTeX table code to a file
write.table(table_2, here("results", "tables", "table_2.tex"), sep = "", row.names = FALSE)

#### Table 3  ------------------------------------------------------------


latex_code <- xtable(combined_table_3, align = c("l", "l", "r", "r", "l", "l"),include.rownames = FALSE)
latex_output <- capture.output(print(latex_code, type = "latex", floating = FALSE))
latex_output[grepl("SPACER", latex_output)] <- "\\vspace{1cm}"
file_path <- here("results",  "tables", "table_3.tex")
table_3 <- cat(latex_output, sep = "\n")
write.table(latex_output, file_path, row.names = FALSE)

#### Table 4  ------------------------------------------------------------
# List of object names
#### * Table 4A --------------------------------------------------------------

object_names_table_4A <- c("cs_all_results_log_patent_stock_nevertreated_log_age_fixed_assets_simple_results","cs_all_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                                "cs_all_results_patented_nevertreated_log_age_fixed_assets_simple_results","cs_all_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table(object_names_table_4A, here("results", "tables", "table_4A.tex"), "Summary of ATT - All firms and orders")



#### * Table 4B --------------------------------------------------------------
# List of object names
object_names_table_4B <- c("cs_ht_resultslog_patent_stock_nevertreated_log_age_fixed_assets_simple_results","cs_ht_resultslog_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                           "cs_ht_resultspatented_nevertreated_log_age_fixed_assets_simple_results","cs_ht_resultspatented_notyettreated_log_age_fixed_assets_simple_results")

generate_latex_table(object_names_table_4B, here("results","tables", "table_4B.tex"), "Summary of ATT - high-tech orders")

#### * Table 4C --------------------------------------------------------------
# List of object names
object_names_table_4C<-c("cs_lt_results_log_patent_stock_nevertreated_none_simple_results","cs_lt_results_log_patent_stock_notyettreated_none_simple_results",
                         "cs_lt_results_patented_nevertreated_log_age_fixed_assets_simple_results","cs_lt_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table(object_names_table_4C, here("results", "tables", "table_4C.tex"),"Summary of ATT - low-tech orders")











#### Table 5  ------------------------------------------------------------

#### * Table 5A --------------------------------------------------------------
# List of object names
object_names_table_5A<-c("cs_one_results_log_patent_stock_nevertreated_log_age_fixed_assets_simple_results","cs_one_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                         "cs_one_results_patented_nevertreated_log_age_fixed_assets_simple_results","cs_one_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table(object_names_table_5A, here("results", "tables", "table_5A.tex"),"Summary of ATT - suppliers receiving one order")

#### * Table 5B --------------------------------------------------------------
# List of object names
object_names_table_5B<-c("cs_multiple_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                         "cs_multiple_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table(object_names_table_5B, here("results", "tables", "table_5B.tex"),"Summary of ATT - suppliers receiving multiple orders")



#### * Table 5C --------------------------------------------------------------
# List of object names 
object_names_table_5C<- c("cs_pre_2008_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results", "cs_pre_2008_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_5C, here("results", "tables", "table_5C.tex"),"Summary of ATT - during LHC")


#### * Table 5D--------------------------------------------------------------
# List of object names 
object_names_table_5D<- c("cs_post_2008_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                          "cs_post_2008_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_5D, here("results", "tables", "table_5D.tex"),"Summary of ATT - after LHC")


#### * Table 5E--------------------------------------------------------------
# List of object names 
object_names_table_5E<- c("cs_100k_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                          "cs_100k_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_5E, here("results", "tables", "table_5E.tex"),"Summary of ATT - 100k")


#### * Table 5F--------------------------------------------------------------
# List of object names 
object_names_table_5F<- c("cs_more_100k_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                         "cs_more_100k_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_5F, here("results", "tables", "table_5F.tex"),"Summary of ATT - >100k")



#### Table 6 ------------------------------------------------------------

#### * Table 6A --------------------------------------------------------------

object_names_table_6A <- c("cs_large_results_log_patent_stock_nevertreated_log_age_fixed_assets_simple_results", "cs_large_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                           "cs_large_results_patented_nevertreated_log_age_fixed_assets_simple_results", "cs_large_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table(object_names_table_6A, here("results", "tables", "table_6A.tex"),"Summary of ATT - Large Companies")

#### *Table 6B  ----------------------------------------------------------------

object_names_table_6B <- c("cs_SME_results_log_patent_stock_nevertreated_log_age_fixed_assets_simple_results", "cs_SME_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                           "cs_SME_results_patented_nevertreated_log_age_fixed_assets_simple_results", "cs_SME_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table(object_names_table_6B, here("results", "tables", "table_6B.tex"),"Summary of ATT - SMEs")

#### *Table 6C  ----------------------------------------------------------------
object_names_table_6C <- c("cs_start_up_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                           "cs_start_up_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_6C, here("results", "tables", "table_6C.tex"),"Summary of ATT - start_ups")


####*Table 6D  ----------------------------------------------------------------
object_names_table_6D <- c("cs_incumbent_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                           "cs_incumbent_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_6D, here("results", "tables", "table_6D.tex"),"Summary of ATT - incumbent")


####*Table 6E  ----------------------------------------------------------------
object_names_table_6E <- c("cs_ES_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                          "cs_ES_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_6E, here("results", "tables", "table_6E.tex"),"Summary of ATT - Spain")

####*Table 6F  ----------------------------------------------------------------
object_names_table_6F <- c("cs_FR_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                           "cs_FR_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_6F, here("results", "tables", "table_6F.tex"),"Summary of ATT - France")

####*Table 6G  ----------------------------------------------------------------
object_names_table_6G <-c("cs_IT_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                          "cs_IT_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_6G, here("results", "tables", "table_6G.tex"),"Summary of ATT - Italy")

####*Table 6H  ----------------------------------------------------------------
object_names_table_6H <- c("cs_UK_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results",
                             "cs_UK_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_6H, here("results", "tables", "table_6H.tex"),"Summary of ATT - UK")


# #### Table 9  ------------------------------------------------------------
# 
# 
# #### *Table 9A  ----------------------------------------------------------------
# 
# object_names_table_9A <- c("cs_large_results_patented_nevertreated_none_simple_results", "cs_large_results_patented_notyettreated_none_simple_results",
#                            "cs_large_results_patented_nevertreated_log_age_fixed_assets_simple_results", "cs_large_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table(object_names_table_9A, here("results", "tables", "table_9A.tex"),"Summary of ATT - Large Companies")
# 
# #### *Table 9B  ----------------------------------------------------------------
# 
# object_names_table_9B <- c("cs_SME_results_patented_nevertreated_none_simple_results", "cs_SME_results_patented_notyettreated_none_simple_results",
#                            "cs_SME_results_patented_nevertreated_log_age_fixed_assets_simple_results", "cs_SME_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table(object_names_table_9B, here("results", "tables", "table_9B.tex"),"Summary of ATT - SMEs")
# 
# #### *Table 9C  ----------------------------------------------------------------
# object_names_table_9C <- c("cs_start_up_results_patented_notyettreated_none_simple_results",
#                            "cs_start_up_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table_2elements(object_names_table_9C, here("results", "tables", "table_9C.tex"),"Summary of ATT - start_ups")
# 
# #### *Table 9D  ----------------------------------------------------------------
# object_names_table_9D <- c("cs_incumbent_results_patented_notyettreated_none_simple_results",
#                            "cs_incumbent_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table_2elements(object_names_table_9D, here("results", "tables", "table_9D.tex"),"Summary of ATT - incumbent")
# 
# #### *Table 9E  ----------------------------------------------------------------
# object_names_table_9E <- c("cs_ES_results_patented_notyettreated_none_simple_results",
#                            "cs_ES_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table_2elements(object_names_table_9E, here("results", "tables", "table_9E.tex"),"Summary of ATT - Spain")
# 
# #### *Table 9F  ----------------------------------------------------------------
# object_names_table_9F <- c("cs_FR_results_patented_notyettreated_none_simple_results",
#                            "cs_FR_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table_2elements(object_names_table_9F, here("results", "tables", "table_9F.tex"),"Summary of ATT - France")
# 
# #### *Table 9G  ----------------------------------------------------------------
# object_names_table_9G <-c("cs_IT_results_patented_notyettreated_none_simple_results",
#                           "cs_IT_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table_2elements(object_names_table_9G, here("results", "tables", "table_9G.tex"),"Summary of ATT - Italy")
# 
# #### *Table 9H  ----------------------------------------------------------------
# object_names_table_9H <- c("cs_UK_results_patented_notyettreated_none_simple_results",
#                            "cs_UK_results_patented_notyettreated_log_age_fixed_assets_simple_results")
# generate_latex_table_2elements(object_names_table_9H, here("results", "tables", "table_9H.tex"),"Summary of ATT - UK")


#### Table 7-----------------------------------------------------------------

object_names_table_7 <- c("cs_placebo_results_log_patent_stock_notyettreated_log_age_fixed_assets_simple_results", 
                          "cs_placebo_results_patented_notyettreated_log_age_fixed_assets_simple_results")
generate_latex_table_2elements(object_names_table_7, here("results", "tables", "table_7.tex"),"Summary of ATT - placebo")


cs_

