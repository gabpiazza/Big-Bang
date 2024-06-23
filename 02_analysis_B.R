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
  "foreign", "knitr", "kableExtra", "data.table", "visdat", "lubridate"
) # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)
## 1.2 Create functions ----------------------------------------------------
`%notin%` <- Negate(`%in%`)

perform_analysis <- function(y_var, control_group_type, dataset, covariates = NULL) {
  data_filtered <- dataset %>%
    filter(first_order %notin% years_few_observations)
  
  if (!is.null(covariates) && length(covariates) > 0) {
    xformla <- as.formula(paste( "~", paste(covariates, collapse = " + ")))
  } else {
    xformla <- as.formula(paste( "~ 1"))
    
  }
  
  # Expected column names
  expected_columns <- c("bvd_id_numeric", "year", y_var, "first_order")
  
  # Check for missing columns
  missing_columns <- expected_columns[!expected_columns %in% colnames(data_filtered)]
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
  }
  
  cs_results <- att_gt(
    yname = y_var,
    tname = "year",
    idname = "bvd_id_numeric",
    gname = "first_order_2",
    xformla = xformla,
    data = data_filtered,
    control_group = control_group_type,
    est_method = "ipw",
    clustervars = "bvd_id_numeric", 
    pl=TRUE, 
    cores=8,
    bstrap = T,
    panel = TRUE,
    biters = 3000,
    allow_unbalanced_panel = TRUE,
  )
  
  # You can add further calculations if needed.
  
  
  # Compute the different results
  cs_results.dyn <- aggte(cs_results, type = "dynamic", na.rm = TRUE, max_e = 10, min_e = -10, cband = F)
  cs_results.sim <- aggte(cs_results, type = "simple", na.rm = TRUE)
  cs_results.grp <- aggte(cs_results, type = "group", na.rm = TRUE)
  cs_results.cal <- aggte(cs_results, type = "calendar", na.rm = TRUE)
  
  sim_ratio <- cs_results.sim$overall.att / cs_results.sim$overall.se
  
  # Visualization and confidence intervals for dynamic results
  cs_results_plot_dyn <- ggdid(cs_results.dyn)
  cs_results_data_dyn <- cs_results_plot_dyn$data
  cs_results_data_dyn$lower <- cs_results_data_dyn$att - cs_results_data_dyn$c * cs_results_data_dyn$att.se
  cs_results_data_dyn$upper <- cs_results_data_dyn$att + cs_results_data_dyn$c * cs_results_data_dyn$att.se
  
  # Create a list to store all the results
  results_list <- list(
    main = cs_results,
    dynamic = cs_results.dyn,
    simple = cs_results.sim,
    group = cs_results.grp,
    calendar = cs_results.cal,
    sim_ratio = sim_ratio,
    dynamic_plot_data = cs_results_data_dyn
  )
  
  return(results_list)
}


run_all_analyses_multiple_combinations <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c("pre_log_operating_revenue_turnover","pre_log_ebitda","pre_log_fixed_assets", "age")
  
  covariate_combinations <- list("none" = NULL)
  
  # Loop through all sizes of combinations
  for (i in 1:length(all_covariates)) {
    combinations <- combn(all_covariates, i)
    for (j in 1:ncol(combinations)) {
      combination_name <- paste(combinations[,j], collapse="_")
      covariate_combinations[[combination_name]] <- combinations[,j]
    }
  }
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each combination
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}

run_all_analyses_multiple_combinations_ht <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c("first_order_tech","pre_log_operating_revenue_turnover","pre_log_ebitda","pre_log_application_stock","pre_log_fixed_assets")
  
  covariate_combinations <- list("none" = NULL)
  
  # Loop through all sizes of combinations
  for (i in 1:length(all_covariates)) {
    combinations <- combn(all_covariates, i)
    for (j in 1:ncol(combinations)) {
      combination_name <- paste(combinations[,j], collapse="_")
      covariate_combinations[[combination_name]] <- combinations[,j]
    }
  }
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each combination
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  return(results)
}



run_not_yet_analyses <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c( "pre_log_operating_revenue_turnover","pre_log_ebitda","pre_log_fixed_assets", "age")
  
  covariate_combinations <- list("none" = NULL)
  
  # Loop through all sizes of combinations
  for (i in 1:length(all_covariates)) {
    combinations <- combn(all_covariates, i)
    for (j in 1:ncol(combinations)) {
      combination_name <- paste(combinations[,j], collapse="_")
      covariate_combinations[[combination_name]] <- combinations[,j]
    }
  }
  
  # Calculate total number of iterations
  total_iterations <- length(y_vars) * length(control_groups) * length(covariate_combinations)
  
  # Create a progress bar
  pb <- progress_bar$new(total = total_iterations, format = "[:bar] :percent :elapsedfull")
  
  # Analyze data with each combination
  for (y_var in y_vars) {
    for (control_group in control_groups) {
      for (cov_name in names(covariate_combinations)) {
        covariates <- covariate_combinations[[cov_name]]
        results_key <- paste(y_var, control_group, cov_name, sep = "_")
        
        results[[results_key]] <- perform_analysis(y_var, control_group, dataset, covariates)
        
        # Update the progress bar
        pb$tick()
      }
    }
  }
  
  
  
  return(results)
}



print_simple_results <- function(results) {
  for (result_name in names(results)) {
    cat("\nResults for:", result_name, "\n")
    for (y_var in names(results[[result_name]])) {
      cat("For y_var:", y_var, "\n")
      if(is.list(results[[result_name]][[y_var]]$simple)) {
        simple_result <- results[[result_name]][[y_var]]$simple$overall.att
        if (!is.null(simple_result)) {
          cat(simple_result, "\n")
        } else {
          cat("NULL\n")
        }
      } else {
        cat("Error: Unexpected structure in results\n")
      }
    }
  }
}

save_all_results <- function(all_results, save_path) {
  # Calculate total number of items to process
  total_items <- sum(sapply(all_results, function(x) length(unlist(x))))
  
  # Create a progress bar
  pb <- txtProgressBar(min = 0, max = total_items, style = 3)
  
  # Initialize counter
  progress_counter <- 0
  
  # Loop through the results and save them
  for (analysis_name in names(all_results)) {
    analysis_result <- all_results[[analysis_name]]
    
    # Check if analysis_result is a list or dataframe
    if (!is.list(analysis_result) && !is.data.frame(analysis_result)) {
      stop(paste("Expected a list or dataframe for analysis_name:", analysis_name, "but got:", class(analysis_result)))
    }
    
    for (y_var in names(analysis_result)) {
      result_for_var <- analysis_result[[y_var]]
      
      # Handle the numeric type specifically for sim_ratio
      if (y_var == "sim_ratio" && is.numeric(result_for_var)) {
        saveRDS(result_for_var, file = paste0(save_path, analysis_name, "_", y_var, "_results.rds"))
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
        next
      }
      
      # If not sim_ratio, check if result_for_var is a list or dataframe
      if (!is.list(result_for_var) && !is.data.frame(result_for_var)) {
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
        next # Just skip non-list, non-dataframe items
      }
      
      # Save the main results
      saveRDS(result_for_var, file = paste0(save_path, analysis_name, "_", y_var, "_results.rds"))
      progress_counter <- progress_counter + 1
      setTxtProgressBar(pb, progress_counter)
      
      # Save dynamic visualization data if it exists
      if (!is.null(result_for_var$dynamic_data)) {
        saveRDS(result_for_var$dynamic_data, file = paste0(save_path, analysis_name, "_", y_var, "_dynamic_data.rds"))
        progress_counter <- progress_counter + 1
        setTxtProgressBar(pb, progress_counter)
      }
    }
  }
  # Close the progress bar
  close(pb)
}



# 2. Directories,  loading data and setting up variables-----------------------------------------


## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data folders 
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
full_panel_file <- "full_panel"
output_folder<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/results/output/"
main_results<- paste0(output_folder, "main/")
het_results<- paste0(output_folder, "heterogeneity/")
mechanisms_results <- paste0(output_folder, "mechanisms/")

## 2.2 Loading and preparing the data  --------------------------------------------------------------------
full_panel<- readRDS(paste0(data_proc_dir, full_panel_file))
full_panel$bvd_id_numeric <- as.numeric(as.factor(full_panel$bvd_id_number))
full_panel_ht <- full_panel %>% filter(first_order_tech==1)
full_panel_lt<- full_panel %>% filter(first_order_tech==0)
full_panel_one<- full_panel %>% filter(total_orders ==1)
full_panel_multiple<- full_panel %>% filter(total_orders >1)
full_panel_large_projects<- full_panel %>% filter(subproject_1_first_year %in% c("LHC", "HL"))
full_panel_less_than_100k<- full_panel %>% filter(first_order_amount <=100000)
full_panel_greater_than_100k<- full_panel %>% filter(first_order_amount >100000)
full_panel_SME<- full_panel %>% filter(SME_status ==1)
full_panel_start_ups<- full_panel %>% filter(age<=5)



y_vars<- vars <- c( "probability_publications", "probability_applications", 
                    "log_applications", "log_application_stock",
                    "log_weighted_patent_apps",
                   "log_publications", "log_publication_stock"

                   )

years_few_observations <- c(1995, 1996, 2008, 2020)


# 3. Run the analysis with CS ---------------------------------------------


##3.1 All suppliers ------------------------------------------------------

cs_all_results <- run_all_analyses_multiple_combinations(y_vars, full_panel)
library(beepr)
beep()
sim_ratios_all<- lapply(cs_all_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

data_frame_sim_ratio_all_results<- as.data.frame(sim_ratios_all)
sim_ratios_all_long <- gather(data_frame_sim_ratio_all_results, key = "variable", value = "value")

path_to_save <-paste0(main_results, "cs_all_")
save_all_results(cs_all_results, path_to_save)

## 3.2 High-tech suppliers ------------------------------------------------
cs_ht_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_ht)
sim_ratios_ht <- lapply(cs_ht_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_ht_results<- as.data.frame(sim_ratios_ht)
sim_ratios_ht_long <- gather(data_frame_sim_ratio_ht_results, key = "variable", value = "value")

path_to_save <-paste0(main_results, "cs_ht_")
save_all_results(cs_ht_results, path_to_save)

## 3.3 Low-tech suppliers ------------------------------------------------
cs_lt_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_lt)
sim_ratios_lt <- lapply(cs_lt_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_lt_results<- as.data.frame(sim_ratios_lt)
sim_ratios_lt_long <- gather(data_frame_sim_ratio_lt_results, key = "variable", value = "value")
path_to_save <-paste0(main_results, "cs_lt_")
save_all_results(cs_lt_results, path_to_save)
## #3.4 One order -------------------------------------------------------------------

cs_one_results <- run_not_yet_analyses(y_vars, full_panel_one)
sim_ratios_one <- lapply(cs_one_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_one_results<- as.data.frame(sim_ratios_one)
sim_ratios_one_long <- gather(data_frame_sim_ratio_one_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_one_")
save_all_results(cs_one_results, path_to_save)

## #3.4 Multiple orders -------------------------------------------------------------------

cs_multiple_results <-run_not_yet_analyses(y_vars, full_panel_multiple)
sim_ratios_multiple <- lapply(cs_multiple_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_multiple_results<- as.data.frame(sim_ratios_multiple)
sim_ratios_multiple_long <- gather(data_frame_sim_ratio_multiple_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_multiple_")
save_all_results(cs_multiple_results, path_to_save)

## #3.5 Large Projects -------------------------------------------------------------------
cs_large_projects_results <- run_not_yet_analyses(y_vars, full_panel_large_projects)
sim_ratios_large_projects <- lapply(cs_large_projects_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_large_projects_results<- as.data.frame(sim_ratios_large_projects)
sim_ratios_large_projects_long <- gather(data_frame_sim_ratio_large_projects_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_large_projects_")
save_all_results(cs_large_projects_results, path_to_save)
## #3.6 Small orders -------------------------------------------------------------------
cs_less_than_100k_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_less_than_100k)
sim_ratios_less_than_100k <- lapply(cs_less_than_100k_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_less_than_100k_results<- as.data.frame(sim_ratios_less_than_100k)
sim_ratios_less_than_100k_long <- gather(data_frame_sim_ratio_less_than_100k_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_less_than_100k_projects_")
save_all_results(cs_less_than_100k_results, path_to_save)


## #3.7 Large orders -------------------------------------------------------------------
cs_greater_than_100k_results <- run_not_yet_analyses(y_vars, full_panel_greater_than_100k)
sim_ratios_greater_than_100k <- lapply(cs_greater_than_100k_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_greater_than_100k_results<- as.data.frame(sim_ratios_greater_than_100k)
sim_ratios_greater_than_100k_long <- gather(data_frame_sim_ratio_greater_than_100k_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_greater_than_100k_projects_")
save_all_results(cs_greater_than_100k_results, path_to_save)

## #3.8 SME -------------------------------------------------------------------
cs_SME_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_SME)
sim_ratios_SME <- lapply(cs_SME_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_SME_results<- as.data.frame(sim_ratios_SME)
sim_ratios_SME_long <- gather(data_frame_sim_ratio_SME_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_SME_")
save_all_results(cs_SME_results, path_to_save)

## #3.9 Start_ups -------------------------------------------------------------------
cs_startups_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_start_ups)
sim_ratios_startups <- lapply(cs_startups_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_startups_results<- as.data.frame(sim_ratios_startups)
sim_ratios_startups_long <- gather(data_frame_sim_ratio_startups_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_start_ups_")
save_all_results(cs_startups_results, path_to_save)




