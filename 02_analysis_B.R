#' ---
#' title: 02_analysis_B
#' author: Gabriele Piazza
#' date: 2024-06-20
#' Description: This scripts does some descriptive analysis
#' The data was created using scripts 01_dataprep_A,B,C,D
#' 
# 1.  Set up --------------------------------------------------------------

## 1.1 Install & Load packages --------------------------------------------------------

IPW did # some setup: a cheeky little bit of code to check and install packages
need <- c(
  "Matching", "panelView", "tjbal", "MatchIt", "parallel","WeightIt", "tidyverse", 
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

# Placeholder perform_analysis function
perform_analysis <- function(y_var, control_group_type, dataset, covariates = NULL) {
  data_filtered <- dataset %>%
    filter(first_order %notin% years_few_observations)
  
  if (!is.null(covariates) && length(covariates) > 0) {
    xformla <- as.formula(paste( "~", paste(covariates, collapse = " + ")))
  } else {
    xformla <- as.formula("~ 1")
  }
  
  # Expected column names
  expected_columns <- c("bvd_id_numeric", "year", y_var, "first_order_2")
  
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
    bstrap = TRUE,
    panel = TRUE,
    biters = 3000,
    allow_unbalanced_panel = TRUE
  )
  
  # Compute the different results
  cs_results.dyn <- aggte(cs_results, type = "dynamic", na.rm = TRUE, max_e = 10, min_e = -10, cband = FALSE)
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

run_all_analyses_multiple_combinations_parallel <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Create all possible covariate combinations
  all_covariates <- c("pre_log_fixed_assets", "pre_log_operating_revenue_turnover")
  # all_covariates <- c("pre_log_operating_revenue_turnover","pre_log_ebitda","pre_log_fixed_assets", "age")
  
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
  
  # Function to wrap the analysis
  analysis_function <- function(params, dataset, covariate_combinations) {
    y_var <- params[[1]]
    control_group <- params[[2]]
    cov_name <- params[[3]]
    covariates <- covariate_combinations[[cov_name]]
    results_key <- paste(y_var, control_group, cov_name, sep = "_")
    result <- perform_analysis(y_var, control_group, dataset, covariates)
    return(list(key = results_key, result = result))
  }
  
  # Create a list of all parameter combinations
  param_list <- expand.grid(y_vars, control_groups, names(covariate_combinations), stringsAsFactors = FALSE)
  param_list <- split(param_list, seq(nrow(param_list)))
  
  # Use parallel processing
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores)
  
  # Export necessary variables and functions to the cluster
  clusterExport(cl, varlist = c("perform_analysis", "covariate_combinations", "dataset"))
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(did))
  
  # Perform the analyses in parallel
  results_list <- parLapply(cl, param_list, analysis_function, dataset = dataset, covariate_combinations = covariate_combinations)
  
  # Stop the cluster
  stopCluster(cl)
  
  # Update the progress bar and collect the results
  for (res in results_list) {
    results[[res$key]] <- res$result
    pb$tick()
  }
  
  return(results)
}
run_all_analyses_multiple_combinations <- function(y_vars, dataset) {
  
  results <- list()
  
  control_groups <- c('nevertreated', 'notyettreated')
  
  # Create all possible covariate combinations
  # all_covariates <- c("pre_log_operating_revenue_turnover","pre_log_ebitda","pre_log_fixed_assets", "age")
  all_covariates <- c("pre_log_fixed_assets", "pre_log_operating_revenue_turnover")
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
  all_covariates <- c( "pre_log_fixed_assets","pre_log_operating_revenue_turnover")
  
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
full_panel_ht <- full_panel_patenting %>% filter(first_order_tech==1)
full_panel_lt<- full_panel_patenting %>% filter(first_order_tech==0)
full_panel_one<- full_panel_patenting %>% 
  filter((supplier_status == 0) | (supplier_status == 1 & total_orders < 2))
full_panel_multiple <- full_panel_patenting %>%
  filter((supplier_status == 0) | (supplier_status == 1 & total_orders > 1))

full_panel_large_projects<- full_panel_patenting %>% 
  filter((supplier_status == 0) | (supplier_status == 1 &subproject_1_first_year %in% c("LHC")))

full_panel_less_than_100k<- full_panel_patenting %>% 
  filter((supplier_status == 0) | (supplier_status == 1 & first_order_amount <= 100000))

full_panel_greater_than_100k<- full_panel_patenting %>% 
  filter((supplier_status == 0) | (supplier_status == 1 & first_order_amount  >100000))

full_panel_above_average_order <- full_panel_patenting %>% 
  filter((supplier_status == 0) | (supplier_status == 1 & first_order_amount <= mean(first_order_amount)))

full_panel_below_average_order <- full_panel_patenting %>% 
  filter((supplier_status==0) | (supplier_status == 1 & first_order_amount <=mean(first_order_amount)))

full_panel_SME<- full_panel_patenting %>% filter(SME_status ==1)
full_panel_large_companies <- full_panel_patenting %>% filter(SME_status ==0)

full_panel_below_mean_age<- full_panel_patenting %>% filter(age<=mean(age))
full_panel_above_mean_age<- full_panel_patenting %>% filter(age>mean(age))

full_panel_financially_constrained_firms <- full_panel_patenting %>% filter(pre_log_fixed_assets<mean(pre_log_fixed_assets))
full_panel_financially_unconstrained_firms<- full_panel_patenting %>% filter(pre_log_fixed_assets>= mean(pre_log_fixed_assets))


# Count of companies ------------------------------------------------------

number_full_panel<- full_panel %>% group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="all")
number_low_tech <-  full_panel_lt %>% group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="low_tech")
number_high_tech <- full_panel_ht %>% group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="high_tech")
number_less_than_100k<- full_panel_less_than_100k %>% group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="less_than_100k")
number_greater_than_100k<- full_panel_greater_than_100k %>%  group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="greater_than_100k")
number_SME<- full_panel_SME %>% group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="SME")
number_large_companies<- full_panel_large_companies %>% group_by(supplier_status) %>% 
  summarise(number_companies = n_distinct(bvd_id_number)) %>% mutate(category ="Large Companies")
number_firms_category <- bind_rows(number_full_panel,
                              number_low_tech,
                              number_high_tech,
                              number_less_than_100k,
                              number_greater_than_100k,
                              number_SME,
                              number_large_companies)
number_firms_category_wide<- number_firms_category %>% pivot_wider(
  names_from = supplier_status, 
  values_from = number_companies,
  names_prefix = "status_"
)
# y_vars<- vars <- c( "probability_publications", "probability_applications", 
#                     "log_applications", "log_application_stock",
#                     "log_weighted_patent_apps",
#                    "log_publications", "log_publication_stock")

pivot_wider(
  names_from = supplier_status, 
  values_from = number_companies,
  names_prefix = "status_"
)

y_vars<- vars <- c(  "log_applications",
                     "asinh_applications",
                     "log_application_stock",
                    "log_weighted_patent_apps",
                    "log_publications", "probability_applications"
                    )
y_vars<-c("asinh_application_stock")

y_vars_mechanisms  <- c(
  "probability_WIPO_code_apps",
  "probability_collaborations_apps",
  "probability_multiple_patent_offices_apps",
  "log_WIPO_code_apps",
  "log_multiple_inventors_apps",
  "log_collaborations_apps",
  "log_multiple_patent_offices_apps")
 
y_vars_log_patent_stock<- c("log_application_stock")



years_few_observations <- c(1995, 1996, 2008, 2020)


# 3. Run the analysis with CS ---------------------------------------------


##3.1 All suppliers ------------------------------------------------------

cs_all_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_patenting)
library(beepr)
beep(5)
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
beep(5)

## 3.2 High-tech suppliers ------------------------------------------------
full_panel_ht <- full_panel_patenting %>% filter(first_order_tech ==1)
cs_ht_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_ht)
beep(5)
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
beep(5)

## 3.3 Low-tech suppliers ------------------------------------------------
full_panel_lt <- full_panel_patenting %>% filter(first_order_tech ==0)
cs_lt_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_lt)
sim_ratios_lt <- lapply(cs_lt_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
beep(5)
data_frame_sim_ratio_lt_results<- as.data.frame(sim_ratios_lt)
sim_ratios_lt_long <- gather(data_frame_sim_ratio_lt_results, key = "variable", value = "value")
path_to_save <-paste0(main_results, "cs_lt_")
save_all_results(cs_lt_results, path_to_save)
beep(5)

## #3.4 One order -------------------------------------------------------------------
cs_one_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_one)
beep(5)
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

cs_multiple_results <-run_all_analyses_multiple_combinations(y_vars, full_panel_multiple)
beep(5)
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
cs_large_projects_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_large_projects)
sim_ratios_large_projects <- lapply(cs_large_projects_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
beep(5)
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
beep(5)

## #3.7 Large orders -------------------------------------------------------------------
cs_greater_than_100k_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_greater_than_100k)
sim_ratios_greater_than_100k <- lapply(cs_greater_than_100k_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
beep(5)
data_frame_sim_ratio_greater_than_100k_results<- as.data.frame(sim_ratios_greater_than_100k)
sim_ratios_greater_than_100k_long <- gather(data_frame_sim_ratio_greater_than_100k_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_greater_than_100k_projects_")
save_all_results(cs_greater_than_100k_results, path_to_save)


## #3.8 Below average orders -------------------------------------------------------------------
cs_below_average_order_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_below_average_order)
sim_ratios_below_average_order <- lapply(cs_below_average_order_results , function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_below_average_order_results<- as.data.frame(sim_ratios_below_average_order)
sim_ratios_below_average_order_long <- gather(data_frame_sim_ratio_below_average_order_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_below_average_order_")
save_all_results(cs_below_average_order_results, path_to_save)
beep(5)

## #3.9 Above average orders -------------------------------------------------------------------
cs_above_average_order_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_above_average_order)
sim_ratios_above_average_order <- lapply(cs_above_average_order_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
beep(5)
data_frame_sim_ratio_above_average_order_results<- as.data.frame(sim_ratios_above_average_order)
sim_ratios_above_average_order_long <- gather(data_frame_sim_ratio_above_average_order_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_greater_than_100k_projects_")
save_all_results(cs_greater_than_100k_results, path_to_save)




## #3.10 SME -------------------------------------------------------------------
cs_SME_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_SME)
beep(3)
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
## #3.11 Large Companies -------------------------------------------------------------------
cs_large_companies_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_large_companies)
beep(3)
sim_ratios_large_companies <- lapply(cs_large_companies_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_large_companies_results<- as.data.frame(sim_ratios_large_companies)
sim_ratios_large_companies_long <- gather(data_frame_sim_ratio_large_companies_results, key = "variable", value = "value")
path_to_save <-paste0(het_results, "cs_large_companies_")
save_all_results(cs_large_companies_results, path_to_save)
beep(3)
## #3.12 Start_ups -------------------------------------------------------------------
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

## #3.13 Below Mean age -------------------------------------------------------------------
cs_below_average_age_results <- run_all_analyses_multiple_combinations(y_vars, full_panel_below_mean_age)
sim_ratios_below_age <- lapply(cs_below_average_age_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_below_age_results<- as.data.frame(sim_ratios_below_age)
sim_ratios_below_age_long <- gather(data_frame_sim_ratio_below_age_results, key = "variable", value = "value")


## #3.13 Above Mean age -------------------------------------------------------------------
cs_above_average_age_results<-  run_all_analyses_multiple_combinations(y_vars, full_panel_above_mean_age)
sim_ratios_above_age<- lapply(cs_above_average_age_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_above_age_results<- as.data.frame(sim_ratios_above_age)
sim_ratios_above_age_long <- gather(data_frame_sim_ratio_above_age_results, key = "variable", value = "value")


## #3.14 Financially constrained firms  -------------------------------------------------------------------
cs_financially_constrained_results<-  run_not_yet_analyses(y_vars, full_panel_financially_constrained_firms)
sim_ratios_financially_constrained<- lapply(cs_financially_constrained_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_financially_constrained_results<- as.data.frame(sim_ratios_financially_constrained)
sim_ratios_financially_constrained_long <- gather(data_frame_sim_ratio_financially_constrained_results, key = "variable", value = "value")




## #3.15 Financially unconstrained firms  -------------------------------------------------------------------
cs_financially_unconstrained_results<-  run_not_yet_analyses(y_vars, full_panel_financially_unconstrained_firms)
sim_ratios_financially_unconstrained<- lapply(cs_financially_unconstrained_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})
data_frame_sim_ratio_financially_unconstrained_results<- as.data.frame(sim_ratios_financially_unconstrained)
sim_ratios_financially_unconstrained_long <- gather(data_frame_sim_ratio_financially_unconstrained_results, key = "variable", value = "value")

# 4. Mechanisms -----------------------------------------------------------

cs_all_mech_results <- run_all_analyses_multiple_combinations(y_vars_mechanisms, full_panel_patenting)
library(beepr)
beep()
sim_ratios_mechanisms_all<- lapply(cs_all_mech_results, function(x) {
  if ("sim_ratio" %in% names(x)) {
    return(x$sim_ratio)
  } else {
    return(NULL)
  }
})

beep(5)

data_frame_sim_ratio_all_mechanisms_results<- as.data.frame(sim_ratios_mechanisms_all)
sim_ratios_all_mechanisms_long <- gather(data_frame_sim_ratio_all_mechanisms_results, key = "variable", value = "value")

path_to_save <-paste0(mechanisms_results, "cs_all_")
save_all_results(cs_all_mech_results, path_to_save)
beep()


