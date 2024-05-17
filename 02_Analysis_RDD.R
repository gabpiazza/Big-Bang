#Info --------------------------------------------------------------------
##
##Script name: 02_analysis_RDD
##
##Purpose of script: Preparing the procurement data for analaysis##
##Author: Gabriele Piazza
##
##Date Created: 2023-08-22
##
##Copyright (c) Gabriele Piazza, 2023
##Email: g.piazza@lse.ac.uk 
##

##
## Notes:
##   
##


# 1. Load packages --------------------------------------------------------


# 2.  Load data -----------------------------------------------------------



# 3.   Data prep ----------------------------------------------------------

one_order_data<- matched_potential_suppliers_orbis_selected_age_20 %>% 
  filter(total_orders <3)

# Calculate the change in patent stock
ten_year_change<- one_order_data %>% 
  filter(!is.na(first_order)) %>%
  group_by(bvd_id_number) %>%
  mutate(initial_ebitda = ifelse(year == first_order, log(ebitda+0.0001), NA),
         ten_year_ebitda = ifelse(year == first_order + 5, log(ebitda+0.0001), NA)) %>%
  summarise(initial_ebitda = first(na.omit(initial_ebitda)),
            ten_year_ebitda = first(na.omit(ten_year_ebitda))) %>%
  mutate(change_in_ebitda = ten_year_ebitda - initial_ebitda) %>% 
  ungroup()

ten_year_change_patent<- one_order_data %>% 
  filter(!is.na(first_order)) %>%
  group_by(bvd_id_number) %>%
  mutate(log_patent_stock = ifelse(year == first_order, log_patent_stock, NA),
         ten_year_log_patent_stock = ifelse(year == first_order + 5, log_patent_stock, NA)) %>%
  summarise(initial_log_patent_stock = first(na.omit(log_patent_stock)),
            ten_year_log_patent_stock = first(na.omit(ten_log_patent_stock))) %>%
  mutate(change_in_log_patent_stock = ten_year_log_patent_stock - initial_log_patent_stock) %>% 
  ungroup()

selected_variables<- one_order_data %>% select(bvd_id_number, first_order_amount) %>% 
  distinct()

rdd_data<- ten_year_change %>% left_join(selected_variables)


ggplot(rdd_data, aes(x = log(first_order_amount), y = ten_year_ebitda)) + 
  geom_point() + 
  labs(x = "First Order Amount", y = "Change in Log Patent Stock") +
  theme_minimal()
result <- RDestimate(ten_year_change ~ log(first_order_amount), 
                     data = rdd_data, 
                     cutpoint = 10)  
