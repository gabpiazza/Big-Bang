# Load  the data 
suppliers_2016<- read_excel("data_raw/21_10_27_Suppliers_cern_2016_nocontacts.xlsx")
suppliers_2014_21<- read_excel("data_raw/2021-06-29 - CERN Orders 2014-2021_clean.xlsx")
suppliers_2016<-clean_names(suppliers_2016)
suppliers_2016<- suppliers_2016 %>% rename(chf_amount = sum_chf_amount)
suppliers_2014_21 <- clean_names(suppliers_2014_21)

selected_variables <- c("order_date", "order_number", "supplier_code", "country", "chf_amount")

suppliers_2016<- suppliers_2016 %>% select(all_of(selected_variables))
suppliers_2014_21<- suppliers_2014_21 %>% select(all_of(selected_variables))

suppliers_all<- rbind(suppliers_2016, suppliers_2014_21)
suppliers_all<- suppliers_all %>% distinct()


countries_orders<- suppliers_all %>% group_by(country) %>% 
  summarize(total_amount = sum(chf_amount), no_orders = n())
countries_orders$share_amount<- prop.table(countries_orders$total_amount)*100
countries_orders$share_orders<- prop.table(countries_orders$no_orders)*100
countries_orders_top<- countries_orders %>% filter(share_amount >=1 & share_orders>=1)



## Plot the share of matched orders by country 

country_plot_orders_share<- ggplot(countries_orders_top, aes(x = reorder(as.factor(country),-share_orders), y = share_orders, fill= factor(-no_orders))) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "% of total orders", x= "")+
  scale_fill_grey(start = 0.1, end =0.9)+
  theme_classic()+
  theme(legend.position="none")

country_plot_amount_share<- ggplot(countries_orders_top, aes(x = reorder(as.factor(country),-share_amount), y = share_amount, fill= factor(-total_amount))) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "% of total amount", x= "")+
  scale_fill_grey(start = 0.1, end =0.9)+
  theme_classic()+
  theme(legend.position="none")

#Load the potential suppliers
potential_suppliers<- read_csv("data_raw/22_03_08_potential_suppliers_procurement.csv")
potential_suppliers<- clean_names(potential_suppliers)
potential_suppliers_by_country <- potential_suppliers %>% 
  distinct() %>% 
  group_by(country) %>% 
  summarize(no_registered_companies = n())
potential_suppliers_by_country$share_registered_companies <- prop.table(potential_suppliers_by_country$no_registered_companies)*100
potential_suppliers_by_country_top<- potential_suppliers_by_country %>% filter(share_registered_companies>=1)

  
country_plot_potential_suppliers<- ggplot(potential_suppliers_by_country_top, aes(x = reorder(as.factor(country),-share_registered_companies), 
                                                                           y = share_registered_companies, fill= factor(-no_registered_companies))) +
  geom_bar(stat = "identity", position = "dodge")+labs(y= "% of all registered companies", x= "")+
  scale_fill_grey(start = 0.1, end =0.9)+
  theme_classic()+
  theme(legend.position="none")  
  
nrow(potential_suppliers)


ita_fr_es_uk<- c("IT", "FR", "ES", "UK")
suppliers_seleted_countries <- suppliers_all %>% filter(country %in% ita_fr_es_uk)


suppliers_2016_selected_countries<- suppliers_2016 %>% dplyr::filter(COUNTRY %in% ita_fr_es_uk)
suppliers_2016_selected_countries<- suppliers_2016_selected_countries %>% 
  dplyr::select(SUPPLIER_CODE, SUPPLIER_NAME, COUNTRY) %>% distinct()

suppliers_2014_21_selected_countries<- suppliers_2014_21 %>% dplyr::filter(COUNTRY %in% ita_fr_es_uk)
suppliers_2014_21_selected_countries<- suppliers_2014_21_selected_countries %>% 
  dplyr::select(SUPPLIER_CODE, SUPPLIER_NAME, COUNTRY) %>% distinct()

suppliers_all_selected_countries <- rbind(suppliers_2016_selected_countries, suppliers_2014_21_selected_countries)
suppliers_all_selected_countries<- suppliers_all_selected_countries %>% distinct()
                                          
                                                                                   
suppliers_2016_selected<- suppliers_2016 %>% 
  dplyr::select(SUPPLIER_CODE, SUPPLIER_NAME, COUNTRY) %>% distinct()

suppliers_2021_selected<- suppliers_2014_21%>% 
  dplyr::select(SUPPLIER_CODE, SUPPLIER_NAME, COUNTRY) %>% distinct()

suppliers_all_selected<- rbind(suppliers_2016_selected, suppliers_2021_selected)
suppliers_all_selected<- suppliers_all_selected %>% distinct()

share_IT_ES_FR_UK<- (nrow(suppliers_2014_21_selected_countries)/nrow(suppliers_all_selected))*100



# Potential suppliers -----------------------------------------------------

potential_suppliers<- read_csv("data_raw/22_03_08_potential_suppliers_procurement.csv")
nrow(potential_suppliers)
potential_suppliers_selected_countries<- potential_suppliers %>% dplyr::filter(COUNTRY %in% ita_fr_es) %>% distinct()
share_IT_ES_FR_pot <- (nrow(potential_suppliers_selected_countries)/nrow(potential_suppliers))

length(suppliers_2014_21_selected_countries)
nrow