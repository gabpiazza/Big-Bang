#Script name: 04_graphs
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
# some setup: a cheeky little bit of code to check and install packages
need <- c("tidyverse","stargazer", "janitor", "here","readxl","foreign","xtable","gridExtra","grid","knitr","kableExtra","haven", "fuzzyjoin", "data.table","panelView", "visdat", "beepr", "lubridate", "readxl") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

options(scipen = 999)

###1.2 Load functions ----------------------------------------------------

# Function to read all 'data' containing files from a specific directory

# Example usage

# ###1.3 Load data --------------------------------------------------------
## 2.1 Setting up the directory -------------------------------------------------------
## Setting up the directories for the data folders 
data_raw_dir <- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_raw/"
data_proc_dir<- "/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/data_proc/"
full_panel_file <- "full_panel"
results_folder<-"/Users/gabrielepiazza/Dropbox/PhD/CERN_procurement/Analysis/results/" 
output_folder<- paste0(results_folder, "output")
main_results<- paste0(output_folder, "main")
het_results<- paste0(output_folder, "heterogeneity")
mechanisms_results <- paste0(output_folder, "mechanisms")
figures_folder <- paste0(results_folder, "figures/")

# order_by_year <- readRDS(here("results", "output","graph_order_by_year.rds" ))
# all_orders_tech<- read_csv(here( "data_proc", "all_orders_tech_level.csv")) %>% select(-'...1')
# all_cs_data<- readRDS(here("results", "output", "all_cs_data_graph.rds"))




# Replace this with the actual path to your directory

# List files that contain 'data' in their names
main_files <- list.files(main_results, pattern = "dynamic_plot_data", full.names = TRUE)
het_files <- list.files(het_results, pattern = "dynamic_plot_data", full.names = TRUE)
mechanisms_files<- list.files(mechanisms_results, pattern = "dynamic_plot_data", full.names = TRUE)
# List files that contain 'data' in their names

# Loop through each file to modify its content and save separately
for(file in main_files) {
  # Load the data
  data <- readRDS(file)
  
  # Determine the source variable based on file name conditions
  if (grepl("none", file)) {
    source_value <- "Callaway & Sant'Anna"
  } else {
    source_value <- "Callaway & Sant'Anna with covariates"
  }
  
  # Add the source variable to the dataframe
  data$source <- source_value
  
  # Add control_nevertreated variable if file name contains "nevertreated"
  if(grepl("nevertreated", file)) {
    data$control <- "Baseline"
  } else {
    data$control <- "Late treated"
  }
  
  # Generate a unique object name based on the file name
  object_name <- gsub("[.][^.]+$", "", basename(file))
  object_name <- gsub("[^A-Za-z0-9]", "_", object_name)
  
  # Assign the dataframe to a new object in the R environment
  assign(object_name, data)
}
#2 Create Figures  ----------------------------------------------------------

## Figure 1A ----------------------------------------------------------------
proportions_data <- order_by_year %>%
  group_by(year) %>%
  filter(year >1994) %>% 
  mutate(status = ifelse(year >= first_order, "Supplier", "Not Yet Supplier")) %>%
  group_by(year, status) %>%
  summarise(count = n_distinct(bvd_id_number)) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = count / total * 100)




# Plot
fig_1A<- ggplot(proportions_data, aes(x = as.factor(year), y = percentage, fill = status)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1),  # Adjust text angle for better visibility
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank()) +

  scale_y_continuous(labels = percent_format(scale = 100)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +  # Display every second year
  scale_fill_manual(values = c("Supplier" = "blue", "Not Yet Supplier" = "grey"))


fig_1 <- fig_1 +theme(legend.position = "bottom", legend.title = element_blank(),
                        axis.ticks.x = element_blank(), axis.ticks.y = element_blank()) 

ggsave(here("results","figures","fig_1A.jpeg"), fig_1A, width = 10, height = 7)

## Figure 1B ----------------------------------------------------------------
all_orders_tech$LHC_period <- ifelse(all_orders_tech$order_date >= 1998 & all_orders_tech$order_date <= 2008, "LHC Construction", "Rest of the Period")

all_orders_tech_graph<- all_orders_tech %>%  
  group_by(order_date, LHC_period, tech_level) %>%
  summarise(CHF_amount = sum(chf_amount, na.rm = TRUE), .groups = 'drop') %>%
  mutate(tech_level = factor(tech_level, levels = c(0, 1), labels = c("Low Tech", "High Tech")))

# Define colors for tech levels
base_colors <- c("#377EB8", "#E41A1C")  # Colors for low and high techenhanced_colors <- ifelse(all_orders_tech_graph$order_date >= 1998 & all_orders_tech_graph$order_date <= 2008, 

# all_orders_tech_graph$LHC_period <- ifelse(all_orders_tech_graph$order_date >= 1998 & all_orders_tech_graph$order_date <= 2008, "LHC Construction", "Rest of the Period")
# Plotting
fig_1B <- ggplot(all_orders_tech_graph, aes(x = order_date, y = CHF_amount, fill = interaction(LHC_period, tech_level))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("LHC Construction.Low Tech" = base_colors[1], 
                               "Rest of the Period.Low Tech" = alpha(base_colors[1], 0.5),
                               "LHC Construction.High Tech" = base_colors[2], 
                               "Rest of the Period.High Tech" = alpha(base_colors[2], 0.5)),
                    labels = c("LHC - Low Tech", "Rest - Low Tech", 
                               "LHC - High Tech", "Rest - High Tech")) +
  labs(title = "Total CHF Amount by Year and Tech Level", x = "Year", y = "Total Amount (Millions CHF)", fill = "") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"), breaks = scales::pretty_breaks(n = 10)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box.spacing = grid::unit(1, "cm")  # Adjust spacing within the legend box
  )
ggsave(here("results","figures","fig_1B.jpeg"), fig_1B, width = 10, height = 7)# Adding lines and labels for the LHC construction period

## Figure 1C ----------------------------------------------------------------


all_orders_tech_graph <- all_orders_tech %>%
  group_by(order_date, LHC_period, tech_level) %>%
  summarise(order_count = n(), .groups = 'drop') %>%
  mutate(tech_level = factor(tech_level, levels = c(0, 1), labels = c("Low Tech", "High Tech")))

# Plotting with conditional color adjustments using alpha for transparency
fig_1C <- ggplot(all_orders_tech_graph, aes(x = order_date, y = order_count, fill = interaction(LHC_period, tech_level))) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("LHC Construction.Low Tech" = base_colors[1], 
                               "Rest of the Period.Low Tech" = alpha(base_colors[1], 0.5),
                               "LHC Construction.High Tech" = base_colors[2], 
                               "Rest of the Period.High Tech" = alpha(base_colors[2], 0.5)),
                    labels = c("LHC - Low Tech", "Rest - Low Tech", 
                               "LHC - High Tech", "Rest - High Tech")) +
  labs(title = "Total Number of Orders by Year and Tech Level", x = "Year", y = "Total Number of Orders", fill = "") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box.spacing = grid::unit(1, "cm")
  )
ggsave(here("results","figures","fig_1C.jpeg"), fig_1C, width = 10, height = 7)# Adding lines and labels for the LHC construction period


## Figure 2 ----------------------------------------------------------------


### Figure 2A -------------------------------------------------------------

# Create the data for all orders

all_orders_data_log_applications <- rbind(cs_all_log_applications_nevertreated_pre_log_fixed_assets_dynamic_plot_data_results,
                                          cs_all_log_applications_notyettreated_pre_log_fixed_assets_dynamic_plot_data_results)

all_orders_data_log_applications<- all_orders_data_log_applications %>% 
  filter(year>-6 & year <6)

fig_2A<- ggplot(all_orders_data_log_applications, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent applications") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

fig_2A
#ggsave(here("results","figures","fig_2.jpeg"), fig_2, width = 10, height = 7)


### Figure 2B ----------------------------------------------------------------


all_cs_ht_data<- rbind(cs_ht_log_applications_nevertreated_pre_log_fixed_assets_dynamic_plot_data_results,
                       cs_ht_log_applications_notyettreated_pre_log_fixed_assets_dynamic_plot_data_results)



all_cs_ht_data<- all_cs_ht_data %>% 
  filter(year>-6 & year<6)



fig_2B<- ggplot(all_cs_ht_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent applications") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

fig_2B



### Figure 2C ----------------------------------------------------------------

all_cs_lt_data<- rbind(cs_lt_log_applications_nevertreated_pre_log_fixed_assets_dynamic_plot_data_results,
                       cs_lt_log_applications_notyettreated_pre_log_fixed_assets_dynamic_plot_data_results)

all_cs_lt_data<- all_cs_lt_data %>% 
  filter(year>-6 & year<6)



fig_2C <- ggplot(all_cs_lt_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent applications") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

fig_2C


# Define a function to extract the legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
common_legend <- g_legend(fig_2A)
# Remove legends from individual plots and add titles


fig_2A <- fig_2A + labs(title = "All orders") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_2B <- fig_2B + labs(title = "High-tech orders") + guides(color=FALSE) + xlab(NULL) + ylab(NULL)
fig_2C <- fig_2C + labs(title = "Low-tech orders") + guides(color=FALSE) + xlab(NULL) + ylab(NULL)

# Arrange the plots and add the labels
fig_2<- grid.arrange(fig_2A,
   fig_2B, fig_2C, 
  ncol = 2, # Arrange in one column
  # Add the common x-axis label at the bottom plot
  bottom = textGrob("Relative timing", gp = gpar(fontface = "bold", cex = 1)),
  # Add the common y-axis label to the side of all plots
  left = textGrob("Effect on (log) patent applications", rot = 90, gp = gpar(fontface = "bold", cex = 1))
)




# Place the plots above the extracted legend

ggsave(paste0(figures_folder, "fig_2.jpeg"), fig_2, width = 10, height = 10)









# ggsave(here("results","figures","fig_3.jpeg"), fig_3, width = 10, height = 7)

### Figure 3A ----------------------------------------------------------------
all_cs_one_data<- rbind(cs_one_results_log_patent_stock_nevertreated_log_age_fixed_assets_dynamic_plot_data_results,
                         cs_one_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)


all_cs_one_data<- all_cs_one_data %>% 
  filter(year>-6)


fig_3A<- ggplot(all_cs_one_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

### Figure 3B ----------------------------------------------------------------
all_cs_multiple_data<- rbind(cs_multiple_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)


fig_3B<- ggplot(all_cs_multiple_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

### Figure 3C ----------------------------------------------------------------
all_cs_post_data<- cs_post_2008_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results

all_cs_post_data<- all_cs_post_data %>% 
  filter(year>-6)

fig_3C<- ggplot(all_cs_post_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-1, 1)

### Figure 3D ----------------------------------------------------------------
all_cs_pre_data<- rbind(cs_pre_2008_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)
all_cs_pre_data<- all_cs_pre_data %>% 
  filter(year>-5)

fig_3D<- ggplot(all_cs_pre_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

fig_3C
### Figure 3E ----------------------------------------------------------------
all_cs_100k_data<- rbind(cs_100k_results_log_patent_stock_nevertreated_log_age_fixed_assets_dynamic_plot_data_results,
                         cs_100k_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)

all_cs_100k_data<- all_cs_100k_data %>% 
  filter(year>-6)

fig_3D<- ggplot(all_cs_100k_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

fig_3E



### Figure 3F ----------------------------------------------------------------
all_cs_more_100k_data<- rbind(cs___100k_results_log_pat_applications_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)



fig_3F<- ggplot(all_cs_more_100k_data, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)

# ### Figure 3F ----------------------------------------------------------------
# 
# fig_3F<- ggplot(all_cs_1mil_data, aes(x = year, y = att, group = source, color = source)) +
#   geom_line(aes(group = source)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
#   geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
#   geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
#   labs(title = "",
#        x = "Relative timing",
#        y = "ATT",
#        color = "Model") +  # This customizes the legend title
#   scale_color_manual(values = c("red","darkcyan"), 
#                      labels = c("Callaway & Sant'Anna", "Callaway & Sant'Anna with covariates")) +  # This customizes the legend labels
#   theme(legend.position = "bottom",
#         panel.background = element_rect(fill = "white"),
#         plot.background = element_rect(fill = "white"),
#         panel.grid.major = element_line(color = "grey90"),
#         panel.grid.minor = element_line(color = "grey95")) +
#   ylim(-2, 2.5)  # This moves the legend to the bottom





# Define a function to extract the legend
g_legend <- function(a_gplot) {
  a_ggplot_build <- ggplot_build(a_gplot)
  a_gtable <- ggplot_gtable(a_ggplot_build)
  leg <- which(sapply(a_gtable$layout$name, function(x) x == "guide-box"))
  if (length(leg) == 0) {
    stop("No legend found")
  }
  legend <- a_gtable$grobs[[leg]]
  return(legend)
}
common_legend <- g_legend(fig_3A)




# Remove legends from individual plots and add titles
fig_3A <- fig_3A + labs(title = "Single orders") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_3B <- fig_3B + labs(title = "Multiple_orders") +guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_3D <- fig_3D + labs(title = "LHC-orders") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_3C <- fig_3C + labs(title = "Post-LHC orders") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_3E <- fig_3E + labs(title = "≤100k orders") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_3F <- fig_3F + labs(title = ">100k orders") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)

# Arrange the plots and add the labels
 fig_3<- fig_3 <- grid.arrange(
   grobs = list(fig_3A, fig_3B, fig_3C, fig_3D, fig_3E, fig_3F, common_legend),
      ncol = 2, 
 nrow = 4,  # Adjust the number of rows to accommodate the plots and the legend
 heights = c(rep(3, 3), 1),  # Adjust the heights to give appropriate space to the legend
   bottom = textGrob("Relative timing", gp = gpar(fontface = "bold", cex = 1)),
   left = textGrob("Effect on (log) patent stock", rot = 90, gp = gpar(fontface = "bold", cex = 1))
 )


# # Extract the legend from one of the plots (e.g., fig_3A)
# 
# # Arrange the plots in a 2x2 grid
# plots_grob <- arrangeGrob(fig_3A, fig_3B, fig_3, fig_3D, fig_3E, fig_3F, ncol = 2, nrow = 3)
# 
# 
# # Combine the plots grid and the legend using grid.arrange
# fig_3 <- grid.arrange(plots_grob, common_legend,
#                       nrow = 3, 
#                       heights = c(5, 1))  # Adjust the heights ratio as needed
# 
# # Save the final plot
# ggsave("fig_3.jpeg", fig_3, width = 12, height = 10)
# 
# 
# # Place the plots above the extracted legend
#fig_3<- grid.arrange(plots_grob, common_legend, nrow = 4, heights = c(rep(3, 3), 1))


ggsave(here("results","figures","fig_3.jpeg"), fig_3, width = 10, height = 10)


## Figure 4 ----------------------------------------------------------------


### Figure 4A ----------------------------------------------------------------



all_SME_data_log_patent_stock <- rbind(cs_SME_results_log_patent_stock_nevertreated_log_age_fixed_assets_dynamic_plot_data_results,
                                          cs_SME_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)
all_SME_data_log_patent_stock<- all_SME_data_log_patent_stock %>% 
  filter(year>-6)

fig_4A<- ggplot(all_SME_data_log_patent_stock, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 0.5)


### Figure 4B ----------------------------------------------------------------

all_large_data_log_patent_stock <- rbind(cs_large_results_log_patent_stock_nevertreated_log_age_fixed_assets_dynamic_plot_data_results,
                                       cs_large_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results)
all_large_data_log_patent_stock<- all_large_data_log_patent_stock %>% 
  filter(year>-6)

fig_4B<- ggplot(all_large_data_log_patent_stock, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-1, 1.5)




### Figure 4C ----------------------------------------------------------------

all_start_up_data_log_patent_stock <- cs_start_up_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results
all_start_up_data_log_patent_stock<- all_start_up_data_log_patent_stock %>% 
  filter(year>-6)



fig_4C<- ggplot(all_start_up_data_log_patent_stock, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 1.5)

### Figure 4D ----------------------------------------------------------------

all_incumbent_data_log_patent_stock <- cs_incumbent_results_log_patent_stock_notyettreated_log_age_fixed_assets_dynamic_plot_data_results
all_incumbent_data_log_patent_stock<- all_incumbent_data_log_patent_stock %>% 
  filter(year>-6)

fig_4D<- ggplot(all_incumbent_data_log_patent_stock, aes(x = year, y = att, group = control, color = control)) +
  geom_point(position = position_dodge(0.4)) +  # Apply dodging to points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.4)) +  # Apply dodging to error bars
  geom_vline(aes(xintercept = 0), linetype = "dashed", color ="black" ) +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "grey50") +
  labs(title = "",
       x = "Relative timing",
       y = "Effect on (log) patent stock") +  
  scale_color_manual(values = c("red","darkcyan"), 
                     labels = c("Baseline", "Late treated")) +  
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95")) +
  ylim(-0.5, 1.5)

common_legend <- g_legend(fig_4A)
# Remove legends from individual plots and add titles
fig_4A <- fig_4A + labs(title = "SMEs") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_4B <- fig_4B + labs(title = "Large Companies") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_4C <- fig_4C + labs(title = "Start-ups") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)
fig_4D <- fig_4D + labs(title = "Incumbent") + guides(color=FALSE)+ xlab(NULL) + ylab(NULL)

# Extract the legend from one of the plots (e.g., fig_3A)

# Arrange the plots in a 2x2 grid

fig_4<- grid.arrange(
  fig_4A, fig_4B, fig_4C, fig_4D, common_legend, 
  ncol = 2, # Arrange in one column
  # Add the common x-axis label at the bottom plot
  bottom = textGrob("Relative timing", gp = gpar(fontface = "bold", cex = 1)),
  # Add the common y-axis label to the side of all plots
  left = textGrob("Effect on (log) patent stock", rot = 90, gp = gpar(fontface = "bold", cex = 1))
)
ggsave(here("results","figures","fig_4.jpeg"), fig_4, width = 10, height = 7)


# Define the process in a function
# Load necessary package
# Load necessary package
if (!require("DiagrammeR")) install.packages("DiagrammeR")
library(DiagrammeR)

# Create a more detailed graph using DiagrammeR's graphviz feature
graph <- grViz("
digraph flowchart {
  # Graph properties
  graph [layout = dot, rankdir=LR]

  # Node properties
  node [shape = box, style = filled, fillcolor = lightblue, fontname = Helvetica]

  # Edge properties
  edge [color = gray, fontname = Helvetica]

  # Nodes
  Start [label = 'Estimated Contract Value']
  Direct [label = '< 10k CHF\nDirect Contact & Award']
  Inquiry [label = '10k-200k CHF\nPrice Inquiry']
  Public [label = '> 200k CHF\nPublic Announcement']
  Inquiry_Over_100k [label = 'Order > 100k CHF\nJustice Priority']
  Contract_Lowest_Bid [label = 'Award to Lowest Bid']
  Contract_Best_Value [label = 'Award to Best Value']
  End [label = 'End']

  # Edges with conditions
  Start -> Direct [label = 'Direct']
  Start -> Inquiry [label = 'Price Inquiry']
  Start -> Public [label = 'Public Tender']
  Inquiry -> Inquiry_Over_100k [label = 'If > 100k CHF']
  Inquiry -> Contract_Lowest_Bid [label = 'If ≤ 100k CHF\nLowest Compliant Bid']
  Inquiry_Over_100k -> Contract_Best_Value [label = 'Best Value for Money']
  Direct -> End
  Contract_Lowest_Bid -> End
  Contract_Best_Value -> End
  Public -> Contract_Best_Value [label = 'Qualification Met']
}
")

# Print the graph
print(graph)
