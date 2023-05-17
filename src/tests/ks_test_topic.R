library(arrow)
library(dplyr)

"
Compute KS test between two topics
"
rm(list = ls())
gc()

# Load Data ####
social = "twitter"
is_shuffled = FALSE

filename <- ifelse(
  is_shuffled == FALSE,
  paste(social,
        "overall_graph_metrics.parquet",
        sep = "_"),
  paste(social,
        "shuffled_graph_metrics.parquet",
        sep = "_")
)

overall_graph_metrics_filename <-
  paste(
    "/media/gabett/Volume/data-repository/panconesi-football-elections/overall_tree_data",
    filename,
    sep = "/"
  )

print(paste("Reading", overall_graph_metrics_filename))

df_metrics <- read_parquet(overall_graph_metrics_filename)
football_graph_metrics <- df_metrics %>%
  filter(topic == "football")
elections_graph_metrics <- df_metrics %>%
  filter(topic == "elections")

# KS Tests ####
sample_size <- 100
pvalue_toxicity_ratio = tibble()
pvalue_tree_size = tibble()
pvalue_max_width = tibble()
pvalue_max_depth = tibble()
pvalue_wiener_index = tibble()
pvalue_unique_users = tibble()
pvalue_avg_toxicity_distance = tibble()

# Toxicity Ratio

for(i in 1:100)
{
  print(i)
  
  sample_football_graph_metrics <- football_graph_metrics %>% 
    sample_n(sample_size)
   
  sample_elections_graph_metrics <- elections_graph_metrics %>% 
    sample_n(sample_size)
  
  ks_test_toxicity_ratio <-
    ks.test(
      sample_football_graph_metrics$toxicity_ratio,
      sample_elections_graph_metrics$toxicity_ratio,
      na.action = na.omit()
    )$p.value
  ks_test_toxicity_ratio < tibble(pvalue = ks_test_toxicity_ratio)
  pvalue_toxicity_ratio <- rbind(pvalue_toxicity_ratio, ks_test_toxicity_ratio)
  toxicity_ratio_non_significative_percentage <- length(which(pvalue_toxicity_ratio > 0.05)) / length(pvalue_toxicity_ratio)
  
  # Tree Size
  ks_test_tree_size <- ks.test(
    sample_football_graph_metrics$tree_size,
    sample_elections_graph_metrics$tree_size,
    na.action = na.omit()
  )$p.value
  ks_test_tree_size <- tibble(pvalue = ks_test_tree_size)
  pvalue_tree_size <- rbind(pvalue_tree_size, ks_test_tree_size)
  tree_size_non_significative_percentage <- length(which(pvalue_tree_size > 0.05)) / length(pvalue_tree_size)
  
  # Max Width
  ks_test_max_width <- ks.test(
    sample_football_graph_metrics$max_width,
    sample_elections_graph_metrics$max_width,
    na.action = na.omit()
  )$p.value
  ks_test_max_width <- tibble(pvalue = ks_test_max_width)
  pvalue_max_width <- rbind(pvalue_max_width, ks_test_max_width)
  
  max_width_non_significative_percentage <- length(which(pvalue_max_width > 0.05)) / length(pvalue_max_width)
  max_width_non_significative_percentage <- tibble(max_width_non_significative_percentage)
  max_width_non_significative_percentage$metric = "Max Width"
  
  # Max Depth
  ks_test_max_depth <- ks.test(
    sample_football_graph_metrics$max_depth,
    sample_elections_graph_metrics$max_depth,
    na.action = na.omit()
  )$p.value
  ks_test_max_depth <- tibble(ks_test_max_depth)
  pvalue_max_depth <- rbind(pvalue_max_depth, ks_test_max_depth)
  
  max_depth_non_significative_percentage <- length(which(pvalue_max_depth > 0.05)) / length(pvalue_max_depth)
  max_depth_non_significative_percentage <- tibble(max_depth_non_significative_percentage)
  max_depth_non_significative_percentage$metric = "Max Depth"
  
  # Avg. Toxicity Distance
  ks_test_avg_toxicity_distance <-
    ks.test(
      sample_football_graph_metrics$avg_toxicity_distance,
      sample_elections_graph_metrics$avg_toxicity_distance,
      na.action = na.omit()
    )$p.value
  ks_test_avg_toxicity_distance <- tibble(ks_test_avg_toxicity_distance)
  pvalue_avg_toxicity_distance <- rbind(pvalue_avg_toxicity_distance, ks_test_avg_toxicity_distance)
  
  avg_toxicity_distance_non_significative_percentage <- length(which(pvalue_avg_toxicity_distance > 0.05)) / length(pvalue_avg_toxicity_distance)
  avg_toxicity_distance_non_significative_percentage <- tibble(avg_toxicity_distance_non_significative_percentage)
  avg_toxicity_distance_non_significative_percentage$metric = "Avg. Toxicity Distance"
  
  # Wiener Index
  ks_test_wiener_index <-
    ks.test(
      sample_football_graph_metrics$wiener_index,
      sample_elections_graph_metrics$wiener_index,
      na.action = na.omit()
    )$p.value
  ks_test_wiener_index <- tibble(ks_test_wiener_index)
  pvalue_wiener_index <- rbind(pvalue_wiener_index, ks_test_wiener_index)
  
  wiener_index_non_significative_percentage <- length(which(pvalue_wiener_index > 0.05)) / length(pvalue_wiener_index)
  wiener_index_non_significative_percentage <- tibble(wiener_index_non_significative_percentage)
  wiener_index_non_significative_percentage$metric = "Wiener Index"
  
  # N. of Unique Users
  ks_test_number_of_unique_users <-
    ks.test(
      sample_football_graph_metrics$number_of_unique_users,
      sample_elections_graph_metrics$number_of_unique_users,
      na.action = na.omit()
    )$p.value
  ks_test_number_of_unique_users <- tibble(ks_test_number_of_unique_users)
  pvalue_unique_users <- rbind(pvalue_wiener_index, ks_test_number_of_unique_users)
  pvalue_unique_users <- tibble(pvalue_unique_users)
  
  unique_users_non_significative_percentage <- length(which(pvalue_unique_users > 0.05)) / length(pvalue_unique_users)
  unique_users_non_significative_percentage <- tibble(unique_users_non_significative_percentage)
  unique_users_non_significative_percentage$metric = "Unique Users"
  }

