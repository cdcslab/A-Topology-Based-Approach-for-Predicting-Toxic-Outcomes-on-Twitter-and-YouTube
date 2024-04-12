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
    "overall_tree_data",
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

# Toxicity Ratio
ks_test_toxicity_ratio <-
  wilcox.test(
    football_graph_metrics$toxicity_ratio,
    elections_graph_metrics$toxicity_ratio,
    na.action = na.omit()
  )$p.value
ks_test_toxicity_ratio

# Tree Size
ks_test_tree_size <- wilcox.test(
  football_graph_metrics$tree_size,
  elections_graph_metrics$tree_size,
  na.action = na.omit()
)$p.value
ks_test_tree_size

# Max Width
ks_test_max_width <- wilcox.test(
  football_graph_metrics$max_width,
  elections_graph_metrics$max_width,
  na.action = na.omit()
)$p.value
ks_test_max_width

# Max Depth
ks_test_max_depth <- wilcox.test(
  football_graph_metrics$max_depth,
  elections_graph_metrics$max_depth,
  na.action = na.omit()
)$p.value
ks_test_max_depth

# Avg. Toxicity Distance
ks_test_avg_toxicity_distance <-
  wilcox.test(
    football_graph_metrics$avg_toxicity_distance,
    elections_graph_metrics$avg_toxicity_distance,
    na.action = na.omit()
  )$p.value
ks_test_avg_toxicity_distance

# Wiener Index
ks_test_wiener_index <-
  wilcox.test(
    football_graph_metrics$wiener_index,
    elections_graph_metrics$wiener_index,
    na.action = na.omit()
  )$p.value
ks_test_wiener_index

# N. of Unique Users
ks_test_number_of_unique_users <-
  wilcox.test(
    football_graph_metrics$number_of_unique_users,
    elections_graph_metrics$number_of_unique_users,
    na.action = na.omit()
  )$p.value
ks_test_number_of_unique_users

