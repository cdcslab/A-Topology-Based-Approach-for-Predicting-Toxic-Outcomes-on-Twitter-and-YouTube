library(arrow)
library(dplyr)

"
Compute KS test between two topics
"
set.seed(42)
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
    filename,
    sep = "/"
  )

print(paste("Reading", overall_graph_metrics_filename))

df_metrics <- read_parquet(overall_graph_metrics_filename)
resampled_football <- df_metrics %>%
  filter(topic == "football")
resampled_elections <- df_metrics %>%
  filter(topic == "elections")

# Performing undersampling
min_topic <- which.min(c(dim(resampled_football)[1], dim(resampled_elections)[1]))
min_size <- min(c(dim(resampled_football)[1], dim(resampled_elections)[1]))

if(min_topic == 1) # Football
{
  resampled_elections <- resampled_elections %>% 
    sample_n(min_size)
  resampled_football <- resampled_football
} else { # Elections
  resampled_football <- resampled_football %>% 
    sample_n(min_size)
  resampled_elections <- resampled_elections
}
# KS Tests ####

# Toxicity Ratio
ks_test_toxicity_ratio <-
  ks.test(
    resampled_football$toxicity_ratio,
    resampled_elections$toxicity_ratio,
    na.action = na.omit()
  )$p.value
ks_test_toxicity_ratio

# Tree Size
ks_test_tree_size <- ks.test(
  resampled_football$tree_size,
  resampled_elections$tree_size,
  na.action = na.omit()
)$p.value
ks_test_tree_size

# Max Width
ks_test_max_width <- ks.test(
  resampled_football$max_width,
  resampled_elections$max_width,
  na.action = na.omit()
)$p.value
ks_test_max_width

# Max Depth
ks_test_max_depth <- ks.test(
  resampled_football$max_depth,
  resampled_elections$max_depth,
  na.action = na.omit()
)$p.value
ks_test_max_depth

# Avg. Toxicity Distance
ks_test_avg_toxicity_distance <-
  ks.test(
    resampled_football$avg_toxicity_distance,
    resampled_elections$avg_toxicity_distance,
    na.action = na.omit()
  )$p.value
ks_test_avg_toxicity_distance

# Wiener Index
ks_test_wiener_index <-
  ks.test(
    resampled_football$wiener_index,
    resampled_elections$wiener_index,
    na.action = na.omit()
  )$p.value
ks_test_wiener_index

# N. of Unique Users
ks_test_number_of_unique_users <-
  ks.test(
    resampled_football$number_of_unique_users,
    resampled_elections$number_of_unique_users,
    na.action = na.omit()
  )$p.value
ks_test_number_of_unique_users

