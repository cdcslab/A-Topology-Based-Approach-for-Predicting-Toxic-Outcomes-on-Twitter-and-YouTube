library(arrow)
library(dplyr)

rm(list = ls())
gc()
set.seed(42)

# Load Twitter Data ####

tp = "elections"
is_shuffled = FALSE

filename <- ifelse(
  is_shuffled == FALSE,
  paste("twitter",
        "overall_graph_metrics.parquet",
        sep = "_"),
  paste("twitter",
        "shuffled_graph_metrics.parquet",
        sep = "_")
)

twitter_topic_graph_metrics_filename <-
  paste(
    "/media/gabett/Volume/data-repository/panconesi-football-elections/overall_tree_data",
    filename,
    sep = "/"
  )

twitter_topic_graph_metrics <- read_parquet(twitter_topic_graph_metrics_filename)

twitter_topic_graph_metrics <- twitter_topic_graph_metrics %>% 
  filter(topic == tp)

# Load YouTube Data ####
social = "youtube"
filename <- ifelse(
  is_shuffled == FALSE,
  paste("youtube",
        "overall_graph_metrics.parquet",
        sep = "_"),
  paste("youtube",
        "shuffled_graph_metrics.parquet",
        sep = "_")
)

youtube_topic_graph_metrics_filename <-
  paste(
    "/media/gabett/Volume/data-repository/panconesi-football-elections/overall_tree_data",
    filename,
    sep = "/"
  )

youtube_topic_graph_metrics <- read_parquet(youtube_topic_graph_metrics_filename)
youtube_topic_graph_metrics <- youtube_topic_graph_metrics %>%
  filter(topic == tp)

# KS Tests ####

# Toxicity Ratio
ks_test_toxicity_ratio <- ks.test(twitter_topic_graph_metrics$toxicity_ratio, 
                                  youtube_topic_graph_metrics$toxicity_ratio,
                                  na.action = na.omit())$p.value
ks_test_toxicity_ratio

# Assortativity
# ks_test_assortativity <- ks.test(twitter_topic_graph_metrics$assortativity, 
#                                  youtube_topic_graph_metrics$assortativity, na.action = na.omit(), na.action = na.omit())$p.value
# ks_test_assortativity

# Tree Size
ks_test_tree_size <- ks.test(twitter_topic_graph_metrics$tree_size, 
                             youtube_topic_graph_metrics$tree_size, na.action = na.omit(), na.action = na.omit())$p.value
ks_test_tree_size

# Max Width
ks_test_max_width <- ks.test(twitter_topic_graph_metrics$max_width, 
                             youtube_topic_graph_metrics$max_width, na.action = na.omit(), na.action = na.omit())$p.value
ks_test_max_width

# Max Depth
ks_test_max_depth <- ks.test(twitter_topic_graph_metrics$max_depth, 
                             youtube_topic_graph_metrics$max_depth, na.action = na.omit(), na.action = na.omit())$p.value
ks_test_max_depth

# Avg. Toxicity Distance
ks_test_avg_toxicity_distance <- ks.test(twitter_topic_graph_metrics$avg_toxicity_distance, 
                                         youtube_topic_graph_metrics$avg_toxicity_distance, na.action = na.omit())$p.value
ks_test_avg_toxicity_distance

# Wiener Index
ks_test_wiener_index <- ks.test(twitter_topic_graph_metrics$wiener_index, 
                                youtube_topic_graph_metrics$wiener_index, na.action = na.omit())$p.value
ks_test_wiener_index

# N. of Unique Users
ks_test_number_of_unique_users <- ks.test(twitter_topic_graph_metrics$number_of_unique_users, 
                                          youtube_topic_graph_metrics$number_of_unique_users, na.action = na.omit())$p.value
ks_test_number_of_unique_users