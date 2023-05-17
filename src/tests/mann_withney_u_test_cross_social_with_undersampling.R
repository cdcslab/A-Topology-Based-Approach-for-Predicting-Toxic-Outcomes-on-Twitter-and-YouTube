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

# Performing undersampling

min_topic <- which.min(c(dim(twitter_topic_graph_metrics)[1], dim(youtube_topic_graph_metrics)[1]))
min_size <- min(c(dim(twitter_topic_graph_metrics)[1], dim(youtube_topic_graph_metrics)[1]))

if(min_topic == 1) # Twitter
{
  resampled_youtube_topic_graph_metrics <- youtube_topic_graph_metrics %>% 
    sample_n(min_size)
  resampled_twitter_topic_graph_metrics <- twitter_topic_graph_metrics
} else { # Youtube
  resampled_twitter_topic_graph_metrics <- twitter_topic_graph_metrics %>% 
    sample_n(min_size)
  resampled_youtube_topic_graph_metrics <- youtube_topic_graph_metrics
}
# KS Tests ####

# Toxicity Ratio
ks_test_toxicity_ratio <-
  wilcox.test(
    resampled_twitter_topic_graph_metrics$toxicity_ratio,
    resampled_youtube_topic_graph_metrics$toxicity_ratio,
    na.rm=TRUE,
    paired = FALSE
  )$p.value
ks_test_toxicity_ratio

# Assortativity
# ks_test_assortativity <-
#   wilcox.test(
#     resampled_twitter_topic_graph_metrics$assortativity,
#     resampled_youtube_topic_graph_metrics$assortativity,
#     na.rm = TRUE
#   )$p.value
# ks_test_assortativity

# Tree Size
ks_test_tree_size <- wilcox.test(
  resampled_twitter_topic_graph_metrics$tree_size,
  resampled_youtube_topic_graph_metrics$tree_size,
  na.rm = TRUE,
  paired = F
)$p.value
ks_test_tree_size

# Max Width
ks_test_max_width <- wilcox.test(
  resampled_twitter_topic_graph_metrics$max_width,
  resampled_youtube_topic_graph_metrics$max_width,
  na.rm = TRUE
)$p.value
ks_test_max_width

# Max Depth
ks_test_max_depth <- wilcox.test(
  resampled_twitter_topic_graph_metrics$max_depth,
  resampled_youtube_topic_graph_metrics$max_depth,
  na.rm = TRUE,
  paired = F
)$p.value
ks_test_max_depth

# Avg. Toxicity Distance
ks_test_avg_toxicity_distance <-
  wilcox.test(
    resampled_twitter_topic_graph_metrics$avg_toxicity_distance,
    resampled_youtube_topic_graph_metrics$avg_toxicity_distance,
    na.rm = TRUE
  )$p.value
ks_test_avg_toxicity_distance

# Wiener Index
ks_test_wiener_index <-
  wilcox.test(
    resampled_twitter_topic_graph_metrics$wiener_index,
    resampled_youtube_topic_graph_metrics$wiener_index,
    na.rm = TRUE
  )$p.value
ks_test_wiener_index

# N. of Unique Users
ks_test_number_of_unique_users <-
  wilcox.test(
    resampled_twitter_topic_graph_metrics$number_of_unique_users,
    resampled_youtube_topic_graph_metrics$number_of_unique_users,
    na.rm = TRUE
  )$p.value
ks_test_number_of_unique_users

