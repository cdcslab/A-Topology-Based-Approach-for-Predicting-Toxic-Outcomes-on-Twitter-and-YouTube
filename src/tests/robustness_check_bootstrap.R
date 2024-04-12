library(arrow)
library(dplyr)
library(arrow)

# Reset ####
rm(list = ls())
gc()

# Variables ####
social = "youtube"

filename <-
  paste(social,
        "overall_graph_metrics.parquet",
        sep = "_")

overall_graph_metrics_filename <-
  paste(
    "overall_tree_data",
    filename,
    sep = "/"
  )

output_filename <- paste(
  "robustness_check_data",
  paste(social, "bootstrap_results.parquet", sep = "_"),
  sep = "/"
)

# Load Data ####
print(paste("Reading", overall_graph_metrics_filename))

df_metrics <- read_parquet(overall_graph_metrics_filename)
football_graph_metrics <- df_metrics %>%
  filter(topic == "football")
elections_graph_metrics <- df_metrics %>%
  filter(topic == "elections")


football_graph_metrics$assortativity = as.numeric(football_graph_metrics$assortativity)
elections_graph_metrics$assortativity = as.numeric(elections_graph_metrics$assortativity)

# Boostrap procedure ####
"
1. Perform a KS test on original elections and football data and save the statistic
2. Combine elections and football data from the same social platform
3. Extract two samples from this combined data, where the sample size is the maximum size between the two topics
4. Perform a KS test on the two samples and check if KS_sample < KS_original
"

features_to_test <- colnames(elections_graph_metrics)[8:15]
feature_index <- 8
n_extractions <- 1000

total_bootstrap_ks_statistics <-
  tibble(
    i = numeric(),
    sample_statistic = numeric(),
    original_statistic = numeric(),
    bootstrap_size = numeric(),
    topic = character()
  )

for (feature in features_to_test)
{
  feature_ks_statistics = tibble(
    i = numeric(),
    sample_statistic = numeric(),
    original_statistic = numeric(),
    bootstrap_size = numeric(),
    topic = character()
  )
  
  print(paste("Working with feature", colnames(elections_graph_metrics)[feature_index]))
  print("Performing KS original on original data")
  
  original_ks_test_statistic <-
    ks.test(elections_graph_metrics[, feature_index],
            football_graph_metrics[, feature_index],
            na.action = na.omit)$statistic
  
  df_elections_feature_without_na <-
    elections_graph_metrics[, feature_index] %>%
    na.omit() %>%
    as.data.frame() %>%
    rename(feature = ".")
  
  df_football_feature_without_na <-
    football_graph_metrics[, feature_index] %>%
    na.omit() %>%
    as.data.frame() %>%
    rename(feature = ".")
  
  df_total_for_bootstrap <- rbind(df_elections_feature_without_na,
                                  df_football_feature_without_na)
  
  bootstrap_sample_size <-
    max(dim(df_elections_feature_without_na)[1],
        dim(df_football_feature_without_na)[1])
  print(paste("Working with sample size of", bootstrap_sample_size))
  
  
  n_of_lower_sample_statistics = 0
  for (i in 1:n_extractions)
  {
    sample_1 <- df_total_for_bootstrap %>%
      sample_n(bootstrap_sample_size)
    
    sample_2 <- df_total_for_bootstrap %>%
      sample_n(bootstrap_sample_size)
    
    sample_test_statistic <- ks.test(sample_1$feature,
                                     sample_2$feature,
                                     na.action = na.omit())$statistic
    
    sample_test_statistic <-
      tibble(i,
             sample_statistic = sample_test_statistic)
    
    feature_ks_statistics <- rbind(feature_ks_statistics,
                                   sample_test_statistic)
  }
  
  print("Saving results to final dataframe")
  feature_ks_statistics$original_statistic <-
    original_ks_test_statistic
  feature_ks_statistics$feature <- feature
  feature_ks_statistics$bootstrap_size <- bootstrap_sample_size
  
  total_bootstrap_ks_statistics <-
    rbind(total_bootstrap_ks_statistics,
          feature_ks_statistics)
  
  feature_index <- feature_index + 1
}


# Plot Results ####
total_bootstrap_ks_statistics$sample_statistic <-
  as.vector(unlist(total_bootstrap_ks_statistics$sample_statistic, use.names = FALSE))
total_bootstrap_ks_statistics$original_statistic <-
  as.vector(unlist(
    total_bootstrap_ks_statistics$original_statistic,
    use.names = FALSE
  ))

plot_total_bootstrap_ks_statistics <-
  ggplot(total_bootstrap_ks_statistics) +
  geom_line(aes(i, sample_statistic)) +
  geom_hline(data = total_bootstrap_ks_statistics,
             aes(yintercept = original_statistic),
             color = "red") +
  facet_wrap(~ feature, ncol = 4, scales = "free") +
  ylim(0, .4) +
  labs(x = "Extraction Index",
       y = "KS Statistic")
plot_total_bootstrap_ks_statistics

plot_hist_total_bootstrap_ks_statistics <-
  ggplot(total_bootstrap_ks_statistics) +
  geom_histogram(aes(sample_statistic)) +
  geom_vline(data = total_bootstrap_ks_statistics,
             aes(xintercept = original_statistic),
             color = "red") +
  facet_wrap(~ feature, ncol = 4, scales = "free") +
  labs(x = "KS Statistic",
       y = "Count")
plot_hist_total_bootstrap_ks_statistics

# Write results ####
print("Writing results")
write_parquet(total_bootstrap_ks_statistics,
              output_filename)
