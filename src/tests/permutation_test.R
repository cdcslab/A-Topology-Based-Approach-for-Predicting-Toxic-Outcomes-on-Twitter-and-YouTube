library(arrow)
library(dplyr)
library(arrow)
library(ggplot2)
library(stringr)

set.seed(42)

# Reset ####
rm(list = ls())
gc()

# Variables ####
social = "twitter"

filename <-
  paste(social,
        "overall_graph_metrics.parquet",
        sep = "_")

overall_graph_metrics_filename <-
  paste(
    "/media/gabett/Volume/data-repository/panconesi-football-elections/overall_tree_data",
    filename,
    sep = "/"
  )

output_filename <- paste(
  "/media/gabett/Volume/data-repository/panconesi-football-elections/robustness_check_data",
  paste(social, "permutation_results.parquet", sep = "_"),
  sep = "/"
)

pvalue_output_filename <- paste(
  "/media/gabett/Volume/data-repository/panconesi-football-elections/robustness_check_data",
  paste(social, "pvalues_permutation_results.parquet", sep = "_"),
  sep = "/"
)

figures_folder <-
  paste(
    "/home/gabett/Documents/repository/football-elections-cascade-comparison/figures/tests"
  )
figure_output_filename <- paste(figures_folder,
                                paste(social, "permutation_test_results.pdf", sep = "_"),
                                sep = "/")
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

features_to_test <- colnames(elections_graph_metrics)[8:15]
feature_index <- 8
n_extractions <- 1000

total_mean_differences <-
  tibble(
    i = numeric(),
    difference_mean_shuffled = numeric(),
    difference_mean_original = numeric(),
    topic = character()
  )


for (feature in features_to_test)
{
  feature_shuffled_mean_difference = tibble(
    i = numeric(),
    difference_mean_shuffled = numeric(),
    difference_mean_original = numeric(),
    topic = character()
  )
  
  print(paste("Working with feature", colnames(elections_graph_metrics)[feature_index]))
  
  df_elections_feature_without_na <-
    elections_graph_metrics[, feature_index] %>%
    na.omit() %>%
    as.data.frame() %>%
    rename(feature = ".") %>%
    mutate(topic = "elections")
  
  df_football_feature_without_na <-
    football_graph_metrics[, feature_index] %>%
    na.omit() %>%
    as.data.frame() %>%
    rename(feature = ".") %>%
    mutate(topic = "football")
  
  difference_mean_original_datasets <-
    abs(
      mean(df_elections_feature_without_na$feature,
           na.rm = T) - mean(df_football_feature_without_na$feature,
                             na.rm = T)
    )
  
  df_total_for_feature <- rbind(df_elections_feature_without_na,
                                df_football_feature_without_na) %>%
    setDT()
  
  for (i in 1:n_extractions)
  {
    df_total_for_feature[, feature := sample(feature)]
    
    elections_shuffled <- df_total_for_feature %>%
      filter(topic == "elections")
    
    football_shuffled <- df_total_for_feature %>%
      filter(topic == "football")
    
    difference_mean_shuffled_datasets <-
      abs(
        mean(elections_shuffled$feature,
             na.rm = T) -
          mean(football_shuffled$feature,
               na.rm = T)
      )
    
    shuffled_sample_difference <-
      tibble(i,
             difference_mean_shuffled = difference_mean_shuffled_datasets)
    
    feature_shuffled_mean_difference <-
      rbind(feature_shuffled_mean_difference,
            shuffled_sample_difference)
  }
  
  print("Saving results to final dataframe")
  feature_shuffled_mean_difference$difference_mean_original <-
    difference_mean_original_datasets
  feature_shuffled_mean_difference$feature <- feature
  
  total_mean_differences <-
    rbind(total_mean_differences,
          feature_shuffled_mean_difference)
  
  feature_index <- feature_index + 1
}

p_values_by_topic <- total_mean_differences %>%
  group_by(feature) %>%
  summarise(p_value = sum(
    abs(difference_mean_shuffled) >= abs(difference_mean_original)
  ) / n_extractions)

total_mean_differences <- as.data.frame(total_mean_differences)
total_mean_differences$feature <-
  str_replace_all(total_mean_differences$feature, "_", " ")
total_mean_differences$feature <-
  str_to_title(total_mean_differences$feature)

plot_test_distributions <- ggplot(data = total_mean_differences) +
  geom_histogram(aes(x = difference_mean_shuffled),
                 col = "black",
                 fill = "lightblue") +
  facet_wrap( ~ feature, ncol = 3, nrow = 3, scale = "free_x") +
  geom_vline(aes(xintercept = difference_mean_original), col = "red") +
  geom_vline(aes(xintercept = -difference_mean_original),
             col = "red") +
  labs(x = "Test Statistics",
       y = "Count") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    strip.text.x = element_text(size = 20),
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom"
  )
plot_test_distributions

# Write results ####
print("Writing results")
write_parquet(total_mean_differences,
              output_filename)

write_parquet(p_values_by_topic,
      pvalue_output_filename)

ggsave(
  figure_output_filename,
  plot_test_distributions,
  width = 40,
  height = 40,
  device = "pdf",
  units = "cm"
)

