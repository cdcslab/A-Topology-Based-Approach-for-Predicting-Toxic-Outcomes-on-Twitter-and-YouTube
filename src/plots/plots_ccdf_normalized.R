library(tibble)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)
library(arrow)
rm(list = ls())
gc()

is_toxicity_shuffling_enabled = F

# Functions ####
create_ccdf <- function(x) {
  x <- as.numeric(x)
  ecdf_vector = ecdf(x)
  ccdf_vector <- 1 - ecdf_vector(sort(unique(x)))
  ccdf <- cbind.data.frame(x = sort(unique(x)), y = ccdf_vector)
  return(ccdf)
}



create_normalized_ccdf <- function(x) {
  x = as.numeric(x)
  x = na.omit(x)

  max_x = max(x)
  min_x = min(x)
  
  
  if (max_x - min_x == 0)
    min_x = -1
    #stop("Denominator is equal to 0.")
  
  x <- (x - min_x) / (max_x - min_x)
  
  
  ecdf_vector = ecdf(x)
  ccdf_vector <- 1 - ecdf_vector(sort(unique(x)))
  ccdf <- cbind.data.frame(x = sort(unique(x)), y = ccdf_vector)
  
  return(ccdf)
}

# Variables ####
youtube_graph_metrics <- read_parquet(
  "./overall_tree_data/youtube_overall_graph_metrics.parquet"
)

twitter_graph_metrics <- read_parquet(
  "./overall_tree_data/twitter_overall_graph_metrics.parquet"
)

if(is_toxicity_shuffling_enabled)
{
  figures_folder = "./figures/ccdfs_normalized/shuffled"
} else {
  figures_folder = "./figures/ccdfs_normalized/unshuffled"
}

topic_colors <- c("Football" = "#ff7b00",
                  "Elections" = "#003f88")

# Youtube CCDF Analysis ####
graph_metrics_for_plotting <- youtube_graph_metrics %>%
  select(-c(
    children_index,
    created_at,
    id,
    video_id,
    parent_id,
    root
  ))

# Elections
elections_graph_metrics_for_plotting <-
  graph_metrics_for_plotting %>%
  filter(topic == "elections")

elections_ccdf_tree_size = create_normalized_ccdf(elections_graph_metrics_for_plotting$tree_size)
elections_ccdf_max_width = create_normalized_ccdf(elections_graph_metrics_for_plotting$max_width)
elections_ccdf_max_depth = create_normalized_ccdf(elections_graph_metrics_for_plotting$max_depth)
elections_ccdf_number_of_unique_users = create_normalized_ccdf(elections_graph_metrics_for_plotting$number_of_unique_users)
elections_ccdf_toxicity_ratio = create_normalized_ccdf(elections_graph_metrics_for_plotting$toxicity_ratio)

elections_ccdf_assortativity = create_normalized_ccdf(elections_graph_metrics_for_plotting$assortativity)
elections_ccdf_avg_toxicity_distance = create_normalized_ccdf(elections_graph_metrics_for_plotting$avg_toxicity_distance)
elections_ccdf_wiener_index = create_normalized_ccdf(elections_graph_metrics_for_plotting$wiener_index)

# Football
football_graph_metrics_for_plotting <-
  graph_metrics_for_plotting %>%
  filter(topic == "football")

football_ccdf_tree_size = create_normalized_ccdf(football_graph_metrics_for_plotting$tree_size)
football_ccdf_max_width = create_normalized_ccdf(football_graph_metrics_for_plotting$max_width)
football_ccdf_max_depth = create_normalized_ccdf(football_graph_metrics_for_plotting$max_depth)
football_ccdf_number_of_unique_users = create_normalized_ccdf(football_graph_metrics_for_plotting$number_of_unique_users)
football_ccdf_toxicity_ratio = create_normalized_ccdf(football_graph_metrics_for_plotting$toxicity_ratio)
football_ccdf_assortativity = create_normalized_ccdf(football_graph_metrics_for_plotting$assortativity)
football_ccdf_avg_toxicity_distance = create_normalized_ccdf(football_graph_metrics_for_plotting$avg_toxicity_distance)
football_ccdf_wiener_index = create_normalized_ccdf(football_graph_metrics_for_plotting$wiener_index)

df_max_width = rbind(
  tibble(elections_ccdf_max_width,
         topic = "Football"),
  tibble(football_ccdf_max_width,
         topic = "Elections")
)
df_max_width$metric = "Max Width"

df_assortativity = rbind(
  tibble(elections_ccdf_assortativity,
         topic = "Football"),
  tibble(football_ccdf_assortativity,
         topic = "Elections")
)
df_assortativity$metric = "Assortativity"

df_max_depth = rbind(
  tibble(elections_ccdf_max_depth,
         topic = "Football"),
  tibble(football_ccdf_max_depth,
         topic = "Elections")
)
df_max_depth$metric = "Max Depth"

df_avg_toxicity_distance = rbind(
  tibble(elections_ccdf_avg_toxicity_distance,
         topic = "Football"),
  tibble(football_ccdf_avg_toxicity_distance,
         topic = "Elections")
)
df_avg_toxicity_distance$metric = "Avg. Toxicity\nDistance"

df_wiener_index = rbind(
  tibble(elections_ccdf_wiener_index,
         topic = "Football"),
  tibble(football_ccdf_wiener_index,
         topic = "Elections")
)
df_wiener_index$metric = "Wiener Index"

df_tree_size = rbind(
  tibble(elections_ccdf_tree_size,
         topic = "Football"),
  tibble(football_ccdf_tree_size,
         topic = "Elections")
)
df_tree_size$metric = "Tree Size"

df_toxicity_ratio = rbind(
  tibble(elections_ccdf_toxicity_ratio,
         topic = "Elections"),
  tibble(football_ccdf_toxicity_ratio,
         topic = "Football")
)
df_toxicity_ratio$metric = "Toxicity Ratio"

df_number_of_unique_users = rbind(
  tibble(elections_ccdf_number_of_unique_users,
         topic = "Football"),
  tibble(football_ccdf_number_of_unique_users,
         topic = "Elections")
)
df_number_of_unique_users$metric = "N. Unique\nUsers"

df_youtube = rbind(
  df_assortativity,
  df_avg_toxicity_distance,
  df_max_depth,
  df_max_width,
  df_number_of_unique_users,
  df_toxicity_ratio,
  df_tree_size,
  df_wiener_index
)

# Plot
youtube_ccdf_plot <- ggplot(data = df_youtube) +
  geom_line(aes(x, y, color = topic),  alpha = 0.8) +
  geom_point(aes(x, y, color = topic)) +
  facet_wrap(~ metric,
             ncol = 8,
             scales = "free_x",
             shrink = T) +
  scale_x_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x, n = 4),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_y_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_color_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(
    y = "CCDF",
    x = "Metric Value",
    title = "YouTube",
    colour = "Topic"
  ) +
  theme_classic() +
  theme(
    strip.text.x = element_text(size = 25),
    strip.text.y = element_text(size = 30),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines")
  )
youtube_ccdf_plot

# Saving Plot
ccdf_plot_filename = paste(figures_folder,
                           "youtube_overall_tree_ccdf_normalized.pdf", 
                           sep = "/")

ggsave(
  ccdf_plot_filename,
  youtube_ccdf_plot,
  width = 40,
  height = 20,
  units = "cm",
  device = "pdf"
)

# Twitter CCDF Analysis ####
graph_metrics_for_plotting <- twitter_graph_metrics %>%
  select(-c(
    children_index,
    created_at,
    id,
    conversation_id,
    parent_id,
    root
  ))

# Elections
elections_graph_metrics_for_plotting <-
  graph_metrics_for_plotting %>%
  filter(topic == "elections")

elections_ccdf_tree_size = create_normalized_ccdf(elections_graph_metrics_for_plotting$tree_size)
elections_ccdf_max_width = create_normalized_ccdf(elections_graph_metrics_for_plotting$max_width)
elections_ccdf_max_depth = create_normalized_ccdf(elections_graph_metrics_for_plotting$max_depth)
elections_ccdf_number_of_unique_users = create_normalized_ccdf(elections_graph_metrics_for_plotting$number_of_unique_users)
elections_ccdf_toxicity_ratio = create_normalized_ccdf(elections_graph_metrics_for_plotting$toxicity_ratio)

elections_ccdf_assortativity = create_normalized_ccdf(elections_graph_metrics_for_plotting$assortativity)
elections_ccdf_avg_toxicity_distance = create_normalized_ccdf(elections_graph_metrics_for_plotting$avg_toxicity_distance)
elections_ccdf_wiener_index = create_normalized_ccdf(elections_graph_metrics_for_plotting$wiener_index)

# Football
football_graph_metrics_for_plotting <-
  graph_metrics_for_plotting %>%
  filter(topic == "football")

football_ccdf_tree_size = create_normalized_ccdf(football_graph_metrics_for_plotting$tree_size)
football_ccdf_max_width = create_normalized_ccdf(football_graph_metrics_for_plotting$max_width)
football_ccdf_max_depth = create_normalized_ccdf(football_graph_metrics_for_plotting$max_depth)
football_ccdf_number_of_unique_users = create_normalized_ccdf(football_graph_metrics_for_plotting$number_of_unique_users)
football_ccdf_toxicity_ratio = create_normalized_ccdf(football_graph_metrics_for_plotting$toxicity_ratio)

football_ccdf_assortativity = create_normalized_ccdf(football_graph_metrics_for_plotting$assortativity)
football_ccdf_avg_toxicity_distance = create_normalized_ccdf(football_graph_metrics_for_plotting$avg_toxicity_distance)
football_ccdf_wiener_index = create_normalized_ccdf(football_graph_metrics_for_plotting$wiener_index)


df_assortativity = rbind(
  tibble(elections_ccdf_assortativity,
         topic = "Football"),
  tibble(football_ccdf_assortativity,
         topic = "Elections")
)
df_assortativity$metric = "Assortativity"

df_max_width = rbind(
  tibble(elections_ccdf_max_width,
         topic = "Football"),
  tibble(football_ccdf_max_width,
         topic = "Elections")
)
df_max_width$metric = "Max Width"

df_max_depth = rbind(
  tibble(elections_ccdf_max_depth,
         topic = "Football"),
  tibble(football_ccdf_max_depth,
         topic = "Elections")
)
df_max_depth$metric = "Max Depth"

df_avg_toxicity_distance = rbind(
  tibble(elections_ccdf_avg_toxicity_distance,
         topic = "Elections"),
  tibble(football_ccdf_avg_toxicity_distance,
         topic = "Football")
)
df_avg_toxicity_distance$metric = "Avg. Toxicity\nDistance"

df_wiener_index = rbind(
  tibble(elections_ccdf_wiener_index,
         topic = "Football"),
  tibble(football_ccdf_wiener_index,
         topic = "Elections")
)
df_wiener_index$metric = "Wiener Index"

df_tree_size = rbind(
  tibble(elections_ccdf_tree_size,
         topic = "Football"),
  tibble(football_ccdf_tree_size,
         topic = "Elections")
)
df_tree_size$metric = "Tree Size"

df_toxicity_ratio = rbind(
  tibble(elections_ccdf_toxicity_ratio,
         topic = "Elections"),
  tibble(football_ccdf_toxicity_ratio,
         topic = "Football")
)
df_toxicity_ratio$metric = "Toxicity Ratio"

df_number_of_unique_users = rbind(
  tibble(elections_ccdf_number_of_unique_users,
         topic = "Football"),
  tibble(football_ccdf_number_of_unique_users,
         topic = "Elections")
)
df_number_of_unique_users$metric = "N. Unique\nUsers"

df_twitter = rbind(
  df_assortativity,
  df_avg_toxicity_distance,
  df_max_depth,
  df_max_width,
  df_number_of_unique_users,
  df_toxicity_ratio,
  df_tree_size,
  df_wiener_index
)

# Plot ###
twitter_ccdf_plot <- ggplot(data = df_twitter) +
  geom_line(aes(x, y, color = topic),  alpha = 0.8) +
  geom_point(aes(x, y, color = topic)) +
  facet_wrap(~ metric,
             ncol = 8,
             scales = "free_x",
             shrink = T) +
  scale_x_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x, n = 4),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_color_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(
    y = "CCDF",
    x = "Metric Value",
    title = "Twitter",
    colour = "Topic"
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    strip.text.x = element_text(size = 13),
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines")
  )
twitter_ccdf_plot

# Saving Plot
ccdf_plot_filename = paste(figures_folder,
                           "twitter_overall_tree_ccdf_normalized.pdf", 
                           sep = "/")

ggsave(
  ccdf_plot_filename,
  twitter_ccdf_plot,
  width = 40,
  height = 20,
  units = "cm",
  device = "pdf"
)

# Compound plot ####
# Plot
df_twitter$social = "Twitter"
df_youtube$social = "YouTube"
df_total <- rbind(df_twitter, df_youtube)

compound_ccdf_plot <- ggplot(data = df_total) +
  geom_line(aes(x, y, color = topic),  alpha = 0.8) +
  geom_point(aes(x, y, color = topic)) +
  facet_grid(rows = vars(social),
             cols = vars(metric),
             scales = "free_x",
             shrink = T) +
  scale_x_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x, n = 4),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_y_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_color_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(
    y = "CCDF",
    x = "Metric Value",
    colour = "Topic"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    strip.text.x = element_text(size = 25),
    strip.text.y = element_text(size = 30),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines")
  )
compound_ccdf_plot

# Saving Plot
compound_ccdf_plot_filename = paste(figures_folder,
                           "overall_tree_ccdf_normalized.pdf", 
                           sep = "/")

ggsave(
  compound_ccdf_plot_filename,
  compound_ccdf_plot,
  width = 60,
  height = 20,
  units = "cm",
  device = "pdf"
)
