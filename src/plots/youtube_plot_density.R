library(tibble)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)
library(tidyr)
library(arrow)

rm(list = ls())
gc()

is_toxicity_shuffling_enabled = T

# Variables ####
graph_metrics <- read_parquet(
  "/media/gabett/Volume/data-repository/panconesi-football-elections/overall_tree_data/youtube_graph_metrics.csv"
)

graph_metrics <- as.data.frame(graph_metrics)
figures_folder = "./figures/distributions/unshuffled"

topic_colors <- c("Football" = "#ff7b00",
                  "Elections" = "#003f88")

# Prepare Metrics ####
graph_metrics$topic <- ifelse(graph_metrics$topic == "football", "Football", "Elections")

graph_metrics_for_plotting <- graph_metrics %>%
  select(-c(
    children_index,
    created_at,
    id,
    video_id,
    parent_id,
    root,
    social,
    toxicity_score,
    assortativity
  )) %>% 
  rename(
         "Tree Size" = tree_size,
         "Max Width" = max_width,
         "Max Depth" = max_depth,
         "N. Unique Users" = number_of_unique_users,
         "Toxicity Ratio" = toxicity_ratio,
         "Wiener Index" = wiener_index,
         "Avg. Toxicity Distance" = avg_toxicity_distance
         ) %>% 
  pivot_longer(!topic, 
               names_to = "metric",
               values_to = "value",
               values_drop_na = T)

# Plot ###
density_plot <- ggplot(data = graph_metrics_for_plotting) +
  geom_density(aes(value, fill = topic), color = "black", alpha = 0.8) +
  facet_wrap(~ metric,
             ncol = 4,
             scales = "free",
             shrink = T) +
  scale_x_continuous(
    trans = log(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x, n =  4),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_y_continuous(
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^.x))
  ) +
  scale_fill_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(
    y = "Count",
    x = "Metric Value",
    title = "YouTube Tree Metrics Distributions",
    fill = "Topic"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
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
density_plot

# Saving Plot ####
plot_filename = paste(figures_folder,
                           "youtube_overall_tree_densities.pdf", 
                           sep = "/")

ggsave(
  plot_filename,
  density_plot,
  width = 40,
  height = 20,
  units = "cm",
  device = "pdf"
)
