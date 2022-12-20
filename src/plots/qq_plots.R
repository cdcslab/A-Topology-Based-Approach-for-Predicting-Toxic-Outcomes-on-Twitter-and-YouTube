library(tibble)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)

rm(list = ls())
gc()

# Functions ####

normalize_distribution <- function(x)
{
  x = na.omit(x)
  min_x = min(x)
  max_x = max(x)
  
  x = (x - min_x) / (max_x - min_x)
  
  return(x)
}

# Variables ####
football_graph_metrics_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/shuffled_football_graph_metrics.csv"
football_graph_metrics = read.csv(football_graph_metrics_filename)
football_graph_metrics$topic = "Football"

elections_graph_metrics_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/shuffled_elections_graph_metrics.csv"
elections_graph_metrics = read.csv(elections_graph_metrics_filename)
elections_graph_metrics$topic = "Elections"

graph_metrics = rbind(football_graph_metrics, elections_graph_metrics)
graph_metrics$root = as.character(graph_metrics$root)

figures_folder = "./figures"

# Plot Structural Metrics ####

# Football

football_quantile_assortativity = tibble(quantile = quantile(
  (football_graph_metrics$assortativity),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_assortativity$topic = "Football"

football_quantile_tree_size = tibble(quantile = quantile(
  (football_graph_metrics$tree_size),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_tree_size$topic = "Football"

football_quantile_wiener_index = tibble(quantile = quantile(
  (football_graph_metrics$wiener_index),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_wiener_index$topic = "Football"

football_quantile_depth = tibble(quantile = quantile(
  (football_graph_metrics$depth),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_depth$topic = "Football"

football_quantile_avg_toxicity_distance = tibble(quantile = quantile(
  (football_graph_metrics$avg_toxicity_distance),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_avg_toxicity_distance$topic = "Football"

football_quantile_toxicity_ratio = tibble(quantile = quantile(
  (football_graph_metrics$toxicity_ratio),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_toxicity_ratio$topic = "Football"

df_lcc_size_percentage = football_graph_metrics %>%
  filter(tree_size_lcc >= 10)

football_quantile_lcc_size_percentage = tibble(quantile = quantile(
  (df_lcc_size_percentage$lcc_size_percentage),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_lcc_size_percentage$topic = "Football"

football_quantile_max_width = tibble(quantile = quantile(
  (football_graph_metrics$max_width),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
football_quantile_max_width$topic = "Football"

# Elections
elections_quantile_assortativity = tibble(quantile = quantile(
  (elections_graph_metrics$assortativity),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_assortativity$topic = "Elections"

elections_quantile_tree_size = tibble(quantile = quantile(
  (elections_graph_metrics$tree_size),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_tree_size$topic = "Elections"

elections_quantile_wiener_index = tibble(quantile = quantile(
  (elections_graph_metrics$wiener_index),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_wiener_index$topic = "Elections"

elections_quantile_depth = tibble(quantile = quantile(
  (elections_graph_metrics$depth),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_depth$topic = "Elections"

elections_quantile_avg_toxicity_distance = tibble(quantile = quantile(
  (elections_graph_metrics$avg_toxicity_distance),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_avg_toxicity_distance$topic = "Elections"

elections_quantile_toxicity_ratio = tibble(quantile = quantile(
  (elections_graph_metrics$toxicity_ratio),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_toxicity_ratio$topic = "Elections"

df_lcc_size_percentge = elections_graph_metrics %>%
  filter(tree_size_lcc >= 10)

elections_lcc_size_percentage = tibble(quantile = quantile(
  (elections_graph_metrics$lcc_size_percentage),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_lcc_size_percentage$topic = "Elections"

elections_quantile_max_width = tibble(quantile = quantile(
  (elections_graph_metrics$max_width),
  na.rm = T,
  probs = seq(0, 1, 0.01)
))
elections_quantile_max_width$topic = "Elections"

plot_cascade_metrics_colors <- c("Football" = "#264653",
                                 "Elections" = "#e76f51")

# QQ Plot Assortativity ####
quantile_assortativity = tibble(quantile_football = football_quantile_assortativity$quantile,
                                quantile_elections = elections_quantile_assortativity$quantile)

qq_plot_assortativity = ggplot(quantile_assortativity) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  labs(x = "Football",
       y = "Elections",
       title = "Assortativity",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_assortativity

# QQ Plot Tree Size ####
quantile_tree_size = tibble(quantile_football = football_quantile_tree_size$quantile,
                            quantile_elections = elections_quantile_tree_size$quantile)

qq_plot_tree_size = ggplot(quantile_tree_size) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +   
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "Tree Size",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_tree_size

# QQ Plot Depth ####
quantile_depth = tibble(quantile_football = football_quantile_depth$quantile,
                        quantile_elections = elections_quantile_depth$quantile)

qq_plot_depth = ggplot(quantile_depth) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                              labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "Max Depth",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_depth

# QQ Plot Avg. Toxicity Diistance ####
quantile_avg_toxicity_distance = tibble(quantile_football = football_quantile_avg_toxicity_distance$quantile,
                                        quantile_elections = elections_quantile_avg_toxicity_distance$quantile)

qq_plot_avg_toxicity_distance = ggplot(quantile_avg_toxicity_distance) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +   
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "Avg Toxicity\nDistance",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_avg_toxicity_distance

# QQ Plot Width ####
quantile_max_width = tibble(quantile_football = football_quantile_max_width$quantile,
                            quantile_elections = elections_quantile_max_width$quantile)

qq_plot_max_width = ggplot(quantile_max_width) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +   
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "Max Width",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_max_width

# QQ LCC Size Percentage ####
quantile_lcc_size_percentage = tibble(quantile_football = football_quantile_lcc_size_percentage$quantile,
                                      quantile_elections = elections_lcc_size_percentage$quantile)

qq_plot_lcc_size_percentage = ggplot(quantile_lcc_size_percentage) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +   
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "LCC Size\nPercentage",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_lcc_size_percentage

# QQ Plot LCC Wiener Index ####
quantile_wiener_index = tibble(quantile_football = football_quantile_wiener_index$quantile,
                               quantile_elections = elections_quantile_wiener_index$quantile)

qq_plot_wiener_index = ggplot(quantile_wiener_index) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "Wiener Index",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_wiener_index

# QQ Plot Toxicity Ratio ####
quantile_toxicity_ratio = tibble(quantile_football = football_quantile_toxicity_ratio$quantile,
                                 quantile_elections = elections_quantile_toxicity_ratio$quantile)

qq_plot_toxicity_ratio = ggplot(quantile_toxicity_ratio) +
  geom_point(aes(x = quantile_football,
                 y = quantile_elections)) +
  geom_abline(intercept = 0) +   
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(x = "Football",
       y = "Elections",
       title = "Toxicity\nRatio",) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
qq_plot_toxicity_ratio

# QQ Plot Compounds ####

plot_compound_structural_metrics_1 = qq_plot_tree_size + qq_plot_assortativity + qq_plot_depth + qq_plot_wiener_index +
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "top",
        plot.margin = margin(0.8, 0.5, 1, 1, "cm"))

plot_compound_structural_metrics_1
plot_compound_structural_metrics_filename = paste(figures_folder,
                                                  "qq_plot_compound_structural_metrics_1.pdf",
                                                  sep = "/")

ggsave(
  plot_compound_structural_metrics_filename,
  plot_compound_structural_metrics_1,
  width = 40,
  height = 15,
  units = "cm",
  device = "pdf"
)

plot_compound_structural_metrics_2 = qq_plot_max_width + qq_plot_avg_toxicity_distance + qq_plot_toxicity_ratio + qq_plot_lcc_size_percentage +
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "top",
        plot.margin = margin(0.8, 0.5, 1, 1, "cm"))

plot_compound_structural_metrics_2

plot_compound_structural_metrics_filename = paste(figures_folder,
                                                  "qq_plot_compound_normalized_structural_metrics_2.pdf",
                                                  sep = "/")

ggsave(
  plot_compound_structural_metrics_filename,
  plot_compound_structural_metrics_2,
  width = 40,
  height = 15,
  units = "cm",
  device = "pdf"
)
