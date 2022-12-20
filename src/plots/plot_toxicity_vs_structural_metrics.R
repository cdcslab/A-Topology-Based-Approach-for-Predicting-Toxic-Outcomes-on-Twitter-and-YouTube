library(tibble)
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)

rm(list = ls())
gc()


# Functions ####
create_ccdf <- function(x) {
  ecdf_vector = ecdf(x)
  ccdf_vector <- 1 - ecdf_vector(sort(unique(x)))
  ccdf <- cbind.data.frame(x = sort(unique(x)), y = ccdf_vector)
  return(ccdf)
}

# Variables ####
football_graph_metrics_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/football/football_graph_metrics.csv"
football_graph_metrics = read.csv(football_graph_metrics_filename)
football_graph_metrics$topic = "Football"

elections_graph_metrics_filename = "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/elections_graph_metrics.csv"
elections_graph_metrics = read.csv(elections_graph_metrics_filename)
elections_graph_metrics$topic = "Elections"

graph_metrics = rbind(football_graph_metrics, elections_graph_metrics)
graph_metrics$root = as.character(graph_metrics$root)

figures_folder = "./figures"

# Plot Structural Metrics ####
football_ccdf_assortativity = create_ccdf(football_graph_metrics$assortativity)
football_ccdf_tree_size = create_ccdf(football_graph_metrics$tree_size_lcc)

football_graph_metrics$wiener_index = football_graph_metrics$wiener_index / 2
football_ccdf_wiener_index = create_ccdf(football_graph_metrics$wiener_index)
football_ccdf_eccentricity = create_ccdf(football_graph_metrics$eccentrity_from_root)

elections_ccdf_assortativity = create_ccdf(elections_graph_metrics$assortativity)
elections_ccdf_tree_size = create_ccdf(elections_graph_metrics$tree_size_lcc)
elections_ccdf_wiener_index = create_ccdf(elections_graph_metrics$wiener_index)
elections_ccdf_eccentricity = create_ccdf(elections_graph_metrics$eccentrity_from_root)

plot_cascade_metrics_colors <- c("football" = "#264653",
                                 "elections" = "#e76f51")

# toxicity vs Structural Metrics ####
n_breaks = 10
football_distributions = football_graph_metrics %>%
  mutate(points_toxicity = cut(toxicity_ratio, breaks = seq(
    from = 0,
    to = max(football_graph_metrics$toxicity_ratio),
    by = max(football_graph_metrics$toxicity_ratio) / n_breaks
  )))

elections_distributions = elections_graph_metrics %>%
  mutate(points_toxicity = cut(toxicity_ratio, breaks = seq(
    from = 0,
    to = max(football_graph_metrics$toxicity_ratio),
    by = max(football_graph_metrics$toxicity_ratio) / n_breaks
  )))

# Plot Toxicity vs Tree Size ####
plot_football_toxicity_vs_tree_size = ggplot(data = football_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = tree_size, color = "football"), show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Toxicity Ratio",
    y = "Tree Size",
    color = "Topic",
    title = "",
    subtitle = ""
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )

plot_football_toxicity_vs_tree_size

plot_elections_toxicity_vs_tree_size = ggplot(data = elections_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = tree_size, color = "elections"), show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Toxicity Ratio",
    y = "Tree Size",
    color = "Topic",
    title = "",
    subtitle = ""
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )

plot_elections_toxicity_vs_tree_size

# Plot Toxicit Wiener Index ####
plot_football_toxicity_vs_wiener_index = ggplot(data = football_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = wiener_index, color = "football"), show.legend = F) +
  labs(
    x = "Toxicity Ratio",
    y = "Wiener Index",
    color = "Topic",
    title = "",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )

plot_football_toxicity_vs_wiener_index

plot_elections_toxicity_vs_wiener_index = ggplot(data = elections_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = wiener_index, color = "elections"), show.legend = F) +
  labs(
    x = "Toxicity Ratio",
    y = "Wiener Index",
    color = "Topic",
    title = "",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )

plot_elections_toxicity_vs_wiener_index

# Plot toxicity vs depth ####
plot_football_toxicity_vs_depth = ggplot(data = football_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = eccentrity_from_root, color = "football"), show.legend = F) +
  labs(
    x = "Toxicity Ratio",
    y = "Depth",
    color = "Topic",
    title = "Football",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )

plot_football_toxicity_vs_depth

plot_elections_toxicity_vs_depth = ggplot(data = elections_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = eccentrity_from_root, color = "elections"), show.legend = F) +
  labs(
    x = "Toxicity Ratio",
    y = "Depth",
    color = "Topic",
    title = "Elections",
    subtitle = ""
  ) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )

plot_elections_toxicity_vs_depth

# Toxicity vs Structural Virality ####
pdf_football_toxicity_vs_assortativity = ggplot(data = football_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = assortativity, color = "football"), show.legend = F) +
  labs(
    x = "Toxicity Ratio",
    y = "Assortativity",
    color = "Topic",
    title = "",
    subtitle = ""
  ) +
  #scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
pdf_football_toxicity_vs_assortativity

pdf_elections_toxicity_vs_assortativity = ggplot(data = elections_distributions, aes(x = points_toxicity)) +
  geom_boxplot(aes(y = assortativity, color = "elections"), show.legend = F) +
  labs(
    x = "Toxicity Ratio",
    y = "Assortativity",
    color = "Topic",
    title = "",
    subtitle = ""
  ) +
  # scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 22, angle = 90),
    axis.text.x = element_text(vjust = 0.5),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    aspect.ratio = 1
  )
pdf_elections_toxicity_vs_assortativity
