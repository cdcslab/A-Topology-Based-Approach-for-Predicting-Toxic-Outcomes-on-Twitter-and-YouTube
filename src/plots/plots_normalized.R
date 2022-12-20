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

create_normalized_ccdf <- function(x) {
  x = na.omit(x)
  max_x = max(x)
  min_x = min(x)
  
  if (max_x - min_x == 0)
    stop("Denominator is equal to 0.")
  
  x = (x - min_x) / (max_x - min_x)
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
football_ccdf_tree_size = create_normalized_ccdf(football_graph_metrics$tree_size_lcc)
football_ccdf_wiener_index = create_normalized_ccdf(football_graph_metrics$wiener_index)
football_ccdf_depth = create_normalized_ccdf(football_graph_metrics$depth)
football_ccdf_avg_toxicity_distance = create_normalized_ccdf(football_graph_metrics$avg_toxicity_distance)
football_ccdf_toxicity_ratio = create_ccdf(football_graph_metrics$toxicity_ratio)

df_lcc_size_percentge = football_graph_metrics %>% 
  filter(tree_size_lcc >= 10)
football_lcc_size_percentage = create_ccdf(df_lcc_size_percentge$lcc_size_percentage)

football_ccdf_max_width = create_normalized_ccdf(football_graph_metrics$max_width)

elections_ccdf_assortativity = create_ccdf(elections_graph_metrics$assortativity)
elections_ccdf_tree_size = create_normalized_ccdf(elections_graph_metrics$tree_size_lcc)
elections_ccdf_wiener_index = create_normalized_ccdf(elections_graph_metrics$wiener_index)
elections_ccdf_depth = create_normalized_ccdf(elections_graph_metrics$depth)
elections_ccdf_avg_toxicity_distance = create_normalized_ccdf(elections_graph_metrics$avg_toxicity_distance)
elections_ccdf_toxicity_ratio = create_ccdf(elections_graph_metrics$toxicity_ratio)

df_lcc_size_percentge = elections_graph_metrics %>% 
  filter(tree_size_lcc >= 10)
elections_lcc_size_percentage = create_ccdf(df_lcc_size_percentge$lcc_size_percentage)
elections_ccdf_max_width = create_normalized_ccdf(elections_graph_metrics$max_width)

plot_cascade_metrics_colors <- c("football" = "#264653",
                                 "elections" = "#e76f51")

# Plot Assortativity ####
plot_assortativity = ggplot() +
  geom_line(data = elections_ccdf_assortativity,
            aes(x, y, color = "elections"),
            show.legend = F) +
  geom_line(data = football_ccdf_assortativity,
            aes(x, y, color = "football"),
            show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 1)) +
  #scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Assortativity",
    y = "",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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

plot_assortativity


# Plot Depth ####
plot_depth = ggplot() +
  geom_line(data = elections_ccdf_depth,
            aes(x, y, color = "elections"),
            show.legend = F) +
  geom_line(data = football_ccdf_depth,
            aes(x, y, color = "football"),
            show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  #scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  #scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Depth",
    y = "",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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
plot_depth

# Plot Wiener Index ####
plot_wiener_index = ggplot() +
  geom_line(data = elections_ccdf_wiener_index,
            aes(x, y, color = "elections"),
            show.legend = ) +
  geom_line(data = football_ccdf_wiener_index,
            aes(x, y, color = "football"),
            show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  #scale_x_continuous(limits = c(0, 4), breaks = seq(0, 4, 1)) +
  #scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Wiener Index",
    y = "",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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

plot_wiener_index

# Plot Tree Size ####
plot_tree_size = ggplot() +
  geom_line(data = elections_ccdf_tree_size,
            aes(x, y, color = "elections"),
            show.legend = F) +
  geom_line(data = football_ccdf_tree_size,
            aes(x, y, color = "football"),
            show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Tree Size",
    y = "CCDF",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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
plot_tree_size

# Plot Toxicity Ratio
plot_toxicity_ratio = ggplot() +
  geom_line(
    data = elections_ccdf_toxicity_ratio,
    aes(x, y,
        color = "football"),
    alpha = 1,
    show.legend = F
  ) +
  geom_line(
    data = football_ccdf_toxicity_ratio,
    aes(x, y,
        color = "elections"),
    show.legend = F,
    alpha = 1
  ) +
  
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  #scale_y_continuous(labels = c(10^1, 10^1.5, 10^2), breaks = c(10^1, 10^1.5, 10^2)) +
  scale_x_continuous(labels = c(0, 0.50, 1), breaks = c(0, 0.50, 1)) +
  # scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Toxicity Ratio",
    y = "CCDF",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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
plot_toxicity_ratio

# Plot LCC Size ###
plot_lcc_size_percentage = ggplot() +
  geom_histogram(
    data = elections_graph_metrics,
    aes(x = lcc_size_percentage,
        fill = "football"),
    alpha = 1,
    show.legend = F
  ) +
  geom_histogram(
    data = football_graph_metrics,
    aes(x = lcc_size_percentage,
        fill = "elections"),
    show.legend = F,
    alpha = 1
  ) +
  
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  # scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "LCC Size\nPercentage",
    y = "Number of Trees",
    color = "Topic",
    title = "",
  ) +
  scale_fill_manual(values = plot_cascade_metrics_colors) +
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
plot_lcc_size_percentage

# Plot Average Toxicity Distance
plot_avg_toxicity_distance = ggplot() +
  geom_line(data = elections_ccdf_avg_toxicity_distance,
            aes(x, y, color = "elections"),
            show.legend = F) +
  geom_line(data = football_ccdf_avg_toxicity_distance,
            aes(x, y, color = "football"),
            show.legend = T) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  #scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Avg Toxicity\nDistance",
    y = "CCDF",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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
plot_avg_toxicity_distance

# Width ####
plot_width = ggplot() +
  geom_line(data = elections_ccdf_max_width ,
            aes(x, y, color = "elections"),
            show.legend = F) +
  geom_line(data = football_ccdf_max_width,
            aes(x, y, color = "football"),
            show.legend = F) +
  scale_y_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  #scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2)) +
  scale_x_log10(labels = trans_format("log10", math_format(10 ^ .x))) +
  labs(
    x = "Max Tree Width",
    y = "CCDF",
    color = "Topic",
    title = "",
  ) +
  scale_color_manual(values = plot_cascade_metrics_colors) +
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
plot_width

# Merging ####
plot_compound_structural_metrics_1 = plot_tree_size + plot_assortativity + plot_depth + plot_wiener_index +
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "top",
        plot.margin = margin(0.8, 0.5, 1, 1, "cm"))

plot_compound_structural_metrics_1
plot_compound_structural_metrics_filename = paste(figures_folder,
                                                  "plot_compound_normalized_structural_metrics_1.pdf",
                                                  sep = "/")

ggsave(
  plot_compound_structural_metrics_filename,
  plot_compound_structural_metrics_1,
  width = 40,
  height = 15,
  units = "cm",
  device = "pdf"
)

plot_compound_structural_metrics_2 = plot_width + plot_avg_toxicity_distance + plot_toxicity_ratio + plot_lcc_size_percentage +
  plot_layout(ncol = 4, guides = "collect") &
  theme(legend.position = "top",
        plot.margin = margin(0.8, 0.5, 1, 1, "cm"))

plot_compound_structural_metrics_2
plot_compound_structural_metrics_filename = paste(figures_folder,
                                                  "plot_compound_normalized_structural_metrics_2.pdf",
                                                  sep = "/")

ggsave(
  plot_compound_structural_metrics_filename,
  plot_compound_structural_metrics_2,
  width = 40,
  height = 15,
  units = "cm",
  device = "pdf"
)
