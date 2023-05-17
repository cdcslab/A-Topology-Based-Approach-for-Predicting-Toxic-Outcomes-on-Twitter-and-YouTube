library(ggpubr)
library(ggplot2)
library(dplyr)
library(patchwork)
rm(list = ls())

n = 20


# Loading ####
filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/youtube_overall_tree_data/graph_metrics.csv"
df_metrics <- read.csv(filename)
df_metrics$topic <- ifelse(df_metrics$topic == "football", "Football", "Elections")
topic_colors <- c("Football" = "#ff7b00",
                  "Elections" = "#003f88")

# Create theme ####
theme_metrics <- function() {
  theme_bw() %+replace%
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 24),
      legend.title = element_text(size = 25),
      legend.text = element_text(size = 20),
      plot.title = element_text(size = 40),
      plot.subtitle = element_text(size = 32),
      panel.spacing = unit(2, "lines"),
      aspect.ratio = 1
    )
}

# Toxicity Ratio vs Tree Size ####
min = min(df_metrics$tree_size) # 2
max = max(df_metrics$tree_size) # 3974

df_metrics$tree_size_bin = rep(NA)

log_breaks = seq(log10(10 ^ 0), log10(10 ^ 4), length.out = n)

for (i in 1:dim(df_metrics)[1])
{
  value = log10(df_metrics$tree_size[i])
  result = which(abs(log_breaks - value) == min(abs(log_breaks - value)))
  
  df_metrics$tree_size_bin[i] = log_breaks[result]
}

df_metrics$tree_size_bin = 10 ^ (df_metrics$tree_size_bin)
df_metrics_to_plot <- df_metrics %>%
  filter(is.na(toxicity_ratio) == F) %>%
  group_by(tree_size_bin, topic) %>%
  summarise(average_toxicity_ratio = mean(toxicity_ratio))


# Plot
plot_tree_size = ggplot(data = df_metrics_to_plot,
                            aes(
                              x = tree_size_bin,
                              y = average_toxicity_ratio,
                              color = topic,
                              fill = topic
                            )) +
  geom_line(show.legend = F) +
  geom_point(size = 2, show.legend = F) +
  scale_color_manual(values = topic_colors) +
  xscale("log10", .format = T) +
  labs(x = "Tree Size (log)",
       y = "Avg. Toxicity Ratio",
       color = "Topic",
       fill = "Topic") +
  theme_metrics()
plot_tree_size

# Average Toxicity Ratio vs Max Depth ####
min = min(df_metrics$max_depth) # 1
max = max(df_metrics$max_depth) # 5

log_breaks = seq(log10(10 ^ 0), log10(10 ^ 1), length.out = n)
df_metrics$depth_bin = NA

for (i in 1:dim(df_metrics)[1])
{
  value = log10(df_metrics$max_depth[i])
  result = which(abs(log_breaks - value) == min(abs(log_breaks - value)))
  
  df_metrics$depth_bin[i] = log_breaks[result]
}

df_metrics$depth_bin = 10 ^ (df_metrics$depth_bin)
df_metrics_to_plot <- df_metrics %>%
  filter(is.na(toxicity_ratio) == F) %>%
  group_by(depth_bin, topic) %>%
  summarise(average_toxicity_ratio = mean(toxicity_ratio))

# Plot Max Depth
plot_depth = ggplot(data = df_metrics_to_plot,
                    aes(
                      x = depth_bin,
                      y = average_toxicity_ratio,
                      color = topic,
                      fill = topic
                    )) +
  geom_line(show.legend = F) +
  geom_point(size = 2, show.legend = F) +
  scale_color_manual(values = topic_colors) +
  xscale("log10", .format = T) +
  labs(x = "Depth (log)",
       y = NULL,
       color = "Topic",
       fill = "Topic") +
  theme_metrics()
plot_depth

# Prepare data for Max Width ####
min = min(df_metrics$max_width) # 1
max = max(df_metrics$max_width) # 6437

log_breaks = seq(log10(10 ^ 0), log10(10 ^ 4), length.out = n)
df_metrics$width_bin = NA

for (i in 1:dim(df_metrics)[1])
{
  value = log10(df_metrics$max_width[i])
  result = which(abs(log_breaks - value) == min(abs(log_breaks - value)))
  
  df_metrics$width_bin[i] = log_breaks[result]
}

df_metrics$width_bin = 10 ^ (df_metrics$width_bin)
df_metrics_to_plot <- df_metrics %>%
  filter(is.na(toxicity_ratio) == F) %>%
  group_by(width_bin, topic) %>%
  summarise(average_toxicity_ratio = mean(toxicity_ratio))


# Plot Width
plot_width = ggplot(data = df_metrics_to_plot,
                    aes(
                      x = width_bin,
                      y = average_toxicity_ratio,
                      color = topic,
                      fill = topic
                    )) +
  geom_line(show.legend = F) +
  geom_point(size = 2, show.legend = F) +
  scale_color_manual(values = topic_colors) +
  xscale("log10", .format = T) +
  labs(x = "Width (log)",
       y = NULL,
       color = "Topic",
       fill = "Topic") +
  theme_metrics()
plot_width

# Prepare data for Wiener Index ####
min = min(df_metrics$wiener_index) # 1
max = max(df_metrics$wiener_index) # 2.33

log_breaks = seq(log10(10 ^ 0), log10(10 ^ 1), length.out = n)
df_metrics$wiener_index_bin = NA

for (i in 1:dim(df_metrics)[1])
{
  value = log10(df_metrics$wiener_index[i])
  result = which(abs(log_breaks - value) == min(abs(log_breaks - value)))
  
  df_metrics$wiener_index_bin[i] = log_breaks[result]
}

df_metrics$wiener_index_bin = 10 ^ (df_metrics$wiener_index_bin)
df_metrics_to_plot <- df_metrics %>%
  filter(is.na(toxicity_ratio) == F) %>%
  group_by(wiener_index_bin, topic) %>%
  summarise(average_toxicity_ratio = mean(toxicity_ratio))

plot_wiener_index = ggplot(data = df_metrics_to_plot,
                           aes(
                             x = wiener_index_bin,
                             y = average_toxicity_ratio,
                             color = topic,
                             fill = topic
                           )) +
  geom_line(show.legend = F) +
  geom_point(size = 2, show.legend = T) +
  scale_color_manual(values = topic_colors) +
  xscale("log10", .format = T) +
  labs(x = "Wiener\nIndex (log)",
       y = NULL,
       color = "Topic",
       fill = "Topic") +
  theme_metrics()
plot_wiener_index

# Compound Plot
plot_compound_metrics_vs_toxicity =
  plot_tree_size +
  plot_depth + 
  plot_width +
  plot_wiener_index +
  plot_annotation(title = 'Toxicity Ratio vs Metrics',
                  subtitle = 'n = 20',
                  theme = theme(plot.title = element_text(size = 18,
                                                          hjust = 0.5),
                                plot.subtitle = element_text(hjust = .5))) +
  plot_layout(guides = 'collect', ncol = 4)

plot_compound_metrics_vs_toxicity

ggsave(
  "./figures/youtube_toxicity_ratio_vs_metrics.pdf",
  plot_compound_metrics_vs_toxicity,
  device = "pdf",
  units = "cm",
  width = 40,
  height = 20
)
