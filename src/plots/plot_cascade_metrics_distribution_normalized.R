library(ggplot2)
library(dplyr)
library(arrow)
library(scales)
library(tidyr)
library(stringr)
library(patchwork)

"
Script that plots the distribution of the final cascade metrics of each conversation,
divided by topic.
"


# Setup
set.seed(42)

# Reset
rm(list = ls())
gc()

# Function ####
normalize_feature_if_numeric <- function(x)
{
  if(is.numeric(x))
  {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
  }
  else
  {
    x
  }

}

# External Arguments
args <- commandArgs(trailingOnly = TRUE)
args <- c("youtube",
          "original",
          "overall_tree_data",
          "figures/distributions")

if (length(args) < 4)
{
  
  stop(
    "4 arguments are required:\n\tSocial\n\ttype of the graphs (original or shuffled)\n\tInput Folder\n\tOutput Folder"
  )
}

social <- args[1]
shuffled_label <- ifelse(args[2] == "original", "", "shuffled")
input_folder <- args[3] 
output_folder <- args[4]

# General variables
input_filename <-
  paste(
    input_folder,
    ifelse(
      shuffled_label == "",
      paste(social, "overall_graph_metrics.parquet", sep = "_"),
      paste(social, shuffled, "overall_graph_metrics.parquet", sep = "_")
    ),
    sep = "/"
  )

topic_colors <- c("Football" = "#ff7b00",
                  "Elections" = "#003f88")

# Read data and prepare columns
df_metrics <- as.data.frame(read_parquet(input_filename)) %>%
  select(
    tree_size,
    max_width,
    max_depth,
    number_of_unique_users,
    toxicity_ratio,
    assortativity,
    avg_toxicity_distance,
    topic,
    wiener_index
  )

# Normalizing between 0 and 1
df_metrics <- sapply(df_metrics, normalize_feature_if_numeric)
df_metrics <- as.data.frame(df_metrics)
df_metrics <- df_metrics %>% 
  mutate_at(vars(-"topic"), as.numeric)

df_metrics$assortativity = as.numeric(df_metrics$assortativity)

df_football <- df_metrics %>%
  filter(topic == "football")

df_elections <- df_metrics %>%
  filter(topic == "elections")

# Compute Metrics
df_football_metrics <- df_football %>%
  pivot_longer(!topic,
               names_to = "metric",
               values_to = "value")

df_elections_metrics <- df_elections %>%
  pivot_longer(!topic,
               names_to = "metric",
               values_to = "value")


df_metrics <- rbind(df_football_metrics,
                    df_elections_metrics)

rm(
  df_elections_metrics,
  df_football_metrics,
  input_filename,
  df_elections,
  df_football
)

gc()

# Prepare data for plotting ####
if(social == "youtube")
{
  df_metrics <- df_metrics %>%
    filter(metric != "max_depth" &
             metric != "wiener_index")
}

# Prettify labels
df_metrics$topic <- str_to_title(df_metrics$topic)
df_metrics$metric <-
  str_replace_all(df_metrics$metric, "_", " ")
df_metrics$metric <- str_to_title(df_metrics$metric)

df_metrics$metric = ifelse(df_metrics$metric == "Number Of Unique Users",
                           "N. Of Unique Users",
                           df_metrics$metric)

plot_binnable_metrics <- df_metrics %>%
  filter(metric %in% c("Max Width", "N. Of Unique Users", "Tree Size", "Max Depth")) %>%
  ggplot(aes(x = value,
             fill = topic)) +
  geom_histogram(alpha = 0.7,
                 position = "identity",
                 color = "black",
                 bins = 40,
                 show.legend = F) +
  facet_wrap(~ metric,
             scales = "free",
             nrow = 1,
             ncol = 4) +
  scale_y_continuous(
    expand = c(0, NA),
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x)),
    limits = c(NA, 10000)
  ) +
  scale_color_manual(values = topic_colors) +
  scale_fill_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(y = "Count",
       x = "",
       title = paste("Overall Distribution of\nnormalized cascade metrics on", str_to_title(social)),
       fill = "Topic") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    strip.text.x = element_text(size = 20),
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines")
  )
plot_binnable_metrics

plot_non_binnable_metrics <- df_metrics %>%
  filter(!metric %in% c("Max Width", "N. Of Unique Users", "Tree Size", "Max Depth")) %>%
  ggplot(aes(x = value,
             fill = topic)) +
  geom_histogram(alpha = 0.7,
                 position = "identity",
                 color = "black",
                 bins = 40) +
  facet_wrap(~ metric,
             scales = "free",
             nrow = 1,
             ncol = 5) +
  scale_y_continuous(
    expand = c(0, NA),
    trans = log10_trans(),
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x)),
    limits = c(NA, 10000)
  ) +
  scale_color_manual(values = topic_colors) +
  scale_fill_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(y = "Count",
       x = "Metric Value",
       title = "",
       fill = "Topic") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 15),
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
plot_non_binnable_metrics

plot_metrics <- (plot_binnable_metrics + plot_non_binnable_metrics) + 
  plot_layout(heights = c(1,1,0,1))
plot_metrics

pdf_filename <- ifelse(
  shuffled_label == "",
  paste("plot", social, "overall_graph_metrics_normalized.pdf", sep = "_"),
  paste("plot", social, shuffled_label, "overall_graph_metrics_normalized.pdf", sep = "_")
)
output_filename <- paste(output_folder,
                         ifelse(shuffled_label == "",
                                "unshuffled",
                                "shuffled"),
                         pdf_filename,
                         sep = "/")

ggsave(output_filename,
       plot_metrics,
       width = 40,
       height = 40,
       units = "cm",
       device = "pdf")
