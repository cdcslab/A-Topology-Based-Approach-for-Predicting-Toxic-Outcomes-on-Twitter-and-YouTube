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

# External Arguments
args <- commandArgs(trailingOnly = TRUE)
args <- c(
  "youtube",
  "original",
  "./overall_tree_data",
  "./figures/distributions"
)

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
  paste(input_folder,
        ifelse(
          shuffled_label == "",
          paste(social, "overall_graph_metrics.parquet", sep = "_"),
          paste(social, shuffled, "overall_graph_metrics.parquet", sep = "_")
        ),
        sep = "/")

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

df_metrics$assortativity = as.numeric(df_metrics$assortativity)

gc()

# Prepare data for plotting ####
if (social == "youtube")
{
  df_metrics <- df_metrics %>%
    select(-c(max_depth,
              wiener_index))
}

# Prettify labels
df_metrics$topic <- str_to_title(df_metrics$topic)
names(df_metrics) <-
  str_replace_all(names(df_metrics), "_", " ")
names(df_metrics) <- str_to_title(names(df_metrics))
names(df_metrics) = ifelse(names(df_metrics) == "Number Of Unique Users",
                           "N. Of Unique Users",
                           names(df_metrics))
# Create Plots ####

# List of variables to plot

var_list <- c("Max Width",
              "N. Of Unique Users",
              "Assortativity",
              "Toxicity Ratio",
              "Avg Toxicity Distance")
plot_list = list()

for (i in seq(1, length(var_list))) {
  print(var_list[i])
  plot <- df_metrics %>%
    ggplot(aes(x = `Tree Size`,
               y = .data[[var_list[i]]],
               color = Topic)) +
    stat_summary_bin(
      fun = mean,
      size = 2,
      bins = 4,
      geom = "point",
      aes(color = Topic)
    ) +
    stat_summary_bin(fun = mean,
                     bins = 4,
                     geom = "line",
                     aes(color = Topic)) +
    scale_x_continuous(
      trans = log10_trans(),
      breaks = trans_breaks("log10", function(x)
        10 ^ x),
      labels = trans_format("log10", math_format(10 ^ .x))
    ) +
    scale_color_manual(values = topic_colors) +
    theme(aspect.ratio = 1) +
    labs(y = var_list[i],
         x = "Tree Size",
         color = "Topic") +
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
  plot
  plot_list[[i]] <- plot
}

compound_plots <- plot_list[[1]]

for (i in 2:length(plot_list))
{
  compound_plots <- compound_plots + plot_list[[i]]
}

compound_plots <-
  compound_plots +
  plot_layout(guides = "collect",
              nrow = 1,
              ncol = 5) +
  theme(plot.title = element_text(30, vjust = 2))


compound_plots

pdf_filename <- ifelse(
  shuffled_label == "",
  paste(
    "plot",
    social,
    "overall_graph_metrics_by_tree_size.pdf",
    sep = "_"
  ),
  paste(
    "plot",
    social,
    shuffled_label,
    "overall_graph_metrics_by_tree_size.pdf",
    sep = "_"
  )
)
output_filename <- paste(
  output_folder,
  ifelse(shuffled_label == "",
         "unshuffled",
         "shuffled"),
  pdf_filename,
  sep = "/"
)

ggsave(
  output_filename,
  compound_plots,
  width = 50,
  height = 15,
  units = "cm",
  device = "pdf"
)

