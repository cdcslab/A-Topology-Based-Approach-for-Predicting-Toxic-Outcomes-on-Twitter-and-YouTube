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
  "twitter",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter/football_twitter_data_scored.parquet",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/twitter/elections_twitter_data_scored.parquet",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/football/youtube/youtube_football_entire_dataset.parquet",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/youtube/youtube_elections_entire_dataset.parquet",
  "/home/gabett/Documents/repository/football-elections-cascade-comparison/figures"
)

if (length(args) < 5)
{
  stop(
    "5 arguments are required:"
  )
}

social <- args[1]
input_twitter_football <- args[2]
input_twitter_elections <- args[3]
input_youtube_football <- args[4]
input_youtube_elections <- args[5]
figures_folder <- args[6]

# General variables
topic_colors <- c("Football" = "#ff7b00",
                  "Elections" = "#003f88")

# Read data and prepare columns
df_twitter_football <- read_parquet(input_twitter_football)
df_twitter_elections <- read_parquet(input_twitter_elections)
df_youtube_football <- read_parquet(input_youtube_football)

df_youtube_elections <- read_parquet(input_youtube_elections)

df_twitter <- plyr::rbind.fill(df_twitter_football,
                               df_twitter_elections)
df_twitter$social = "Twitter"

df_youtube <- plyr::rbind.fill(df_youtube_football,
                               df_youtube_elections)
df_youtube$social = "YouTube"

rm(df_twitter_football,
   df_twitter_elections,
   df_youtube_football,
   df_youtube_elections)

gc()

# Prepare data for plotting ####

df_twitter <- df_twitter %>% 
  distinct(id, 
           .keep_all = T) %>% 
  select(created_at,
         toxicity_score,
         social,
         topic)

df_youtube <- df_youtube %>% 
  distinct(comment_id, 
           .keep_all = T) %>% 
  select(created_at = created_at_comment,
         toxicity_score,
         social,
         topic)

df_to_plot <- rbind(df_twitter,
                    df_youtube)

df_to_plot$created_at = lubridate::as_date(df_to_plot$created_at)


df_to_plot$topic = ifelse(is.na(df_to_plot$topic),
                          "elections",
                          df_to_plot$topic)

df_to_plot$topic = ifelse(df_to_plot$topic == "elections",
                           "Elections",
                           "Football")

# Distribution of Toxicity Score ####
plot_distribution_toxicity_score <-
  ggplot(df_to_plot,
         aes(x = toxicity_score,
             fill = topic)) +
  geom_histogram(alpha = 0.7,
                 position = "identity",
                 color = "black",
                 bins = 50,
                 show.legend = T) +
  labs(x = "Toxicity Score",
       y = "N. of elements",
       fill = "Topic",
       title = "") + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                 labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = topic_colors) +
  scale_fill_manual(values = topic_colors) +
  facet_wrap(~social) +
  theme_bw() +
  xlim(0, 1) + 
  theme(
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 35),
    legend.title = element_text(size = 35, vjust = 1),
    legend.text = element_text(size = 22),
    strip.text.x = element_text(size = 25),
    legend.position = 'right',
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    aspect.ratio = 1,
    panel.spacing = unit(2, "lines")
  )

plot_distribution_toxicity_score

figure_filename <- paste(figures_folder,
                         "toxicity_score_distribution.pdf")

ggsave(figure_filename,
       plot_distribution_toxicity_score,
       device = "pdf",
       height = 30,
       width = 50,
       unit = "cm")

