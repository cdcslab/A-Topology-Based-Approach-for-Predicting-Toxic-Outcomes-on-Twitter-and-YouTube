library(ggplot2)
library(dplyr)
library(arrow)
library(scales)
library(tidyr)
library(stringr)
library(patchwork)

Sys.setlocale("LC_ALL","en_GB.UTF-8")

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

df_to_plot <- df_to_plot %>% 
  filter(created_at >= "2022-08-25") %>% 
  filter(created_at <= "2022-12-25")

df_to_plot$created_at = lubridate::as_date(df_to_plot$created_at)


df_to_plot$topic = ifelse(is.na(df_to_plot$topic),
                          "elections",
                          df_to_plot$topic)
df_to_plot <- 
  df_to_plot %>% 
  group_by(created_at,
           social,
           topic) %>% 
  summarise(toxicity_score = mean(toxicity_score, na.rm = T))

df_to_plot$topic = ifelse(df_to_plot$topic == "elections",
                           "Elections",
                           "Football")

# Plot Time Series ####
plot_toxicity_score_per_day <-
  ggplot(df_to_plot,
         aes(x = as.Date(created_at),
             y = toxicity_score,
             color = topic)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_vline(xintercept = as.Date("2022-09-25"),
             linetype = "dashed",
             col = "red") +
  labs(x = "Time",
       y = "Avg. Daily Toxicity Score",
       color = "Topic",
       title = "") + 
  xlim(as.Date("2022-08-25"), 
       as.Date("2022-12-25")) +
  ylim(0, 0.3) + 
  # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = topic_colors) +
  facet_wrap(~social) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 30),
    axis.title = element_text(size = 40),
    legend.title = element_text(size = 40, vjust = 1),
    legend.text = element_text(size = 30),
    strip.text.x = element_text(size = 40),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    aspect.ratio = 1,
    legend.position = "bottom"
  ) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

plot_toxicity_score_per_day

figure_filename <- paste(figures_folder,
                         "plot_toxicity_score_evolution.pdf",
                         sep = "/")

ggsave(figure_filename,
       plot_toxicity_score_per_day,
       device = "pdf",
       height = 30,
       width = 50,
       unit = "cm")

# Regressions ####

plot_fit_data <- ggplot_build(plot_toxicity_score_per_day)$data[[2]]

twitter_elections <- df_to_plot %>% 
  filter(topic == "Elections" &
           social == "Twitter") %>% 
  ungroup() %>% 
  select(toxicity_score,
         created_at)

fit_twitter_elections <- lm(toxicity_score ~ created_at, twitter_elections)
summary(fit_twitter_elections)

twitter_football <- df_to_plot %>% 
  filter(topic == "Football" &
           social == "Twitter") %>% 
  ungroup() %>% 
  select(toxicity_score,
         created_at)


fit_twitter_football <- lm(toxicity_score ~ created_at, twitter_football)
summary(fit_twitter_football)

youtube_elections <- df_to_plot %>% 
  filter(topic == "Elections" &
           social == "YouTube") %>% 
  ungroup() %>% 
  select(toxicity_score,
         created_at)


fit_youtube_elections <- lm(toxicity_score ~ created_at, youtube_elections)
summary(fit_youtube_elections)
youtube_football <- df_to_plot %>% 
  filter(topic == "Football" &
           social == "YouTube") %>% 
  ungroup() %>% 
  select(toxicity_score,
         created_at)
  
fit_youtube_football <- lm(toxicity_score ~ created_at, youtube_football)
summary(fit_youtube_football)
