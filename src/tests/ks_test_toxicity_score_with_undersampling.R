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
  "/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter/football_twitter_data_scored.parquet",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/twitter/elections_twitter_data_scored.parquet",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/football/youtube/youtube_football_entire_dataset.parquet",
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/youtube/youtube_elections_entire_dataset.parquet",
)

if (length(args) < 4)
{
  stop(
    "5 arguments are required:"
  )
}

input_twitter_football <- args[1]
input_twitter_elections <- args[2]
input_youtube_football <- args[3]
input_youtube_elections <- args[4]

# Read data and prepare columns
df_twitter_football <- read_parquet(input_twitter_football)
df_twitter_elections <- read_parquet(input_twitter_elections)
df_youtube_football <- read_parquet(input_youtube_football)
df_youtube_elections <- read_parquet(input_youtube_elections)

# Without undersampling ####

# KS test
ks_p_value_twitter_both_topics <- ks.test(df_twitter_football$toxicity_score,
                                       df_twitter_elections$toxicity_score,
        na.action = na.omit())$p.value
ks_p_value_twitter_both_topics

ks_p_value_youtube_both_topics <- ks.test(df_youtube_football$toxicity_score,
                                       df_youtube_elections$toxicity_score,
                                       na.action = na.omit())$p.value
ks_p_value_youtube_both_topics

ks_p_football_cross_social <- ks.test(df_twitter_football$toxicity_score,
                                       df_youtube_football$toxicity_score,
                                       na.action = na.omit())$p.value
ks_p_football_cross_social

ks_p_elections_cross_social <- ks.test(df_twitter_elections$toxicity_score,
                                       df_youtube_elections$toxicity_score,
                                       na.action = na.omit())$p.value
ks_p_elections_cross_social

# Mann Withney
mw_p_value_twitter_both_topics <- wilcox.test(df_twitter_football$toxicity_score,
                                       df_twitter_elections$toxicity_score,
                                       na.rm=TRUE,
                                       paired = FALSE)$p.value
mw_p_value_twitter_both_topics

mw_p_value_youtube_both_topics <- wilcox.test(df_youtube_football$toxicity_score,
                                       df_youtube_elections$toxicity_score,
                                       na.rm=TRUE,
                                       paired = FALSE)$p.value
mw_p_value_youtube_both_topics

mw_p_football_cross_social <- wilcox.test(df_twitter_football$toxicity_score,
                                   df_youtube_football$toxicity_score,
                                   na.rm=TRUE,
                                   paired = FALSE)$p.value
mw_p_football_cross_social

mw_p_elections_cross_social <- wilcox.test(df_twitter_elections$toxicity_score,
                                    df_youtube_elections$toxicity_score,
                                    na.rm=TRUE,
                                    paired = FALSE)$p.value
mw_p_elections_cross_social


# KS test
ks_p_value_twitter_both_topics <- ks.test(df_twitter_football$toxicity_score,
                                          df_twitter_elections$toxicity_score,
                                          na.action = na.omit())$p.value
ks_p_value_twitter_both_topics

ks_p_value_youtube_both_topics <- ks.test(df_youtube_football$toxicity_score,
                                          df_youtube_elections$toxicity_score,
                                          na.action = na.omit())$p.value
ks_p_value_youtube_both_topics

ks_p_football_cross_social <- ks.test(df_twitter_football$toxicity_score,
                                      df_youtube_football$toxicity_score,
                                      na.action = na.omit())$p.value
ks_p_football_cross_social

ks_p_elections_cross_social <- ks.test(df_twitter_elections$toxicity_score,
                                       df_youtube_elections$toxicity_score,
                                       na.action = na.omit())$p.value
ks_p_elections_cross_social

# With Undersampling ####
# Mann Withney
# Twitter
min_topic <- which.min(c(dim(df_twitter_football)[1], dim(df_twitter_elections)[1]))
min_size <- min(c(dim(df_twitter_football)[1], dim(df_twitter_elections)[1]))

if(min_topic == 1) # Twitter
{
  resampled_df_twitter_elections <- df_twitter_elections %>% 
    sample_n(min_size)
  resampled_df_twitter_football <- df_twitter_football
} else { # Elections
  resampled_df_twitter_football <- df_twitter_football %>% 
    sample_n(min_size)
  resampled_df_twitter_elections <- df_twitter_elections
}

ks_resampled_p_value_twitter_both_topics <- ks.test(resampled_df_twitter_football$toxicity_score,
                                                    resampled_df_twitter_elections$toxicity_score,
                                          na.action = na.omit())$p.value
ks_resampled_p_value_twitter_both_topics

mw_resampled_p_value_twitter_both_topics <- wilcox.test(resampled_df_twitter_football$toxicity_score,
                                                        resampled_df_twitter_elections$toxicity_score,
                                              na.rm=TRUE,
                                              paired = FALSE)$p.value
mw_resampled_p_value_twitter_both_topics

# Youtube
min_topic <- which.min(c(dim(df_youtube_football)[1], dim(df_youtube_elections)[1]))
min_size <- min(c(dim(df_youtube_football)[1], dim(df_youtube_elections)[1]))

if(min_topic == 1) # Football
{
  resampled_df_youtube_elections <- df_youtube_elections %>% 
    sample_n(min_size)
  resampled_df_youtube_football <- df_youtube_football
} else { # Elections
  resampled_df_youtube_football <- df_youtube_football %>% 
    sample_n(min_size)
  resampled_df_youtube_elections <- df_youtube_elections
}

ks_resampled_p_value_youtube_both_topics <- ks.test(resampled_df_youtube_football$toxicity_score,
                                                    resampled_df_youtube_elections$toxicity_score,
                                                    na.action = na.omit())$p.value
ks_resampled_p_value_youtube_both_topics

mw_resampled_p_value_youtube_both_topics <- wilcox.test(resampled_df_youtube_football$toxicity_score,
                                                        resampled_df_youtube_elections$toxicity_score,
                                                        na.rm=TRUE,
                                                        paired = FALSE)$p.value
mw_resampled_p_value_youtube_both_topics

# Football
min_topic <- which.min(c(dim(df_youtube_football)[1], dim(df_twitter_football)[1]))
min_size <- min(c(dim(df_youtube_football)[1], dim(df_twitter_football)[1]))

if(min_topic == 1) # Youtube
{
  resampled_df_twitter_football <- df_twitter_football %>% 
    sample_n(min_size)
  resampled_df_youtube_football <- df_youtube_football
} else { # Twitter
  resampled_df_youtube_football <- df_youtube_football %>% 
    sample_n(min_size)
  resampled_df_df_twitter_football <- df_twitter_football
}

ks_resampled_p_value_football <- ks.test(resampled_df_youtube_football$toxicity_score,
                                                    resampled_df_twitter_football$toxicity_score,
                                                    na.action = na.omit())$p.value
ks_resampled_p_value_football

mw_resampled_p_value_football <- wilcox.test(resampled_df_youtube_football$toxicity_score,
                                                        resampled_df_twitter_football$toxicity_score,
                                                        na.rm=TRUE,
                                                        paired = FALSE)$p.value
mw_resampled_p_value_football

# Elections
min_topic <- which.min(c(dim(df_youtube_elections)[1], dim(df_twitter_elections)[1]))
min_size <- min(c(dim(df_youtube_elections)[1], dim(df_twitter_elections)[1]))

if(min_topic == 1) # Youtube
{
  resampled_df_twitter_elections <- df_twitter_elections %>% 
    sample_n(min_size)
  resampled_df_youtube_elections <- df_youtube_elections
} else { # Twitter
  resampled_df_youtube_elections <- df_youtube_elections %>% 
    sample_n(min_size)
  resampled_df_df_twitter_elections <- df_twitter_elections
}

ks_resampled_p_value_elections <- ks.test(resampled_df_youtube_elections$toxicity_score,
                                         resampled_df_twitter_elections$toxicity_score,
                                         na.action = na.omit())$p.value
ks_resampled_p_value_elections

mw_resampled_p_value_elections <- wilcox.test(resampled_df_youtube_elections$toxicity_score,
                                             resampled_df_twitter_elections$toxicity_score,
                                             na.rm=TRUE,
                                             paired = FALSE)$p.value
mw_resampled_p_value_elections
