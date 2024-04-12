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
  "football/twitter/football_twitter_data_scored.parquet",
  "elections/twitter/elections_twitter_data_scored.parquet",
  "football/youtube/youtube_football_entire_dataset.parquet",
  "elections/youtube/youtube_elections_entire_dataset.parquet",
  "figures"
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

# Load data ####
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

df_average_daily_toxicity_score_by_topic <- rbind(df_twitter,
                    df_youtube)

df_average_daily_toxicity_score_by_topic$created_at = lubridate::as_date(df_average_daily_toxicity_score_by_topic$created_at)


df_average_daily_toxicity_score_by_topic$topic = ifelse(is.na(df_average_daily_toxicity_score_by_topic$topic),
                          "elections",
                          df_average_daily_toxicity_score_by_topic$topic)
df_average_daily_toxicity_score_by_topic <- 
  df_average_daily_toxicity_score_by_topic %>% 
  group_by(created_at,
           social,
           topic) %>% 
  summarise(toxicity_score = mean(toxicity_score, na.rm = T))

# Testing statistical differences ####
sample_size <- 50
df_average_daily_toxicity_score_by_topic <- df_average_daily_toxicity_score_by_topic %>% 
  filter(created_at >= "2022-08-25") %>% 
  filter(created_at <= "2022-12-25") %>% 
  ungroup()
# Twitter

toxicity_twitter_pvalues = c()

for(i in 1:100)
{
  print(i)
  
  sample_elections_toxicity_twitter <- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "elections" & 
             social == "Twitter") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  sample_football_toxicity_twitter <- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "football" & 
             social == "Twitter") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  p_value <- ks.test(
    sample_elections_toxicity_twitter$toxicity_score,
    sample_football_toxicity_twitter$toxicity_score,
    na.action = na.omit()
  )$p.value
  
  toxicity_twitter_pvalues <- append(toxicity_twitter_pvalues, p_value)
}

hist(toxicity_twitter_pvalues)

# Youtube 

toxicity_youtube_pvalues = c()

for(i in 1:100)
{
  print(i)
  
  sample_elections_toxicity_youtube <- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "elections" & 
             social == "YouTube") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  sample_football_toxicity_youtube <- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "football" & 
             social == "YouTube") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  p_value <- ks.test(
    sample_elections_toxicity_youtube$toxicity_score,
    sample_football_toxicity_youtube$toxicity_score,
    na.action = na.omit()
  )$p.value
  
  toxicity_youtube_pvalues <- append(toxicity_youtube_pvalues, p_value)
}

hist(toxicity_youtube_pvalues)

# Elections Both Platforms 

toxicity_elections_pvalues = c()

for(i in 1:100)
{
  print(i)
  
  sample_elections_toxicity_youtube <- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "elections" & 
             social == "YouTube") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  sample_elections_toxicity_twitter<- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "elections" & 
             social == "Twitter") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  p_value <- ks.test(
    sample_elections_toxicity_youtube$toxicity_score,
    sample_elections_toxicity_twitter$toxicity_score,
    na.action = na.omit(),
    exact = T
  )$p.value
  
  toxicity_elections_pvalues <- append(toxicity_elections_pvalues, p_value)
}

hist(toxicity_elections_pvalues)

# Football Both Platforms 

toxicity_football_pvalues = c()

for(i in 1:100)
{
  print(i)
  
  sample_football_toxicity_youtube <- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "football" & 
             social == "YouTube") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  sample_football_toxicity_twitter<- df_average_daily_toxicity_score_by_topic %>% 
    filter(topic == "elections" & 
             social == "Twitter") %>% 
    sample_n(sample_size) %>% 
    select(toxicity_score) %>% 
    drop_na() 
  
  p_value <- ks.test(
    sample_football_toxicity_youtube$toxicity_score,
    sample_football_toxicity_twitter$toxicity_score,
    na.action = na.omit(),
    exact = T
  )$p.value
  
  toxicity_football_pvalues <- append(toxicity_football_pvalues, p_value)
}

hist(toxicity_football_pvalues)
