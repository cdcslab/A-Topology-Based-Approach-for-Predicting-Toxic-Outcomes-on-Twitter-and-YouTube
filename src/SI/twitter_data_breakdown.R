# R libraries and reset, echo=FALSE, warning=FALSE
library(arrow)
library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

rm(list = ls())
gc()

# Football
tp = "football"

## Global Variables
folder_filename <- paste("./", tp, "twitter", sep ="/")

## Load Data
input_filename <- paste(folder_filename, 
                        "football_twitter_data_with_in_reply_to_id_filled.RData",
                        sep = "/")
load(input_filename)

df_data_football <- df
rm(df)

## Statistics
football_number_of_original_tweets <- df_data_football %>% 
  filter(is.na(in_reply_to_id) == T) %>% 
  count() %>% 
  pull()
   
football_number_of_comments <- df_data_football %>%
  filter(is.na(in_reply_to_id) == F) %>% 
    count() %>% 
    pull()

football_number_of_users_posting <- df_data_football %>%
  filter(is.na(in_reply_to_id) == T) %>% 
  distinct(author_id) %>% 
  count() %>% 
  pull()

football_number_of_users_commenting <- df_data_football %>% 
  filter(is.na(in_reply_to_id) == F) %>% 
  distinct(author_id) %>% 
  count() %>% 
  pull()

football_number_of_users_conversations_at_least_10_comments <- df_data_football %>% 
  group_by(conversation_id) %>% 
  summarise(n_comments = n()) %>% 
  filter(n_comments >= 10) %>% 
  count() %>% 
  pull()


# Elections
tp = "elections"

## Global Variables
folder_filename <- paste("./", tp, "twitter", sep ="/")

## Load Data
input_filename <- paste(folder_filename, 
                        "elections_twitter_data_with_in_reply_to_id_filled.RData",
                        sep = "/")
load(input_filename)

df_data_elections <- df
rm(df)

## Statistics
elections_number_of_original_tweets <- df_data_elections %>% 
  filter(is.na(in_reply_to_id) == T) %>% 
  count() %>% 
  pull()

elections_number_of_comments <- df_data_elections %>%
  filter(is.na(in_reply_to_id) == F) %>% 
  count() %>% 
  pull()

elections_number_of_users_posting <- df_data_elections %>%
  filter(is.na(in_reply_to_id) == T) %>% 
  distinct(author_id) %>% 
  count() %>% 
  pull()

elections_number_of_users_commenting <- df_data_elections %>% 
  filter(is.na(in_reply_to_id) == F) %>% 
  distinct(author_id) %>% 
  count() %>% 
  pull()

elections_number_of_users_conversations_at_least_10_comments <- df_data_elections %>% 
  group_by(conversation_id) %>% 
  summarise(n_comments = n()) %>% 
  filter(n_comments >= 10) %>% 
  count() %>% 
  pull()
