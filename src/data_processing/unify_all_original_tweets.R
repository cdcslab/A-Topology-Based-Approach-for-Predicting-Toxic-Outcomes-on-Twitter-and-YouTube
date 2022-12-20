library(dplyr)
library(tibble)

original_tweets_folder = 
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/original_tweets"

original_tweet_files = list.files(original_folder,
                                  full.names = T)

total_df = tibble()
for(file in original_tweet_files)
{
  cat("Working with", file, "\n")
  file_df = read.csv(file)
  total_df = plyr::rbind.fill(total_df, file_df)
}

total_df_cleaned = total_df %>% 
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == T &
           is.na(replied_id) == T &
           is.na(in_reply_to_user_id) == T) %>% 
  select(-c(X.1, X, Unnamed..0)) %>% 
  distinct(id, .keep_all = T)

total_df_cleaned$created_at = lubridate::as_date(total_df_cleaned$created_at)

output_filename = 
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/original_tweets/tweets_unified.csv"

write.csv(total_df_cleaned, output_filename)

# Sanity checks ###
library(ggplot2)
library(scales)
time_series_df = total_df_cleaned %>% 
  group_by(created_at) %>% 
  summarise(n_posts = n())

options(scipen = 999)
ggplot(time_series_df, aes(x = created_at, y = n_posts)) + 
  geom_point() + 
  geom_line() + 
  
  scale_y_log10(
    breaks = trans_breaks("log10", function(x)
      10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  ylim(0, NA) + 
  scale_x_date(date_breaks = "1 week", date_labels = "%b %Y") +
  theme_bw() + 
  ggtitle("Election Posts by Month")

  