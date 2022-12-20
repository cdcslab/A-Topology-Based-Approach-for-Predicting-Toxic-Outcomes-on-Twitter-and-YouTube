"
We need to count:

- The number of posts
- The number of posts with at least x comments
- The number of users
- The number of total comments
- The number of total comments with a label != NA

Plus we need to produce the timelines for both posts/comments.

"
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)
library(logr)
library(sassy)

rm(list = ls())
gc()


log_filename <- "./logs/data_breakdown_log"
report_filename <- "./logs/twitter_data_breakdown_report.pdf"

lf <- log_open(
  log_filename,
  logdir = F,
  autolog = F,
  show_notes = T
)
df_stats = tibble()

# Elections ####
elections_tweets_folder <-
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/original_tweets"
elections_comments_filename <-
  "/media/gabett/Volume/data-repository/panconesi-football-elections/elections/comments/comments_conversations-folder_unified.RData"

elections_tweets_filenames <- list.files(elections_tweets_folder,
                                         full.names = T)

df_tweets <- tibble()
for (f in elections_tweets_filenames)
{
  print(f)
  df_tmp <- read.csv(f)
  
  df_tweets <- rbind.fill(df_tweets,
                          df_tmp)
}

rm(df_tmp)
gc()

df_tweets_eligibile <- df_tweets %>%
  distinct(id, .keep_all = T) %>%
  mutate(created_at = as_date(created_at)) %>%
  filter(created_at >= "2022-08-25" &
           created_at <= "2022-10-25")

rm(df_tweets)

df_tweets_eligibile$replied_id = ifelse(df_tweets_eligibile$replied_id == "",
                                        NA,
                                        df_tweets_eligibile$replied_id)
df_tweets_eligibile$quoted_id = ifelse(df_tweets_eligibile$quoted_id == "",
                                       NA,
                                       df_tweets_eligibile$quoted_id)
df_tweets_eligibile$retweeted_id = ifelse(df_tweets_eligibile$retweeted_id == "",
                                          NA,
                                          df_tweets_eligibile$retweeted_id)

df_tweets_eligibile$author_id = as.character(df_tweets_eligibile$author_id)

elections_n_posts = df_tweets_eligibile %>%
  count()

elections_n_original_posts = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == T &
           is.na(replied_id) == T) %>%
  count()

elections_n_retweets = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == F &
           is.na(quoted_id) == T &
           is.na(replied_id) == T) %>%
  count()

elections_n_quotes = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == F &
           is.na(replied_id) == T) %>%
  count()

elections_n_replies = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == T &
           is.na(replied_id) == F) %>%
  count()

elections_n_users = df_tweets_eligibile %>%
  distinct(author_id) %>%
  count()

df_tweets_eligibile$like_count = as.numeric(df_tweets_eligibile$like_count)
df_tweets_eligibile$reply_count = as.numeric(df_tweets_eligibile$reply_count)
df_tweets_eligibile$quote_count = as.numeric(df_tweets_eligibile$quote_count)
df_tweets_eligibile$retweet_count = as.numeric(df_tweets_eligibile$retweet_count)

elections_n_total_likes = sum(df_tweets_eligibile$like_count, na.rm = T)
elections_n_total_replies = sum(df_tweets_eligibile$reply_count, na.rm = T)
elections_n_total_quotes = sum(df_tweets_eligibile$quote_count, na.rm = T)
elections_n_total_retweets = sum(df_tweets_eligibile$retweet_count, na.rm = T)

load(elections_comments_filename)

elections_n_conversation_ids = total_comments_df %>%
  distinct(conversation_id) %>%
  count()

elections_n_users_commenting = total_comments_df %>%
  distinct(author_id) %>%
  count()

df_stats_elections = tibble(
  n_posts = elections_n_posts$n,
  n_unique_users_posting = elections_n_users$n,
  n_original_posts = elections_n_original_posts$n,
  n_retweets = elections_n_retweets$n,
  n_quotes = elections_n_quotes$n,
  n_replies = elections_n_replies$n,
  n_collected_likes = elections_n_total_likes,
  n_collected_replies = elections_n_total_replies,
  n_collected_quotes = elections_n_total_quotes,
  n_collected_retweets = elections_n_total_retweets,
  n_conversation_ids = elections_n_conversation_ids$n,
  n_unique_users_commenting = elections_n_users_commenting$n
)

rownames(df_stats_elections) = "Elections"

df_stats = rbind(df_stats,
                 df_stats_elections)

rm(
  df_tweets_eligibile,
  elections_comments_filename,
  elections_n_conversation_ids,
  elections_n_original_posts,
  elections_n_posts,
  elections_n_quotes,
  elections_n_replies,
  elections_n_retweets,
  elections_n_total_likes,
  elections_n_total_quotes,
  elections_n_total_replies,
  elections_n_total_retweets,
  elections_others,
  elections_n_others,
  n_posts,
  elections_n_users,
  elections_n_users_commenting,
  elections_tweets_filenames,
  elections_tweets_folder,
  total_comments_df,
  df_stats_elections
)

gc()

# Football ####
football_tweets_filename <-
  "/media/gabett/Volume/data-repository/panconesi-football-elections/football/footbal_twitter_timeline.csv"
football_comments_filename <-
  "/media/gabett/Volume/data-repository/panconesi-football-elections/football/comments_unified/comments_conversations-folder_unified.RData"

df_tweets_eligibile <- read.csv(
  football_tweets_filename,
  colClasses = c(
    "id" = "character",
    "author_id" = "character",
    "conversation_id" = "character"
  )
) %>%
  distinct(id, .keep_all = T) %>%
  mutate(created_at = as_date(created_at))

df_tweets_eligibile$replied_id = ifelse(df_tweets_eligibile$replied_id == "",
                                        NA,
                                        df_tweets_eligibile$replied_id)
df_tweets_eligibile$quoted_id = ifelse(df_tweets_eligibile$quoted_id == "",
                                       NA,
                                       df_tweets_eligibile$quoted_id)
df_tweets_eligibile$retweeted_id = ifelse(df_tweets_eligibile$retweeted_id == "",
                                          NA,
                                          df_tweets_eligibile$retweeted_id)

df_tweets_eligibile$author_id = as.character(df_tweets_eligibile$author_id)

football_n_posts = df_tweets_eligibile %>%
  count()

football_n_original_posts = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == T &
           is.na(replied_id) == T) %>%
  count()

football_n_retweets = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == F &
           is.na(quoted_id) == T &
           is.na(replied_id) == T) %>%
  count()

football_n_quotes = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == F &
           is.na(replied_id) == T) %>%
  count()

football_n_replies = df_tweets_eligibile %>%
  filter(is.na(retweeted_id) == T &
           is.na(quoted_id) == T &
           is.na(replied_id) == F) %>%
  count()

football_n_users = df_tweets_eligibile %>%
  distinct(author_id) %>%
  count()

df_tweets_eligibile$like_count = as.numeric(df_tweets_eligibile$like_count)
df_tweets_eligibile$reply_count = as.numeric(df_tweets_eligibile$reply_count)
df_tweets_eligibile$quote_count = as.numeric(df_tweets_eligibile$quote_count)
df_tweets_eligibile$retweet_count = as.numeric(df_tweets_eligibile$retweet_count)

football_n_total_likes = sum(df_tweets_eligibile$like_count, na.rm = T)
football_n_total_replies = sum(df_tweets_eligibile$reply_count, na.rm = T)
football_n_total_quotes = sum(df_tweets_eligibile$quote_count, na.rm = T)
football_n_total_retweets = sum(df_tweets_eligibile$retweet_count, na.rm = T)

load(football_comments_filename)

football_n_conversation_ids = total_comments_df %>%
  distinct(conversation_id) %>%
  count()

football_n_users_commenting = total_comments_df %>%
  distinct(author_id) %>%
  count()

df_stats_football = tibble(
  n_posts = football_n_posts$n,
  n_unique_users_posting = football_n_users$n,
  n_original_posts = football_n_original_posts$n,
  n_retweets = football_n_retweets$n,
  n_quotes = football_n_quotes$n,
  n_replies = football_n_replies$n,
  n_collected_likes = football_n_total_likes,
  n_collected_replies = football_n_total_replies,
  n_collected_quotes = football_n_total_quotes,
  n_collected_retweets = football_n_total_retweets,
  n_conversation_ids = football_n_conversation_ids$n,
  n_unique_users_commenting = football_n_users_commenting$n
)

rownames(df_stats_football) = "Football"

df_stats = rbind(df_stats,
                 df_stats_football)

rm(
  df_tweets_eligibile,
  football_comments_filename,
  football_n_conversation_ids,
  football_n_original_posts,
  football_n_posts,
  football_n_quotes,
  football_n_replies,
  football_n_retweets,
  football_n_total_likes,
  football_n_total_quotes,
  football_n_total_replies,
  football_n_total_retweets,
  football_others,
  football_n_others,
  n_posts,
  football_n_users,
  football_n_users_commenting,
  football_tweets_filename,
  total_comments_df,
  df_stats_football
)

gc()

# Printing log data ####
log_print("Twitter Dataset composition:")
df_stats %>% 
  put()

tbl <- create_table(df_stats)

# Create report
pdf <- create_report(report_filename, 
                     output_type = "PDF", 
                     font = "Arial",
                     paper_size = "A4") %>% 
  titles("Twitter Football-Elections Data Breakdown", 
         bold = TRUE) %>% 
  add_content(tbl)

# write out the report
res <- write_report(pdf)

# Close log
log_close()

# View results
writeLines(readLines(lf))
