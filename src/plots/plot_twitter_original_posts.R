library(dplyr)
library(ggplot2)
library(scales)
library(arrow)

rm(list = ls())
gc()
Sys.setlocale("LC_TIME", "en_GB.UTF-8")
figures_folder <- "/home/gabett/Documents/repository/football-elections-cascade-comparison/figures/"

# Load Twitter Data ####
elections_df_filename <- paste("./elections/twitter",
                         "elections_twitter_data_scored.parquet",
                         sep = "/")
football_df_filename <- paste("./football/twitter",
                               "football_twitter_data_scored.parquet",
                               sep = "/")

df_elections <- read_parquet(elections_df_filename)
df_elections <- df_elections %>%
  distinct(conversation_id, .keep_all = T) %>% 
  filter(created_at <= "2022-10-25") %>% 
  filter(is.na(in_reply_to_user_id))

df_football <- read_parquet(football_df_filename)
df_football <- df_football %>% 
  distinct(conversation_id, .keep_all = T) %>% 
  filter(created_at <= "2022-10-25") %>% 
  filter(is.na(in_reply_to_user_id))

df_elections$created_at <- lubridate::as_date(df_elections$created_at)
df_football$created_at <- lubridate::as_date(df_football$created_at)

df_football_posts_by_day <- df_football %>%
  group_by(created_at) %>%
  summarise(posts_per_day = n()) %>%
  mutate(topic = "Football",
         social = "Twitter")

df_elections_posts_by_day <- df_elections %>%
  group_by(created_at) %>%
  summarise(posts_per_day = n()) %>%
  mutate(topic = "Elections",
         social = "Twitter")

df_twitter_posts_by_day = rbind(df_football_posts_by_day,
                        df_elections_posts_by_day)

df_posts_by_day <- rbind(df_twitter_posts_by_day)

# Plot Time Series ####
plot_posts_per_day <-
  ggplot(df_posts_by_day,
         aes(x = created_at,
             y = posts_per_day,
             color = topic)) +
  geom_line() +
  geom_point() +
  #geom_smooth(method = "lm") + 
  geom_vline(xintercept =  lubridate::as_date("2022-09-26"),
             linetype = "dashed",
             col = "orange") +
  labs(x = "Time",
       y = "N. Posts per Day",
       color = "Topic",
       title = "") + 
  xlim(as.Date("2022-08-25"),
       NA) +
  scale_color_manual(values = c("Football" = "#ff7b00",
                                                "Elections" = "#003f88")) +
  facet_wrap(~social) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 25),
    axis.title = element_text(size = 35),
    legend.title = element_text(size = 35, vjust = 1),
    legend.text = element_text(size = 22),
    strip.text.x = element_text(size = 25),
    legend.position = 'right',
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    aspect.ratio = 1
  )

plot_posts_per_day

figure_filename <- paste(figures_folder,
                         "plot_twitter_original_posts.pdf")

ggsave(figure_filename,
       plot_posts_per_day,
       device = "pdf",
       height = 30,
       width = 50,
       unit = "cm")
