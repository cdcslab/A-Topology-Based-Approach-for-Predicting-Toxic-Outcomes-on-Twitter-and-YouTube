library(arrow)
library(dplyr)
df <- read_parquet("football/twitter/football_twitter_data_scored.parquet")

count_labelled <- as.data.frame(df) %>% filter(is.na(toxicity_score) == FALSE) %>% count() %>% pull()
label_percentage <- count_labelled / dim(df)[1]
label_percentage

count_toxic <- as.data.frame(df) %>% filter(toxicity_score >= 0.6) %>% count() %>% pull()
toxic_percentage <- count_toxic / count_labelled
toxic_percentage
