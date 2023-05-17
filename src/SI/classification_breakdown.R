library(arrow)
library(dplyr)
df <- read_parquet("/media/gabett/Volume/data-repository/panconesi-football-elections/football/twitter/football_twitter_data_scored.parquet")

count_labelled <- as.data.frame(df) %>% filter(is.na(toxicity_score) == FALSE) %>% count() %>% pull()
label_percentage <- count_labelled / dim(df)[1]
label_percentage

count_toxic <- as.data.frame(df) %>% filter(toxicity_score >= 0.6) %>% count() %>% pull()
toxic_percentage <- count_toxic / count_labelled
toxic_percentage

# Twitter Elections: 92.5%, 8035937 over 8685730, 3.4% 279303
# Twitter Football:  98.6%, 3142173 over 3184593, 2%, 63588
# YouTube Football: 99.8%, 1295421 over 1296837, 2.4% toxic, 27799
# YouTube Elections: 99.6%, 1389115 over 1393369, 5.2% toxic, 72395
