library(arrow)
library(dplyr)
library(arrow)
library(ggplot2)
library(stringr)
library(tidyr)

set.seed(42)

# Reset ####
rm(list = ls())
gc()

# Variables ####
folder_filename <-
  "/media/gabett/Volume/data-repository/panconesi-football-elections/ML/feature_importance/"
filenames <- list.files(folder_filename, full.names = T)

figures_folder <-
  "/home/gabett/Documents/repository/football-elections-cascade-comparison/figures/ML/"
figure_output_filename <- paste(figures_folder,
                                "plot_feature_importance.pdf",
                                sep = "/")


# Load Data ####
df_feature_importances <- tibble()
for (file in filenames)
{
  print(file)
  df_tmp <- read.csv(file) %>%
    select(-X)
  
  df_tmp <- df_tmp %>%
    pivot_longer(is_root_toxic:toxicity_ratio,
                 names_to = "metric",
                 values_to = "value")
  
  df_feature_importances <-
    plyr::rbind.fill(df_feature_importances, df_tmp)
}

df_feature_importances$social = ifelse(df_feature_importances$social == "twitter",
                                       "Twitter",
                                       "YouTube")

df_feature_importances$metric <-
  str_replace_all(df_feature_importances$metric, "_", " ")
df_feature_importances$metric <-
  str_to_title(df_feature_importances$metric)

df_feature_importances$metric = ifelse(
  df_feature_importances$metric == "Number Of Unique Users",
  "N. Of Unique Users",
  df_feature_importances$metric
)

df_feature_importances$interval <- ifelse(df_feature_importances$interval == "overall",
                                          "Overall",
                                          df_feature_importances$interval)

df_feature_importances$interval <- factor(df_feature_importances$interval, levels=c( "Bin [10, 100]", 
                                                                                     "Bin [100, 1000]", 
                                                                                     "Bin [1000, 10000]", 
                                                                                     "Overall"))

min_max_feature <- df_feature_importances %>% 
  filter(metric == "Toxicity Ratio") %>% 
  group_by(social) %>% 
  summarise(min = median(value),
            max = median(value))
# Plot ####
topic_colors <- c("Twitter" = "#1DA1F2",
                  "YouTube" = "#FF0000")

plot_feature_importance <- ggplot(df_feature_importances,
       aes(x = metric,
           y = value,
           fill = social)) +
  geom_boxplot(position = position_dodge(1)) +
  geom_jitter(
    color = "black",
    size = 0.4,
    alpha = 0.9,
    position = position_dodge(1)
  ) +
  coord_flip() +
  facet_wrap( ~ interval) +
  scale_fill_manual(values = topic_colors) +
  theme(aspect.ratio = 1) +
  labs(x = "Metric",
       y = "F1 Decrease",
       title = "",
       fill = "Social") +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 30),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 40),
    plot.subtitle = element_text(size = 32),
    strip.text.x = element_text(size = 20),
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom"
  )
plot_feature_importance

ggsave(figure_output_filename,
       plot_feature_importance,
       width = 30,
       height = 30,
       units = "cm",
       device = "pdf")
