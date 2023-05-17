library(dplyr)
library(ggplot)

rm(list = ls())
gc()

input_filename <- "/media/gabett/Volume/data-repository/panconesi-football-elections/ML/toxicity_prediction_metrics.csv"

df_metrics <- read.csv(input_filename)

bins = c("[10, 100]","(100, 1000]","(1000, 10000]","Overall")
topic_colors <- c("Football" = "#ff7b00",
                  "Elections" = "#003f88")

df_metrics_pivoted <- df_metrics %>% 
  filter(Tree.Interval == bins[1]) %>% 
  pivot_longer(Accuracy:AUC,
               names_to = "metric", 
               values_to = "value")  %>% 
  filter(metric %in% c("Accuracy", "F1", "AUC"))

ggplot(df_metrics_pivoted, 
       aes(x = Model,
           y = value,
           fill = Topic)) +
  geom_bar(stat = "identity",
           position = "dodge") + 
  geom_text(aes(label = value),
            position = position_dodge(width = 0.9), 
            vjust = 0.4,
            hjust = 1.2,
            color = "white") +
  facet_grid(vars(Social), vars(metric)) + 
  coord_flip() +
  scale_fill_manual(values = topic_colors) +
  theme_minimal() +
  labs(
    title = (paste("Prediction results for bin", bins[1])),
    y = "Metric Value",
    x = "Algorithm"
  )

