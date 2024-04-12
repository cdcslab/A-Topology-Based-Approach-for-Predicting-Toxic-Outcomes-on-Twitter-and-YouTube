library(dplyr)
library(ggplot2)
library(tidyr)

rm(list = ls())
gc()

input_filename <-
  "ML/gbrt_cross_topic_metrics.csv"

figures_folder <- "figures/ML"

df_metrics <- read.csv(input_filename)

topic_colors <- c("Football vs Elections" = "#ff7b00",
                  "Elections vs Football" = "#003f88")

df_metrics_pivoted <- df_metrics %>%
  pivot_longer(Accuracy:AUC,
               names_to = "metric",
               values_to = "value")  %>%
  filter(metric %in% c("Accuracy", "F1"))

df_metrics_pivoted$Tree.Interval <-
  factor(
    df_metrics_pivoted$Tree.Interval,
    levels = rev(c("[10, 100]",
                   "(100, 1000]",
                   "(1000, 10000]",
                   "Overall"))
  )

df_metrics_pivoted$metric <- factor(df_metrics_pivoted$metric,
                                    levels = c("Accuracy", "F1"))
plot_gbrt_metrics <- ggplot(df_metrics_pivoted,
                            aes(x = Tree.Interval,
                                y = value,
                                fill = Topic)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  geom_text(
    aes(label = value),
    position = position_dodge(width = 0.9),
    size = 10,
    vjust = 0.4,
    hjust = 1.2,
    color = "white"
  ) +
  facet_grid(vars(Social), vars(metric)) +
  coord_flip() +
  scale_fill_manual(values = topic_colors) +
  ylim(0,1) + 
  theme_minimal() +
  labs(title = "",
       y = "Metric Value",
       x = "Interval") +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 40),
    legend.title = element_text(size = 40, vjust = 1),
    legend.text = element_text(size = 30),
    strip.text.x = element_text(size = 30),
    strip.text.y = element_text(size = 30),
    panel.spacing.x=unit(2, "lines"),
    legend.key.height = unit(1, "cm"),
    legend.key.width = unit(1, "cm"),
    aspect.ratio = 1,
    legend.position = "bottom"
  )
plot_gbrt_metrics

figure_filename <- paste(figures_folder,
                         "plot_cross_topic_gbrt_metrics.pdf",
                         sep = "/")


ggsave(figure_filename,
       plot_gbrt_metrics,
       device = "pdf",
       height = 30,
       width = 40,
       unit = "cm")
