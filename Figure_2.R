
setwd("C:/Aceton_method")

library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

df <- read.csv("RealWorld_Application_SARS_PMMoV.csv")



df_long_1 <- df %>%
  select(Date, PMMoV_EPISENS, PMMoV_Aceton) %>%
  drop_na() %>%
  mutate(PMMoV_EPISENS_1 = log10(PMMoV_EPISENS)) %>%
  mutate(PMMoV_Aceton_1 = log10(PMMoV_Aceton)) %>%
  pivot_longer(cols = c(PMMoV_EPISENS_1, PMMoV_Aceton_1), names_to = "Method", values_to = "Value") %>%
  mutate(Method = factor(Method, levels = c("PMMoV_EPISENS_1", "PMMoV_Aceton_1")))

df_long_2 <- df %>%
  select(Date, SARS_EPISENS, SARS_Aceton) %>%
  drop_na() %>%
  mutate(SARS_EPISENS_1 = log10(SARS_EPISENS)) %>%
  mutate(SARS_Aceton_1 = log10(SARS_Aceton)) %>%
  pivot_longer(cols = c(SARS_EPISENS_1, SARS_Aceton_1), names_to = "Method", values_to = "Value") %>%
  mutate(Method = factor(Method, levels = c("SARS_EPISENS_1", "SARS_Aceton_1")))


my_colors <- c("PMMoV_EPISENS_1" = "lightskyblue3", "PMMoV_Aceton_1" = "thistle1")
my_colors_SARS <- c("SARS_EPISENS_1" = "lightskyblue3", "SARS_Aceton_1" = "thistle1")
#my_colors_SARS <- c("SARS_EPISENS_1" = "#F8766D", "SARS_Aceton_1" = "#00BFC4")
#my_colors <- c("PMMoV_EPISENS_1" = "#F8766D", "PMMoV_Aceton_1" = "#00BFC4")

fig_PMMoV <- ggplot(df_long_1, aes(x = Method, y = Value)) +
  geom_boxplot(aes(fill = Method), width = 0.35, alpha = 0.25, outlier.shape = NA) +
  geom_line(aes(group = Date), color = "gray50", alpha = 0.6) +
  geom_point(aes(fill = Method), shape = 21, size = 3, colour = "black") +
  scale_y_continuous(limits = c(6.8, 7.85), breaks = seq(6.8, 7.7, 0.3)) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x  = element_blank(),     
    #axis.text.x   = element_blank(),    
    #axis.ticks.x  = element_blank(),     
    axis.title.y  = element_blank(), 
    axis.line = element_line(linewidth = 0.5, lineend = "square"),
    #axis.line.x   = element_blank(), 
    text = element_text(colour = "black", size = 11),
    legend.position = "none",
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(-2, "mm")
  )



fig_SARS <- ggplot(df_long_2, aes(x = Method, y = Value)) +
  geom_boxplot(aes(fill = Method), width = 0.35, alpha = 0.25, outlier.shape = NA) +
  geom_line(aes(group = Date), color = "gray50", alpha = 0.6) +
  geom_point(aes(fill = Method), shape = 21, size = 3, colour = "black") +
  scale_y_continuous(limits = c(2, 5.5), breaks = seq(2, 5, 1)) +
  scale_fill_manual(values = my_colors_SARS) +
  scale_color_manual(values = my_colors_SARS) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x  = element_blank(),     
    #axis.text.x   = element_blank(),    
    #axis.ticks.x  = element_blank(),     
    axis.title.y  = element_blank(), 
    axis.line = element_line(linewidth = 0.5, lineend = "square"),
    #axis.line.x   = element_blank(), 
    text = element_text(colour = "black", size = 11),
    legend.position = "none",
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(-2, "mm")
  )


#675*360
fig_2 <- plot_grid(
  fig_PMMoV, fig_SARS,
  ncol = 2,
  align = "v",
  rel_heights = c(1.0, 1.0)
)
fig_2


#----------------------------------------------------------------------------------------------------Supporing: Histogram, Q-Q plot
df_ratio <- df %>% filter(!is.na(SARS_Ratio))
df_ratio_PMMoV <- df %>% filter(!is.na(PMMoV_Ratio))

# 共通のテーマ設定（先ほどのグラフとデザインを統一）
my_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.title.x  = element_blank(), 
    axis.title.y  = element_blank(), 
    axis.line = element_line(linewidth = 0.5, lineend = "square"),
    text = element_text(colour = "black", size = 11),
    axis.ticks = element_line(linewidth = 0.5),
    axis.ticks.length = unit(-2, "mm"),
    panel.grid.minor = element_blank()
  )

p_hist_log <- ggplot(df_ratio, aes(x = log10(SARS_Ratio))) +
  geom_histogram(bins = 8, fill = "#00BFC4", color = "black", alpha = 0.5, linewidth = 0.4) +
  geom_vline(aes(xintercept = median(log10(SARS_Ratio))), linetype = "dashed", color = "black", linewidth = 0.6) +
  my_theme

p_qq_log <- ggplot(df_ratio, aes(sample = log10(SARS_Ratio))) +
  stat_qq_line(color = "black", linewidth = 0.8, linetype = "dotted") +
  stat_qq(shape = 21, fill = "#00BFC4", color = "black", size = 3, alpha = 0.85) +
  my_theme


p_hist_log_PMMoV <- ggplot(df_ratio_PMMoV, aes(x = log10(PMMoV_Ratio))) +
  geom_histogram(bins = 7, fill = "#F8766D", color = "black", alpha = 0.5, linewidth = 0.4) +
  geom_vline(aes(xintercept = median(log10(PMMoV_Ratio))), linetype = "dashed", color = "black", linewidth = 0.6) +
  my_theme

p_qq_log_PMMoV <- ggplot(df_ratio_PMMoV, aes(sample = log10(PMMoV_Ratio))) +
  stat_qq_line(color = "black", linewidth = 0.8, linetype = "dotted") +
  stat_qq(shape = 21, fill = "#F8766D", color = "black", size = 3, alpha = 0.85) +
  my_theme


#650*420
fig_S <- plot_grid(
  p_hist_log_PMMoV, p_qq_log_PMMoV,
  p_hist_log, p_qq_log,
  ncol = 2,
  align = "v",
  rel_heights = c(1.0, 1.0)
)
fig_S



# Shapilo-wilk test
shapiro.test(log10(df_ratio$SARS_Ratio))
shapiro.test(log10(df_ratio_PMMoV$PMMoV_Ratio))
