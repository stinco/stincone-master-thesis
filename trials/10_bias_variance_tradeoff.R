#####
# 10. Bias-Variance tradeoff
# Leonardo Stincone
# 20/02/2021
# R 4.0.2
#####



# Libraries ####

library(tidyverse)
library(knitr)
library(kableExtra)
library(lubridate)
library(scales)
library(ggeasy)
library(gridExtra)
library(cowplot)
library(grid)
library(gtable)
library(mgcv)
library(splines)
library(forcats)

theme_set(theme_bw())

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]

line_size <- 2



# Bias-Variance plot ####

train_error <- function(x){
  (1 - 0.1) / (x + 1)^2
}

test_error <- function(x){
  train_error(x) + x^2/10 + 0.1
}


text_low <- textGrob(
  "Low Complexity\nHigh Bias\nLow Variance\nUnderfitting",
  gp = gpar(fontsize = 10)
)

text_high <- textGrob(
  "High Complexity\nLow Bias\nHigh Variance\nOverfitting",
  gp = gpar(fontsize = 10)
)

text_opt <- textGrob("Optimal Complexity", gp = gpar(fontsize = 10))

text_y <- -0.3
text_x_low <- 0.25
text_x_high <- 3.25


x_opt <- 1.0479
text_y_opt <- 1.55


tibble(
  x = c(0, 0),
  y = c(0, 0),
  # set = c("Training", "Test") %>% 
  set = c("In-Sample Error", "Out-Of-Sample Error") %>% 
    # factor(., levels = .)
    fct_inorder()
) %>% 
  ggplot(aes(x = x, y = y, color = set)) +
  geom_vline(
    xintercept = x_opt,
    linetype = "dotted"
  ) +
  stat_function(
    # data = tibble(x = 0, y = 0, set = "Training set"),
    data = tibble(x = 0, y = 0, set = "In-Sample Error"),
    fun = train_error,
    size = line_size
  ) +
  stat_function(
    # data = tibble(x = 0, y = 0, set = "Test set"),
    data = tibble(x = 0, y = 0, set = "Out-Of-Sample Error"),
    fun = test_error,
    size = line_size
  ) +
  scale_x_continuous(limits = c(0, 3.5)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Model Complexity", y = "Loss") +
  theme(plot.margin = unit(c(1.5, 1, 1.2, 1), "lines")) +
  # annotation_custom(text_high, xmin = 0.5, xmax = 0.5, ymin = -0.07, ymax = -0.07) + 
  annotation_custom(
    grob = text_low,
    xmin = text_x_low, xmax = text_x_low,
    ymin = text_y, ymax = text_y
  ) +
  annotation_custom(
    grob = text_high,
    xmin = text_x_high, xmax = text_x_high,
    ymin = text_y, ymax = text_y
  ) + 
  annotation_custom(
    grob = text_opt,
    xmin = x_opt, xmax = x_opt,
    ymin = text_y_opt, ymax = text_y_opt
  ) + 
  coord_cartesian(
    ylim = c(0, 1.4),
    clip = "off"
  ) +
  easy_remove_axes(
    which = "both",
    what = c("text", "ticks")
  ) +
  easy_remove_legend_title() +
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(5, 0, 0, 0)
    # legend.box.margin = margin(15, 0, 0, 0)
  )

?easy_legend_at
















