#####
# 4. Variables effects
# Leonardo Stincone
# 13/12/2020
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

theme_set(theme_bw())


# Variables effect trials ####

set.seed(42)

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]


n <- 100
b0 <- 1
b1 <- 2
b2 <- 1
b12 <- -1
sigma <- .2

df1 <- tibble(x = runif(n = n, min = 0, max = 1)) %>% 
  mutate(mu = b0 + b1 * x)
df1$y <- rnorm(n = n, mean = df1$mu, sd = sigma)
# mod1 <- lm(data = df1, formula = y ~ x)

df2 <- tibble(x = c(rep(0, times = n/2), rep(1, times = n/2))) %>% 
  mutate(mu = b0 + b2 * x)
df2$y <- rnorm(n = n, mean = df2$mu, sd = sigma)
# mod2 <- lm(data = df2, formula = y ~ x)

df3 <- tibble(x1 = runif(n = n, min = 0, max = 1),
              x2 = c(rep(0, times = n/2), rep(1, times = n/2))) %>% 
  mutate(mu = b0 + b1 * x1 + b2*x2)
df3$y <- rnorm(n = n, mean = df3$mu, sd = sigma)
# mod3 <- lm(data = df3, formula = y ~ x1 + x2)

df4 <- tibble(x1 = runif(n = n, min = 0, max = 1),
              x2 = c(rep(0, times = n/2), rep(1, times = n/2))) %>% 
  mutate(mu = b0 + b1 * x1 + b2 * x2 + b12 * x1 * x2)
df4$y <- rnorm(n = n, mean = df4$mu, sd = sigma)
# mod3 <- lm(data = df3, formula = y ~ x1 + x2)


p1 <- df1 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_abline(
    intercept = b0,
    slope = b1,
    color = col1
  ) +
  geom_point(alpha = .5) +
  labs(title = "Quantitative variable") #+
  # easy_remove_axes(
  #   which = "both",
  #   what = "text",
  #   teach = FALSE
  # )
  

p2 <- df2 %>% 
  ggplot(aes(x = x, y = y)) +
  geom_abline(
    intercept = b0,
    slope = b2,
    color = col1
  ) +
geom_point(alpha = .5) +
  geom_point(
    data = tibble(x = c(0, 1), y = c(b0, b0 + b2)),
    mapping = aes(x = x, y = y),
    color = col1,
    size = 3,
    alpha = .8
  ) +
  labs(title = "Qualitative variable") #+
  # easy_remove_axes(
  #   which = "both",
  #   what = "text",
  #   teach = FALSE
  # )


p3 <- df3 %>% 
  mutate(x2 = factor(x2)) %>% 
  ggplot(aes(x = x1, color = x2, y = y)) +
  geom_abline(
    intercept = b0,
    slope = b1,
    color = col1
  ) +
  geom_abline(
    intercept = b0 + b2,
    slope = b1,
    color = col2
  ) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c(col1, col2)) +
  labs(title = "Quantitative and qualitative variable without interaction") #+
  # easy_remove_axes(
  #   which = "both",
  #   what = "text",
  #   teach = FALSE
  # )


p4 <- df4 %>% 
  mutate(x2 = factor(x2)) %>% 
  ggplot(aes(x = x1, color = x2, y = y)) +
  geom_abline(
    intercept = b0,
    slope = b1,
    color = col1
  ) +
  geom_abline(
    intercept = b0 + b2,
    slope = b1 + b12,
    color = col2
  ) +
  geom_point(alpha = .5) +
  scale_color_manual(values = c(col1, col2)) +
  labs(title = "Quantitative and qualitative variable with interaction") #+
  # easy_remove_axes(
  #   which = "both",
  #   what = "text",
  #   teach = FALSE
  # )


p1
p2
p3
p4

# grid.arrange(
#   p1, p2, p3, p4
# )


# cowplot ####

# install.packages("cowplot")
library(cowplot)

plot_grid_split <- function(..., align = "hv", axis= "tblr"){
  aligned_plots <- cowplot::align_plots(..., align=align, axis=axis)
  plots <- lapply(1:length(aligned_plots), function(x){
    cowplot::ggdraw(aligned_plots[[x]])
  })
  invisible(capture.output(plots))
}


plot_grid(p1, p2, p3, p4,
          align = 'vh',
          axis = "l")

plot_grid_split(p1, p2, p3, p4)





?plot_grid












