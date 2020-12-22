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
library(cowplot)
library(grid)
library(gtable)


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






# Quantitative variables ####

set.seed(42)

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]


n <- 200
b0 <- 1
b1 <- 2
b2 <- 1
b12 <- -1
sigma <- 0.05


f1 <- function(x){1.5 * (x - .3)^2 + .25}

f2 <- function(x){20*(x - .5)^4 + -4 * (x - .8)^2 - 2 * x + 2}

f3 <- function(x){
  case_when(
    x <= .25 ~ -2 * x + 1,
    x <= .75 ~ -1/2 * x + 5/8,
    TRUE ~ 1/4
  )
}

f4 <- function(x){
  case_when(
    x <= .75 ~ 1.5 * (x - .75)^2 + .2,
    TRUE ~ .2
  )
}

df <- tibble(x = runif(n = n, min = 0, max = 1)) %>% 
  mutate(
    mu1 = f1(x),
    mu2 = f2(x),
    mu3 = f3(x),
    mu4 = f4(x)
  )


df$y1 <- rnorm(n = n, mean = df$mu1, sd = sigma)
df$y2 <- rnorm(n = n, mean = df$mu2, sd = sigma)
df$y3 <- rnorm(n = n, mean = df$mu3, sd = sigma)
df$y4 <- rnorm(n = n, mean = df$mu4, sd = sigma)


df %>% 
  select(x, y1:y4) %>% 
  pivot_longer(cols = y1:y4) %>% 
  ggplot() +
  stat_function(data = tibble(name = "y1"),
                fun = f1,
                col = col1,
                size = 2) +
  stat_function(data = tibble(name = "y2"),
                fun = f2,
                col = col1,
                size = 2) +
  geom_vline(data = tibble(name = "y3", xint = c(.25, .75)),
             aes(xintercept = xint),
             linetype = "dotted") +
  stat_function(data = tibble(name = "y3"),
                fun = f3,
                col = col1,
                size = 2) +
  geom_vline(data = tibble(name = "y4", xint = .75),
             aes(xintercept = xint),
             linetype = "dotted") +
  stat_function(data = tibble(name = "y4"),
                fun = f4,
                col = col1,
                size = 2) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  scale_x_continuous(limits = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  ) +
  facet_wrap(~name)




p1 <- df %>% 
  select(x, value = y1) %>% 
  ggplot() +
  stat_function(
    fun = f1,
    col = col1,
    size = 2
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  scale_x_continuous(limits = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )


p2 <- df %>% 
  select(x, value = y2) %>%  
  ggplot() +
  stat_function(
    fun = f2,
    col = col1,
    size = 2
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  scale_x_continuous(limits = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )

p3 <- df %>% 
  select(x, value = y3) %>%  
  ggplot() +
  # geom_vline(data = tibble(xint = c(.25, .75)),
  #            aes(xintercept = xint),
  #            linetype = "dotted") +
  geom_vline(xintercept = c(.25, .75),
             linetype = "dotted") +
  stat_function(
    fun = f3,
    col = col1,
    size = 2
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  scale_x_continuous(limits = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )

p4 <- df %>% 
  select(x, value = y4) %>%  
  ggplot() +
  geom_vline(xintercept = .75,
             linetype = "dotted") +
  stat_function(
    fun = f4,
    col = col1,
    size = 2
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  scale_x_continuous(limits = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )



# response families ####

set.seed(42)

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]

line_size <- 2

n <- 200
b0 <- -2
b1 <- 4
# b2 <- 1
# b12 <- -1
sigma <- .2
alpha <- 2

df <- tibble(x = runif(n = n, min = 0, max = 1)) %>% 
  mutate(
    eta1 = b0 + b1 * x,
    eta2 = b0 + b1 * x,
    eta3 = b0 + b1 * x,
    eta4 = b0 + b1 * x,
    mu1 = eta1,
    mu2 = plogis(eta2),
    mu3 = exp(eta3),
    mu4 = exp(eta4)
  )

df$y1 <- rnorm(n = n, mean = df$mu1, sd = sigma)
df$y2 <- rbinom(n = n, size = 1, prob = df$mu2)
df$y3 <- rpois(n = n, lambda = df$mu3)
df$y4 <- rgamma(n = n, shape = alpha, rate = alpha/df$mu4)



p_resp_1 <- df %>% 
  select(x, value = y1) %>% 
  ggplot() +
  geom_abline(
    intercept = b0,
    slope = b1,
    col = col1,
    size = line_size
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  # scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )

p_resp_2 <- df %>% 
  select(x, value = y2) %>%  
  ggplot() +
  stat_function(
    fun = function(x){plogis(b0 + b1 * x)},
    col = col1,
    size = line_size,
    xlim = c(-0.05, 1.05)
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  # scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )

p_resp_3 <- df %>% 
  select(x, value = y3) %>%  
  ggplot() +
  stat_function(
    fun = function(x){exp(b0 + b1 * x)},
    col = col1,
    size = line_size,
    xlim = c(-0.05, 1.05)
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  # scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )

p_resp_4 <- df %>% 
  select(x, value = y4) %>%  
  ggplot() +
  stat_function(
    fun = function(x){exp(b0 + b1 * x)},
    col = col1,
    size = line_size,
    xlim = c(-0.05, 1.05)
  ) +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  # scale_x_continuous(limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, 1)) +
  easy_remove_axes(
    which = "both",
    what = "text",
    teach = FALSE
  )


plot_grid_split(p_resp_1, p_resp_2, p_resp_3, p_resp_4,
                align = "h")




# Significative variables ####

set.seed(42)

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]

line_size <- 2

n <- 1000
b0 <- -2
b1 <- 4
sigma <- 2

df <- tibble(x = rbeta(n = n, shape1 = 3, shape2 = 3)) %>% 
  mutate(
    mu1 = 0,
    mu2 = b0 + b1 * x,
    x_group = floor(10*x) / 10 + 0.05
  )

df$y1 <- rnorm(n = n, mean = df$mu1, sd = sigma)
df$y2 <- rnorm(n = n, mean = df$mu2, sd = sigma)


# Top-left plot
df %>% 
  select(x, value = y1) %>% 
  ggplot() +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(-7, 7)) +
  labs(x = "x", y = "y")


# Top-right plot
df %>% 
  select(x, value = y2) %>% 
  ggplot() +
  geom_point(aes(x = x, y = value),
             alpha = .4) +
  coord_cartesian(xlim = c(0, 1),
                  ylim = c(-7, 7)) +
  labs(x = "x", y = "y")



# Aggregate data
df_summary <- df %>% 
  group_by(x_group) %>% 
  summarize(
    n = n(),
    y1_mean = mean(y1),
    y1_sd = sd(y1) / sqrt(n),
    y2_mean = mean(y2),
    y2_sd = sd(y2) / sqrt(n)
  ) %>% 
  mutate(
    y1_up = y1_mean + 2 * y1_sd,
    y1_down = y1_mean - 2 * y1_sd,
    y2_up = y2_mean + 2 * y2_sd,
    y2_down = y2_mean - 2 * y2_sd
  )



# Bottom-left plot
df_plot_1 <- df_summary %>% 
  select(x = x_group, mean = y1_mean, down = y1_down, up = y1_up, n) %>% 
  pivot_longer(cols = c("mean", "n"),
               names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("mean", "n")))


p_1 <- df_plot_1 %>%
  mutate(x = factor(x)) %>%
  ggplot(aes(x = x, y = value)) +
  facet_grid(
    variable ~ .,
    scales = "free",
    labeller = labeller(variable = c("mean" = "y", "n" = "count"))
  ) +
  geom_point(data = filter(df_plot_1, variable == "mean")) +
  geom_line(data = filter(df_plot_1, variable == "mean"),
            group = 1) +
  geom_ribbon(data = filter(df_plot_1, variable == "mean"),
              aes(x = x, ymin = down, ymax = up),
              alpha = .5) +
  geom_col(data = filter(df_plot_1, variable == "n"),
           alpha = .8, col = hue_pal()(1), fill = hue_pal()(1)) +
  labs(x = "x", y = "", title = "")


p_1_ylim <- p_1 +
  coord_cartesian(ylim = c(-2.5, 2.5))

g_1 <- ggplotGrob(p_1)
g_1_ylim <- ggplotGrob(p_1_ylim)

g_1[["grobs"]][[2]] <- g_1_ylim[["grobs"]][[2]]
g_1[["grobs"]][[6]] <- g_1_ylim[["grobs"]][[6]]


g_1$heights[7] = 3*g_1$heights[7]
grid.draw(g_1)



# Bottom-right plot
df_plot_2 <- df_summary %>% 
  select(x = x_group, mean = y2_mean, down = y2_down, up = y2_up, n) %>% 
  pivot_longer(cols = c("mean", "n"),
               names_to = "variable", values_to = "value") %>% 
  mutate(variable = factor(variable, levels = c("mean", "n")))
  


p_2 <- df_plot_2 %>%
  mutate(x = factor(x)) %>%
  ggplot(aes(x = x, y = value)) +
  facet_grid(
    variable ~ .,
    scales = "free",
    labeller = labeller(variable = c("mean" = "y", "n" = "count"))#,
  ) +
  geom_point(data = filter(df_plot_2, variable == "mean")) +
  geom_line(data = filter(df_plot_2, variable == "mean"),
            group = 1) +
  geom_ribbon(data = filter(df_plot_2, variable == "mean"),
              aes(x = x, ymin = down, ymax = up),
              alpha = .5) +
  geom_col(data = filter(df_plot_2, variable == "n"),
           alpha = .8, col = hue_pal()(1), fill = hue_pal()(1)) +
  labs(x = "x", y = "", title = "")

p_2_ylim <- p_2 +
  coord_cartesian(ylim = c(-2.5, 2.5))

g_2 <- ggplotGrob(p_2)
g_2_ylim <- ggplotGrob(p_2_ylim)

g_2[["grobs"]][[2]] <- g_2_ylim[["grobs"]][[2]]
g_2[["grobs"]][[6]] <- g_2_ylim[["grobs"]][[6]]


g_2$heights[7] = 3*g_2$heights[7]
grid.draw(g_2)















