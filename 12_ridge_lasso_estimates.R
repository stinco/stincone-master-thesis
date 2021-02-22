#####
# 12. Ridge-LASSO Estimates
# Leonardo Stincone
# 21/02/2021
# R 4.0.2
#####

install.packages("ggforce")
# Libraries ####

library(tidyverse)
library(knitr)
library(kableExtra)
library(lubridate)
library(scales)
library(ggeasy)
library(ggforce)      # For plotting ellipses
library(gridExtra)
library(cowplot)
library(grid)
library(gtable)
library(mgcv)
library(splines)
library(forcats)
library(glmnet)       # For Elastic Net


theme_set(theme_bw())

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]

line_size <- 2



# Simulate data ####

n <- c(45, 20, 20, 10, 5)
b <- c(0, 2, 1, -1, -2)

sigma <- .5

set.seed(123)

df <- tibble(
  x = c(
    rep("a", n[1]),
    rep("b", n[2]),
    rep("c", n[3]),
    rep("d", n[4]),
    rep("e", n[5])
  ) %>% 
    factor(),
  mu = c(
    rep(b[1], n[1]),
    rep(b[2], n[2]),
    rep(b[3], n[3]),
    rep(b[4], n[4]),
    rep(b[5], n[5])
  )
)

df$y <- rnorm(n = sum(n), mean = df$mu, sd = sigma)


# # Maximum Likelihood
# df_summarize <- df %>% 
#   group_by(x) %>% 
#   summarize(
#     y_mean = mean(y),
#     y_sd = sd(y),
#     y_mean_sd = sd(y) / sqrt(n())
#   )
# 
# df %>% 
#   ggplot(aes(x = x, y = y)) +
#   geom_point(
#     data = df_summarize,
#     mapping = aes(x = x, y = y_mean, color = x),
#     # color = col1,
#     size = 5#,
#     # alpha = .8
#   ) +
#   geom_point(alpha = .5) +
#   easy_remove_legend()



# Ridge regression

x_mat <- model.matrix(y ~ x, data = df) 

# x_mat[1:5, ]

lambda_grid <- 10^seq(log10(0.001), log10(100), length.out = 100)

fit_ridge <- glmnet(
  x = x_mat, y = df$y,
  alpha = 1, lambda = lambda_grid
)

# # plot(fit_ridge, xvar = "norm", label = TRUE)
# plot(fit_ridge, xvar = "lambda", label = TRUE)
# # plot(fit_ridge, xvar = "dev", label = TRUE)
# 
# names(fit_ridge)
# 
# fit_ridge$lambda


fit_ridge_beta_long <- fit_ridge$beta %>% 
  t() %>% 
  as.matrix() %>% 
  as_data_frame() %>% 
  mutate(
    lambda = fit_ridge$lambda,
    log_lambda = log(lambda)
  ) %>% 
  # select(-`(Intercept)`) %>% 
  rename(xa = `(Intercept)`) %>% 
  pivot_longer(
    cols = xa:xe,
    names_to = "coefficient"
  ) %>% 
  mutate(coefficient = str_sub(coefficient, 2, 2))

# fit_ridge_beta_long %>% 
#   filter(value > 0) %>%
#   group_by(coefficient) %>% 
#   filter(lambda == max(lambda))

# lambda <- c(0, 0.1, 1, 10)
lambda <- c(0.1, 0.3, 0.6, 1)

fit_ridge_beta_long %>% 
  ggplot(aes(x = lambda, y = value, color = coefficient)) +
  geom_vline(
    xintercept = lambda,
    linetype = "dotted"
  ) +
  geom_line() +
  geom_text(
    fit_ridge_beta_long %>% 
      filter(lambda == min(lambda)),
    mapping = aes(label = coefficient),
    nudge_x = -0.1
  ) +
  scale_x_log10() +
  easy_remove_legend()
  




fit_ridge_pred <- predict(
  fit_ridge,
  type = "response",
  s = lambda,
  newx = unique(x_mat)
) %>% 
  as_tibble() %>%
  mutate(x = c("a", "b", "c", "d", "e")) %>% 
  pivot_longer(
    cols = -x
  ) %>% 
  left_join(
    tibble(
      name = as.character(1:length(lambda)),
      lambda = lambda
    ),
    by = "name"
  ) %>% 
  mutate(
    lambda = str_c("lambda = ", lambda) %>% 
      fct_inorder()
  )


# fit_ridge_pred %>% 
#   ggplot(aes(x = x, y = value, color = x)) +
#   geom_point() +
#   facet_wrap(~lambda) +
#   easy_remove_legend()


# df_summarize


df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(
    data = fit_ridge_pred,
    mapping = aes(x = x, y = value, color = x),
    size = 3#,
    # alpha = .8
  ) +
  geom_point(alpha = .3) +
  geom_point(
    data = fit_ridge_pred,
    mapping = aes(x = x, y = value, color = x),
    size = 3,
    alpha = .8
  ) +
  facet_wrap(~lambda) +
  easy_remove_legend()





































