#####
# 12. Ridge-LASSO Estimates
# Leonardo Stincone
# 21/02/2021
# R 4.0.2
#####


# Libraries ####

# install.packages("ggforce")
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
b <- c(0, 2, 0.2, -1, -2)

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



# Ridge regression ####

x_mat <- model.matrix(y ~ x, data = df) 


lambda_grid_ridge <- 10^seq(log10(0.001), log10(100), length.out = 100)
# lambda_grid_lasso <- 10^seq(log10(0.001), log10(10), length.out = 100)

alpha_ridge <- 0
# alpha_lasso <- 1

fit_ridge <- glmnet(
  x = x_mat, y = df$y,
  alpha = alpha_ridge, lambda = lambda_grid_ridge
)


fit_ridge_beta_long <- fit_ridge$beta %>% 
  t() %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(
    lambda = fit_ridge$lambda,
    log_lambda = log(lambda)
  ) %>% 
  rename(xa = `(Intercept)`) %>% 
  pivot_longer(
    cols = xa:xe,
    names_to = "coefficient"
  ) %>% 
  mutate(coefficient = str_sub(coefficient, 2, 2))

lambda_ridge <- c(0, 0.1, 1, 10)
# lambda_lasso <- c(0, 0.1, 0.6, 1)


p_ridge_coeff <- fit_ridge_beta_long %>% 
  ggplot(aes(x = lambda, y = value, color = coefficient)) +
  geom_vline(
    xintercept = c(lambda_ridge[lambda_ridge > 0]),
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
  s = lambda_ridge,
  newx = unique(x_mat)
) %>% 
  as_tibble() %>%
  mutate(x = c("a", "b", "c", "d", "e")) %>% 
  pivot_longer(
    cols = -x
  ) %>% 
  left_join(
    tibble(
      name = as.character(1:length(lambda_ridge)),
      lambda = lambda_ridge
    ),
    by = "name"
  ) #%>% 
  # mutate(
  #   lambda = str_c("lambda = ", lambda) %>% 
  #     fct_inorder()
  # )



p_ridge_pred_1 <- df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_hline(
    # yintercept = mean(df$y),
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[1]) %>% 
      filter(x == "a"),
    mapping = aes(yintercept = value),
    linetype = "dotted"
  ) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[1]),
    mapping = aes(x = x, y = value, color = x),
    size = 3
  ) +
  geom_point(alpha = .3) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[1]),
    mapping = aes(x = x, y = value, color = x),
    size = 3,
    alpha = .8
  ) +
  # facet_wrap(~lambda) +
  easy_remove_legend()

p_ridge_pred_2 <- df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_hline(
    # yintercept = mean(df$y),
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[2]) %>% 
      filter(x == "a"),
    mapping = aes(yintercept = value),
    linetype = "dotted"
  ) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[2]),
    mapping = aes(x = x, y = value, color = x),
    size = 3
  ) +
  geom_point(alpha = .3) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[2]),
    mapping = aes(x = x, y = value, color = x),
    size = 3,
    alpha = .8
  ) +
  # facet_wrap(~lambda) +
  easy_remove_legend()

p_ridge_pred_3 <- df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_hline(
    # yintercept = mean(df$y),
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[3]) %>% 
      filter(x == "a"),
    mapping = aes(yintercept = value),
    linetype = "dotted"
  ) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[3]),
    mapping = aes(x = x, y = value, color = x),
    size = 3
  ) +
  geom_point(alpha = .3) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[3]),
    mapping = aes(x = x, y = value, color = x),
    size = 3,
    alpha = .8
  ) +
  # facet_wrap(~lambda) +
  easy_remove_legend()

p_ridge_pred_4 <- df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_hline(
    # yintercept = mean(df$y),
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[4]) %>% 
      filter(x == "a"),
    mapping = aes(yintercept = value),
    linetype = "dotted"
  ) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[4]),
    mapping = aes(x = x, y = value, color = x),
    size = 3
  ) +
  geom_point(alpha = .3) +
  geom_point(
    data = fit_ridge_pred %>% 
      filter(lambda == lambda_ridge[4]),
    mapping = aes(x = x, y = value, color = x),
    size = 3,
    alpha = .8
  ) +
  # facet_wrap(~lambda) +
  easy_remove_legend()

p_ridge_pred_1
p_ridge_pred_2
p_ridge_pred_3
p_ridge_pred_4
p_ridge_coeff





# LASSO regression ####

x_mat <- model.matrix(y ~ x, data = df) 


# lambda_grid_ridge <- 10^seq(log10(0.001), log10(100), length.out = 100)
lambda_grid_lasso <- 10^seq(log10(0.001), log10(10), length.out = 100)

# alpha_ridge <- 0
alpha_lasso <- 1

fit_lasso <- glmnet(
  x = x_mat, y = df$y,
  alpha = alpha_lasso, lambda = lambda_grid_lasso
)


fit_lasso_beta_long <- fit_lasso$beta %>% 
  t() %>% 
  as.matrix() %>% 
  as_tibble() %>% 
  mutate(
    lambda = fit_lasso$lambda,
    log_lambda = log(lambda)
  ) %>% 
  rename(xa = `(Intercept)`) %>% 
  pivot_longer(
    cols = xa:xe,
    names_to = "coefficient"
  ) %>% 
  mutate(coefficient = str_sub(coefficient, 2, 2))

# lambda_ridge <- c(0, 0.1, 1, 10)
lambda_lasso <- c(0, 0.1, 0.6, 1)


p_lasso_coeff <- fit_lasso_beta_long %>% 
  ggplot(aes(x = lambda, y = value, color = coefficient)) +
  geom_vline(
    xintercept = c(lambda_lasso[lambda_lasso > 0]),
    linetype = "dotted"
  ) +
  geom_line() +
  geom_text(
    fit_lasso_beta_long %>% 
      filter(lambda == min(lambda)),
    mapping = aes(label = coefficient),
    nudge_x = -0.1
  ) +
  scale_x_log10() +
  easy_remove_legend()
  

fit_lasso_pred <- predict(
  fit_lasso,
  type = "response",
  s = lambda_lasso,
  newx = unique(x_mat)
) %>% 
  as_tibble() %>%
  mutate(x = c("a", "b", "c", "d", "e")) %>% 
  pivot_longer(
    cols = -x
  ) %>% 
  left_join(
    tibble(
      name = as.character(1:length(lambda_lasso)),
      lambda = lambda_lasso
    ),
    by = "name"
  ) %>% 
  mutate(
    lambda = str_c("lambda = ", lambda) %>% 
      fct_inorder()
  )


p_lasso_pred <- df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_hline(
    # yintercept = mean(df$y),
    data = fit_lasso_pred %>% 
      filter(x == "a"),
    mapping = aes(yintercept = value),
    linetype = "dotted"
  ) +
  geom_point(
    data = fit_lasso_pred,
    mapping = aes(x = x, y = value, color = x),
    size = 3
  ) +
  geom_point(alpha = .3) +
  geom_point(
    data = fit_lasso_pred,
    mapping = aes(x = x, y = value, color = x),
    size = 3,
    alpha = .8
  ) +
  facet_wrap(~lambda) +
  easy_remove_legend()


p_lasso_coeff
p_lasso_pred

























