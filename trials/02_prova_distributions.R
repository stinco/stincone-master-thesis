#####
# 2. Prova Distributions
# Leonardo Stincone
# 21/11/2020
# R 4.0.2
#####

library(tidyverse)
library(knitr)
library(kableExtra)

theme_set(theme_bw())




# Poisson ####

lambda = 2.5

tibble(
  x = 0:20
) %>% 
  mutate(
    y = dpois(x = x, lambda = lambda)
  ) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(aes(xend = x),
               yend = 0) +
  labs(x = "k", y = "p(k)",
       title = "Poisson Distribution",
       subtitle = str_c("lambda = ", lambda))



# tibble(x = 0:20) %>% 
#   crossing(
#     tibble(
#       lambda = c(.8, 1, 2.5, 10)
#     )
#   ) %>% 
#   mutate(
#     y = dpois(x = x, lambda = lambda),
#     lambda_label = str_c("lambda = ", lambda) %>% 
#       fct_inorder()
#   ) %>% 
#   ggplot(aes(x = x, y = y)) +
#   geom_point() +
#   geom_segment(
#     aes(xend = x),
#     yend = 0
#   ) +
#   facet_wrap(~lambda_label) +
#   labs(
#     x = "k", y = "p(k)",
#     title = "Poisson Distribution"
#   )

tibble(x = 0:22) %>% 
  crossing(
    tibble(
      lambda = c(.8, 1, 2.5, 10)
    )
  ) %>% 
  mutate(
    y = dpois(x = x, lambda = lambda),
    lambda_label = str_c("lambda = ", lambda) %>% 
      fct_inorder()
  ) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(
    aes(xend = x),
    yend = 0
  ) +
  facet_wrap(~lambda_label) +
  coord_cartesian(
    xlim = c(0, 20)
  ) +
  labs(
    x = "n", y = "p(n)"
  )


# Gamma ####


tibble(x = seq(from = 0, to = 25, by = .01)) %>% 
  crossing(
    tibble(
      alpha = c(.8, 1, 2, 16),
      rho = c(.2, .25, .5, 2)
    )
  ) %>% 
  mutate(
    y = dgamma(x = x, shape = alpha, rate = rho),
    # label = str_c("alpha = ", alpha, ", rho = ", rho) %>% 
    #   fct_inorder()
    label = str_c("\u03b1 = ", alpha, ", \u03c1 = ", rho) %>% 
      fct_inorder()
  ) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(~label) +
  coord_cartesian(
    ylim = c(0, .3),
    xlim = c(0, 20)
  ) +
  labs(
    x = "z", y = "f(z)"
  )






# Binomial ####


tibble(x = seq(from = 0, to = 10, by = 1)) %>% 
  crossing(
    tibble(
      n = c(1, 4, 10, 10),
      p = c(.4, .2, .2, .6)
    )
  ) %>%
  filter(x <= n) %>% 
  mutate(
    y = dbinom(x = x, size = n, prob = p),
    # label = str_c("alpha = ", alpha, ", rho = ", rho) %>% 
    #   fct_inorder()
    label = str_c("n = ", n, ", p = ", p) %>% 
      fct_inorder()
  ) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_segment(aes(xend = x),
               yend = 0) +
  facet_wrap(~label) +
  coord_cartesian(
    # ylim = c(0, .3),
    xlim = c(0, 10)
  ) +
  labs(
    x = "x", y = "f(x)"
  ) +
  scale_x_continuous(breaks = seq(from = 0, to = 10, by = 2))










