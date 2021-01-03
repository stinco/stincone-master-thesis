#####
# 7. Wiggliness
# Leonardo Stincone
# 2/01/2021
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


theme_set(theme_bw())


# Pauli trial ####

# df <- tibble(
#   x = seq(from = 0, to = 1, by = 0.001),
#   y = 
# )

par(mar = c(2, 2, 0, 0), mfrow = c(2, 3))
layout(matrix(1:6, byrow = FALSE, nrow = 2))
f = expression(sin(2*pi*x) + x)
curve(eval(f, data.frame(x = x)), from = 0, to = 1, ylim = c(-1, 1))
d2 = D(D(f, name = "x"), name = "x")
curve(eval(d2, data.frame(x = x))^2, ylim = c(0, 17000), yaxs = "i")

f = expression(sin(3*pi*x) + x)
curve(eval(f, data.frame(x = x)), from = 0, to = 1, ylim = c(-1, 1))
d2 = D(D(f, name = "x"), name = "x")
curve(eval(d2, data.frame(x = x))^2, ylim = c(0, 17000), yaxs = "i")

#f = expression((0.5 + 5*x)*sin(5*pi*x^3)/(5.5))
f = expression(0.76*(sin(pi*x) + sin(4*pi*x)) - 0.5)
curve(eval(f, data.frame(x = x)), from = 0, to = 1, ylim = c(-1, 1))
d2 = D(D(f, name = "x"), name = "x")
curve(eval(d2, data.frame(x = x))^2, ylim = c(0, 17000), yaxs = "i")




# My trial ####

f2_expr <- expression(sin(2*pi*x))
f2_2_expr <- D(D(f2_expr, name = "x"), name = "x")

f3_expr <- expression(sin(3*pi*x))
f3_2_expr <- D(D(f3_expr, name = "x"), name = "x")

f4_expr <- expression(0.76*(sin(pi*x) + sin(4*pi*x)) - 0.5)
f4_2_expr <- D(D(f4_expr, name = "x"), name = "x")



f1 <- function(x){x - 1/2}
f1_2 <- function(x){0}

f1 <- function(x){eval(f2_expr[[1]])}
f2_2 <- function(x){eval(f2_2_expr)^2}

f2 <- function(x){eval(f3_expr[[1]])}
f3_2 <- function(x){eval(f3_2_expr)^2}

f3 <- function(x){eval(f4_expr[[1]])}
f4_2 <- function(x){eval(f4_2_expr)^2}



tibble(
  x = 0:1,
  y = 0:1,
  type = c("f", "f2") %>% 
    factor(levels = c("f", "f2")),
  fun = 1:2
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_abline(aes(intercept = intercept, slope = slope),
              data = data.frame(x = 0, y = 0,
                                type = factor("f"),
                                fun = 1,
                                intercept = -.5,
                                slope = 1),
              col = col1,
              size = line_size) +
  geom_area(stat = "function", fun = f1_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 1),
            alpha = .5,
            col = col1,
            size = line_size / 2) +
  stat_function(fun = f1,
                data = data.frame(x = 0, y = 0,
                                  type = factor("f"),
                                  fun = 2),
                col = col1,
                size = line_size) +
  geom_area(stat = "function", fun = f2_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 2),
            alpha = .5,
            col = col1,
            size = line_size / 2) +
  stat_function(fun = f2,
                data = data.frame(x = 0, y = 0,
                                  type = factor("f"),
                                  fun = 3),
                col = col1,
                size = line_size) +
  geom_area(stat = "function", fun = f3_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 3),
            alpha = .5,
            col = col1,
            size = line_size / 2) +
  stat_function(fun = f3,
                data = data.frame(x = 0, y = 0,
                                  type = factor("f"),
                                  fun = 4),
                col = col1,
                size = line_size) +
  geom_area(stat = "function", fun = f4_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 4),
            alpha = .5,
            col = col1,
            size = line_size / 2) +
  facet_grid(type ~ fun,
             scales = "free_y") +
  scale_x_continuous(limits = c(0, 1))


