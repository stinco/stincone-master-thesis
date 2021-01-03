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


# wiggliness ####

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


f1_expr <- expression(sin(2*pi*x))
f1_2_expr <- D(D(f1_expr, name = "x"), name = "x")

f2_expr <- expression(sin(3*pi*x))
f2_2_expr <- D(D(f2_expr, name = "x"), name = "x")

f3_expr <- expression(0.76*(sin(pi*x) + sin(4*pi*x)) - 0.5)
f3_2_expr <- D(D(f3_expr, name = "x"), name = "x")


f1 <- function(x){eval(f1_expr[[1]])}
f1_2 <- function(x){eval(f1_2_expr)^2}

f2 <- function(x){eval(f2_expr[[1]])}
f2_2 <- function(x){eval(f2_2_expr)^2}

f3 <- function(x){eval(f3_expr[[1]])}
f3_2 <- function(x){eval(f3_2_expr)^2}



tibble(
  x = (0:100) / 100,
  y = rep(0, 101),
  type = c(rep("f", 50), rep("f2", 51)) %>% 
    factor(levels = c("f", "f2")),
  fun = c(rep(1, 101), rep(2, 0), rep(3, 0))
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  stat_function(fun = f1,
                data = data.frame(x = 0, y = 0,
                                  type = factor("f"),
                                  fun = 1)) +
  geom_area(stat = "function", fun = f1_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 1),
            alpha = .5) +
  stat_function(fun = f2,
                data = data.frame(x = 0, y = 0,
                                  type = factor("f"),
                                  fun = 2)) +
  geom_area(stat = "function", fun = f2_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 2),
            alpha = .5) +
  stat_function(fun = f3,
                data = data.frame(x = 0, y = 0,
                                  type = factor("f"),
                                  fun = 3)) +
  geom_area(stat = "function", fun = f3_2,
            data = data.frame(x = 0, y = 0,
                              type = factor("f2"),
                              fun = 3),
            alpha = .5) +
  facet_grid(type ~ fun,
             scales = "free_y")


sin(pi/2)

eval(f1, data.frame(x = 1.1, panel = 0))

f1(1)
f1_2(1)


f <- function(x){eval(f1[[1]])}
f(1)

f1_2(.2)
