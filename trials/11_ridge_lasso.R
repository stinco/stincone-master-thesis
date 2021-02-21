#####
# 11. Ridge-LASSO
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


theme_set(theme_bw())

col1 <- hue_pal()(2)[1]
col2 <- hue_pal()(2)[2]

line_size <- 2



# Plot Lagrange ####

c0_calc <- function(angle, a, b){
  cos(angle)^2/a^2 + sin(angle)^2/b^2
}
c1_calc <- function(angle, a, b){
  sin(angle)^2/a^2 + cos(angle)^2/b^2
}
c2_calc <- function(angle, a, b){
  sin(2*angle)/a^2 - sin(2*angle)/b^2
}

write_equation <- function(c0, c1, c2){
  str_c(
    "minimize (",
    c0,
    "*(x-2)^2 + ",
    c1,
    "*(y-1/2)^2 + ",
    c2,
    "*(x-2)(y-1/2))"
  )
}

# Ridge
write_equation_ridge <- function(c0, c1, c2){
  str_c(
    write_equation(c0, c1, c2),
    " over (x^2+y^2 = 1)"
  )
}

# LASSO
write_equation_lasso <- function(c0, c1, c2){
  str_c(
    write_equation(c0, c1, c2),
    " over (|x|+|y| = 1)"
  )
}


x0 <- 2
y0 <- 1/2
angle <- pi/8

a <- 2
b <- 1

c0 <- c0_calc(angle, a, b)
c1 <- c1_calc(angle, a, b)
c2 <- c2_calc(angle, a, b)

write_equation_lasso(c0, c1, c2)
write_equation_ridge(c0, c1, c2)

# LASSO
# dev1 <- 1/64*(41-12*sqrt(3))
dev1 <- 10768094850759922072879/33946139684317548575040
a1 <- 2 * sqrt(dev1)
b1 <- 1 * sqrt(dev1)

# Ridge
# dev2 <- 0.309895
dev2 <- 0.289349
a2 <- 2 * sqrt(dev2)
b2 <- 1 * sqrt(dev2)



p_ridge <- tibble(
  x = 0,
  y = 0
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_segment(
    x = -Inf, y = 0, xend = +Inf, yend = 0,
    arrow = arrow(length=unit(0.30,"cm"), type = "closed")
  ) +
  geom_segment(
    x = 0, y = -Inf, xend = 0, yend = Inf,
    arrow = arrow(length=unit(0.30,"cm"), type = "closed")
  ) +
  geom_point(
    data = tibble(x = x0, y = y0),
    mapping = aes(x = x, y = y),
    size = 3,
    alpha = .8,
    color = "grey20"
  ) +
  geom_text(
    data = tibble(
      x = x0, y = y0, label = expression(hat(beta)[ML])
    ),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_x = 0.05
  ) +
  geom_text(
    data = tibble(
      x = 3, y = 0, label = expression(hat(beta)[1])
    ),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_y = 0.11, nudge_x = 0.05
  ) +
  geom_text(
    data = tibble(
      x = 0, y = 1.2, label = expression(hat(beta)[2])
    ),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_x = 0.05
  ) +
  geom_ellipse(
    data = tibble(
      x = rep(x0, 2),
      y = rep(y0, 2),
      x0 = rep(x0, 2),
      y0 = rep(y0, 2),
      a = c(a/5, a/3),
      b = c(b/5, b/3),
      angle = rep(angle, 2)
    ),
    mapping = aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
    color = "grey20"
  ) +
  # Ridge
  geom_ellipse(
    aes(x0 = 0, y0 = 0, a = 1, b = 1, angle = 0),
    fill = "grey20",
    alpha = .5
  ) +
  # Ridge
  geom_ellipse(
    aes(x0 = x0, y0 = y0, a = a2, b = b2, angle = angle),
    color = col1
  ) +
  geom_point(
    data = tibble(x = 0.988412, y = 0.151795),
    mapping = aes(x = x, y = y),
    size = 3,
    alpha = .8,
    color = col1
  ) +
  geom_text(
    data = tibble(x = 0.988412, y = 0.151795, label = expression(hat(beta)[Ridge])),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_x = 0.05,
    color = col1
  ) +
  coord_equal(
    xlim = c(-1.1, 2.9),
    ylim = c(-1.1, 1.1),
    clip = "off"
  ) +
  easy_remove_axes(
    which = "both",
    what = c("ticks", "title", "text", "line")
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    panel.border = element_blank()
  )


p_lasso <- tibble(
  x = 0,
  y = 0
) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_segment(
    x = -Inf, y = 0, xend = +Inf, yend = 0,
    arrow = arrow(length=unit(0.30,"cm"), type = "closed")
  ) +
  geom_segment(
    x = 0, y = -Inf, xend = 0, yend = Inf,
    arrow = arrow(length=unit(0.30,"cm"), type = "closed")
  ) +
  geom_point(
    data = tibble(x = x0, y = y0),
    mapping = aes(x = x, y = y),
    size = 3,
    alpha = .8,
    color = "grey20"
  ) +
  geom_text(
    data = tibble(
      x = x0, y = y0, label = expression(hat(beta)[ML])
    ),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_x = 0.05
  ) +
  geom_text(
    data = tibble(
      x = 3, y = 0, label = expression(hat(beta)[1])
    ),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_y = 0.11, nudge_x = 0.05
  ) +
  geom_text(
    data = tibble(
      x = 0, y = 1.2, label = expression(hat(beta)[2])
    ),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_x = 0.05
  ) +
  geom_ellipse(
    data = tibble(
      x = rep(x0, 2),
      y = rep(y0, 2),
      x0 = rep(x0, 2),
      y0 = rep(y0, 2),
      a = c(a/5, a/3),
      b = c(b/5, b/3),
      angle = rep(angle, 2)
    ),
    mapping = aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle),
    color = "grey20"
  ) +
  # LASSO
  geom_polygon(
    data = tibble(
      x = c(1, 0, -1, 0),
      y = c(0, 1, 0, -1)
    ),
    alpha = .5,
    size = 0
  ) +
  geom_path(
    data = tibble(
      x = c(1, 0, -1, 0, 1),
      y = c(0, 1, 0, -1, 0),
    ),
  ) +
  # LASSO
  geom_ellipse(
    aes(x0 = x0, y0 = y0, a = a1, b = b1, angle = angle),
    color = col2
  ) +
  geom_point(
    data = tibble(x = 1, y = 0),
    mapping = aes(x = x, y = y),
    size = 3,
    alpha = .8,
    color = col2
  ) +
  geom_text(
    data = tibble(x = 1, y = 0, label = expression(hat(beta)[LASSO])),
    mapping = aes(x = x, y = y, label = label),
    parse = T, hjust = 0, nudge_x = 0.05,
    color = col2
  ) +
  coord_equal(
    xlim = c(-1.1, 2.9),
    ylim = c(-1.1, 1.1),
    clip = "off"
  ) +
  easy_remove_axes(
    which = "both",
    what = c("ticks", "title", "text", "line")
  ) +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    panel.border = element_blank()
  )


p_ridge
p_lasso








