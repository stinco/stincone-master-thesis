#####
# 9. B-splines
# Leonardo Stincone
# 12/02/2021
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



# Pauli ####

intnodi = seq(0, 1, length = 7)
aggnodi = c(seq(-0.5, -0.1, length = 4),
            seq(1.1, 1.5, length = 4))
par(mar = c(3, 0, 0, 0), mfrow = c(1, 1))
plot(c(aggnodi, intnodi), rep(0, length = 15),
     xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", bty = "n",
     pch = "|", xlim = c(-0.7, 1.7))
abline(h = 0)
text(intnodi, rep(-0.4, length(intnodi)),
     labels = c(expression(tau[1]), expression(tau[2]),
                "...", "...", "...",
                expression(tau[K - 1]), expression(tau[K])),
     cex = 0.7)
text(aggnodi, rep(-0.4, length(intnodi)),
     labels = c(expression(xi[1]), "...", expression(xi[M]),
                "a", "b",
                expression(nu[1]), "...", expression(nu[M])),
     cex = 0.7)
text(aggnodi, rep(-0.8, length(aggnodi)),
     labels = c(expression(kappa[1]), "...", expression(kappa[M]),
                "", "",
                expression(kappa[K + M + 1]), "...", expression(kappa[K + 2*M])),
     cex = 0.7)
text(intnodi, rep(-0.8, length(intnodi)),
     labels = c(expression(kappa[M + 1]), expression(kappa[M + 2]),
                "...", "...", "...",
                expression(kappa[M + K - 1]), expression(kappa[M + K])),
     cex = 0.7)




nodi = c(-1, 0, 0.5, 1)
par(mfrow = c(1, 3), mar = c(3, 3, 2, 0.2))
x = lidar$range
Xtps = outer(x, nodi, FUN = function(x, y) abs(x-y)^3)

Xtps[1:5, 1:4]

library(splines)

base = bs(x, knots = nodi, degree = 1, intercept = TRUE)
matplot(x, base, type = "l", xlab = "range (standardized)", main = "1st degree B-splines", lty = 1)
points(nodi, rep(0, length(nodi)), pch = 20)

base = bs(x, knots = nodi, degree = 2, intercept = TRUE)
matplot(x, base, type = "l", xlab = "range (standardized)", main = "2nd degree B-splines", lty = 1)
points(nodi, rep(0, length(nodi)), pch = 20)

base = bs(x, knots = nodi, degree = 3, intercept = TRUE)
matplot(x, base, type = "l", xlab = "range (standardized)", main = "3rd degree B-splines", lty = 1)
points(nodi, rep(0, length(nodi)), pch = 20)



knots <- c(1,2,3)
x <- seq(from = 0, to = 4, by = .01)

base_1 = bs(x, knots = knots, degree = 1, intercept = TRUE)

p_b_splines_1 <- cbind(x, base_1) %>% 
        as_tibble() %>% 
        pivot_longer(
                cols = -x,
                names_to = "base",
                values_to = "value"
        ) %>% 
        ggplot(aes(x = x, y = value, color = base)) +
        geom_line() +
        easy_remove_axes(
                which = "y",
                what = "text",
                teach = FALSE
        ) +
        easy_remove_legend() +
        scale_x_continuous(
                breaks = 0:4,
                labels = c(
                        expression(nu[1]),
                        expression(nu[2]),
                        expression(nu[3]),
                        expression(nu[4]),
                        expression(nu[5])
                )
        )


base_2 = bs(x, knots = knots, degree = 2, intercept = TRUE)

p_b_splines_2 <- cbind(x, base_2) %>% 
        as_tibble() %>% 
        pivot_longer(
                cols = -x,
                names_to = "base",
                values_to = "value"
        ) %>% 
        ggplot(aes(x = x, y = value, color = base)) +
        geom_line() +
        easy_remove_axes(
                which = "y",
                what = "text",
                teach = FALSE
        ) +
        easy_remove_legend() +
        scale_x_continuous(
                breaks = 0:4,
                labels = c(
                        expression(nu[1]),
                        expression(nu[2]),
                        expression(nu[3]),
                        expression(nu[4]),
                        expression(nu[5])
                )
        )


base_3 = bs(x, knots = knots, degree = 3, intercept = TRUE)

p_b_splines_3 <- cbind(x, base_3) %>% 
        as_tibble() %>% 
        pivot_longer(
                cols = -x,
                names_to = "base",
                values_to = "value"
        ) %>% 
        ggplot(aes(x = x, y = value, color = base)) +
        geom_line() +
        easy_remove_axes(
                which = "y",
                what = "text",
                teach = FALSE
        ) +
        easy_remove_legend() +
        scale_x_continuous(
                breaks = 0:4,
                labels = c(
                        expression(nu[1]),
                        expression(nu[2]),
                        expression(nu[3]),
                        expression(nu[4]),
                        expression(nu[5])
                )
        )








