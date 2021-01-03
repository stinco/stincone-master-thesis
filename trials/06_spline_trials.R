#####
# 6. Spline trials
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


# Read data ####
lidar = read.table("data/lidar.dat", header = TRUE) %>% 
  as_tibble()

lidar = lidar[sort.list(lidar$range), ]

# Standardizzo lidar$range
lidar$range = (lidar$range - mean(lidar$range)) / sd(lidar$range)


# Trial 1 ####
lidar

fbase = function(x, k, p){
  ifelse(x - k > 0, (x - k)^p, 0)
}
fbase1 = function(x, k, p){
  ifelse(x - k > 0, (x - k)^p, NA)
} 


# nodi = c(-1, 0, 1)
nodi = c(seq(min(lidar$range), max(lidar$range), by = 0.25))
p = 3

par(
  mar = c(0, 6, 0, 0),
  oma = c(2, 0, 0, 0),
  mfrow = c(length(nodi), 1)
)

for (i in nodi[-length(nodi)]){
  curve(fbase1(x, i, p),
        add = FALSE, lwd = 2, col = "blue",
        xlim = range(lidar$range),
        xaxt = "n", ylab = "", las = 1)
} 

curve(fbase1(x, nodi[length(nodi)], p),
      xlim = range(lidar$range),
      lwd = 2, col = "blue",
      ylab = "", las = 1)



i = 1




# Trial 2 ####

nodi = c(0, 1)
p = 1

par(mfrow = c(1, 1), mar = c(2, 4, 0.2, 0.2))
plot(lidar$range, lidar$logratio, pch = 20, xlab = "range (standardized)", ylab = "logratio")
fbase = function(x, k, p) ifelse(x-k>0, (x-k)^p, 0)
lidar1 = lidar
for (i in nodi){
  lidar1 = cbind(lidar1, fbase(lidar1$range, i, p))
  names(lidar1)[ncol(lidar1)] = paste("b", i, sep = "")
}
fit = lm(logratio~., data = lidar1)
lines(lidar$range, predict(fit), col = "red", lwd = 2)
rug(nodi, lwd = 4, col = "blue")

fbase1 = function(x, k, p) ifelse(x-k > 0, (x-k)^p, NA)
par(mar = c(0, 6, 0, 0), oma = c(2, 0, 0, 0), mfrow = c(length(nodi)+1, 1))
for (i in c(min(lidar$range), nodi[-length(nodi)])) curve(fbase1(x, i, p), add = FALSE, lwd = 2, col = "blue", xlim = range(lidar$range), xaxt = "n", ylab = "", las = 1)
curve(fbase1(x, nodi[length(nodi)], p), xlim = range(lidar$range), lwd = 2, col = "blue", ylab = "", las = 1)




# Pauli revisited ####


# Linear

fit <- lm(logratio~., data = lidar)

p_cub_spline_1 <- lidar %>% 
  ggplot(aes(x = range, y = logratio)) +
  geom_abline(
    intercept = fit$coefficients[1],
    slope = fit$coefficients[2],
    col = col1,
    size = line_size
  ) +
  geom_point()



# Cubic spline

fbase = function(x, k, p) ifelse(x - k > 0, (x - k)^p, 0)
p <- 3


plot_spline_lidar <- function(dataset, nodes, p = p){
  # Create basis for splines
  lidar1 <- dataset
  for (i in 1:length(nodes)){
    lidar1 <- lidar1 %>% 
      mutate(b = fbase(range, nodes[i], p))
    
    names(lidar1)[ncol(lidar1)] = paste("b", i, sep = "")
  }
  
  # Fit GLM
  fit <- lm(logratio~., data = lidar1)
  
  # Create fitted values
  new_lidar1 <- tibble(
    range = seq(from = min(lidar$range), to = max(lidar$range), length.out = 1e3)
  )
  for (i in 1:length(nodes)){
    new_lidar1 <- new_lidar1 %>% 
      mutate(b = fbase(range, nodes[i], p))
    
    names(new_lidar1)[ncol(new_lidar1)] = paste("b", i, sep = "")
  }
  
  new_lidar1$fit <- predict(fit, newdata = new_lidar1)
  
  
  p <- lidar1 %>% 
    ggplot(aes(x = range, y = logratio)) +
    geom_vline(data = tibble(nodes),
               aes(xintercept = nodes),
               linetype = "dotted") +
    geom_line(
      data = new_lidar1,
      aes(x = range, y = fit),
      col = col1,
      size = line_size
    ) +
    geom_point()
  
  return(p)
}


p_cub_spline_2 <- plot_spline_lidar(dataset = lidar,
                        nodes = c(-1, 0, 1),
                        p = 3)

p_cub_spline_3 <- plot_spline_lidar(dataset = lidar,
                        nodes = seq(from = -1.5, to = 1.5, by = .5),
                        p = 3)

p_cub_spline_4 <- plot_spline_lidar(dataset = lidar,
                        nodes = seq(from = -1.7, to = 1.7, by = .05),
                        p = 3)













