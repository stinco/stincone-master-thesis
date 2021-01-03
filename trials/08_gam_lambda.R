#####
# 8. GAM lambda
# Leonardo Stincone
# 3/01/2021
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



# Trial 1 ####

lidar


# Hyper-parameters choice

k <- 50

# sp1 <- 0
# sp2 <- 1e-4
# sp3 <- 1e-2
# sp4 <- 10

sp1 <- 0
sp2 <- 10
sp3 <- 1e3
sp4 <- 1e6




# Model fitting

## Attention, the default basis is bs='tp'
# tp: thin plate regression splines
# cr: natural cubic regression spline
# bs: b-slines

gam1 <- gam(formula = as.formula(str_c("logratio ~ s(range, bs = 'cr', k = ", k, ")")),
            sp = sp1,
            data = lidar,
            family = gaussian())

gam2 <- gam(formula = as.formula(str_c("logratio ~ s(range, bs = 'cr', k = ", k, ")")),
            sp = sp2,
            data = lidar,
            family = gaussian())

gam3 <- gam(formula = as.formula(str_c("logratio ~ s(range, bs = 'cr', k = ", k, ")")),
            sp = sp3,
            data = lidar,
            family = gaussian())

gam4 <- gam(formula = as.formula(str_c("logratio ~ s(range, bs = 'cr', k = ", k, ")")),
            sp = sp4,
            data = lidar,
            family = gaussian())



gam1$smooth[[1]]$xp  ## extract knots locations

str(gam1)
str(gam1$smooth)
str(gam1$smooth[[1]])

gam1$smooth[[1]]$xp
# gam2$smooth[[1]]$xp
# gam3$smooth[[1]]$xp
# gam4$smooth[[1]]$xp




# plot(gam1)

# lidar %>% 
#   ggplot(aes(x = range, y = logratio)) +
#   geom_point()


# Predictions on new data

new_lidar <- tibble(range = seq(from = min(lidar$range),
                                to = max(lidar$range),
                                length.out = 1e3))

new_lidar$fit1 <- predict(gam1, newdata = new_lidar)
new_lidar$fit2 <- predict(gam2, newdata = new_lidar)
new_lidar$fit3 <- predict(gam3, newdata = new_lidar)
new_lidar$fit4 <- predict(gam4, newdata = new_lidar)


new_lidar_long <- new_lidar %>% 
  pivot_longer(
    cols = fit1:fit4,
    names_to = "model"
  ) %>% 
  mutate(
    lambda = case_when(
      model == "fit1" ~ sp1,
      model == "fit2" ~ sp2,
      model == "fit3" ~ sp3,
      model == "fit4" ~ sp4
    ),
    lambda_label = str_c("lambda = ", lambda) %>% 
      factor(levels = str_c("lambda = ", c(sp1, sp2, sp3, sp4)))
  )


# Plotting results

lidar %>% 
  ggplot(aes(x = range, y = logratio)) +
  # geom_vline(data = tibble(knots = gam1$smooth[[1]]$xp),
  #            aes(xintercept = knots),
  #            linetype = "dotted",
  #            alpha = .5) +
  geom_line(
    data = new_lidar_long,
    aes(x = range, y = value, color = lambda_label),
    # col = col1,
    size = line_size
  ) +
  geom_point() +
  facet_wrap(.~lambda_label) +
  easy_remove_legend(
    teach = F
  )




# Trial 2 ####

# Hyperparameters
k <- 50

sp1 <- 0
sp2 <- 10
sp3 <- 1e3
sp4 <- 1e6


# Plotting function
plot_gam_lidar <- function(dataset, k, sp){
  
  # Model fitting
  
  ## Attention, the default basis is bs='tp'
  # tp: thin plate regression splines
  # cr: natural cubic regression spline
  # bs: b-slines
  
  gam1 <- gam(formula = as.formula(str_c("logratio ~ s(range, bs = 'cr', k = ", k, ")")),
              sp = sp,
              data = dataset,
              family = gaussian())
  
  
  # Predictions on new data
  new_dataset <- tibble(range = seq(from = min(dataset$range),
                                  to = max(dataset$range),
                                  length.out = 1e3))
  
  new_dataset$fit1 <- predict(gam1, newdata = new_dataset)
  
  
  # Plotting results
  p <- dataset %>% 
    ggplot(aes(x = range, y = logratio)) +
    geom_line(
      data = new_dataset,
      aes(x = range, y = fit1),
      col = col1,
      size = line_size
    ) +
    geom_point(alpha = .5) +
    easy_remove_legend(
      teach = F
    )
  
  return(p)
  
}

p_gam_lambda_1 <- plot_gam_lidar(dataset = lidar, k = k, sp = sp1)
p_gam_lambda_2 <- plot_gam_lidar(dataset = lidar, k = k, sp = sp2)
p_gam_lambda_3 <- plot_gam_lidar(dataset = lidar, k = k, sp = sp3)
p_gam_lambda_4 <- plot_gam_lidar(dataset = lidar, k = k, sp = sp4)


plot_grid_split(p_gam_lambda_1, p_gam_lambda_2, p_gam_lambda_3, p_gam_lambda_4)



