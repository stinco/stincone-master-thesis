#####
# 3. Prova Tables
# Leonardo Stincone
# 7/12/2020
# R 4.0.2
#####

# Libraries ####

library(tidyverse)
library(knitr)
library(kableExtra)
library(lubridate)

theme_set(theme_bw())


# Table tials ####

table <- tibble(
  make = c("Fiat", "Alfa-Romeo", "Lancia", "Ferrari"),
  z1 = c(1, 0, 0, 0),
  z2 = c(0, 1, 0, 0),
  z3 = c(0, 0, 1, 0)
)

table %>% 
  kable(
    # format = "latex",
    booktabs = T,
    align = "lrrr",
    vline = "",
    toprule = "", midrule = "\\hline",
    linesep = "", bottomrule = "",
    caption = "Dummy variables encoding.",
    label = "dummy-variables"
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = FALSE
  )


table <- tibble(
  Distribution = c("Normal", "Poisson", "Gamma", "Scaled Binomial"),
  Notation = c("$N(\\mu, \\sigma^2), \\\\ \\mu\\in\\mathbb{R}, \\ \\sigma>0$",
               "$Poisson(\\mu), \\\\ \\mu>0$",
               "$Gamma(\\alpha, \\mu), \\\\ \\alpha>0, \\ \\mu>0$",
               "$Binom(n, p)/n \\\\ n\\in\\mathbb{N}, \\ p\\in]0,1[$"),
  `$\\Theta$` = c("$\\mathbb{R}$",
                  "$\\mathbb{R}$",
                  "$]-\\infty, 0[$",
                  "$\\mathbb{R}$"),
  `$\\theta$` = c("$\\mu$",
                  "$\\log{(\\mu)}$",
                  "$-\\frac{1}{\\mu}$",
                  "$\\log{\\frac{p}{1-p}}$"),
  `$\\Lambda$` = c("$]0, +\\infty[$",
                  "$\\left\\{1\\right\\}$",
                  "$]0,+\\infty[$",
                  "$\\left\\{\\frac{1}{n}\\right\\}$"),
  `$\\lambda$` = c("$\\sigma^2$",
                  "$1$",
                  "$\\frac{1}{\\alpha}$",
                  "$\\frac{1}{n}$"),
  `$b(\\theta)$` = c("$\\frac{\\theta^2}{2}$",
                  "$e^{\\theta}$",
                  "$-\\log{\\left(-\\theta\\right)}$",
                  "$\\log\\left(1+e^{\\theta}\\right)$"),
)



table %>% 
  kable(
    # format = "latex",
    booktabs = T,
    align = "llccccc",
    vline = "",
    toprule = "", midrule = "\\hline",
    linesep = "", bottomrule = "",
    caption = "Some Exponential Families",
    label = "exp-families",
    escape = FALSE
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = FALSE
  )







table <- tibble(
  Distribution = linebreak(c("Normal", "Poisson", "Gamma", "Scaled\nBinomial"),
                           align = "l"),
  Notation = linebreak(c("$N(\\mu, \\sigma^2)$,\n$\\mu\\in\\mathbb{R}, \\ \\sigma>0$",
                         "$Poisson(\\mu)$,\n$\\mu>0$",
                         "$Gamma(\\alpha, \\mu)$,\n$\\alpha>0, \\ \\mu>0$",
                         "$Binom(n, p)/n$,\n$n\\in\\mathbb{N}, \\ p\\in]0,1[$"),
                       align = "c"),
  `$\\Theta$` = c("$\\mathbb{R}$",
                  "$\\mathbb{R}$",
                  "$]-\\infty, 0[$",
                  "$\\mathbb{R}$"),
  `$\\theta$` = c("$\\mu$",
                  "$\\log{(\\mu)}$",
                  "$-\\frac{1}{\\mu}$",
                  "$\\log{\\frac{p}{1-p}}$"),
  `$\\Lambda$` = c("$]0, +\\infty[$",
                  "$\\left\\{1\\right\\}$",
                  "$]0,+\\infty[$",
                  "$\\left\\{\\frac{1}{n}\\right\\}$"),
  `$\\lambda$` = c("$\\sigma^2$",
                  "$1$",
                  "$\\frac{1}{\\alpha}$",
                  "$\\frac{1}{n}$"),
  `$b(\\theta)$` = c("$\\frac{\\theta^2}{2}$",
                  "$e^{\\theta}$",
                  "$-\\log{\\left(-\\theta\\right)}$",
                  "$\\log\\left(1+e^{\\theta}\\right)$"),
)

table %>% 
  kable(
    # format = "latex",
    booktabs = T,
    align = "lcccccc",
    vline = "",
    # toprule = "", midrule = "\\midrule",
    # linesep = "\\\\[-1em]", bottomrule = "",
    # toprule = "", midrule = "\\toprule\\addlinespace",
    # linesep = "\\addlinespace\\hline\\addlinespace", bottomrule = "",
    toprule = "", midrule = "\\midrule\\addlinespace",
    linesep = "\\addlinespace\\addlinespace", bottomrule = "",
    caption = "Some Exponential Families",
    label = "exp-families",
    escape = FALSE
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = FALSE
  ) %>% 
  row_spec(0, bold = T)




# dummy variables table ####

table <- tibble(
  Make = c("Fiat", "Alfa-Romeo", "Lancia", "Ferrari"),
  `$z_1$` = c(1, 0, 0, 0),
  `$z_2$` = c(0, 1, 0, 0),
  `$z_3$` = c(0, 0, 1, 0)
)

table %>% 
  kable(
    # format = "latex",
    booktabs = T,
    align = "lccc",
    vline = "",
    # toprule = "", midrule = "\\hline",
    # linesep = "", bottomrule = "",
    toprule = "", midrule = "\\toprule\\addlinespace",
    linesep = "", bottomrule = "",
    caption = "Dummy variables encoding.",
    label = "dummy-variables",
    escape = FALSE
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = FALSE
  ) %>% 
  row_spec(0, bold = T)







# dmy("13/11/2020") - (dmy("8/12/2020") - dmy("13/11/2020"))
# dmy("13/11/2020") - dmy("6/10/2020")
# dmy("13/11/2020") + 38




# Canonical function ####


table <- tibble(
  Distribution = linebreak(c("Normal", "Poisson", "Gamma", "Scaled\nBinomial"),
                           align = "l"),
  `\\makecell[c]{Cumulant function\\\\$b(\\theta)$}` = c("$\\frac{\\theta^2}{2}$",
                     "$e^{\\theta}$",
                     "$-\\log{\\left(-\\theta\\right)}$",
                     "$\\log\\left(1+e^{\\theta}\\right)$"),
  `$b'(\\theta)$` = c("$\\theta$",
                      "$e^\\theta$",
                      "$-\\frac{1}{\\theta}$",
                      "$\\frac{e^{\\theta}}{1 + e^{\\theta}}$"),
  `$g(\\mu)=b'^{-1}(\\mu)$` = c("$\\mu$",
                                  "$\\log{(\\mu)}$",
                                  "$-\\frac{1}{\\mu}$",
                                  "$\\log{\\left( \\frac{p}{1-p} \\right)}$"),
)

table %>% 
  kable(
    # format = "latex",
    booktabs = T,
    align = "lcccccc",
    vline = "",
    # toprule = "", midrule = "\\midrule",
    # linesep = "\\\\[-1em]", bottomrule = "",
    toprule = "", midrule = "\\toprule\\addlinespace",
    linesep = "\\addlinespace\\hline\\addlinespace", bottomrule = "",
    # toprule = "", midrule = "\\midrule\\addlinespace",
    # linesep = "\\addlinespace\\addlinespace", bottomrule = "",
    caption = "Canonical link functions.",
    label = "can-link-fun",
    escape = FALSE
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = FALSE
  ) %>% 
  row_spec(1, bold = T)



















