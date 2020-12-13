#####
# 5. Prova Tables html
# Leonardo Stincone
# 13/12/2020
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



if(!knitr::is_latex_output()){
  
  table <- table %>% 
    mutate(
      Distribution = Distribution %>% 
        str_replace_all("\\\\makecell\\[[lrc]\\]\\{(.*)\\}", "\\1") %>% 
        str_replace_all("\\\\\\\\", "<br>"),
      Notation = Notation %>% 
        str_replace_all("\\\\makecell\\[[lrc]\\]\\{(.*)\\}", "\\1") %>% 
        str_replace_all("\\\\\\\\", "<br>")
    )
  
  names(table) <- names(table) %>% 
    str_replace_all("\\\\makecell\\[[lrc]\\]\\{(.*)\\}", "\\1") %>% 
    str_replace_all("\\\\\\\\", "<br>")
}


# table$Distribution %>%
#   str_replace_all("\\\\makecell\\[[lrc]\\]\\{(.*)\\}", "\\1") %>% 
#   str_replace_all("\\\\\\\\", "\n")






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
    caption = "Some Linear Exponential Families.",
    label = "exp-families",
    escape = FALSE
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = FALSE
  ) %>% 
  row_spec(1, bold = T)
