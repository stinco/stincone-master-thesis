library(tidyverse)
library(knitr)
library(kableExtra)

table <- tibble(
  make = c("Fiat", "Alfa-Romeo", "Lancia", "Ferrari"),
  z1 = c(1, 0, 0, 0),
  z2 = c(0, 1, 0, 0),
  z3 = c(0, 0, 1, 0)
)

table %>% 
  kable(
    format = "latex",
    align = "lrrr",
    vline = "",
    toprule = "", midrule = "\\hline",
    linesep = "", bottomrule = "",
    caption = "Dummy variable encoding.",
    label = "dummy-variables"
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position"
  )










