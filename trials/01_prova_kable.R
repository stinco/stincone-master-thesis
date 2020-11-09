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
    # format = "latex",
    align = "lrrr",
    vline = "",
    toprule = "", midrule = "\\hline",
    linesep = "", bottomrule = "",
    caption = "Dummy variable encoding.",
    label = "dummy-variables"
  ) %>% 
  kable_styling(
    position = "center",
    latex_options = "hold_position",
    full_width = F
  )


ose_ex <- tibble(
  Level = c("Advanced (Adv)", "Intermediate (Int)", "Elementary (Ele)"),
  Example = c("Amsterdam still looks liberal to tourists, who were recently assured by the Labour Mayor that the city’s marijuana-selling coffee shops would stay open despite a new national law tackling drug tourism. But the Dutch capital may lose its reputation for tolerance over plans to dispatch nuisance neighbours
to scum villages made from shipping containers.", "To tourists, Amsterdam still seems very liberal. Recently the city’s Mayor assured them that the city’s marijuana-selling coffee shops would stay open despite a new national law to prevent drug tourism. But the Dutch capitals plans to send nuisance neighbours to scum villages made from shipping containers may damage its reputation for tolerance.", "To tourists, Amsterdam still seems very liberal. Recently the city’s Mayor told them that the coffee shops that sell marijuana would stay open, although there is a new national law to stop drug tourism. But the Dutch capital has a plan to send antisocial neighbours to scum villages made from shipping containers, and so maybe now people wont think it is a liberal city any more.")
)


ose_ex

ose_ex %>% 
  knitr::kable(
    # format = "latex",
    booktabs = T,
    caption="An OSE Corpus passage at different reading levels.",
    col.names = c("Reading Level", "Example"),
    label = "ose-example",
    linesep = "\\addlinespace\\hline\\addlinespace"
  ) 


#%>% 
  kable_styling() %>%
  column_spec(2, width="29em") %>%
  row_spec(
    0,
    bold=T
  )













