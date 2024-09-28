# Common packages
suppressPackageStartupMessages({
  library("here")
  library("tidyverse")
  library("knitr")
  library("markdown")
  library("scales")
  library("psych")
  library("lavaan")
  library("semPlot")
  library("semTools")
  library("patchwork")
  library("gridExtra")
  library("bayesplot")
  library("ggExtra")
  library("ggpubr")
  library("viridis")
  library("ggokabeito")
})

theme_set(bayesplot::theme_default(base_size = 18, base_family = "sans"))

# knitr chunk options ----------------------------------------------------------

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  message = FALSE,
  warning = FALSE,
  error = FALSE,
  tidy = "styler"
)

# dplyr options ----------------------------------------------------------------

options(repr.plot.width = 8, repr.plot.height = 8 / 1.618033988749895)

set.seed(42)
