##The Set-ESEM model For Study 1. Separating different sets of theoretical constructs into mini ESEM blocks. In this case, 
##we are separating the constructs targeting the perception about teacher from self-perception

library(tidyverse)
library(lavaan)
library(here)

study1_dat <- rio::import(
  here::here(
    "data", "marsh_alamer", "Study_1_data.csv"
    )
  )

esem1 <- '

  # the long format (more flexible) each factor is defined separately 
  efa("teacher")*Teacher_autonomy =~ T_autonomy1 + T_autonomy2 + T_autonomy3 + T_competence1 + T_competence2 + T_competence3 + T_relatedness1 + T_relatedness2 + T_relatedness3
  efa("teacher")*Teacher_competence =~ T_autonomy1 + T_autonomy2 + T_autonomy3 + T_competence1 + T_competence2 + T_competence3 + T_relatedness1 + T_relatedness2 + T_relatedness3
  efa("teacher")*Teacher_relatedness =~ T_autonomy1 + T_autonomy2 + T_autonomy3 + T_competence1 + T_competence2 + T_competence3 + T_relatedness1 + T_relatedness2 + T_relatedness3
  
  # the short format (less flexible) all factors defined in one instance (remove ”##” if you want to use this)
  # efa("teacher")*Teacher_autonomy + 
  # efa("teacher")*Teacher_competence + 
  # efa("teacher")*Teacher_relatedness =~ T_autonomy1 + T_autonomy2 + T_autonomy3 + T_competence1 + T_competence2 + T_competence3 + T_relatedness1 + T_relatedness2 + T_relatedness3
  
  # defining the second ESEM block
  efa("self")*Self_Meaning =~ S_meaning1 + S_meaning2 + S_meaning3 + S_confidence1 + S_confidence2 + S_confidence3 + S_Intrinsic1 + S_Intrinsic2+S_Intrinsic3
  efa("self")*Self_Confidence =~ S_meaning1 + S_meaning2 + S_meaning3 + S_confidence1 + S_confidence2 + S_confidence3 + S_Intrinsic1 + S_Intrinsic2+S_Intrinsic3
  efa("self")*Intrinsic_Motivation =~ S_meaning1 + S_meaning2 + S_meaning3 + S_confidence1 + S_confidence2 + S_confidence3 + S_Intrinsic1 + S_Intrinsic2+S_Intrinsic3
  
  # defining the outcome variable
  Intent_to_Quit =~ Intent_to_withdraw1 + Intent_to_withdraw2 + Intent_to_withdraw3 + Intent_to_withdraw4 + Intent_to_withdraw5
  
  # defining the structural part
  Self_Meaning ~ Teacher_autonomy + Teacher_competence + Teacher_relatedness
  Self_Confidence ~ Teacher_autonomy + Teacher_competence + Teacher_relatedness
  Intrinsic_Motivation ~ Teacher_autonomy + Teacher_competence + Teacher_relatedness
  Intent_to_Quit ~ Self_Meaning + Self_Confidence + Intrinsic_Motivation + 
                   Teacher_autonomy + Teacher_competence + Teacher_relatedness
'

# requesting the results
out1 <- sem(
  model = esem1, 
  data = study1_dat,
  estimator = "MLR", # verbose = TRUE, test = "yuan.bentler",
  rotation = "geomin", 
  rotation.args = list(geomin.epsilon = 0.005)
  )
summary(out1, standardized = TRUE, fit.measures = TRUE)
fitMeasures(out1)

semPlot::semPaths(
  out1,
  what = "col", whatLabels = "no", style = "mx", 
  layout = "tree", nCharNodes = 7,
  shapeMan = "rectangle", sizeMan = 5, sizeMan2 = 4
)


## --------------------
## Simple-structure CFA
## --------------------

## NOTE: This model is not recommended because it is overly restrictive.
##       CFA results were only provided to serve as a comparison with ESEM.
##       This syntax is only provided in order to reproduce results reported
##       in Tables 1 and 2 of the ESEM tutorial.

cfa1 <- ' ## Specify the measurement model

  # "teacher" factors
  Teacher_autonomy    =~    T_autonomy1 +    T_autonomy2 +    T_autonomy3
  Teacher_competence  =~  T_competence1 +  T_competence2 +  T_competence3
  Teacher_relatedness =~ T_relatedness1 + T_relatedness2 + T_relatedness3
  
  # "self" factors
  Self_Meaning         =~    S_meaning1 +    S_meaning2 +    S_meaning3
  Self_Confidence      =~ S_confidence1 + S_confidence2 + S_confidence3
  Intrinsic_Motivation =~  S_Intrinsic1 +  S_Intrinsic2 +  S_Intrinsic3
  
  # defining the outcome variable
  Intent_to_Quit =~ Intent_to_withdraw1 + Intent_to_withdraw2 + Intent_to_withdraw3 + Intent_to_withdraw4 + Intent_to_withdraw5
  
  # specify the structural model
  Self_Meaning ~ Teacher_autonomy + Teacher_competence + Teacher_relatedness
  Self_Confidence ~ Teacher_autonomy + Teacher_competence + Teacher_relatedness
  Intrinsic_Motivation ~ Teacher_autonomy + Teacher_competence + Teacher_relatedness
  Intent_to_Quit ~ Self_Meaning + Self_Confidence + Intrinsic_Motivation + 
                   Teacher_autonomy + Teacher_competence + Teacher_relatedness
  
  # residual covariances among mediating factors in Block 2 ("self")
  # (not automatically estimated due to being predictors as well,
  #  but ESEM rotation allows their covariances to be nonzero)
  Self_Meaning    ~~ Self_Confidence + Intrinsic_Motivation
  Self_Confidence ~~ Intrinsic_Motivation
'

# requesting the results
fit1 <- sem(model = cfa1, data = Study_1_data_for_sharing,
            estimator = "MLR", std.lv = TRUE)
summary(fit1, standardized = TRUE, fit.measures = TRUE)
fitMeasures(fit1)

