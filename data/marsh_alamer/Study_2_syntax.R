##The Set-ESEM model For Study 2. Separating Time 1 and Time 2 constructs into mini ESEM blocks.

library(lavaan)

## import example data from plain-text CSV file
study2_dat <- rio::import(
  here::here(
    "data", "marsh_alamer", "Study_2_data.csv"
    )
  )


## ----------
## (Set-)ESEM
## ----------

## ESEM whose results model-fit results are also reported in Table 3 and
## whose standardized coefficients are partially reported in Table 4

esem2.config <- '

  # The measurement model
  # WITHOUT equality constraints on factor loadings
  
  # Time 1 Set-ESEM
  efa("time1")*SelfConfidenceT1 =~ SelfConf1T1 + SelfConf2T1 + SelfConf3T1 + SelfConf4T1 + Intr1T1 + Intr2T1 + Intr3T1 + Auton1T1 + Auton2T1 + Auton3T1 + Auton4T1
  efa("time1")*IntrinsicT1 =~ SelfConf1T1 + SelfConf2T1 + SelfConf3T1 + SelfConf4T1 + Intr1T1 + Intr2T1 + Intr3T1 + Auton1T1 + Auton2T1 + Auton3T1 + Auton4T1
  efa("time1")*AutonomyT1 =~ SelfConf1T1 + SelfConf2T1 + SelfConf3T1 + SelfConf4T1 + Intr1T1 + Intr2T1 + Intr3T1 + Auton1T1 + Auton2T1 + Auton3T1 + Auton4T1
  
  # Time 2 Set-ESEM
  efa("time2")*SelfConfidenceT2 =~ SelfConf1T2 + SelfConf2T2 + SelfConf3T2 + SelfConf4T2 + Intr1T2 + Intr2T2 + Intr3T2 + Auton1T2 + Auton2T2 + Auton3T2 + Auton4T2
  efa("time2")*IntrinsicT2 =~ SelfConf1T2 + SelfConf2T2 + SelfConf3T2 + SelfConf4T2 + Intr1T2 + Intr2T2 + Intr3T2 + Auton1T2 + Auton2T2 + Auton3T2 + Auton4T2
  efa("time2")*AutonomyT2 =~ SelfConf1T2 + SelfConf2T2 + SelfConf3T2 + SelfConf4T2 + Intr1T2 + Intr2T2 + Intr3T2 + Auton1T2 + Auton2T2 + Auton3T2 + Auton4T2
  
  # The structural model
  SelfConfidenceT2 ~ SelfConfidenceT1
  IntrinsicT2 ~ IntrinsicT1
  AutonomyT2 ~ AutonomyT1
  L2_achievement ~ SelfConfidenceT1 + IntrinsicT1 + AutonomyT1 + SelfConfidenceT2 + IntrinsicT2 + AutonomyT2
  
  # Residual correlations
  SelfConf1T2 ~~ SelfConf1T1
  SelfConf2T2 ~~ SelfConf2T1
  SelfConf3T2 ~~ SelfConf3T1
  SelfConf4T2 ~~SelfConf4T1
  
  Auton1T2 ~~ Auton1T1
  Auton2T2 ~~ Auton2T1
  Auton3T2 ~~ Auton3T1
  Auton4T2 ~~ Auton4T1
  
  Intr1T2 ~~ Intr1T1
  Intr2T2 ~~ Intr2T1
  Intr3T2 ~~ Intr3T1
'

out2.config <- sem(
  model = esem2.config, 
  data = study2_dat,
  estimator = "MLR", # verbose = TRUE,
  rotation = "geomin", 
  rotation.args = list(geomin.epsilon = 0.005)
)

summary(out2.config, standardized = TRUE, fit.measures = TRUE)


## Metric-invariance (equality constraints on primary loadings), whose 
## model-fit results are also reported in Table 3
esem2.metric <- '

  # The measurement model
  # WITH equality constraints on factor loadings
  
  # Time 1 Set-ESEM
  efa("time1")*SelfConfidenceT1 =~ a*SelfConf1T1 + b*SelfConf2T1 + c*SelfConf3T1 + d*SelfConf4T1 + e*Intr1T1 + f*Intr2T1 + g*Intr3T1 + h*Auton1T1 + i*Auton2T1 + j*Auton3T1 + k*Auton4T1
  efa("time1")*IntrinsicT1 =~ aa*SelfConf1T1 + bb*SelfConf2T1 + cc*SelfConf3T1 + dd*SelfConf4T1 + ee*Intr1T1 + ff*Intr2T1 + gg*Intr3T1 + hh*Auton1T1 + ii*Auton2T1 + jj*Auton3T1 + kk*Auton4T1
  efa("time1")*AutonomyT1 =~ aaa*SelfConf1T1 + bbb*SelfConf2T1 + ccc*SelfConf3T1 + ddd*SelfConf4T1 + eee*Intr1T1 + fff*Intr2T1 + ggg*Intr3T1 + hhh*Auton1T1 + iii*Auton2T1 + jjj*Auton3T1 + kkk*Auton4T1
  
  # Time 2 Set-ESEM
  efa("time2")*SelfConfidenceT2 =~ a*SelfConf1T2 + b*SelfConf2T2 + c*SelfConf3T2 + d*SelfConf4T2 + e*Intr1T2 + f*Intr2T2 + g*Intr3T2 + h*Auton1T2 + i*Auton2T2 + j*Auton3T2 + k*Auton4T2
  efa("time2")*IntrinsicT2 =~ aa*SelfConf1T2 + bb*SelfConf2T2 + cc*SelfConf3T2 + dd*SelfConf4T2 + ee*Intr1T2 + ff*Intr2T2 + gg*Intr3T2 + hh*Auton1T2 + ii*Auton2T2 + jj*Auton3T2 + kk*Auton4T2
  efa("time2")*AutonomyT2 =~ aaa*SelfConf1T2 + bbb*SelfConf2T2 + ccc*SelfConf3T2 + ddd*SelfConf4T2 + eee*Intr1T2 + fff*Intr2T2 + ggg*Intr3T2 + hhh*Auton1T2 + iii*Auton2T2 + jjj*Auton3T2 + kkk*Auton4T2
  
  # Free factors variances at Time 2
  SelfConfidenceT2 ~~ NA*SelfConfidenceT2
  IntrinsicT2 ~~ NA*IntrinsicT2
  AutonomyT2 ~~ NA*AutonomyT2
  
  # The structural model
  SelfConfidenceT2 ~ SelfConfidenceT1
  IntrinsicT2 ~ IntrinsicT1
  AutonomyT2 ~ AutonomyT1
  L2_achievement ~ SelfConfidenceT1 + IntrinsicT1 + AutonomyT1 + SelfConfidenceT2 + IntrinsicT2 + AutonomyT2
  
  # Residual correlations
  SelfConf1T2 ~~ SelfConf1T1
  SelfConf2T2 ~~ SelfConf2T1
  SelfConf3T2 ~~ SelfConf3T1
  SelfConf4T2 ~~SelfConf4T1
  
  Auton1T2 ~~ Auton1T1
  Auton2T2 ~~ Auton2T1
  Auton3T2 ~~ Auton3T1
  Auton4T2 ~~ Auton4T1
  
  Intr1T2 ~~ Intr1T1
  Intr2T2 ~~ Intr2T1
  Intr3T2 ~~ Intr3T1

'

out2.metric <- sem(
  model = esem2.metric, data = study2_dat,
  estimator = "MLR", # verbose = TRUE,
  rotation = "geomin", rotation.args = list(geomin.epsilon = 0.005)
)
summary(out2.metric, standardized = TRUE, fit.measures = TRUE)




## --------------------
## Simple-structure CFA
## --------------------

## NOTE: These models are not recommended because they are overly restrictive.
##       CFA results were only provided to serve as a comparison with ESEM.
##       This syntax is only provided in order to reproduce results reported
##       in Tables 3 and 4 of the ESEM tutorial.


## CFA whose results model-fit results are also reported in Table 3 and
## whose standardized coefficients are partially reported in Table 4
cfa2.config <- '

  # The measurement model
  # WITHOUT equality constraints on factor loadings
  
  # Time 1 Set-ESEM
  SelfConfidenceT1 =~ SelfConf1T1 + SelfConf2T1 + SelfConf3T1 + SelfConf4T1
  IntrinsicT1      =~     Intr1T1 +     Intr2T1 +     Intr3T1
  AutonomyT1       =~    Auton1T1 +    Auton2T1 +    Auton3T1 +    Auton4T1
  
  # Time 2 Set-ESEM
  SelfConfidenceT2 =~ SelfConf1T2 + SelfConf2T2 + SelfConf3T2 + SelfConf4T2
  IntrinsicT2      =~     Intr1T2 +     Intr2T2 +     Intr3T2
  AutonomyT2       =~    Auton1T2 +    Auton2T2 +    Auton3T2 +    Auton4T2
  
  ## The structural model
  SelfConfidenceT2 ~ SelfConfidenceT1
  IntrinsicT2 ~ IntrinsicT1
  AutonomyT2 ~ AutonomyT1
  L2_achievement ~ SelfConfidenceT1 + IntrinsicT1 + AutonomyT1 + SelfConfidenceT2 + IntrinsicT2 + AutonomyT2
  
  # Residual correlations
  SelfConf1T2 ~~ SelfConf1T1
  SelfConf2T2 ~~ SelfConf2T1
  SelfConf3T2 ~~ SelfConf3T1
  SelfConf4T2 ~~SelfConf4T1
  
  Auton1T2 ~~ Auton1T1
  Auton2T2 ~~ Auton2T1
  Auton3T2 ~~ Auton3T1
  Auton4T2 ~~ Auton4T1
  
  Intr1T2 ~~ Intr1T1
  Intr2T2 ~~ Intr2T1
  Intr3T2 ~~ Intr3T1
  
  # Residual correlations among Time-2 factors
  # (not automatically estimated due to being predictors as well,
  #  but ESEM rotation allows their covariances to be nonzero)
  SelfConfidenceT2 ~~ IntrinsicT2 + AutonomyT2
  IntrinsicT2      ~~ AutonomyT2

'

fit2.config <- sem(
  model = cfa2.config, 
  data = study2_dat,
  estimator = "MLR", 
  std.lv = TRUE
)
summary(fit2.config, standardized = TRUE, fit.measures = TRUE)


## CFA with metric-invariance (equality constraints on primary loadings), whose 
## model-fit results are also reported in Table 3
cfa2.metric <- '

  # The measurement model
  # WITHOUT equality constraints on factor loadings
  
  # Time 1 Set-ESEM
  SelfConfidenceT1 =~ a*SelfConf1T1 + b*SelfConf2T1 + c*SelfConf3T1 + d*SelfConf4T1
  IntrinsicT1      =~     e*Intr1T1 +     f*Intr2T1 +     g*Intr3T1
  AutonomyT1       =~    h*Auton1T1 +    i*Auton2T1 +    j*Auton3T1 +    k*Auton4T1
  
  # Time 2 Set-ESEM
  SelfConfidenceT2 =~ a*SelfConf1T2 + b*SelfConf2T2 + c*SelfConf3T2 + d*SelfConf4T2
  IntrinsicT2      =~     e*Intr1T2 +     f*Intr2T2 +     g*Intr3T2
  AutonomyT2       =~    h*Auton1T2 +    i*Auton2T2 +    j*Auton3T2 +    k*Auton4T2
  
  # Free factors variances at Time 2
  SelfConfidenceT2 ~~ NA*SelfConfidenceT2
  IntrinsicT2 ~~ NA*IntrinsicT2
  AutonomyT2 ~~ NA*AutonomyT2
  
  # The structural model
  SelfConfidenceT2 ~ SelfConfidenceT1
  IntrinsicT2 ~ IntrinsicT1
  AutonomyT2 ~ AutonomyT1
  L2_achievement ~ SelfConfidenceT1 + IntrinsicT1 + AutonomyT1 + SelfConfidenceT2 + IntrinsicT2 + AutonomyT2
  
  # Residual correlations
  SelfConf1T2 ~~ SelfConf1T1
  SelfConf2T2 ~~ SelfConf2T1
  SelfConf3T2 ~~ SelfConf3T1
  SelfConf4T2 ~~SelfConf4T1
  
  Auton1T2 ~~ Auton1T1
  Auton2T2 ~~ Auton2T1
  Auton3T2 ~~ Auton3T1
  Auton4T2 ~~ Auton4T1
  
  Intr1T2 ~~ Intr1T1
  Intr2T2 ~~ Intr2T1
  Intr3T2 ~~ Intr3T1
  
  # Residual correlations among Time-2 factors
  # (not automatically estimated due to being predictors as well,
  #  but ESEM rotation allows their covariances to be nonzero)
  SelfConfidenceT2 ~~ IntrinsicT2 + AutonomyT2
  IntrinsicT2      ~~ AutonomyT2

'

fit2.metric <- sem(
  model = cfa2.metric, 
  data = study2_dat,
  estimator = "MLR", 
  std.lv = TRUE
)

summary(fit2.metric, standardized = TRUE, fit.measures = TRUE)

