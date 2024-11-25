rm(list = ls()) # clear workspace

library(dplyr)
library(stringr)
library(paran)
library(lavaan)
library(semPlot)
library(lsr)
library(car)
library(pROC)
library(graphics)
library(qgraph)
library(effectsize)
library(psych)
library(reportROC)
library(semTools)

##################
## 1. Read data ##
##################

## Population sample
setwd("BLINDED") #set WD to data file location

data = read.csv2("data_total_OSF.csv"
                , header = T 
                , sep = "," 
                , stringsAsFactors = F) # read data

data$X <- NULL # delete column without information

data$SW_mean = as.numeric(data$SW_mean) # convert from character to numeric
data$SW_AD_mean = as.numeric(data$SW_AD_mean)
data$SW_HI_mean = as.numeric(data$SW_HI_mean)

data$CA_mean = as.numeric(data$CA_mean)
data$CA_AD_mean = as.numeric(data$CA_AD_mean)
data$CA_HI_mean = as.numeric(data$CA_HI_mean)

data$HA_mean = as.numeric(data$HA_mean)
data$HA_AD_mean = as.numeric(data$HA_AD_mean)
data$HA_HI_mean = as.numeric(data$HA_HI_mean)

## Clinical sample
setwd("BLINDED") #set WD to data file location

data_clin = read.csv2("data_clinical_OSF.csv"
                 , header = T 
                 , sep = "," 
                 , stringsAsFactors = F) # read data

data_clin$X <- NULL # delete column without information

data_clin$SW_mean = as.numeric(data_clin$SW_mean) # convert from character to numeric
data_clin$SW_AD_mean = as.numeric(data_clin$SW_AD_mean)
data_clin$SW_HI_mean = as.numeric(data_clin$SW_HI_mean)

data_clin$CA_mean = as.numeric(data_clin$CA_mean)
data_clin$CA_AD_mean = as.numeric(data_clin$CA_AD_mean)
data_clin$CA_HI_mean = as.numeric(data_clin$CA_HI_mean)

data_clin$HA_mean = as.numeric(data_clin$HA_mean)
data_clin$HA_AD_mean = as.numeric(data_clin$HA_AD_mean)
data_clin$HA_HI_mean = as.numeric(data_clin$HA_HI_mean)


# Information on missing data in the general population sample
SWAN_vars = colnames(data)[str_detect(colnames(data),'SW01')] 
sum(is.na(data[, SWAN_vars])) # 1 data point missing
sum(!is.na(data[, SWAN_vars])) # 7163 not missing -> 0.01% missing


###########################
## 2. Sample description ##
###########################

## General population sample
length(data$id)

# age
mean(data$age, na.rm = T) #28.56
sd(data$age, na.rm = T) #11.09
min(data$age, na.rm = T) #18
max(data$age, na.rm = T) #71

# gender distribution
table(data$gender) # 287 female, 109 male, 2 diverse

# medication
table(data$medication) # 1 yes, 396 no

# education
table(data$education) 


## Clinical sample
length(data_clin$id) # 49 participants

# age
mean(data_clin$age, na.rm = T) #27.67
sd(data_clin$age, na.rm = T) #6.45
min(data_clin$age, na.rm = T) #19
max(data_clin$age, na.rm = T) #46

# gender distribution
table(data_clin$gender) # 19 female, 29 male, 1 diverse, 1 NA

# medication
table(data_clin$medication) # 10 yes, 37 no, 2 N/A

# education
table(data_clin$education) 


#####################################################
## 3. Mean scores + SD & correlations of subscales ##
#####################################################

# means + SD
mean(data$SW_mean, na.rm = T) # 3.77
sd(data$SW_mean, na.rm = T) # 0.83

mean(data$SW_AD_mean, na.rm = T) # 3.78
sd(data$SW_AD_mean, na.rm = T) # 0.92

mean(data$SW_HI_mean, na.rm = T) # 3.76
sd(data$SW_HI_mean, na.rm = T) # 0.97

# min-max on item level
mean(data$SW01_01, na.rm = T) #3.89
mean(data$SW01_02, na.rm = T) #3.53
mean(data$SW01_03, na.rm = T) #4.35
mean(data$SW01_04, na.rm = T) #3.92
mean(data$SW01_05, na.rm = T) #4.09
mean(data$SW01_06, na.rm = T) #3.94
mean(data$SW01_07, na.rm = T) #4.09
mean(data$SW01_08, na.rm = T) #2.64
mean(data$SW01_09, na.rm = T) #3.57
mean(data$SW01_10, na.rm = T) #3.46
mean(data$SW01_11, na.rm = T) #4.37
mean(data$SW01_12, na.rm = T) #3.74
mean(data$SW01_13, na.rm = T) #3.98
mean(data$SW01_14, na.rm = T) #3.44
mean(data$SW01_15, na.rm = T) #3.81
mean(data$SW01_16, na.rm = T) #3.90
mean(data$SW01_17, na.rm = T) #3.52
mean(data$SW01_18, na.rm = T) #3.61
 
sd(data$SW01_01, na.rm = T) #1.30
sd(data$SW01_02, na.rm = T) #1.32
sd(data$SW01_03, na.rm = T) #1.25
sd(data$SW01_04, na.rm = T) #1.46
sd(data$SW01_05, na.rm = T) #1.50
sd(data$SW01_06, na.rm = T) #1.41
sd(data$SW01_07, na.rm = T) #1.30
sd(data$SW01_08, na.rm = T) #1.32
sd(data$SW01_09, na.rm = T) #1.48
sd(data$SW01_10, na.rm = T) #1.48
sd(data$SW01_11, na.rm = T) #1.31
sd(data$SW01_12, na.rm = T) #1.25
sd(data$SW01_13, na.rm = T) #1.39
sd(data$SW01_14, na.rm = T) #1.54
sd(data$SW01_15, na.rm = T) #1.44
sd(data$SW01_16, na.rm = T) #1.41
sd(data$SW01_17, na.rm = T) #1.25
sd(data$SW01_18, na.rm = T) #1.41

# correlations of subscales
cor.test(data$SW_AD_mean, data$SW_HI_mean) # r = 0.56, p < 0.001

cor.test(data$SW_AD_mean, data$SW_mean) # r = 0.88, p < 0.001

cor.test(data$SW_HI_mean, data$SW_mean) # r = 0.89, p < 0.001


####################
## 3. 3-way ANOVA ##
####################
data_ed1 = subset(data, data$education == 1) # subsets for educational level
data_ed2 = subset(data, data$education == 2)
data_ed3 = subset(data, data$education == 3)
data_ed4 = subset(data, data$education == 4)
data_f = subset(data, data$gender == 1)
data_m = subset(data, data$gender == 2)

# SWAN-TOT
m1 <- lm(SW_mean ~ gender 
         + age 
         + education 
         + gender*age*education
         , data = data)
Anova(m1, type = 2) 
eta_squared(m1)

mean(data_f$SW_mean) #3.82
sd(data_f$SW_mean) #0.77
mean(data_m$SW_mean) #3.68
sd(data_m$SW_mean) #0.95

mean(data_f$SW_AD_mean) #3.87
sd(data_f$SW_AD_mean) #0.85
mean(data_m$SW_AD_mean) #3.60
sd(data_m$SW_AD_mean) #1.02

# SWAN-AD
m2 <- lm(SW_AD_mean ~ gender 
         + age 
         + education 
         + gender*age*education
         , data = data)
Anova(m2, type = 2) 
eta_squared(m2)

# SWAN-HI
m3 <- lm(SW_HI_mean ~ gender 
         + age 
         + education 
         + gender*age*education
         , data = data)
Anova(m3, type = 2) 
eta_squared(m3)

# SWAN-HI post-hoc analyses for different educational levels
m4 <- lm(SW_HI_mean ~ gender 
         + age 
         + gender*age
         , data = data_ed2)
Anova(m4, type = 2) 
eta_squared(m4)

m5 <- lm(SW_HI_mean ~ gender 
         + age 
         + gender*age
         , data = data_ed3)
Anova(m5, type = 2) 
eta_squared(m5)

m6 <- lm(SW_HI_mean ~ gender 
         + age 
         + gender*age
         , data = data_ed4)
Anova(m6, type = 2) 
eta_squared(m5)

# SWAN-HI post-hoc analyses for different gender
m7 <- lm(SW_HI_mean ~ age
         + education 
         + age*education
         , data = data_f)
summary(m7)
Anova(m7, type = 2) 
eta_squared(m7)

m8 <- lm(SW_HI_mean ~ age
         + education 
         + age*education
         , data = data_m)
summary(m8)
Anova(m8, type = 2) 
eta_squared(m8)

####################
## 3b. SEM-Models ##
####################

# age

sem_age1 <- '
        SW_GF =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06
                           + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12 
                           + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;
        SW_GF ~ age
'
fit_age1 <- sem(sem_age1, data=data)
summary(fit_age1)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_GF ~                                             
#    age               0.001    0.003    0.285    0.775


sem_age2 <- '
        SW_AD =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 
                + SW01_07 + SW01_08 + SW01_09; 
        SW_AD ~ age
'
fit_age2 <- sem(sem_age2, data=data)
summary(fit_age2)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_AD ~                                             
#    age               0.004    0.003    1.018    0.309

sem_age3 <- '
        SW_HI =~ SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 
                + SW01_16 + SW01_17 + SW01_18; 
        SW_HI ~ age
'
fit_age3 <- sem(sem_age3, data=data)
summary(fit_age3)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_HI ~                                             
#    age              -0.002    0.005   -0.530    0.596

# gender 
sem_gender1 <- '
        SW_GF =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06
                           + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12 
                           + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;
        SW_GF ~ gender
'
fit_gender1 <- sem(sem_gender1, data=data)
summary(fit_gender1)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_GF ~                                             
#    gender           -0.152    0.074   -2.043    0.041

sem_gender2 <- '
        SW_AD =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 
                + SW01_07 + SW01_08 + SW01_09; 
        SW_AD ~ gender
'
fit_gender2 <- sem(sem_gender2, data=data)
summary(fit_gender2)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_AD ~                                             
#    gender           -0.271    0.084   -3.219    0.001

sem_gender3 <- '
        SW_HI =~ SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 
                + SW01_16 + SW01_17 + SW01_18; 
        SW_HI ~ gender
'
fit_gender3 <- sem(sem_gender3, data=data)
summary(fit_gender3)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_HI ~                                             
#    gender           -0.067    0.111   -0.607    0.544

# education
sem_education1 <- '
        SW_GF =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06
                           + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12 
                           + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;
        SW_GF ~ education
'
fit_education1 <- sem(sem_education1, data=data)
summary(fit_education1)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_GF ~                                             
#    education         0.170    0.059    2.897    0.004

sem_education2 <- '
        SW_AD =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 
                + SW01_07 + SW01_08 + SW01_09; 
        SW_AD ~ education
'
fit_education2 <- sem(sem_education2, data=data)
summary(fit_education2)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_AD ~                                             
#    education         0.209    0.066    3.196    0.001

sem_education3 <- '
        SW_HI =~ SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 
                + SW01_16 + SW01_17 + SW01_18; 
        SW_HI ~ education
'
fit_education3 <- sem(sem_education3, data=data)
summary(fit_education3)

# Regressions:
#                   Estimate  Std.Err  z-value  P(>|z|)
#  SW_HI ~                                             
#    education         0.172    0.086    2.016    0.044

# interactions

sem_interaction1 <- '
        SW_GF =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06
                           + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12
                           + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;
        SW_GF ~ age + gender + age:gender
'

sem_interaction1 <- sem(sem_interaction1, data=data)
summary(sem_interaction1)

sem_interaction2 <- '
        SW_AD =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 
                + SW01_07 + SW01_08 + SW01_09; 
        SW_AD ~ age + gender + age:gender
'

fit_interaction2 <- sem(sem_interaction2, data=data)
summary(fit_interaction2)

sem_interaction3 <- '
        SW_HI =~ SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 
                + SW01_16 + SW01_17 + SW01_18; 
        SW_HI ~ age + gender + age:gender
'

fit_interaction3 <- sem(sem_interaction3, data=data)
summary(fit_interaction3)

sem_interaction4 <- '
        SW_GF =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06
                           + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12
                           + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;
        SW_GF ~ education + gender + education:gender
'

fit_interaction4 <- sem(sem_interaction4, data=data)
summary(fit_interaction4)

sem_interaction5 <- '
        SW_AD =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 
                + SW01_07 + SW01_08 + SW01_09; 
        SW_AD ~ education + gender + education:gender
'

fit_interaction5 <- sem(sem_interaction5, data=data)
summary(fit_interaction5)

sem_interaction6 <- '
        SW_HI =~ SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 
                + SW01_16 + SW01_17 + SW01_18; 
        SW_HI ~ education + gender + education:gender
'

fit_interaction6 <- sem(sem_interaction6, data=data)
summary(fit_interaction6)


sem_interaction7 <- '
        SW_GF =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06
                           + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12
                           + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;
        SW_GF ~ education + age + education:age
'

fit_interaction7 <- sem(sem_interaction7, data=data)
summary(fit_interaction7)

sem_interaction8 <- '
        SW_AD =~ SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 
                + SW01_07 + SW01_08 + SW01_09; 
        SW_AD ~ education + age + education:age
'

fit_interaction8 <- sem(sem_interaction8, data=data)
summary(fit_interaction8)

sem_interaction9 <- '
        SW_HI =~ SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 
                + SW01_16 + SW01_17 + SW01_18; 
        SW_HI ~ education + age + education:age
'

fit_interaction9 <- sem(sem_interaction9, data=data)
summary(fit_interaction9)



########################
## 4. Skew & Kurtosis ##
########################

# Formula to calculate skew and kurtosis
funSkewKurtosis = function(x) {
  w = length(x)
  m1 = mean(x)
  m2 = sum((x-m1)^2)
  m3 = sum((x-m1)^3)
  m4 = sum((x-m1)^4)
  s1 = sd(x)
  skew = w*m3/(w-1)/(w-2)/s1^3
  sdskew = sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis = (w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis = sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  mat = matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
             dimnames=list(c("skew","kurtosis"), c("estimate","se")))
  return(mat)
}

# Skew and Kurtosis SWAN-TOT
f1 = funSkewKurtosis(data$SW_mean) # skew = -0.12, kurtosis= 0.51 
# Significance test against 0
f1[1,1]/f1[1,2] # z = -1.001
f1[2,1]/f1[2,2] # z = 2.09

# Skew and Kurtosis SWAN-AD
f2 = funSkewKurtosis(data$SW_AD_mean) # skew = -0.48, SE 0.122, kurtosis= 0.74
# Significance test against 0
f2[1,1]/f2[1,2] # z = -3.91
f2[2,1]/f2[2,2] # z = 3.05

# Skew and Kurtosis SWAN-AD
f3 = funSkewKurtosis(data$SW_HI_mean) # skew = -0.48, SE 0.122, kurtosis= 0.74
# Significance test against 0
f3[1,1]/f3[1,2] # z = 1.26
f3[2,1]/f3[2,2] # z = -1.41

# Histograms
setwd("Z:/A_SWAN-DE_FB_JK_LB/08_Publication/Abbildungen") # set WD

tiff("2022-11-05_Histogram_SWAN.tiff", width = 680, height = 260, units = "px")

par(mfrow = c(1,4))
par(pty = "s")
par(mar = c(5,0.5,0.5,0.5))

# empty plot
hist(data$SW_mean
     , main = NULL
     , xlab = ""
     , col = "white"
     , density = NULL
     , border = F
     , axes = F)

# SWAN-TOT
hist(data$SW_mean
     , xlab = ""
     , ylab = ""
     , main = ""
     , las = 1
     , breaks = 20
     , freq = F
     , lwd = 1
     , cex.axis = 1.5
     , cex.lab = 1.5
     , cex.main = 1.5
     , xlim = c(0,6)
     , ylim = c(0,0.7))

mtext(side = 2
      , line = 4
      , "Density"
      , cex = 1.25
      , font = 1)

mtext(side = 3
      , line = 1
      , "SWAN-TOT"
      , cex = 1.3
      , font = 1)

m <- mean(data$SW_mean)
s <- sd(data$SW_mean)

curve(dnorm(x,m,s),add=TRUE, lwd = 2) 

# SWAN-AD
hist(data$SW_AD_mean
     , xlab = ""
     , main = ""
     , las = 1
     , breaks = 20
     , freq = F
     , lwd = 1
     , xlim = c(0,6)
     , cex.axis = 1.5
     , cex.main = 1.5
     , cex.lab = 1.75
     , axes = F)

m <- mean(data$SW_AD_mean)
s <- sd(data$SW_AD_mean)

mtext(side = 3
      , line = 1
      , "SWAN-AD"
      , cex = 1.3
      , font = 1)
mtext(side = 1
      , line = 3
      , "Mean"
      , cex = 1.25
      , font = 1)

axis(1, cex.axis = 1.5)

curve(dnorm(x,m,s),add=TRUE, lwd = 2) 

# SWAN-HI
hist(data$SW_HI_mean
     , xlab = ""
     , main = ""
     , las = 1
     , breaks = 20
     , freq = F
     , lwd = 1
     , xlim = c(0,6)
     , cex.axis = 1.5
     , cex.main = 1.5
     , cex.lab = 1.5
     , axes = F)

m <- mean(data$SW_HI_mean)
s <- sd(data$SW_HI_mean)

axis(1, cex.axis = 1.5)

mtext(side = 3
      , line = 1
      , "SWAN-HI"
      , cex = 1.3
      , font = 1)

curve(dnorm(x,m,s),add=TRUE, lwd = 2) 

dev.off()

# QQ-Plots
setwd("BLINDED") # set WD

tiff("2022-11-05_QQ_SWAN.tiff", width = 680, height = 260, units = "px")

par(mfrow = c(1,4))
par(pty = "s")
par(mar = c(5,0.5,0.5,0.5))

# Empty plot
qqnorm(data$SW_mean
       , xlab = ""
       , ylab = ""
       , main = NULL
       , las = 1
       , cex.main = 1.75
       , cex.axis = 1.5
       , cex.lab = 1.75
       , axes = F
       , col = "white")

# SWAN-TOT
qqnorm(data$SW_mean
       , main = ""
       , ylab = ""
       , xlab = ""
       , las = 1
       , cex.main = 1.75
       , cex.axis = 1.5
       , cex.lab = 1.75
       , frame = F)

qqline(data$SW_mean)

mtext(side = 2
      , line = 3
      , "Sample quantiles"
      , cex = 1.25
      , font = 1)

mtext(side = 3
      , line = 1
      , "SWAN-TOT"
      , cex = 1.3
      , font = 1)

# SWAN-AD
qqnorm(data$SW_AD_mean
       , main = ""
       , xlab = ""
       , las = 1
       , cex.main = 1.75
       , cex.axis = 1.5
       , cex.lab = 1.75
       , axes = F)

qqline(data$SW_AD_mean)

axis(1, cex.axis = 1.5)

mtext(side = 3
      , line = 1
      , "SWAN-AD"
      , cex = 1.3
      , font = 1)
mtext(side = 1
      , line = 3.5
      , "Theoretical quantiles"
      , cex = 1.25
      , font = 1)


# SWAN-HI
qqnorm(data$SW_HI_mean
       , main = ""
       , xlab = ""
       , ylab = ""
       , las = 1
       , cex.main = 1.75
       , cex.axis = 1.5
       , cex.lab = 1.75
       , axes = F)

qqline(data$SW_HI_mean)

mtext(side = 3
      , line = 1
      , "SWAN-HI"
      , cex = 1.3
      , font = 1)

axis(1, cex.axis = 1.5)

dev.off()

####################
## 5. Reliability ##
####################

SWAN_vars = colnames(data)[str_detect(colnames(data),'SW01')] 

SW_AD <- colnames(data[,c('SW01_01'
                          , 'SW01_02'
                          , 'SW01_03'
                          , 'SW01_04'
                          , 'SW01_05'
                          , 'SW01_06'
                          , 'SW01_07'
                          , 'SW01_08'
                          , 'SW01_09')])

SW_HI <- colnames(data[,c('SW01_10'
                          , 'SW01_11'
                          , 'SW01_12'
                          , 'SW01_13'
                          , 'SW01_14'
                          , 'SW01_15'
                          , 'SW01_16'
                          , 'SW01_17'
                          , 'SW01_18')])

# Cronbachs alphas
psych::alpha(data[,SWAN_vars]) # 0.90 
psych::alpha(data[,SW_AD]) # 0.85
psych::alpha(data[,SW_HI]) # 0.87

# McDonalds omegas 
install.packages("GPArotation")
library(GPArotation)
psych::omega(data[SWAN_vars], nfactors = 2)
#                                                  g  F1*  F2*
# Omega total for total scores and subscales    0.91 0.84 0.87

# Correlation Matrix
corr.test(data[,c(SW_AD, "SW_AD_mean")]) # 0.53 - 0.77
corr.test(data[,c(SW_HI, "SW_HI_mean")]) # 0.62 - 0.77
corr.test(data[,c(SWAN_vars, "SW_mean")]) # 0.48 - 0.73

## Figures 1 and 2

## SWAN wrt to gender
# SWAN-TOT
setwd("Z:/A_SWAN-DE_FB_JK_LB/08_Publication/Abbildungen") # set WD

tiff("2022-11-05_SWAN_gender.tiff", width = 680, height = 260, units = "px")

par(mfrow = c(1,4))
par(pty = "s")
par(mar = c(0.5, 0.5, 0.5, 0.5))

# empty plot with axis labeling
plot(data$SW_mean
     , axes = F
     , xlim = c(0,3)
     , ylim= c(0,6)
     , ann = F
     , pch = 19
     , col = "white"
     , cex = 0.75)

# SWAN-TOT
plot(data$age
     , data$SW_mean
     , col  = "white"
     , ylim = c(0,6)
     , xlim = c(18,71)
     , ann = F
     , cex = 1.25
     , axes = F)

axis(1
     , at = seq(20,70,10)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)
axis(2
     , at = seq(0,6,1)
     , cex.axis = 1.75
     , las = 1
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 2
      , line = 2.5
      , "Mean"
      , cex = 1.25
      , font = 1)
mtext(side = 3
      , line = 1
      , "SWAN-TOT"
      , cex = 1.3
      , font = 1)

points(data$age[data$gender == "1"], data$SW_mean[data$gender == "1"], col = "indianred3", pch = 16)
points(data$age[data$gender == "2"], data$SW_mean[data$gender == "2"], col = "steelblue4", pch = 16)

r_tot_w = lm(data$SW_mean[data$gender == "1"]~ data$age[data$gender == "1"])
r_tot_m = lm(data$SW_mean[data$gender == "2"]~ data$age[data$gender == "2"])

abline(r_tot_w
       , col = "indianred3"
       , lwd = 2)
abline(r_tot_m
       , col = "steelblue4"
       , lwd = 2)

# legend
legend("bottomleft"
       , col = c("indianred3", "steelblue4")
       , legend = c("female","male")
       , pch = 16
       , horiz = T
       , cex = 1.5
       , bty = "n")

# SWAN-AD
plot(data$age
     , data$SW_AD_mean
     , col  ="white"
     , ylim = c(0,6)
     , xlim = c(18,71)
     , ann = F
     , cex = 1.25
     , axes = F)

axis(1
     , at = seq(20,70,10)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 1
      , line = 3
      , "Age (in years)"
      , cex = 1.25
      , font = 1)
mtext(side = 3
      , line = 1
      , "SWAN-AD"
      , cex = 1.3
      , font = 1)

points(data$age[data$gender == "1"], data$SW_AD_mean[data$gender == "1"], col = "indianred3", pch = 16)
points(data$age[data$gender == "2"], data$SW_AD_mean[data$gender == "2"], col = "steelblue4", pch = 16)

r_ua_w = lm(data$SW_AD_mean[data$gender== "1"]~ data$age[data$gender == "1"])
r_ua_m = lm(data$SW_AD_mean[data$gender == "2"]~ data$age[data$gender == "2"])

abline(r_ua_w
       , col = "indianred3"
       , lwd = 2)
abline(r_ua_m
       , col = "steelblue4"
       , lwd = 2)


# SWAN-HI
plot(data$age
     , data$SW_HI_mean
     , col  ="white"
     , ylim = c(0,6)
     , xlim = c(18,71)
     , ann = F
     , cex = 1.25, axes = F)

axis(1
     , at = seq(20,70,10)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "SWAN-HI"
      , cex = 1.3
      , font = 1)

points(data$age[data$gender == "1"], data$SW_HI_mean[data$gender == "1"], col = "indianred3", pch = 16)
points(data$age[data$gender == "2"], data$SW_HI_mean[data$gender == "2"], col = "steelblue4", pch = 16)

r_hi_w = lm(data$SW_HI_mean[data$gender == "1"]~ data$age[data$gender == "1"])
r_hi_m = lm(data$SW_HI_mean[data$gender == "2"]~ data$age[data$gender == "2"])

abline(r_hi_w
       , col = "indianred3"
       , lwd = 2)
abline(r_hi_m
       , col = "steelblue4"
       , lwd = 2)

dev.off() 


# SWAN wrt to educational level
# SWAN-TOT
setwd("BLINDED") # set WD

tiff("2022-11-05_SWAN_education.tiff", width = 680, height = 280, units = "px")

par(mfrow = c(1,4))
par(pty = "s")
par(mar = c(0.5, 0.5, 0.5, 0.5))

# empty plot with axis labeling
plot(data$SW_mean
     , axes = F
     , xlim = c(0,3)
     , ylim= c(0,6)
     , ann = F
     , pch = 19
     , col = "white"
     , cex = 0.75)

# SWAN-TOT
plot(data$age
     , data$SW_mean
     , col  ="white"
     , ylim = c(0,6)
     , xlim = c(18,71)
     , ann = F
     , cex = 1.25
     , axes = F)

axis(1
     , at = seq(20,70,10)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)
axis(2
     , at = seq(0,6,1)
     , cex.axis = 1.75
     , las = 1
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "SWAN-TOT"
      , cex = 1.3
      , font = 1)
mtext(side = 2
      , line = 2.5
      , "Mean"
      , cex = 1.25 )

points(data$age[data$education == 3], data$SW_mean[data$education == 3], col = "lightsalmon", pch = 16)
points(data$age[data$education == 2], data$SW_mean[data$education == 2], col = "darkseagreen4", pch = 16)
points(data$age[data$education == 1], data$SW_mean[data$education == 1], col = "darkseagreen1", pch = 16)
points(data$age[data$education == 4], data$SW_mean[data$education == 4], col = "lightsalmon4", pch = 16)

r_tot_fa = lm(data$SW_mean[data$education == 3]~ data$age[data$education == 3])
r_tot_hr = lm(data$SW_mean[data$education == 2]~ data$age[data$education == 2])
#r_tot_s = lm(data$SW_mean[data$education == 1]~ data$age[data$education == 1])
r_tot_u = lm(data$SW_mean[data$education == 4]~ data$age[data$education == 4])

abline(r_tot_fa
       , col = "lightsalmon"
       , lwd = 2)
abline(r_tot_hr
       , col = "darkseagreen4"
       , lwd = 2)
#abline(r_tot_s
#, col = "darkseagreen1"
#, lwd = 2)
abline(r_tot_u
       , col = "lightsalmon4"
       , lwd = 2)

#  legend
legend("bottomleft"
       , cex = 1
       , col = c("darkseagreen1","darkseagreen4","lightsalmon","lightsalmon4")
       , legend = c("Secondary school student","Secondary school leaving certificate","High school diploma","University degree")
       , pch = 16
       , bty = "n")

# SWAN-AD
plot(data$age
     , data$SW_AD_mean
     , col  ="white"
     , ylim = c(0,6)
     , xlim = c(18,71)
     , ann = F
     , cex = 1.25
     , axes = F)

axis(1
     , at = seq(20,70,10)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "SWAN-AD"
      , cex = 1.3
      , font = 1)
mtext(side = 1
      , line = 3
      , "Age (in years)"
      , cex = 1.25)

points(data$age[data$education == 3], data$SW_AD_mean[data$education == 3], col = "lightsalmon", pch =16)
points(data$age[data$education == 2], data$SW_AD_mean[data$education == 2], col = "darkseagreen4", pch =16)
points(data$age[data$education == 1], data$SW_AD_mean[data$education == 1], col = "darkseagreen1", pch =16)
points(data$age[data$education == 4], data$SW_AD_mean[data$education == 4], col = "lightsalmon4", pch =16)

r_ua_fa = lm(data$SW_AD_mean[data$education == 3]~ data$age[data$education == 3])
r_ua_hr = lm(data$SW_AD_mean[data$education == 2]~ data$age[data$education == 2])
#r_tot_s = lm(data$SW_AD_mean[data$education == 1]~ data$age[data$education == 1])
r_ua_u = lm(data$SW_AD_mean[data$education == 4]~ data$age[data$education == 4])

abline(r_ua_fa
       , col = "lightsalmon"
       , lwd = 2)
abline(r_ua_hr
       , col = "darkseagreen4"
       , lwd = 2)
# abline(r_ua_s
#, col = "darkseagreen1"
#, lwd = 2)
abline(r_ua_u
       , col = "lightsalmon4"
       , lwd = 2)

# SWAN-HI
plot(data$age
     , data$SW_HI_mean
     , col  = "white"
     , ylim = c(0,6)
     , xlim = c(18,71)
     , ann = F
     , cex = 1.25
     , axes = F)

axis(1
     , at = seq(20,70,10)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "SWAN-HI"
      , cex = 1.3
      , font = 1)

points(data$age[data$education == 3], data$SW_HI_mean[data$education == 3], col = "lightsalmon", pch =16)
points(data$age[data$education == 2], data$SW_HI_mean[data$education == 2], col = "darkseagreen4", pch =16)
points(data$age[data$education == 1], data$SW_HI_mean[data$education == 1], col = "darkseagreen1", pch =16)
points(data$age[data$education == 4], data$SW_HI_mean[data$education == 4], col = "lightsalmon4", pch =16)

r_hi_fa = lm(data$SW_HI_mean[data$education == 3]~ data$age[data$education == 3])
r_hi_hr = lm(data$SW_HI_mean[data$education == 2]~ data$age[data$education == 2])
#r_hi_s = lm(data$SW_HI_mean[data$education == 1]~ data$age[data$education == 1])
r_hi_u = lm(data$SW_HI_mean[data$education == 4]~ data$age[data$education == 4])

abline(r_hi_fa
       , col = "lightsalmon"
       , lwd = 2)
abline(r_hi_hr
       , col = "darkseagreen4"
       , lwd = 2)
# abline(r_hi_s
#, col = "darkseagreen1"
#, lwd = 2)
abline(r_hi_u
       , col = "lightsalmon4"
       , lwd = 2)

dev.off()



######################
## 6. Validity: CFA ##
######################

# Model 1: Bifactor Model 
# Model specification
swan_model_2 <- 'SW_GF =~ NA*SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18; SW_AD =~ NA*SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 + SW01_07 + SW01_08 + SW01_09; SW_HI =~ NA*SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18;SW_GF ~~ 1*SW_GF; SW_AD ~~ 1*SW_AD; SW_HI ~~ 1*SW_HI; SW_GF ~~ 0*SW_AD; SW_AD ~~ 0*SW_HI; SW_HI ~~ 0*SW_GF'

# Model calculation 
swan_m2_cfa <- cfa(swan_model_2
                   , data = data_SWAN
                   , std.lv = T
                   , missing = "fiml"
                   , estimator = "MLR")
# Summary
summary(swan_m2_cfa, standardized = T, fit = T)

mi <- modindices(swan_m2_cfa, minimum.value = 10, sort = T)


# Model 2: Bifactor Model with Modification Items 5&7 and 2&6 
# Model specification
swan_model_3 <- 'SW_GF =~ NA*SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 + SW01_07 + SW01_08 + SW01_09 + SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18; SW_AD =~ NA*SW01_01 + SW01_02 + SW01_03 + SW01_04 + SW01_05 + SW01_06 + SW01_07 + SW01_08 + SW01_09; SW_HI =~ NA*SW01_10 + SW01_11 + SW01_12 + SW01_13 + SW01_14 + SW01_15 + SW01_16 + SW01_17 + SW01_18; SW_GF ~~ 1*SW_GF; SW_AD ~~ 1*SW_AD; SW_HI ~~ 1*SW_HI; SW_GF ~~ 0*SW_AD; SW_AD ~~ 0*SW_HI; SW_HI ~~ 0*SW_GF; SW01_05 ~~ SW01_07; SW01_02 ~~ SW01_06'

# Model calculation 
swan_m3_cfa <- cfa(swan_model_3
                   , data = data_SWAN
                   , std.lv = T
                   , missing = "fiml"
                   , estimator = "MLR")

# Summary
s = summary(swan_m3_cfa, standardized = T, fit = T) # standardised factor loading is std.all

# R2 for standardised factor loadings
for (i in 1:length(data_SWAN)){
  r = (s$pe[0+i,10])^2 + (s$pe[0+18+i,10])^2 
  print(r)
}


# Model Comparison Model 2 + 3
sbs.chi(352.86, 308.602, 117, 115, 1.231, 1.199) # sig. different (3 better)


# Figure Structural Model (Model 3)
m = matrix(nrow = 18, ncol = 3)
m[,1] = c(rep(0, 4), "SW_A", rep(0, 8), "SW_H", rep(0, 4))
m[,2] = c(SWAN_vars)
m[,3] = c(rep(0, 9), "SW_G", rep(0, 8))

str_model <- semPaths(swan_m3_cfa
                      , layout = m
                      , intercepts = F
                      , what = "std"
                      , style = "lisrel"
                      , edge.color = "grey10"
                      , fade = F
                      , edge.label.cex = 0.6
                      , sizeMan = 6
                      , sizeInt = 5
                      , sizeLat = 8
                      , sizeMan2 = 3
                      , esize = 1
                      , residuals = F
                      , curvePivot = F)

# Add edge connect point to end point of edges (2nd column) for bifactor:
str_model$graphAttributes$Edges$edgeConnectPoints[1:18,2] <- 0.5 * pi

# Add edge connect point to end point of edges (2nd column) for other factors:
str_model$graphAttributes$Edges$edgeConnectPoints[19:36,2] <- 1.5 * pi

str_model$graphAttributes$Edges$edgeConnectPoints[37,2] <-  0.5 * pi
str_model$graphAttributes$Edges$edgeConnectPoints[38,2] <-  0.5 * pi
str_model$graphAttributes$Edges$edgeConnectPoints[39,2] <-  0.5 * pi
str_model$graphAttributes$Edges$edgeConnectPoints[40,2] <-  0.5 * pi

str_model$graphAttributes$Edges$edgeConnectPoints[37,1] <-  0.5 * pi
str_model$graphAttributes$Edges$edgeConnectPoints[38,1] <-  0.5 * pi
str_model$graphAttributes$Edges$edgeConnectPoints[39,1] <-  0.5 * pi
str_model$graphAttributes$Edges$edgeConnectPoints[40,1] <-  0.5 * pi

str_model$graphAttributes$Edges$curve[37] <- 1.8 * pi

str_model$graphAttributes$Edges$curve[38] <- 1 * pi

str_model$graphAttributes$Nodes$labels <- c("SWAN_01"
                                            , "SWAN_02"
                                            , "SWAN_03"
                                            , "SWAN_04"
                                            , "SWAN_05"
                                            , "SWAN_06"
                                            , "SWAN_07"
                                            , "SWAN_08"
                                            , "SWAN_09"
                                            , "SWAN_10"
                                            , "SWAN_11"
                                            , "SWAN_12"
                                            , "SWAN_13"
                                            , "SWAN_14"
                                            , "SWAN_15"
                                            , "SWAN_16"
                                            , "SWAN_17"
                                            , "SWAN_18"
                                            , "G-ADHD"
                                            , "SP-AD"
                                            , "SP-HI")

tiff("2022-11-05_Structural model.tiff", width = 480, height = 480, units = "px")
plot(str_model)
dev.off()

################################
## 7b. Measurement invariance ##
################################

## gender 
data_mi_gender <- data[data$gender != 3,] # exclude "diverse" because of small sample size
data_mi_gender <- data_mi_gender[!is.na(data_mi_gender$gender), ] # exclude rows with missing data

cfa.config <- cfa(swan_model_3, data = data_mi_gender, group = "gender")
cfa.metric <- cfa(swan_model_3, data = data_mi_gender, group = "gender", group.equal = "loadings")
cfa.scalar <- cfa(swan_model_3, data = data_mi_gender, group = "gender", group.equal = c("loadings", "intercepts"))
cfa.strict <- cfa(swan_model_3, data = data_mi_gender, group = "gender", group.equal = c("loadings", "intercepts", "residuals"))

fitmeasures(cfa.config, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.scalar, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.strict, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

MesInv_gender <- lavTestLRT(cfa.config, cfa.metric, cfa.scalar, cfa.strict)

## age 
table(data$age) # generate age groups to compare
data_mi_age <- data[!is.na(data$age), ]
data_mi_age$agegroup[data_mi_age$age < 20] <- 1
data_mi_age$agegroup[data_mi_age$age >= 20 & data_mi_age$age < 30] <- 2
data_mi_age$agegroup[data_mi_age$age >= 30 & data_mi_age$age < 40] <- 3
data_mi_age$agegroup[data_mi_age$age >= 40 & data_mi_age$age < 50] <- 4
data_mi_age$agegroup[data_mi_age$age >= 50] <- 5
table(data_mi_age$agegroup)

cfa.config <- cfa(swan_model_3, data = data_mi_age, group = "agegroup")
cfa.metric <- cfa(swan_model_3, data = data_mi_age, group = "agegroup", group.equal = "loadings")
cfa.scalar <- cfa(swan_model_3, data = data_mi_age, group = "agegroup", group.equal = c("loadings", "intercepts"))
cfa.strict <- cfa(swan_model_3, data = data_mi_age, group = "agegroup", group.equal = c("loadings", "intercepts", "residuals"))

fitmeasures(cfa.config, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.scalar, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.strict, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

MesInv_age <- lavTestLRT(cfa.config, cfa.metric, cfa.scalar, cfa.strict)


## education
table(data$education) # groups 1 and 2 are too small --> integrate in group 3
data_mi_education_gr <- data[!is.na(data$education), ]
data_mi_education_gr$education_gr[data_mi_education_gr$education == 1] <- 3
data_mi_education_gr$education_gr[data_mi_education_gr$education == 2] <- 3
data_mi_education_gr$education_gr[data_mi_education_gr$education == 3] <- 3
data_mi_education_gr$education_gr[data_mi_education_gr$education == 4] <- 4
table(data_mi_education_gr$education_gr)

cfa.config <- cfa(swan_model_3, data = data_mi_education_gr, group = "education_gr")
cfa.metric <- cfa(swan_model_3, data = data_mi_education_gr, group = "education_gr", group.equal = "loadings")
cfa.scalar <- cfa(swan_model_3, data = data_mi_education_gr, group = "education_gr", group.equal = c("loadings", "intercepts"))
cfa.strict <- cfa(swan_model_3, data = data_mi_education_gr, group = "education_gr", group.equal = c("loadings", "intercepts", "residuals"))

fitmeasures(cfa.config, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.metric, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.scalar, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
fitmeasures(cfa.strict, fit.measures = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))

MesInv_edu <- lavTestLRT(cfa.config, cfa.metric, cfa.scalar, cfa.strict)
# Strict measurement invariance given 


MesInv_gender
MesInv_age
MesInv_edu


############################
## 8. Convergent validity ##
############################

## General population sample
# SWAN and ADHS-SB
cor.test(data$SW_mean, data$HA_mean) # -0.59
cor.test(data$SW_AD_mean, data$HA_AD_mean) #-0.63
cor.test(data$SW_HI_mean, data$HA_HI_mean) #-0.54

# SWAN and CAARS-K-SB
cor.test(data$SW_mean, data$CA_mean) #-0.42
cor.test(data$SW_AD_mean, data$CA_AD_mean) #-0.54
cor.test(data$SW_HI_mean, data$CA_HI_mean) #-0.38

## Clinical sample -> see script "prepare data for analysis"


# Figure 5
setwd("BLINDED") # set WD

tiff("2022-11-02_ADHS-SB_SWAN.tiff", width = 680, height = 280, units = "px")

par(mfrow = c(1,4))
par(pty = "s")
par(mar = c(5, 0.5, 0.5, 0.5))

# empty plot with axis labeling
plot(data$HA_mean
     , data$SW_mean
     , axes = F
     , xlim = c(0,3)
     , ylim= c(0,6)
     , ann = F
     , pch = 19
     , col = "white"
     , cex = 0.75)

# SWAN-TOT vs. ADHS-SB-TOT
plot(data$HA_mean
     , data$SW_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F
     , pch = 19
     , col = "gray40"
     , cex = 0.75)

axis(1
     , at = seq(0,3,1)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)
axis(2
     , at = seq(0,6,1)
     , cex.axis = 1.75
     , las = 1
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 2
      , line = 3
      , "Mean SWAN-DE-SB"
      , cex = 1.25)
mtext(side = 3
      , line = 1
      , "TOT scales"
      , cex = 1.25
      , font = 1)

reg2 = lm(SW_mean ~ HA_mean
          , data = data)

abline(reg2
       , lwd = 2
       , col = "darkred")

# SWAN-AD vs. ADHS-SB-AD
plot(data$HA_AD_mean
     , data$SW_AD_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F
     , pch = 19
     , col = "gray50"
     , cex = 0.75)

axis(1
     , at = seq(0,3,1)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "AD scales"
      , cex = 1.25
      , font = 1)
mtext(side = 1
      , line = 3
      , "Mean ADHS-SB"
      , cex = 1.25)

reg2AD = lm(SW_AD_mean ~ HA_AD_mean
            , data = data)

abline(reg2AD
       , lwd = 2
       , col = "darkred")

# SWAN-HI vs. ADHS-SB-HI
plot(data$HA_HI_mean
     , data$SW_HI_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F 
     , pch = 19
     , col = "gray70"
     , cex = 0.75)

axis(1
     , at = seq(0,3,1)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "HI scales"
      , cex = 1.25
      , font = 1)

reg2HI = lm(SW_HI_mean ~ HA_HI_mean
            , data = data)

abline(reg2HI
       , lwd = 2
       , col = "darkred")

dev.off() 


# Figure 6
setwd("BLINDED") # set WD

tiff("2022-11-02_CAARS_SWAN.tiff", width = 680, height = 280, units = "px")

par(mfrow = c(1,4))
par(pty = "s")
par(mar = c(5,0.5,0.5,0.5))

# empty plot
plot(data$CA_mean
     , data$SW_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F
     , pch = 19
     , col = "white"
     , cex = 0.75)

# SWAN-TOT vs. CAARS-K-SB-TOT
plot(data$CA_mean
     , data$SW_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F
     , pch = 19
     , col = "gray40"
     , cex = 0.75)

axis(1
     , at = seq(0,3,1)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)
axis(2
     , at = seq(0,6,1)
     , cex.axis = 1.75
     , las = 1
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 2
      , line = 3
      , "Mean SWAN-DE-SB"
      , cex = 1.25)
mtext(side = 3
      , line = 1
      , "TOT scales"
      , cex = 1.25
      , font = 1)

reg1 = lm(SW_mean ~ CA_mean
          , data = data)

abline(reg1
       , lwd = 2
       , col = "darkred")

# SWAN-AD vs. CAARS-K-SB-AD
plot(data$CA_AD_mean
     , data$SW_AD_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F
     , pch = 19
     , col = "gray50"
     , cex = 0.75)

axis(1
     , at = seq(0,3,1)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "AD scales"
      , cex = 1.25, font = 1)
mtext(side = 1
      , line = 3
      , "Mean CAARS-K-SB"
      , cex = 1.25)

reg1AD = lm(SW_AD_mean ~ CA_AD_mean
            , data = data)

abline(reg1AD, lwd = 2, col = "darkred")

# SWAN-HI vs. CAARS-K-SB-HI
plot(data$CA_HI_mean
     , data$SW_HI_mean
     , axes = F
     , xlim = c(0,3)
     , ylim = c(0,6)
     , ann = F
     , pch = 19
     , col = "gray70"
     , cex = 0.75)

axis(1
     , at = seq(0,3,1)
     , cex.axis = 1.75
     , lwd = 1.5
     , lwd.ticks = 1.5)

mtext(side = 3
      , line = 1
      , "HI scales"
      , cex = 1.25
      , font = 1)

reg1HI = lm(SW_HI_mean ~ CA_HI_mean
            , data = data)

abline(reg1HI
       , lwd = 2
       , col = "darkred")

dev.off() 


## Clinical sample
# SWAN
SW_AD <- colnames(data_clinical[,c('SW01_01', 'SW01_02', 'SW01_03', 'SW01_04', 'SW01_05','SW01_06', 'SW01_07', 'SW01_08', 'SW01_09')])
SW_HI <- colnames(data_clinical[,c('SW01_10', 'SW01_11', 'SW01_12', 'SW01_13', 'SW01_14', 'SW01_15', 'SW01_16', 'SW01_17', 'SW01_18')])

data_clinical$SW_mean <- rowMeans(data_clinical[SWAN_var], na.rm = T) # mean SWAN-TOT
data_clinical$SW_AD_mean<- rowMeans(data_clinical[SW_AD], na.rm = T) # mean SWAN-AD
data_clinical$SW_HI_mean<- rowMeans(data_clinical[SW_HI], na.rm = T) # mean SWAN-HI

# CAARS
CA_AD = colnames(data_clinical[,c("CA01_03", "CA01_05", "CA01_17", "CA01_18", "CA01_21")])
CA_HI = colnames(data_clinical[,c("CA01_04", "CA01_06", "CA01_10", "CA01_11", "CA01_23", "CA01_01", "CA01_07", "CA01_08", "CA01_13", "CA01_20")])

data_clinical$CA_mean = rowMeans(data_clinical[,CAARS_var])
data_clinical$CA_AD_mean = rowMeans(data_clinical[,CA_AD])
data_clinical$CA_HI_mean <- rowMeans(data_clinical[,CA_HI])

# ADHS-SB (HASE)
HA_AD <- colnames(data_clinical[,c('HA01_01', 'HA01_02', 'HA01_03', 'HA01_04', 'HA01_05', 'HA01_06', 'HA01_07', 'HA01_08', 'HA01_09')])
HA_HI <- colnames(data_clinical[,c('HA01_10', 'HA01_11', 'HA01_12', 'HA01_13', 'HA01_14', 'HA01_15', 'HA01_16', 'HA01_17', 'HA01_18')])

data_clinical$HA_mean <- rowMeans(data_clinical[HASE_var], na.rm = T)
data_clinical$HA_AD_mean <- rowMeans(data_clinical[HA_AD], na.rm = T)
data_clinical$HA_HI_mean <- rowMeans(data_clinical[HA_HI], na.rm = T)

# SWAN and ADHS-SB

cor.test(data_clinical$SW_mean, data_clinical$HA_mean) # -0.74
cor.test(data_clinical$SW_AD_mean, data_clinical$HA_AD_mean) #-0.61
cor.test(data_clinical$SW_HI_mean, data_clinical$HA_HI_mean) #-0.79

# SWAN and CAARS-K-SB
cor.test(data_clinical$SW_mean, data_clinical$CA_mean) #-0.49
cor.test(data_clinical$SW_AD_mean, data_clinical$CA_AD_mean) #-0.50
cor.test(data_clinical$SW_HI_mean, data_clinical$CA_HI_mean) #-0.63


#########################
## 9. Clinical utility ##
#########################

# Diff with w/o ADHD diagnosis on SWAN-TOT (ANCOVA)
d1 = lm(SW_mean ~ diagnosis_now 
        + gender 
        + age 
        + education 
        + gender*age*education
        , data = data)
Anova(d1, type = 2) 
eta_squared(d1)

mean(data$SW_mean[data$diagnosis_now == 1]) # with diagnosis
sd(data$SW_mean[data$diagnosis_now == 1])
mean(data$SW_mean[data$diagnosis_now == 2]) # without diagnosis
sd(data$SW_mean[data$diagnosis_now == 2])


# Diff with w/o ADHD diagnosis on SWAN-AD (ANCOVA)
d2 = lm(SW_AD_mean ~ diagnosis_now 
        + gender 
        + age 
        + education 
        + gender*age*education
        , data = data)
Anova(d2, type = 2) 
eta_squared(d2)

mean(data$SW_AD_mean[data$diagnosis_now == 1]) # with diagnosis
sd(data$SW_AD_mean[data$diagnosis_now == 1])
mean(data$SW_AD_mean[data$diagnosis_now == 2]) # without diagnosis
sd(data$SW_AD_mean[data$diagnosis_now == 2])


# Diff with w/o ADHD diagnosis on SWAN-HI (ANCOVA)
d3 = lm(SW_HI_mean ~ diagnosis_now 
        + gender 
        + age 
        + education 
        + gender*age*education
        , data = data)
Anova(d3, type = 2) 
eta_squared(d3)

mean(data$SW_HI_mean[data$diagnosis_now == 1]) # with diagnosis
sd(data$SW_HI_mean[data$diagnosis_now == 1])
mean(data$SW_HI_mean[data$diagnosis_now == 2]) # without diagnosis
sd(data$SW_HI_mean[data$diagnosis_now == 2])

## ROC-Analyses
# SWAN-TOT
modroc_TOT  = reportROC(gold = data$diagnosis_now
                  , predictor = data$SW_mean
                  , plot = F) 
print(modroc_TOT) #AUC = 98.5% # PPV = 1.00, NPV = 0.39 # SEN = 0.956, SPE = 1.000, Cutoff = 2.639

  
# SWAN-AD
modroc_AD  = reportROC(gold = data$diagnosis_now
                       , predictor = data$SW_AD_mean
                       , plot = F) 
print(modroc_AD) #AUC = 94.9% # PPV = 0.99, NPV = 0.278, SEN = 0.933, SPE = 0.909, Cutoff = 2.611

# SWAN-HI
modroc_HI  = reportROC(gold = data$diagnosis_now 
                       , predictor = data$SW_HI_mean
                       , plot = F) 
print(modroc_HI) #AUC = 96.1%, PPV = 0.997, NPV = 0.227, SEN = 0.912, SPE = 0.909, Cutoff = 2.72




