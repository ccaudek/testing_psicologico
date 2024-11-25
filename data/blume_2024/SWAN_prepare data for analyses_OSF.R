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


####################################
## 1. Read data population sample ##
####################################

setwd("BLINDED") #set WD to data file location

data_pop = read.csv("BLINDED.csv"
               , header = T 
               , sep = ";" 
               , stringsAsFactors = F) # read data

## General Adaptation of the Data
# Delete unnecessary columns
data_pop$SERIAL <- NULL
data_pop$REF <- NULL
data_pop$QUESTNNR <- NULL
data_pop$MODE <- NULL
data_pop$STARTED <- NULL
data_pop$BE06_01 <- NULL
data_pop$BE06_02 <- NULL
data_pop$BE06_03 <- NULL
data_pop$BE06_04 <- NULL
data_pop$BE06_05 <- NULL

# Rename CASE
data_pop$CASE <- seq(1:405) # replace with number
names(data_pop)[names(data_pop) == "CASE"] <- "id"

# Rename variables
names(data_pop)[names(data_pop) == "SD01"] <- "gender"
names(data_pop)[names(data_pop) == "SD11_01"] <- "age"

data_pop$edu <- data_pop$SD10
data_pop$education <- NULL
data_pop$education[data_pop$edu == 1] <- "Schueler/in"
data_pop$education[data_pop$edu == 3 | data_pop$edu == 4] <- "Haupt & Real"
data_pop$education[data_pop$edu == 6 | data_pop$edu == 7] <- "(Fach-)Abitur"
data_pop$education[data_pop$edu == 8 | data_pop$edu == 10 | data_pop$edu == 11] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Diplom"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "diplom"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Fachhochschul-Diplom (Dipl.-Informatiker (FH))"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Diplom/Universit?t"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Dipl.-Ing."] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Staatsexamen"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "1. Staatsexamen (Lehramt)"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "3. Staatsexamen Medizin"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Dipl. Ing."] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "2. Staatsexamen"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "1. Staatsexamen"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Staatsexamem"] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Magister "] <- "Uniabschluss"
data_pop$education[data_pop$SD10_14 == "Abitur, Berufsausbildung mit Staatsexamen"] <- "(Fach-)Abitur"

data_pop$education[data_pop$education == "Schueler/in"] <- 1
data_pop$education[data_pop$education == "Haupt & Real"] <- 2
data_pop$education[data_pop$education == "(Fach-)Abitur"] <- 3
data_pop$education[data_pop$education == "Uniabschluss"] <- 4
data_pop$education = as.numeric(data_pop$education) # converts character into integer

names(data_pop)[names(data_pop) == "SD12"] <- "diagnosis_now"
names(data_pop)[names(data_pop) == "SD13"] <- "diagnosis_ever"
names(data_pop)[names(data_pop) == "SD14"] <- "medication"

data_pop <- data_pop[,!names(data_pop) %in% c("SW01_11")] # excludes item 11
SWAN_pop <- colnames(data_pop)[str_detect(colnames(data_pop),'SW01')] # detects all columns with SW01
for(i in 12:19){
  names(data_pop)[names(data_pop) == paste0("SW01_",i)] <- paste0("SW01_", i-1)
}


##################################
## 2. Read data clinical sample ##
##################################

setwd("BLINDED") # set WD

data_clinical = read.csv("BLINDED.csv"
                         , header = T
                         , sep = ";" 
                         , stringsAsFactors = F
                         , na.strings = c(-77, -99)) # read data

## General Adaptation of the Data
data_clinical$id <- seq(1:length(data_clinical$BE06_05)) # adds id variable
data_clinical$BE06_05 <- NULL
names(data_clinical)[names(data_clinical) == "diagnosis"] <- "diagnosis_type"
data_clinical$diagnosis_now <- 1

data_clinical$gender <- dplyr::recode(data_clinical$gender, "1" = "2", "2" = "1", "3" = "3")
data_clinical$gender <- as.numeric(data_clinical$gender)


##########################
## 3. Further adaptions ##
##########################

# Recode scales 0-6
# SWAN
SWAN_var <- colnames(data_pop)[str_detect(colnames(data_pop),'SW01')]
data_pop[,SWAN_var] = data_pop[,SWAN_var] - 1 # Recode Scale from 0 to 6
data_clinical[,SWAN_var] = data_clinical[,SWAN_var] - 1 # Recoding Scale from 0 to 6

# ADHS-SB (HASE) 0-3
HASE_pop <- colnames(data_pop)[str_detect(colnames(data_pop),'HA01')]
HASE_clinical <- colnames(data_clinical)[str_detect(colnames(data_clinical),'HA01')]
HASE_var <- colnames(data_pop)[str_detect(colnames(data_pop),'HA01')]
data_pop[,HASE_var] = data_pop[,HASE_var] - 1 # Recoding Scale from 0 to 3

# CAARS 0-3
CAARS_pop <- colnames(data_pop)[str_detect(colnames(data_pop),'CA01')]
CAARS_clinical <- colnames(data_clinical)[str_detect(colnames(data_clinical),'CA01')]
CAARS_var <- colnames(data_pop)[str_detect(colnames(data_pop),'CA01')]
data_pop[,CAARS_var] = data_pop[,CAARS_var] - 1 # Recoding Scale from 0 to 3


####################################
## 4. Establish population sample ##
####################################

data_clinical_AD = subset(data_clinical, diagnosis_type == 1) # creates subsets for AD type diagnosis
data_clinical_HI = subset(data_clinical, diagnosis_type == 2) # creates subsets for HI type diagnosis
data_clinical_C = subset(data_clinical, diagnosis_type == 3) # creates subsets for Combined type diagnosis

set.seed(4276)
data_clinical_ADrand = data_clinical_AD[sample(1:nrow(data_clinical_AD), 3), ] # draws 3 random participants from AD type
data_clinical_HIrand = data_clinical_HI[sample(1:nrow(data_clinical_HI), 1), ] # draws 2 random participant from HI type
data_clinical_Crand = data_clinical_C[sample(1:nrow(data_clinical_C), 7), ] # draws 7 random participants from combined type

data_pop_noADHD <- data_pop[data_pop$diagnosis_ever == 2 & data_pop$diagnosis_now == 2, ]
data_pop_noADHD <- data_pop_noADHD[,c("id"
                                      , "gender"
                                      , "age"
                                      , "diagnosis_ever"
                                      , "diagnosis_now"
                                      , "medication"
                                      , "education"
                                      , SWAN_var
                                      , HASE_var
                                      , CAARS_var)]

data_total <- full_join(data_pop_noADHD, data_clinical_ADrand)
data_total <- full_join(data_total, data_clinical_HIrand)
data_total <- full_join(data_total, data_clinical_Crand)


##############################################
## 5. Calculate means of scales + subscales ##
##############################################

## data_total
# SWAN
SW_AD = colnames(data_total[,c('SW01_01', 'SW01_02', 'SW01_03', 'SW01_04', 'SW01_05','SW01_06', 'SW01_07', 'SW01_08', 'SW01_09')])
SW_HI = colnames(data_total[,c('SW01_10', 'SW01_11', 'SW01_12', 'SW01_13', 'SW01_14', 'SW01_15', 'SW01_16', 'SW01_17', 'SW01_18')])

data_total$SW_mean = rowMeans(data_total[SWAN_var], na.rm = T) # mean SWAN-TOT
data_total$SW_AD_mean = rowMeans(data_total[SW_AD], na.rm = T) # mean SWAN-AD
data_total$SW_HI_mean = rowMeans(data_total[SW_HI], na.rm = T) # mean SWAN-HI

# CAARS
CA_AD = colnames(data_total[,c("CA01_03", "CA01_05", "CA01_17", "CA01_18", "CA01_21")])
CA_HI = colnames(data_total[,c("CA01_04", "CA01_06", "CA01_10", "CA01_11", "CA01_23", "CA01_01", "CA01_07", "CA01_08", "CA01_13", "CA01_20")])

data_total$CA_mean = rowMeans(data_total[,CAARS_var])
data_total$CA_AD_mean = rowMeans(data_total[,CA_AD])
data_total$CA_HI_mean = rowMeans(data_total[,CA_HI])

# ADHS-SB (HASE)
HA_AD <- colnames(data_total[,c('HA01_01', 'HA01_02', 'HA01_03', 'HA01_04', 'HA01_05', 'HA01_06', 'HA01_07', 'HA01_08', 'HA01_09')])
HA_HI <- colnames(data_total[,c('HA01_10', 'HA01_11', 'HA01_12', 'HA01_13', 'HA01_14', 'HA01_15', 'HA01_16', 'HA01_17', 'HA01_18')])

data_total$HA_mean <- rowMeans(data_total[HASE_var], na.rm = T)
data_total$HA_AD_mean <- rowMeans(data_total[HA_AD], na.rm = T)
data_total$HA_HI_mean <- rowMeans(data_total[HA_HI], na.rm = T)


## data_clinical
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






