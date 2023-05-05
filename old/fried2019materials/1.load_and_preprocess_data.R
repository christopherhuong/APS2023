##################################################################################
#                                                                                #
#           Using network analysis to examine links between individual           #
#            depressive symptoms, inflammatory markers, and covariates           #   
#                                                                                #
#                            Fried et al., 2019                                  #
#                          Psychological Medicine                                #
#                           https://osf.io/6ehrm/                                #
#                                                                                #
#                  1. Syntax file for data preprocessing                         #
#                                                                                #
#                         Last updated: 08-09-2019                               #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- Loading packages & Data ---------------------------------
# -------------------------------------------------------------------------

setwd("~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis")
library(foreign)
library(qgraph)
library(bootnet)
library(dplyr)
library(markdown)
library(huge)
library(summarytools)


# set working directories
dataDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/data_raw/"
dataPrDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/data_processed/"



# -------------------------------------------------------------------------
# --------------- Process Data for Network 1a -----------------------------
# -------------------------------------------------------------------------

# Load data + collapse one single dataframe
file_names <- list.files(dataDir);file_names
ID <- lapply(paste0(dataDir, file_names), read.spss, to.data.frame = T, use.value.labels = F)
ID <- bind_cols(ID)

# Subset data to create data set with variables of interest
Data <- subset(ID, select = c(pident, ahsCRP, aIL6, aTNFa, aids, aids01, aids02, aids03, aids04, aids05, aids06, aids07, aids08, aids09a, aids10, aids11, aids12, aids13, aids14, aids15, aids16, aids17, aids18, aids19, aids20, aids21, aids22, aids23, aids24, aids25, aids26, aids27, aids28, Sexe, Age, aauditsc, AIPMETOT, asmokstat, abmi, anumdis_treat))

# Recode variables and remove missing values
Data[, 34]<-Data[, 34] - 1 #code sex as 0 and 1
Data$aids11<-recode(Data$aids11, "5"=2, "6"=3, "7"=4) #recode appetite
Data$aids12<-recode(Data$aids12, "5"=2, "6"=3, "7"=4) #recode weight

# Delete missing data
Data <- na.omit(Data)                       # 263 NA omitted; n = 2981 to n = 2718
Data <- Data[!apply(Data<0, 1, any), ]      # further 275 omitted; n = 2443
Data_CRPhigh <- subset(Data, ahsCRP >= 10)  # make extra dataset of CRP removals
Data <- subset(Data, ahsCRP<10)             # further 122 omitted due to CRP values ≥ 10 to exclude N with acute infection; n = 2321

# Compare CRP removals with other people /re depression
t.test(Data_CRPhigh$aids, Data$aids)        #  No significant difference on IDS sum score
sd(Data$aids)
sd(Data_CRPhigh$aids)

# Detect skew based on visual inspection
hist(Data$ahsCRP)   #CRP
hist(Data$aIL6)     #IL6
hist(Data$aTNFa)    #alpha
hist(Data$aids)     #IDS total
hist(Data$aauditsc) #exercise 
hist(Data$AIPMETOT) #alcohol
summary(Data)

# Apply nonparanormal transformation to skewed distributions
Data[, 2:5]<-huge.npn(Data[, 2:5]) # normalize markers: ahsCRP, aIL6, aTNF, aids (=IDS total)
Data[, 36:37]<-huge.npn(Data[, 36:37]) # normalize exercise; alcohol

# Detect outliers
library(outliers)

hist(Data$ahsCRP)   #CRP
sum(scores(Data$ahsCRP, type="z", prob=0.997))    # 79 / 7 obs above the 3rd standard deviation based on z-scores (second value after transformation)
boxplot(Data$ahsCRP)
arrows(0.9, mean(Data$ahsCRP) - 3*sd(Data$ahsCRP), 0.9 , mean(Data$ahsCRP) + 3*sd(Data$ahsCRP), # +- 3SD
       code = 3, col = "red", angle = 40, length = .1)

hist(Data$aIL6)     #IL6
sum(scores(Data$aIL6, type="z", prob=0.997))      # 7 / 8 obs above the 3rd standard deviation based on z-scores (second value after transformation)
boxplot(Data$aIL6)
arrows(0.9, mean(Data$aIL6) - 3*sd(Data$aIL6), 0.9 , mean(Data$aIL6) + 3*sd(Data$aIL6), # +- 3SD
       code = 3, col = "red", angle = 40, length = .1)

hist(Data$aTNFa)    #alpha
sum(scores(Data$aTNFa, type="z", prob=0.997))     # 43 / 19 above the 3rd standard deviation based on z-scores (second value after transformation)
boxplot(Data$aTNFa)
arrows(0.9, mean(Data$aTNFa) - 3*sd(Data$aTNFa), 0.9 , mean(Data$aTNFa) + 3*sd(Data$aTNFa), # +- 3SD
       code = 3, col = "red", angle = 40, length = .1)



# -------------------------------------------------------------------------
# --------------- Get Data for Network 1a ---------------------------------
# -------------------------------------------------------------------------

data_n_1a <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

# Fill in
data_n_1a$data <- as.matrix(subset(Data, select = c(ahsCRP, aIL6, aTNFa, aids)))
data_n_1a$type <- c(rep("g", 4))
data_n_1a$level <- c(1, 1, 1, 1)
data_n_1a$labels <- c("IM1", "IM2", "IM3", "D1")
data_n_1a$grouplabels <-list("Inflammatory Markers" = c(1:3), "Depression Total" = c(4)) 
  
saveRDS(data_n_1a, file = paste0(dataPrDir, "data_n_1a.RDS"))



# -------------------------------------------------------------------------
# --------------- Get Data for Network 1b ---------------------------------
# -------------------------------------------------------------------------

data_n_1b <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

# Fill in
data_n_1b$data <- as.matrix(subset(Data, select = c(ahsCRP, aIL6, aTNFa, aids, Sexe, Age,aauditsc, asmokstat, AIPMETOT, abmi, anumdis_treat)))
data_n_1b$type <- c(rep("g", 4), rep("c", 1), rep("g", 5), rep("p", 1))
data_n_1b$level <- c(1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1)
data_n_1b$names <- c("CRP", "IL-6", "TNF-alpha", "IDS Total","Sex", "Age", "Alcohol", "Smoking","Exercise", "BMI", "Chronic Diseases")
data_n_1b$labels <- c("IM1", "IM2", "IM3", "D1", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
data_n_1b$grouplabels <- list("Inflammatory Markers" = c(1:3), "Depression Total" = c(4), "Covariates" = c(5:11))

saveRDS(data_n_1b, file = paste0(dataPrDir, "data_n_1b.RDS"))



# -------------------------------------------------------------------------
# --------------- Get Data for Network 2a ---------------------------------
# -------------------------------------------------------------------------

#Analyses 2a/b > mgm 9 DSM-IV MDD criteria symptoms building on the model from Jokela and colleagues (2016)
#create data set
pident<-Data$pident
sadmood<-Data$aids05
interest<-pmax(Data$aids17, Data$aids19)
sleep<-do.call(pmax, Data[, 6:9])
energy<-Data$aids18
appetite<-pmax(Data$aids11, Data$aids12)
moving<-pmax(Data$aids21, Data$aids22)
concentration<-Data$aids13
worthlessness<-Data$aids14
thoughtsofdeath<-Data$aids16
sex<-Data$Sexe
age<-Data$Age
crp<-Data$ahsCRP
alcohol<-Data$aauditsc
exercise<-Data$AIPMETOT
smoking<-Data$asmokstat
bmi<-Data$abmi
diseases<-Data$anumdis_treat

#Analysis 2a: mgm 9 DSM-IV MDD criteria symptoms, CRP, sex & age, gamma 0.25
#estimate mgm network
Rep2a <- cbind(crp, sadmood, interest, sleep, energy, appetite, moving, concentration, worthlessness, thoughtsofdeath, sex, age)

data_n_2a <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

# Fill in
data_n_2a$data <- Rep2a
data_n_2a$type <- c(rep("g", 10), rep("c", 1), rep("g", 1))
data_n_2a$level <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1)
data_n_2a$names <- c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age")
data_n_2a$labels <- c("IM1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "C1", "C2")
data_n_2a$grouplabels <- list("Inflammation Marker" = c(1), "DSM Depression Criteria" = c(2:10), "Covariates" = c(11:12))   

saveRDS(data_n_2a, file = paste0(dataPrDir, "data_n_2a.RDS"))



# -------------------------------------------------------------------------
# --------------- Get Data for Network 2b ---------------------------------
# -------------------------------------------------------------------------

#Analysis 2b: mgm 9 Depression Symptoms, CRP, all covariates, gamma = 0.25
#estimate mgm network
Rep2b <- cbind(crp, sadmood, interest, sleep, energy, appetite, moving, concentration, worthlessness, thoughtsofdeath, sex, age, alcohol, exercise, smoking, bmi, diseases)


data_n_2b <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

# Fill in
data_n_2b$data <- Rep2b
data_n_2b$type <- c(rep("g", 10), rep("c", 1), rep("g", 5), rep("p", 1))
data_n_2b$level <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1)
data_n_2b$names <- c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
data_n_2b$labels <- c("IM1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
data_n_2b$grouplabels <- list("Inflammation Marker" = c(1), "DSM Depression Criteria" = c(2:10), "Covariates" = c(11:17))

saveRDS(data_n_2b, file = paste0(dataPrDir, "data_n_2b.RDS"))



# -------------------------------------------------------------------------
# --------------- Get Data for Network 3a ---------------------------------
# -------------------------------------------------------------------------

data_n_3a <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

# Fill in
data_n_3a$data <- as.matrix(subset(Data, select = c(2:4, 6:33)))
data_n_3a$type <- c(rep("g", 31))
data_n_3a$level <- c(rep(1, 31))
data_n_3a$names <- c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest", "energy level", "capacity for pleasure", "interest in sex", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy")
data_n_3a$labels <- c("IM1", "IM2", "IM3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28")
data_n_3a$grouplabels <- list("Inflammatory Markers" = c(1:3), "Depressive Symptoms" = c(4:31))   

saveRDS(data_n_3a, file = paste0(dataPrDir, "data_n_3a.RDS"))



# -------------------------------------------------------------------------
# --------------- Get Data for Network 3b ---------------------------------
# -------------------------------------------------------------------------

data_n_3b <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)

# Fill in
data_n_3b$data <- as.matrix(subset(Data, select = c(2:4, 6:40)))
data_n_3b$type <- c(rep("g", 31), rep("c", 1), rep("g", 5), rep("p", 1))
data_n_3b$level <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1)
data_n_3b$names <- c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest", "energy level", "capacity for pleasure", "interest in sex", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
data_n_3b$labels <- c("IM1", "IM2", "IM3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
data_n_3b$grouplabels <- list("Inflammatory Markers" = c(1:3), "Depressive Symptoms" = c(4:31), "Covariates" = c(32:38))

saveRDS(data_n_3b, file = paste0(dataPrDir, "data_n_3b.RDS"))



# -------------------------------------------------------------------------
# --------------- Descriptives --------------------------------------------
# -------------------------------------------------------------------------

datax <- data_n_3b$data
view(dfSummary(datax))



#             |￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                  This is the end of  
#                 part 1: preprocessing
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ


