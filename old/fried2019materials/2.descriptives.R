##################################################################################
#                                                                                #
#           Using network analysis to examine links between individual           #
#            depressive symptoms, inflammatory markers, and covariates           #   
#                                                                                #
#                            Fried et al., 2019                                  #
#                          Psychological Medicine                                #
#                           https://osf.io/6ehrm/                                #
#                                                                                #
#                  2. Syntax file for descriptive statistics                     #
#                                                                                #
#                         Last updated: 08-09-2019                               #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- Loading packages & Data ---------------------------------
# -------------------------------------------------------------------------

library(foreign)
library(qgraph)
library(bootnet)
library(dplyr)
library(markdown)
library(huge)

# set working directories
setwd("~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis")
dataDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/data_raw/"
dataPrDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/data_processed/"



# -------------------------------------------------------------------------
# --------------- Process Data for Analysis of Sample Characteristics -----
# -------------------------------------------------------------------------

# Load data + collapse one single dataframe
file_names <- list.files(dataDir);file_names
ID <- lapply(paste0(dataDir, file_names), read.spss, to.data.frame = T, use.value.labels = F)
ID <- bind_cols(ID)

# Subset data to create data set with variables of interest
Data <- subset(ID, select = c(pident, ahsCRP, aIL6, aTNFa, aids, aids01, aids02, aids03, aids04, aids05, aids06, aids07, aids08, aids09a, aids10, aids11, aids12, aids13, aids14, aids15, aids16, aids17, aids18, aids19, aids20, aids21, aids22, aids23, aids24, aids25, aids26, aids27, aids28, Sexe, Age, aauditsc, AIPMETOT, asmokstat, abmi, anumdis_treat))

# Recode variables and remove missing values
Data[, 34]<-Data[, 34] - 1 #code sex as 0 and 1
Data$aids11<-recode(Data$aids11, "5"=2, "6"=3, "7"=4)#recode appetite
Data$aids12<-recode(Data$aids12, "5"=2, "6"=3, "7"=4)#recode weight
Data<-subset(Data, ahsCRP<10)#remove CRP values above 10 to exclude PP with acute infection; n = 2787

# Delete missing rows
Data <- na.omit(Data)#206 NA omitted; n = 2581
Data <- Data[!apply(Data<0, 1, any), ]#further 260 omitted; n = 2321

# Apply nonparanormal transformation to skewed distributions
Data[, 2:5]<-huge.npn(Data[, 2:5]) # normalize markers; IDS total
Data[, 36:37]<-huge.npn(Data[, 36:37]) # normalize exercise; alcohol



# -------------------------------------------------------------------------
# --------------- Sample Characteristics ----------------------------------
# -------------------------------------------------------------------------

# sex
table(Data$Sexe)
Men<-subset(Data, Data$Sexe == 0)
Women<-subset(Data, Data$Sexe == 1)

# age women
mean(Women$Age)
sd(Women$Age)
median(Women$Age)
hist(Women$Age)

# age men
mean(Men$Age)
sd(Men$Age)
median(Men$Age)
hist(Men$Age)
range(Data$Age)  # total age range

# body mass index (BMI)
mean(Data$abmi)
sd(Data$abmi)
median(Data$abmi)

# IDS total score distribution
quantile(Data$aids)

# chronic diseases
quantile(Data$anumdis_treat)

# alcohol consumption
mean(Data[, 36])
sd(Data[, 36])
median(Data[, 36])

# exercise
mean(Data[, 37])
sd(Data[, 37])
median(Data[, 37])

# smoking
table(Data$asmokstat)

# inflammatory markers
hist(Data$ahsCRP)
hist(Data$aIL6)
hist(Data$aTNFa)

# inflammatory marker correlations
cor(Data[,c(2:4)])



#             |￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                  This is the end of  
#                 part 2: descriptives
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ


