library(tidyverse)
library(qgraph)          #visualize networks
library(mgm)           #mixed graphical modeling
library(bootnet)         # runs all basic network analyses
library(NetworkComparisonTest)  #compare networks
library(networktools)         #bridge symptoms, goldbricker for node select
library(networktree)



############### data cleaning ###############
load("dat.RData")

dat <- dat %>%
  subset(Country == "United States"|
         Country == "United Kingdom"|
         Country == "France"|
         Country == "Australia"|
         Country == "Canada"|
         Country == "Germany"|
         Country == "New Zealand"|
         Country == "Ireland"|
         Country == "Spain"|
         Country == "Italy"|
         Country == "Portugal"|
         Country == "Albania"|
         Country == "Switzerland"|
         Country == "Austria"|
         Country == "Netherlands"|
         Country == "Poland"|
         Country == "Armenia"|
         Country == "Finland"|
         Country == "Denmark")



mhm <-data.frame()[1:nrow(dat), ]  # create new DF with x rows and 0 cols

mhm <- add_column( mhm,
                   
    
                   
                   selfworth_and_confidence =  dat$Self.Worth...Confidence,
                   drive_and_motivation =   dat$Drive...Motivation.1,
                   sleep_quality = dat$Sleep.Quality ,
                   relationships_with_others =   dat$Relationships.with.others,
                   emotional_resilience =    dat$Emotional.Resilience,
                   energy_levels =    dat$Energy.Level,
                   focus_and_concentration =   dat$Focus...Concentration,
                   appetite_regulation =   dat$Appetite.Regulation,
                   outlook_and_optimism = dat$Outlook...Optimism,
                   restless_and_hyperactivity  =   dat$Restlessness...Hyperactivity,
                   fear_and_anxiety =  dat$Fear...Anxiety,
                   avoidance_and_withdrawl = dat$Avoidance...Withdrawal,
                   anger_and_irritability =   dat$Anger...Irritability,
                   suicidal_thoughts_or_intentions =   dat$Suicidal.Thoughts.or.Intentions,
                   physical_pain = dat$Experience.of.Pain,
                   guilt_and_blame =   dat$Guilt...Blame ,
                   sadness_distress_hopelessness =  dat$Feelings.of.Sadness..Distress.or.Hopelessness ,
                   physical_health_issues =    dat$Physical.Health.Issues ,
                 
                   
                   
                   sex = dat$Biological.Sex,
                   employment = dat$Employment,
                   relationship_status = dat$Current.Family.Situation,
                   seeking_mental_health_treatment = dat$Mental.Health.Treatment.Status,
                   medical_diagnosis = dat$Presence.Absence.of.Diagnosed.Medical.Disorder,
                   childhood_trauma = dat$Childhood.traumas,
                   exercise_frequency = dat$Frequency.of.doing.exercise,
                   good_nights_sleep_frequency = dat$Frequency.of.getting.a.good.nights.sleep,
                   frequency_of_socializing = dat$Frequency.of.Socializing,
                   age = dat$Age,
                   educational_attainment = dat$Education,
                 
                   )






###########



summary(mhm$sex)
mhm <- mhm %>%
  mutate(sex = case_when(sex == "Female" ~ 0,
                         sex == "Male" ~ 1))




summary(mhm$age)
mhm$age <- as.integer(mhm$age)



summary(mhm$educational_attainment)
mhm <- mhm %>%
  subset(educational_attainment == "Primary Education"|
         educational_attainment == "Some High School" |
         educational_attainment == "High School" |
         educational_attainment == "Vocational certification" |
         educational_attainment == "Associateâ€™s Degree" |
         educational_attainment == "Bachelor's Degree" |
         educational_attainment == "Master's Degree" |
         educational_attainment == "PhD" |
         educational_attainment == "J.D" |
         educational_attainment == "M.D."    )

mhm <- droplevels(mhm)

mhm <- mhm %>%              
  mutate(educational_attainment = case_when(educational_attainment == "Primary Education" ~ 1,
                               educational_attainment == "Some High School" ~ 1,
                               educational_attainment == "High School" ~ 2,
                               educational_attainment == "Vocational certification" ~ 3,
                               educational_attainment == "Associateâ€™s Degree" ~ 3,
                               educational_attainment == "Bachelor's Degree" ~ 4,
                               educational_attainment == "Master's Degree" ~ 5,
                               educational_attainment == "PhD" ~ 5,
                               educational_attainment == "J.D" ~ 5,
                               educational_attainment == "M.D." ~ 5))
summary(mhm$educational_attainment)



summary(mhm$employment)
mhm <- mhm %>%              
  mutate(employment = case_when(employment == "Employed /Self employed" ~ 1,
                            employment == "Retired" ~ 1,
                            employment == "Homemaker" ~ 1,
                            employment == "Studying" ~ 1,
                            employment == "Not able to work" ~ 0,
                            employment == "Unemployed" ~ 0))



summary(mhm$exercise_frequency)
mhm <- mhm %>%              
  mutate(exercise_frequency = case_when(exercise_frequency == "Rarely/Never" ~ 1,
                                        exercise_frequency == "Less than once a week" ~ 2,
                                        exercise_frequency == "Once a week" ~ 3,
                                        exercise_frequency == "Few days a week" ~ 4,
                                        exercise_frequency == "Every day" ~ 5))


summary(mhm$good_nights_sleep_frequency)
mhm <- mhm %>%
  mutate(good_nights_sleep_frequency = case_when(good_nights_sleep_frequency == "Hardly ever" ~ 1,
                                                 good_nights_sleep_frequency == "Some of the time" ~ 2,
                                                 good_nights_sleep_frequency == "Most of the time" ~ 3,
                                                 good_nights_sleep_frequency == "All of the time" ~ 4))


summary(mhm$frequency_of_socializing)
mhm <- mhm %>%
  mutate(frequency_of_socializing = case_when(frequency_of_socializing == "Rarely/Never" ~ 1,
                                              frequency_of_socializing == "1-3 times a month" ~ 2,
                                              frequency_of_socializing == "Once a week" ~ 3,
                                              frequency_of_socializing == "Several days a week" ~ 4))


summary(mhm$relationship_status)
mhm <- mhm %>%
  mutate(relationship_status = case_when(relationship_status == "Divorced/Separated" ~ 0,
                                         relationship_status == "Single (never married or in a civil partnership)" ~ 0,
                                         relationship_status == "Widowed" ~ 0,
                                         relationship_status == "Married/Civil Partnership" ~ 1,
                                         relationship_status == "In a relationship" ~ 1,
                                         relationship_status == "In a cohabiting relationship" ~ 1))



summary(mhm$seeking_mental_health_treatment)
mhm <- mhm %>%
  mutate(seeking_mental_health_treatment = case_when(seeking_mental_health_treatment == "No" ~ 0,
                                                    seeking_mental_health_treatment == "Yes" ~ 1))


summary(mhm$medical_diagnosis)
mhm <- mhm %>%
  mutate(medical_diagnosis = case_when(medical_diagnosis == "No" ~ 0,
                                       medical_diagnosis == "Yes" ~ 1))


mhm$childhood_trauma <- 
  if_else((mhm$childhood_trauma == "|I did not experience any of the above during my childhood")|(mhm$childhood_trauma == "|None of the above"), 
          0, 1)

summary(mhm$childhood_trauma)


summary(mhm)
mhm <- na.omit(mhm)  # n = 71951 to n = 66620


save(mhm, file = "mhm_deprcovs.RData")


# ####################
# ####################   GOLDBRICKER (IDENTIFY REDUNDANT ITEMS)
# ####################
# 
gb <- goldbricker(mhm, p = 0.05, method = "hittner2003",
            threshold = 0.25, corMin = 0.5, progressbar = T)

gb
# Suggested reductions: Less than 25 % of correlations are significantly different for the following pairs:
#   [1] "No suggested reductions"







depr_items <- list("data" = NULL, 
                  "type" = NULL, 
                  "level" = NULL,
                  "names" = NULL,
                  "labels" = NULL,
                  "grouplabels"= NULL)


depr_items$data <- as.matrix(subset(mhm, select = c(1:18)))
depr_items$type <- c(rep("g", 18))
depr_items$level <- c(rep(1, 18))
depr_items$names <- c(colnames(depr_items$data))
depr_items$labels <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10",
                      "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18" )



depr_network <- mgm(data = depr_items$data, 
                              type = depr_items$type, 
                              level = depr_items$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = .25)  #conservative edge weight estimates



#sum of absolute value of edge weights
sum(abs((depr_network$pairwise$wadj)))  # = 19.9

# Compute Predictability
# predictability indicates the amount variance in a given variable that is explained by 
# all nodes it connects with
Pred1a <- predict(depr_network, depr_items$data)
Pred1a$errors
pie1a<- as.numeric(as.character(Pred1a$errors[1:18, 3]))#predictability estimates
mean(pie1a) #0.495


depr_graph <- qgraph(depr_network$pairwise$wadj, layout = "spring", cut=0,
                  nodeNames=depr_items$names, labels = depr_items$labels,
                  vsize=5,
                  label.cex = 1,
                  label.scale = F,
                  label.scale.equal = F,
                  label.prop = 1,
                  edge.color = depr_network$pairwise$edgecolor,
                  pie = pie1a, pieBorder = 0.25) 












#################                       ##########################
#################     ADD COVARIATES    ##########################
#################                       ##########################

depr_covs <- list("data" = NULL, 
                   "type" = NULL, 
                   "level" = NULL,
                   "names" = NULL,
                   "labels" = NULL,
                   "grouplabels"= NULL)


depr_covs$data <- as.matrix(mhm)
depr_covs$type <- c(rep("g", 18), rep("c", 6), rep("g", 5))
depr_covs$level <- c(rep(1, 18), rep(2, 6), rep(1, 5))
depr_covs$names <- c(colnames(depr_items$data), "sex: 0=F, 1=M", "employment: 0=unemployed, 1=employed",
                     "relationship status: 0=single, 1=relationship", "seeking mental health treatment: 0=no, 1=yes",
                     "medical diagnosis: 0=no, 1=yes", "childhood trauma: 0=no, 1=yes",
                     "exercise frequency", "good nights sleep frequency", "frequency of socializing",
                     "age", "educational attainment")
depr_covs$labels <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10",
                       "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18",
                      "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11")
depr_covs$grouplabels <- list("Depressive Symptoms" = c(1:18), "Covariates" = c(19:29))   



depr_covs_network <- mgm(data = depr_covs$data, 
                    type = depr_covs$type, 
                    level = depr_covs$level, 
                    lambdaSel = "EBIC", 
                    lambdaGam = .25,
                    binarySign = T)



#sum of edge weights 
sum(abs(depr_covs_network$pairwise$wadj)) # = 43.62
# just depression items
sum(abs(depr_covs_network$pairwise$wadj[c(1:18), c(1:18)]))   # = 18.35

18.35/19.9 # = reduced 92.2%


# Compute Predictability
Pred1b <- predict(depr_covs_network, depr_covs$data)
Pred1b$errors

pie1bb<- as.numeric(as.character(Pred1b$errors[1:18, 3]))
pie1bbb<- as.numeric(as.character(Pred1b$errors[19:24, 5]))
pie1bbbb<- as.numeric(as.character(Pred1b$errors[25:29, 3]))

pie1b <- c(pie1bb, pie1bbb, pie1bbbb)#predictability estimates as one piece
mean(pie1b)  #0.383






depr_covs_graph <- qgraph(depr_covs_network$pairwise$wadj, layout = "spring", 
                          cut=0, maximum = 0.6, 
                   groups=depr_covs$grouplabels, palette = "colorblind",
                   nodeNames=depr_covs$names, labels = depr_covs$labels, 
                   vsize=4.5,
                   label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                   edge.color = depr_covs_network$pairwise$edgecolor,
                   pie = pie1b, pieBorder = 0.25) 





# Network Comparison ------------------------------------------------------

# sum of edge weights
sum(abs((depr_network$pairwise$wadj)))  # = 19.9
sum(abs(depr_covs_network$pairwise$wadj[c(1:18), c(1:18)]))   # = 18.35

18.35/19.9 # = reduced 92.2% by conditioning on covariates


cor(as.vector(depr_network$pairwise$wadj), 
    as.vector(depr_covs_network$pairwise$wadj[c(1:18),c(1:18)]))
# 0.987





















































