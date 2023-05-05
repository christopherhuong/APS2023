library(rio)
library(tidyverse)



dat <- import('adult19.csv')


dat <- select(dat,
             SEX_A, AGEP_A, EDUC_A, RACEALLP_A, BMICAT_A,
             
             PAIFRQ3M_A, #pain freq
             PHSTAT_A, #general health status
             
             DEPEV_A, #diagnosed w depression
             ANXEV_A, #diagnosed w anxiety disorder
             DEPFREQ_A, DEPMED_A, DEPLEVEL_A,
             ANXFREQ_A, ANXMED_A, ANXLEVEL_A,
             
             PHQ81_A, PHQ82_A, PHQ83_A, PHQ84_A,
             PHQ85_A, PHQ86_A, PHQ87_A, PHQ88_A,
             
             GAD71_A, GAD72_A, GAD73_A, GAD74_A,
             GAD75_A, GAD76_A, GAD77_A)
               

colnames(dat) <- c("sex", "age", "edu", "race", "bmi",
                   "pain_freq", "genhlth", 
                   "depr_diag", "anx_diag",
                   "depr_freq", "depr_med", "depr_lvl",
                   "anx_freq", "anx_med", "anx_lvl",
                   "phq1", "phq2", "phq3", "phq4",
                   "phq5", "phq6", "phq7", "phq8",
                   "gad1", "gad2", "gad3", "gad4",
                   "gad5", "gad6", "gad7")




# SEX ---------------------------------------------------------------------

dat <- dat %>%
  mutate(sex = case_when(sex == 1 ~ 1,
                         sex == 2 ~ 0,
                         sex == 7 ~ NA_real_))
         
# 1 = male
# 2 = female
# NAs = 3

#   0     1 
# 17261 14733 

# AGE ---------------------------------------------------------------------

# 18-85+

dat[, 2][dat[, 2] == 97] <- NA
dat[, 2][dat[, 2] == 99] <- NA

# NAs = 81


# EDU ---------------------------------------------------------------------


dat <- dat %>%
  mutate(edu = case_when(edu == 0 ~ 0, #never/kinder only
                         edu == 1 ~ 1, #grade 1-11
                         edu == 2 ~ 2, #grade 12, no diploma
                         edu == 3 ~ 3, #GED or equiv
                         edu == 4 ~ 4, #HS diploma,
                         edu == 5 ~ 5, #some college
                         edu == 6 ~ 6, #AA/vocational degree
                         edu == 7 ~ 6, #AA/vocational degree
                         edu == 8 ~ 7, #bachelors degree
                         edu == 9 ~ 8, #graduate degree
                         edu == 10 ~ 8, #graduate degree
                         edu == 11 ~ 8, #graduate degree
                         edu == 97 ~ NA_real_,
                         edu == 99 ~ NA_real_
                         ))

# NAs = 179


# RACE --------------------------------------------------------------------


dat <- dat %>%
  mutate(race = case_when(race == 1 ~ 1,  #white only
                          race == 2 ~ 0,  #non-white
                          race == 3 ~ 0,
                          race == 4 ~ 0,
                          race == 5 ~ 0,
                          race == 6 ~ 0,
                          race == 8 ~ NA_real_))

# NAs = 1555



# BMI ---------------------------------------------------------------------


dat[, 5][dat[, 5] == 9] <- NA
# NAs = 867



# PAIN FREQ ---------------------------------------------------------------

dat[, 6][dat[, 6] == 7] <- NA
dat[, 6][dat[, 6] == 8] <- NA
dat[, 6][dat[, 6] == 9] <- NA

#never, some days, most days, every day

#NAs = 693



# GEN HEALTH --------------------------------------------------------------

dat <- dat %>%
  mutate(genhlth = case_when(genhlth == 1 ~ 5, #5 = excellent
                             genhlth == 2 ~ 4, #4 = very good
                             genhlth == 3 ~ 3, #3 = good
                             genhlth == 4 ~ 2, #2 = fair
                             genhlth == 5 ~ 1, #1 = poor
                             genhlth == 7 ~ NA_real_,
                             genhlth == 9 ~ NA_real_))

# NAs = 22


# DEPR DIAG ---------------------------------------------------------------

dat <-
























