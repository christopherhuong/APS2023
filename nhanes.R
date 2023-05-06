library(rio)
library(tidyverse)




dat1 <- import('nhanes_dpq.XPT')
dat2 <- import('nhanes_demo.XPT')
dat3 <- import('nhanes_pa.XPT')
dat4 <- import('nhanes_bmi.XPT')


dat <- inner_join(dat1, dat2, by = "SEQN")
dat <- inner_join(dat, dat3, by = "SEQN")
dat <- inner_join(dat, dat4, by = "SEQN")
rm(dat1,dat2,dat3,dat4)



dat <- dat %>%
  select(RIAGENDR, RIDAGEYR, DMDEDUC2, PAD680,
         WHD010, WHD020,
    
         DPQ010, DPQ020, DPQ030, DPQ040,  DPQ050,  
         DPQ060, DPQ070, DPQ080, DPQ090
         )


colnames(dat) <- c("SEX", "AGE", "EDU", "SED",
                   "HT", "WT",
                   
                   "PHQ1", "PHQ2", "PHQ3", "PHQ4", "PHQ5",
                   "PHQ6", "PHQ7", "PHQ8", "PHQ9")




# SEX ---------------------------------------------------------------------

dat <- dat %>%
  mutate(SEX = case_when(SEX == 1 ~ 1, #M
                         SEX == 2 ~ 0  #F
                         ))


# AGE ---------------------------------------------------------------------

# integer


# EDU ---------------------------------------------------------------------

dat <- dat %>%
  mutate(EDU = case_when(EDU == 1 ~ 1, #less than 9th grade
                         EDU == 2 ~ 2, #grade 9-12, no diploma
                         EDU == 3 ~ 3, #HS / GED
                         EDU == 4 ~ 4, #some college / AA
                         EDU == 5 ~ 5, #college or above
                         EDU == 7 ~ NA_real_,
                         EDU == 9 ~ NA_real_
  ))


# SED ---------------------------------------------------------------------

dat[, 4][dat[, 4] == 7777] <- NA
dat[, 4][dat[, 4] == 9999] <- NA



# BMI ---------------------------------------------------------------------

#Height (inches)
dat[, 5][dat[, 5] == 7777] <- NA
dat[, 5][dat[, 5] == 9999] <- NA

#Weight (lbs)
dat[, 6][dat[, 6] == 7777] <- NA
dat[, 6][dat[, 6] == 9999] <- NA


# BMI Formula: weight (lb) / [height (in)]^2 x 703

dat <- dat %>%
  mutate(BMI = (WT/HT^2)*703, .after = SED)

dat <- dat %>%
  select(-c(WT,HT))




# PHQ 9 -------------------------------------------------------------------


phqfun <- function(df, PHQ){
  df <- df %>%
    mutate(!!sym(PHQ) := case_when(!!sym(PHQ) == 0 ~ 0, #not at all
                                   !!sym(PHQ) == 1 ~ 1, #several days
                                   !!sym(PHQ) == 2 ~ 2, #more than half days
                                   !!sym(PHQ) == 3 ~ 3, # nearly every day
                                   !!sym(PHQ) == 7 ~ NA_real_,
                                   !!sym(PHQ) == 9 ~ NA_real_))
  return(df)
}



#PHQ1- Have little interest in doing things
dat <- phqfun(dat, 'PHQ1')

#PHQ2- Feeling down, depressed, or hopeless
dat <- phqfun(dat, 'PHQ2')

#PHQ3- Trouble sleeping or sleeping too much
dat <- phqfun(dat, 'PHQ3')

#PHQ4- Feeling tired or having little energy
dat <- phqfun(dat, 'PHQ4')

#PHQ5- Poor appetite or overeating
dat <- phqfun(dat, 'PHQ5')

#PHQ6- Feeling bad about yourself
dat <- phqfun(dat, 'PHQ6')

#PHQ7- Trouble concentrating on things
dat <- phqfun(dat, 'PHQ7')

#PHQ8- Moving or speaking slowly or too fast
dat <- phqfun(dat, 'PHQ8')

#PHQ9- Thoughts you would be better off dead
dat <- phqfun(dat, 'PHQ9')



#save(dat, file = 'nhanes_clean.rdata')
load('nhanes_clean.rdata')

# missingness -------------------------------------------------------------

library(naniar)
gg_miss_var(dat, show_pct = T)


dat <- dat %>%
  drop_na((PHQ1 | PHQ2 | PHQ3 | PHQ4 | PHQ5 | PHQ6 | PHQ7 | PHQ8 | PHQ9))

# dropped 689

# split sample ------------------------------------------------------------

dat_m <- 
  select(filter(dat, SEX == 1), PHQ1, PHQ2, PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9)

dat_f <- 
  select(filter(dat, SEX == 0), PHQ1, PHQ2, PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9)


dat_mcovs <- 
  select(filter(dat, SEX == 1), -SEX)

dat_fcovs <-
  select(filter(dat, SEX == 0), -SEX)


# estimate networks -------------------------------------------------------
library(qgraph)       
library(mgm)         
library(bootnet)         
library(networktools) 












