

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


# DEPR DIAGNOSIS ---------------------------------------------------------------

dat <- dat %>%
  mutate(depr_diag = case_when(depr_diag == 1 ~ 1,  #YES
                               depr_diag == 2 ~ 0,  #NO
                               depr_diag == 7 ~ NA_real_,
                               depr_diag == 9 ~ NA_real_))

# NAs = 63



# DEPR MED ----------------------------------------------------------------

dat <- dat %>%
  mutate(depr_med = case_when(depr_med == 1 ~ 1,  #YES
                              depr_med == 2 ~ 0,  #NO
                              depr_med == 7 ~ NA_real_,
                              depr_med == 8 ~ NA_real_,
                              depr_med == 9 ~ NA_real_))



# ANXIETY DISORDER DIAGNOSIS ----------------------------------------------------------------

dat <- dat %>%
  mutate(anx_diag = case_when(anx_diag == 1 ~ 1,  #YES
                              anx_diag == 2 ~ 0,  #NO
                              anx_diag == 7 ~ NA_real_,
                              anx_diag == 9 ~ NA_real_))


# NAs = 62


# ANXIETY MED -------------------------------------------------------------

dat <- dat %>%
  mutate(anx_med = case_when(anx_med == 1 ~ 1,  #YES
                             anx_med == 2 ~ 0,  #NO
                             anx_med == 7 ~ NA_real_,
                             anx_med == 8 ~ NA_real_,
                             anx_med == 9 ~ NA_real_))



# PHQ 8 -------------------------------------------------------------------


phqfun <- function(df, PHQ){
  df <- df %>%
    mutate(!!sym(PHQ) := case_when(!!sym(PHQ) == 1 ~ 0, #not at all
                                  !!sym(PHQ) == 2 ~ 1, #several days
                                  !!sym(PHQ) == 3 ~ 2, #more than half days
                                  !!sym(PHQ) == 4 ~ 3, # nearly every day
                                  !!sym(PHQ) == 7 ~ NA_real_,
                                  !!sym(PHQ) == 8 ~ NA_real_,
                                  !!sym(PHQ) == 9 ~ NA_real_))
  return(df)
}


#PHQ1- How often little interest in things, past 2 weeks
dat <- phqfun(dat, 'phq1')

#PHQ2- How often feeling down, past 2 weeks
dat <- phqfun(dat, 'phq2')

#PHQ3- How often trouble with sleeping, past 2 weeks
dat <- phqfun(dat, 'phq3')

#PHQ4- How often feeling tired, past 2 weeks 
dat <- phqfun(dat, 'phq4')

#PHQ5- How often undereating or overeating, past 2 weeks
dat <- phqfun(dat, 'phq5')

#PHQ6- How often feeling bad about self, past 2 weeks
dat <- phqfun(dat, 'phq6')

#PHQ7- How often trouble concentrating, past 2 weeks
dat <- phqfun(dat, 'phq7')

#PHQ8- How often moving or speaking slow or fast, past 2 weeks
dat <- phqfun(dat, 'phq8')


# PHQ SUM SCORE

dat$phq_sum <- dat$phq1 + dat$phq2 + dat$phq3 + dat$phq4 +
  dat$phq5 + dat$phq6 + dat$phq7 + dat$phq8


# GAD 7 -------------------------------------------------------------------



gadfun <- function(df, GAD){
  df <- df %>%
    mutate(!!sym(GAD) := case_when(!!sym(GAD) == 1 ~ 0, #not at all
                                   !!sym(GAD) == 2 ~ 1, #several days
                                   !!sym(GAD) == 3 ~ 2, #more than half days
                                   !!sym(GAD) == 4 ~ 3, # nearly every day
                                   !!sym(GAD) == 7 ~ NA_real_,
                                   !!sym(GAD) == 8 ~ NA_real_,
                                   !!sym(GAD) == 9 ~ NA_real_))
  return(df)
}


# GAD1- How often felt nervous/anxious/on edge, past 2 weeks
dat <- gadfun(dat, 'gad1')

# GAD2- How often can't stop/control worrying, past 2 weeks
dat <- gadfun(dat, 'gad2')

# GAD3- How often worrying too much, past 2 weeks
dat <- gadfun(dat, 'gad3')

# GAD4- How often had trouble relaxing, past 2 weeks
dat <- gadfun(dat, 'gad4')

# GAD5- How often can't sit still, past 2 weeks
dat <- gadfun(dat, 'gad5')

# GAD6- How often became easily annoyed, past 2 weeks 
dat <- gadfun(dat, 'gad6')

# GAD7- How often felt afraid, past 2 weeks 
dat <- gadfun(dat, 'gad7')



# GAD sum score
dat$gad_sum <- dat$gad1 + dat$gad2 + dat$gad3 + dat$gad4 +
  dat$gad5 + dat$gad6 + dat$gad7




# split sample M F --------------------------------------------------------
library(tidyverse)


#save(dat, file = 'nhis19_clean.rdata')
load('nhis19_clean.rdata')


dat_m <- dat %>%
  filter(sex == 1)

dat_f <- dat %>%
  filter(sex == 0)



# select variables --------------------------------------------------------

## just phq
dat_m1 <- dat_m %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8)

dat_f1 <- dat_f %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8)


## phq + gad
dat_m2 <- dat_m %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         gad1, gad2, gad3, gad4, gad5, gad6, gad7)

dat_f2 <- dat_f %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         gad1, gad2, gad3, gad4, gad5, gad6, gad7)


## phq  + covariates
dat_m3 <- dat_m %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         age, edu, race, bmi, pain_freq, genhlth, depr_med)

dat_f3 <- dat_f %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         age, edu, race, bmi, pain_freq, genhlth, depr_med)



nameslong <- c("How often little interest in things",
               "How often feeling down",
               "How often trouble with sleeping",
               "How often feeling tired",
               "How often undereating or overeating",
               "How often feeling bad about self",
               "How often trouble concentrating",
               "How often moving or speaking slow or fast")


# estimate PHQ networks -------------------------------------------------------

library(qgraph)       
library(mgm)         
library(bootnet)         
library(networktools) 
library(NetworkComparisonTest)



# male --------------------------------------------------------------------


net_m1 <- estimateNetwork(dat_m1, default = "EBICglasso",
                        missing = "pairwise",
                        signed = T)



#save(net1, file='net1.rdata')

netplot_m1 <- plot(net_m1, layout = "circle", vsize = 5.5, 
                 border.color="black",
                 nodeNames = nameslong,
                 legend.mode = "names",
                 label.cex = 1.2,
                 legend.cex = .85,
                 layoutOffset = c(0,0),
                 GLratio = 2.0,
                 width = 12, height = 9,
                 filetype = "pdf", filename = "netplot_m1",
                 plot = T)




pdf('ei_m1.pdf', width = 4, height = 5)
ei_m1 <- centralityPlot(netplot_m1, include = c("ExpectedInfluence"), scale = 'z-scores',
                           labels = nameslong)
dev.off()



# female ------------------------------------------------------------------


net_f1 <- estimateNetwork(dat_f1, default = "EBICglasso",
                          missing = "pairwise",
                          signed = T)



#save(net1, file='net1.rdata')

netplot_f1 <- plot(net_f1, layout = "circle", vsize = 5.5, 
                   border.color="black",
                   nodeNames = nameslong,
                   legend.mode = "names",
                   label.cex = 1.2,
                   legend.cex = .85,
                   layoutOffset = c(0,0),
                   GLratio = 2.0,
                   width = 12, height = 9,
                   filetype = "pdf", filename = "netplot_f1",
                   plot = T)




pdf('ei_f1.pdf', width = 4.2, height = 5)
ei_f1 <- centralityPlot(netplot_f1, include = c("ExpectedInfluence"), scale = 'z-scores',
                        labels = nameslong)
dev.off()






nct1 <- NCT(net_m1, net_f1, it = 500, weighted = T,
            test.edges = T, edges = "all",
            test.centrality = T,
            centrality = c("expectedInfluence"), nodes = "all",
            
            progressbar = T,
            verbose = T)






# PHQ + covariates --------------------------------------------------------
#####################
######################
#########################
############################
############

## phq + gad + covariates
dat_m3 <- dat_m %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         age, edu, race, bmi, pain_freq, genhlth, depr_med)

dat_f3 <- dat_f %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         age, edu, race, bmi, pain_freq, genhlth, depr_med)



nameslong3 <- c("How often little interest in things",
               "How often feeling down",
               "How often trouble with sleeping",
               "How often feeling tired",
               "How often undereating or overeating",
               "How often feeling bad about self",
               "How often trouble concentrating",
               "How often moving or speaking slow or fast",
               "Age", "Edu", "Race", "BMI",
               "Pain Frequency", "General Health",
               "Take medication for depression")



# male --------------------------------------------------------------------


net_m3 <- estimateNetwork(dat_m3, default = "EBICglasso",
                          missing = "pairwise",
                          signed = T)





netplot_m3 <- plot(net_m3, layout = "circle", vsize = 5.5, 
                   border.color="black",
                   nodeNames = nameslong3,
                   legend.mode = "names",
                   label.cex = 1.2,
                   legend.cex = .85,
                   layoutOffset = c(0,0),
                   GLratio = 2.0,
                   width = 12, height = 9,
                   filetype = "pdf", filename = "netplot_m3",
                   plot = T)




pdf('ei_m3.pdf', width = 4, height = 5)
ei_m3 <- centralityPlot(netplot_m3, include = c("ExpectedInfluence"), scale = 'z-scores',
                        labels = nameslong3)
dev.off()



# female ------------------------------------------------------------------


net_f3 <- estimateNetwork(dat_f3, default = "EBICglasso",
                          missing = "pairwise",
                          signed = T)





netplot_f3 <- plot(net_f3, layout = "circle", vsize = 5.5, 
                   border.color="black",
                   nodeNames = nameslong3,
                   legend.mode = "names",
                   label.cex = 1.2,
                   legend.cex = .85,
                   layoutOffset = c(0,0),
                   GLratio = 2.0,
                   width = 12, height = 9,
                   filetype = "pdf", filename = "netplot_f3",
                   plot = T)




pdf('ei_f3.pdf', width = 4.2, height = 5)
ei_f3 <- centralityPlot(netplot_f3, include = c("ExpectedInfluence"), scale = 'z-scores',
                        labels = nameslong3)
dev.off()






nct3 <- NCT(net_m3, net_f3, it = 500, weighted = T,
            test.edges = T, edges = "all",
            test.centrality = T,
            centrality = c("expectedInfluence"), nodes = "all",
            
            progressbar = T,
            verbose = T)

summary(nct3)


plot(nct3, what = "network")








# network tree ------------------------------------------------------------
library(networktree)



<<<<<<< HEAD


dat_0 <- dat %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         sex)



f <- paste(paste0(colnames(select(dat_0, -c(sex))),
                  collapse = " + "), "~ sex")

f <- as.formula(f)


tr_0 <- networktree(f, data = dat_0, model = "correlation",
                    transform = "glasso",
                    maxdepth = 2)


save(tr_0, file = "tree_0.RData")


print(tr_0, digits = 2)


comparetree(tr_0, id1=2, id2=3, plot=TRUE,
            highlights = 15,
            layout="circle",
            plot.type="compare")




















# network tree with phq + covariates --------------------------------------



dat_1 <- dat %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         sex, age, edu, race, bmi, pn_fq, genhlth, d_med)



f <- paste(paste0(colnames(select(dat_1, -c(sex))),
                  collapse = " + "), "~ sex")

f <- as.formula(f)


tr_1 <- networktree(f, data = dat_1, model = "correlation",
                    transform = "glasso",
                    maxdepth = 2)


save(tr_1, file = "tree_1.RData")


print(tr_1, digits = 2)


comparetree(tr_1, id1=2, id2=3, plot=TRUE,
            highlights = 15,
            layout="circle",
            plot.type="compare")









=======
dat1 <- dat %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         sex, age, edu, bmi, pain_freq, genhlth, depr_med)


f <- paste(paste0(colnames(select(dat1, -c(sex))),
                  collapse = " + "), "~ sex")

f  <- as.formula(f)
f

tr1 <- networktree(f, data = dat1, model = "correlation", maxdepth = 3,
                  transform = "glasso")


save(tr1, file = "tr1.RData")


print(tr1, digits = 2)



## Add a comparison plot
comparetree(tr1, id1=2, id2=3, plot=TRUE,
            highlights=10,
            detect="glasso",
            layout="circle"
            )

# $highlights
# node1     node2         id1         id2 (id1 - id2)
# 1    age       edu -0.04640757  0.04298341 -0.08939098
# 2    age      phq4 -0.08804541 -0.02522512 -0.06282029
# 3   phq2      phq6  0.30228351  0.36256175 -0.06027825
# 4    age pain_freq  0.20312983  0.14904300  0.05408683
# 5    bmi       edu -0.06974628 -0.01689640 -0.05284988
# 6    age   genhlth -0.18184515 -0.23453221  0.05268705
# 7   phq7      phq8  0.25097030  0.30039354 -0.04942324
# 8    bmi  depr_med  0.06384389  0.01508117  0.04876272
# 9   phq6      phq7  0.15671979  0.11016163  0.04655816
# 10   bmi   genhlth -0.14904626 -0.10995419 -0.03909207


# phq, gad, covs ----------------------------------------------------------


dat2 <- dat %>%
  select(phq1, phq2, phq3, phq4, phq5, phq6, phq7, phq8,
         gad1, gad2, gad3, gad4, gad5, gad6, gad7,
         sex, age, edu, bmi, pain_freq, genhlth, depr_med)



f <- paste(paste0(colnames(select(dat2, -c(sex))),
                  collapse = " + "), "~ sex")

f  <- as.formula(f)
f

tr2 <- networktree(f, data = dat2, model = "correlation", maxdepth = 3,
                   transform = "glasso")


save(tr1, file = "tr1.RData")


print(tr2, digits = 2)



## Add a comparison plot
comparetree(tr2, id1=2, id2=3, plot=TRUE,
            highlights=10,
            layout="circle"
)



# $highlights
# node1    node2         id1         id2 (id1 - id2)
# 1    age      edu -0.04530132  0.04298692 -0.08828824
# 2   gad2     gad3  0.49524201  0.41294378  0.08229823
# 3    age  genhlth -0.17985040 -0.23507181  0.05522141
# 4    age     gad6 -0.11450520 -0.06053078 -0.05397442
# 5    bmi      edu -0.06700364 -0.01570665 -0.05129700
# 6    age     phq4 -0.05017215  0.00000000 -0.05017215
# 7   phq7     phq8  0.19924970  0.24913959 -0.04988990
# 8    bmi depr_med  0.06528518  0.01607549  0.04920969
# 9   gad5     gad6  0.07253333  0.12130924 -0.04877591
# 10  phq6     phq7  0.10322112  0.05580763  0.04741348
# 
# 

>>>>>>> a7875ae5088a540b7b8add58d9196a3bb0bd8a54





























