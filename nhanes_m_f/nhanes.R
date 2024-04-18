library(rio)
library(tidyverse)




dat1 <- import('nhanes_m_f/nhanes_dpq.XPT')
dat2 <- import('nhanes_m_f/nhanes_demo.XPT')
dat3 <- import('nhanes_m_f/nhanes_pa.XPT')
dat4 <- import('nhanes_m_f/nhanes_bmi.XPT')


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
load('nhanes_m_f/nhanes_clean.rdata')

# missingness -------------------------------------------------------------

library(naniar)
gg_miss_var(dat, show_pct = T)


dat <- dat %>%
  drop_na((PHQ1 | PHQ2 | PHQ3 | PHQ4 | PHQ5 | PHQ6 | PHQ7 | PHQ8 | PHQ9))

# dropped 689


nameslong <- c("Have little interest in doing this",
               "Feeling down, depressed, or hopeless",
               "Trouble sleeping or sleeping too much",
               "Feeling tired or having little energy",
               "Poor appetite or overeating",
               "Feeling bad about yourself",
               "Trouble concentrating on things",
               "Moving or speaking slowly or too fast",
               "Thoughts you would be better off dead"
)




# split sample ------------------------------------------------------------

dat_m <- 
  select(filter(dat, SEX == 1), PHQ1, PHQ2, PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9)

dat_f <- 
  select(filter(dat, SEX == 0), PHQ1, PHQ2, PHQ3, PHQ4, PHQ5, PHQ6, PHQ7, PHQ8, PHQ9)





# estimate networks -------------------------------------------------------
library(qgraph)       
library(mgm)         
library(bootnet)         
library(networktools) 
library(NetworkComparisonTest)
library(networktree)


# goldbricker for redundant nodes -----------------------------------------


gb <- goldbricker(dat_m, p = 0.01, method = "hittner2003",
                  threshold = 0.25, corMin = 0.5, progressbar = T)
gb

# Suggested reductions: Less than 25 % of correlations are significantly different for the following pairs: 
#   PHQ5 & PHQ3 
#  0.1428571 
# fatigue & irregular appetite. these are different constructs tho?

gb <- goldbricker(dat_f, p = 0.01, method = "hittner2003",
                  threshold = 0.25, corMin = 0.5, progressbar = T)
gb

# Suggested reductions: Less than 25 % of correlations are significantly different for the following pairs: 
#   [1] "No suggested reductions"




# estimate networks -------------------------------------------------------

# males -------------------------------------------------------------------



net_m <- estimateNetwork(dat_m, default = "ggmModSelect",
                        stepwise=T,
                        missing = "pairwise",
                        signed = T)




netplot_m <- plot(net_m, layout = "circle", vsize = 5, 
                 border.color="black",
                 nodeNames = nameslong,
                 filetype = "pdf", filename = "netplot_m",
                 plot = T)




pdf('ei_m.pdf', width = 4, height = 5)
ei_m <- centralityPlot(netplot_m, include = c("ExpectedInfluence"), scale = 'z-scores',
                      labels = nameslong)
dev.off()


# females -----------------------------------------------------------------



net_f <- estimateNetwork(dat_f, default = "ggmModSelect",
                         stepwise=T,
                         missing = "pairwise",
                         signed = T)




netplot_f <- plot(net_f, layout = "circle", vsize = 5, 
                  border.color="black",
                  nodeNames = nameslong,
                  filetype = "pdf", filename = "netplot_f",
                  plot = T)


pdf('ei_f.pdf', width = 4.15, height = 5)
ei_f <- centralityPlot(netplot_f, include = c("ExpectedInfluence"), scale = 'z-scores',
                       labels = nameslong)
dev.off()





# bootstrap ---------------------------------------------------------------






# NCT ---------------------------------------------------------------------



nct1 <- NCT(net_m, net_f, it = 500, weighted = T,
           test.edges = T, edges = "all",
           test.centrality = T,
           centrality = c("expectedInfluence"), nodes = "all",
           
           progressbar = T,
           verbose = T)

save(nct1, file = "nct1.RData")



summary(nct1)





# WITH COVARIATES ---------------------------------------------------------



nameslongcovs <- c("Age", "Education", "Sedentary", "BMI",
               "Have little interest in doing this",
               "Feeling down, depressed, or hopeless",
               "Trouble sleeping or sleeping too much",
               "Feeling tired or having little energy",
               "Poor appetite or overeating",
               "Feeling bad about yourself",
               "Trouble concentrating on things",
               "Moving or speaking slowly or too fast",
               "Thoughts you would be better off dead"
)




dat_mcovs <- 
  select(filter(dat, SEX == 1), -SEX)

dat_fcovs <-
  select(filter(dat, SEX == 0), -SEX)



# estimate male networks --------------------------------------------------




net_mcovs <- estimateNetwork(dat_mcovs, default = "EBICglasso",
                         missing = "pairwise",
                         signed = T)




netplot_mcovs <- plot(net_mcovs, layout = "circle", vsize = 5, 
                  border.color="black",
                  nodeNames = nameslongcovs,
                  filetype = "pdf", filename = "netplot_mcovs",
                  plot = T)




pdf('ei_mcovs.pdf', width = 4, height = 5)
ei_mcovs <- centralityPlot(netplot_mcovs, include = c("ExpectedInfluence"), scale = 'z-scores',
                       labels = nameslongcovs)
dev.off()


# females -----------------------------------------------------------------



net_fcovs <- estimateNetwork(dat_fcovs, default = "EBICglasso",
                         missing = "pairwise",
                         signed = T)




netplot_fcovs <- plot(net_fcovs, layout = "circle", vsize = 5, 
                  border.color="black",
                  nodeNames = nameslongcovs,
                  filetype = "pdf", filename = "netplot_fcovs",
                  plot = T)


pdf('ei_fcovs.pdf', width = 4.15, height = 5)
ei_fcovs <- centralityPlot(netplot_fcovs, include = c("ExpectedInfluence"), scale = 'z-scores',
                       labels = nameslongcovs)
dev.off()





# bootstrap ---------------------------------------------------------------






# NCT ---------------------------------------------------------------------



nct1covs <- NCT(net_mcovs, net_fcovs, it = 500, weighted = T,
            test.edges = T, edges = "all",
            test.centrality = T,
            centrality = c("expectedInfluence"), nodes = "all",
            
            progressbar = T,
            verbose = T)

save(nct1covs, file = "nct1covs.RData")



summary(nct1covs)








<<<<<<< HEAD



# network tree, PHQ only --------------------------------------------------

library(networktree)



dat_0 <- dat %>%
  select(PHQ1, PHQ2, PHQ3, PHQ4, PHQ5, PHQ6,
         PHQ7, PHQ8, PHQ9,
         SEX)



f <- paste(paste0(colnames(select(dat_0, -c(SEX))),
                  collapse = " + "), "~ SEX")

f <- as.formula(f)


tr_0 <- networktree(f, data = dat_0, model = "correlation",
                    transform = "glasso",
                    maxdepth = 3)


#save(tr_0, file = "tree_0.RData")


print(tr_0, digits = 2)


comparetree(tr_0, id1=2, id2=3, plot=TRUE,
            highlights = 10,
            layout="circle",
            plot.type="compare")
=======
# network trees -----------------------------------------------------------
>>>>>>> a7875ae5088a540b7b8add58d9196a3bb0bd8a54







<<<<<<< HEAD
library(networktree)

# networktree with phq + covariates ---------------------------------------
=======

f <- paste(paste0(colnames(select(dat, -c(BMI))),
                  collapse = " + "), "~ BMI")

f  <- as.formula(f)
f

tr1 <- networktree(f, data = dat, model = "correlation", maxdepth = 3,
                   transform = "glasso")



print(tr1, digits = 2)



## Add a comparison plot
comparetree(tr2, id1=2, id2=3, plot=TRUE,
            highlights=10,
            layout="circle"
)
>>>>>>> a7875ae5088a540b7b8add58d9196a3bb0bd8a54





<<<<<<< HEAD


f <- paste(paste0(colnames(select(dat, -c(SEX))),
                  collapse = " + "), "~ SEX")

f <- as.formula(f)


tr_1 <- networktree(f, data = dat, model = "correlation",
                    transform = "glasso",
                    maxdepth = 2)


#save(tr_0, file = "tree_0.RData")


print(tr_1, digits = 2)


comparetree(tr_0, id1=2, id2=3, plot=TRUE,
            highlights = 10,
            layout="circle",
            plot.type="compare")
=======
>>>>>>> a7875ae5088a540b7b8add58d9196a3bb0bd8a54











