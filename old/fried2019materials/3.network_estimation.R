##################################################################################
#                                                                                #
#           Using network analysis to examine links between individual           #
#            depressive symptoms, inflammatory markers, and covariates           #   
#                                                                                #
#                            Fried et al., 2019                                  #
#                          Psychological Medicine                                #
#                           https://osf.io/6ehrm/                                #
#                                                                                #
#              3. Syntax file for main estimation of network models              #
#                                                                                #
#                         Last updated: 08-09-2019                               #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- Loading packages & Data ---------------------------------
# -------------------------------------------------------------------------

library(mgm) 
library(qgraph)

# load("networks.RData") for all R files to reproduce analyses below; does not contain data ...
# ... which we are not allowed to make public for this project; see manuscript on how to get the NESDA data.

# In general, we'll estimate all networks with gamma=0 for the main paper, and with gamma=.25 for sensitivity analyses.
# ... We'll then compare both models to report differences. 



# -------------------------------------------------------------------------
# --------------- Global Settings -----------------------------------------
# -------------------------------------------------------------------------

# set working directory to catch processed data (output of data_preprocessing.R)
setwd("~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis")
dataPrDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/data_processed/"



# -------------------------------------------------------------------------
# --------------- Estimate Network 1a -------------------------------------
# -------------------------------------------------------------------------

# Get data
data_n_1a <- readRDS(file = paste0(dataPrDir, "data_n_1a.RDS"))

# ---------- Gamma = 0 model ----------

# Fit model
bs_network1a_gam0_init <- mgm(data = data_n_1a$data, 
                              type = data_n_1a$type, 
                              level = data_n_1a$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = 0)

# Compute Predictability
Pred1a <- predict(bs_network1a_gam0_init, data_n_1a$data)
Pred1a$errors
pie1a<- as.numeric(as.character(Pred1a$errors[1:4, 3]))#predictability estimates
mean(pie1a)

# Plot Network
Names1a<-c("CRP", "IL-6", "TNF-alpha", "IDS Total")
Labels1a<-c("IM1", "IM2", "IM3", "D1")
GroupLabels1a <- list("Inflammatory Markers" = c(1:3), "Depression Total" = c(4)) 

N1amgm0 <- qgraph(bs_network1a_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                  maximum = 0.4, 
                  groups=GroupLabels1a, palette = "colorblind",
                  nodeNames=Names1a, labels = Labels1a, vsize=4.5,
                  label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network1a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=4, ncol=4),
                  pie = pie1a, pieBorder = 0.25) 

# ---------- Gamma = .25 model ----------

# Fit model
bs_network1a_gam25_init <- mgm(data = data_n_1a$data, 
                              type = data_n_1a$type, 
                              level = data_n_1a$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = 0.25)

# Compute Predictability
Pred1a <- predict(bs_network1a_gam25_init, data_n_1a$data)
Pred1a$errors
pie1a<- as.numeric(as.character(Pred1a$errors[1:4, 3]))#predictability estimates
mean(pie1a)

# Plot Network
Names1a <- c("CRP", "IL-6", "TNF-alpha", "IDS Total")
Labels1a <- c("IM1", "IM2", "IM3", "D1")
GroupLabels1a <- list("Inflammatory Markers" = c(1:3), "Depression Total" = c(4)) 

N1amgm25 <- qgraph(bs_network1a_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels1a, palette = "colorblind",
                 nodeNames=Names1a, labels = Labels1a, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network1a_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=4, ncol=4),
                 pie = pie1a, pieBorder = 0.25) 

# ---------- 1a comparison ----------

# For model 1a there is no difference for biopsycho edges when gamma = 0 or when gamma = 0.25
# In fact, both models are *exactly* identical
cor(as.vector(bs_network1a_gam0_init$pairwise$wadj), as.vector(bs_network1a_gam25_init$pairwise$wadj))

# Plot model 1a as pdf for main figure and supplementary

N1amgm25 <- qgraph(bs_network1a_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                   maximum = 0.4, 
                   groups=GroupLabels1a, theme = "colorblind",
                   nodeNames=Names1a, labels = Labels1a, vsize=9,
                   label.cex=1.3, legend.cex=.38, GLratio = 1.8, 
                   edge.color = matrix(as.vector(lapply(bs_network1a_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=4, ncol=4),
                   pie = pie1a, pieBorder = 0.25, 
                   filetype = "pdf", filename = "Figure 1 Panel A gam25", legend = FALSE)

N1amgm0 <- qgraph(bs_network1a_gam0_init$pairwise$wadj, layout = N1amgm25$layout, cut=0,
                  maximum = 0.4, 
                  groups=GroupLabels1a, palette = "colorblind",
                  nodeNames=Names1a, labels = Labels1a, vsize=9,
                  label.cex=1.3, legend.cex=.38, GLratio = 1.8, 
                  edge.color = matrix(as.vector(lapply(bs_network1a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=4, ncol=4),
                  pie = pie1a, pieBorder = 0.25, 
                  filetype = "pdf", filename = "Figure 1 Panel A gam0", legend = FALSE)



# -------------------------------------------------------------------------
# --------------- Estimate Network 1b -------------------------------------
# -------------------------------------------------------------------------

data_n_1b <- readRDS(file = paste0(dataPrDir, "data_n_1b.RDS"))

# ---------- Gamma = 0 model ----------

# Fit initial model
bs_network1b_gam0_init <- mgm(data = data_n_1b$data, 
                              type = data_n_1b$type, 
                              level = data_n_1b$level, 
                              lambdaSel = "EBIC", 
                              binarySign = TRUE, 
                              lambdaGam = 0)

# Compute Predictability
Pred1b <- predict(bs_network1b_gam0_init, data_n_1b$data)
Pred1b$errors
pie1bb <- as.numeric(as.character(Pred1b$errors[1:4, 3]))
pie1bbb <- as.numeric(as.character(Pred1b$errors[5, 5]))
pie1bbbb <- as.numeric(as.character(Pred1b$errors[6:11, 3]))
pie1b <- c(pie1bb, pie1bbb, pie1bbbb)#predictability estimates as one piece
mean(pie1b)

# Plot Network
Names1b<-c("CRP", "IL-6", "TNF-alpha", "IDS Total","Sex", "Age", "Alcohol", "Smoking","Exercise", "BMI", "Chronic Diseases")
Labels1b<-c("IM1", "IM2", "IM3", "D1", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
GroupLabels1b <- list("Inflammatory Markers" = c(1:3), "Depression Total" = c(4), "Covariates" = c(5:11))   

N1bmgm0 <- qgraph(bs_network1b_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels1b, palette = "colorblind",
                 nodeNames=Names1b, labels = Labels1b, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network1b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=11, ncol=11),
                 pie = pie1b, pieBorder = 0.25) 

# ---------- Gamma = .25 model ----------

# Fit initial model
bs_network1b_gam25_init <- mgm(data = data_n_1b$data, 
                              type = data_n_1b$type, 
                              level = data_n_1b$level, 
                              lambdaSel = "EBIC", 
                              binarySign = TRUE, 
                              lambdaGam = 0.25)

# Compute Predictability
Pred1b2 <- predict(bs_network1b_gam25_init, data_n_1b$data)
Pred1b2$errors
pie1bb2 <- as.numeric(as.character(Pred1b$errors[1:4, 3]))
pie1bbb2 <- as.numeric(as.character(Pred1b$errors[5, 5]))
pie1bbbb2 <- as.numeric(as.character(Pred1b$errors[6:11, 3]))
pie1b2 <- c(pie1bb2, pie1bbb2, pie1bbbb2)#predictability estimates as one piece
mean(pie1b2)

# Plot Network
Names1b<-c("CRP", "IL-6", "TNF-alpha", "IDS Total","Sex", "Age", "Alcohol", "Smoking","Exercise", "BMI", "Chronic Diseases")
Labels1b<-c("IM1", "IM2", "IM3", "D1", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
GroupLabels1b <- list("Inflammatory Markers" = c(1:3), "Depression Total" = c(4), "Covariates" = c(5:11))   

N1bmgm25 <- qgraph(bs_network1b_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels1b, palette = "colorblind",
                 nodeNames=Names1b, labels = Labels1b, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network1b_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=11, ncol=11),
                 title="Network 1b gamma 0.25", pie = pie1b2, pieBorder = 0.25) 

# ---------- 1b comparison ----------

# For predictability of the two above models, only one node differs, minimally: 
# gamma 0:   0.214 0.150 0.048 0.079 0.146 0.185 0.155 0.092 0.011 0.264 0.076
# gamma .25: 0.214 0.150 0.048 0.079 0.124 0.185 0.155 0.092 0.011 0.257 0.076

cor(as.vector(bs_network1b_gam25_init$pairwise$wadj), as.vector(bs_network1b_gam0_init$pairwise$wadj)) # .994

# For model 1b there is no difference for biopsycho edges when gamma = 0 or when gamma = 0.25

# Plot model 1b as pdf for paper + supplementary

N1bmgm0 <- qgraph(bs_network1b_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                   maximum = 0.4, theme='colorblind',
                   groups=GroupLabels1b, palette = "colorblind",
                   nodeNames=Names1b, labels = Labels1b, vsize=6,
                   label.cex=1.3, legend.cex=.65, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network1b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=11, ncol=11),
                   pie = pie1b, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 1 Panel B gam0") 

N1bmgm25 <- qgraph(bs_network1b_gam25_init$pairwise$wadj, layout = N1bmgm0$layout, cut=0,
                  maximum = 0.4, 
                  groups=GroupLabels1b, palette = "colorblind", 
                  nodeNames=Names1b, labels = Labels1b, vsize=6,
                  label.cex=1.3, legend.cex=.65, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network1b_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=11, ncol=11),
                  pie = pie1b2, pieBorder = 0.25,
                  filetype = "pdf", filename = "Figure 1 Panel B gam25") 

# ---------- Final figure 1 (further finalized in inkscape for paper) ----------

pdf("Fig1_Final.pdf", width=15, height=7)
layout(matrix(c(1,2),1,2,byrow=TRUE),widths = c(2.3,3.7))
final1a <- qgraph(bs_network1a_gam0_init$pairwise$wadj, layout = N1amgm25$layout, cut=0,
                 maximum = 0.4,
                 groups=GroupLabels1a, palette = "colorblind",
                 nodeNames=Names1a, labels = Labels1a, vsize=10,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8, 
                 edge.color = matrix(as.vector(lapply(bs_network1a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=4, ncol=4),
                 pie = pie1a, pieBorder = 0.25, 
                 legend = FALSE, title="A")
final1b <- qgraph(bs_network1b_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                  maximum = 0.4, 
                  groups=GroupLabels1b, palette = "colorblind",
                  nodeNames=Names1b, labels = Labels1b, vsize=8,
                  label.cex=1.3, legend.cex=.65, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network1b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=11, ncol=11),
                  pie = pie1b, pieBorder = 0.25, title="B")
dev.off()



# -------------------------------------------------------------------------
# --------------- Estimate Network 2a -------------------------------------
# -------------------------------------------------------------------------

data_n_2a <- readRDS(file = paste0(dataPrDir, "data_n_2a.RDS"))

# ---------- Gamma = 0 model ----------

# Fit initial model
bs_network2a_gam0_init <- mgm(data = data_n_2a$data, 
                              type = data_n_2a$type, 
                              level = data_n_2a$level, 
                              lambdaSel = "EBIC", 
                              binarySign = TRUE, 
                              lambdaGam = 0)

# Compute Predictability
Pred2a <- predict(bs_network2a_gam0_init, data_n_2a$data)
Pred2a$errors
pie2aa<- as.numeric(as.character(Pred2a$errors[1:10, 3]))
pie2aaa<- as.numeric(as.character(Pred2a$errors[11, 5]))
pie2aaaa<- as.numeric(as.character(Pred2a$errors[12, 3]))
pie2a<-c(pie2aa, pie2aaa, pie2aaaa)#predictability estimates as one piece
mean(pie2a)
#error.nCC for sex is negative; recode error.cc for sex
pie2aab<- as.numeric(as.character(Pred2a$errors[11, 4]))
pie2a<-c(pie2aa, pie2aab, pie2aaaa) # combine


# Plot Network
RepNames2a<-c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age")
RepLabels2a<-c("IM1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "C1", "C2")
RepGroups2a<-list("Inflammation Marker" = c(1), "DSM Depression Criteria" = c(2:10), "Covariates" = c(11:12))   

N2amgm0 <- qgraph(bs_network2a_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=RepGroups2a, palette = "colorblind",
                 nodeNames=RepNames2a, labels = RepLabels2a, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network2a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=12, ncol=12),
                 pie = pie2a, pieBorder = 0.25) 

# ---------- Gamma = .25 model ----------

# Fit initial model
bs_network2a_gam25_init <- mgm(data = data_n_2a$data, 
                              type = data_n_2a$type, 
                              level = data_n_2a$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = .25, 
                              binarySign = TRUE)


# Compute Predictability
Pred2a2 <- predict(bs_network2a_gam25_init, data_n_2a$data)
Pred2a2$errors
pie2aa2<- as.numeric(as.character(Pred2a2$errors[1:10, 3]))
pie2aaa2<- as.numeric(as.character(Pred2a2$errors[11, 5]))
pie2aaaa2<- as.numeric(as.character(Pred2a2$errors[12, 3]))
pie2a2<-c(pie2aa2, pie2aaa2, pie2aaaa2) #predictability estimates as one piece
mean(pie2a2)
#error.nCC for sex is negative; recode error.cc for sex
pie2aab2<- as.numeric(as.character(Pred2a2$errors[11, 4]))
pie2a2<-c(pie2aa2, pie2aab2, pie2aaaa2) #predictability estimates as one piece

# Plot Network
RepNames2a<-c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age")
RepLabels2a<-c("IM1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "C1", "C2")
RepGroups2a<-list("Inflammation Marker" = c(1), "DSM Depression Criteria" = c(2:10), "Covariates" = c(11:12))   

N2amgm25 <- qgraph(bs_network2a_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=RepGroups2a, palette = "colorblind",
                 nodeNames=RepNames2a, labels = RepLabels2a, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network2a_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=12, ncol=12),
                 pie = pie2a, pieBorder = 0.25) 

# ---------- 2a comparison ----------

# identical predictability
# [1] 0.034 0.544 0.586 0.190 0.511 0.195 0.363 0.504 0.357 0.264 0.651 0.038
# [1] 0.034 0.544 0.586 0.190 0.511 0.195 0.363 0.504 0.357 0.264 0.651 0.038

# edges
cor(as.vector(bs_network2a_gam0_init$pairwise$wadj), as.vector(bs_network2a_gam25_init$pairwise$wadj)) #1

# perfectly identical

# Plot model 2a as pdf

N2amgm25 <- qgraph(bs_network2a_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                   maximum = 0.4, 
                   groups=RepGroups2a, palette = "colorblind",
                   nodeNames=RepNames2a, labels = RepLabels2a, vsize=8,
                   label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network2a_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=12, ncol=12),
                   pie = pie2a, pieBorder = 0.25, 
                   filetype = "pdf", filename = "Figure 2 Panel A gam25", legend = FALSE) 

N2amgm0 <- qgraph(bs_network2a_gam0_init$pairwise$wadj, layout = N2amgm25$layout, cut=0,
                   maximum = 0.4, 
                   groups=RepGroups2a, palette = "colorblind",
                   nodeNames=RepNames2a, labels = RepLabels2a, vsize=8,
                   label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network2a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=12, ncol=12),
                   pie = pie2a2, pieBorder = 0.25, 
                   filetype = "pdf", filename = "Figure 2 Panel A gam0", legend = FALSE)



# -------------------------------------------------------------------------
# --------------- Estimate Network 2b -------------------------------------
# -------------------------------------------------------------------------

data_n_2b <- readRDS(file = paste0(dataPrDir, "data_n_2b.RDS"))

# ---------- Gamma = 0 model ----------

# Fit initial model
bs_network2b_gam0_init <- mgm(data = data_n_2b$data, 
                               type = data_n_2b$type, 
                               level = data_n_2b$level, 
                               lambdaSel = "EBIC", 
                               lambdaGam = 0, 
                               binarySign = TRUE)

# Compute Predictability
Pred2b <- predict(bs_network2b_gam0_init, data_n_2b$data)
Pred2b$errors
pie2bb<- as.numeric(as.character(Pred2b$errors[1:10, 3]))
pie2bbb<- as.numeric(as.character(Pred2b$errors[11, 5]))
pie2bbbb<- as.numeric(as.character(Pred2b$errors[12:17, 3]))
pie2b<-c(pie2bb, pie2bbb, pie2bbbb) # predictability estimates as one piece
mean(pie2b)

# Plot Network
RepNames2b<-c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
RepLabels2b<-c("IM1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
RepGroups2b<-list("Inflammation Marker" = c(1), "DSM Depression Criteria" = c(2:10), "Covariates" = c(11:17))

N2bmgm0 <- qgraph(bs_network2b_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=RepGroups2b, palette = "colorblind",
                 nodeNames=RepNames2b, labels = RepLabels2b, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network2b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                 title="Network 2b gamma 0", pie = pie2b, pieBorder = 0.25) 

# ---------- Gamma = .25 model ----------

# Fit initial model
bs_network2b_gam25_init <- mgm(data = data_n_2b$data, 
                               type = data_n_2b$type, 
                               level = data_n_2b$level, 
                               lambdaSel = "EBIC", 
                               lambdaGam = .25,
                               binarySign = TRUE)

# Compute Predictability
Pred2b2 <- predict(bs_network2b_gam25_init, data_n_2b$data)
Pred2b2$errors
pie2bb2<- as.numeric(as.character(Pred2b2$errors[1:10, 3]))
pie2bbb2<- as.numeric(as.character(Pred2b2$errors[11, 5]))
pie2bbbb2<- as.numeric(as.character(Pred2b2$errors[12:17, 3]))
pie2b2<-c(pie2bb2, pie2bbb2, pie2bbbb2) # predictability estimates as one piece
mean(pie2b2)

# Plot Network
RepNames2b<-c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
RepLabels2b<-c("IM1", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
RepGroups2b<-list("Inflammation Marker" = c(1), "DSM Depression Criteria" = c(2:10), "Covariates" = c(11:17))

N2bmgm25 <- qgraph(bs_network2b_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4,
                 groups=RepGroups2b, palette = "colorblind",
                 nodeNames=RepNames2b, labels = RepLabels2b, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network2b_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                 title="Network 2b gamma 0.25", pie = pie2b, pieBorder = 0.25) 

# ---------- 2b comparison ----------

# very minor differences in only 2 nodes
# [1] 0.173 0.543 0.585 0.190 0.518 0.216 0.363 0.504 0.353 0.261 0.151 0.201 0.158 0.017 0.086 0.263 0.088
# [1] 0.173 0.543 0.585 0.190 0.518 0.216 0.363 0.504 0.353 0.261 0.151 0.198 0.158 0.014 0.086 0.263 0.088

# For model 2b there is no difference for biopsycho edges when gamma = 0 or when gamma = 0.25

cor(as.vector(bs_network2b_gam25_init$pairwise$wadj), as.vector(bs_network2b_gam0_init$pairwise$wadj)) #.9999

# Plot model 2b as pdf

N2bmgm25 <- qgraph(bs_network2b_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                   maximum = 0.4, 
                   groups=RepGroups2b, palette = "colorblind",
                   nodeNames=RepNames2b, labels = RepLabels2b, vsize=6,
                   label.cex=1.3, legend.cex=.45, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network2b_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                   pie = pie2b2, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 2 Panel B gam25", width=6, height=5) 

N2bmgm0 <- qgraph(bs_network2b_gam0_init$pairwise$wadj, layout = N2bmgm25$layout, cut=0,
                   maximum = 0.4, 
                   groups=RepGroups2b, palette = "colorblind",
                   nodeNames=RepNames2b, labels = RepLabels2b, vsize=6,
                   label.cex=1.3, legend.cex=.45, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network2b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                   pie = pie2b, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 2 Panel B gam0", width=6, height=5) 

# ---------- Final figure 2 (further finalized in inkscape for paper) ----------

pdf("Fig2_Final.pdf", width=15, height=7)
layout(matrix(c(
  1,2),1,2,byrow=TRUE),widths = c(2.3,3.7))
final2a <- qgraph(bs_network2a_gam0_init$pairwise$wadj, layout = N2amgm25$layout, cut=0,
                  maximum = 0.4, 
                  groups=RepGroups2a, palette = "colorblind",
                  nodeNames=RepNames2a, labels = RepLabels2a, vsize=8.5,
                  label.cex=1.3, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network2a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=12, ncol=12),
                  pie = pie2a2, pieBorder = 0.25,
                  legend = FALSE, title="A")

final2b <- qgraph(bs_network2b_gam0_init$pairwise$wadj, layout = N2bmgm25$layout, cut=0,
                  maximum = 0.4, 
                  groups=RepGroups2b, palette = "colorblind",
                  nodeNames=RepNames2b, labels = RepLabels2b, vsize=6.6,
                  label.cex=1.3, legend.cex=.56, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network2b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                  pie = pie2b, pieBorder = 0.25, title="B")
dev.off()



# -------------------------------------------------------------------------
# --------------- Estimate Network 3a -------------------------------------
# -------------------------------------------------------------------------

data_n_3a <- readRDS(file = paste0(dataPrDir, "data_n_3a.RDS"))

# ---------- Gamma = 0 model ----------

# Fit model
bs_network3a_gam0_init <- mgm(data = data_n_3a$data, 
                              type = data_n_3a$type, 
                              level = data_n_3a$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = 0, 
                              binarySign = TRUE)

# Compute Predictability
Pred3a <- predict(bs_network3a_gam0_init, data_n_3a$data)
Pred3a$errors
pie3a <- as.numeric(as.character(Pred3a$errors[1:31, 3]))#predictability estimates
mean(pie3a)

# Plot Network
Names3a<-c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response to good or desired events", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy")
Labels3a<-c("IM1", "IM2", "IM3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28")
GroupLabels3a <- list("Inflammatory Markers" = c(1:3), "Depressive Symptoms" = c(4:31))   

N3amgm0 <- qgraph(bs_network3a_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels3a, palette = "colorblind",
                 nodeNames=Names3a, labels = Labels3a, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network3a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),
                 title="Network 3a gamma 0", pie = pie3a, pieBorder = 0.25) 

# ---------- Gamma = .25 model ----------

# Fit model
bs_network3a_gam25_init <- mgm(data = data_n_3a$data, 
                              type = data_n_3a$type, 
                              level = data_n_3a$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = 0.25,
                              binarySign = TRUE)

# Compute Predictability
Pred3a2 <- predict(bs_network3a_gam25_init, data_n_3a$data)
Pred3a2$errors
pie3a2 <- as.numeric(as.character(Pred3a2$errors[1:31, 3]))#predictability estimates
mean(pie3a2)

# Plot Network
Names3a<-c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy")
Labels3a<-c("IM1", "IM2", "IM3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28")
GroupLabels3a <- list("Inflammatory Markers" = c(1:3), "Depressive Symptoms" = c(4:31))   

N3amgm25 <- qgraph(bs_network3a_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels3a, palette = "colorblind",
                 nodeNames=Names3a, labels = Labels3a, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network3a_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),
                 title="Network 3a gamma 0.25", pie = pie3a2, pieBorder = 0.25) 

# ---------- 3a comparison ----------

cor(pie3a, pie3a2) # 0.9995915
cor(as.vector(bs_network3a_gam0_init$pairwise$wadj), as.vector(bs_network3a_gam25_init$pairwise$wadj)) #.9975

sum(abs(bs_network3a_gam25_init$pairwise$wadj)) #19.22338
sum(abs(bs_network3a_gam0_init$pairwise$wadj))  #19.70415

# Model 3a with gamma = 0 recovers two additional edges in comparison with model 3a with gamma = 0.25;
# Both edges are positive, CRP and IL-6 with aches & pain

# Plot model 3a as pdf

N3amgm25 <- qgraph(bs_network3a_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                   maximum = 0.4, 
                   groups=GroupLabels3a, palette = "colorblind",
                   nodeNames=Names3a, labels = Labels3a, vsize=6,
                   label.cex=1.3, legend = FALSE, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network3a_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),
                   pie = pie3a2, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 3 Panel A gam25") 

N3amgm0 <- qgraph(bs_network3a_gam0_init$pairwise$wadj, layout = N3amgm25$layout, cut=0,
                   maximum = 0.4, 
                   groups=GroupLabels3a, palette = "colorblind",
                   nodeNames=Names3a, labels = Labels3a, vsize=6,
                   label.cex=1.3, legend = FALSE, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network3a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),
                   pie = pie3a, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 3 Panel A gam0") 



# -------------------------------------------------------------------------
# --------------- Estimate Network 3b -------------------------------------
# -------------------------------------------------------------------------

data_n_3b <- readRDS(file = paste0(dataPrDir, "data_n_3b.RDS"))

# ---------- Gamma = 0 model ----------

# Fit model
bs_network3b_gam0_init <- mgm(data = data_n_3b$data, 
                              type = data_n_3b$type, 
                              level = data_n_3b$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = 0, 
                              binarySign = TRUE)

# Compute Predictability
Pred3b <- predict(bs_network3b_gam0_init, data_n_3b$data)
Pred3b$errors

pie3bb<- as.numeric(as.character(Pred3b$errors[1:31, 3]))
pie3bbb<- as.numeric(as.character(Pred3b$errors[32, 5]))
pie3bbbb<- as.numeric(as.character(Pred3b$errors[33:38, 3]))

pie3b <- c(pie3bb, pie3bbb, pie3bbbb)#predictability estimates as one piece
mean(pie3b)

# Plot Network
Names3b<-c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
Labels3b<-c("IM1", "IM2", "IM3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
GroupLabels3b <- list("Inflammatory Markers" = c(1:3), "Depressive Symptoms" = c(4:31), "Covariates" = c(32:38))

N3bmgm0 <- qgraph(bs_network3b_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels3b, palette = "colorblind",
                 nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network3b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                 pie = pie3b, pieBorder = 0.25) 

# ---------- Gamma = .25 model ----------

# Fit model
bs_network3b_gam25_init <- mgm(data = data_n_3b$data, 
                              type = data_n_3b$type, 
                              level = data_n_3b$level, 
                              lambdaSel = "EBIC", 
                              lambdaGam = .25, 
                              binarySign = TRUE)

# Compute Predictability
Pred3b2 <- predict(bs_network3b_gam25_init, data_n_3b$data)
Pred3b2$errors

pie3bb2<- as.numeric(as.character(Pred3b2$errors[1:31, 3]))
pie3bbb2<- as.numeric(as.character(Pred3b2$errors[32, 5]))
pie3bbbb2<- as.numeric(as.character(Pred3b2$errors[33:38, 3]))

pie3b2 <- c(pie3bb2, pie3bbb2, pie3bbbb2) #predictability estimates as one piece
mean(pie3b2)

# Plot Network
Names3b<-c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
Labels3b<-c("IM1", "IM2", "IM3", "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "C1", "C2", "C3", "C4", "C5", "C6", "C7")
GroupLabels3b <- list("Inflammatory Markers" = c(1:3), "Depressive Symptoms" = c(4:31), "Covariates" = c(32:38))

N3bmgm25 <- qgraph(bs_network3b_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                 maximum = 0.4, 
                 groups=GroupLabels3b, palette = "colorblind",
                 nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                 label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                 edge.color = matrix(as.vector(lapply(bs_network3b_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                 pie = pie3b2, pieBorder = 0.25) 

# ---------- 3b comparison ----------

cor(pie3b, pie3b2) # .999
cor(as.vector(bs_network3b_gam0_init$pairwise$wadj), as.vector(bs_network3b_gam25_init$pairwise$wadj)) # .996

# Plot model 3b as pdf

N3bamgm25 <- qgraph(bs_network3b_gam25_init$pairwise$wadj, layout = "spring", cut=0,
                   maximum = 0.4, legend.cex=.39,
                   groups=GroupLabels3b, palette = "colorblind",
                   nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                   label.cex=1.3, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network3b_gam25_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                   pie = pie3b2, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 3 Panel B gam25") 

N3bamgm0 <- qgraph(bs_network3b_gam0_init$pairwise$wadj, layout = N3bmgm25$layout, cut=0,
                   maximum = 0.4, legend.cex=.39, 
                   groups=GroupLabels3b, palette = "colorblind",
                   nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                   label.cex=1.3, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply( bs_network3b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                   pie = pie3b, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 3 Panel B gam0") 

sum(abs(bs_network3b_gam0_init$pairwise$wadj[1, c(4:31)])) # no edges between symptoms and markers
sum(abs(bs_network3b_gam0_init$pairwise$wadj[2, c(4:31)])) # no edges between symptoms and markers
sum(abs(bs_network3b_gam0_init$pairwise$wadj[3, c(4:31)])) # no edges between symptoms and markers

# ---------- Final figure 3 (further finalized in inkscape for paper) ----------

pdf("Fig3_Final.pdf", width=15, height=7)
layout(matrix(c(
  1,2),1,2,byrow=TRUE),widths = c(2.3,3.7))
final3a <- qgraph(bs_network3a_gam0_init$pairwise$wadj, layout = N3amgm25$layout, cut=0,
                  maximum = 0.4, 
                  groups=GroupLabels3a, palette = "colorblind",
                  nodeNames=Names3a, labels = Labels3a, vsize=6,
                  label.cex=1.3, legend = FALSE, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network3a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),
                  pie = pie3a, pieBorder = 0.25,
                  legend = FALSE, title="A")

final3b <- qgraph(bs_network3b_gam0_init$pairwise$wadj, layout = N3bmgm25$layout, cut=0,
                  maximum = 0.4, legend.cex=.39, 
                  groups=GroupLabels3b, palette = "colorblind",
                  nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                  label.cex=1.3, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network3b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                  pie = pie3b, pieBorder = 0.25, title="B")
dev.off()



# -------------------------------------------------------------------------
# --------------- Estimate Network 4 (3b without regularization) ----------
# -------------------------------------------------------------------------

bs_network3b_init <- mgm(data = data_n_3b$data, 
                         type = data_n_3b$type, 
                         level = data_n_3b$level, 
                         lambdaSel = "EBIC", 
                         lambdaGam = 0, 
                         lambdaSeq = 0, 
                         binarySign = TRUE)

# Compute Predictability
Pred3bx <- predict(bs_network3b_init, data_n_3b$data)

Pred3bx$errors

pie3bx<- as.numeric(as.character(Pred3bx$errors[1:31, 3]))
pie3bxx<- as.numeric(as.character(Pred3bx$errors[32, 5]))
pie3bxxx<- as.numeric(as.character(Pred3bx$errors[33:38, 3]))

pie3bx <- c(pie3bx, pie3bxx, pie3bxxx) #predictability estimates as one piece
mean(pie3bx)

# ---------- compare to 3b with gam0 ----------

cor(pie3b, pie3bx) #.9976
cor(as.vector(bs_network3b_gam0_init$pairwise$wadj), as.vector(bs_network3b_init$pairwise$wadj)) #.931

N3bamgmx <- qgraph(bs_network3b_init$pairwise$wadj, layout = N3bmgm25$layout, cut=0,
                   maximum = 0.4, legend.cex=.39, 
                   groups=GroupLabels3b, palette = "colorblind",
                   nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                   label.cex=1.3, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network3b_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                   pie = pie3bx, pieBorder = 0.25,
                   filetype = "pdf", filename = "Figure 4 no regularization") 

sum(bs_network3b_init$pairwise$wadj[upper.tri(bs_network3b_init$pairwise$wadj)] != 0)  #420
sum(bs_network3b_init$pairwise$wadj[upper.tri(bs_network3b_init$pairwise$wadj)] == 0)  #283
283/703 # 40.3% edges exact zero

# now plot only symptom-marker edges
noreg <- bs_network3b_init$pairwise$wadj
noreg[c(1:3), c(1:3)]     <-0
noreg[c(4:31), c(4:31)]   <-0
noreg[c(31:38), c(31:38)] <-0
noreg[c(4:31), c(31:38)]  <-0
noreg[c(31:38), c(4:31)]  <-0
noreg[c(1:3), c(31:38)]   <-0
noreg[c(31:38), c(1:3)]   <-0

N3bamgmxx <- qgraph(noreg, layout = N3bmgm25$layout, cut=0,
                    maximum = 0.4, legend.cex=.39, 
                    groups=GroupLabels3b, palette = "colorblind",
                    nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                    label.cex=1.3, GLratio = 1.8,
                    edge.color = matrix(as.vector(lapply(bs_network3b_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                    pie = pie3bx, pieBorder = 0.25,
                    filetype = "pdf", filename = "Figure 4 zoomed in") 

bs_network3b_gam0_init$pairwise$wadj[c(1:3), c(4:31)] # 0 edges between symptoms and markers
bs_network3b_init$pairwise$wadj[c(1:3), c(4:31)]      # plenty of edges remain; symptoms for which all markers are connected: 1, 4, 6, 23
bs_network3b_init$pairwise$edgecolor[c(1:3), c(4:31)] # of these 12 edges for symptoms 1, 4, 6 and 23, one is negative (6 #2)

# ---------- Final figure 4 (further finalized in inkscape for paper) ----------

pdf("Fig4_Final.pdf", width=15, height=7)
layout(matrix(c(
  1,2),1,2,byrow=TRUE),widths = c(2.3,3.7))
N3bamgmx <- qgraph(bs_network3b_init$pairwise$wadj, layout = N3bmgm25$layout, cut=0,
                   maximum = 0.4, legend.cex=.39, 
                   groups=GroupLabels3b, palette = "colorblind",
                   nodeNames=Names3b, labels = Labels3b, vsize=6,
                   label.cex=1.3, GLratio = 1.8,
                   edge.color = matrix(as.vector(lapply(bs_network3b_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                   pie = pie3bx, pieBorder = 0.25,
                   legend = FALSE, title="A")

N3bamgmxx <- qgraph(noreg, layout = N3bmgm25$layout, cut=0,
                    maximum = 0.15, legend.cex=.36, 
                    groups=GroupLabels3b, palette = "colorblind",
                    nodeNames=Names3b, labels = Labels3b, vsize=4.5,
                    label.cex=1.3, GLratio = 1.8, 
                    edge.color = matrix(as.vector(lapply(bs_network3b_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=38, ncol=38),
                    pie = pie3bx, pieBorder = 0.25, title="B")
dev.off()



# -------------------------------------------------------------------------
# --------- Save all adjacency matrices and edge color output -------------
# -------------------------------------------------------------------------

# I apologize for this messy code but save() does not work with the original output, ...
# ... and I cannot save the full objects because they contain the raw data.

# Below you find the 'legend', i.e. what object is what. These are in the order of the syntax above. 
# We provide both weighted adjacency matrices and edge.color because these are the two objects required to plot the graphs in mgm().

# save(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,a1,a2,a3,a4,a5,a6,a7, file="networks.RData")
# #load(file = "networks.RData")
# 
# a <- bs_network1a_gam0_init$pairwise$wadj
# b <- bs_network1a_gam0_init$pairwise$edgecolor
# c <- bs_network1a_gam25_init$pairwise$wadj
# d <- bs_network1a_gam25_init$pairwise$edgecolor
# colnames(a) <- colnames(c) <- c("CRP", "IL-6", "TNF-alpha", "IDS Total")
# 
# e <- bs_network1b_gam0_init$pairwise$wadj
# f <- bs_network1b_gam0_init$pairwise$edgecolor
# g <- bs_network1b_gam25_init$pairwise$wadj
# h <- bs_network1b_gam25_init$pairwise$edgecolor
# colnames(e) <- colnames(g) <- c("CRP", "IL-6", "TNF-alpha", "IDS Total","Sex", "Age", "Alcohol", "Smoking","Exercise", "BMI", "Chronic Diseases")
# 
# i <- bs_network2a_gam0_init$pairwise$wadj
# j <- bs_network2a_gam0_init$pairwise$edgecolor
# k <- bs_network2a_gam25_init$pairwise$wadj
# l <- bs_network2a_gam25_init$pairwise$edgecolor
# colnames(i) <- colnames(k) <- c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age")
# 
# m <- bs_network2b_gam0_init$pairwise$wadj
# n <- bs_network2b_gam0_init$pairwise$edgecolor
# o <- bs_network2b_gam25_init$pairwise$wadj
# p <- bs_network2b_gam25_init$pairwise$edgecolor
# colnames(m) <- colnames(o) <- c("CRP", "Sad Mood", "Interest/Pleasure", "Sleep Problems", "Energy Level", "Appetite/Weight", "Restlessness/Retardation", "Concentration/Decisions", "Feeling Worthless/Guilty", "Thoughts of Death", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
# 
# q <- bs_network3a_gam0_init$pairwise$wadj
# r <- bs_network3a_gam0_init$pairwise$edgecolor
# s <- bs_network3a_gam25_init$pairwise$wadj
# t <- bs_network3a_gam25_init$pairwise$edgecolor
# colnames(q) <- colnames(s) <- c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy")
# 
# u <- bs_network3b_gam0_init$pairwise$wadj
# v <- bs_network3b_gam0_init$pairwise$edgecolor
# w <- bs_network3b_gam25_init$pairwise$wadj
# x <- bs_network3b_gam25_init$pairwise$edgecolor
# colnames(u) <- colnames(w) <- c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")
# 
# # Run "4.sensitivity.R" before you run this.
# a1 <- n_r2$pairwise$wadj[c(1:17), c(1:17)]
# a2 <- n_r2$pairwise$edgecolor[c(1:17), c(1:17)]
# a3 <- n_r3$pairwise$wadj[c(1:31), c(1:31)]
# a4 <- bs_network3a_gam0_init$pairwise$edgecolor[c(1:31), c(1:31)]
# a5 <- bs_network3b_init$pairwise$wadj
# a6 <- bs_network3b_init$pairwise$edgecolor
# a7 <- noreg
# colnames(a5) <- colnames(a7) <- c("CRP", "IL-6", "TNF-alpha", "fall asleep", "sleep during the night", "wake up too early", "sleep too much", "feel sad", "feel irritable", "feel anxious or tense", "mood response", "mood time of the day", "mood quality", "appetite", "weight", "trouble concentrating", "view of self", "view of future", "suicidal ideation", "interest loss", "energy level", "capacity for pleasure", "loss of libido", "feel slowed down", "feel restless", "aches and pain", "bodily symptoms", "panic or phobia", "constipation/diarrhea", "interpersonal sensitivity", "leaden paralysis/physical energy", "Sex", "Age", "Alcohol", "Exercise", "Smoking", "BMI", "Chronic Diseases")



#             |￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                  This is the end of  
#                   part 3: networks
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ


