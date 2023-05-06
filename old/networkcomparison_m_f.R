library(tidyverse)
library(qgraph)          #visualize networks
library(mgm)           #mixed graphical modeling
library(bootnet)         # runs all basic network analyses
library(NetworkComparisonTest)  #compare networks
library(networktools)         #bridge symptoms, goldbricker for node select
library(networktree)




load("mhm_deprcovs.RData")

mhm <- select(mhm, -good_nights_sleep_frequency)





depr_f <- list("data" = NULL,
               "type" = NULL,
               "level" = NULL,
               "names" = NULL,
               "labels" = NULL,
               "group labels" = NULL)


depr_f$data <- as.matrix(subset(mhm, sex == 0, select = -sex))
depr_f$type <- c(rep("g", 18), rep("c", 5), rep("g", 4))
depr_f$level <- c(rep(1, 18), rep(2, 5), rep(1, 4))
depr_f$names <- c(colnames(mhm[,c(1:18)]), "employment: 0=unemployed, 1=employed",
                  "relationship status: 0=single, 1=relationship", "seeking mental health treatment: 0=no, 1=yes",
                  "medical diagnosis: 0=no, 1=yes", "childhood trauma: 0=no, 1=yes",
                  "exercise frequency", "frequency of socializing",
                  "age", "educational attainment")
depr_f$labels <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10",
                      "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18",
                      "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9")
depr_f$grouplabels <- list("Depressive Symptoms" = c(1:18), "Covariates" = c(19:27))





network_f <- estimateNetwork(data = depr_f$data,
                             default = c("mgm"),
                             type = depr_f$type,
                             level = depr_f$level,
                             criterion = "EBIC",
                             tuning = 0.25,
                             binarySign = T )


save(network_f, file = "network_f.RData")



pred_f <- predict(network_f, depr_f$data)
pred_f$errors
pred_f1 <- as.numeric(as.character(pred_f$errors[1:18, 3]))
mean(pred_f1)  #mean predictability of depr items = 0.496
pred_f2 <- as.numeric(as.character(pred_f$errors[19:23, 5]))
pred_f3 <- as.numeric(as.character(pred_f$errors[24:27, 3]))
pred_f4 <- c(pred_f1, pred_f2, pred_f3)
mean(pred_f4)  #mean predictability = 0.374




graph_f <- plot(network_f, layout = "spring", 
                cut=0, maximum = 0.6, 
                groups=depr_f$grouplabels, palette = "colorblind",
                nodeNames=depr_f$names, labels = depr_f$labels, 
                vsize=4.5,
                label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                
                pie = pred_f4, pieBorder = 0.25) 



centralityPlot(graph_f, scale = "z-scores", include = c("ExpectedInfluence"),
               labels = depr_f$names)




ei_f <- centralityTable(graph_f, labels = depr_f$names)
ei_f <- subset(ei_f, measure == "ExpectedInfluence") 







# Male network ------------------------------------------------------------




depr_m <- list("data" = NULL,
               "type" = NULL,
               "level" = NULL,
               "names" = NULL,
               "labels" = NULL,
               "group labels" = NULL)

depr_m$data <- as.matrix(subset(mhm, sex == 1, select = -sex))
depr_m$type <- c(rep("g", 18), rep("c", 5), rep("g", 4))
depr_m$level <- c(rep(1, 18), rep(2, 5), rep(1, 4))
depr_m$names <- c(colnames(mhm[,c(1:18)]), "employment: 0=unemployed, 1=employed",
                  "relationship status: 0=single, 1=relationship", "seeking mental health treatment: 0=no, 1=yes",
                  "medical diagnosis: 0=no, 1=yes", "childhood trauma: 0=no, 1=yes",
                  "exercise frequency", "frequency of socializing",
                  "age", "educational attainment")
depr_m$labels <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10",
                   "D11", "D12", "D13", "D14", "D15", "D16", "D17", "D18",
                   "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9")
depr_m$grouplabels <- list("Depressive Symptoms" = c(1:18), "Covariates" = c(19:27))







network_m <- estimateNetwork(data = depr_m$data,
                                default = c("mgm"),
                                type = depr_m$type,
                                level = depr_m$level,
                                criterion = "EBIC",
                                tuning = 0.25,
                                binarySign = T )


save(network_m, file = "network_m.RData")


pred_m <- predict(network_m, depr_m$data)
pred_m$errors   
pred_m1 <- as.numeric(as.character(pred_m$errors[1:18, 3]))
mean(pred_m1)  #mean predictability of depr items = 0.518
pred_m2 <- as.numeric(as.character(pred_m$errors[19:23, 5]))
pred_m3 <- as.numeric(as.character(pred_m$errors[24:27, 3]))
pred_m4 <- c(pred_m1, pred_m2, pred_m3)
mean(pred_m4)  #mean predictability = 0.39





graph_m <- plot(network_m, layout = "spring", 
                cut=0, maximum = 0.6, 
                groups=depr_m$grouplabels, palette = "colorblind",
                nodeNames=depr_m$names, labels = depr_m$labels, 
                vsize=4.5,
                label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                
                pie = pred_m4, pieBorder = 0.25) 



centralityPlot(graph_m, scale = "z-scores", include = c("ExpectedInfluence"),
               labels = depr_m$names)



ei_m <- centralityTable(graph_m, labels = depr_m$names)
ei_m <- subset(ei_m, measure == "ExpectedInfluence") 








# stability tests ---------------------------------------------------------

bf1 <- bootnet(network_f, boots = 500, nCores = 4, 
               statistics = c("expectedInfluence", "edge"))

bf2 <- bootnet(network_f, boots = 500, nCores = 4, 
                    statistics = c("expectedInfluence", "edge"),
                    type = "case")

bm1 <- bootnet(network_m, boots = 500, nCores = 4, 
              statistics = c("expectedInfluence", "edge"))

bm2 <- bootnet(network_m, boots = 500, nCores = 4, 
               statistics = c("expectedInfluence", "edge"),
               type = "case")

save(bf1, file = "bf1.RData")
save(bf2, file = "bf2.RData")
save(bm1, file = "bm1.RData")
save(bm2, file = "bm2.RData")



#centrality stability coefficient
# 
corStability(bf2)
plot(bf2, statistic = "edge")
plot(bf2, statistic = "expectedInfluence")

corStability(bm2)
plot(bm2, statistic = "edge")
plot(bm2, statistic = "expectedInfluence")

#edge stability graph
plot(bf1, labels = F, order = "sample")
plot(bm1, labels = F, order = "sample")


# EI difference test, black squares = significant, gray = nonsignificant
plot(bf1, "expectedInfluence", order = "sample", labels = T)
plot(bm1, "expectedInfluence", order = "sample", labels = T)


#edge weights difference test
plot(bf1, "edge", plot = "difference", onlyNonZero = T, order = "sample", labels = F)
plot(bm1, "edge", plot = "difference", onlyNonZero = T, order = "sample")





# network comparison ------------------------------------------------------


sum(abs(network_f$results$pairwise$wadj))   # = 36.378
sum(abs(network_f$results$pairwise$wadj[1:18,]))  # = 23.32

sum(abs(network_m$results$pairwise$wadj))   # = 35.558
sum(abs(network_m$results$pairwise$wadj[1:18,]))  # = 22.63



### correlation of absolute value of edge weights  = 0.923
cor(as.vector(network_f$results$pairwise$wadj),
    as.vector(network_m$results$pairwise$wadj))


### put signs on edge weights
adjm_f <- as.vector(network_f$results$pairwise$wadj) * network_f$results$pairwise$signs
adjm_m <- as.vector(network_m$results$pairwise$wadj) * network_m$results$pairwise$signs

### correlate networks = 0.945
cor(as.vector(adjm_f), as.vector(adjm_m))


edgediff <- adjm_f - adjm_m
edgediff <- as.matrix(edgediff)
edgediff <- abs(edgediff)

colnames(edgediff) <- as.list(colnames(select(mhm, -sex)))
rownames(edgediff) <- as.list(colnames(select(mhm, -sex)))


### keep only significantly different edges
colnames(nct$einv.pvals) <- c("V1", "V2", "p")
nctedges <- subset(nct$einv.pvals, p < 0.01)




### correlation of expected influence = 0.777
cor(as.vector(ei_f$value),
    as.vector(ei_m$value))



nct <- NCT(network_f, network_m, it = 500, weighted = T,
           test.edges = T, edges = "all",
           test.centrality = T,
           centrality = c("expectedInfluence"), nodes = "all",
           
           progressbar = T,
           verbose = T)

save(nct, file = "nct.RData")

summary(nct)

# NETWORK INVARIANCE TEST
# Test statistic M:  0.2015938 
# p-value 0 
# 
#edges are significantly different between networks


# GLOBAL STRENGTH INVARIANCE TEST
# Global strength per group:  18.18882 17.77921 
# Test statistic S:  0.4096133 
# p-value 0.726 
#
#no difference in global strength (network density)






