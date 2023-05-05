##################################################################################
#                                                                                #
#           Using network analysis to examine links between individual           #
#            depressive symptoms, inflammatory markers, and covariates           #   
#                                                                                #
#                            Fried et al., 2019                                  #
#                          Psychological Medicine                                #
#                           https://osf.io/6ehrm/                                #
#                                                                                #
#               5. Syntax file for creating network bootstraps                   #
#                                                                                #
#                         Last updated: 08-09-2019                               #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- Loading packages & Data ---------------------------------
# -------------------------------------------------------------------------

library(mgm) 
library(bootnet)
library(OpenMx)



# -------------------------------------------------------------------------
# --------------- Global Settings -----------------------------------------
# -------------------------------------------------------------------------

# set working directories
setwd("~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis")
dataPrDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/data_processed/"
bsDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/bs_files/"
figDir <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/bs_plots/"



# -------------------------------------------------------------------------
# --------------- Load data -----------------------------------------------
# -------------------------------------------------------------------------

data_n_1a <- readRDS(file = paste0(dataPrDir, "data_n_1a.RDS"))
data_n_1b <- readRDS(file = paste0(dataPrDir, "data_n_1b.RDS"))
data_n_2a <- readRDS(file = paste0(dataPrDir, "data_n_2a.RDS"))
data_n_2b <- readRDS(file = paste0(dataPrDir, "data_n_2b.RDS"))
data_n_3a <- readRDS(file = paste0(dataPrDir, "data_n_3a.RDS"))
data_n_3b <- readRDS(file = paste0(dataPrDir, "data_n_3b.RDS"))

### load bootstraps 
# load(file = paste0(bsDir, "bs1a.RData"))
# load(file = paste0(bsDir, "bs1b.RData"))
# load(file = paste0(bsDir, "bs2a.RData"))
# load(file = paste0(bsDir, "bs2b.RData"))
# load(file = paste0(bsDir, "bs3a.RData"))
# load(file = paste0(bsDir, "bs3b.RData"))
# load(file = paste0(bsDir, "bs4a.RData"))



# -------------------------------------------------------------------------
# --------------- Re-estimate all networks via bootnet --------------------
# -------------------------------------------------------------------------

# First replace the labels so plots look prettier
colnames(data_n_1a$data)<-Labels1a
colnames(data_n_1b$data)<-Labels1b
colnames(data_n_2a$data)<-RepLabels2a
colnames(data_n_2b$data)<-RepLabels2b
colnames(data_n_3a$data)<-Labels3a
colnames(data_n_3b$data)<-Labels3b

# Then re-estimate all networks via bootnet
n1a <- estimateNetwork(data = data_n_1a$data,   
                       type = data_n_1a$type, 
                       level = data_n_1a$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0)

n1b <- estimateNetwork(data = data_n_1b$data,   
                       type = data_n_1b$type, 
                       level = data_n_1b$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0)     

n2a <- estimateNetwork(data = data_n_2a$data,   
                       type = data_n_2a$type, 
                       level = data_n_2a$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0)     

n2b <- estimateNetwork(data = data_n_2b$data,   
                       type = data_n_2b$type, 
                       level = data_n_2b$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0)     

n3a <- estimateNetwork(data = data_n_3a$data,   
                       type = data_n_3a$type, 
                       level = data_n_3a$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0)     

n3b <- estimateNetwork(data = data_n_3b$data,   
                       type = data_n_3b$type, 
                       level = data_n_3b$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0)     

n4a <- estimateNetwork(data = data_n_3b$data,   
                       type = data_n_3b$type, 
                       level = data_n_3b$level, 
                       default="mgm",
                       criterion = "EBIC", 
                       tuning = 0,
                       lambdaSeq = 0)   

### check if bootnet results match mgm estimation results, using absolute values ...
### ... for estimateNetwork because this is the way pairwise$wadj is encoded in mgm

cor(vech(bs_network1a_gam0_init$pairwise$wadj), vech(abs(n1a$graph)))
cor(vech(bs_network1b_gam0_init$pairwise$wadj), vech(abs(n1b$graph)))

cor(vech(bs_network2a_gam0_init$pairwise$wadj), vech(abs(n2a$graph)))
cor(vech(bs_network2b_gam0_init$pairwise$wadj), vech(abs(n2b$graph)))

cor(vech(bs_network3a_gam0_init$pairwise$wadj), vech(abs(n3a$graph)))
cor(vech(bs_network3b_gam0_init$pairwise$wadj), vech(abs(n3b$graph)))

cor(vech(bs_network3b_init$pairwise$wadj),      vech(abs(n4a$graph)))

# all are 1, good to go



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 1a ------------------------------------
# -------------------------------------------------------------------------

# Set bootstrap number
nB <- 500 

# Bootstrap:
bs1a <- bootnet(n1a, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs1a.pdf"))
plot(bs1a, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs1a_diff.pdf"))
plot(bs1a, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_1a.RData")
save(bs1a, file = paste0(bsDir, "bs1a.RData"))



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 1b ------------------------------------
# -------------------------------------------------------------------------

bs1b <- bootnet(n1b, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs1b.pdf"))
plot(bs1b, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs1b_diff.pdf"))
plot(bs1b, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_1b.RData")
save(bs1b, file = paste0(bsDir, "bs1b.RData"))



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 2a ------------------------------------
# -------------------------------------------------------------------------

bs2a$bootTable$graph

bs2a <- bootnet(n2a, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs2a.pdf"), height=ncol(data_n_2a$data))
plot(bs2a, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs2a_diff.pdf"))
plot(bs2a, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_2a.RData")
save(bs2a, file = paste0(bsDir, "bs2a.RData"))



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 2b ------------------------------------
# -------------------------------------------------------------------------

bs2b <- bootnet(n2b, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs2b.pdf"), height=ncol(data_n_2b$data))
plot(bs2b, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs2b_diff.pdf"))
plot(bs2b, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_2b.RData")
save(bs2b, file = paste0(bsDir, "bs2b.RData"))



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 3a ------------------------------------
# -------------------------------------------------------------------------

bs3a <- bootnet(n3a, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs3a.pdf"), height=ncol(data_n_3a$data)*2)
plot(bs3a, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs3a_diff.pdf"))
plot(bs3a, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_3a.RData")
save(bs3a, file = paste0(bsDir, "bs3a.RData"))



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 3b ------------------------------------
# -------------------------------------------------------------------------

bs3b <- bootnet(n3b, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs3b.pdf"), height=ncol(data_n_3b$data)*2)
plot(bs3b, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs3b_diff.pdf"))
plot(bs3b, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_3b.RData")
save(bs3b, file = paste0(bsDir, "bs3b.RData"))



# -------------------------------------------------------------------------
# --------------- Bootstrap Network 4 ------------------------------------
# -------------------------------------------------------------------------

bs4a <- bootnet(n4a, nBoots = nB, nCores = 8)

pdf(paste0(figDir, "bs4a.pdf"), height=ncol(data_n_3b$data)*2)
plot(bs4a, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(figDir, "bs4a_diff.pdf"))
plot(bs4a, order="sample", plot="difference", onlyNonZero=TRUE)
dev.off()

# load(file = "bs_n_4a.RData")
save(bs4a, file = paste0(bsDir, "bs4a.RData"))


#             |￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                  This is the end of  
#                 part 5: bootstrapping
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ



