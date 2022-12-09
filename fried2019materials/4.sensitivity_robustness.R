##################################################################################
#                                                                                #
#           Using network analysis to examine links between individual           #
#            depressive symptoms, inflammatory markers, and covariates           #   
#                                                                                #
#                            Fried et al., 2019                                  #
#                          Psychological Medicine                                #
#                           https://osf.io/6ehrm/                                #
#                                                                                #
#             4. Syntax file for sensitivity & robustness analyses               #
#                                                                                #
#                         Last updated: 08-09-2019                               #
#                                                                                #
##################################################################################

# Goal1: find out whether adding more parameters to the network models ...
# ... explains why networks with more parameter do not recover depression-inflammation links

# Goal2: re-estimate network 3b without regularization whilst ... 
# ... still controlling for multiple testing



# -------------------------------------------------------------------------
# --------------- compare 2b and 3b ---------------------------------------
# -------------------------------------------------------------------------

### generate 21 random variables so we have the same number of variables as 3b
set.seed(1337)
df_r2 <- matrix(rnorm(21*2321), ncol=21)
df_r2 <- cbind(data_n_2b$data, df_r2)
type_r2 <- c(data_n_2b$type, rep("g", 21))
level_r2 <- c(data_n_2b$level, rep(1, 21))

# estimate new model 2b with 21 random variables
n_r2 <- mgm(data = df_r2, 
                 type = type_r2, 
                 level = level_r2, 
                 lambdaSel = "EBIC",
                 lambdaGam = 0, 
                 binarySign = TRUE)

# compare 2b original and new
N2bmgm0 <- qgraph(bs_network2b_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                  maximum = 0.4,
                  groups=RepGroups2b, palette = "colorblind",
                  nodeNames=RepNames2b, labels = RepLabels2b, vsize=4.5,
                  label.cex=1.3, legend.cex=.50, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network2b_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                  filetype = "pdf", filename = "Sensitivity 2 Panel B gam0")

n_r2_plot <- qgraph(n_r2$pairwise$wadj[c(1:17), c(1:17)], layout = N2bmgm0$layout, cut=0, 
                    maximum = 0.4, groups=RepGroups2b, palette = "colorblind",
                    nodeNames=RepNames2b, labels = RepLabels2b, vsize=4.5,
                    label.cex=1.3, legend.cex=.50, GLratio = 1.8,
                    edge.color = matrix(as.vector(lapply(n_r2$pairwise$edgecolor[c(1:17), c(1:17)], function(x) {gsub("darkgreen", "blue", x)})), nrow=17, ncol=17),
                    theme='colorblind',
                    filetype = "pdf", filename = "Sensitivity 2 Panel B gam0 random") 

# ---------- comparison ----------

# absolute sum of adjacency matrices for the two networks
sum(abs(bs_network2b_gam0_init$pairwise$wadj)) #13.08535
sum(abs(n_r2$pairwise$wadj[c(1:17), c(1:17)])) #11.75213
11.75213 /13.08535 # Reduced to 89.8%

# absolute sum of all edges of CRP
sum(abs(bs_network2b_gam0_init$pairwise$wadj[1,])) # 0.5959818
sum(abs(n_r2$pairwise$wadj[1,]))                   # 0.4887698
0.4887698 / 0.5959818 # Reduced to 82%

# both symptom-marker associations disappear once adding 17 random variables; so power is an explanation.

cor(as.vector(bs_network2b_gam0_init$pairwise$wadj), as.vector(n_r2$pairwise$wadj[c(1:17), c(1:17)])) #.993



# -------------------------------------------------------------------------
# --------------- compare 3a and 3b ---------------------------------------
# -------------------------------------------------------------------------

# generate 7 random variables so we have the same number of variables as 3b
set.seed(1337)
df_r3 <- matrix(rnorm(7*2321), ncol=7)
df_r3 <- cbind(data_n_3a$data, df_r3)
type_r3 <- c(data_n_3a$type, rep("g", 7))
level_r3 <- c(data_n_3a$level, rep(1, 7))

# estimate new model 3a with 7 random variables
n_r3 <- mgm(data = df_r3, 
            type = type_r3, 
            level = level_r3, 
            lambdaSel = "EBIC",
            lambdaGam = 0, 
            binarySign = TRUE)

# compare 3a original and new
N3amgm0 <- qgraph(bs_network3a_gam0_init$pairwise$wadj, layout = "spring", cut=0,
                  maximum = 0.4, 
                  groups=GroupLabels3a, palette = "colorblind",
                  nodeNames=Names3a, labels = Labels3a, vsize=4.5,
                  label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                  edge.color = matrix(as.vector(lapply(bs_network3a_gam0_init$pairwise$edgecolor, function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),
                  title="Network 3a gamma 0",  filetype = "pdf", filename = "Sensitivity 3 Panel A gam0") 

n_r3_plot <- qgraph(n_r3$pairwise$wadj[c(1:31), c(1:31)], layout = N3amgm0$layout, cut=0, 
                    maximum = 0.4, groups=GroupLabels3a, palette = "colorblind",
                    nodeNames=Names3a, labels = Labels3a, vsize=4.5,
                    label.cex=1.3, legend.cex=.38, GLratio = 1.8,
                    edge.color = matrix(as.vector(lapply(bs_network3a_gam0_init$pairwise$edgecolor[c(1:31), c(1:31)], function(x) {gsub("darkgreen", "blue", x)})), nrow=31, ncol=31),theme='colorblind',
                    title="Network 3a gamma 0 random", filetype = "pdf", filename = "Sensitivity 3 Panel A gam0 random") 

# ---------- comparison ----------

# absolute sum of adjacency matrices for the two networks
sum(abs(bs_network3a_gam0_init$pairwise$wadj)) #19.70415
sum(abs(n_r3$pairwise$wadj[c(1:31), c(1:31)])) #19.57883
19.57883 /19.70415 # Reduced to 99.4%

# absolute sum of all edges of 3 inflammatory markers
sum(abs(bs_network3a_gam0_init$pairwise$wadj[c(1:3),])) # 0.76660
sum(abs(n_r3$pairwise$wadj[c(1:3),]))                   # 0.76393
0.76393 / 0.76660 # Reduced to 99.7%

bs_network3a_gam0_init$pairwise$wadj[c(1:3), c(4:31)] # 2 edges with symptom aches&pain

cor(as.vector(bs_network3a_gam0_init$pairwise$wadj), as.vector(n_r3$pairwise$wadj[c(1:31), c(1:31)])) #.999



#             |￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                  This is the end of  
#                  part 4: robustness
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ


    