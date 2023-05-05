
# Social Anxiety and Eating Disorder Comorbidity and Underlying Vulnerabilities: Using Network Analysis to Conceptualize Comorbidity 

##Analyses were conducted using MacOS Version 10.12.2, RStudio Version 1.0.143

# ---- Data Preparation: Multiple imputation, goldbricker ----
# ---- Step 1: Multiple Imputation ----
library(mice) #Version 2.46.0 at time of analysis
SAED <- read.table("SAED.csv", header=TRUE, sep=",")
summary(SAED)
#specify missing value (in this case, -99)
replace99toNA <- function(x){
  y <-x
  y[y==-99] <- NA
  return(y)
}
SAED <- as.data.frame(apply(SAED, 2, replace99toNA)) ## recode -99 as NA
md.pattern(SAED)
library(VIM) #Version 4.7.0 at time of analysis
mice_plot <- aggr(SAED, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(SAED), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
imputed_SAED <- mice(SAED, m=5, maxit = 50, method = 'pmm', seed = 500)
load("completed_imputation.Rdata")
summary(imputed_SAED)
imputed_SAED$imp$BMI

completeSAED2 <- complete(imputed_SAED,2)

# ---- Step 2: comparing correlations using goldbricker----
## Depends: cocor, qgraph
library(cocor) #Version 1.1-3 at time of analysis
library(qgraph) #Version 1.4.4 at time of analysis
library(devtools) #Version 1.13.4 at time of analysis
install_github("paytonjjones/networktools") ## Version at time of analysis: SHA=2dbd99a7
library(networktools)

?goldbricker ## function from package networktools

####run
gb_SAED <- goldbricker(completeSAED2, threshold=0.25, p=0.01)
gb_SAED
gb_SAED$proportion_matrix

## Based on these results, we removed one item from each
## pair, resulting in the removal of the following items:
## Sps4, Fmps18, Sias12, fmps25, Fmps34




# ---- Data Analyses: SAED1, SAED2, SAED3 ----
# ---- SAED1 - SA and ED variables only ----
###Eating Disorder Examination Questionnaire items: edeq1, edeq2, edeq3, edeq4, edeq5, edeq6, edeq13, edeq14, edeq32, edeq33, edeq38, edeq39, edeq40
###Eating Disorder Inventory-2 items: edi3, edi7, edi8, edi14, edi16, edi17, edi19
###Social Interaction Anxiety Scale items: sias2, sias3
###Social Phobia Scale items: sps2, sps3, sps6, sps9, sps10, sps11, sps13, sps14, sps15, sps20
SAED1 <- read.table("/Users/Data/SAED only.csv", header=TRUE, sep=",")
summary(SAED1)
library(qgraph)
library(bootnet) #Version 1.0.1 at time of analysis
library(dplyr) #Version 0.7.4 at time of analysis
library(networktools)
n=32 #number of nodes
names1 <- c("restrict","fast", "avoid foods", "food rules", "food interfere", "fear stop eat", 
            "fear gain wt", "felt fat", "wt judge", "shape judge", "ppl see eat", "see body", "others see body",
            "big stomach", "guilty overeat", "binge eat", "stuff", "desire thin",
            "vomit", "keep gain wt", "eye contact", "talk feelings", "public toilet", "listen voice",
            "enter room", "see faint", "drink public", "eat public", "carry tray", "lose control", "attract attention", "ppl watching")
longnames1 <- c('my stomach is too big', 'feel guilty after overeating', 'I have gone on eating binges', 'I stuff myself with food when others are not around', 'preoccupation with the desire to be thinner', 'vomit to lose weight', 'worry that I will keep gaining weight', 'restriction', 'fasting', 'avoid certain foods',  'food rules', 'fear of not being able to stop eating', 'loss of control over eating', 'fear of gaining weight or becoming fat', 'feel fat', 'overvaluation of weight', 'overvaluation of shape', 'other people seeing you eat', 'discomfort seeing your own body', 'discomfort with others seeing your body', 'difficulty making eye contact with others', 'tense when talking about myself', 'using public toilet', 'aware of others listening to me', 'feel self-conscious when entering a room', 'panicky that others will see me faint',  'others seeing you drink in public', 'eating in public', 'carrying tray across crowded cafeteria', 'worry I will lose control in front of others', 'worry about attracting attention', 'tense when others are watching me')
groupsSAED1=list("ED"=c(1:20),"SA"=c(21:32)) #create groups
SAEDnetwork1 <- estimateNetwork(SAED1, default="EBICglasso")
pdf("SAEDnetwork1.pdf", width=10)
glassoSAED1<-plot(SAEDnetwork1, labels=names1, layout="spring", vsize=6, cut=0, border.width=1.5, border.color="black", groups=groupsSAED1, color=c('#a8e6cf', '#dcedc1'),nodeNames = longnames1, legend.cex=.4)
dev.off()

#Centrality
centralitySAED1 <- centralityPlot(glassoSAED1)
write.csv(centralitySAED1, "centralitySAED1.csv")
centrality(SAEDnetwork1)$InDegree
scale(centrality(SAEDnetwork1)$InDegree)
centrality(SAEDnetwork1)$Closeness
centrality(SAEDnetwork1)$Betweenness
CentralSAED1 <- centralityTable(glassoSAED1)
write.csv(CentralSAED1, "CentralSAED1.csv")
cor(centrality(SAEDnetwork1)$InDegree, centrality(SAEDnetwork1)$Betweenness, 
    method = "spearman") 
pdf("SAEDcentrality1.pdf", width=10)

# ---- Bridge Analyses (SAED1) ----
#first is to create values for all bridge centralities#
bridge(glassoSAED1, communities=c('1','1','1','1','1','1','1','1','1','1',
                                  '1','1','1','1','1','1','1','1','1','1',
                                  '2','2','2','2','2','2','2','2','2','2','2','2'), useCommunities = "all", directed = NULL, nodes = NULL)
#this is to create graph#
SAED1bridge <- bridge(glassoSAED1, communities=c('1','1','1','1','1','1','1','1','1','1',
                                                 '1','1','1','1','1','1','1','1','1','1',
                                                 '2','2','2','2','2','2','2','2','2','2','2','2'), useCommunities = "all", directed = NULL, nodes = NULL)
pdf("SAED1bridge.pdf", width = 15)
plot(SAED1bridge)
dev.off()

#Constructing a partial correlation matrix 
SAED1Gedges <-getWmat(SAEDnetwork1)
write.csv(SAED1Gedges, "SAED1edges.csv")

# ---- Stability analysis (SAED1) ----
SAED1boot1<- bootnet(SAEDnetwork1, nBoots=1000,nCores=8)
SAED1boot2 <- bootnet(SAEDnetwork1, nBoots=1000,nCores=8, type="case")
save(SAED1boot1, file = "SAED1boot1.Rdata")
save(SAED1boot2, file = "SAED1boot2.Rdata")
##If closed R after saving bootnet files you can reload them below#
load("SAED1boot1.Rdata")
load("SAED1boot2.Rdata")

### Plot edge weight CI
pdf("StabilitySAED1.pdf")
plot(SAED1boot1, labels = FALSE, order = "sample") 
dev.off()

### Edge weights diff test
pdf("SAED1wdifftest.pdf")
plot(SAED1boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

### Plot centrality stability
pdf("SAED1stability.pdf") 
plot(SAED1boot2)
dev.off()

### Centrality stability coefficient
corStability(SAED1boot2)

### Centrality diff test
pdf("SAED1cdifftest.pdf")
plot(SAED1boot1, "strength", order="sample", labels=TRUE, names=names1) 
dev.off()

# ---- NCT for SAED1 ----
SAEDclinical1 <- read.table("C:/Users/Data/NCT/SAEDclinical1.csv", header=TRUE, sep=",")
SAEDnonclinical1 <- read.table("C:/Users/Data/NCT/SAEDnonclinical1.csv", header=TRUE, sep=",")
library(NetworkComparisonTest) #Version 2.0.1 at time of analysis
NCT1 <- NCT(SAEDclinical1, SAEDnonclinical1, it = 1000,  binary.data=FALSE, paired=FALSE, test.edges=TRUE, edges='all', progressbar=TRUE)
summary(NCT1)

##Compute correlation of strength centralities###
network1 <- estimateNetwork(SAEDclinical1, default="EBICglasso")
network2 <- estimateNetwork(SAEDnonclinical1, default="EBICglasso")
cor(centrality(network1)$InDegree, centrality(network2)$InDegree)

#SAEDnonclinical1
summary(SAEDnonclinical1)
n=32 #number of nodes
groupsSAEDnonclinical1=list("ED"=c(1:20),"SA"=c(21:32)) #create groups
SAEDnonclinicalnetwork1 <- estimateNetwork(SAEDnonclinical1, default="EBICglasso")
pdf("SAEDnonclinicalnetwork1.pdf", width=10)
glassoSAEDnonclinical1<-plot(SAEDnonclinicalnetwork1, labels=names1, layout="spring", vsize=6, cut=0, border.width=1.5, border.color="black", 
                             groups=groupsSAEDnonclinical1, color=c('#a8e6cf', '#dcedc1'), nodeNames = names1,legend.cex=.4)
dev.off()

#Centrality
centralitySAEDnonclinical1 <- centralityPlot(glassoSAEDnonclinical1)
write.csv(centralitySAEDnonclinical1, "centralitySAEDnonclinical1.csv")
centrality(SAEDnonclinicalnetwork1)$InDegree
scale(centrality(SAEDnonclinicalnetwork1)$InDegree)
centrality(SAEDnonclinicalnetwork1)$Closeness
centrality(SAEDnonclinicalnetwork1)$Betweenness
CentralSAEDnonclinical1 <- centralityTable(glassoSAEDnonclinical1)
write.csv(CentralSAEDnonclinical1, "CentralSAEDnonclinical1.csv")
cor(centrality(SAEDnonclinicalnetwork1)$InDegree, centrality(SAEDnonclinicalnetwork1)$Betweenness, 
    method = "spearman") 
pdf("SAEDnonclinicalcentrality1.pdf", width=10)

#SAEDclinical1
summary(SAEDclinical1)
n=32 #number of nodes
groupsSAEDclinical1=list("ED"=c(1:20),"SA"=c(21:32)) #create groups
SAEDnetworkclinical1 <- estimateNetwork(SAEDclinical1, default="EBICglasso")
pdf("SAEDnetworkclinical1.pdf", width=10)
glassoSAEDclinical1<-plot(SAEDnetworkclinical1, labels=names1, layout="spring", vsize=6, cut=0, border.width=1.5, border.color="black", 
                          groups=groupsSAEDclinical1, color=c('#a8e6cf', '#dcedc1'), nodeNames = names1,legend.cex=.4)
dev.off()

#Centrality
centralitySAEDclinical1 <- centralityPlot(glassoSAEDclinical1)
write.csv(centralitySAEDclinical1, "centralitySAEDclinical1.csv")
centrality(SAEDnetworkclinical1)$InDegree
scale(centrality(SAEDnetworkclinical1)$InDegree)
centrality(SAEDnetworkclinical1)$Closeness
centrality(SAEDnetworkclinical1)$Betweenness
CentralSAEDclinical1 <- centralityTable(glassoSAEDclinical1)
write.csv(CentralSAEDclinical1, "CentralSAEDclinical1.csv")
cor(centrality(SAEDnetworkclinical1)$InDegree, centrality(SAEDnetworkclinical1)$Betweenness, 
    method = "spearman") 
pdf("SAEDcentralityclinical1.pdf", width=10)

###END OF SAED1###################################################################

# ---- SAED2 – SA, ED, with SAAS ----
###Social Appearance Anxiety Scale items: saas4, saas5, saas8, saas9, saas10, saas11, saas12, saas13, saas14, saas15
###Eating Disorder Examination Questionnaire items: edeq1, edeq2, edeq3, edeq4, edeq5, edeq6, edeq13, edeq14, edeq32, edeq33, edeq38, edeq39, edeq40
###Eating Disorder Inventory-2 items: edi3, edi7, edi8, edi14, edi16, edi17, edi19
###Social Interaction Anxiety Scale items: sias2, sias3
###Social Phobia Scale items: sps2, sps3, sps6, sps9, sps10, sps11, sps13, sps14, sps15, sps20

SAED2 <- read.table("/Users/Data/SAED SAA.csv", header=TRUE, sep=",")
summary(SAED2)
n=42 #number of nodes
names2 <- c("appearance reject", "appearance flaws", "appearance difficult", "appearance opport", "nervous appearance", "appearance comment", "appearance standards", "appearance judge", "notice flaws", "appearance abandon",  "restrict", "fast", "avoid foods", "food rules", "food interfere", "fear stop eat", "fear gain wt",  "felt fat", "wt judge", "shape judge", "ppl see eat", "see body", "others see body",
            "big stomach", "guilty overeat", "binge eat", "stuff", "desire thin", "vomit", "keep gain wt", "eye contact", "talk feelings", "public toilet", "listen voice","enter room", "see faint", "drink public", "eat public", "carry tray", "lose control", "attract attention", "ppl watching")
longnames2 <- c('rejection based on appearance', 'people talk about my appearance flaws', 'appearance makes life more difficult',  'missed opportunities because of my appearance', 'feel nervous about my appearance', 'feel anxious when people comment on my appearance',  'fear of not meeting appearance standards', 'judgment on appearance', 'others noticing flaws in my appearance', 'abandonment because of appearance', 'my stomach is too big', 'feel guilty after overeating', 'I have gone on eating binges', 'I stuff myself with food when others are not around',   'preoccupation with the desire to be thinner', 'vomit to lose weight', 'worry that I will keep gaining weight', 'restriction', 'fasting', 'avoid certain foods',  'food rules', 'fear of not being able to stop eating', 'loss of control over eating', 'fear of gaining weight or becoming fat', 'feel fat', 'overvaluation of weight',  'overvaluation of shape', 'other people seeing you eat', 'discomfort seeing your own body', 'discomfort with others seeing your body', 'difficulty making eye contact with others', 'tense when talking about myself', 'using public toilet', 'aware of others listening to me', 'feel self-conscious when entering a room', 'panicky that others will see me faint',  'others seeing you drink in public', 'eating in public', 'carrying tray across crowded cafeteria', 'worry I will lose control in front of others', 'worry about attracting attention', 'tense when others are watching me')
groupsSAED2=list("SAA"=c(1:10),"ED"=c(11:30),"SA"=c(31:42)) #create groups
SAEDnetwork2 <- estimateNetwork(SAED2, default="EBICglasso")
pdf("SAEDnetwork2.pdf", width=15, height=10)
glassoSAED2<-plot(SAEDnetwork2, labels=names2, layout="spring", vsize=6, cut=0, border.width=1.5, border.color="black",  groups=groupsSAED2, color=c('#ffd3b6','#a8e6cf', '#dcedc1'), nodeNames = longnames2, legend.cex=.4)
dev.off()

#Centrality
centralitySAED2 <- centralityPlot(glassoSAED2)
centrality(SAEDnetwork2)$InDegree
scale(centrality(SAEDnetwork2)$InDegree)
centrality(SAEDnetwork2)$Closeness
centrality(SAEDnetwork2)$Betweenness
cor(centrality(SAEDnetwork2)$InDegree, centrality(SAEDnetwork2)$Betweenness, 
    method = "spearman") 

# ---- Bridge Analyses (SAED2) ----
bridge(glassoSAED2, communities=c('1','1','1','1','1','1','1','1','1',"1",
                                  '2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2',
                                  '3','3','3','3','3','3','3','3','3','3','3','3'), useCommunities = "all", directed = NULL, nodes = NULL)
#this is used to get a graph)
SAED2bridge <- bridge(glassoSAED2, communities=c('1','1','1','1','1','1','1','1','1',"1",
                                                 '2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2',
                                                 '3','3','3','3','3','3','3','3','3','3','3','3'), useCommunities = "all", directed = NULL, nodes = NULL)
pdf("SAED2bridgeALL.pdf", width = 15)
plot(SAED2bridge)
dev.off()

#Constructing a partial correlation matrix 
SAED2edges <-getWmat(SAEDnetwork2)
write.csv(SAED2edges, "SAED2edges.csv")

# ---- Stability analysis (SAED2) ----
SAED2boot1<- bootnet(SAEDnetwork2, nBoots=1000,nCores=8)
SAED2boot2 <- bootnet(SAEDnetwork2, nBoots=1000,nCores=8, type="case")
save(SAED2boot1, file = "SAED2boot1.Rdata")
save(SAED2boot2, file = "SAED2boot2.Rdata")
##If closed R after saving bootnet files you can reload them below#
load("SAED2boot1.Rdata")
load("SAED2boot2.Rdata")

### Plot edge weight CI
pdf("StabilitySAED2.pdf")
plot(SAED2boot1, labels = FALSE, order = "sample") 
dev.off()

### Edge weights diff test
pdf("SAED2wdifftest.pdf")
plot(SAED2boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

### Plot centrality stability
pdf("SAED2stability.pdf") 
plot(SAED2boot2)
dev.off()

### Centrality stability coefficient
corStability(SAED2boot2)

### Centrality diff test
pdf("SAED2cdifftest.pdf")
plot(SAED2boot1, "strength", order="sample", labels=TRUE) 
dev.off()

# ---- NCT for SAED2 ----
SAEDclinical2 <- read.table("/Users/Data/NCT/SAEDclinical2.csv", header=TRUE, sep=",")
SAEDnonclinical2 <- read.table("/Users/Data/NCT/SAEDnonclinical2.csv", header=TRUE, sep=",")
NCT2 <- NCT(SAEDclinical2, SAEDnonclinical2, it = 100, binary.data = FALSE, weighted = TRUE)
summary(NCT2)

###END OF SAED2##################################################################

# ---- SAED 3 – SA, ED, with perfectionism  ----
###Eating Disorder Examination Questionnaire items: edeq1, edeq2, edeq3, edeq4, edeq5, edeq6, edeq13, edeq14, edeq32, edeq33, edeq38, edeq39, edeq40
###Eating Disorder Inventory-2 items: edi3, edi7, edi8, edi14, edi16, edi17, edi19
###Social Interaction Anxiety Scale items: sias2, sias3
###Social Phobia Scale items: sps2, sps3, sps6, sps9, sps10, sps11, sps13, sps14, sps15, sps20
###Frost Multidimensional Perfectionism Scale items: fmps4, fmps9, fmps10, fmps13, fmps14, fmps21, fmps23

SAED3 <- read.table("/Users/Data/SAED Perf.csv", header=TRUE, sep=",")
summary(SAED3)
names3 <- c("restrict", "fast", "avoid foods", "food rules", "food interfere", "fear stop eat", "fear gain wt", "felt fat", "wt judge", "shape judge",  "ppl see eat", "see body", "others see body", "big stomach", "guilty overeat", "binge eat", "stuff", "desire thin", "vomit", "keep gain wt", "eye contact", "talk feelings", "public toilet", "listen voice", "enter room", "see faint", "drink public", "eat public", "carry tray", "lose control", 
            "attract attention", "ppl watching", "high standards", "work failure", "upset mistakes","someone better", 
            "all-or-nothing perfect", "think less mistake","perfect inferior")
longnames3 <- c('my stomach is too big', 'feel guilty after overeating', 'I have gone on eating binges', 'I stuff myself with food when others are not around',  'preoccupation with the desire to be thinner', 'vomit to lose weight', 'worry that I will keep gaining weight', 'restriction', 'fasting', 'avoid certain foods',  'food rules', 'fear of not being able to stop eating', 'loss of control over eating', 'fear of gaining weight or becoming fat', 'feel fat', 'overvaluation of weight',  'overvaluation of shape', 'other people seeing you eat', 'discomfort seeing your own body', 'discomfort with others seeing your body', 'difficulty making eye contact with others','tense when talking about myself', 'using public toilet', 'aware of others listening to me', 'feel self-conscious when entering a room', 'panicky that others will see me faint',  'others seeing you drink in public', 'eating in public', 'carrying tray across crowded cafeteria', 'worry I will lose control in front of others', 'worry about attracting attention', 'tense when others are watching me', 'set high standards for myself', 'If I fail at work', 'I’m a failure as a person', 'I should be upset if I make a mistake',  'if someone does a task better than me, I failed the whole task', 'failing partly is as bad as being a complete failure', 'people will think less of me if I make a mistake', 'not doing as well as others makes me an inferior person')
n=39 #number of nodes
groupsSAED3=list(ED=1:20,SAD=21:32,FMPS=33:39) #create groups
SAEDnetwork3 <- estimateNetwork(SAED3, default="EBICglasso")
pdf("SAEDnetwork3.pdf", width=15,height=10)
glassoSAED3<-plot(SAEDnetwork3, labels=names3, layout="spring", vsize=6, cut=0, border.width=1.5, border.color="black",  groups=groupsSAED3, color=c('#a8e6cf', '#dcedc1', '#ffd3b6'), nodeNames = longnames3, legend.cex=.4)
dev.off()

#Centrality
centralitySAED3 <- centralityPlot(glassoSAED3)
centrality(SAEDnetwork3)$InDegree
scale(centrality(SAEDnetwork3)$InDegree)
centrality(SAEDnetwork3)$Closeness
centrality(SAEDnetwork3)$Betweenness
cor(centrality(SAEDnetwork3)$InDegree, centrality(SAEDnetwork3)$Betweenness, 
    method = "spearman") 
pdf("SAEDcentrality3.pdf", width=10)

# ---- Bridge analyses (SAED3) ----
bridge(glassoSAED3, communities=c('1','1','1','1','1','1','1','1','1','1','1','1',
                                  '2','2','2','2','2','2','2','2','2','2','2','2','2',
                                  '2','2','2','2','2','2','2',
                                  '3','3','3','3','3','3','3'), useCommunities = "all",
       directed = NULL, nodes = NULL)
#this is used to get a graph#
SAED3bridge <- bridge(glassoSAED3, communities=c('1','1','1','1','1','1','1','1','1','1','1','1',
                                                 '2','2','2','2','2','2','2','2','2','2','2','2','2',
                                                 '2','2','2','2','2','2','2',
                                                 '3','3','3','3','3','3','3'), useCommunities = "all",
                      directed = NULL, nodes = NULL)
pdf("SAED3bridgeALL.pdf", width = 15)
plot(SAED3bridge)
dev.off()

#Constructing a partial correlation matrix 
SAED3edges <-getWmat(SAEDnetwork3)
write.csv(SAED3edges, "SAED3edges.csv")

# ---- Stability analyses (SAED3) ----
SAED3boot1<- bootnet(SAEDnetwork3, nBoots=1000,nCores=8)
SAED3boot2 <- bootnet(SAEDnetwork3, nBoots=1000,nCores=8, type="case")
save(SAED3boot1, file = "SAED3boot1.Rdata")
save(SAED3boot2, file = "SAED3boot2.Rdata")
##If closed R after saving bootnet files you can reload them below#
load("SAED3boot1.Rdata")
load("SAED3boot2.Rdata")


### Plot edge weight CI
pdf("StabilitySAED3.pdf")
plot(SAED3boot1, labels = FALSE, order = "sample") 
dev.off()

### Edge weights diff test
pdf("SAED3wdifftest.pdf")
plot(SAED3boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

### Plot centrality stability
pdf("SAED3stability.pdf") 
plot(SAED3boot2)
dev.off()

### Centrality stability coefficient
corStability(SAED3boot2)

### Centrality diff test
pdf("SAED3cdifftest.pdf")
plot(SAED3boot1, "strength", order="sample", labels=TRUE) 
dev.off()


# ---- NCT for SAED3 ----
SAEDclinical3 <- read.table("/Users/Data/NCT/SAEDclinical3.csv", header=TRUE, sep=",")
SAEDnonclinical3 <- read.table("/Users/Data/NCT/SAEDnonclinical3.csv", header=TRUE, sep=",")
NCT3 <- NCT(SAEDclinical3, SAEDnonclinical3, it = 100, binary.data = FALSE, weighted = TRUE)
summary(NCT3)

###END OF SAED3##################################################################

#--- SAED 4 – SA, ED, with SAAS and perfectionism  ----
###Social Appearance Anxiety Scale items: saas4, saas5, saas8, saas9, saas10, saas11, saas12, saas13, saas14, saas15
###Eating Disorder Examination Questionnaire items: edeq1, edeq2, edeq3, edeq4, edeq5, edeq6, edeq13, edeq14, edeq32, edeq33, edeq38, edeq39, edeq40
###Eating Disorder Inventory-2 items: edi3, edi7, edi8, edi14, edi16, edi17, edi19
###Social Interaction Anxiety Scale items: sias2, sias3
###Social Phobia Scale items: sps2, sps3, sps6, sps9, sps10, sps11, sps13, sps14, sps15, sps20
###Frost Multidimensional Perfectionism Scale items: fmps4, fmps9, fmps10, fmps13, fmps14, fmps21, fmps23

SAED4 <- read.table("/Users/Data/SAED all.csv", header=TRUE, sep=",")
library(qgraph)
library(bootnet) 
library(dplyr)
n=49 #number of nodes
names4 <- c("appearance reject", "appearance flaws", "appearance difficult", "appearance opport", "nervous appearance", "appearance comment", "appearance standards", "appearance judge", "notice flaws", "appearance abandon", "restrict", "fast", "avoid foods", "food rules", "food interfere", "fear stop eat", "fear gain wt", "felt fat", "wt judge", "shape judge", "ppl see eat", "see body", "others see body", "big stomach", "guilty overeat", "binge eat", "stuff", "desire thin", "vomit", "keep gain wt", "eye contact", "talk feelings", "public toilet", "listen voice", "enter room", "see faint", "drink public", "eat public", "carry tray", "lose control",  "attract attention", "ppl watching", "high standards", "work failure", "upset mistakes","someone better", 
            "all-or-nothing perfect", "think less mistake", "perfect inferior")
longnames4 <- c('rejection based on appearance', 'people talk about my appearance flaws',          'appearance makes life more difficult',  'missed opportunities because of my appearance', 'feel nervous about my appearance', 'feel anxious when people comment on my appearance', 'fear of not meeting appearance standards', 'judgment on appearance', 'others noticing flaws in my appearance', 'abandonment because of appearance', 'restriction', 'fasting', 'avoid certain foods', 
                'food rules', 'fear of not being able to stop eating', 'loss of control over eating', 'fear of gaining weight or becoming fat', 'feel fat', 'overvaluation of weight', 'overvaluation of shape', 'other people seeing you eat', 'discomfort seeing your own body', 'discomfort with others seeing your body', 'my stomach is too big', 'feel guilty after overeating', 'I have gone on eating binges', 'I stuff myself with food when others are not around', 'preoccupation with the desire to be thinner', 'vomit to lose weight', 'worry that I will keep gaining weight', 'difficulty making eye contact with others','tense when talking about myself', 'using public toilet', 'aware of others listening to me', 'feel self-conscious when entering a room', 'panicky that others will see me faint', 'others seeing you drink in public', 'eating in public', 'carrying tray across crowded cafeteria', 'worry I will lose control in front of others', 'worry about attracting attention', 'tense when others are watching me', 'set high standards for myself', 'If I fail at work I’m a failure as a person', 'I should be upset if I make a mistake', 'if someone does a task better than me I failed the whole task', 'failing partly is as bad as being a complete failure', 'people will think less of me if I make a mistake', 'not doing as well as others makes me an inferior person')
groupsSAED4=list("SAA"=c(1:10),"ED"=c(11:30),"SA"=c(31:42), "FMPS" = c(43:49)) 
SAEDnetwork4 <- estimateNetwork(SAED4, default="EBICglasso")
pdf("SAEDnetwork4.pdf", width=20, height=15)
glassoSAED4<-plot(SAEDnetwork4, labels=names4, layout="spring", vsize=6, cut=0, border.width=1.5, border.color="black", groups=groupsSAED4, color=c('#ffd3b6','#a8e6cf', '#dcedc1', 'skyblue'), nodeNames = longnames4,legend.cex=.4)
dev.off()

#Centrality
centralitySAED4 <- centralityPlot(glassoSAED4)
centrality(SAEDnetwork4)$InDegree
scale(centrality(SAEDnetwork4)$InDegree)
centrality(SAEDnetwork4)$Closeness
centrality(SAEDnetwork4)$Betweenness
cor(centrality(SAEDnetwork4)$InDegree, centrality(SAEDnetwork4)$Betweenness, 
    method = "spearman") 

#---- Bridge analyses (SAED4) ----
bridge(glassoSAED4, communities=c('1','1','1','1','1','1','1','1','1',"1",
                                  '2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2',
                                  '3','3','3','3','3','3','3','3','3','3','3','3'), useCommunities = "all", directed = NULL, nodes = NULL) #this is used to get a graph
SAED4bridge <- bridge(glassoSAED4, communities=c('1','1','1','1','1','1','1','1','1',"1",
                                                 '2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2','2',
                                                 '3','3','3','3','3','3','3','3','3','3','3','3'), useCommunities = "all", directed = NULL, nodes = NULL)
pdf("SAED4bridgeALL.pdf", width = 15)
plot(SAED4bridge)
dev.off()

#Constructing a partial correlation matrix 
SAED4edges <-getWmat(SAEDnetwork4)
write.csv(SAED4edges, "SAED4edges.csv")

#---- Stability analyses (SAED4) ----
SAED4boot1<- bootnet(SAEDnetwork4, nBoots=1000,nCores=8)
SAED4boot2 <- bootnet(SAEDnetwork4, bBoots=1000,nCores=8, type="case")
save(SAED4boot1, file = "SAED4boot1.Rdata")
save(SAED4boot2, file = "SAED4boot2.Rdata")
##If closed R after saving bootnet files you can reload them below#
load("SAED4boot1.Rdata")
load("SAED4boot2.Rdata")
############################

### Plot edge weight CI
pdf("StabilitySAED4.pdf")
plot(SAED4boot1, labels = FALSE, order = "sample") 
dev.off()

### Edge weights diff test
pdf("SAED4wdifftest.pdf")
plot(SAED4boot1, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")
dev.off()

### Plot centrality stability
pdf("SAED4stability.pdf") 
plot(SAED4boot2)
dev.off()

### Centrality stability coefficient
corStability(SAED4boot2)

### Centrality diff test
pdf("SAED4cdifftest.pdf")
plot(SAED4boot1, "strength", order="sample", labels=TRUE) 
dev.off()












Figure 8. A network of eating disorder (ED), social anxiety disorder (SAD), social appearance anxiety (SAA), and perfectionism (FMPS) symptoms. N = 2215.






























Figure 9. Centrality plot of the eating disorder-social anxiety-perfectionism-social appearance anxiety disorder network (Network 4). The plot is standardized, where larger scores indicate greater centrality. Most central items are underlined and marked with either a light green (SAD symptom) or aqua (ED symptom) dot for clarity.
















































Figure 10. Bridge plots for network 1 (SAD-ED only). The plot is standardized, where larger scores indicate greater centrality




































Figure 11. Centrality stability plot for social anxiety-eating disorder symptom network (Network 1).


















































Figure 12. Edge stability plot for social anxiety-eating disorder symptom network (Network 1).


















































Figure 13. Centrality difference test plot for social anxiety-eating disorder symptom network (Network 1).



















































Figure 14. Bridge plots for social anxiety-eating disorder-social appearance anxiety symptom network (Network 2).





































Figure 15. Centrality stability plot for social anxiety-eating disorder-social appearance anxiety symptom network (Network 2).
















































Figure 16. Edge stability plot for social anxiety-eating disorder-social appearance anxiety symptom network (Network 2).
















































Figure 17. Centrality difference plot for social anxiety-eating disorder-social appearance anxiety symptom network (Network 2).


















































Figure 18. Bridge plot for social anxiety-eating disorder-perfectionism symptom network (Network 3).




































Figure 19. Centrality stability plot for social anxiety-eating disorder-perfectionism network (Network 3).


















































Figure 20. Edge stability plot for social anxiety-eating disorder-perfectionism symptom network (Network 3).
















































Figure 21. Centrality difference plot for social anxiety-eating disorder-perfectionism symptom network (Network 3).

















































Figure 22. Centrality stability plot for the combined network (Network 4).














Figure 23. Edge stability plot for the combined network (Network 4).
















Table 3. Goldbricker proportion matrix for network item selection. Proportions are reported for item pairs with less than 25% of significantly different correlations. One item from each pair was removed based on a theoretical approach (bolded).

Item Pair	Proportion
SPS_4 & SPS_6	0.114
FMPS_18 & FMPS_10	0.151
FMPS_34 & FMPS_14	0.207
SIAS_12 & SIAS_3	0.226
FMPS_34 & FMPS_13	0.226
FMPS_25 & FMPS_21	0.226
FMPS_25 & FMPS_23	0.245

Notes: SPS = Social Phobia Scale; FMPS = Frost Multidimensional Perfectionism Scale; SIAS = Social Interaction Anxiety Scale.




































Node-dropping Results – bootnet

SAED1boot3<- bootnet(SAEDnetwork1, nBoots=1000, type="node")
save(SAED1boot3, file = "SAED1boot3.Rdata")
plot(SAED1boot3)























SAED2boot3<- bootnet(SAEDnetwork2, nBoots=1000, type="node")
save(SAED2boot3, file = "SAED2boot3.Rdata")
plot(SAED2boot3)


























SAED3boot3<- bootnet(SAEDnetwork3, nBoots=1000, type="node")
save(SAED3boot3, file = "SAED3boot3.Rdata")
plot(SAED3boot3)






Table 4. Means and SDs of Self-report Assessments


Scale	Clinical (n = 508)	Non-clinical  (n = 1707)	Combined
M (SD)	M (SD)	M (SD)
ED	SAD	Total clinical	Community	Undergraduate	Total Non-clinical	All 
EDE-Q	3.42 (1.44)	2.37 (1.62)	3.05 (1.60)	1.57 (1.40)	1.61 (1.26)	1.61 (1.26)	1.97 (1.51)
EDI
Drive for thinness							
32.07 (7.89)	15.86 (4.49)	29.98 (8.89)	17.25 (8.26)	21.98 (8.76)	21.77 (8.79)	23.58 (9.42)
Body Dissatisfaction	43.22 (9.38)	34.63 (11.73)	41.39 (10.51)	30.13 (11.81)	30.47(10.10)	30.45 (10.17)	33.00 (10.23)
Binge
10.83 (6.15)	14.53 (2.71)	11.02 (5.99)	9.11 (4.34)	11.07 (4.47)	10.98 (4.48)	10.99 (4.82)
SIAS 
40.56 (18.93)	51.09 (12.83)	46.15 (16.80)	19.82 (15.70)	27.69 (14.89)	26.23 (15.34)	30.44 (17.88)
SPS
30.52 (19.37)	36.35 (16.53)	32.85 (18.49)	9.92 (11.54)	16.97 (12.92)	15.15 (12.95)	19.41 (16.33)
SAAS
59.16 (15.24)	50.05 (16.49)	57.17 (15.95)	31.62 (15.43)	38.00 (15.21)	37.02 (15.41)	40.92 (17.44)
FMPS
93.91 (27.00)	93.56 (22.22)	94.31 (24.64)	81.55 (21.05)	83.01 (18.77)	83.00 (18.77)	85.41 (21.44)
Maladaptive perfectionism	67.02 (22.8)	67.75 (18.96)	67.62 (20.87)	56.69 (17.47)	58.33 (15.76)	58.32 (15.76)
60.27 (17.95)



