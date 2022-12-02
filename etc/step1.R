library(tidyverse)
library(qgraph)          #visualize networks
library(bootnet)         # runs all basic network analyses
library(NetworkComparisonTest)  #compare networks
library(networktools)         #bridge symptoms, goldbricker for node select
library(networktree)

load("dat.RData")

############### data cleaning ###############

mhm <-data.frame()[1:nrow(dat), ]  # create new DF with x rows and 0 cols

mhm <- add_column( mhm,

                    sex = dat$Biological.Sex,
                    adapt = dat$Adaptability.to.Change,
                    selfworth =  dat$Self.Worth...Confidence,
                    creativity =  dat$Creativity...Problem.Solving,
                    drive =   dat$Drive...Motivation.1,
                    stable =  dat$Stability...Calmness,
                    sleepQ = dat$Sleep.Quality ,
                    self.ctrl = dat$Self.Control...Impulsivity,
                    learn =  dat$Ability.to.Learn,
                    motor =  dat$Coordination,
                    relations =   dat$Relationships.with.others,
                    resilience =    dat$Emotional.Resilience,
                    plan =  dat$Planning...Organisation,
                    sex.satis =    dat$Physical.Intimacy ,
                    speech =    dat$Speech...Language ,
                    memory =     dat$Memory,
                    cooperate  =  dat$Social.interactions...Cooperation,
                    dec.make =   dat$Decision.making...Risk.taking,
                    curious =    dat$Curiosity..Interest...Enthusiasm,
                    energy =    dat$Energy.Level,
                    emot.cntrl =    dat$Emotional.Control,
                    focus =   dat$Focus...Concentration,
                    appetite =   dat$Appetite.Regulation,
                    empathy =  dat$Empathy,
                    sensory =   dat$Sensory.Sensitivity,
                    self.image =   dat$Self.Image,
                    optimist = dat$Outlook...Optimism,
                    select.attn =  dat$Selective.Attention,
                    restless  =   dat$Restlessness...Hyperactivity,
                    fear.anx =  dat$Fear...Anxiety,
                    infectns =   dat$Susceptibility.to.Infections,
                    aggressn =  dat$Aggression.Towards.Others,
                    avoid = dat$Avoidance...Withdrawal,
                    unwantd.thots =  dat$Unwanted..Strange.or.Obsessive.Thoughts,
                    Mood.Swings = dat$Mood.Swings,
                    detach.real =     dat$Sense.of.being.detached.from.reality,
                    nightmare =   dat$Nightmares,
                    addict = dat$Addictions,
                    anger =   dat$Anger...Irritability,
                    SI =   dat$Suicidal.Thoughts.or.Intentions,
                    pain = dat$Experience.of.Pain,
                    guilt =   dat$Guilt...Blame ,
                    halluc  = dat$Hallucinations ,
                    trauma.FB = dat$Traumatic.Flashbacks ,
                    compulsv.actns =    dat$Repetitive.or.Compulsive.Actions ,
                    sadness =  dat$Feelings.of.Sadness..Distress.or.Hopelessness ,
                    unhealthy =    dat$Physical.Health.Issues ,
                    brainfog =    dat$Confusion.or.Slowed.Thinking )



mhm <- mhm %>%
  subset(country == "United States",
         country == "United Kingdom",
         country == "France",
         country == "Australia",
         country == "Canada",
         country == "Germany",
         country == "New Zealand",
         country == "Ireland")

###########
mhm_m <- mhm %>%
  subset(sex == "Male"  )

mhm_m <- select(mhm_m, -sex)
summary(mhm)


mhm_f <- mhm %>%
  subset(sex == "Female"  )

mhm_f <- select(mhm_f, -sex)
summary(mhm)
###########


#dropped 7745 (2.3%) rows 
#(2 arabic, "prefer not say" for sex & mhseek, and 819 "intersex/other")
# 


# COMPARE ITEM LEVEL MEANS AND VARIANCES



# ####################
# ####################   GOLDBRICKER (IDENTIFY REDUNDANT ITEMS)
# ####################
# 
# gb <- goldbricker(mhm, p = 0.05, method = "hittner2003", 
#             threshold = 0.25, corMin = 0.5, progressbar = T)
# 
# gb
# # Suggested reductions: Less than 25 % of correlations are significantly different for the following pairs: 
# #   [1] "No suggested reductions"





#########################
######################### SPLIT SAMPLE, ESTIMATE NETWORK 
#########################




###########################
n = 47

names1 <- c("adapt",       "slfwrth",      "create",   "drive",      "stable",        
             "sleepQ",     "slfctrl",      "learn",    "motor",      "relate",     
             "resil",      "plan",         "sexsat",   "spch",       "mem",        
             "cprt",       "decsn",        "curious",  "energy",     "emot",    
             "focus",      "apptt",        "empath",   "sense",      "slfimg",    
             "optim",      "attn",         "rstlss",   "anx",        "infctn",      
             "aggres",     "avoid",        "unthot",  "mdswng",      "detach",   
             "nghtmr",    "addict",       "anger",    "SI",         "pain",          
             "guilt",      "hllc",         "FB",       "cmplsv",     "sad",       
             "unhlth",     "brnfog")

nameslong <- c("adaptability to change", "self worth & confidence", "creativity", 
               "drive and motivation", "stability and calmness", "sleep quality",
               "self control & impulsivity", "ability to learn", " coordination",
               "relationships with others", "emotional resilience", "planning & organization",
               "physical intimacy", "speech & language", "memory", "social interactions & cooperation",
               "decision making & risk taking", "curiosity, interest, & enthusiasm", 
               "energy level", "emotional control", "focus & concentration", "appetite regulation", "empathy",
               "sensory sensitivity", "self-image", "outlook & optimism", "selective attention",
               "restlessness & hyperactivity", "fear & anxiety", "susceptibility to infections",
               "aggression towards others", "avoidance & withdrawl", "unwanted, strange, or obsessive thoughts",
               "mood swings", "sense of being detached from reality", "nightmares",
               "addictions", "anger & irritability", "suicidal thoughts or intentions",
               "experience of pain", "guilt & blame", "hallucinations", "traumatic flashbacks",
               "repeptitive or compulsive actions", "feelings of sadness, distress, hopelessness",
               "physical health issues", "confusion or slowed thinking"
                 )




# Compute polychoric correlations:
cormat_m <- cor_auto(mhm_m)
# Optimize network:
net_m <- ggmModSelect(cormat_m, nrow(mhm_m),
                      verbose = T,
                         stepwise = T, gamma = 0.5, nCores = 4)


save(net_m, file = "net_m.RData")

# # Plot results:
qgraph_m <- qgraph(net_m$graph, layout = "spring", cut = 0, vsize = 3,
       labels = names1,
       nodeNames = nameslong,
       label.cex = 1,
       label.scale = F,
       label.scale.equal = F,
       label.prop = 1,
       minimum = .05,
       title = "male network structure")






############ females

cormat_f <- cor_auto(mhm_f)

net_f <- ggmModSelect(cormat_f, nrow(mhm_f),
                      verbose = T,
                      stepwise = T, gamma = 0.5, nCores = 4)

save(net_f, file = "net_f.RData")


qgraph_f<- qgraph(net_f$graph, layout = "spring", cut = 0, vsize = 3,
       labels = names1,
       nodeNames = nameslong,
       label.cex = 1,
       label.scale = F,
       label.scale.equal = F,
       label.prop = 1,
       minimum = .05,
       title = "female network structure")




ef_m <- centralityPlot(qgraph_m, include = "ExpectedInfluence")

ef_f <- centralityPlot(qgraph_f, include = "ExpectedInfluence")





##########   network comparison test

nct <- NCT(net_m, net_f, it = 500, weighted = T,
           test.edges = T,
           edges = "ALL",
           test.centrality = T,
           centrality = c("expectedInfluence"))



summary(nct)

## Plotting of NCT results
## See the help file of plot.NCT for more information about the plotting function and its arguments
# Plot results of the network structure invariance test (not reliable with only 10 permutations!):
plot(nct, what="network")
# Plot results of global strength invariance test (not reliable with only 10 permutations!):
plot(nct, what="expectedInfluence")
# Plot results of the edge invariance test (not reliable with only 10 permutations!):
# Note that two distributions are plotted
plot(nct, what="edge")
# Without testing for (an) individual edge(s)
# The arguments 'test.edges' and 'edges' don't need to be specified
# Not run
# Res_0 <- NCT(data1, data2, gamma=0, it=10, binary.data = TRUE)


library(EGAnet)


ega_low <- EGA(mhm_low)

ega_high <- EGA(mhm_high)

# 
# net_low <- estimateNetwork(mhm_low, default = "ggmModSelect",  
#                                 stepwise = F)
# 
# save(net_low, file = "net_low.RData")
# 
# 
# netplot_low  <-plot(net_low, labels=names1, layout="spring", vsize=6,
#                  cut=0, border.width=1.5, border.color="black",
#                  nodeNames = nameslong, legend.cex=.8,
#                  label.scale.equal = F,
#                  label.prop = 1,
#                  vsize =  2*exp(-47/80)+1   )






