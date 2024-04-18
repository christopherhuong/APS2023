
library(rio)
library(dplyr)
library(mgm)
library(qgraph)

dpq <- import("nhanes_m_f/attempt/P_DPQ.XPT")
paq <- import("nhanes_m_f/attempt/P_PAQ.XPT")

d <- inner_join(dpq, paq, by="SEQN")

rm(dpq, paq)



d <- d %>%
  select(PAQ650, PAQ665, PAD680,
         # PAD615, PAD630, PAD645, PAD660, PAD675,
         DPQ010, DPQ020, DPQ030, DPQ040,  DPQ050,
         DPQ060, DPQ070, DPQ080, DPQ090
  )


names(d) <- c("VIG_LEIS", "MOD_LEIS", "SED",
              # "VIG_WORK", "MOD_WORK", "WALK", "VIG_LEIS", "MOD_LEIS",
               "PHQ1", "PHQ2", "PHQ3", "PHQ4", "PHQ5",
               "PHQ6", "PHQ7", "PHQ8", "PHQ9"
              )

# recode NAs
for(i in 1:2){
  d <- d %>% mutate(!!colnames(d)[i] := case_when(.data[[colnames(d)[i]]] == 1 ~ 1,
                                                  .data[[colnames(d)[i]]] == 2 ~ 2,
                                                  .data[[colnames(d)[i]]] == 9 ~ NA_real_))
}


for(i in 4:12){
  d <- d %>% mutate(!!colnames(d)[i] := case_when(.data[[colnames(d)[i]]] == 0 ~ 0, # not at all
                                                  .data[[colnames(d)[i]]] == 1 ~ 1, # several days
                                                  .data[[colnames(d)[i]]] == 2 ~ 2, # more than half of days,
                                                  .data[[colnames(d)[i]]] == 3 ~ 3, # nearly every day,
                                                  .data[[colnames(d)[i]]] == 7 ~ NA_real_, # refused
                                                  .data[[colnames(d)[i]]] == 9 ~ NA_real_ # don't know
                                                  ))
}



# Replace values with NA
d[which(d$SED==7777), "SED"] <- NA
d[which(d$SED==9999), "SED"] <- NA

d$SED <- ifelse(d$SED < median(d$SED, na.rm=T), 1, 2)

mgm_d <- list(
  data = na.omit(d[, -c(1,2)]),
  type = c("c", rep("g", 9)),
  level = c(2, rep(1, 9))
)


mnm <- mgm(data=mgm_d$data,
           type=mgm_d$type,
           level = mgm_d$level,
           moderators=1,
           lambdaSel = "EBIC",
           lambdaGam = 0.25,
           ruleReg = "AND")

qgraph(mnm$pairwise$wadj,
       edge.color=mnm$pairwise$edgecolor)

l_mgm_cond <- list()
for(g in 1:2) l_mgm_cond[[g]] <- condition(mnm, values=list("10" = g))


v_max <- rep(NA, 2)
for(g in 1:2) v_max[g] <- max(l_mgm_cond[[g]]$pairwise$wadj)





pdf("nhanes_m_f/attempt/mgm.pdf", width = 9, height = 3)
par(mfrow=c(1,2))
for(g in 1:2) {
  qgraph(input = l_mgm_cond[[g]]$pairwise$wadj, 
         edge.color = l_mgm_cond[[g]]$pairwise$edgecolor, 
         layout = "circle", mar=c(2,3,5,3),
         maximum = max(v_max), vsize=16, esize=23, 
         edge.labels  = TRUE, edge.label.cex = 3)
  mtext(text = paste0("Group ", g), line=2.5)
}
dev.off()



d1 <- na.omit(d[d$PA==1, -c(1,2,12)])
d2 <- na.omit(d[d$PA==2, -c(1,2,12)])

nct1 <- NCT(d1, d2,
            it=500,
            test.edges = T,
            test.centrality = T)

nct1
summary(nct1)



# create leisure PA variable
# PA = 1 if yes to vigorous and/or moderate recreational activities in typical week
# PA == 2 if not
d$PA <- ifelse(d$VIG_LEIS == 1 | d$MOD_LEIS == 1, 1, 2)
d_lm <- d
d_lm$PHQ_tot <- rowSums(d_lm[, 4:12])

t.test(d_lm$PHQ_tot[d_lm$PA==1], d_lm$PHQ_tot[d_lm$PA==2])















