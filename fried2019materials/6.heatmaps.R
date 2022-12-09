##################################################################################
#                                                                                #
#           Using network analysis to examine links between individual           #
#            depressive symptoms, inflammatory markers, and covariates           #   
#                                                                                #
#                            Fried et al., 2019                                  #
#                          Psychological Medicine                                #
#                           https://osf.io/6ehrm/                                #
#                                                                                #
#               6. Syntax file for creating network heatmaps                     #
#                                                                                #
#                         Last updated: 08-09-2019                               #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- Loading packages & Data ---------------------------------
# -------------------------------------------------------------------------

library("ggcorrplot")



# -------------------------------------------------------------------------
# --------------- Global Settings -----------------------------------------
# -------------------------------------------------------------------------

figDir2 <- "~/Dropbox/Research/My Projects/2017 - Fried von Stockert. Inflammation/Analysis/figures/"



# -------------------------------------------------------------------------
# --------------- Plot network edges in heatmaps --------------------------
# -------------------------------------------------------------------------

colnames(n1a$graph)<- rownames(n1a$graph) <- Labels1a
colnames(n1b$graph)<- rownames(n1b$graph) <- Labels1b
colnames(n2a$graph)<- rownames(n2a$graph) <- RepLabels2a
colnames(n2b$graph)<- rownames(n2b$graph) <- RepLabels2b
colnames(n3a$graph)<- rownames(n3a$graph) <- Labels3a
colnames(n3b$graph)<- rownames(n3b$graph) <- Labels3b
colnames(n4a$graph)<- rownames(n4a$graph) <- Labels3b

pdf(paste0(figDir2, "heatmap_1a.pdf"))
ggcorrplot(n1a$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=2, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 1a")
dev.off()

pdf(paste0(figDir2, "heatmap_1b.pdf"))
ggcorrplot(n1b$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=2, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 1b")
dev.off()

pdf(paste0(figDir2, "heatmap_2a.pdf"))
ggcorrplot(n2a$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=2, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 2a")
dev.off()

pdf(paste0(figDir2, "heatmap_2b.pdf"), width=7, height=7)
ggcorrplot(n2b$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=2, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 2b")
dev.off()

pdf(paste0(figDir2, "heatmap_3a.pdf"), width=7, height=7)
ggcorrplot(n3a$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=1.4, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 3a")
dev.off()

pdf(paste0(figDir2, "heatmap_3b.pdf"), width=7, height=7)
ggcorrplot(n3b$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=1.3, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 3b")
dev.off()

pdf(paste0(figDir2, "heatmap_4a.pdf"), width=7, height=7)
ggcorrplot(n4a$graph, method="circle", legend = "Edge Weights", 
           lab=TRUE, lab_size=1.3, type = "lower", color= c("red", "white", "blue"),
           show.diag = TRUE, tl.srt = 70, title = "Network 4")
dev.off()


#             |￣￣￣￣￣￣￣￣￣￣￣￣￣￣￣|
#                    This is the end of  
#                     part 6: heatmaps.
#                 After 1700 lines of code
#                 we're finally done here :)
#             |＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿| 
#                    (\__/)  ||
#                    (•ㅅ•)  ||
#                   /  　  づ



