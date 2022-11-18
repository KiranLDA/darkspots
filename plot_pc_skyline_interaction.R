
library(ggbiplot)
library(ggfortify)

load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")

# save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData"))
# # write.csv(tdwg3@data, paste0(basepath, "variables_table.csv"))
#
tdwg3@data$spp_discovered =  (tdwg3@data$tot_exp_spp_wcvp-tdwg3@data$left_spp_wcvp)
tdwg3@data$prop_discovered =  tdwg3@data$spp_discovered/ tdwg3@data$tot_exp_spp_wcvp
tdwg3@data$prop_remaining =  tdwg3@data$left_spp_wcvp/ tdwg3@data$tot_exp_spp_wcvp



prop_spp = tdwg3@data$left_spp_wcvp / tdwg3@data$tot_exp_spp_wcvp



#prop_spp,
variable_list = c( "discoveries", "descriptions")

goal_list = c("Goal.1.Score", "Goal.2.Score",
              "Goal.3.Score", "Goal.4.Score",
              "Goal.5.Score", "Goal.6.Score",
              "Goal.7.Score", "Goal.8.Score",
              "Goal.9.Score", "Goal.10.Score",
              "Goal.11.Score", "Goal.12.Score",
              "Goal.13.Score", "Goal.14.Score",
              "Goal.15.Score", "Goal.16.Score",
              "Goal.17.Score")

rows= complete.cases(tdwg3@data[,goal_list])
sdg.pca <- prcomp(tdwg3@data[rows,goal_list], center = TRUE,scale. = TRUE)
extract.pca <- get_pca_var(sdg.pca)


plot(tdwg3@data$prop_remaining[rows] , sdg.pca$x[, 1],pch=19)
plot(tdwg3@data$discoveries[rows] , sdg.pca$x[, 1],pch=19)
plot(tdwg3@data$descriptions[rows] , sdg.pca$x[, 1],pch=19)


tdwg3@data$PC1 = NA
tdwg3@data$PC1[rows] = sdg.pca$x[, 1]
tdwg3@data$PC2 = NA
tdwg3@data$PC2[rows] = sdg.pca$x[, 2]


tdwg3_PCA = tdwg3
# save(tdwg3_PCA, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/tdwg3_PCA.RData"))



#----------------------------------------------------
plot_ly(data = tdwg3@data[rows,], x = ~prop_remaining, y = ~PC1,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)

plot_ly(data = tdwg3@data[rows,], x = ~prop_remaining, y = ~PC2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)


plot_ly(data = tdwg3@data[rows,], x = ~discoveries, y = ~PC1,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)

plot_ly(data = tdwg3@data[rows,], x = ~discoveries, y = ~PC2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)



plot_ly(data = tdwg3@data[rows,], x = ~descriptions, y = ~PC1,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)

plot_ly(data = tdwg3@data[rows,], x = ~descriptions, y = ~PC2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)





summary(sdg.pca)
str(sdg.pca)
# ggbiplot(sdg.pca, groups = tdwg3@data$Region[rows])

subset =tdwg3@data[rows,]
rownames(subset) = tdwg3@data[rows,"LEVEL3_COD"]

autoplot(sdg.pca, data = subset, colour = "prop_discovered",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         # label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.3,
         size=12,
         label.size = 4,
         lwd=4
)+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() +
  scale_color_gradient2(midpoint=0.5, low="black", mid="purple",
                        high="yellow", space ="Lab" )


#--------------------------------------------------------------------------------

goal_list = c( #"tot_exp_spp_wcvp",
               "Prop_6hrs",
               "Goal.1.Score",
               "Goal.9.Score",
               "Goal.15.Score")

rows= complete.cases(tdwg3@data[,goal_list])

umap.data = tdwg3@data[rows,goal_list]
umap.labels = tdwg3@data[rows,"LEVEL3_COD"]
umap.umap = umap(umap.data, n_components = 2)
layout <- umap.umap[["layout"]]
layout <- data.frame(layout)
final <- cbind(layout, umap.labels)

tdwg3@data$UMAP1 = NA
tdwg3@data$UMAP1[rows] = final$X1
tdwg3@data$UMAP2 = NA
tdwg3@data$UMAP2[rows] = final$X2

plot_ly(data = tdwg3@data[rows,], x = ~UMAP1, y = ~UMAP2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~spp_left, size = ~spp_left)

plot_ly(data = tdwg3@data[rows,], x = ~UMAP1, y = ~UMAP2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = as.numeric(as.factor(tdwg3@data[rows,"Income.Group"])),
        size = ~spp_left)






plot_ly(data = tdwg3@data[rows,], x = ~prop_remaining, y = ~Goal.15.Score,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)

plot_ly(data = tdwg3@data[rows,], x = ~Goal.9.Score, y = ~Goal.15.Score,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)

plot_ly(data = tdwg3@data[rows,], x = ~UMAP1, y = ~UMAP2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~tot_exp_spp_wcvp, size = ~tot_exp_spp_wcvp)


plot_ly(data = tdwg3@data[rows,], x = ~UMAP1, y = ~UMAP2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~Income.Group, size = ~discoveries)


        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~Income.Group, size = ~tot_exp_spp_wcvp)



plot_ly(data = tdwg3@data[rows,], x = ~prop_remaining, y = ~UMAP2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~Income.Group, size = ~tot_exp_spp_wcvp)

#------------------------------
# SIMPLER PCA
#-------------------------------



sdg.pca <- prcomp(tdwg3@data[rows,goal_list], center = TRUE,scale. = TRUE)
# extract.pca <- get_pca_var(sdg.pca)

tdwg3@data$PC1 = NA
tdwg3@data$PC1[rows] = sdg.pca$x[, 1]
tdwg3@data$PC2 = NA
tdwg3@data$PC2[rows] = sdg.pca$x[, 2]

plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~spp_left, size = ~spp_left)


subset =tdwg3@data[rows,]
rownames(subset) = tdwg3@data[rows,"LEVEL3_COD"]

autoplot(sdg.pca, data = subset, colour = "spp_left",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         # label = TRUE,#"Country",
         # label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.7,
         size=3,
         label.size = 3,
         lwd=4)+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() +
  scale_color_gradient2(midpoint=1000, low="black", mid="purple",
                        high="yellow", space ="Lab" )

autoplot(sdg.pca, data = subset, colour = "discoveries",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         # label = TRUE,#"Country",
         # label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.7,
         size=3,
         label.size = 3,
         lwd=4)+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() +
  scale_color_gradient2(midpoint=0.02, low="black", mid="purple",
                        high="yellow", space ="Lab" )
