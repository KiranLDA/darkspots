# library(plotly)
# library(umap)
# library(dplyr)
# library(devtools)
# install_github("vqv/ggbiplot")

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

test = tdwg3@data[,c("LEVEL3_NAM",
                     "COUNTRY","ISO_code" ,
                     "ISO_code_3",
                     "Region",
                     "SDG.Index.Score")]



test$cols = as.numeric(as.factor(tdwg3@data$Region))
legend.cols = as.numeric(as.factor(levels(as.factor(tdwg3@data$Region))))



plot(tdwg3@data[,"SDG.Index.Score"], tdwg3@data$discoveries, pch=19, col=test$cols)


plot(tdwg3@data$Goal.1.Score, tdwg3@data$discoveries, pch=19)
plot(tdwg3@data$Goal.1.Score, tdwg3@data$left_spp_wcvp, pch=19)
plot(tdwg3@data$SDG.Index.Score, tdwg3@data$left_spp_wcvp, pch=19)



plot(tdwg3@data$SDG.Index.Score, prop_spp, pch=19)
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
              "Goal.17.Score", "SDG.Index.Score")

for (x in 1:length(variable_list)){
  par(mfrow=c(3,3))
  for (y in 1:length(goal_list)){
    plot(tdwg3@data[,variable_list[x]],tdwg3@data[,goal_list[y]],
         xlab = variable_list[x],
         ylab = goal_list[y],
         pch=19, col=test$cols)
  }
  legend("topright", legend=levels(as.factor(tdwg3@data$Region)), pch=16, col=legend.cols)

}


par(mfrow=c(3,3))
for (y in 1:length(goal_list)){
  plot(prop_spp,tdwg3@data[,goal_list[y]],
       xlab = "proportion of described species",
       ylab = goal_list[y],
       pch=19, col=test$cols)
}
legend("topright", legend=levels(as.factor(tdwg3@data$Region)), pch=16, col=legend.cols)


rows= complete.cases(tdwg3@data[,goal_list])
sdg.pca <- prcomp(tdwg3@data[rows,goal_list], center = TRUE,scale. = TRUE)


summary(sdg.pca)
str(sdg.pca)
# ggbiplot(sdg.pca, groups = tdwg3@data$Region[rows])

subset =tdwg3@data[rows,]
rownames(subset) = tdwg3@data[rows,"LEVEL3_COD"]

p <- autoplot(sdg.pca, data = subset, colour = "prop_discovered",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
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

p
ggplotly(p)


#PCA
goal_list = c(#"Prop_6hrs", "Annual deforestation (% of change) - (for online table only)",
              #"Plant species (higher), threatened",
              "tot_exp_spp_wcvp",
              # "Goal.1.Score", "Goal.2.Score",
              # "Goal.3.Score", "Goal.4.Score",
              # "Goal.5.Score", "Goal.6.Score",
              # "Goal.7.Score", "Goal.8.Score",
              "Goal.9.Score",# "Goal.10.Score",
              # "Goal.11.Score",
              # "Goal.12.Score",
              # "Goal.13.Score", "Goal.14.Score",
              "Goal.15.Score")#,
              # "Goal.16.Score",
              # "Goal.17.Score")
rows= complete.cases(tdwg3@data[,goal_list])
sdg.pca <- prcomp(tdwg3@data[rows,goal_list], center = TRUE,scale. = TRUE)


summary(sdg.pca)
str(sdg.pca)
# ggbiplot(sdg.pca, groups = tdwg3@data$Region[rows])

subset =tdwg3@data[rows,]
rownames(subset) = tdwg3@data[rows,"LEVEL3_COD"]

autoplot(sdg.pca, data = subset, colour = "prop_discovered",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         # label = TRUE,#"Country",
         # label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.3,
         size=12,
         label.size = 3,
         lwd=4
         )+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() +
  scale_color_gradient2(midpoint=0.5, low="black", mid="purple",
                        high="yellow", space ="Lab" )



autoplot(sdg.pca, data = subset, colour = "prop_remaining",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         label = TRUE,#"Country",
         # shape= FALSE,
         alpha=0.5,
         size=13,
         label.size = 5)+
  theme_bw() +
  scale_color_gradient2(midpoint=0.5, low="black", mid="purple",
                        high="yellow", space ="Lab" )



subset$log_descriptions = log10(subset$descriptions)


autoplot(sdg.pca, data = subset, colour = "log_descriptions",#'colour = "descriptions",#Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.3,
         size=15,
         label.size = 3,
         lwd=4
)+
  theme_bw() +
  scale_color_gradient2(midpoint=-1.5, low="black", mid="purple",
                        high="yellow", space ="Lab" )

autoplot(sdg.pca, data = subset, colour = "discoveries",#'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.3,
         size=15,
         label.size = 3,
         lwd=4)+
  theme_bw() +
  scale_color_gradient2(midpoint= 0.02,low="black", mid="purple",
                        high="yellow", space ="Lab" )


#---------------------------------------------------------
library(cluster)
autoplot(clara(tdwg3@data[rows,goal_list], 4),
         data = subset, #colour = 'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         # loadings = TRUE,
         # loadings.label = TRUE,
         # colour = "magma",
         label = TRUE,#"Country",
         shape= FALSE,
         label.size = 3)

autoplot(fanny(tdwg3@data[rows,goal_list], 4), frame = TRUE,
         data = subset,
         label = TRUE,
         shape= FALSE,
         label.size = 3)
autoplot(pam(tdwg3@data[rows,goal_list], 4), frame = TRUE, frame.type = 'norm',
         data = subset,
         label = TRUE,
         shape= FALSE,
         label.size = 3)
