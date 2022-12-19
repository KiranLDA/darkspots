#PC1 Lower is richer
#PC2 lower has more protection
# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
library(plotly)
library(tidyverse)
# data =  tdwg3_PCA@data
data =  tdwg3@data


normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

# 1) Aucun "cout"
# 2) Pauvrete
# 3) Richesse
# 4) Pas de protection
# 5) Protection
# 6) Pauvrete et pas de protection
# 7) Pauvrete et protection
# 8) Richesse et pas de protection
# 9) Richesse et protection


# Prioritise areas with the most species left to discover
data$benefit_1 = data$shortfalls_norm_index#(1-normalise(data$SR_shortfalls))
data$benefit_1 = normalise(data$benefit_1)
data$Rank_scenatio_1 <- rank(-data$benefit_1, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places to discover
data$benefit_2 = data$shortfalls_norm_index + normalise(data$PC1)#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_2 = normalise(data$benefit_2)
data$Rank_scenatio_2 <- rank(-data$benefit_2, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places to discover
data$benefit_3 =data$shortfalls_norm_index + (1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
data$benefit_3 = normalise(data$benefit_3)
data$Rank_scenatio_3 <- rank(-data$benefit_3, na.last = "keep", ties.method = "first")

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
data$benefit_4 = (data$shortfalls_norm_index) + normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$benefit_4 = normalise(data$benefit_4)
data$Rank_scenatio_4 <- rank(-data$benefit_4, na.last = "keep", ties.method = "first")


#Prioritise areas with most species and smallest potential for biodiversity loss PC2
data$benefit_5 =  (data$shortfalls_norm_index) + (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$benefit_5 = normalise(data$benefit_5)
data$Rank_scenatio_5 <- rank(-data$benefit_5, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
data$benefit_6 = data$shortfalls_norm_index + normalise(normalise(data$PC1)+  (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_6 = normalise(data$benefit_6)
data$Rank_scenatio_6 <- rank(-data$benefit_6, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places to discover with protection
data$benefit_7 = data$shortfalls_norm_index + normalise(normalise(data$PC1)+  (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_7 = normalise(data$benefit_7)
data$Rank_scenatio_7 <- rank(-data$benefit_7, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
data$benefit_8 = data$shortfalls_norm_index + normalise((1-normalise(data$PC1))+  (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_8 = normalise(data$benefit_8)
data$Rank_scenatio_8 <- rank(-data$benefit_8, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places to discover with protection
data$benefit_9 = data$shortfalls_norm_index + normalise((1-normalise(data$PC1)) +  (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_9 = normalise(data$benefit_9)
data$Rank_scenatio_9 <- rank(-data$benefit_9, na.last = "keep", ties.method = "first")



data$rank_diff_1_2 = data$Rank_scenatio_1 - data$Rank_scenatio_2 #-
data$rank_diff_1_3 = data$Rank_scenatio_1 - data$Rank_scenatio_3 #- data$Rank_scenatio_1
data$rank_diff_1_4 = data$Rank_scenatio_1 - data$Rank_scenatio_4 #- data$Rank_scenatio_1
data$rank_diff_1_5 = data$Rank_scenatio_1 - data$Rank_scenatio_5 #- data$Rank_scenatio_1
data$rank_diff_1_6 = data$Rank_scenatio_1 - data$Rank_scenatio_6 #- data$Rank_scenatio_1
data$rank_diff_1_7 = data$Rank_scenatio_1 - data$Rank_scenatio_7 #- data$Rank_scenatio_1
data$rank_diff_1_8 = data$Rank_scenatio_1 - data$Rank_scenatio_8 #- data$Rank_scenatio_1
data$rank_diff_1_9 = data$Rank_scenatio_1 - data$Rank_scenatio_9 #- data$Rank_scenatio_1



data$PC1_normalised = normalise(data$PC1)
data$PC2_normalised = normalise(data$PC2)
data$PC3_normalised = normalise(data$PC3)


#PLOT THE LOT
#
# t <- list(
#   family = "sans serif",
#   size = 12,
#   color = toRGB("grey50"))
#
# plot_ly(data = data, x = ~Rank_scenatio_2, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_2, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise lower income group regions') %>%
#   add_text(textfont = t, textposition = "top")
#
#
# plot_ly(data = data, x = ~Rank_scenatio_3, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_3, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise higher income group  regions') %>%
#   add_text(textfont = t, textposition = "top")
#
#
# plot_ly(data = data, x = ~Rank_scenatio_4, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_4, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise less protected regions') %>%
#   add_text(textfont = t, textposition = "top")
#
#
# plot_ly(data = data, x = ~Rank_scenatio_5, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_5, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise lower income regions with least protection') %>%
#   add_text(textfont = t, textposition = "top")


s2 <- ggplot(data = data, aes(x = Rank_scenatio_2, y = Rank_scenatio_1,
        label = LEVEL3_COD,#LEVEL3_NAM,
        color = rank_diff_1_2), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 2", y ="Rank scenario 1")+
  theme_bw()

s2
ggsave(paste0(basepath, "Rank_1_2.pdf"), width = 20, height = 16, units = "cm")




s3 <- ggplot(data = data, aes(x = Rank_scenatio_3, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_3), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 3", y ="Rank scenario 1")+
  theme_bw()

s3
ggsave(paste0(basepath, "Rank_1_3.pdf"), width = 20, height = 16, units = "cm")




s4 <- ggplot(data = data, aes(x = Rank_scenatio_4, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_4), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 4", y ="Rank scenario 1")+
  theme_bw()

s4
ggsave(paste0(basepath, "Rank_1_4.pdf"), width = 20, height = 16, units = "cm")



s5 <- ggplot(data = data, aes(x = Rank_scenatio_5, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_5), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 5", y ="Rank scenario 1")+
  theme_bw()

s5
ggsave(paste0(basepath, "Rank_1_5.pdf"), width = 20, height = 16, units = "cm")





s6 <- ggplot(data = data, aes(x = Rank_scenatio_6, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_6), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 6", y ="Rank scenario 1")+
  theme_bw()

s6
ggsave(paste0(basepath, "Rank_1_6.pdf"), width = 20, height = 16, units = "cm")





s7 <- ggplot(data = data, aes(x = Rank_scenatio_7, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_7), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 7", y ="Rank scenario 1")+
  theme_bw()

s7
ggsave(paste0(basepath, "Rank_1_7.pdf"), width = 20, height = 16, units = "cm")




s8 <- ggplot(data = data, aes(x = Rank_scenatio_8, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_8), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 8", y ="Rank scenario 1")+
  theme_bw()

s8
ggsave(paste0(basepath, "Rank_1_8.pdf"), width = 20, height = 16, units = "cm")






s9 <- ggplot(data = data, aes(x = Rank_scenatio_9, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_9), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 9", y ="Rank scenario 1")+
  theme_bw()

s9
ggsave(paste0(basepath, "Rank_1_9.pdf"), width = 20, height = 16, units = "cm")

##############################################################
# Find countries that are consistently top of prioritisation
#sun the ranks
data$Rank_sum = (data$Rank_scenatio_1+
                    data$Rank_scenatio_2+
                    data$Rank_scenatio_3+
                    data$Rank_scenatio_4+
                    data$Rank_scenatio_5+
                    data$Rank_scenatio_6+
                    data$Rank_scenatio_7+
                    data$Rank_scenatio_8+
                    data$Rank_scenatio_9)

# rank the ranks
data$Rank_sum_ranked = rank(data$Rank_sum, na.last = "keep", ties.method = "first")
data$Rank_sum_ranked


#sun the absolute rank differences
data$Rank_diff_sum = (abs(data$rank_diff_1_2)+
                    abs(data$rank_diff_1_3)+
                    abs(data$rank_diff_1_4)+
                    abs(data$rank_diff_1_5)+
                    abs(data$rank_diff_1_6)+
                    abs(data$rank_diff_1_7)+
                    abs(data$rank_diff_1_8)+
                    abs(data$rank_diff_1_9))

# rank the ranks
data$Rank_diff_sum_ranked = rank(data$Rank_diff_sum, na.last = "keep", ties.method = "first")
data$Rank_diff_sum_ranked


# #PLOT THE LOT
# plot_ly(data = data, x = ~Rank_scenatio_3, y = ~Rank_scenatio_1,
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~rank_diff_1_3, size = 3) %>%
#   layout(title = 'Prioritise Poor regions')
#
#
# #PLOT THE LOT
# plot_ly(data = data, x = ~Rank_scenatio_4, y = ~Rank_scenatio_1,
#        text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~rank_diff_1_4, size = 3) %>%
#   layout(title = 'Prioritise regions with less protection')

######

#Save PCA and Ranks

tdwg3@data = data

save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData"))
save(tdwg3, file = paste0(basepath, "app_data.RData"))
write.csv(tdwg3@data, paste0(basepath, "variables_table.csv"))

# rgdal::writeOGR(obj=tdwg3,
#                 dsn="C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp",
#                 layer="darkspots",
#                 driver="ESRI Shapefile")


library(raster)
shapefile(tdwg3, filename='C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp', overwrite=TRUE)
shapefile(tdwg3, filename=paste0(basepath, "/model_outputs.shp"), overwrite=TRUE)

