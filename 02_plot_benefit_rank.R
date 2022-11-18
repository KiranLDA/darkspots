#PC1 Lower is richer
#PC2 lower has more protection
# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
library(plotly)
library(tidyverse)
# data =  tdwg3_PCA@data
data =  tdwg3@data


normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}


# Prioritise areas with the most species left to discover
#normalise(table$`SR_shortfalls`)
data$benefit_1 = normalise(data$SR_shortfalls)#(1-normalise(data$SR_shortfalls))
# data$benefit_1[is.na(data$benefit_1)]=0
data$Rank_scenatio_1 <- rank(-data$benefit_1, na.last = "keep", ties.method = "first")


# prioritise areas with the most species in poor places to discover
data$benefit_2 = normalise(data$SR_shortfalls) + normalise(data$PC1)#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
# data$benefit_2[is.na(data$benefit_2)]=0
data$Rank_scenatio_2 <- rank(-data$benefit_2, na.last = "keep", ties.method = "first")


# prioritise areas with the most species in rich places to discover
data$benefit_3 = normalise(data$SR_shortfalls) + (1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
# data$benefit_3[is.na(data$benefit_3)]=0
data$Rank_scenatio_3 <- rank(-data$benefit_3, na.last = "keep", ties.method = "first")

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
data$benefit_4 = normalise(data$PC2) + (normalise(data$SR_shortfalls))#(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
# data$benefit_4[is.na(data$benefit_4)]=0
data$Rank_scenatio_4 <- rank(-data$benefit_4, na.last = "keep", ties.method = "first")

#Prioritise areas with most species, poor, and biggest potential for biodiversity loss PC2
data$benefit_5 = (normalise(data$SR_shortfalls)) + (normalise(data$PC2)) +  ((normalise(data$PC1)))#(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
# data$benefit_5[is.na(data$benefit_5)]=0
data$Rank_scenatio_5 <- rank(-data$benefit_5, na.last = "keep", ties.method = "first")

data$rank_diff_1_2 = data$Rank_scenatio_1 - data$Rank_scenatio_2 #-
data$rank_diff_1_3 = data$Rank_scenatio_1 - data$Rank_scenatio_3 #- data$Rank_scenatio_1
data$rank_diff_1_4 = data$Rank_scenatio_1 - data$Rank_scenatio_4 #- data$Rank_scenatio_1
data$rank_diff_1_5 = data$Rank_scenatio_1 - data$Rank_scenatio_5 #- data$Rank_scenatio_1

data$PC1_normalised = normalise(data$PC1)
data$PC2_normalised = normalise(data$PC2)
data$PC3_normalised = normalise(data$PC3)


#PLOT THE LOT

t <- list(
  family = "sans serif",
  size = 12,
  color = toRGB("grey50"))

plot_ly(data = data, x = ~Rank_scenatio_2, y = ~Rank_scenatio_1,
        text = ~LEVEL3_NAM,
        color = ~rank_diff_1_2, size = 3) %>%
  add_markers() %>%
  layout(title = 'Prioritise lower income group regions') %>%
  add_text(textfont = t, textposition = "top")


plot_ly(data = data, x = ~Rank_scenatio_3, y = ~Rank_scenatio_1,
        text = ~LEVEL3_NAM,
        color = ~rank_diff_1_3, size = 3) %>%
  add_markers() %>%
  layout(title = 'Prioritise higher income group  regions') %>%
  add_text(textfont = t, textposition = "top")


plot_ly(data = data, x = ~Rank_scenatio_4, y = ~Rank_scenatio_1,
        text = ~LEVEL3_NAM,
        color = ~rank_diff_1_4, size = 3) %>%
  add_markers() %>%
  layout(title = 'Prioritise less protected regions') %>%
  add_text(textfont = t, textposition = "top")


plot_ly(data = data, x = ~Rank_scenatio_5, y = ~Rank_scenatio_1,
        text = ~LEVEL3_NAM,
        color = ~rank_diff_1_5, size = 3) %>%
  add_markers() %>%
  layout(title = 'Prioritise lower income regions with least protection') %>%
  add_text(textfont = t, textposition = "top")




tdwg3@data = data


# tdwg3_PCA.df <- fortify(tdwg3_PCA, region = "DMA")
# tdwg3_PCA.df <- rename(tdwg3_PCA.df, DMA = id)
#
# library(ggplot2)
# ggplot(tdwg3_PCA, aes( x = long, y = lat, group =  rank_diff_1_2)) +
#   geom_polygon(data = tdwg3_PCA, aes( x = long, y = lat, group =  rank_diff_1_2), color="white") +
#   theme_void()



#
# library(broom)
# NHSBoards_tidy <- tidy(tdwg3_PCA)
#
# ggplot(NHSBoards_tidy, aes(x = long, y = lat, col =  Rank_scenatio_3, fill= Rank_scenatio_3)) +
#   geom_polygon(color = "black", size = 0.1) +
#   coord_equal() +
#   theme_minimal()
#
#
# ggplot(NHSBoards_tidy, aes(x = long, y = lat, group =  group)) +
#   geom_polygon(color = "black", size = 0.1) +
#   coord_equal() +
#   theme_minimal()


#
#
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
save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData"))
write.csv(tdwg3@data, paste0(basepath, "variables_table.csv"))

# rgdal::writeOGR(obj=tdwg3,
#                 dsn="C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp",
#                 layer="darkspots",
#                 driver="ESRI Shapefile")


library(raster)
shapefile(tdwg3, filename='C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp', overwrite=TRUE)
