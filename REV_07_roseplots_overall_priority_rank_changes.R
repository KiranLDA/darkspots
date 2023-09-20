#
# #######################################################################
# basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/"
#
# load(paste0(basepath, "REV_app_data.RData"))
#
# library(dplyr)
# # library(plyr)
# # detach("package:plyr", unload=TRUE)
#
# #######################################################
# # ROSEPLOT smallest rank change
# #######################################################
# #load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "Rank_diff_sum")] %>%
#   arrange(LEVEL1_COD, Rank_diff_sum)
# colnames(data) = c("individual","group", "value")
# data$value = max(data$value,na.rm=T)-data$value
#
# tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
#                          LEVEL1_NAM = c("Europe","Africa", "Asia-\nTemperate","Asia-Tropical","Australasia", "Pacific", "North     \nAmerica", "South    \nAmerica", "Antarctic"))
#
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar=1
# to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
# colnames(to_add) = colnames(data)
# to_add$group=rep(levels(data$group), each=empty_bar)
# data=rbind(data, to_add)
#
# data = data %>% dplyr::arrange(group) %>%
#   dplyr::group_by(group) %>%
#   dplyr::mutate(rank = rank(-value))
# data = data %>% dplyr::filter(rank <= RANK)
#
# data$id=seq(1, nrow(data))
# data$label = NA
# for (gp in 1:9){
#   data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
# }
# #remmove antarctic
# data= data[data$group<9,]
#
# # Get the name and the y position of each label
# label_data=data
# number_of_bar=nrow(label_data)
# angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust<-ifelse( angle < -90, 1, 0)
# label_data$angle<-ifelse(angle < -90, angle+180, angle)
#
#
#
# #####################################
# # Windplot
# #####################################
# p = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Greys")) +
#   ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(),#element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()
#
#   ) +
#   geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
#                               max(data$value,na.rm=T)/5), color = "gray90") +
#   geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
#   coord_polar() +
#   geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
#                               y1 = rep(max(data$value,na.rm=T)*3.5, 8),
#                               label = tdwg1_names$LEVEL1_NAM[1:8]),
#             aes(x=x1, y=y1, label = label),
#             color="black", fontface="bold",alpha=0.9, size=4,
#             inherit.aes = FALSE
#   ) +
#
#   geom_text(data=label_data, aes(x=id, y=value+0.001,
#                                  label=individual, hjust=hjust),
#             color="black", fontface="bold",alpha=0.7, size=3,
#             angle= label_data$angle, inherit.aes = FALSE )
#
# p
# ggsave(paste0(basepath, "smallest_rank_diff_roseplot.pdf"), width = 20, height = 20, units = "cm")
#
#
# ##############################################################################################################
# ##############################################################################################################
# ##############################################################################################################
# # ROSEPLOT for biggest rank change
# #######################################################
# #load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "Rank_diff_sum")] %>%
#   arrange(LEVEL1_COD, Rank_diff_sum)
# colnames(data) = c("individual","group", "value")
# # data$value = max(data$value,na.rm=T)-data$value
#
# tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
#                          LEVEL1_NAM = c("Europe","Africa", "Asia-\nTemperate","Asia-Tropical","Australasia", "Pacific", "North     \nAmerica", "South    \nAmerica", "Antarctic"))
#
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar=1
# to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
# colnames(to_add) = colnames(data)
# to_add$group=rep(levels(data$group), each=empty_bar)
# data=rbind(data, to_add)
#
# data = data %>% dplyr::arrange(group) %>%
#   dplyr::group_by(group) %>%
#   dplyr::mutate(rank = rank(-value))
# data = data %>% dplyr::filter(rank <= RANK)
#
# data$id=seq(1, nrow(data))
# data$label = NA
# for (gp in 1:9){
#   data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
# }
# #remmove antarctic
# data= data[data$group<9,]
#
# # Get the name and the y position of each label
# label_data=data
# number_of_bar=nrow(label_data)
# angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust<-ifelse( angle < -90, 1, 0)
# label_data$angle<-ifelse(angle < -90, angle+180, angle)
#
#
#
# #####################################
# # Windplot
# #####################################
# p = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Greys")) +
#   ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(),#element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()
#
#   ) +
#   geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
#                               max(data$value,na.rm=T)/5), color = "gray90") +
#   geom_vline(xintercept = c(0.5,5.5,10.5,16.5,21.5,26.5,31.5,36.5),
#                #seq(1,dim(data)[1],RANK)-0.5,
#              color = "black") +
#   coord_polar() +
#   geom_text(data = data.frame(x1 = seq((RANK/2)+1,dim(data)[1],RANK),
#                               y1 = rep(max(data$value,na.rm=T)*3.5, 8),
#                               label = tdwg1_names$LEVEL1_NAM[1:8]),
#             aes(x=x1, y=y1, label = label),
#             color="black", fontface="bold",alpha=0.9, size=4,
#             inherit.aes = FALSE
#   ) +
#
#   geom_text(data=label_data, aes(x=id, y=value+0.001,
#                                  label=individual, hjust=hjust),
#             color="black", fontface="bold",alpha=0.7, size=3,
#             angle= label_data$angle, inherit.aes = FALSE )
#
# p
# ggsave(paste0(basepath, "biggest_rank_diff_roseplot.pdf"), width = 20, height = 20, units = "cm")
#
#
#
# #####################################################################################################################################
# #####################################################################################################################################
# #######################################################
# # ROSEPLOT highest ranked
# #######################################################
# #load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "Rank_sum")] %>%
#   arrange(LEVEL1_COD, Rank_sum)
# colnames(data) = c("individual","group", "value")
# data$value = max(data$value,na.rm=T)-data$value
#
# tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
#                          LEVEL1_NAM = c("Europe","Africa", "Asia-\nTemperate","Asia-Tropical","Australasia", "Pacific", "North     \nAmerica", "South    \nAmerica", "Antarctic"))
#
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar=1
# to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
# colnames(to_add) = colnames(data)
# to_add$group=rep(levels(data$group), each=empty_bar)
# data=rbind(data, to_add)
#
# data = data %>% dplyr::arrange(group) %>%
#   dplyr::group_by(group) %>%
#   dplyr::mutate(rank = rank(-value))
# data = data %>% dplyr::filter(rank <= RANK)
#
# data$id=seq(1, nrow(data))
# data$label = NA
# for (gp in 1:9){
#   data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
# }
# #remmove antarctic
# data= data[data$group<9,]
#
# # Get the name and the y position of each label
# label_data=data
# number_of_bar=nrow(label_data)
# angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust<-ifelse( angle < -90, 1, 0)
# label_data$angle<-ifelse(angle < -90, angle+180, angle)
#
#
#
# #####################################
# # Windplot
# #####################################
# p = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Greys")) +
#   ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(),#element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()
#
#   ) +
#   geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
#                               max(data$value,na.rm=T)/5), color = "gray90") +
#   geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
#   coord_polar() +
#   geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
#                               y1 = rep(max(data$value,na.rm=T)*3.5, 8),
#                               label = tdwg1_names$LEVEL1_NAM[1:8]),
#             aes(x=x1, y=y1, label = label),
#             color="black", fontface="bold",alpha=0.9, size=4,
#             inherit.aes = FALSE
#   ) +
#
#   geom_text(data=label_data, aes(x=id, y=value+0.001,
#                                  label=individual, hjust=hjust),
#             color="black", fontface="bold",alpha=0.7, size=3,
#             angle= label_data$angle, inherit.aes = FALSE )
#
# p
# ggsave(paste0(basepath, "highest_ranked_roseplot.pdf"), width = 20, height = 20, units = "cm")
#
#
# ##############################################################################################################
# ##############################################################################################################
# #####################################################################################################################################
# #####################################################################################################################################
# #######################################################
# # ROSEPLOT lowest ranked
# #######################################################
# #load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "Rank_sum")] %>%
#   arrange(LEVEL1_COD, Rank_sum)
# colnames(data) = c("individual","group", "value")
# # data$value = max(data$value,na.rm=T)-data$value
#
# tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
#                          LEVEL1_NAM = c("Europe","Africa", "Asia-\nTemperate","Asia-Tropical","Australasia", "Pacific", "North     \nAmerica", "South    \nAmerica", "Antarctic"))
#
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar=1
# to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
# colnames(to_add) = colnames(data)
# to_add$group=rep(levels(data$group), each=empty_bar)
# data=rbind(data, to_add)
#
# data = data %>% dplyr::arrange(group) %>%
#   dplyr::group_by(group) %>%
#   dplyr::mutate(rank = rank(-value))
# data = data %>% dplyr::filter(rank <= RANK)
#
# data$id=seq(1, nrow(data))
# data$label = NA
# for (gp in 1:9){
#   data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
# }
# #remmove antarctic
# data= data[data$group<9,]
#
# # Get the name and the y position of each label
# label_data=data
# number_of_bar=nrow(label_data)
# angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust<-ifelse( angle < -90, 1, 0)
# label_data$angle<-ifelse(angle < -90, angle+180, angle)
#
#
#
# #####################################
# # Windplot
# #####################################
# p = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Greys")) +
#   ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(),#element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()
#
#   ) +
#   geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
#                               max(data$value,na.rm=T)/5), color = "gray90") +
#   geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
#   coord_polar() +
#   geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
#                               y1 = rep(max(data$value,na.rm=T)*3.5, 8),
#                               label = tdwg1_names$LEVEL1_NAM[1:8]),
#             aes(x=x1, y=y1, label = label),
#             color="black", fontface="bold",alpha=0.9, size=4,
#             inherit.aes = FALSE
#   ) +
#
#   geom_text(data=label_data, aes(x=id, y=value+0.001,
#                                  label=individual, hjust=hjust),
#             color="black", fontface="bold",alpha=0.7, size=3,
#             angle= label_data$angle, inherit.aes = FALSE )
#
# p
# ggsave(paste0(basepath, "lowest_ranked_roseplot.pdf"), width = 20, height = 20, units = "cm")
#
#
# ##############################################################################################################
# ##############################################################################################################
