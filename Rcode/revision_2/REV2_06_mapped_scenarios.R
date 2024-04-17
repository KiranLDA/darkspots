
# load dependencies
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
require(tmap)


# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/"
load( file = paste0(basepath, "REV_app_data.RData"))


##############################################################################
##############################################################################
### FUNCTIONS
##############################################################################
##############################################################################

library(classInt)

rotate <- function(x) t(apply(x, 2, rev))



colmat<-function(nquantiles=4, upperleft=rgb(0,150,235, maxColorValue=255),
                 upperright=rgb(130,0,80, maxColorValue=255),
                 bottomleft="grey",
                 bottomright=rgb(255,230,15, maxColorValue=255)
                 , xlab="x label", ylab="y label"
){


  my.data<-seq(0,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="fisher")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
  }
  seqs<-seq(0,100,(100/nquantiles))
  seqs[1]<-1
  return(col.matrix[c(seqs), c(seqs)])
}


normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}


##############################################################################
##############################################################################
## LOAD DATA
##############################################################################
##############################################################################

darkspots <- tdwg3#st_read(paste0(basepath, "/model_outputs.shp"))

####  PROJECT A in ECKERT IV
PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sf_use_s2(FALSE)
m = st_buffer(darkspots, 0)
darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)


#####################################################################
# write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT = read.csv( "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")

grid.DT <- data.table::as.data.table(grid.DT)


##########################################################################################
#############################################################################################
# Normalise data for manual bivariate map
##########################################################################################
##########################################################################################

areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))

# darkspots.prj$income = normalise(darkspots.prj$PC1)
# darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
darkspots.prj$linnean_yrs = - normalise(darkspots.prj$discoveries_time_diff)#- normalise(darkspots.prj$dscvrs_t_)
darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$descriptions_time_diff)#- normalise(darkspots.prj$dscrptns_t)
darkspots.prj$linnean = darkspots.prj$SR_unknown_norm#darkspots.prj$SR_nk_ # SR_unknown_norm
darkspots.prj$wallacean = darkspots.prj$SR_nogeoloc_norm #SR_ng_ #

#scale
complete_rows = !is.na(darkspots.prj$SR_unknown)
darkspots.prj$linnean_sc = darkspots.prj$SR_unknown
darkspots.prj$linnean_sc[complete_rows] = normalise(SAR(darkspots.prj$SR_unknown[complete_rows],
                                                        areas$land_area[complete_rows]))

complete_rows = !is.na(darkspots.prj$SR_nogeoloc)
darkspots.prj$wallacean_sc = darkspots.prj$SR_nogeoloc
darkspots.prj$wallacean_sc[complete_rows] = normalise(SAR(darkspots.prj$SR_nogeoloc[complete_rows],
                                                          areas$land_area[complete_rows]))

##########################################################################################
##########################################################################################
#  Plot scenarios
##########################################################################################
##########################################################################################


for (scenario in 1:9){

  darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S")])
  # darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("mean_randomised_rank_scenario_",scenario,"_S")])
  data = darkspots.prj
  # create map
  map <- ggplot() +
    geom_sf(data = data, mapping = aes(fill = benefit),
            color = aes(fill = benefit),#NA,
            size = 0.4, show.legend = FALSE) +
    geom_sf() +  #+
    geom_point( data= data,
                aes(color =  benefit,  #fill = bi_class,
                    geometry = geometry),
                size = 2,
                stat = "sf_coordinates"
    ) +
    scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
    scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
    guides(color = "none") +
    bi_theme() +
    geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                             |(long %in% c(-180,180) & lat %in% c(-90,90)
                               & region == "EW")],
              aes(x = X, y = Y, group = group),
              linetype = "solid", colour = "black", size = .3) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text=element_text(size=8),
          legend.title=element_text(size=10)
    )


  legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit),
                                                  color = aes(fill = benefit),#NA,
                                                  size = 0.4, show.legend = TRUE) +
                                  geom_sf() +  #+
                                  geom_point( data= data,
                                              aes(color =  benefit,  #fill = bi_class,
                                                  geometry = geometry),
                                              size = 2,
                                              stat = "sf_coordinates"
                                  ) +
                                  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
                                  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
                                  # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                  # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                  guides(color = "none",
                                         fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
                                  bi_theme() +
                                  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                                                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                                                             & region == "EW")],
                                            aes(x = X, y = Y, group = group),
                                            linetype = "solid", colour = "black", size = .3) +
                                  theme(axis.title.y=element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.text.x=element_blank(),
                                        panel.border = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        panel.grid.major = element_blank(),
                                        legend.text=element_text(size=8),
                                        legend.title=element_text(size=10)))


  # combine map with legend
  assign(paste0("finalPlot",scenario),
         ggdraw() +
           draw_plot(map, 0, 0, 1, 1) +
           draw_plot(legend, 0.8, .7, 0.24, 0.24))


  #######################################################
  # ROSEPLOT
  #######################################################

  RANK=5

  data = st_drop_geometry(darkspots.prj)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit")] %>%
    arrange(LEVEL1_COD, benefit)
  colnames(data) = c("individual","group", "value")

  tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                           LEVEL1_NAM = c("Europe","Africa", "Asia-\nTemperate","Asia-Tropical","Australasia", "Pacific", "North     \nAmerica", "South    \nAmerica", "Antarctic"))

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=1
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)

  data = data %>% dplyr::arrange(group) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(rank = rank(-value))
  data = data %>% dplyr::filter(rank <= RANK)

  data$id=seq(1, nrow(data))
  data$label = NA
  for (gp in 1:9){
    data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
  }
  #remmove antarctic
  data= data[data$group<9,]

  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)

  p = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
    scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
    ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),#element_text(size = 10),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()

    ) +
    geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
                                max(data$value,na.rm=T)/5), color = "gray90") +
    geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
    coord_polar() +
    geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                                y1 = rep(max(data$value,na.rm=T)*3.5, 8),
                                label = tdwg1_names$LEVEL1_NAM[1:8]),
              aes(x=x1, y=y1, label = label),
              color="black", fontface="bold",alpha=0.9, size=4,
              inherit.aes = FALSE
    ) +

    geom_text(data=label_data, aes(x=id, y=value+0.001,
                                   label=individual, hjust=hjust),
              color="black", fontface="bold",alpha=0.7, size=3,
              angle= label_data$angle, inherit.aes = FALSE )

  # p
  assign(paste0("p",scenario),p)

  #=================================================================================================================
  ##################################################################
  #  SCALED
  ##################################################################


  darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S_sc")])
  # darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("mean_randomised_rank_scenario_",scenario,"_S_sc")])

  data = darkspots.prj
  # create map
  map <- ggplot() +
    geom_sf(data = data, mapping = aes(fill = benefit),
            color = aes(fill = benefit),#NA,
            size = 0.4, show.legend = FALSE) +
    geom_sf() +  #+
    geom_point( data= data,
                aes(color =  benefit,  #fill = bi_class,
                    geometry = geometry),
                size = 2,
                stat = "sf_coordinates"
    ) +
    scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
    scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
    guides(color = "none") +
    bi_theme() +
    geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                             |(long %in% c(-180,180) & lat %in% c(-90,90)
                               & region == "EW")],
              aes(x = X, y = Y, group = group),
              linetype = "solid", colour = "black", size = .3) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.text=element_text(size=8),
          legend.title=element_text(size=10)
    )

  legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit),
                                                  color = aes(fill = benefit),#NA,
                                                  size = 0.4, show.legend = TRUE) +
                                  geom_sf() +  #+
                                  geom_point( data= data,
                                              aes(color =  benefit,  #fill = bi_class,
                                                  geometry = geometry),
                                              size = 2,
                                              stat = "sf_coordinates"
                                  ) +
                                  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
                                  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
                                  # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                  # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                  guides(color = "none",
                                         fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
                                  bi_theme() +
                                  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                                                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                                                             & region == "EW")],
                                            aes(x = X, y = Y, group = group),
                                            linetype = "solid", colour = "black", size = .3) +
                                  theme(axis.title.y=element_blank(),
                                        axis.title.x=element_blank(),
                                        axis.text.y=element_blank(),
                                        axis.text.x=element_blank(),
                                        panel.border = element_blank(),
                                        panel.grid.minor = element_blank(),
                                        panel.grid.major = element_blank(),
                                        legend.text=element_text(size=8),
                                        legend.title=element_text(size=10)))


  # combine map with legend
  assign(paste0("finalPlot",scenario,"sc"),
         ggdraw() +
           draw_plot(map, 0, 0, 1, 1) +
           draw_plot(legend, 0.8, .7, 0.24, 0.24))


  #######################################################
  # ROSEPLOT
  #######################################################
  # load(paste0(basepath, "app_data.RData"))

  RANK=5

  data = st_drop_geometry(darkspots.prj)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit")] %>%
    arrange(LEVEL1_COD, benefit)
  colnames(data) = c("individual","group", "value")

  tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                           LEVEL1_NAM = c("Europe","Africa", "Asia-\nTemperate","Asia-Tropical","Australasia", "Pacific", "North     \nAmerica", "South    \nAmerica", "Antarctic"))

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar=1
  to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
  colnames(to_add) = colnames(data)
  to_add$group=rep(levels(data$group), each=empty_bar)
  data=rbind(data, to_add)

  data = data %>% dplyr::arrange(group) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(rank = rank(-value))
  data = data %>% dplyr::filter(rank <= RANK)

  data$id=seq(1, nrow(data))
  data$label = NA
  for (gp in 1:9){
    data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
  }
  #remmove antarctic
  data= data[data$group<9,]

  # Get the name and the y position of each label
  label_data=data
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  label_data$angle<-ifelse(angle < -90, angle+180, angle)


  p = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
    scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
    ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),#element_text(size = 10),
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()

    ) +
    geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
                                max(data$value,na.rm=T)/5), color = "gray90") +
    geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
    coord_polar() +
    geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                                y1 = rep(max(data$value,na.rm=T)*3.5, 8),
                                label = tdwg1_names$LEVEL1_NAM[1:8]),
              aes(x=x1, y=y1, label = label),
              color="black", fontface="bold",alpha=0.9, size=4,
              inherit.aes = FALSE
    ) +

    geom_text(data=label_data, aes(x=id, y=value+0.001,
                                   label=individual, hjust=hjust),
              color="black", fontface="bold",alpha=0.7, size=3,
              angle= label_data$angle, inherit.aes = FALSE )

  # p
  assign(paste0("p",scenario,"sc"),p)


  ggarrange(get(paste0("finalPlot",scenario)),
            get(paste0("p",scenario)),
            get(paste0("finalPlot",scenario,"sc")),
            get(paste0("p",scenario,"sc")),
            labels = c("a.", "b.","c.","d."),
            font.label = list(size = 30),
            ncol = 2, nrow = 2, widths = c(1, 0.5))

  ggsave(paste0(basepath, "scenario_",scenario,".pdf"),  width = 40, height = 30, units = "cm")
  ggsave(paste0(basepath, "scenario_",scenario,".png"),  width = 40, height = 30, units = "cm",bg="white")

}












#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# #####################################################################################################################
# #######################################
# # darkspots
# #######################################
# # SCENARIO 1
# data = darkspots.prj
#
#
# # create map
# map1 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_1_S),
#           color = aes(fill = benefit_1_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_1_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map1
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_1_S),
#                                                 color = aes(fill = benefit_1_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_1_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot1 <- ggdraw() +
#   draw_plot(map1, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot1
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_1_S")] %>%
#   arrange(LEVEL1_COD, benefit_1_S)
# colnames(data) = c("individual","group", "value")
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
# p1 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p1
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
#
# #################################
# # SCENARIO 1 SCALED
# data = darkspots.prj
#
#
# # create map
# map1sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_1_S_sc),
#           color = aes(fill = benefit_1_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_1_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map1sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_1_S_sc),
#                                                 color = aes(fill = benefit_1_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_1_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot1sc <- ggdraw() +
#   draw_plot(map1sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot1sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_1_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_1_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p1sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p1sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot1, p1 , finalPlot1sc, p1sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_1.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_1.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
#
#
# ####################################################################################################
# # SCENARIO 2
#
# data = darkspots.prj
#
# # create map
# map2 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_2_S),
#           color = aes(fill = benefit_2_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_2_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map2
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_2_S),
#                                                 color = aes(fill = benefit_2_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_2_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot2 <- ggdraw() +
#   draw_plot(map2, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot2
# # ggsave(paste0(basepath, "map_scenario_2_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_2_S")] %>%
#   arrange(LEVEL1_COD, benefit_2_S)
# colnames(data) = c("individual","group", "value")
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
# p2 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p2
# # ggsave(paste0(basepath, "roseplot_scenario_2_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map2,p2,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_2_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
# #################################
# # SCENARIO 2 SCALED
# data = darkspots.prj
#
#
# # create map
# map2sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_2_S_sc),
#           color = aes(fill = benefit_2_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_2_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map2sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_2_S_sc),
#                                                 color = aes(fill = benefit_2_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_2_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot2sc <- ggdraw() +
#   draw_plot(map2sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot2sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_2_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_2_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p2sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p2sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot2, p2 , finalPlot2sc, p2sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_2.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_2.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
#
# ####################################################################################################
# # SCENARIO 3
#
# data = darkspots.prj
#
# # create map
# map3 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_3_S),
#           color = aes(fill = benefit_3_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_3_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map3
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_3_S),
#                                                 color = aes(fill = benefit_3_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_3_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot3 <- ggdraw() +
#   draw_plot(map3, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot3
# # ggsave(paste0(basepath, "map_scenario_3_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_3_S")] %>%
#   arrange(LEVEL1_COD, benefit_3_S)
# colnames(data) = c("individual","group", "value")
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
# p3 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p3
# # ggsave(paste0(basepath, "roseplot_scenario_3_S.pdf"), width = 15, height = 15, units = "cm")
#
# # ggarrange(map3,p3,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_3_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
# #################################
# # SCENARIO 3 SCALED
# data = darkspots.prj
#
#
# # create map
# map3sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_3_S_sc),
#           color = aes(fill = benefit_3_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_3_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map3sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_3_S_sc),
#                                                 color = aes(fill = benefit_3_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_3_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot3sc <- ggdraw() +
#   draw_plot(map3sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot3sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_3_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_3_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p3sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p3sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot3, p3 , finalPlot3sc, p3sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_3.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_3.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
# ####################################################################################################
# # SCENARIO 4
#
# data = darkspots.prj
#
# # create map
# map4 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_4_S),
#           color = aes(fill = benefit_4_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_4_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map4
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_4_S),
#                                                 color = aes(fill = benefit_4_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_4_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot4 <- ggdraw() +
#   draw_plot(map4, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot4
# # ggsave(paste0(basepath, "map_scenario_4_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_4_S")] %>%
#   arrange(LEVEL1_COD, benefit_4_S)
# colnames(data) = c("individual","group", "value")
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
# p4 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p4
# # ggsave(paste0(basepath, "roseplot_scenario_4_S.pdf"), width = 15, height = 15, units = "cm")
#
# # ggarrange(map4,p4,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_4_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
# #################################
# # SCENARIO 4 SCALED
# data = darkspots.prj
#
#
# # create map
# map4sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_4_S_sc),
#           color = aes(fill = benefit_4_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_4_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map4sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_4_S_sc),
#                                                 color = aes(fill = benefit_4_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_4_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot4sc <- ggdraw() +
#   draw_plot(map4sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot4sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_4_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_4_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p4sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p4sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot4, p4 , finalPlot4sc, p4sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_4.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_4.png"),  width = 40, height = 30, units = "cm",bg="white")
#
# ####################################################################################################
# # SCENARIO 5
#
# data = darkspots.prj
#
# # create map
# map5 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_5_S),
#           color = aes(fill = benefit_5_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_5_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map5
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_5_S),
#                                                 color = aes(fill = benefit_5_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_5_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot5 <- ggdraw() +
#   draw_plot(map5, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot5
# # ggsave(paste0(basepath, "map_scenario_5_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_5_S")] %>%
#   arrange(LEVEL1_COD, benefit_5_S)
# colnames(data) = c("individual","group", "value")
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
# p5 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p5
# # ggsave(paste0(basepath, "roseplot_scenario_5_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map5,p5,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_5_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
# #################################
# # SCENARIO 5 SCALED
# data = darkspots.prj
#
#
# # create map
# map5sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_5_S_sc),
#           color = aes(fill = benefit_5_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_5_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map5sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_5_S_sc),
#                                                 color = aes(fill = benefit_5_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_5_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot5sc <- ggdraw() +
#   draw_plot(map5sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot5sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_5_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_5_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p5sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p5sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot5, p5 , finalPlot5sc, p5sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_5.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_5.png"),  width = 40, height = 30, units = "cm",bg="white")
#
# ####################################################################################################
# # SCENARIO 6
#
# data= darkspots.prj
#
# # create map
# map6 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_6_S),
#           color = aes(fill = benefit_6_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_6_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map6
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_6_S),
#                                                 color = aes(fill = benefit_6_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_6_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot6 <- ggdraw() +
#   draw_plot(map6, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot6
# # ggsave(paste0(basepath, "map_scenario_6_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_6_S")] %>%
#   arrange(LEVEL1_COD, benefit_6_S)
# colnames(data) = c("individual","group", "value")
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
# p6 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p6
# # ggsave(paste0(basepath, "roseplot_scenario_6_S.pdf"), width = 15, height = 15, units = "cm")
#
#
#
# # ggarrange(map6,p6,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_6_S.pdf"),
# #        width = 47, height = 17, units = "cm")
# #################################
# # SCENARIO 6 SCALED
# data = darkspots.prj
#
#
# # create map
# map6sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_6_S_sc),
#           color = aes(fill = benefit_6_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_6_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map6sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_6_S_sc),
#                                                 color = aes(fill = benefit_6_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_6_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot6sc <- ggdraw() +
#   draw_plot(map6sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot6sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_6_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_6_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p6sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p6sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot6, p6 , finalPlot6sc, p6sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_6.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_6.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
#
#
# ####################################################################################################
# # SCENARIO 7
#
# data = darkspots.prj
#
# # create map
# map7 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_7_S),
#           color = aes(fill = benefit_7_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_7_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map7
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_7_S),
#                                                 color = aes(fill = benefit_7_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_7_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot7 <- ggdraw() +
#   draw_plot(map7, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot7
# # ggsave(paste0(basepath, "map_scenario_7_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_7_S")] %>%
#   arrange(LEVEL1_COD, benefit_7_S)
# colnames(data) = c("individual","group", "value")
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
# p7 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p7
# # ggsave(paste0(basepath, "roseplot_scenario_7_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# #
# # ggarrange(map7,p7,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_7_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
# #################################
# # SCENARIO 7 SCALED
# data = darkspots.prj
#
#
# # create map
# map7sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_7_S_sc),
#           color = aes(fill = benefit_7_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_7_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map7sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_7_S_sc),
#                                                 color = aes(fill = benefit_7_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_7_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot7sc <- ggdraw() +
#   draw_plot(map7sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot7sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_7_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_7_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p7sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p7sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot7, p7 , finalPlot7sc, p7sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_7.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_7.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
#
#
# ####################################################################################################
# # SCENARIO 8
#
# data = darkspots.prj
#
# # create map
# map8 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_8_S),
#           color = aes(fill = benefit_8_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_8_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map8
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_8_S),
#                                                 color = aes(fill = benefit_8_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_8_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot8 <- ggdraw() +
#   draw_plot(map8, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot8
# # ggsave(paste0(basepath, "map_scenario_8_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_8_S")] %>%
#   arrange(LEVEL1_COD, benefit_8_S)
# colnames(data) = c("individual","group", "value")
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
# p8 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p8
# # ggsave(paste0(basepath, "roseplot_scenario_8_S.pdf"), width = 15, height = 15, units = "cm")
#
#
#
#
# # ggarrange(map8,p8,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_8_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
#
#
# #################################
# # SCENARIO 8 SCALED
# data = darkspots.prj
#
#
# # create map
# map8sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_8_S_sc),
#           color = aes(fill = benefit_8_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_8_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map8sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_8_S_sc),
#                                                 color = aes(fill = benefit_8_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_8_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot8sc <- ggdraw() +
#   draw_plot(map8sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot8sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_8_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_8_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p8sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p8sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot8, p8 , finalPlot8sc, p8sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_8.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_8.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
#
# ####################################################################################################
# # SCENARIO 9
#
# data = darkspots.prj
#
# # create map
# map9 <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_9_S),
#           color = aes(fill = benefit_9_S),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_9_S,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map9
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_9_S),
#                                                 color = aes(fill = benefit_9_S),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_9_S,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot9 <- ggdraw() +
#   draw_plot(map9, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot9
# # ggsave(paste0(basepath, "map_scenario_9_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_9_S")] %>%
#   arrange(LEVEL1_COD, benefit_9_S)
# colnames(data) = c("individual","group", "value")
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
# p9 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p9
# # ggsave(paste0(basepath, "roseplot_scenario_9_S.pdf"), width = 15, height = 15, units = "cm")
#
# # ggarrange(map9,p9,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_9_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
# #################################
# # SCENARIO 9 SCALED
# data = darkspots.prj
#
#
# # create map
# map9sc <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_9_S_sc),
#           color = aes(fill = benefit_9_S_sc),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_9_S_sc,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#   guides(color = "none") +
#   bi_theme() +
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
# map9sc
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_9_S_sc),
#                                                 color = aes(fill = benefit_9_S_sc),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_9_S_sc,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Benefit"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 )
# )
#
#
#
#
# # combine map with legend
# finalPlot9sc <- ggdraw() +
#   draw_plot(map9sc, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot9sc
# # ggsave(paste0(basepath, "map_scenario_1_S.pdf"), width = 30, height = 12, units = "cm")
#
#
#
#
# #######################################################
# # ROSEPLOT
# #######################################################
# # load(paste0(basepath, "app_data.RData"))
#
# RANK=5
#
# data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_9_S_sc")] %>%
#   arrange(LEVEL1_COD, benefit_9_S_sc)
# colnames(data) = c("individual","group", "value")
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
# p9sc = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGn")) +
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
# p9sc
# # ggsave(paste0(basepath, "roseplot_scenario_1_S.pdf"), width = 15, height = 15, units = "cm")
#
#
# # ggarrange(map1,p1,ncol = 2, nrow = 1, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1_S.pdf"),
# #        width = 47, height = 17, units = "cm")
#
#
# ggarrange(finalPlot9, p9 , finalPlot9sc, p9sc ,
#           labels = c("a.", "b.","c.","d."),
#           font.label = list(size = 30),
#           ncol = 2, nrow = 2, widths = c(1, 0.5))
#
# ggsave(paste0(basepath, "scenario_9.pdf"),  width = 40, height = 30, units = "cm")
# ggsave(paste0(basepath, "scenario_9.png"),  width = 40, height = 30, units = "cm",bg="white")
#
#
# #
# #
# # ggarrange(map1,p1,map2,p2, map3,p3,ncol = 2, nrow = 3, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_1-3_S.pdf"),
# #        width = 47, height = 50, units = "cm")
# #
# #
# # ggarrange(map4,p4,map5,p5, map6,p6,ncol = 2, nrow = 3, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_4-6_S.pdf"),
# #        width = 47, height = 50, units = "cm")
# #
# #
# # ggarrange(map7,p7,map8,p8, map9,p9,ncol = 2, nrow = 3, widths = c(1, 0.5))
# # ggsave(paste0(basepath, "scenarios_7-9_S.pdf"),
# #        width = 47, height = 50, units = "cm")
#
#
#
# #
# # ggarrange(map1,map2,map3,
# #           map4,map6,map8,
# #           map5, map7,map9,
# #          ncol = 3, nrow = 3)
# # ggsave(paste0(basepath, "maps_1-9_S.pdf"),
# #        width = 80, height = 50, units = "cm")
# #
# # ggarrange(p1,p2,p3,
# #           p4,p6,p8,
# #           p5,p7,p9,
# #          ncol = 3, nrow = 3)
# # ggsave(paste0(basepath, "roseplots_1-9_S.pdf"),
# #        width = 42, height = 42, units = "cm")
#
#








