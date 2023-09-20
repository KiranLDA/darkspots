
# load dependencies
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
require(tmap)
library(dplyr) #roseplot
# library(plyr)
# detach("package:plyr", unload=TRUE)

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




##############################################################################
##############################################################################
## LOAD DATA
##############################################################################
##############################################################################



darkspots <- tdwg3#st_read(paste0(basepath, "/model_outputs.shp"))


normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

#####################################################################

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

# # create a bounding box - world extent
# b.box <- as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")
#
# # assign CRS to box
# WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#
# proj4string(b.box) <- WGS84
#
# # create graticules/grid lines from box
# grid <- gridlines(b.box,
#                   easts  = seq(from=-180, to=180, by=20),
#                   norths = seq(from=-90, to=90, by=10))
#
# # give the PORJ.4 string for Eckert IV projection
# proj_eckert <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# # transform bounding box
# grid.DT <- data.table::data.table(map_data(SpatialLinesDataFrame(sl=grid,
#                                                                  data=data.frame(1:length(grid)),
#                                                                  match.ID = FALSE)))
# # assign matrix of projected coordinates as two columns in data table
# grid.DT[, c("X","Y") := data.table::data.table(proj4::project(cbind(long, lat),
#                                                               proj=proj_eckert))]

#####################################################################
##########################################################################################
#############################################################################################
# Normalise data for manual bivariate map
##########################################################################################
##########################################################################################


darkspots.prj$drk_unprotect = darkspots.prj$benefit_4_S#darkspots.prj$bnf_4
darkspots.prj$drk = darkspots.prj$benefit_1_S#darkspots.prj$bnf_1
darkspots.prj$income = normalise(darkspots.prj$PC1)
darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
darkspots.prj$linnean_yrs = - normalise(darkspots.prj$discoveries_time_diff)#- normalise(darkspots.prj$dscvrs_t_)
darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$descriptions_time_diff)#- normalise(darkspots.prj$dscrptns_t)
darkspots.prj$linnean = darkspots.prj$SR_unknown_norm#darkspots.prj$SR_nk_ # SR_unknown_norm
darkspots.prj$wallacean = darkspots.prj$SR_nogeoloc_norm #SR_ng_ #
# darkspots.prj$linnean_sc = darkspots.prj$SR_unknown_norm_sc#darkspots.prj$SR_nk_ # SR_unknown_norm
# darkspots.prj$wallacean_sc = darkspots.prj$SR_nogeoloc_norm_sc #SR_ng_ #

#scale
complete_rows = !is.na(darkspots.prj$SR_unknown_norm)
darkspots.prj$linnean_sc = darkspots.prj$SR_unknown_norm
darkspots.prj$linnean_sc[complete_rows] = SAR(darkspots.prj$SR_unknown_norm[complete_rows],
                                              areas$land_area[complete_rows])

complete_rows = !is.na(darkspots.prj$SR_nogeoloc_norm)
darkspots.prj$wallacean_sc = darkspots.prj$SR_nogeoloc_norm
darkspots.prj$wallacean_sc[complete_rows] = SAR(darkspots.prj$SR_nogeoloc_norm[complete_rows],
                                                areas$land_area[complete_rows])




#######################################
##########################################################
# Linnean
#######################################

data = darkspots.prj


# create map
map1 <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = linnean),
          color = aes(fill = linnean),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  linnean,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  # scale_fill_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
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
map1


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = linnean),
                                                color = aes(fill = linnean),#NA,
                                                size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  linnean,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
                                # scale_fill_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Normalised Linnean shortfall"),override.aes = list(size = 0.5)) +
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
)




# combine map with legend
finalPlot1 <- ggdraw() +
  draw_plot(map1, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot1
ggsave(paste0(basepath, "Linnean_gap_map.pdf"), width = 30, height = 12, units = "cm")


#######################################################
#######################################################
# ROSEPLOT


# load(paste0(basepath, "app_data.RData"))

RANK=5

###########################################################
#### Linnean
################################################################
 #SR_nogeoloc_norm
data = st_drop_geometry(darkspots.prj[,c("LEVEL3_NAM","LEVEL1_COD", "linnean")]) %>%
  arrange(LEVEL1_COD, linnean)
colnames(data) = c("individual","group", "value")




tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-\nTemperate",
                                        "Asia-Tropical",
                                        "Australasia",
                                        "Pacific",
                                        "North     \nAmerica",
                                        "South    \nAmerica",
                                        "Antarctic"))



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





#####################################
# Windplot
#####################################
p1 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
  # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues")) +
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  # scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  # scale_fill_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#6eabbd", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150

  # scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*2.8) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),#element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()#,
    # plot.margin = unit(rep(1,4), "cm")

  ) +
  geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
                              max(data$value,na.rm=T)/5), color = "gray90") +
  geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                              y1 = rep(max(data$value,na.rm=T)*2.7, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
            #               label_data$angle[seq(5,81, 10)]+90),
            # hjust=0.2,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.001,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p1
ggsave(paste0(basepath, "linnean_gap_roseplot.pdf"), width = 20, height = 20, units = "cm")






####################################################################################################
##########################################################
# Wallacean
#######################################

data = darkspots.prj


# create map
map2 <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = wallacean),
          color = aes(fill = wallacean),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  wallacean,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
  # scale_fill_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150

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
map2


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = wallacean),
                                                color = aes(fill = wallacean),#NA,
                                                size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  wallacean,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
                                # scale_fill_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Normalised Wallacean shortfall"),override.aes = list(size = 0.5)) +
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
)




# combine map with legend
finalPlot2 <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot2
ggsave(paste0(basepath, "Wallacean_gap_map.pdf"), width = 30, height = 12, units = "cm")




#######################################################
# ROSEPLOT
#######################################################
# load(paste0(basepath, "app_data.RData"))

RANK=5

data = st_drop_geometry(darkspots.prj[,c("LEVEL3_NAM","LEVEL1_COD", "wallacean")]) %>%
  arrange(LEVEL1_COD, wallacean)
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



#####################################
# Windplot
#####################################
p2 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds")) +
  # scale_fill_gradient2(high = "#c85a59", mid = "#dbbab9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#c85a59", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*2.8) +
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
                              y1 = rep(max(data$value,na.rm=T)*2.7, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            inherit.aes = FALSE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.001,
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p2
ggsave(paste0(basepath, "wallacean_gap_roseplot.pdf"), width = 20, height = 20, units = "cm")






ggarrange(map1,p1,map2,p2, ncol = 2, nrow = 2, widths = c(1, 0.5))
ggsave(paste0(basepath, "linnean_wallacean_together.pdf"),
       width = 40, height = 30, units = "cm")
ggsave(paste0(basepath, "linnean_wallacean_together.png"),
       width = 40, height = 30, units = "cm", bg = 'white')
##########################################################################################
#############################################################################################
#  SCALED
##########################################################################################
##########################################################################################



#######################################
##########################################################
# Linnean
#######################################

data = darkspots.prj


# create map
map1 <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = linnean_sc),
          color = aes(fill = linnean_sc),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  linnean_sc,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  # scale_fill_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
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
map1


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = linnean_sc),
                                                color = aes(fill = linnean_sc),#NA,
                                                size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  linnean_sc,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
                                # scale_fill_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Normalised Linnean shortfall"),override.aes = list(size = 0.5)) +
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
)




# combine map with legend
finalPlot1 <- ggdraw() +
  draw_plot(map1, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot1
ggsave(paste0(basepath, "Linnean_gap_map_sc.pdf"), width = 30, height = 12, units = "cm")


#######################################################
#######################################################
# ROSEPLOT


# load(paste0(basepath, "app_data.RData"))

RANK=5

###########################################################
#### Linnean
################################################################
#SR_nogeoloc_norm
data = st_drop_geometry(darkspots.prj[,c("LEVEL3_NAM","LEVEL1_COD", "linnean_sc")]) %>%
  arrange(LEVEL1_COD, linnean_sc)
colnames(data) = c("individual","group", "value")




tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-\nTemperate",
                                        "Asia-Tropical",
                                        "Australasia",
                                        "Pacific",
                                        "North     \nAmerica",
                                        "South    \nAmerica",
                                        "Antarctic"))



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





#####################################
# Windplot
#####################################
p1 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
  # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues")) +
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  # scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Blues"))+
  # scale_fill_gradient2(high = "#6eabbd", mid = "#bfd3d9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#6eabbd", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150

  # scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*2.8) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),#element_text(size = 10),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()#,
    # plot.margin = unit(rep(1,4), "cm")

  ) +
  geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
                              max(data$value,na.rm=T)/5), color = "gray90") +
  geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                              y1 = rep(max(data$value,na.rm=T)*2.7, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
            #               label_data$angle[seq(5,81, 10)]+90),
            # hjust=0.2,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.001,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p1
ggsave(paste0(basepath, "linnean_gap_roseplot_sc.pdf"), width = 20, height = 20, units = "cm")






####################################################################################################
##########################################################
# Wallacean
#######################################

data = darkspots.prj


# create map
map2 <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = wallacean_sc),
          color = aes(fill = wallacean_sc),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  wallacean_sc,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
  # scale_fill_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150

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
map2


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = wallacean_sc),
                                                color = aes(fill = wallacean_sc),#NA,
                                                size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  wallacean_sc,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds"))+
                                # scale_fill_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "#c85a59", mid = "#dbbab9",low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Normalised Wallacean shortfall"),override.aes = list(size = 0.5)) +
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
)




# combine map with legend
finalPlot2 <- ggdraw() +
  draw_plot(map2, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot2
ggsave(paste0(basepath, "Wallacean_gap_map_sc.pdf"), width = 30, height = 12, units = "cm")




#######################################################
# ROSEPLOT
#######################################################
# load(paste0(basepath, "app_data.RData"))

RANK=5

data = st_drop_geometry(darkspots.prj[,c("LEVEL3_NAM","LEVEL1_COD", "wallacean_sc")]) %>%
  arrange(LEVEL1_COD, wallacean_sc)
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



#####################################
# Windplot
#####################################
p2 = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=value), stat="identity", alpha=0.9) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "Reds")) +
  # scale_fill_gradient2(high = "#c85a59", mid = "#dbbab9", low ="#e8e8e8" , midpoint = 0.6)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#c85a59", low ="#e8e8e8" , midpoint = 0.6)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*2.8) +
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
                              y1 = rep(max(data$value,na.rm=T)*2.7, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            inherit.aes = FALSE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.001,
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p2
ggsave(paste0(basepath, "wallacean_gap_roseplot_sc.pdf"), width = 20, height = 20, units = "cm")






ggarrange(map1,p1,map2,p2, ncol = 2, nrow = 2, widths = c(1, 0.5))
ggsave(paste0(basepath, "linnean_wallacean_together_sc.pdf"),
       width = 40, height = 30, units = "cm")
ggsave(paste0(basepath, "linnean_wallacean_together_sc.png"),
    width = 40, height = 30, units = "cm", bg = 'white')
