
# load dependencies
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
require(tmap)


# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"


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


# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
darkspots <- st_read(paste0(basepath, "/model_outputs.shp"))



####  PROJECT A in ECKERT IV

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sf_use_s2(FALSE)
m = st_buffer(darkspots, 0)
darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)


####
# create bounding box
# create a bounding box - world extent
b.box <- as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")

# assign CRS to box
WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

proj4string(b.box) <- WGS84

# create graticules/grid lines from box
grid <- gridlines(b.box,
                  easts  = seq(from=-180, to=180, by=20),
                  norths = seq(from=-90, to=90, by=10))

# give the PORJ.4 string for Eckert IV projection
proj_eckert <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# transform bounding box
grid.DT <- data.table::data.table(map_data(SpatialLinesDataFrame(sl=grid,
                                                     data=data.frame(1:length(grid)),
                                                     match.ID = FALSE)))
# assign matrix of projected coordinates as two columns in data table
grid.DT[, c("X","Y") := data.table::data.table(proj4::project(cbind(long, lat),
                                           proj=proj_eckert))]
################



normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}


##########################################################################################
#############################################################################################
# Normalise data for manual bivariate map
##########################################################################################
##########################################################################################
total_area = 24922455
areas = read.csv(paste0(basepath, "twdg3_land_area.csv"))



darkspots.prj$drk_unprotect = darkspots.prj$bnf_4
darkspots.prj$drk = darkspots.prj$bnf_1
darkspots.prj$income = normalise(darkspots.prj$PC1)
darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
darkspots.prj$linnean_yrs = - normalise(darkspots.prj$dscvrs_t_)
darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$dscrptns_t)
darkspots.prj$linnean = darkspots.prj$SR_nk_ # SR_unknown_norm
darkspots.prj$wallacean = darkspots.prj$SR_ng_ # SR_nogeoloc_norm #
darkspots.prj$areas = areas$land_area


###########################
# c('Global', 'Global & Regional', "None", 'Regional'))
colours_map = c("#2a9bc1","#483737","#cccccc", "#84cfbb")
#c( "#2a9bc1","black", "grey90","#84cfbb")
#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"),
#, c("black","brown","grey","red"))+



#####################################################################################################################
#######################################
# darkspots
#######################################
# SCENARIO 1


data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__1),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = cbind(LEVEL3_N=data$LEVEL3_N,
                   S1 = data$color_class)
data = darkspots.prj

#
# ##########
# data = data.frame(darkspots.prj$LEVEL3_N,
#             as.numeric(darkspots.prj$LEVEL1),
#              as.numeric(darkspots.prj$Rn__1))
#
# colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank")
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
# data2$color_class = ifelse(data2$global_rank <= 20, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$global_rank <=20)] = "brown"
# data2 = data2 %>% dplyr::arrange(LEVEL3_N)
# data =  data %>% left_join(data2)
# data2 = data %>% dplyr::arrange(LEVEL3_N)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = cbind(LEVEL3_N=data$LEVEL3_N,
#                    S1 = data$color_class)
#
# data = darkspots.prj


# create map
map1 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01#, show.legend = FALSE
          ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

legend <- cowplot::get_legend(map1)
legend
ggsave(paste0(basepath, "maps_1-9_top_legend.pdf"),
       width = 5, height = 5, units = "cm")

# create map
map1 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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


#######################################
# SCENARIO 2


data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__2),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S2 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map2 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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


#######################################
# SCENARIO 3


data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__3),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S3 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map3 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map3


#######################################
# SCENARIO 4


data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__4),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S4 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map4 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map4




#######################################
# SCENARIO 5

data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__5),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S5 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map5 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map5




#######################################
# SCENARIO 6

data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__6),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S6 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map6 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map6


#######################################
# SCENARIO 7

data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__7),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S7 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map7 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map7


#######################################
# SCENARIO 8

data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__8),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S8 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map8 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map8


#######################################
# SCENARIO 9

data = data.frame(darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rn__9),
                  as.numeric(darkspots.prj$area))

colnames(data) = c("LEVEL3_N","LEVEL1", "global_rank", "area")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_N)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                           "grey")
data2$color_class[which(data2$regional_rank <=5)] = "red"
data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
data2$color_class[is.na(data2$color_class)] = "grey"
data2 = data2 %>% dplyr::arrange(LEVEL3_N)
data =  data %>% dplyr::arrange(LEVEL3_N)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_N)
darkspots.prj$color_class = data$color_class
data = data[which(data$color_class != "grey"),]
priorities = merge(priorities,
                   cbind(LEVEL3_N=data$LEVEL3_N,
                         S9 = data$color_class),
                   all=TRUE)
data = darkspots.prj


# create map
map9 <-  ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA,
          size = 0.01, show.legend = FALSE
  ) +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", size = .3) +

  scale_fill_manual(values =   colours_map,
                    labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
  scale_color_manual(values =  colours_map)+
  guides(color = "none",
         fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +

  # bi_theme() +
  theme_bw() +
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

map9




# "#1f5c6c" # "#328380"





ggarrange(map1,map2,map3,
          map4,map6,map8,
          map5, map7,map9,
          ncol = 3, nrow = 3)
ggsave(paste0(basepath, "maps_1-9_top.pdf"),
       width = 80, height = 50, units = "cm")




####################################################################################
# now do a table ofpriorities
data = data.frame(darkspots.prj$LEVEL3_C, darkspots.prj$LEVEL3_N)
colnames(data) = c("LEVEL3_N", "LEVEL3_C")
priorities = merge(priorities,
             data,
             by = "LEVEL3_N",
             all.x=TRUE)
rownames(priorities) = priorities$LEVEL3_C
# priorities = priorities[,1:(ncol(priorities)-1)]
priorities$freq = apply(priorities[,1:(ncol(priorities)-1)], 1,
                        function(x) {(3*length(which(x=="black" | x == "brown"))
                                      +0.05*length(which(x== "red")))})
  #count(priorities=="black")$freq + count(priorities=="brown")$freq
priorities = priorities[order(priorities[,"freq"],decreasing=TRUE),]


data <- priorities[with(priorities,order(-freq)),]
data <- data[1:50,]

# priorities = priorities[priorities$freq >=6.05,]

library(zoo)
data2= fortify.zoo(zoo(data[2:(ncol(data)-2)]), melt = TRUE)
data2$Label = data$LEVEL3_C[data2$Index]
data2$Value[is.na(data2$Value)] = "grey"

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=factor(Index), y=Series, fill=Value), show.legend = FALSE) +
  geom_tile(color="white", size=0.01) +
  scale_fill_manual(values = colours_map)+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#,
                    #labels = c('Global', 'Global & Regional', "None", 'Regional') )+
  scale_y_discrete(#as.character(1:nrow(priorities)),
                   labels= c("Scenario 1","Scenario 2","Scenario 3",
                             "Scenario 4","Scenario 5","Scenario 6",
                             "Scenario 7","Scenario 8","Scenario 9"))+
  scale_x_discrete(breaks = unique(data2$Index),#as.character(1:nrow(priorities)),
                   labels= unique(data2$Label))+#priorities$LEVEL3_N) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) #+
  # xlab("Botanical Country")# +
  # ylab("Scenario")

gg

ggsave(paste0(basepath, "scenario_matrix_1-9_top.pdf"),
       width = 20, height = 8, units = "cm")


## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=factor(Index), y=Series, fill=Value), show.legend = FALSE) +
  geom_tile(color="white", size=0.5) +
  scale_fill_manual(values = colours_map,#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"),
  labels = c('Global', 'Global & Continental', "None", 'Continental') )+
  scale_y_discrete(#as.character(1:nrow(priorities)),
    labels= c("Scenario 1","Scenario 2","Scenario 3",
              "Scenario 4","Scenario 5","Scenario 6",
              "Scenario 7","Scenario 8","Scenario 9"))+
  scale_x_discrete(breaks = unique(data2$Index),#as.character(1:nrow(priorities)),
                   labels= unique(data2$Label))+#priorities$LEVEL3_N) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        # legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) #+

gg


legend <- cowplot::get_legend(gg)
legend
ggsave(paste0(basepath, "matrix_legend.pdf"),
       width = 7, height = 5, units = "cm")



##########################################################################################
#############################################################################################
# Normalise data for manual bivariate map
##########################################################################################
##########################################################################################


#
# darkspots.prj$bnf_all = (darkspots.prj$bnf_1 +
#                            darkspots.prj$bnf_2 +
#                            darkspots.prj$bnf_3 +
#                            darkspots.prj$bnf_4 +
#                            darkspots.prj$bnf_5 +
#                            darkspots.prj$bnf_6 +
#                            darkspots.prj$bnf_7 +
#                            darkspots.prj$bnf_8 +
#                            darkspots.prj$bnf_9)
#
#
#
# darkspots.prj$bnf_all_norm = normalise(darkspots.prj$bnf_all)

# #####################################################################################################################
# # PLOT
# #######################################
#
# data = darkspots.prj
#
#
# # create map
# map10 <-  ggplot() +
#   geom_sf(data = data, mapping = aes(fill = bnf_all),
#           color = aes(fill = bnf_all),#NA,
#
#           size = 0.4#, show.legend = FALSE
#   ) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  bnf_all,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGnBu"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(7, "YlGnBu"))+
#   guides(color = "none",
#          fill=guide_legend(title="Summed benefit"), override.aes = list(size = 0.5)) +
#
#   # bi_theme() +
#   theme_bw() +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank(),
#         panel.border = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.text=element_text(size=8),
#         legend.title=element_text(size=10)
#   )
#
# map10
# #
# # legend <- cowplot::get_legend(map1)
# # legend
# ggsave(paste0(basepath, "maps_summed_benefit_YlGnBu.pdf"),
#        width = 30, height = 15, units = "cm")
#

