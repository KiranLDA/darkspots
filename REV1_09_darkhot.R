
# load dependencies
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
require(tmap)
library(classInt)
library(dplyr)
library(ggpubr)

# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/"
# load( file = paste0(basepath, "SOTWPF_version","REV_app_data.RData"))
load( file = paste0(basepath, "revision_version/","REV_app_data.RData"))


##############################################################################
##############################################################################
### FUNCTIONS
##############################################################################
##############################################################################


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

normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

SAR <- function(x,y, ref_area_km2=10000){ # Species-Area-Regression
  # glm
  mod <- stats::glm(x ~ log(y), family=poisson(link="log"), start = c(0.5, 0.5))
  # non linear regression
  mod2 <- stats::nls(x ~ c*y^z, start=list(c=exp(coef(mod)[1]), z=coef(mod)[2]))
  z=coef(mod2)[2] # scaling area exponent
  (x*ref_area_km2^z)/(y^z)
}



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


##########

# colours_map = c("#2a9bc1","#483737","#cccccc", "#84cfbb")



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
total_area = 24922455#24922455
areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "tdwg_shortfall_index_land_area.csv"))
colnames(areas) = c( "LEVEL3_COD", "area", "index")

darkspots.prj = darkspots.prj %>% left_join(areas[c( "LEVEL3_COD", "area")])


##################################################################################
# add hotspots
hotspots = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/shortfalls_not_rescaled_darkhot_28092023.csv")
hotspots = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/shortfalls_not_rescaled_darkhot_28092023.csv")

darkspots.prj = darkspots.prj %>% left_join(hotspots[c( "LEVEL3_COD", "hotspot")])
#
# darkspots.prj$darkspots = NA
# darkspots.prj$shortfalls_norm_index_median
#
# darkspots.prj$darkhot = NA



data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rank_scenario_1_S),#as.numeric(darkspots.prj$Rn__1),
                  as.numeric(darkspots.prj$area),
                  as.numeric(darkspots.prj$hotspot))

colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area", "hotspot")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_NAM)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$darkspot = ifelse(data2$cumul_area <= total_area, 1,
                           0)

colours_map = c("#800000","#ff6600",
                "#483737","#cccccc")
data2$darkhot = NA
data2$darkhot[which(data2$hotspot == 1 & data2$darkspot == 1 )] = 1
data2$darkhot[which(data2$hotspot == 1 & data2$darkspot == 0 )] = 2
data2$darkhot[which(data2$hotspot == 0 & data2$darkspot == 1 )] = 3
data2$darkhot[which(data2$hotspot == 0 & data2$darkspot == 0 )] = 4


data2$color_class = colours_map[data2$darkhot]
# data2$color_class_2 = colours_map[data2$darkhot]
# data2$color_class_2[data2$color_class_2 == 2] = 4

data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
data =  data %>% dplyr::arrange(LEVEL3_NAM)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_NAM)
darkspots.prj$color_class = data$color_class
# darkspots.prj$color_class_2 = data$color_class_2

###################################################
# CROP

# hotspots = st_read(dsn = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/hotspots_2016_1/",
#                    layer = "hotspots_2016_1")
hotspots = st_read(dsn = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/Biodiversity_Hotspots_2016/",
                   layer = "Biodiversity_Hotspots_2016")


#
# library(dplyr)
#
# with_holes <- hotspots |>
#   st_union()
# ext_ring <- st_polygon(with_holes[[1]][1]) |> st_sfc(crs = st_crs(hotspots))
#
# par(mfrow=c(1,2))
# plot(with_holes)
# plot(ext_ring)
#
#
hotspots = st_make_valid(hotspots)
#
PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# m = st_buffer(hotspots , 0)
hotspots.prj = st_transform(hotspots,
                            crs = st_crs(PROJ))#darkspots.prj))



# plot(hotspots.prj)
hotspots.prj.union = hotspots.prj %>%
  group_by(OBJECTID) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

hotspots.prj.union = st_make_valid(hotspots.prj.union)

dark_hotspot = st_intersection(darkspots.prj, hotspots.prj.union)


data = darkspots.prj
data$color_class_2 = data$color_class
data$color_class_2[data$color_class_2=="#ff6600"] = "#cccccc"
# "no" lines
map <- ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  geom_sf(data = data,
          mapping = aes(fill = color_class_2),
          color = aes(fill = color_class),#NA, #"black",#
          size = 0.005, show.legend = FALSE
  ) +
  geom_sf(data = dark_hotspot,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA, #"black",#
          size = 0.005, show.legend = FALSE
  ) +
  # geom_sf(data = data, lwd = 0.005, fill = NA, colour ="black") +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +

  scale_fill_manual(values =  c("#483737", "#800000",
                                "#cccccc","#ff6600"),
                    labels = c('Global', 'Global & Regional', "None", 'Regional'),
                    na.value="#cccccc")+ #"#328380",
  scale_color_manual(values = c("#483737", "#800000",
                                "#cccccc","#ff6600"),
                     na.value="#cccccc")+
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


map
ggsave(paste0(basepath, "revision_version/","darkhot_nolines.png"),  width = 40, height = 30, units = "cm",bg="white")
ggsave(paste0(basepath,"revision_version/", "darkhot_nolines.pdf"),  width = 40, height = 30, units = "cm")


#with lines
map + geom_sf(data = data, lwd = 0.1, fill = NA, colour ="black")
ggsave(paste0(basepath, "revision_version/","darkhot_lines.png"),  width = 40, height = 30, units = "cm",bg="white")
ggsave(paste0(basepath,"revision_version/", "darkhot_lines.pdf"),  width = 40, height = 30, units = "cm")




####################################################
# SCALED



data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
                  as.numeric(darkspots.prj$LEVEL1),
                  as.numeric(darkspots.prj$Rank_scenario_1_S_sc),#as.numeric(darkspots.prj$Rn__1),
                  as.numeric(darkspots.prj$area),
                  as.numeric(darkspots.prj$hotspot))

colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area", "hotspot")
data = data %>% dplyr::arrange(global_rank)
data$cumul_area = cumsum(data$area)
data$cumul_area[is.na(data$global_rank)] = NA
data =  data %>% dplyr::arrange(LEVEL3_NAM)

data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
  dplyr::group_by(LEVEL1) %>%
  dplyr::mutate(regional_rank = rank(global_rank))


data2$darkspot = ifelse(data2$cumul_area <= total_area, 1,
                        0)

colours_map = c("#800000","#ff6600",
                "#483737","#cccccc")
data2$darkhot = NA
data2$darkhot[which(data2$hotspot == 1 & data2$darkspot == 1 )] = 1
data2$darkhot[which(data2$hotspot == 1 & data2$darkspot == 0 )] = 2
data2$darkhot[which(data2$hotspot == 0 & data2$darkspot == 1 )] = 3
data2$darkhot[which(data2$hotspot == 0 & data2$darkspot == 0 )] = 4


data2$color_class = colours_map[data2$darkhot]


data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
data =  data %>% dplyr::arrange(LEVEL3_NAM)
data =  data %>% left_join(data2)
data =  data %>% dplyr::arrange(LEVEL3_NAM)
darkspots.prj$color_class = data$color_class



# CROP

# hotspots = st_read(dsn = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/hotspots_2016_1/",
#                    layer = "hotspots_2016_1")
hotspots = st_read(dsn = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/Biodiversity_Hotspots_2016/",
                   layer = "Biodiversity_Hotspots_2016")


#
# library(dplyr)
#
# with_holes <- hotspots |>
#   st_union()
# ext_ring <- st_polygon(with_holes[[1]][1]) |> st_sfc(crs = st_crs(hotspots))
#
# par(mfrow=c(1,2))
# plot(with_holes)
# plot(ext_ring)
#
#
hotspots = st_make_valid(hotspots)
#
PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# m = st_buffer(hotspots , 0)
hotspots.prj = st_transform(hotspots,
                            crs = st_crs(PROJ))#darkspots.prj))



# plot(hotspots.prj)
hotspots.prj.union = hotspots.prj %>%
  group_by(OBJECTID) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

hotspots.prj.union = st_make_valid(hotspots.prj.union)

dark_hotspot = st_intersection(darkspots.prj, hotspots.prj.union)


data = darkspots.prj
data$color_class_2 = data$color_class
data$color_class_2[data$color_class_2=="#ff6600"] = "#cccccc"

# "no" lines
map <- ggplot() +
  geom_point( data= data,
              aes(color =  color_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates", show.legend = FALSE
  ) +
  scale_color_manual(values = c("#483737", "#800000",
                                "#cccccc","#ff6600"),
                     na.value="#cccccc")+
  geom_sf(data = data,
          mapping = aes(fill = color_class_2),
          color = aes(fill = color_class),#NA, #"black",#
          size = 0.005, show.legend = FALSE
  ) +
  scale_fill_manual(values =  c("#483737", "#800000",
                                "#cccccc","#ff6600"),
                    labels = c('Global', 'Global & Regional', "None", 'Regional'),
                    na.value="#cccccc")+ #"#328380",
  geom_sf(data = dark_hotspot,
          mapping = aes(fill = color_class),
          color = aes(fill = color_class),#NA, #"black",#
          size = 0.005, show.legend = FALSE
  ) +
  # scale_fill_manual(values =  c("#483737", "#630000",
  #                               "#cccccc","#ff6600"),
  #                   labels = c('Global', 'Global & Regional', "None", 'Regional'),
  #                   na.value="#cccccc")+ #"#328380",
  # # geom_sf(data = data, lwd = 0.005, fill = NA, colour ="black") +
  # geom_sf() +  #+
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +



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


map
ggsave(paste0(basepath, "revision_version/","darkhot_nolines_sc.png"),  width = 40, height = 30, units = "cm",bg="white")
ggsave(paste0(basepath,"revision_version/", "darkhot_nolines_sc.pdf"),  width = 40, height = 30, units = "cm")


#with lines
map + geom_sf(data = data, lwd = 0.1, fill = NA, colour ="black")
ggsave(paste0(basepath, "revision_version/","darkhot_lines_sc.png"),  width = 40, height = 30, units = "cm",bg="white")
ggsave(paste0(basepath,"revision_version/", "darkhot_lines_sc.pdf"),  width = 40, height = 30, units = "cm")

library(ggpattern)
map + geom_sf_pattern(data = hotspots.prj.union,                # supply map sf object
                      pattern = "crosshatch",#"stripe",
                      pattern_fill = "gray90", pattern_colour = "gray90",
                      pattern_spacing = 0.02,
                      fill = NA)
                      # aes(pattern = hotspot, pattern_fill = hotspot),  # map aesthetics
                      # pattern_color = "NA", pattern_density = 0.5)














####################################
#
# data = darkspots.prj
# ggplot() +
#   geom_point( data= data,
#               aes(color =  color_class,  #fill = bi_class,
#                   geometry = geometry),
#               size = 0.2,
#               stat = "sf_coordinates", show.legend = FALSE
#   ) +
#   geom_sf(data = data,
#           mapping = aes(fill = color_class),
#           color = aes(fill = color_class),#NA,
#           size = 0.01, show.legend = FALSE
#   ) +
#   # geom_sf() +  #+
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", linewidth = .3) +
#
#   scale_fill_manual(values =  c("#483737", "#800000",
#                                 "#cccccc","#ff6600"),
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values = c("#483737", "#800000",
#                                 "#cccccc","#ff6600"))+
#   guides(color = "none",
#          fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +
#
#   # bi_theme() +
#   theme_bw() +
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
#
#
#
# ##################################
# #import shp for hotspots
#
# hotspots = st_read(dsn = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/hotspots_2016_1/",
#                    layer = "hotspots_2016_1")
#
#
#
# library(dplyr)
#
# with_holes <- hotspots |>
#   st_union()
# ext_ring <- st_polygon(with_holes[[1]][1]) |> st_sfc(crs = st_crs(hotspots))
#
# par(mfrow=c(1,2))
# plot(with_holes)
# plot(ext_ring)
#
#
# hotspots = st_make_valid(hotspots)
#
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# # m = st_buffer(hotspots , 0)
# hotspots.prj = st_transform(hotspots,
#                              crs = st_crs(PROJ))#darkspots.prj))
#
#
#
# plot(hotspots.prj)
#
#
#
# dark_hotspot = st_intersection(darkspots.prj, hotspots.prj)
#
#
#
# ################################################
# #   PLOT all
# ###############################################
# # scenario = 1
# # darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S")])
# # darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("Rank_scenario_",scenario,"_S")])
# #
#
#
# # data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
# #                   as.numeric(darkspots.prj$LEVEL1),
# #                   as.numeric(darkspots.prj$rank),#as.numeric(darkspots.prj$Rn__1),
# #                   as.numeric(darkspots.prj$area))
# priorities = data.frame(LEVEL3_NAM=darkspots.prj$LEVEL3_COD)
#
# # scenario = 1
# for (scenario in 1:9){
#
#   darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S")])
#   darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("Rank_scenario_",scenario,"_S")])
#
#
#   data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                     as.numeric(darkspots.prj$LEVEL1),
#                     as.numeric(darkspots.prj$rank),#as.numeric(darkspots.prj$Rn__1),
#                     as.numeric(darkspots.prj$area))
#
#   colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
#   data = data %>% dplyr::arrange(global_rank)
#   data$cumul_area = cumsum(data$area)
#   data$cumul_area[is.na(data$global_rank)] = NA
#   data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
#   data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
#   data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#     dplyr::group_by(LEVEL1) %>%
#     dplyr::mutate(regional_rank = rank(global_rank))
#
#
#   data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                              "grey")
#   data2$color_class[which(data2$regional_rank <=5)] = "red"
#   data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
#   data2$color_class[is.na(data2$color_class)] = "grey"
#   data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
#   data =  data %>% dplyr::arrange(LEVEL3_NAM)
#   data =  data %>% left_join(data2)
#   data =  data %>% dplyr::arrange(LEVEL3_NAM)
#   darkspots.prj$color_class = data$color_class
#   data = data[which(data$color_class != "grey"),]
#   to_add = data.frame(cbind(data$LEVEL3_NAM,
#                             data$color_class))
#   colnames(to_add) = c("LEVEL3_NAM",paste0("S",scenario))
#   priorities = base::merge(priorities,
#                            to_add,
#                            all=TRUE)
#   data = darkspots.prj
#   map <-  ggplot() +
#     geom_point( data= data,
#                 aes(color =  color_class,  #fill = bi_class,
#                     geometry = geometry),
#                 size = 0.2,
#                 stat = "sf_coordinates", show.legend = FALSE
#     ) +
#     geom_sf(data = data,
#             mapping = aes(fill = color_class),
#             color = aes(fill = color_class),#NA,
#             size = 0.01, show.legend = FALSE
#     ) +
#     # geom_sf() +  #+
#     geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                              |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                & region == "EW")],
#               aes(x = X, y = Y, group = group),
#               linetype = "solid", colour = "black", linewidth = .3) +
#
#     scale_fill_manual(values =   colours_map,
#                       labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#     scale_color_manual(values =  colours_map)+
#     guides(color = "none",
#            fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +
#
#     # bi_theme() +
#     theme_bw() +
#     theme(axis.title.y=element_blank(),
#           axis.title.x=element_blank(),
#           axis.text.y=element_blank(),
#           axis.text.x=element_blank(),
#           panel.border = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.grid.major = element_blank(),
#           legend.text=element_text(size=8),
#           legend.title=element_text(size=10)
#     )
#
#   # create map
#   assign(paste0("map",scenario) , map)
# }
#
#
# legend <- cowplot::get_legend(ggplot() +
#                                 geom_point( data= data,
#                                             aes(color =  color_class,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 0.2,
#                                             stat = "sf_coordinates") +
#                                 geom_sf(data = data,
#                                         mapping = aes(fill = color_class),
#                                         color = aes(fill = color_class),#NA,
#                                         size = 0.01#, show.legend = FALSE
#                                 ) +
#                                 # geom_sf() +  #+
#                                 geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                                                          |(long %in% c(-180,180) & lat %in% c(-90,90)
#                                                            & region == "EW")],
#                                           aes(x = X, y = Y, group = group),
#                                           linetype = "solid", colour = "black", size = .3) +
#
#                                 scale_fill_manual(values =   colours_map,
#                                                   labels = c('Darkspot',
#                                                              'Darkspot & regional priority', "None", 'Regional priority only'))+#, na.value="white")+ #"#328380",
#                                 scale_color_manual(values =  colours_map)+
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Priority"), override.aes = list(size = 0)) +
#
#                                 # bi_theme() +
#                                 theme_bw() +
#                                 theme(axis.title.y=element_blank(),
#                                       axis.title.x=element_blank(),
#                                       axis.text.y=element_blank(),
#                                       axis.text.x=element_blank(),
#                                       panel.border = element_blank(),
#                                       panel.grid.minor = element_blank(),
#                                       panel.grid.major = element_blank(),
#                                       legend.text=element_text(size=8),
#                                       legend.title=element_text(size=10)
#                                 ))
#
# ggarrange(map1,map2,map3,
#           map4,map6,map8,
#           map5, map7,map9,
#           ncol = 3, nrow = 3)
#
#
#
# ggsave(paste0(basepath, "maps_1-9_top.pdf"),
#        width = 80, height = 50, units = "cm")
#
