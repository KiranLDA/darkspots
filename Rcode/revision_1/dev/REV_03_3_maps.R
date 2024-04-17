# install.packages("biscale")
# ## install just cowplot and sf
# install.packages(c("cowplot", "sf"))
#
# ## install all suggested dependencies
# install.packages("biscale", dependencies = TRUE)


# load dependencies
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
require(tmap)
sf_use_s2(FALSE)

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


# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
# darkspots <- st_read(paste0(basepath, "/model_outputs.shp"))
darkspots <- tdwg3


####  PROJECT A in ECKERT IV

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sf_use_s2(FALSE)
m = st_buffer(darkspots, 0)
darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)


#
# #####################################################################
# # create bounding box
#
# # create a bounding box - world extent
# b.box <-st_bbox(c(xmin = -180,
#                   xmax = 180,
#                   ymin = -90,
#                   ymax = 90),
#                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#                 # as(raster::extent(-180, 180, -90, 90), "SpatialPolygons")
#
# # assign CRS to box
# # WGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#
# # proj4string(b.box) <- WGS84
#
# # create graticules/grid lines from box
# grid <- st_make_grid(b.box, n= c(20, 20),)
#
# # grid <- gridlines(b.box,
# #                   easts  = seq(from=-180, to=180, by=20),
# #                   norths = seq(from=-90, to=90, by=10))
#
# # give the PORJ.4 string for Eckert IV projection
# proj_eckert <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# grit.DT <- st_transform(grid, proj_eckert)
#
# #
# # # transform bounding box
# # grid.DT <- data.table::data.table(map_data(SpatialLinesDataFrame(sl=grid,
# #                                                                  data=data.frame(1:length(grid)),
# #                                                                  match.ID = FALSE)))
# # # assign matrix of projected coordinates as two columns in data table
# # grid.DT[, c("X","Y") := data.table::data.table(proj4::project(cbind(long, lat),
#                                                               # proj=proj_eckert))]
#
# #####################################################################

# create bounding box
# create a bounding box - world extent
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
# # # assign matrix of projected coordinates as two columns in data table
# grid.DT[, c("X","Y") := data.table::data.table(proj4::project(cbind(long, lat),
#                                                               proj=proj_eckert))]



# write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT = read.csv( "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT <- data.table::as.data.table(grid.DT)
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
# dpeak discovery year
#######################################

#
# dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft=rgb(0,150,235, maxColorValue=255),
#                    upperright= rgb(255,230,15, maxColorValue=255),
#                    bottomleft="black",#"grey",
#                    bottomright=rgb(130,0,80, maxColorValue=255)
# )
# custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

# data <- bi_class(darkspots.prj,y=drk, x=discoveri5,
#                  style = "fisher", dim = dim)
#

data = darkspots.prj


# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = descriptions_max_year), #dscrptns_1
          color = aes(fill = descriptions_max_year),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  descriptions_max_year,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
  # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  guides(color = "none") +
  bi_theme() +
  geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
                           |(long %in% c(-180,180) & lat %in% c(-90,90)
                             & region == "EW")],
            aes(x = X, y = Y, group = group),
            linetype = "solid", colour = "black", linewidth = .3) +
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
  # theme(axis.title.y=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.text.x=element_blank())
map


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = descriptions_max_year),
                                                color = aes(fill = descriptions_max_year),#NA,
                                                size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  descriptions_max_year,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"),
                                                     breaks=c(1800, 1825, 1850, 1875,
                                                              1900, 1925, 1950, 1975,
                                                              2000, 2010))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
                                # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Peak geolocation year"),
                                       override.aes = list(size = 0.5)) +
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
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .5, 0.24, 0.24)


finalPlot

ggsave(paste0(basepath, "skyline_wallacean_map.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(basepath, "skyline_wallacean_map.png"), bg="white", width = 30, height = 12, units = "cm")


#


##########################################################
# dpeak discovery year
#######################################

#
# dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft=rgb(0,150,235, maxColorValue=255),
#                    upperright= rgb(255,230,15, maxColorValue=255),
#                    bottomleft="black",#"grey",
#                    bottomright=rgb(130,0,80, maxColorValue=255)
# )
# custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

# data <- bi_class(darkspots.prj,y=drk, x=discoveri5,
#                  style = "fisher", dim = dim)
#

data = darkspots.prj


# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = discoveries_max_year),
          color = aes(fill = discoveries_max_year),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  discoveries_max_year,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
  # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
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
# theme(axis.title.y=element_blank(),
#       axis.title.x=element_blank(),
#       axis.text.y=element_blank(),
#       axis.text.x=element_blank())
map


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = discoveries_max_year),
                                                color = aes(fill = discoveries_max_year),#NA,
                                                size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  discoveries_max_year,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"),
                                                     breaks=c(1800, 1825, 1850, 1875,
                                                              1900, 1925, 1950, 1975,
                                                              2000, 2010))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
                                # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Peak description year"),
                                       override.aes = list(size = 0.5)) +
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
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .5, 0.24, 0.24)


finalPlot

ggsave(paste0(basepath, "skyline_linnean_map.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(basepath, "skyline_linnean_map.png"), bg="white", width = 30, height = 12, units = "cm")



#############################################
# Linnean vs wallacean
#############################################

dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




data <- bi_class(darkspots.prj,y=linnean, x=wallacean,
                 style ="fisher",#"quantile",#"equal",# "jenks",# , "equal", "fisher"
                 dim = dim)

data

# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = aes(fill = bi_class ),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  bi_class, #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_alpha_continuous(range = c(0.1, 1)) +
  bi_scale_fill(pal = custom_pal4, dim=dim)+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim)+#,flip_axes = TRUE, rotate_pal = TRUE) +
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

map


legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    xlab = "Geolocation gap",
                    ylab = "        Discovery gap",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot

ggsave(paste0(basepath, "time2event_darkspot_map.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(basepath, "time2event_darkspot_map.png"), bg="white", width = 30, height = 12, units = "cm")



##########################
# uncertainty
##########################

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = log(SR_unknown_sd)),
          color = aes(fill = log(SR_unknown_sd)),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  log(SR_unknown_sd),  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
  scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
  # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
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
# theme(axis.title.y=element_blank(),
#       axis.title.x=element_blank(),
#       axis.text.y=element_blank(),
#       axis.text.x=element_blank())
map



legend <- cowplot::get_legend(ggplot() +
                                geom_sf(data = data, mapping = aes(fill = log(SR_unknown_sd)),
                                        color = aes(fill = log(SR_unknown_sd)),#NA,
                                        size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  log(SR_unknown_sd),  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
                                scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
                                # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="log darkspot index sd"),override.aes = list(size = 0.5)) +
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
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot
ggsave(paste0(basepath, "time2event_darkspot_uncertainty_map.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(basepath, "time2event_darkspot_uncertainty_map.png"), bg="white", width = 30, height = 12, units = "cm")




###########################################################################################
###########################################################################################
# SCALED
###########################################################################################
###########################################################################################
#the scaling function
SAR <- function(x,y, ref_area_km2=10000){ # Species-Area-Regression
  # glm
  mod <- stats::glm(x ~ log(y), family=poisson(link="log"), start = c(0.5, 0.5))
  # non linear regression
  mod2 <- stats::nls(x ~ c*y^z, start=list(c=exp(coef(mod)[1]), z=coef(mod)[2]))
  z=coef(mod2)[2] # scaling area exponent
  (x*ref_area_km2^z)/(y^z)
}

areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))


#
#############################################
# Linnean vs wallacean scaled
#############################################

dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




data <- bi_class(darkspots.prj,y=linnean_sc, x=wallacean_sc,
                 style ="fisher",#"quantile",#"equal",# "jenks",# , "equal", "fisher"
                 dim = dim)



# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class),
          color = aes(fill = bi_class),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  bi_class,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  bi_scale_fill(pal = custom_pal4, dim=dim)+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = custom_pal4, dim=dim)+#,flip_axes = TRUE, rotate_pal = TRUE) +
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




legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    xlab = "Geolocation gap",
                    ylab = "        Discovery gap",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot

ggsave(paste0(basepath, "time2event_darkspot_map_sc.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(basepath, "time2event_darkspot_map_sc.png"), bg="white", width = 30, height = 12, units = "cm")


##########################
# uncertainty
##########################

map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = log(SR_unknown_sd_sc)),
          color = aes(fill = log(SR_unknown_sd_sc)),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  log(SR_unknown_sd_sc),  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
  scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
  # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
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
# theme(axis.title.y=element_blank(),
#       axis.title.x=element_blank(),
#       axis.text.y=element_blank(),
#       axis.text.x=element_blank())
map



legend <- cowplot::get_legend(ggplot() +
                                geom_sf(data = data, mapping = aes(fill = log(SR_unknown_sd_sc)),
                                        color = aes(fill = log(SR_unknown_sd_sc)),#NA,
                                        size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  log(SR_unknown_sd_sc),  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
                                scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "Greys")[4:7]))+
                                # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="log darkspot index sd"),override.aes = list(size = 0.5)) +
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
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot
ggsave(paste0(basepath, "time2event_darkspot_uncertainty_map_sc.pdf"), width = 30, height = 12, units = "cm")
ggsave(paste0(basepath, "time2event_darkspot_uncertainty_map_sc.png"), bg="white", width = 30, height = 12, units = "cm")


#
# #############################################
# # Protection + darkspot vs income
# #############################################
#
# dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft="#f99443",#"#944c3b",#"#660000",#"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
#                    upperright= "#fffde7",#"#f3e6b3",#"#FF9933",# BOTTOM LEFT   #rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#6a1b9a",#"#655e8a",# TOP RIGHT     #"brown2",#"black",#"grey",
#                    bottomright= "#f3e5f5"#"#b4ace9"#"#8009a9" #"#9966FF"#"brown1"#rgb(130,0,80, maxColorValue=255)
# )
# custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))
#
#
#
#
# data <- bi_class(darkspots.prj, y=drk_unprotect, x=income,
#                  style = "fisher", dim = dim)
#
#
#
# # create map
# map <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = bi_class),
#           color = aes(fill = bi_class),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  bi_class,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   bi_scale_fill(pal = custom_pal4, dim=dim)+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
#   bi_scale_color(pal = custom_pal4, dim=dim)+#,flip_axes = TRUE, rotate_pal = TRUE) +
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
#
#
#
# legend <- bi_legend(pal = custom_pal4,#"GrPink",
#                     dim = dim,
#                     xlab = "     Lower income",
#                     ylab = "Less Protection & darkspot",
#                     size = 8)
# # combine map with legend
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot
# ggsave(paste0(basepath, "prioritisation_map.pdf"), width = 30, height = 12, units = "cm")
#
#
#
# #############################################
# # Protection + darkspot vs income
# #############################################
#
# dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft="#f99443",#"#944c3b",#"#660000",#"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
#                    upperright= "#fffde7",#"#f3e6b3",#"#FF9933",# BOTTOM LEFT   #rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#6a1b9a",#"#655e8a",# TOP RIGHT     #"brown2",#"black",#"grey",
#                    bottomright= "#f3e5f5"#"#b4ace9"#"#8009a9" #"#9966FF"#"brown1"#rgb(130,0,80, maxColorValue=255)
# )
# custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))
#
#
#
#
# data <- bi_class(darkspots.prj,y=unprotect, x=income,
#                  style = "fisher", dim = dim)
#
#
#
# # create map
# map <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = bi_class),
#           color = aes(fill = bi_class),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  bi_class,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   bi_scale_fill(pal = custom_pal4, dim=dim)+#, flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
#   bi_scale_color(pal = custom_pal4, dim=dim)+#,flip_axes = TRUE, rotate_pal = TRUE) +
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
#
#
#
# legend <- bi_legend(pal = custom_pal4,#"GrPink",
#                     dim = dim,
#                     xlab = "        PC1: Lower income",
#                     ylab = "PC2: Less Protection",
#                     size = 8)
# # combine map with legend
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot
# ggsave(paste0(basepath, "PC1_PC2_map.pdf"), width = 30, height = 12, units = "cm")
#

# #######################################
# ##########################################################
# # darkspots
# #######################################
#
# #
# # dim=4
# # col.matrix<-colmat(nquantiles=dim,
# #                    upperleft=rgb(0,150,235, maxColorValue=255),
# #                    upperright= rgb(255,230,15, maxColorValue=255),
# #                    bottomleft="black",#"grey",
# #                    bottomright=rgb(130,0,80, maxColorValue=255)
# # )
# # custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# # names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))
#
# # data <- bi_class(darkspots.prj,y=drk, x=discoveri5,
# #                  style = "fisher", dim = dim)
# #
#
# data = darkspots.prj
#
#
# # create map
# map <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = benefit_5),
#           color = aes(fill = benefit_5),#NA,
#           size = 0.4, show.legend = FALSE) +
#   geom_sf() +  #+
#   geom_point( data= data,
#               aes(color =  benefit_5,  #fill = bi_class,
#                   geometry = geometry),
#               size = 2,
#               stat = "sf_coordinates"
#   ) +
#   scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
#   scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
#   # scale_fill_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#   # scale_color_gradient2(high = "#003333", mid = "brown", low ="#FF9999", midpoint =1920)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#   guides(color = "none") +
#   bi_theme() +
#   theme(axis.title.y=element_blank(),
#         axis.title.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.text.x=element_blank())
# map
#
#
# legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = benefit_5),
#                                                 color = aes(fill = benefit_5),#NA,
#                                                 size = 0.4, show.legend = TRUE) +
#                                 geom_sf() +  #+
#                                 geom_point( data= data,
#                                             aes(color =  benefit_5,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 2,
#                                             stat = "sf_coordinates"
#                                 ) +
#                                 scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
#                                 scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
#                                 # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
#                                 # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
#                                 guides(color = "none",
#                                        fill=guide_legend(title="Peak discovery year"),override.aes = list(size = 0.5)) +
#                                 bi_theme() +
#                                 theme(
#                                   axis.title.y=element_blank(),
#                                   axis.title.x=element_blank(),
#                                   axis.text.y=element_blank(),
#                                   axis.text.x=element_blank(),
#                                   legend.text=element_text(size=8),
#                                   legend.title=element_text(size=10))
# )
#
#
#
#
# # combine map with legend
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0.8, .7, 0.24, 0.24)
#
#
# finalPlot
#
#




