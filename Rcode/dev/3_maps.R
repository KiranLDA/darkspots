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
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
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
darkspots <- st_read("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp")



####  PROJECT A in ECKERT IV

PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sf_use_s2(FALSE)
m = st_buffer(darkspots, 0)
darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)






##########################################################################################
#############################################################################################
# Normalise data for manual bivariate map
##########################################################################################
##########################################################################################




darkspots.prj$drk_unprotect = darkspots.prj$benefit_4
darkspots.prj$drk = darkspots.prj$benefit_1
darkspots.prj$income = normalise(darkspots.prj$PC1)
darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
darkspots.prj$linnean_yrs = - normalise(darkspots.prj$discoveri1)
darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$descripti1)
darkspots.prj$linnean = darkspots.prj$SR_unknow0 # SR_unknown_norm
darkspots.prj$wallacean = darkspots.prj$SR_nogeol1 # SR_nogeoloc_norm #



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
                 style = "quantile", dim = dim)



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
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())




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




#############################################
# Protection + darkspot vs income
#############################################

dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft="#944c3b",#"#660000",#"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
                   upperright= "#f3e6b3",#"#FF9933",# BOTTOM LEFT   #rgb(255,230,15, maxColorValue=255),
                   bottomleft="#655e8a",# TOP RIGHT     #"brown2",#"black",#"grey",
                   bottomright= "#b4ace9"#"#8009a9" #"#9966FF"#"brown1"#rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




data <- bi_class(darkspots.prj,y=drk_unprotect, x=income,
                 style = "quantile", dim = dim)



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
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())




legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    xlab = "     Lower income",
                    ylab = "Less Protection & darkspot",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot




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
#                  style = "quantile", dim = dim)
#

data = darkspots.prj


# create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = discoveri3),
          color = aes(fill = discoveri3),#NA,
          size = 0.4, show.legend = FALSE) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  discoveri3,  #fill = bi_class,
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
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())
map


legend <- cowplot::get_legend(ggplot() +geom_sf(data = data, mapping = aes(fill = discoveri3),
                                      color = aes(fill = discoveri3),#NA,
                                      size = 0.4, show.legend = TRUE) +
                                geom_sf() +  #+
                                geom_point( data= data,
                                            aes(color =  discoveri3,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 2,
                                            stat = "sf_coordinates"
                                ) +
                                scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
                                scale_color_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG"))+
                                # scale_fill_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
                                # scale_color_gradient2(high = "darkgoldenrod", mid = "grey", low ="chocolate4", midpoint =1900)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
                                guides(color = "none",
                                       fill=guide_legend(title="Peak discovery year"),override.aes = list(size = 0.5)) +
                                bi_theme() +
                                theme(
                                  axis.title.y=element_blank(),
                                      axis.title.x=element_blank(),
                                      axis.text.y=element_blank(),
                                      axis.text.x=element_blank(),
                                  legend.text=element_text(size=8),
                                  legend.title=element_text(size=10))
                              )




# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot



#
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
# #                  style = "quantile", dim = dim)
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




