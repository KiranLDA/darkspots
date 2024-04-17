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



##############################################################################
##############################################################################
##  PLOT
##############################################################################
##############################################################################

#
#
# ################################################
# # Protection + darkspot vs income + darkspot
# #############################################
#
#
#
# data <- bi_class(darkspots, x=benefit_2, y=benefit_4,
#                  style = "quantile", dim = 4)
#
#
# # create map
# map <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = bi_class),
#           color = aes(fill = bi_class),#NA,
#           size = 10.4, show.legend = FALSE) +
#   bi_scale_fill(pal = "DkBlue2", dim = 4, rotate_pal = TRUE) + #"GrPink"
#   labs(
#     title = "Areas with higher data shortfalls",
#     subtitle = "Based on income group and protection status"
#   ) +
#   bi_theme()
#
#
#
# legend <- bi_legend(pal = "DkBlue2",
#                     dim = 4,
#                     xlab = "Lower Income group",
#                     ylab = "Less Protection",
#                     size = 9)
# # combine map with legend
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0.1, .2, 0.2, 0.2)
#
#
# finalPlot
#
#
#
# ##############################
# # PC1` and PC2
#
# data <- bi_class(darkspots, x=PC1, y=PC2, style = "quantile", dim = 3)
#
#
# # create map
# map <- ggplot() +
#   geom_sf(data = data, mapping = aes(fill = bi_class),
#           color = aes(fill = bi_class),#NA,
#           size = 10.4, show.legend = FALSE) +
#   bi_scale_fill(pal = "PurpleOr", dim=3) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
#   labs(
#     title = "PC1 vs PC2",
#     subtitle = "without accounting for darkspots"
#   ) +
#   bi_theme()
#
#
#
# legend <- bi_legend(pal = "PurpleOr",#"GrPink",
#                     dim = 3,
#                     xlab = "Lower Income group",
#                     ylab = "Less Protection",
#                     size = 8)
# # combine map with legend
# finalPlot <- ggdraw() +
#   draw_plot(map, 0, 0, 1, 1) +
#   draw_plot(legend, 0.1, .2, 0.2, 0.2)
#
#
# finalPlot
#


########################################################################################################################
########################################################################################################################
####  PROJECT AND PLOT in ECKERT IV
########################################################################################################################
########################################################################################################################


PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sf_use_s2(FALSE)
m = st_buffer(darkspots, 0)
darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
                                                  xmax = 180,
                                                  ymin = -90,
                                                  ymax = 90))),
                             crs = PROJ)



############################################################
# PC1` and PC2
############################################################

data <- bi_class(darkspots.prj, x=PC1, y=PC2, style = "quantile", dim = 3)


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
  bi_scale_fill(pal = "PurpleOr", dim=3) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = "PurpleOr", dim=3) +
  labs(
    title = "PC1 vs PC2",
    subtitle = "without accounting for darkspots"
  ) +
  guides(color = "none") +
  bi_theme() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())


legend <- bi_legend(pal = "PurpleOr",#"GrPink",
                    dim = 3,
                    xlab = "Lower Income group",
                    ylab = "Less Protection",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .2, 0.2, 0.2)


finalPlot



################################################
# Protection + darkspot vs income + darkspot
#############################################

data <- bi_class(darkspots.prj,x=benefit_2, y=benefit_4, style = "quantile", dim = 3)


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
  bi_scale_fill(pal = "PurpleOr", dim=3,  rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = "PurpleOr", dim=3,  rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())




legend <- bi_legend(pal = "PurpleOr",#"GrPink",
                    dim = 3,
                    xlab = "                Lower Income & darkspot",
                    ylab = "Less Protection & darkspot",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.005, .1, 0.2, 0.2)


finalPlot



################################################
# Protection + darkspot vs income
#############################################



data <- bi_class(darkspots.prj,x=benefit_4, y=PC1_norma0,
                 style = "quantile", dim = 4)


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
  bi_scale_fill(pal = "PurpleOr", dim=4,flip_axes = TRUE, rotate_pal = TRUE) + #"GrPink", dim = 3) +#, rotate_pal = TRUE) +
  bi_scale_color(pal = "PurpleOr", dim=4,flip_axes = TRUE, rotate_pal = TRUE) +
  guides(color = "none") +
  bi_theme() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank())




legend <- bi_legend(pal = "PurpleOr",#"GrPink",
                    dim = 4,
                    xlab = "     Higher income",
                    ylab = "Less Protection & darkspot",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.005, .1, 0.2, 0.2)


finalPlot


################################################
# darkspot unprotected vs income
#############################################


darkspots.prj$drk_unprotect = - darkspots.prj$benefit_4
darkspots.prj$drk = - darkspots.prj$benefit_1
darkspots.prj$income = normalise(darkspots.prj$PC1)
darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
darkspots.prj$linnean_yrs = - normalise(darkspots.prj$discoveri1)
darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$descripti1)



dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright= rgb(255,230,15, maxColorValue=255),
                   bottomleft="black",#"grey",
                   bottomright=rgb(130,0,80, maxColorValue=255)
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
  draw_plot(legend, 0, .7, 0.24, 0.24)


finalPlot







##########################################################
# darkspot vs income
##########################################################
dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright= rgb(255,230,15, maxColorValue=255),
                   bottomleft="black",#"grey",
                   bottomright=rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(darkspots.prj,y=drk, x=income,
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
                    ylab = "Darkspot",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, .7, 0.24, 0.24)


finalPlot





##########################################################
# darkspot vs unprotected
dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright= rgb(255,230,15, maxColorValue=255),
                   bottomleft="black",#"grey",
                   bottomright=rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(darkspots.prj,y=drk, x=unprotect,
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
                    xlab = "     less potection",
                    ylab = "Darkspot",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, .7, 0.24, 0.24)


finalPlot


##########################################################
# darkspot vs skyline
#######################################


dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright= rgb(255,230,15, maxColorValue=255),
                   bottomleft="black",#"grey",
                   bottomright=rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(darkspots.prj,y=drk, x=linnean_yrs,
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
                    xlab = "     Newer peak",
                    ylab = "Darkspot",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, .7, 0.24, 0.24)


finalPlot

#######################################


##########################################################
# darkspot vs collection
#######################################


dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright= rgb(255,230,15, maxColorValue=255),
                   bottomleft="black",#"grey",
                   bottomright=rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(darkspots.prj,y=drk, x=descripti5,
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
                    xlab = "     Descriptions",
                    ylab = "Darkspot",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, .7, 0.24, 0.24)


finalPlot

#######################################
##########################################################
# darkspot vs discoveriesa
#######################################


dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft=rgb(0,150,235, maxColorValue=255),
                   upperright= rgb(255,230,15, maxColorValue=255),
                   bottomleft="black",#"grey",
                   bottomright=rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))

data <- bi_class(darkspots.prj,y=drk, x=discoveri5,
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
                    xlab = "     Descriptions",
                    ylab = "Darkspot",
                    size = 10)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0, .7, 0.24, 0.24)


finalPlot


