
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
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,upperright))#bottomleft))
  my.pal.2<-findColours(my.class,c(bottomleft,bottomright))#upperright, bottomright))
  col.matrix<-matrix(nrow = 101, ncol = 101, NA)
  for(i in 1:101){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[102-i,]<-findColours(my.class,my.col)}
  # plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
  for(i in 1:101){
    col.temp<-col.matrix[i-1,]
    # points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
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



normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

#####################################################################

darkspots.prj$linnean_sky = normalise(darkspots.prj$dscv_2010)
darkspots.prj$wallacean_sky = normalise(darkspots.prj$dscr_2010)
darkspots.prj$linnean_tim2 = normalise(darkspots.prj$SR_nk_ / 28) # SR_unknown_norm
darkspots.prj$wallacean_tim2 = normalise(darkspots.prj$SR_ng_/28) # SR_nogeoloc_norm #

darkspots.prj$linnean_diff = darkspots.prj$linnean_tim2-darkspots.prj$linnean_sky
darkspots.prj$wallacean_diff = darkspots.prj$wallacean_tim2-darkspots.prj$wallacean_sky

#####################################################################################################################
# PLOT
#######################################

data = darkspots.prj

#Needed
# ate map
map_1 <-  ggplot() +
  geom_sf(data = data, mapping = aes(fill = linnean_tim2),
          color = aes(fill = linnean_tim2),#NA,

          size = 0.4#, show.legend = FALSE
  ) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  linnean_tim2,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(4, "Blues"))+
  # scale_color_gradientn(colours=RColorBrewer::brewer.pal(4, "Blues"))+
  scale_fill_gradient2(high = "royalblue1", mid = "grey95", low ="orange", midpoint =0)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  scale_color_gradient2(high = "royalblue1", mid = "grey95", low ="orange", midpoint =0)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  guides(color = "none",
         fill=guide_legend(title="Needed"), override.aes = list(size = 0.5)) +

  # bi_theme() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

map_1

#### happening
map_2 <-  ggplot() +
  geom_sf(data = data, mapping = aes(fill = linnean_sky),
          color = aes(fill = linnean_sky),#NA,
          size = 0.4#, show.legend = FALSE
  ) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  linnean_sky,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(4, "Oranges"))+
  # scale_color_gradientn(colours=RColorBrewer::brewer.pal(4, "Oranges"))+
  scale_fill_gradient2(high = "orange", mid = "grey95", low ="black", midpoint =0)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  scale_color_gradient2(high = "orange", mid = "grey95", low ="black", midpoint =0)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  guides(color = "none",
         fill=guide_legend(title="Happening"), override.aes = list(size = 0.5)) +

  # bi_theme() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

map_2




dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "royalblue1", #rgb(0,150,235, maxColorValue=255),
                   upperright= "olivedrab4", #bottom left #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="grey95", # top right #"black",
                   bottomright="orange"# brown3"#rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))



# "quantile", "equal", "fisher", and "jenks".
data <- bi_class(darkspots.prj, x=linnean_sky, y=linnean_tim2,
                 style = "fisher", dim = dim)



# create map
map_3 <- ggplot() +
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
  # bi_theme() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    ylab = "Still Needed",
                    xlab = "   Happening",
                    size = 10)
# combine map with legend
finalPlot_l <- ggdraw() +
  draw_plot(map_3, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot_l


ggarrange(map_1,map_2,map_3,
          ncol = 1, nrow = 3)

#
# ggsave(paste0(basepath, "maps_summed_benefit_YlGnBu.pdf"),
#        width = 30, height = 15, units = "cm")



################################################################
# wallacean_diff

map_4 <-  ggplot() +
  geom_sf(data = data, mapping = aes(fill = wallacean_tim2),
          color = aes(fill = wallacean_tim2),#NA,

          size = 0.4#, show.legend = FALSE
  ) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  wallacean_tim2,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(4, "Blues"))+
  # scale_color_gradientn(colours=RColorBrewer::brewer.pal(4, "Blues"))+
  scale_fill_gradient2(high = "brown", mid = "grey95", low ="orange", midpoint =0)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  scale_color_gradient2(high = "brown", mid = "grey95", low ="orange", midpoint =0)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  guides(color = "none",
         fill=guide_legend(title="Needed"), override.aes = list(size = 0.5)) +

  # bi_theme() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

map_4

#### happening
map_5 <-  ggplot() +
  geom_sf(data = data, mapping = aes(fill = wallacean_sky),
          color = aes(fill = wallacean_sky),#NA,
          size = 0.4#, show.legend = FALSE
  ) +
  geom_sf() +  #+
  geom_point( data= data,
              aes(color =  wallacean_sky,  #fill = bi_class,
                  geometry = geometry),
              size = 2,
              stat = "sf_coordinates"
  ) +
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(4, "Oranges"))+
  # scale_color_gradientn(colours=RColorBrewer::brewer.pal(4, "Oranges"))+
  scale_fill_gradient2(high = "orange", mid = "grey95", low ="black", midpoint =0)+#mid = "yellow", #use 2 for 3 scale , midpoint = 150
  scale_color_gradient2(high = "orange", mid = "grey95", low ="black", midpoint =0)+#, midpoint = .02 #mid = "yellow", , midpoint = 150
  guides(color = "none",
         fill=guide_legend(title="Happening"), override.aes = list(size = 0.5)) +

  # bi_theme() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

map_5







dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "brown", #rgb(0,150,235, maxColorValue=255),
                   upperright= "orangered3", #bottom left #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="grey95", # top right #"black",
                   bottomright="orange"# brown3"#rgb(130,0,80, maxColorValue=255)
)
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))



# "quantile", "equal", "fisher", and "jenks".
data <- bi_class(darkspots.prj, x=wallacean_sky, y=wallacean_tim2,
                 style = "fisher", dim = dim)



# create map
map_6 <- ggplot() +
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
  # bi_theme() +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text=element_text(size=8),
        legend.title=element_text(size=10)
  )

legend <- bi_legend(pal = custom_pal4,#"GrPink",
                    dim = dim,
                    ylab = "Still Needed",
                    xlab = "   Happening",
                    size = 10)
# combine map with legend
finalPlot_w <- ggdraw() +
  draw_plot(map_6, 0, 0, 1, 1) +
  draw_plot(legend, 0.8, .7, 0.24, 0.24)


finalPlot_w

ggarrange(finalPlot_l,finalPlot_w,ncol=1,nrow=2)
ggsave(paste0(basepath, "maps_happened_needed.pdf"),
       width = 30, height = 25, units = "cm")

