# load dependencies
library(biscale)
library(ggplot2)
library(cowplot)
library(sf)
require(tmap)
library(dplyr)
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

# x =  c'est ton vecteur de donnees (predictions) et y l'aire
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


# write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT = read.csv( "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
grid.DT <- data.table::as.data.table(grid.DT)
#####################################################################


##########################################################################################
#############################################################################################
# Normalise data for manual bivariate map
##########################################################################################
##########################################################################################

# values needed once (don't recalculate each time)
areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))


# darkspots.prj$drk_unprotect = darkspots.prj$benefit_4_S#darkspots.prj$bnf_4
# darkspots.prj$drk = darkspots.prj$benefit_1_S#darkspots.prj$bnf_1
darkspots.prj$income = normalise(darkspots.prj$PC1)
darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
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
RANK=5


##########################################################
# dpeak discovery year Linnear
#######################################

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
finalPlotl <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.9, .5, 0.24, 0.24)


finalPlotl
# ggsave(paste0(basepath, "skyline_linnean_map.pdf"), width = 30, height = 12, units = "cm")
# ggsave(paste0(basepath, "skyline_linnean_map.png"), bg="white", width = 30, height = 12, units = "cm")




# Windplot


data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "discoveries_max_year", "discoveries_y2010")] %>%
  arrange(LEVEL1_COD, discoveries_y2010)
colnames(data) = c("individual","group", "year","value")

# Set a number of 'empty bar' to add at the end of each group
empty_bar=1
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)

# data munging
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

# PLOT
pl = ggplot(data, aes(x=as.factor(id), y=value, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.9) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()#,

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

  geom_text(data=label_data, aes(x=id, y=value+0.001,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

pl



########################################
# peak description year Wallacean
#######################################


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
finalPlotw <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.9, .5, 0.24, 0.24)


finalPlotw


# Windplot


data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD","descriptions_max_year",  "descriptions_y2010")] %>%
  arrange(LEVEL1_COD, descriptions_y2010)
colnames(data) = c("individual","group", "year","value")


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


# plot

pw = ggplot(data, aes(x=as.factor(id), y=value, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.9) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
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
                              y1 = rep(max(data$value,na.rm=T)*3.5, 8),#rep(80, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.001,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )


pw



# ggsave(paste0(basepath, "skyline_linnean_roseplot.pdf"), width = 15, height = 15, units = "cm")





ggarrange(finalPlotl,
          pl ,
          finalPlotw ,
          pw , labels = c("a.", "b.", "c.", "d."),
          font.label = list(size = 30),
          ncol = 2, nrow = 2, widths = c(1, 0.5))

ggsave(paste0(basepath, "peak_year.pdf"),  width = 40, height = 30,  units = "cm")
ggsave(paste0(basepath, "peak_year.png"),  width = 40, height = 30,  units = "cm",bg="white")





#############################################
# Darkspots
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
  draw_plot(legend, 0.85, .7, 0.24, 0.24)


finalPlot

# ggsave(paste0(basepath, "time2event_darkspot_map.pdf"), width = 30, height = 12, units = "cm")
# ggsave(paste0(basepath, "time2event_darkspot_map.png"), bg="white", width = 30, height = 12, units = "cm")



# Windplot color darkspot/hotspot
bi_label <- bi_class(darkspots.prj,y=linnean, x=wallacean,
                     style = "fisher", dim = dim)

data = cbind(st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "shortfalls_norm_index")],
             bi_label$bi_class)  %>%
  arrange(LEVEL1_COD, shortfalls_norm_index)
colnames(data) = c("individual","group", "value", "color")


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


p = ggplot(data, aes(x=as.factor(id), y=value, fill=color)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=color), stat="identity", alpha=0.9) +
  # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
  bi_scale_fill(pal = custom_pal4, dim=dim)+
  # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
  # scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
  #ylim(-2,4) +
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
  #geom_hline(yintercept = seq(0,2,0.5), color = "gray90") +
  geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                              y1 = rep(max(data$value,na.rm=T)*3.5, 8),#rep(4, 8),
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

p




# ggsave(paste0(basepath, "time2event_darkspot_roseplot.pdf"), width = 15, height = 15, units = "cm")


ggarrange(finalPlot,
          p , labels = c("a.", "b."),
          font.label = list(size = 30),
          ncol = 2, nrow = 1, widths = c(1, 0.5))

ggsave(paste0(basepath, "darkspots.pdf"),  width = 40, height = 15,  units = "cm")
ggsave(paste0(basepath, "darkspots.png"),  width = 40, height = 15,   units = "cm",bg="white")






##########################
# uncertainty
##########################
data <- bi_class(darkspots.prj,y=linnean, x=wallacean,
                 style ="fisher",#"quantile",#"equal",# "jenks",# , "equal", "fisher"
                 dim = dim)
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
  draw_plot(legend, 0.85, .7, 0.24, 0.24)


finalPlot

# ggsave(paste0(basepath, "time2event_darkspot_map_sc.pdf"), width = 30, height = 12, units = "cm")
# ggsave(paste0(basepath, "time2event_darkspot_map_sc.png"), bg="white", width = 30, height = 12, units = "cm")





# windplot

bi_label <- bi_class(darkspots.prj,y=linnean_sc, x=wallacean_sc,
                     style = "fisher", dim = dim)


data = cbind(st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "shortfalls_norm_index_median_sc")], bi_label$bi_class) %>%
  arrange(LEVEL1_COD, shortfalls_norm_index_median_sc)
colnames(data) = c("individual","group", "value", "color")



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


# plot
p = ggplot(data, aes(x=as.factor(id), y=value, fill=color)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=color), stat="identity", alpha=0.9) +
  bi_scale_fill(pal = custom_pal4, dim=dim)+
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
                              y1 = rep(max(data$value,na.rm=T)*3.5, 8),#rep(4, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.001,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p
# ggsave(paste0(basepath, "time2event_darkspot_roseplot_sc.pdf"), width = 15, height = 15, units = "cm")



# ggsave(paste0(basepath, "time2event_darkspot_roseplot.pdf"), width = 15, height = 15, units = "cm")


ggarrange(finalPlot,
          p , labels = c("a.", "b."),
          font.label = list(size = 30),
          ncol = 2, nrow = 1, widths = c(1, 0.5))

ggsave(paste0(basepath, "darkspots_sc.pdf"),  width = 40, height = 15,  units = "cm")
ggsave(paste0(basepath, "darkspots_sc.png"),  width = 40, height = 15,   units = "cm",bg="white")








##########################
# uncertainty map
##########################


data <- bi_class(darkspots.prj,y=linnean_sc, x=wallacean_sc,
                 style ="fisher",#"quantile",#"equal",# "jenks",# , "equal", "fisher"
                 dim = dim)

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

