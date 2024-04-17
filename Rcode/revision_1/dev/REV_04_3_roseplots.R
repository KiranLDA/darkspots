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


# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
# basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"


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
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/"
load( file = paste0(basepath, "REV_app_data.RData"))
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
# # create bounding box
# # create a bounding box - world extent
# b.box <-st_bbox(c(xmin = -180,
#                   xmax = 180,
#                   ymin = -90,
#                   ymax = 90),
#                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#
# # create graticules/grid lines from box
# grid <- st_make_grid(b.box, n= c(20, 20),)
#
#
# # give the PORJ.4 string for Eckert IV projection
# proj_eckert <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# grit.DT <- st_transform(grid, proj_eckert)
#

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


# darkspots.prj$drk_unprotect = darkspots.prj$bnf_4
# darkspots.prj$drk = darkspots.prj$bnf_1
# darkspots.prj$income = normalise(darkspots.prj$PC1)
# darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
# darkspots.prj$linnean_yrs = - normalise(darkspots.prj$dscvrs_t_)
# darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$dscrptns_t)
# darkspots.prj$linnean = darkspots.prj$SR_nk_ # SR_unknown_norm
# darkspots.prj$wallacean = darkspots.prj$SR_ng_ # SR_nogeoloc_norm #


# basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"

# load(paste0(basepath, "REVISION_1/app_data.RData"))

library(dplyr)
# library(plyr)
# detach("package:plyr", unload=TRUE)

RANK=5

###########################################################
#### Past Discoveries
################################################################



data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "discoveries_max_year", "discoveries_y2010")] %>%
  arrange(LEVEL1_COD, discoveries_y2010)
colnames(data) = c("individual","group", "year","value")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



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
# data=data %>% arrange(group) #ooriginal

# data=data %>% arrange(group,value) %>%
#   group_by(group) %>%
#   mutate(rank = rank(desc(value), ties.method = "first"))

# data=data %>% arrange(group,value) %>%
#     # group_by(group) %>%
#     mutate(rank = rank(desc(value)))


data = data %>% dplyr::arrange(group) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(rank = rank(-value))
# data=data %>% dplyr::group_by(group) %>%
#   dplyr::arrange(value) %>%
#   dplyr::mutate(rank = rank(-value))
data = data %>% dplyr::filter(rank <= RANK)
# data = data[, c("individual", "group", "value")]

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
p = ggplot(data, aes(x=as.factor(id), y=value, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.9) +
  # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
  # scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
  #ylim(-0.1,0.2) +
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
  #geom_hline(yintercept = seq(0,0.15,0.05), color = "gray90") +
  geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                              y1 = rep(max(data$value,na.rm=T)*3.5, 8),
    #x1 = seq((RANK/2),dim(data)[1],RANK),
                              #y1 = rep(0.2, 8),
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
ggsave(paste0(basepath, "skyline_linnean_roseplot.pdf"), width = 15, height = 15, units = "cm")


###########################################################
#### Past geolocations
################################################################



data = st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD","descriptions_max_year",  "descriptions_y2010")] %>%
  arrange(LEVEL1_COD, descriptions_y2010)
colnames(data) = c("individual","group", "year","value")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



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
# data=data %>% arrange(group) #ooriginal

# data=data %>% arrange(group,value) %>%
#   group_by(group) %>%
#   mutate(rank = rank(desc(value), ties.method = "first"))

# data=data %>% arrange(group,value) %>%
#     # group_by(group) %>%
#     mutate(rank = rank(desc(value)))


data = data %>% dplyr::arrange(group) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(rank = rank(-value))
# data=data %>% dplyr::group_by(group) %>%
#   dplyr::arrange(value) %>%
#   dplyr::mutate(rank = rank(-value))
data = data %>% dplyr::filter(rank <= RANK)
# data = data[, c("individual", "group", "value")]

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
p = ggplot(data, aes(x=as.factor(id), y=value, fill=year)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=year), stat="identity", alpha=0.9) +
  # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
  # scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
  #ylim(-10,80) +
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
  #geom_hline(yintercept = seq(0,120,10), color = "gray90") +
  geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
                              y1 = rep(max(data$value,na.rm=T)*3.5, 8),#rep(80, 8),
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
ggsave(paste0(basepath, "skyline_wallacean_roseplot.pdf"), width = 15, height = 15, units = "cm")




################################################################################################################################
###########################################################################################################################



################################################################################################################################
###########################################################################################################################

#### Biggest darkspots

################################################################################################################################
################################################################################################################################



dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft=rgb(0,150,235, maxColorValue=255),
#                    upperright="grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="black",
#                    bottomright="brown3"#rgb(130,0,80, maxColorValue=255)
# )
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




bi_label <- bi_class(darkspots.prj,y=linnean, x=wallacean,
                     style = "fisher", dim = dim)


data = cbind(st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "shortfalls_norm_index")], bi_label$bi_class) %>%
  arrange(LEVEL1_COD, shortfalls_norm_index)
colnames(data) = c("individual","group", "value", "color")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-\nTemperate",
                                        "Asia-Tropical",
                                        "Australasia",
                                        "Pacific",
                                        "North     \nAmerica     ",
                                        "South    \nAmerica",
                                        "Antarctic"))



# Set a number of 'empty bar' to add at the end of each group
empty_bar=1
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
# data=data %>% arrange(group) #ooriginal

# data=data %>% arrange(group,value) %>%
#   group_by(group) %>%
#   mutate(rank = rank(desc(value), ties.method = "first"))

# data=data %>% arrange(group,value) %>%
#     # group_by(group) %>%
#     mutate(rank = rank(desc(value)))


data = data %>% dplyr::arrange(group) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(rank = rank(-value))
# data=data %>% dplyr::group_by(group) %>%
#   dplyr::arrange(value) %>%
#   dplyr::mutate(rank = rank(-value))
data = data %>% dplyr::filter(rank <= RANK)
# data = data[, c("individual", "group", "value")]

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
# Windplot color darkspot/hotspot
#####################################
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
ggsave(paste0(basepath, "time2event_darkspot_roseplot.pdf"), width = 15, height = 15, units = "cm")

################################################################################################################################
################################################################################################################################

#### SCALED darkspots

################################################################################################################################
################################################################################################################################



dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft= "#6eabbd", #rgb(0,150,235, maxColorValue=255),
                   upperright= "#e8e8e8",  #"grey",# rgb(255,230,15, maxColorValue=255),
                   bottomleft="#554249", #"black",
                   bottomright="#c15e5c"# brown3"#rgb(130,0,80, maxColorValue=255)
)
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft=rgb(0,150,235, maxColorValue=255),
#                    upperright="grey",# rgb(255,230,15, maxColorValue=255),
#                    bottomleft="black",
#                    bottomright="brown3"#rgb(130,0,80, maxColorValue=255)
# )
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




bi_label <- bi_class(darkspots.prj,y=linnean_sc, x=wallacean_sc,
                     style = "fisher", dim = dim)


data = cbind(st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "shortfalls_lognorm_index_sc")], bi_label$bi_class) %>%
  arrange(LEVEL1_COD, shortfalls_lognorm_index_sc)
colnames(data) = c("individual","group", "value", "color")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-\nTemperate",
                                        "Asia-Tropical",
                                        "Australasia",
                                        "Pacific",
                                        "North     \nAmerica     ",
                                        "South    \nAmerica",
                                        "Antarctic"))



# Set a number of 'empty bar' to add at the end of each group
empty_bar=1
to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) = colnames(data)
to_add$group=rep(levels(data$group), each=empty_bar)
data=rbind(data, to_add)
# data=data %>% arrange(group) #ooriginal

# data=data %>% arrange(group,value) %>%
#   group_by(group) %>%
#   mutate(rank = rank(desc(value), ties.method = "first"))

# data=data %>% arrange(group,value) %>%
#     # group_by(group) %>%
#     mutate(rank = rank(desc(value)))


data = data %>% dplyr::arrange(group) %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(rank = rank(-value))
# data=data %>% dplyr::group_by(group) %>%
#   dplyr::arrange(value) %>%
#   dplyr::mutate(rank = rank(-value))
data = data %>% dplyr::filter(rank <= RANK)
# data = data[, c("individual", "group", "value")]

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
# Windplot color darkspot/hotspot
#####################################
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
ggsave(paste0(basepath, "time2event_darkspot_roseplot_sc.pdf"), width = 15, height = 15, units = "cm")




################################################################################################################################
###########################################################################################################################

#### SDGs

################################################################################################################################
################################################################################################################################

#
#
#
# dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft="#f99443",#"#944c3b",#"#660000",#"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
#                    upperright= "#fffde7",#"#f3e6b3",#"#FF9933",# BOTTOM LEFT   #rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#6a1b9a",#"#655e8a",# TOP RIGHT     #"brown2",#"black",#"grey",
#                    bottomright= "#f3e5f5"#"#b4ace9"#"#8009a9" #"#9966FF"#"brown1"#rgb(130,0,80, maxColorValue=255)
# )
#
# #
# # col.matrix<-colmat(nquantiles=dim,
# #                    upperleft ="#660000", # dark red"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
# #                    upperright = "#FF9933",# orange#" #BOTTOM LEFT   "#FF9933" #rgb(255,230,15, maxColorValue=255),
# #                    bottomleft =  "#330066", # dark purple # TOP RIGHT  #"#330066"   #"brown2",#"black",#"grey",
# #                    bottomright =  "#9966FF" #purple "brown1"#rgb(130,0,80, maxColorValue=255)
# # )
# custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))
#
#
#
# bi_label <- bi_class(darkspots.prj, y=drk_unprotect, x=income,
#                      style = "fisher", dim = dim)
#
#
#
#
# data = cbind(st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_4_S")], bi_label$bi_class) %>%
#   arrange(LEVEL1_COD, benefit_4_S)
# colnames(data) = c("individual","group", "value", "color")
# # data[is.na(data$group),"group"]= "Other"
# # data[is.na(data$value),"value"]= 1
# # data$value = log(data$value)
#
#
#
# tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
#                          LEVEL1_NAM = c("Europe",
#                                         "Africa",
#                                         "Asia-\nTemperate",
#                                         "Asia-Tropical",
#                                         "Australasia",
#                                         "Pacific",
#                                         "North          \nAmerica        ",
#                                         "South America",
#                                         "Antarctic"))
#
#
#
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar=1
# to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
# colnames(to_add) = colnames(data)
# to_add$group=rep(levels(data$group), each=empty_bar)
# data=rbind(data, to_add)
# # data=data %>% arrange(group) #ooriginal
#
# # data=data %>% arrange(group,value) %>%
# #   group_by(group) %>%
# #   mutate(rank = rank(desc(value), ties.method = "first"))
#
# # data=data %>% arrange(group,value) %>%
# #     # group_by(group) %>%
# #     mutate(rank = rank(desc(value)))
#
#
# data = data %>% dplyr::arrange(group) %>%
#   dplyr::group_by(group) %>%
#   dplyr::mutate(rank = rank(-value))
# # data=data %>% dplyr::group_by(group) %>%
# #   dplyr::arrange(value) %>%
# #   dplyr::mutate(rank = rank(-value))
# data = data %>% dplyr::filter(rank <= RANK)
# # data = data[, c("individual", "group", "value")]
#
# data$id=seq(1, nrow(data))
# data$label = NA
# for (gp in 1:9){
#   data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
# }
# #remmove antarctic
# data= data[data$group<9,]
#
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
#
#
# #####################################
# # Windplot color darkspot/hotspot
# #####################################
# p = ggplot(data, aes(x=as.factor(id), y=value, fill=color)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=color), stat="identity", alpha=0.9) +
#   # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
#   bi_scale_fill(pal = custom_pal4, dim=dim)+
#   # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
#   # scale_fill_brewer(palette = "RdBu") +
#   # ylim(-10,9) +#120) +
#   ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
#   #ylim(-2,5) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(),#element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()#,
#     # plot.margin = unit(rep(1,4), "cm")
#
#   ) +
#   geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
#                               max(data$value,na.rm=T)/5), color = "gray90") +
#   #geom_hline(yintercept = seq(0,2,0.5), color = "gray90") +
#   geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
#   coord_polar() +
#   geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
#                               y1 = rep(max(data$value,na.rm=T)*3.5, 8),#rep(5, 8),
#                               label = tdwg1_names$LEVEL1_NAM[1:8]),
#             aes(x=x1, y=y1, label = label),
#             color="black", fontface="bold",alpha=0.9, size=4,
#             # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
#             #               label_data$angle[seq(5,81, 10)]+90),
#             # hjust=0.2,
#             inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
#   ) +
#
#   geom_text(data=label_data, aes(x=id, y=value+0.01,#3000,#
#                                  label=individual, hjust=hjust),
#             color="black", fontface="bold",alpha=0.7, size=3,
#             angle= label_data$angle, inherit.aes = FALSE )
#
# p
# ggsave(paste0(basepath, "prioritisation_roseplot.pdf"), width = 15, height = 15, units = "cm")
#
# ################################################################################################################################
# # PC1 vs PC2
#
# ################################################################################################################################
#
#
# dim=4
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft="#f99443",#"#944c3b",#"#660000",#"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
#                    upperright= "#fffde7",#"#f3e6b3",#"#FF9933",# BOTTOM LEFT   #rgb(255,230,15, maxColorValue=255),
#                    bottomleft="#6a1b9a",#"#655e8a",# TOP RIGHT     #"brown2",#"black",#"grey",
#                    bottomright= "#f3e5f5"#"#b4ace9"#"#8009a9" #"#9966FF"#"brown1"#rgb(130,0,80, maxColorValue=255)
# )
#
# #
# # col.matrix<-colmat(nquantiles=dim,
# #                    upperleft ="#660000", # dark red"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
# #                    upperright = "#FF9933",# orange#" #BOTTOM LEFT   "#FF9933" #rgb(255,230,15, maxColorValue=255),
# #                    bottomleft =  "#330066", # dark purple # TOP RIGHT  #"#330066"   #"brown2",#"black",#"grey",
# #                    bottomright =  "#9966FF" #purple "brown1"#rgb(130,0,80, maxColorValue=255)
# # )
# custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
# names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))
#
#
#
# bi_label <- bi_class(darkspots.prj, y=unprotect, x=income,
#                      style = "fisher", dim = dim)
#
#
#
#
# data = cbind(st_drop_geometry(tdwg3)[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_4_S")], bi_label$bi_class) %>%
#   arrange(LEVEL1_COD, benefit_4_S)
# colnames(data) = c("individual","group", "value", "color")
# # data[is.na(data$group),"group"]= "Other"
# # data[is.na(data$value),"value"]= 1
# # data$value = log(data$value)
#
#
#
# tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
#                          LEVEL1_NAM = c("Europe",
#                                         "Africa",
#                                         "Asia-\nTemperate",
#                                         "Asia-Tropical",
#                                         "Australasia",
#                                         "Pacific",
#                                         "North          \nAmerica        ",
#                                         "South America",
#                                         "Antarctic"))
#
#
#
# # Set a number of 'empty bar' to add at the end of each group
# empty_bar=1
# to_add = data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
# colnames(to_add) = colnames(data)
# to_add$group=rep(levels(data$group), each=empty_bar)
# data=rbind(data, to_add)
# # data=data %>% arrange(group) #ooriginal
#
# # data=data %>% arrange(group,value) %>%
# #   group_by(group) %>%
# #   mutate(rank = rank(desc(value), ties.method = "first"))
#
# # data=data %>% arrange(group,value) %>%
# #     # group_by(group) %>%
# #     mutate(rank = rank(desc(value)))
#
#
# data = data %>% dplyr::arrange(group) %>%
#   dplyr::group_by(group) %>%
#   dplyr::mutate(rank = rank(-value))
# # data=data %>% dplyr::group_by(group) %>%
# #   dplyr::arrange(value) %>%
# #   dplyr::mutate(rank = rank(-value))
# data = data %>% dplyr::filter(rank <= RANK)
# # data = data[, c("individual", "group", "value")]
#
# data$id=seq(1, nrow(data))
# data$label = NA
# for (gp in 1:9){
#   data$label[which(data$group == gp)] = tdwg1_names$LEVEL1_NAM[gp]
# }
# #remmove antarctic
# data= data[data$group<9,]
#
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
#
#
# #####################################
# # Windplot color darkspot/hotspot
# #####################################
# p = ggplot(data, aes(x=as.factor(id), y=value, fill=color)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#   geom_bar(aes(x=as.factor(id), y=value, fill=color), stat="identity", alpha=0.9) +
#   # scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
#   bi_scale_fill(pal = custom_pal4, dim=dim)+
#   # scale_fill_gradientn(colours=RColorBrewer::brewer.pal(10, "BrBG")) +
#   # scale_fill_brewer(palette = "RdBu") +
#   # ylim(-10,9) +#120) +
#   ylim(-max(data$value,na.rm=T)*2,max(data$value,na.rm=T)*3.5) +
#   #ylim(-2,5) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",
#     axis.text.y = element_blank(),#element_text(size = 10),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     panel.grid = element_blank()#,
#     # plot.margin = unit(rep(1,4), "cm")
#
#   ) +
#   geom_hline(yintercept = seq(0,max(data$value,na.rm=T),
#                               max(data$value,na.rm=T)/5), color = "gray90") +
#   #geom_hline(yintercept = seq(0,2,0.5), color = "gray90") +
#   geom_vline(xintercept = seq(1,dim(data)[1],RANK)-0.5, color = "black") +
#   coord_polar() +
#   geom_text(data = data.frame(x1 = seq((RANK/2),dim(data)[1],RANK),
#                               y1 = rep(max(data$value,na.rm=T)*3.5, 8),#rep(5, 8),
#                               label = tdwg1_names$LEVEL1_NAM[1:8]),
#             aes(x=x1, y=y1, label = label),
#             color="black", fontface="bold",alpha=0.9, size=4,
#             # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
#             #               label_data$angle[seq(5,81, 10)]+90),
#             # hjust=0.2,
#             inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
#   ) +
#
#   geom_text(data=label_data, aes(x=id, y=value+0.01,#3000,#
#                                  label=individual, hjust=hjust),
#             color="black", fontface="bold",alpha=0.7, size=3,
#             angle= label_data$angle, inherit.aes = FALSE )
#
# p
# ggsave(paste0(basepath, "PC1_PC2_roseplot.pdf"), width = 15, height = 15, units = "cm")
#
