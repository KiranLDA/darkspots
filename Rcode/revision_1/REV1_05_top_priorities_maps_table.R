
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
load( file = paste0(basepath, "REV_app_data.RData"))


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

colours_map = c("#2a9bc1","#483737","#cccccc", "#84cfbb")



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

################################################
#   PLOT all
###############################################
# scenario = 1
# darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S")])
# darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("Rank_scenario_",scenario,"_S")])
#


# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$rank),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
priorities = data.frame(LEVEL3_NAM=darkspots.prj$LEVEL3_COD)

# scenario = 1
for (scenario in 1:9){

  darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S")])
  darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("Rank_scenario_",scenario,"_S")])


  data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
                    as.numeric(darkspots.prj$LEVEL1),
                    as.numeric(darkspots.prj$rank),#as.numeric(darkspots.prj$Rn__1),
                    as.numeric(darkspots.prj$area))

  colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
  data = data %>% dplyr::arrange(global_rank)
  data$cumul_area = cumsum(data$area)
  data$cumul_area[is.na(data$global_rank)] = NA
  data =  data %>% dplyr::arrange(LEVEL3_NAM)

  data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
  data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
    dplyr::group_by(LEVEL1) %>%
    dplyr::mutate(regional_rank = rank(global_rank))


  data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                             "grey")
  data2$color_class[which(data2$regional_rank <=5)] = "red"
  data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
  data2$color_class[is.na(data2$color_class)] = "grey"
  data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
  data =  data %>% dplyr::arrange(LEVEL3_NAM)
  data =  data %>% left_join(data2)
  data =  data %>% dplyr::arrange(LEVEL3_NAM)
  darkspots.prj$color_class = data$color_class
  data = data[which(data$color_class != "grey"),]
  to_add = data.frame(cbind(data$LEVEL3_NAM,
                            data$color_class))
  colnames(to_add) = c("LEVEL3_NAM",paste0("S",scenario))
  priorities = base::merge(priorities,
                           to_add,
                           all=TRUE)
  data = darkspots.prj
  map <-  ggplot() +
    geom_point( data= data,
                aes(color =  color_class,  #fill = bi_class,
                    geometry = geometry),
                size = 0.2,
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
              linetype = "solid", colour = "black", linewidth = .3) +

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

  # create map
  assign(paste0("map",scenario) , map)
}


legend <- cowplot::get_legend(ggplot() +
                                geom_point( data= data,
                                            aes(color =  color_class,  #fill = bi_class,
                                                geometry = geometry),
                                            size = 0.2,
                                            stat = "sf_coordinates") +
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
                                                  labels = c('Darkspot',
                                                             'Darkspot & regional priority', "None", 'Regional priority only'))+#, na.value="white")+ #"#328380",
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
                                ))

ggarrange(map1,map2,map3,
          map4,map6,map8,
          map5, map7,map9,
          ncol = 3, nrow = 3)



ggsave(paste0(basepath, "maps_1-9_top.pdf"),
       width = 80, height = 50, units = "cm")




####################################################################################
# now do a table ofpriorities
# rm(data)
data <- data.frame(darkspots.prj$LEVEL3_COD,darkspots.prj$LEVEL3_NAM)
colnames(data) = c("LEVEL3_NAM", "LEVEL3_COD")
priorities = base::merge(priorities,
                         data,
                         by = c("LEVEL3_NAM"),
                         all.x=TRUE)
rownames(priorities) = priorities$LEVEL3_COD
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
data2$Label = data$LEVEL3_COD[data2$Index]
data2$Value[is.na(data2$Value)] = "grey"

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=factor(Index), y=Series, fill=Value), show.legend = FALSE) +
  geom_tile(color="white", linewidth=0.01) +
  scale_fill_manual(values = colours_map)+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#,
  #labels = c('Global', 'Global & Regional', "None", 'Regional') )+
  scale_y_discrete(labels= c("Scenario 1","Scenario 2","Scenario 3",
                             "Scenario 4","Scenario 5","Scenario 6",
                             "Scenario 7","Scenario 8","Scenario 9"))+
  scale_x_discrete(breaks = unique(data2$Index),#as.character(1:nrow(priorities)),
                   labels= unique(data2$Label))+#priorities$LEVEL3_NAM) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) #+
# xlab("Botanical Country")# +
# ylab("Scenario")

gg

ggsave(paste0(basepath, "scenario_matrix_1-9_top.pdf"),
       width = 20, height = 8, units = "cm")



# ggdraw(add_sub(map1, label = "Basemap", x = .5, y=0.8))
map1 <- map1 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(title = "Baseline",
       y = "     \n   Baseline",
       x="")+#x="Scenario 1") +
  theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
         axis.title.y = element_text(angle=0, hjust = 1, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))

map2 <- map2 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(title = "Lower income group",
       x="")+#x="Scenario 2") +
  theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map3 <- map3 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(title = "Higher income group",
       x="")+#x="Scenario 3") +
  theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map4 <- map4 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(y = "Less \nProtection",
       x="")+#x="Scenario 4") +
  theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map5 <- map5 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(y = "More \nProtection",
       x="")+#x="Scenario 5") +
  theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map6 <- map6 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 6") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map7 <- map7 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 7") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map8 <- map8 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 8") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map9 <- map9 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 9") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))



ggarrange(ggarrange(
  map1,map2,map3,
  map4,map6,map8,
  map5, map7,map9,
  ncol = 3, nrow = 3,
  # labels = c("Baseline","Low income","High income",
  #            "Low protection","","",
  #            "High protection","" ,""),
  # label.x = c(0.2,0.2,0.2,
  #             0,0.2,0.2,
  #             0,0.2,0.2),
  # label.y = c(0.8,0.8,0.8,
  #             0.5,0.2,0.2,
  #             0.5,0.2,0.2)
  labels = c("Scenario 1","Scenario 2","Scenario 3",
             "Scenario 4","Scenario 6","Scenario 8",
             "Scenario 5","Scenario 7","Scenario 9"),
  label.x = c(0.4,0.2,0.2,
              0.4,0.2,0.2,
              0.4,0.2,0.2),
  label.y = c(0.13,0.13,0.13,
              0.2,0.2,0.2,
              0.2,0.2,0.2),
  widths = c(1,0.75,0.75)
),
ggarrange(ggarrange(legend,nrow=2,widths = c(1,1)),
          gg,ncol=2, widths = c(0.3,1)),
nrow=2,ncol=1, heights=c(1,0.5))



ggsave(paste0(basepath, "top_priorities_panels.png"),
       width = 25, height = 25, units = "cm", bg="white")


#=============================================================================================
################################################
#   SCALED
###############################################

priorities = data.frame(LEVEL3_NAM=darkspots.prj$LEVEL3_COD)

# scenario = 1
for (scenario in 1:9){

  darkspots.prj["benefit"] = st_drop_geometry(darkspots.prj[paste0("benefit_",scenario,"_S_sc")])
  darkspots.prj["rank"] = st_drop_geometry(darkspots.prj[paste0("Rank_scenario_",scenario,"_S_sc")])


  data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
                    as.numeric(darkspots.prj$LEVEL1),
                    as.numeric(darkspots.prj$rank),#as.numeric(darkspots.prj$Rn__1),
                    as.numeric(darkspots.prj$area))

  colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
  data = data %>% dplyr::arrange(global_rank)
  data$cumul_area = cumsum(data$area)
  data$cumul_area[is.na(data$global_rank)] = NA
  data =  data %>% dplyr::arrange(LEVEL3_NAM)

  data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
  data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
    dplyr::group_by(LEVEL1) %>%
    dplyr::mutate(regional_rank = rank(global_rank))


  data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
                             "grey")
  data2$color_class[which(data2$regional_rank <=5)] = "red"
  data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
  data2$color_class[is.na(data2$color_class)] = "grey"
  data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
  data =  data %>% dplyr::arrange(LEVEL3_NAM)
  data =  data %>% left_join(data2)
  data =  data %>% dplyr::arrange(LEVEL3_NAM)
  darkspots.prj$color_class = data$color_class
  data = data[which(data$color_class != "grey"),]
  to_add = data.frame(cbind(data$LEVEL3_NAM,
                            data$color_class))
  colnames(to_add) = c("LEVEL3_NAM",paste0("S",scenario))
  priorities = base::merge(priorities,
                           to_add,
                           all=TRUE)
  data = darkspots.prj
  map <-  ggplot() +
    geom_point( data= data,
                aes(color =  color_class,  #fill = bi_class,
                    geometry = geometry),
                size = 0.2,
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
              linetype = "solid", colour = "black", linewidth = .3) +

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

  # create map
  assign(paste0("map",scenario) , map)
}


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

ggarrange(map1,map2,map3,
          map4,map6,map8,
          map5, map7,map9,
          ncol = 3, nrow = 3)



ggsave(paste0(basepath, "maps_1-9_top_sc.pdf"),
       width = 80, height = 50, units = "cm")




####################################################################################
# now do a table ofpriorities
# rm(data)
data <- data.frame(darkspots.prj$LEVEL3_COD,darkspots.prj$LEVEL3_NAM)
colnames(data) = c("LEVEL3_NAM", "LEVEL3_COD")
priorities = base::merge(priorities,
                         data,
                         by = c("LEVEL3_NAM"),
                         all.x=TRUE)
rownames(priorities) = priorities$LEVEL3_COD
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
data2$Label = data$LEVEL3_COD[data2$Index]
data2$Value[is.na(data2$Value)] = "grey"

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=factor(Index), y=Series, fill=Value), show.legend = FALSE) +
  geom_tile(color="white", linewidth=0.01) +
  scale_fill_manual(values = colours_map)+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#,
  #labels = c('Global', 'Global & Regional', "None", 'Regional') )+
  scale_y_discrete(labels= c("Scenario 1","Scenario 2","Scenario 3",
                             "Scenario 4","Scenario 5","Scenario 6",
                             "Scenario 7","Scenario 8","Scenario 9"))+
  scale_x_discrete(breaks = unique(data2$Index),#as.character(1:nrow(priorities)),
                   labels= unique(data2$Label))+#priorities$LEVEL3_NAM) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) #+
# xlab("Botanical Country")# +
# ylab("Scenario")

gg

ggsave(paste0(basepath, "scenario_matrix_1-9_top_sc.pdf"),
       width = 20, height = 8, units = "cm")



# ggdraw(add_sub(map1, label = "Basemap", x = .5, y=0.8))
map1 <- map1 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(title = "Baseline",
       y = "     \n   Baseline",
       x="")+#x="Scenario 1") +
  theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
         axis.title.y = element_text(angle=0, hjust = 1, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))

map2 <- map2 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(title = "Lower income group",
       x="")+#x="Scenario 2") +
  theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map3 <- map3 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(title = "Higher income group",
       x="")+#x="Scenario 3") +
  theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map4 <- map4 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(y = "Less \nProtection",
       x="")+#x="Scenario 4") +
  theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map5 <- map5 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(y = "More \nProtection",
       x="")+#x="Scenario 5") +
  theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
         axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map6 <- map6 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 6") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map7 <- map7 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 7") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map8 <- map8 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 8") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
map9 <- map9 +
  coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
  labs(x="")+#x="Scenario 9") +
  theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))



ggarrange(ggarrange(
  map1,map2,map3,
  map4,map6,map8,
  map5, map7,map9,
  ncol = 3, nrow = 3,
  # labels = c("Baseline","Low income","High income",
  #            "Low protection","","",
  #            "High protection","" ,""),
  # label.x = c(0.2,0.2,0.2,
  #             0,0.2,0.2,
  #             0,0.2,0.2),
  # label.y = c(0.8,0.8,0.8,
  #             0.5,0.2,0.2,
  #             0.5,0.2,0.2)
  labels = c("Scenario 1","Scenario 2","Scenario 3",
             "Scenario 4","Scenario 6","Scenario 8",
             "Scenario 5","Scenario 7","Scenario 9"),
  label.x = c(0.4,0.2,0.2,
              0.4,0.2,0.2,
              0.4,0.2,0.2),
  label.y = c(0.13,0.13,0.13,
              0.2,0.2,0.2,
              0.2,0.2,0.2),
  widths = c(1,0.75,0.75)
),
ggarrange(ggarrange(legend,nrow=2,widths = c(1,1)),
          gg,ncol=2, widths = c(0.3,1)),
nrow=2,ncol=1, heights=c(1,0.5))



ggsave(paste0(basepath, "top_priorities_panels_sc.png"),
       width = 25, height = 25, units = "cm", bg="white")








# # load dependencies
# library(biscale)
# library(ggplot2)
# library(cowplot)
# library(sf)
# require(tmap)
# library(classInt)
# library(dplyr)
# library(ggpubr)
#
# # basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
# basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/"
# load( file = paste0(basepath, "REV_app_data.RData"))
#
#
# ##############################################################################
# ##############################################################################
# ### FUNCTIONS
# ##############################################################################
# ##############################################################################
#
#
# rotate <- function(x) t(apply(x, 2, rev))
#
#
#
# colmat<-function(nquantiles=4, upperleft=rgb(0,150,235, maxColorValue=255),
#                  upperright=rgb(130,0,80, maxColorValue=255),
#                  bottomleft="grey",
#                  bottomright=rgb(255,230,15, maxColorValue=255)
#                  , xlab="x label", ylab="y label"
# ){
#
#
#   my.data<-seq(0,1,.01)
#   my.class<-classIntervals(my.data,n=nquantiles,style="fisher")
#   my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
#   my.pal.2<-findColours(my.class,c(upperright, bottomright))
#   col.matrix<-matrix(nrow = 101, ncol = 101, NA)
#   for(i in 1:101){
#     my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
#     col.matrix[102-i,]<-findColours(my.class,my.col)}
#   plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(0,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=1.3)
#   for(i in 1:101){
#     col.temp<-col.matrix[i-1,]
#     points(my.data,rep((i-1)/100,101),pch=15,col=col.temp, cex=1)
#   }
#   seqs<-seq(0,100,(100/nquantiles))
#   seqs[1]<-1
#   return(col.matrix[c(seqs), c(seqs)])
# }
#
# normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}
#
# SAR <- function(x,y, ref_area_km2=10000){ # Species-Area-Regression
#   # glm
#   mod <- stats::glm(x ~ log(y), family=poisson(link="log"), start = c(0.5, 0.5))
#   # non linear regression
#   mod2 <- stats::nls(x ~ c*y^z, start=list(c=exp(coef(mod)[1]), z=coef(mod)[2]))
#   z=coef(mod2)[2] # scaling area exponent
#   (x*ref_area_km2^z)/(y^z)
# }
#
#
#
# ##############################################################################
# ##############################################################################
# ## LOAD DATA
# ##############################################################################
# ##############################################################################
#
#
#
# darkspots <- tdwg3#st_read(paste0(basepath, "/model_outputs.shp"))
#
#
#
# ####  PROJECT A in ECKERT IV
#
# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# sf_use_s2(FALSE)
# m = st_buffer(darkspots, 0)
# darkspots.prj = st_transform(st_crop(m, st_bbox(c(xmin = -180,
#                                                   xmax = 180,
#                                                   ymin = -90,
#                                                   ymax = 90))),
#                              crs = PROJ)
#
#
#
#
# #####################################################################
# # write.csv(grid.DT, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
# grid.DT = read.csv( "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/gridDT.csv")
#
# grid.DT <- data.table::as.data.table(grid.DT)
#
#
# ##########
#
# colours_map = c("#2a9bc1","#483737","#cccccc", "#84cfbb")
#
#
#
# ##########################################################################################
# #############################################################################################
# # Normalise data for manual bivariate map
# ##########################################################################################
# ##########################################################################################
#
# areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))
#
# # darkspots.prj$income = normalise(darkspots.prj$PC1)
# # darkspots.prj$unprotect = normalise(darkspots.prj$PC2)
# darkspots.prj$linnean_yrs = - normalise(darkspots.prj$discoveries_time_diff)#- normalise(darkspots.prj$dscvrs_t_)
# darkspots.prj$wallacean_yrs = - normalise(darkspots.prj$descriptions_time_diff)#- normalise(darkspots.prj$dscrptns_t)
# darkspots.prj$linnean = darkspots.prj$SR_unknown_norm#darkspots.prj$SR_nk_ # SR_unknown_norm
# darkspots.prj$wallacean = darkspots.prj$SR_nogeoloc_norm #SR_ng_ #
#
# #scale
# complete_rows = !is.na(darkspots.prj$SR_unknown)
# darkspots.prj$linnean_sc = darkspots.prj$SR_unknown
# darkspots.prj$linnean_sc[complete_rows] = normalise(SAR(darkspots.prj$SR_unknown[complete_rows],
#                                                         areas$land_area[complete_rows]))
#
# complete_rows = !is.na(darkspots.prj$SR_nogeoloc)
# darkspots.prj$wallacean_sc = darkspots.prj$SR_nogeoloc
# darkspots.prj$wallacean_sc[complete_rows] = normalise(SAR(darkspots.prj$SR_nogeoloc[complete_rows],
#                                                           areas$land_area[complete_rows]))
#
#
# total_area = 24922455#24922455
# areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "tdwg_shortfall_index_land_area.csv"))
# colnames(areas) = c( "LEVEL3_COD", "area", "index")
#
# darkspots.prj = darkspots.prj %>% left_join(areas[c( "LEVEL3_COD", "area")])
#
#
#
#
#
#
# #######################################
# # SCENARIO 1
#
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_1_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                    S1 = data$color_class)
# data = darkspots.prj
#
# # create map
# legend <- cowplot::get_legend(ggplot() +
#   geom_point( data= data,
#               aes(color =  color_class,  #fill = bi_class,
#                   geometry = geometry),
#               size = 0.2,
#               stat = "sf_coordinates"
#   ) +
#   geom_sf(data = data,
#           mapping = aes(fill = color_class),
#           color = aes(fill = color_class),#NA,
#           size = 0.01#, show.legend = FALSE
#           ) +
#   # geom_sf() +  #+
#   geom_path(data = grid.DT[(long %in% c(-180,180) & region == "NS")
#                            |(long %in% c(-180,180) & lat %in% c(-90,90)
#                              & region == "EW")],
#             aes(x = X, y = Y, group = group),
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Darkspot', 'Darkspot & regional priority', "None", 'Regional priority only'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
#         ))
#
# ggarrange(legend,ncol=1,nrow=1)
# ggsave(paste0(basepath, "maps_1-9_top_legend.pdf"),
#        width = 5, height = 5, units = "cm")
#
# # create map
# map1 <-  ggplot() +
#   geom_point( data= data,
#               aes(color =  color_class,  #fill = bi_class,
#                   geometry = geometry),
#               size = 0.2,
#               stat = "sf_coordinates", show.legend = FALSE
#
#   ) +
#   # scale_x_discrete(name ="Scenario 1") +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map1
#
#
# #######################################
# # SCENARIO 2
#
# data = data.frame(darkspots.prj$LEVEL3_COD,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_2_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S2 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map2 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map2
#
#
# #######################################
# # SCENARIO 3
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_3_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S3 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map3 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map3
#
#
# #######################################
# # SCENARIO 4
#
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_4_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S4 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map4 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map4
#
#
#
#
# #######################################
# # SCENARIO 5
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_5_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S5 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map5 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map5
#
#
#
#
# #######################################
# # SCENARIO 6
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_6_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S6 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map6 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map6
#
#
# #######################################
# # SCENARIO 7
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_7_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S7 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map7 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map7
#
#
# #######################################
# # SCENARIO 8
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_8_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S8 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map8 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map8
#
#
# #######################################
# # SCENARIO 9
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_9_S),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S9 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map9 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map9
#
#
#
#
# # "#1f5c6c" # "#328380"
#
#
# ggarrange(map1,map2,map3,
#           map4,map6,map8,
#           map5, map7,map9,
#           ncol = 3, nrow = 3)
#
#
#
#
#
#
# ggsave(paste0(basepath, "maps_1-9_top.pdf"),
#        width = 80, height = 50, units = "cm")
#
#
#
#
# ####################################################################################
# # now do a table ofpriorities
# data = data.frame(darkspots.prj$LEVEL3_COD, darkspots.prj$LEVEL3_NAM)
# colnames(data) = c("LEVEL3_NAM", "LEVEL3_COD")
# priorities = merge(priorities,
#              data,
#              by = c("LEVEL3_NAM"),
#              all.x=TRUE)
# rownames(priorities) = priorities$LEVEL3_COD
# # priorities = priorities[,1:(ncol(priorities)-1)]
# priorities$freq = apply(priorities[,1:(ncol(priorities)-1)], 1,
#                         function(x) {(3*length(which(x=="black" | x == "brown"))
#                                       +0.05*length(which(x== "red")))})
#   #count(priorities=="black")$freq + count(priorities=="brown")$freq
# priorities = priorities[order(priorities[,"freq"],decreasing=TRUE),]
#
#
# data <- priorities[with(priorities,order(-freq)),]
# data <- data[1:50,]
#
# # priorities = priorities[priorities$freq >=6.05,]
#
# library(zoo)
# data2= fortify.zoo(zoo(data[2:(ncol(data)-2)]), melt = TRUE)
# data2$Label = data$LEVEL3_COD[data2$Index]
# data2$Value[is.na(data2$Value)] = "grey"
#
# ## use a white border of size 0.5 unit to separate the tiles
# gg<- ggplot(data2, aes(x=factor(Index), y=Series, fill=Value), show.legend = FALSE) +
#   geom_tile(color="white", size=0.01) +
#   scale_fill_manual(values = colours_map)+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#,
#                     #labels = c('Global', 'Global & Regional', "None", 'Regional') )+
#   scale_y_discrete(labels= c("Scenario 1","Scenario 2","Scenario 3",
#                              "Scenario 4","Scenario 5","Scenario 6",
#                              "Scenario 7","Scenario 8","Scenario 9"))+
#   scale_x_discrete(breaks = unique(data2$Index),#as.character(1:nrow(priorities)),
#                    labels= unique(data2$Label))+#priorities$LEVEL3_NAM) +
#   theme(axis.text.x=element_text(angle = 45, hjust = 1),
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) #+
#   # xlab("Botanical Country")# +
#   # ylab("Scenario")
#
# gg
#
# ggsave(paste0(basepath, "scenario_matrix_1-9_top.pdf"),
#        width = 20, height = 8, units = "cm")
#
#
#
# # ggdraw(add_sub(map1, label = "Basemap", x = .5, y=0.8))
#  map1 <- map1 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(title = "Baseline",
#        y = "     \n   Baseline",
#        x="Scenario 1") +
#   theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
#          axis.title.y = element_text(angle=0, hjust = 1, size=14,face="bold"),
#          axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#
#  map2 <- map2 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(title = "Lower income group",
#         x="Scenario 2") +
#    theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
#           axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map3 <- map3 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(title = "Higher income group",
#         x="Scenario 3") +
#    theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
#           axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map4 <- map4 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(y = "Less \nProtection",
#         x="Scenario 4") +
#    theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
#           axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map5 <- map5 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(y = "More \nProtection",
#         x="Scenario 5") +
#    theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
#           axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map6 <- map6 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(x="Scenario 6") +
#    theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map7 <- map7 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(x="Scenario 7") +
#    theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map8 <- map8 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(x="Scenario 8") +
#    theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#  map9 <- map9 +
#    coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#    labs(x="Scenario 9") +
#    theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#
# ggarrange(ggarrange(
#   map1,map2,map3,
#   map4,map6,map8,
#   map5, map7,map9,
#   ncol = 3, nrow = 3,
#   # labels = c("Baseline","Low income","High income",
#   #            "Low protection","","",
#   #            "High protection","" ,""),
#   # label.x = c(0.2,0.2,0.2,
#   #             0,0.2,0.2,
#   #             0,0.2,0.2),
#   # label.y = c(0.8,0.8,0.8,
#   #             0.5,0.2,0.2,
#   #             0.5,0.2,0.2)
#   # labels = c("Scenario 1","Scenario 2","Scenario 3",
#   #            "Scenario 4","Scenario 6","Scenario 8",
#   #            "Scenario 5","Scenario 7","Scenario 9"),
#   # label.x = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2),
#   # label.y = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2),
#   widths = c(1,0.75,0.75)
# ),
# ggarrange(legend,gg,ncol=2, widths = c(0.3,1)),
# nrow=2,ncol=1, heights=c(1,0.5))
#
#
#
# ggsave(paste0(basepath, "top_priorities_panels.png"),
#        width = 25, height = 25, units = "cm", bg="white")
#
# #-----------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------
# # SCALED
#
#
#
#
#
#
# #######################################
# # SCENARIO 1
#
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_1_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                    S1 = data$color_class)
# data = darkspots.prj
#
# # create map
# legend <- cowplot::get_legend(ggplot() +
#                                 geom_point( data= data,
#                                             aes(color =  color_class,  #fill = bi_class,
#                                                 geometry = geometry),
#                                             size = 0.2,
#                                             stat = "sf_coordinates"
#                                 ) +
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
#                                                   labels = c('Darkspot', 'Darkspot & regional priority', "None", 'Regional priority only'))+#, na.value="white")+ #"#328380",
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
# ggarrange(legend,ncol=1,nrow=1)
# ggsave(paste0(basepath, "maps_1-9_top_legend_sc.pdf"),
#        width = 5, height = 5, units = "cm")
#
# # create map
# map1 <-  ggplot() +
#   geom_point( data= data,
#               aes(color =  color_class,  #fill = bi_class,
#                   geometry = geometry),
#               size = 0.2,
#               stat = "sf_coordinates", show.legend = FALSE
#
#   ) +
#   # scale_x_discrete(name ="Scenario 1") +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map1
#
#
# #######################################
# # SCENARIO 2
#
# data = data.frame(darkspots.prj$LEVEL3_COD,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_2_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S2 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map2 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map2
#
#
# #######################################
# # SCENARIO 3
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_3_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S3 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map3 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map3
#
#
# #######################################
# # SCENARIO 4
#
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_4_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S4 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map4 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map4
#
#
#
#
# #######################################
# # SCENARIO 5
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_5_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S5 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map5 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map5
#
#
#
#
# #######################################
# # SCENARIO 6
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_6_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S6 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map6 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map6
#
#
# #######################################
# # SCENARIO 7
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_7_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S7 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map7 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map7
#
#
# #######################################
# # SCENARIO 8
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_8_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S8 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map8 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map8
#
#
# #######################################
# # SCENARIO 9
#
# data = data.frame(darkspots.prj$LEVEL3_COD, #darkspots.prj$LEVEL3_C,
#                   as.numeric(darkspots.prj$LEVEL1),
#                   as.numeric(darkspots.prj$Rank_scenario_9_S_sc),#as.numeric(darkspots.prj$Rn__1),
#                   as.numeric(darkspots.prj$area))
#
# colnames(data) = c("LEVEL3_NAM","LEVEL1", "global_rank", "area")
# data = data %>% dplyr::arrange(global_rank)
# data$cumul_area = cumsum(data$area)
# data$cumul_area[is.na(data$global_rank)] = NA
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
#
# data2 = data %>% dplyr::arrange(LEVEL1, global_rank)
# data2 = data2 %>% dplyr::arrange(LEVEL1) %>%
#   dplyr::group_by(LEVEL1) %>%
#   dplyr::mutate(regional_rank = rank(global_rank))
#
#
# data2$color_class = ifelse(data2$cumul_area <= total_area, "black",
#                            "grey")
# data2$color_class[which(data2$regional_rank <=5)] = "red"
# data2$color_class[which(data2$regional_rank <=5 & data2$cumul_area <= total_area)] = "brown"
# data2$color_class[is.na(data2$color_class)] = "grey"
# data2 = data2 %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# data =  data %>% left_join(data2)
# data =  data %>% dplyr::arrange(LEVEL3_NAM)
# darkspots.prj$color_class = data$color_class
# data = data[which(data$color_class != "grey"),]
# priorities = merge(priorities,
#                    cbind(LEVEL3_NAM=data$LEVEL3_NAM,
#                          S9 = data$color_class),
#                    all=TRUE)
# data = darkspots.prj
#
#
# # create map
# map9 <-  ggplot() +
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
#             linetype = "solid", colour = "black", size = .3) +
#
#   scale_fill_manual(values =   colours_map,
#                     labels = c('Global', 'Global & Regional', "None", 'Regional'))+#, na.value="white")+ #"#328380",
#   scale_color_manual(values =  colours_map)+
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
# map9
#
#
#
#
# # "#1f5c6c" # "#328380"
#
#
# ggarrange(map1,map2,map3,
#           map4,map6,map8,
#           map5, map7,map9,
#           ncol = 3, nrow = 3)
#
#
#
#
#
#
# ggsave(paste0(basepath, "maps_1-9_top_sc.pdf"),
#        width = 80, height = 50, units = "cm")
#
#
#
#
# ####################################################################################
# # now do a table ofpriorities
# data = data.frame(darkspots.prj$LEVEL3_COD, darkspots.prj$LEVEL3_NAM)
# colnames(data) = c("LEVEL3_NAM", "LEVEL3_COD")
# priorities = merge(priorities,
#                    data,
#                    by = c("LEVEL3_NAM"),
#                    all.x=TRUE)
# rownames(priorities) = priorities$LEVEL3_COD
# # priorities = priorities[,1:(ncol(priorities)-1)]
# priorities$freq = apply(priorities[,1:(ncol(priorities)-1)], 1,
#                         function(x) {(3*length(which(x=="black" | x == "brown"))
#                                       +0.05*length(which(x== "red")))})
# #count(priorities=="black")$freq + count(priorities=="brown")$freq
# priorities = priorities[order(priorities[,"freq"],decreasing=TRUE),]
#
#
# data <- priorities[with(priorities,order(-freq)),]
# data <- data[1:50,]
#
# # priorities = priorities[priorities$freq >=6.05,]
#
# library(zoo)
# data2= fortify.zoo(zoo(data[2:(ncol(data)-2)]), melt = TRUE)
# data2$Label = data$LEVEL3_COD[data2$Index]
# data2$Value[is.na(data2$Value)] = "grey"
#
# ## use a white border of size 0.5 unit to separate the tiles
# gg<- ggplot(data2, aes(x=factor(Index), y=Series, fill=Value), show.legend = FALSE) +
#   geom_tile(color="white", size=0.01) +
#   scale_fill_manual(values = colours_map)+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#c("#113389", "#2a9bc1",  "#f6fcc8","#84cfbb"))+#,
#   #labels = c('Global', 'Global & Regional', "None", 'Regional') )+
#   scale_y_discrete(labels= c("Scenario 1","Scenario 2","Scenario 3",
#                              "Scenario 4","Scenario 5","Scenario 6",
#                              "Scenario 7","Scenario 8","Scenario 9"))+
#   scale_x_discrete(breaks = unique(data2$Index),#as.character(1:nrow(priorities)),
#                    labels= unique(data2$Label))+#priorities$LEVEL3_NAM) +
#   theme(axis.text.x=element_text(angle = 45, hjust = 1),
#         legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank()) #+
# # xlab("Botanical Country")# +
# # ylab("Scenario")
#
# gg
#
# ggsave(paste0(basepath, "scenario_matrix_1-9_top_sc.pdf"),
#        width = 20, height = 8, units = "cm")
#
#
#
# # ggdraw(add_sub(map1, label = "Basemap", x = .5, y=0.8))
# map1 <- map1 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(title = "Baseline",
#        y = "     \n   Baseline",
#        x="Scenario 1") +
#   theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
#          axis.title.y = element_text(angle=0, hjust = 1, size=14,face="bold"),
#          axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#
# map2 <- map2 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(title = "Lower income group",
#        x="Scenario 2") +
#   theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
#          axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map3 <- map3 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(title = "Higher income group",
#        x="Scenario 3") +
#   theme( plot.title = element_text(hjust = 0.5, size=14,face="bold"),
#          axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map4 <- map4 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(y = "Less \nProtection",
#        x="Scenario 4") +
#   theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
#          axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map5 <- map5 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(y = "More \nProtection",
#        x="Scenario 5") +
#   theme( axis.title.y=element_text(angle=0, hjust = 1, size=14,face="bold"),
#          axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map6 <- map6 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(x="Scenario 6") +
#   theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map7 <- map7 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(x="Scenario 7") +
#   theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map8 <- map8 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(x="Scenario 8") +
#   theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
# map9 <- map9 +
#   coord_sf(crs = st_crs("ESRI:54012"))+#st_crs(PROJ)) +
#   labs(x="Scenario 9") +
#   theme(axis.title.x = element_text(angle=0, hjust = 0.5, size=12))
#
# ggarrange(ggarrange(
#   map1,map2,map3,
#   map4,map6,map8,
#   map5, map7,map9,
#   ncol = 3, nrow = 3,
#   # labels = c("Baseline","Low income","High income",
#   #            "Low protection","","",
#   #            "High protection","" ,""),
#   # label.x = c(0.2,0.2,0.2,
#   #             0,0.2,0.2,
#   #             0,0.2,0.2),
#   # label.y = c(0.8,0.8,0.8,
#   #             0.5,0.2,0.2,
#   #             0.5,0.2,0.2)
#   # labels = c("Scenario 1","Scenario 2","Scenario 3",
#   #            "Scenario 4","Scenario 6","Scenario 8",
#   #            "Scenario 5","Scenario 7","Scenario 9"),
#   # label.x = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2),
#   # label.y = c(0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2,0.2),
#   widths = c(1,0.75,0.75)
# ),
# ggarrange(legend,gg,ncol=2, widths = c(0.3,1)),
# nrow=2,ncol=1, heights=c(1,0.5))
#
#
#
# ggsave(paste0(basepath, "top_priorities_panels_sc.png"),
#        width = 25, height = 25, units = "cm", bg="white")
#
