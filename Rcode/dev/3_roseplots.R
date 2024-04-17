load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")

library(dplyr)
# library(plyr)
# detach("package:plyr", unload=TRUE)



###########################################################
#### Past Discoveries
################################################################



data = tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "discoveries_max_year", "discoveries_y2010")] %>%
  arrange(LEVEL1_COD, discoveries_y2010)
colnames(data) = c("individual","group", "year","value")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-Temperate",
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
data = data %>% dplyr::filter(rank <= 10)
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
  ylim(-0.1,0.2) +
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
  geom_hline(yintercept = seq(0,0.15,0.05), color = "gray90") +
  geom_vline(xintercept = seq(1,81,10)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq(5,81, 10),
                              y1 = rep(0.2, 8),
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
                 style = "quantile", dim = dim)


data = cbind(tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "shortfalls_norm_index")], bi_label$bi_class) %>%
  arrange(LEVEL1_COD, shortfalls_norm_index)
colnames(data) = c("individual","group", "value", "color")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-Temperate",
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
data = data %>% dplyr::filter(rank <= 10)
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
  ylim(-2,4) +
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
  geom_hline(yintercept = seq(0,2,0.5), color = "gray90") +
  geom_vline(xintercept = seq(1,81,10)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq(5,81, 10),
                              y1 = rep(4, 8),
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



################################################################################################################################
###########################################################################################################################

#### SDGs

################################################################################################################################
################################################################################################################################




dim=4
col.matrix<-colmat(nquantiles=dim,
                   upperleft="#944c3b",#"#660000",#"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
                   upperright= "#f3e6b3",#"#FF9933",# BOTTOM LEFT   #rgb(255,230,15, maxColorValue=255),
                   bottomleft="#655e8a",# TOP RIGHT     #"brown2",#"black",#"grey",
                   bottomright= "#b4ace9"#"#8009a9" #"#9966FF"#"brown1"#rgb(130,0,80, maxColorValue=255)
)

#
# col.matrix<-colmat(nquantiles=dim,
#                    upperleft ="#660000", # dark red"darkgoldenrod4",#rgb(0,150,235, maxColorValue=255),
#                    upperright = "#FF9933",# orange#" #BOTTOM LEFT   "#FF9933" #rgb(255,230,15, maxColorValue=255),
#                    bottomleft =  "#330066", # dark purple # TOP RIGHT  #"#330066"   #"brown2",#"black",#"grey",
#                    bottomright =  "#9966FF" #purple "brown1"#rgb(130,0,80, maxColorValue=255)
# )
custom_pal4 <- as.vector(rotate(rotate(col.matrix[2:(dim+1),2:(dim+1)])))
names(custom_pal4)= do.call(paste0, expand.grid(1:(dim), sep="-",1:(dim)))




bi_label <- bi_class(darkspots.prj, y=drk_unprotect, x=income,
                 style = "quantile", dim = dim)




data = cbind(tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "benefit_4")], bi_label$bi_class) %>%
  arrange(LEVEL1_COD, benefit_4)
colnames(data) = c("individual","group", "value", "color")
# data[is.na(data$group),"group"]= "Other"
# data[is.na(data$value),"value"]= 1
# data$value = log(data$value)



tdwg1_names = data.frame(LEVEL1_COD = seq(1,9,1),
                         LEVEL1_NAM = c("Europe",
                                        "Africa",
                                        "Asia-Temperate",
                                        "Asia-Tropical",
                                        "Australasia",
                                        "Pacific",
                                        "North          \nAmerica        ",
                                        "South America",
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
data = data %>% dplyr::filter(rank <= 10)
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
  ylim(-2,5) +
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
  geom_hline(yintercept = seq(0,2,0.5), color = "gray90") +
  geom_vline(xintercept = seq(1,81,10)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq(5,81, 10),
                              y1 = rep(5, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
            #               label_data$angle[seq(5,81, 10)]+90),
            # hjust=0.2,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+0.01,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p

