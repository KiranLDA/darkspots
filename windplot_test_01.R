
library(dplyr)
# library(plyr)
# detach("package:plyr", unload=TRUE)

data = tdwg3@data[,c("LEVEL3_NAM","LEVEL1_COD", "SR_shortfalls", "darkhot")] %>% arrange(LEVEL1_COD, SR_shortfalls)
colnames(data) = c("individual","group", "value", "darkhot")
data[is.na(data$group),"group"]= "Other"
data[is.na(data$value),"value"]= 1
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
data = data %>% filter(rank <= 10)
data = data[, c("individual", "group", "value", "darkhot")]

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
# Windplot color by region
#####################################
p = ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-6000,11000) +
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
  geom_vline(xintercept = seq(1,81,10)-0.5, color = "gray90") +
  geom_hline(yintercept = seq(0,6000,1000), color = "gray90") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq(5,81, 10),y1 = rep(10100, 8),
                                  label = tdwg1_names$LEVEL1_NAM[1:8]),
                aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
            #               label_data$angle[seq(5,81, 10)]+90),
            # hjust=0.2,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
                ) +

  geom_text(data=label_data, aes(x=id, y=value+20,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p




#####################################
# Windplot color darkspot/hotspot
#####################################
p = ggplot(data, aes(x=as.factor(id), y=value, fill=darkhot)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar

  geom_bar(aes(x=as.factor(id), y=value, fill=darkhot), stat="identity", alpha=0.9) +
  scale_fill_manual(values=c("darkred" ,"black", "coral" ,"grey")) +
  # scale_fill_brewer(palette = "RdBu") +
  # ylim(-10,9) +#120) +
  ylim(-6000,11000) +
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
  geom_hline(yintercept = seq(0,6000,1000), color = "gray90") +
  geom_vline(xintercept = seq(1,81,10)-0.5, color = "black") +
  coord_polar() +
  geom_text(data = data.frame(x1 = seq(5,81, 10),y1 = rep(10100, 8),
                              label = tdwg1_names$LEVEL1_NAM[1:8]),
            aes(x=x1, y=y1, label = label),
            color="black", fontface="bold",alpha=0.9, size=4,
            # angle= ifelse(label_data$angle[seq(5,81, 10)]>0 , label_data$angle[seq(5,81, 10)]-90,
            #               label_data$angle[seq(5,81, 10)]+90),
            # hjust=0.2,
            inherit.aes = FALSE#, linetype = 0, size = 8,upright = TRUE
  ) +

  geom_text(data=label_data, aes(x=id, y=value+20,#3000,#
                                 label=individual, hjust=hjust),
            color="black", fontface="bold",alpha=0.7, size=3,
            angle= label_data$angle, inherit.aes = FALSE )

p


