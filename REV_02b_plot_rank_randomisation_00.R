#PC1 Lower is richer
#PC2 lower has more protection
# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
library(plotly)
library(tidyverse)
library(truncnorm)
library(zoo)


normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}

load("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData")


shortfalls_raw <- read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REV_shortfalls_not_rescaled_predictions_with_uncertainty_bounds.csv")

# remove the old columns that weren't working
shortfalls_raw = shortfalls_raw[,c(1:5,10:13)]


# RESCALE THE DATA
# x =  c'est ton vecteur de donnees (predictions) et y l'aire
SAR <- function(x,y, ref_area_km2=10000){ # Species-Area-Regression
  # glm
  mod <- stats::glm(x ~ log(y), family=poisson(link="log"), start = c(0.5, 0.5))
  # non linear regression
  mod2 <- stats::nls(x ~ c*y^z, start=list(c=exp(coef(mod)[1]), z=coef(mod)[2]))
  z=coef(mod2)[2] # scaling area exponent
  (x*ref_area_km2^z)/(y^z)
}



areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))


# data =  tdwg3_PCA@data
data =  st_drop_geometry(tdwg3)


# 1) Aucun "cout"
# 2) Pauvrete
# 3) Richesse
# 4) Pas de protection
# 5) Protection
# 6) Pauvrete et pas de protection
# 7) Pauvrete et protection
# 8) Richesse et pas de protection
# 9) Richesse et protection




############################################################################################
#  weigting as a SUM
############################################################################################

# Prioritise areas with the most species left to discover
weight_1_S = rep(1, length(data$SR_unknown))

# prioritise areas with the most species in poor places to discover
weight_2_S = normalise(data$PC1) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover
weight_3_S = (1-(normalise(data$PC1))) #(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
weight_4_S = normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

#Prioritise areas with most species and smallest potential for biodiversity loss PC2
weight_5_S =  (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
weight_6_S = normalise(normalise(data$PC1) + (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in poor places to discover with protection
weight_7_S = normalise(normalise(data$PC1) + (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
weight_8_S = normalise((1-normalise(data$PC1)) + (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover with protection
weight_9_S = normalise((1-normalise(data$PC1)) + (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))



for (scenario in 1:9){

  #--------------------------------
  # UNSCALED / SUM
  data[,paste0("benefit_",scenario,"_S")] = normalise(normalise(data$SR_nogeoloc) + normalise(data$SR_unknown)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S")]),
                                                        na.last = "keep", ties.method = "first")

  #--------------------------------
  # UNSCALED / SUM UL
  data[,paste0("benefit_",scenario,"_S_UL")] = normalise(normalise(data$SR_nogeoloc_UL) + normalise(data$SR_unknown_UL)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S_UL")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S_UL")]),
                                                        na.last = "keep", ties.method = "first")

  #--------------------------------
  # UNSCALED / SUM LL
  data[,paste0("benefit_",scenario,"_S_LL")] = normalise(normalise(data$SR_nogeoloc_LL) + normalise(data$SR_unknown_LL)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S_LL")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S_LL")]),
                                                           na.last = "keep", ties.method = "first")

}


data$SR_unknown_norm
# plot(data[,paste0("Rank_scenario_",scenario,"_S")],
#      data[,paste0("benefit_",scenario,"_S")], pch=19, xlim=c(0,50))
# points(data[,paste0("Rank_scenario_",scenario,"_S")],
#      data[,paste0("benefit_",scenario,"_S_UL")], pch=19, col="red")
# points(data[,paste0("Rank_scenario_",scenario,"_S")],
#        data[,paste0("benefit_",scenario,"_S_LL")], pch=19, col="blue")

old = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/old_REV_shortfalls_not_rescaled_predictions_with_uncertainty_bounds.csv")



########################################################################
########################################################################
###   Estimate Random benefits and mean Ranks for median upper and lower
########################################################################
########################################################################

samples = 100

MEAN = st_drop_geometry(data$SR_unknown)

# create random tables to fill in
randomised_unknown_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_nogeoloc_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_shortfalls_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_benefit_table = matrix(NA, nrow = length(MEAN), ncol= samples)

# generate random benefits within the expected bounds
complete = which(!is.na(data$SR_unknown))
for (rowi in complete){
  randomised_unknown_table[rowi,] = rnorm(n = samples,
                                          mean = data$SR_unknown[rowi],
                                          sd = data$SR_unknown_sd[rowi])
  success = FALSE
  while (!success) {
    UL_replace = randomised_unknown_table[rowi,] > data$SR_unknown_UL[rowi]
    if (any(UL_replace)){
      randomised_unknown_table[rowi, which(UL_replace)] = rnorm(n = length(which(UL_replace)),
                                                                mean = data$SR_unknown[rowi],
                                                                sd = data$SR_unknown_sd[rowi])
    } else{ success = TRUE}
  }
  success = FALSE
  while (!success) {
    LL_replace = randomised_unknown_table[rowi,] < data$SR_unknown_LL[rowi]
    if (any(LL_replace)){
      randomised_unknown_table[rowi, LL_replace] = rnorm(n = length(which(LL_replace)),
                                                         mean = st_drop_geometry(data$SR_unknown)[rowi],
                                                         sd = st_drop_geometry(data$SR_unknown_sd)[rowi])
    } else{ success = TRUE}
  }
}

complete = which(!is.na(data$SR_nogeoloc))
for (rowi in complete){

  randomised_nogeoloc_table[rowi,] = rnorm(n = samples,
                                           mean = data$SR_nogeoloc[rowi],
                                           sd = data$SR_nogeoloc_sd[rowi])

  success = FALSE
  while (!success) {
    UL_replace = randomised_nogeoloc_table[rowi,] > data$SR_nogeoloc_UL[rowi]
    if (any(UL_replace)){
      randomised_nogeoloc_table[rowi, UL_replace] = rnorm(n = length(which(UL_replace)),
                                                          mean = data$SR_nogeoloc[rowi],
                                                          sd = data$SR_nogeoloc_sd[rowi])
    } else{ success = TRUE}
  }
  success = FALSE
  while (!success) {
    LL_replace = randomised_nogeoloc_table[rowi,] < data$SR_nogeoloc_LL[rowi]
    if (any(LL_replace)){
      randomised_nogeoloc_table[rowi, LL_replace] = rnorm(n = length(which(LL_replace)),
                                                          mean = data$SR_nogeoloc[rowi],
                                                          sd = data$SR_nogeoloc_sd[rowi])
    } else{ success = TRUE}
  }


}


# normalise independently
for (coli in 1:samples){
  randomised_shortfalls_table[,coli] = normalise(normalise(randomised_unknown_table[,coli]) +
                                                   normalise(randomised_nogeoloc_table[,coli]))

  randomised_benefit_table[,coli] =  normalise(normalise(randomised_unknown_table[,coli]) +
                                                 normalise(randomised_nogeoloc_table[,coli])) +  get(paste0("weight_",scenario,"_S"))
#
}
randomised_shortfalls_table = normalise(normalise(cbind(randomised_unknown_table, data$SR_unknown)) +
  normalise(cbind(randomised_nogeoloc_table, data$SR_nogeoloc)))

benefit = normalise(normalise(data$SR_unknown) + normalise(data$SR_nogeoloc)) +  get(paste0("weight_",scenario,"_S"))

randomised_benefit_table = normalise(normalise(cbind(randomised_unknown_table, data$SR_unknown)) +
                                       normalise(cbind(randomised_nogeoloc_table, data$SR_nogeoloc))) +  get(paste0("weight_",scenario,"_S"))


scaled_SR_shortfalls = randomised_shortfalls_table[, (samples+1)]
randomised_shortfalls_table = randomised_shortfalls_table[,1:samples]
scaled_benefit = randomised_benefit_table[, (samples+1)]
randomised_benefit_table = randomised_benefit_table[,1:samples]

for (scenario in 1:9){

  # CALCULATE BENEFIT FOR ALL these randomly generated benefits

  #--------------------------------
  # UNSCALED / SUM
  # assign(paste0("randomised_benefit_table_",scenario,"_S"), sweep(randomised_shortfalls_table, 1, get(paste0("weight_",scenario,"_S")), FUN = "+" ))
  # assign(paste0("randomised_benefit_table_",scenario,"_S"), normalise(normalise(randomised_nogeoloc_table) + normalise(randomised_unknown_table)) + get(paste0("weight_",scenario,"_S")))
  assign(paste0("randomised_benefit_table_",scenario,"_S"), randomised_benefit_table)#(randomised_shortfalls_table + get(paste0("weight_",scenario,"_S"))))

  #--------------------------------
  # UNSCALED / SUM
  temp_out = matrix(NA, nrow = length(MEAN), ncol= samples)
  temp_in = get(paste0("randomised_benefit_table_",scenario,"_S"))

  # rank each column of randomised benefits in table
  for (coli in 1:samples){
    temp_out[,coli] = rank( - temp_in[,coli], na.last = "keep", ties.method = "first")
  }
  assign(paste0("randomised_rank_table_",scenario,"_S"), temp_out)

  # calculate the mean rank per country
  mean_rank = NA
  for (rowi in 1:length(MEAN)){
    mean_rank = c(mean_rank, mean(get(paste0("randomised_rank_table_",scenario,"_S"))[rowi,],na.rm=T))
  }
  data[,paste0("mean_randomised_rank_scenario_",scenario,"_S")] = mean_rank[-1]




}














scenario = 1

#get the order
sort_col = order(-benefit)#order(-data[,paste0("benefit_",scenario,"_S")])
to_plot = get(paste0("randomised_benefit_table_",scenario,"_S"))
# rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
to_plot = to_plot[sort_col,1:samples]
# keep only the top 100


data3= fortify.zoo(zoo(to_plot), melt = TRUE)
data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
# data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
data3$Mean = benefit[data3$Index][sort_col]##data$benefit_1_S[data3$Index][sort_col] # scaled_benefit[data3$Index][sort_col]#


# data3 = data3[data3$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
# gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
#                        fill=Index, color = Index),
#             show.legend = FALSE) +
#   # geom_tile(color="white", size=0.01) +
#   geom_boxplot(aes(middle = median(Value)), outlier.shape=NA)+#, coef = 100) +
#   geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
#   # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
#   # geom_jitter(shape=16, position=position_jitter(0.2)) +
#   scale_x_discrete(labels= unique(data3$Label)) +
#   xlab("Ranked botanical country") +
#   ylab("Expected benefit given model uncertainty") +
#   # coord_flip() +
#   # ylim(0,2.5) +
#   scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   theme_classic() +
#   theme(axis.text.x=element_text(angle=90,hjust=1),
#         legend.position = "none")
#
#
# gg #+ geom_point(aes(x=as.factor(Index), y=Mean, color= "red"))
#
# ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_benefit_plot.pdf",
#        width = 30, height = 12, units = "cm")



# top 100
data3 = data3[data3$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
                       fill=Index),
            show.legend = FALSE) +
  geom_boxplot(aes(middle = median(Value)), outlier.shape=NA, coef = 1000) +
  geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
  scale_x_discrete(labels= unique(data3$Label)) +
  xlab("Ranked botanical country") +
  ylab("Expected benefit given model uncertainty") +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")


gg

ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_benefit_plot_top100.pdf",
       width = 25, height = 15, units = "cm")



## JITTER POINTS
# gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
#                        fill=Index, color=Index),
#             show.legend = FALSE) +
#   geom_jitter(shape=19, position=position_jitter(0.4),size=0.005) +
#   geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
#   scale_x_discrete(labels= unique(data3$Label)) +
#   xlab("Ranked botanical country") +
#   ylab("Expected benefit given model uncertainty") +
#   scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   theme_classic() +
#   theme(axis.text.x=element_text(angle=90,hjust=1),
#         legend.position = "none")
#
#
# gg
#
# ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_benefit_jitter_top100.pdf",
#        width = 25, height = 15, units = "cm")




############################################################
#get the order
sort_col = order(st_drop_geometry(tdwg3)[,paste0("Rank_scenario_",scenario,"_S")])
to_plot = get(paste0("randomised_rank_table_",scenario,"_S"))
# rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
to_plot = to_plot[sort_col,1:samples]
# keep only the top 100
# to_plot = to_plot[1:100,]

data2= fortify.zoo(zoo(to_plot), melt = TRUE)
data2$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data2$Index][sort_col]
data2$Mean = st_drop_geometry(tdwg3$Rank_scenario_1_S)[data2$Index][sort_col]

# data2 = data2[data2$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=as.factor(Index), y=Value, fill=Index), show.legend = FALSE) +
  # geom_tile(color="white", size=0.01) +
  geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  geom_boxplot() +
  scale_x_discrete(labels= unique(data2$Label)) +
  ylab("Rank") +
  xlab("Botanical Country") +
  coord_flip() +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(legend.position = "none")


gg

ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_rank_plot.pdf",
       width = 25, height = 25, units = "cm")


# data2 = data2[data2$Index <=100,]
#
# ## use a white border of size 0.5 unit to separate the tiles
# gg<- ggplot(data2, aes(x=as.factor(Index), y=Value, fill=Index), show.legend = FALSE) +
#   # geom_tile(color="white", size=0.01) +
#   geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
#   geom_boxplot(outlier.shape=NA, coef = 100) +
#   scale_x_discrete(labels= unique(data2$Label)) +
#   ylab("Rank") +
#   xlab("Botanical Country") +
#   coord_flip() +
#   scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   theme_classic() +
#   theme(legend.position = "none")
#
#
# gg

# ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_rank_plot_top100.pdf",
#        width = 15, height = 25, units = "cm")
#
#######################################
# TRY AGAIN
#######################################
samples = 100000
new_benefit_table = matrix(NA, nrow = length(MEAN), ncol= samples)

benefit = normalise(normalise(data$SR_unknown) + normalise(data$SR_nogeoloc)) +  get(paste0("weight_",scenario,"_S"))

for (coli in 1:samples){
  unknown = rep(NA, len = length(data$SR_unknown))
  nogeoloc = rep(NA, len = length(data$SR_unknown))

  complete = which(!is.na(data$SR_unknown))

  for (rowi in complete){
    unknown[rowi] = rnorm(n = 1, mean = data$SR_unknown[rowi], sd = data$SR_unknown_sd[rowi])

    success = FALSE
    while (!success) {
      replace = unknown[rowi] > data$SR_unknown_UL[rowi]
      if (any(replace)){
        unknown[rowi] = rnorm(n = 1, mean = data$SR_unknown[rowi], sd = data$SR_unknown_sd[rowi])
      } else{ success = TRUE}
    }
    success = FALSE
    while (!success) {
      replace = unknown[rowi] < data$SR_unknown_LL[rowi]
      if (any(replace)){
        unknown[rowi] = rnorm(n = 1, mean = data$SR_unknown[rowi], sd = data$SR_unknown_sd[rowi])
      } else{ success = TRUE}
    }
  }


  complete = which(!is.na(data$SR_nogeoloc))

  for (rowi in complete){
    nogeoloc[rowi] = rnorm(n = 1, mean = data$SR_nogeoloc[rowi], sd = data$SR_nogeoloc_sd[rowi])

    success = FALSE
    while (!success) {
      replace = nogeoloc[rowi] > data$SR_nogeoloc_UL[rowi]
      if (any(replace)){
        nogeoloc[rowi] = rnorm(n = 1, mean = data$SR_nogeoloc[rowi], sd = data$SR_nogeoloc_sd[rowi])
      } else{ success = TRUE}
    }
    success = FALSE
    while (!success) {
      replace = nogeoloc[rowi] < data$SR_nogeoloc_LL[rowi]
      if (any(replace)){
        nogeoloc[rowi] = rnorm(n = 1, mean = data$SR_nogeoloc[rowi], sd = data$SR_nogeoloc_sd[rowi])
      } else{ success = TRUE}
    }
  }
  new_benefit_table[,coli] = normalise(normalise(unknown) +
                                  normalise(nogeoloc)) +   get(paste0("weight_",scenario,"_S"))
}

complete = which(!is.na(data$SR_nogeoloc))

summary_benefit_table = matrix(NA, nrow = length(MEAN), ncol= 6)

for (rowi in complete){
  summary_benefit_table[rowi,] = summary(new_benefit_table[rowi,])
}

sort_col = order(-benefit)
plot(benefit[sort_col], type="l")
lines(summary_benefit_table[sort_col,1])
lines(summary_benefit_table[sort_col,5])





scenario = 1

# #get the order
# sort_col = order(-benefit)#order(-data[,paste0("benefit_",scenario,"_S")])
# to_plot = new_benefit_table
# # rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
# to_plot = to_plot[sort_col,1:samples]
# # keep only the top 100
#
#
# data3= fortify.zoo(zoo(to_plot), melt = TRUE)
# data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
# # data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
# data3$Mean = benefit[data3$Index][sort_col]##data$benefit_1_S[data3$Index][sort_col] # scaled_benefit[data3$Index][sort_col]#
#
#
# #
#
# # top 100
# data3 = data3[data3$Index <=100,]
#
# ## use a white border of size 0.5 unit to separate the tiles
# gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
#                        fill=Index),
#             show.legend = FALSE) +
#   geom_boxplot(aes(middle = median(Value)), outlier.shape=19,outlier.size = 0.05) +
#   geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
#   scale_x_discrete(labels= unique(data3$Label)) +
#   xlab("Ranked botanical country") +
#   ylab("Expected benefit given model uncertainty") +
#   scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
#   theme_classic() +
#   theme(axis.text.x=element_text(angle=90,hjust=1),
#         legend.position = "none")
#
#
# gg
#
# # sort by mean
# #get the order
res<-cor.test(x,y, method="kendall")
res


sort_col = order(-summary_benefit_table[,3])#order(-data[,paste0("benefit_",scenario,"_S")])
to_plot = new_benefit_table
# rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
to_plot = to_plot[sort_col,1:samples]
# keep only the top 100


data3= fortify.zoo(zoo(to_plot), melt = TRUE)
data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
# data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
data3$Mean = benefit[data3$Index][sort_col]##data$benefit_1_S[data3$Index][sort_col] # scaled_benefit[data3$Index][sort_col]#


#

# top 100
data3 = data3[data3$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
                       fill=Index),
            show.legend = FALSE) +
  geom_boxplot(aes(middle = median(Value)), outlier.shape=19, outlier.size = 0.05) +
  geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
  scale_x_discrete(labels= unique(data3$Label)) +
  xlab("Ranked botanical country") +
  ylab("Expected benefit given model uncertainty") +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=0.95,vjust=0.2),
        legend.position = "none")


gg

#get the order
sort_col = order(st_drop_geometry(tdwg3)[,paste0("Rank_scenario_",scenario,"_S")])
to_plot = get(paste0("randomised_rank_table_",scenario,"_S"))
# rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
to_plot = to_plot[sort_col,1:samples]
# keep only the top 100
# to_plot = to_plot[1:100,]

data2= fortify.zoo(zoo(to_plot), melt = TRUE)
data2$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data2$Index][sort_col]
data2$Mean = st_drop_geometry(tdwg3$Rank_scenario_1_S)[data2$Index][sort_col]

# data2 = data2[data2$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=as.factor(Index), y=Value, fill=Index), show.legend = FALSE) +
  # geom_tile(color="white", size=0.01) +
  geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  geom_boxplot() +
  scale_x_discrete(labels= unique(data2$Label)) +
  ylab("Rank") +
  xlab("Botanical Country") +
  coord_flip() +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(legend.position = "none")


gg


#######################################
# TRY AGAIN with normalised score
#######################################
samples = 100#00
new_benefit_table = matrix(NA, nrow = length(MEAN), ncol= samples)

benefit = normalise(normalise(data$SR_unknown) + normalise(data$SR_nogeoloc)) +  get(paste0("weight_",scenario,"_S"))




for (coli in 1:samples){
  unknown = rep(NA, len = length(data$SR_unknown_norm))
  nogeoloc = rep(NA, len = length(data$SR_unknown_norm))

  complete = which(!is.na(data$SR_unknown_norm))

  for (rowi in complete){
    unknown[rowi] = rnorm(n = 1, mean = data$SR_unknown_norm[rowi], sd = data$SR_unknown_norm_SD[rowi])

    success = FALSE
    while (!success) {
      replace = unknown[rowi] > data$SR_unknown_norm_UL[rowi]
      if (any(replace)){
        unknown[rowi] = rnorm(n = 1, mean = data$SR_unknown_norm[rowi], sd = data$SR_unknown_norm_SD[rowi])
      } else{ success = TRUE}
    }
    success = FALSE
    while (!success) {
      replace = unknown[rowi] < data$SR_unknown_norm_LL[rowi]
      if (any(replace)){
        unknown[rowi] = rnorm(n = 1, mean = data$SR_unknown_norm[rowi], sd = data$SR_unknown_norm_SD[rowi])
      } else{ success = TRUE}
    }
  }


  complete = which(!is.na(data$SR_nogeoloc_norm))

  for (rowi in complete){
    nogeoloc[rowi] = rnorm(n = 1, mean = data$SR_nogeoloc_norm[rowi], sd = data$SR_nogeoloc_norm_SD[rowi])

    success = FALSE
    while (!success) {
      replace = nogeoloc[rowi] > data$SR_nogeoloc_norm_UL[rowi]
      if (any(replace)){
        nogeoloc[rowi] = rnorm(n = 1, mean = data$SR_nogeoloc_norm[rowi], sd = data$SR_nogeoloc_norm_SD[rowi])
      } else{ success = TRUE}
    }
    success = FALSE
    while (!success) {
      replace = nogeoloc[rowi] < data$SR_nogeoloc_norm_LL[rowi]
      if (any(replace)){
        nogeoloc[rowi] = rnorm(n = 1, mean = data$SR_nogeoloc_norm[rowi], sd = data$SR_nogeoloc_norm_SD[rowi])
      } else{ success = TRUE}
    }
  }
  new_benefit_table[,coli] = normalise(normalise(unknown) +
                                         normalise(nogeoloc)) +   get(paste0("weight_",scenario,"_S"))
}

# new_benefit_table = normalise(normalise(unknown) +
#                                        normalise(nogeoloc)) +   get(paste0("weight_",scenario,"_S"))

complete = which(!is.na(data$SR_nogeoloc_norm))

summary_benefit_table = matrix(NA, nrow = length(MEAN), ncol= 6)

for (rowi in complete){
  summary_benefit_table[rowi,] = summary(new_benefit_table[rowi,])
}

sort_col = order(-benefit)
plot(benefit[sort_col], type="l")
lines(summary_benefit_table[sort_col,1])
lines(summary_benefit_table[sort_col,5])





scenario = 1

#get the order
sort_col = order(-benefit)#order(-data[,paste0("benefit_",scenario,"_S")])
to_plot = new_benefit_table
# rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
to_plot = to_plot[sort_col,1:samples]
# keep only the top 100


data3= fortify.zoo(zoo(to_plot), melt = TRUE)
data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
# data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
data3$Mean = benefit[data3$Index][sort_col]##data$benefit_1_S[data3$Index][sort_col] # scaled_benefit[data3$Index][sort_col]#


#

# top 100
data3 = data3[data3$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
                       fill=Index),
            show.legend = FALSE) +
  geom_boxplot(aes(middle = median(Value)), outlier.shape=19,outlier.size = 0.05) +
  geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
  scale_x_discrete(labels= unique(data3$Label)) +
  xlab("Ranked botanical country") +
  ylab("Expected benefit given model uncertainty") +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")


gg

