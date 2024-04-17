library(zoo)
library(ggplot2)
library(sf)

plot.new()
par(mfrow=c(1,1))


# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/"
load( file = paste0(basepath, "REV_app_data.RData"))

data =  st_drop_geometry(tdwg3)


# functions
normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}

normalize_fixed <- function(x, min_val = 0, max_val = 1000) {
  return ((x - min_val) / (max_val - min_val))
}




########################################################################
########################################################################
###   Estimate Random benefits and mean Ranks for median upper and lower
########################################################################
########################################################################

samples = 10000
# scenario = 1
MEAN = data$SR_unknown

# create random tables to fill in
randomised_unknown_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_nogeoloc_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_shortfalls_table = matrix(NA, nrow = length(MEAN), ncol= samples)


# generate random benefits within the expected bounds


# GENERATE RANDOM LINNEAN SHORTFALL
complete = which(!is.na(data$SR_unknown))
for (rowi in complete){

  # generate a random number
  randomised_unknown_table[rowi,] = rnorm(n = samples,
                                          mean = data$SR_unknown[rowi],
                                          sd = data$SR_unknown_sd[rowi])
  # make sure it is under the upper limit
  success = FALSE
  while (!success) {
    UL_replace = randomised_unknown_table[rowi,] > data$SR_unknown_UL[rowi]
    if (any(UL_replace)){
      randomised_unknown_table[rowi, which(UL_replace)] = rnorm(n = length(which(UL_replace)),
                                                                mean = data$SR_unknown[rowi],
                                                                sd = data$SR_unknown_sd[rowi])
    } else{ success = TRUE}
  }

  # make sure it is above the lower limit
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


# GENERATE RANDOM WALLACEAN SHORTFALL
complete = which(!is.na(data$SR_nogeoloc))
for (rowi in complete){

  # generate a random number
  randomised_nogeoloc_table[rowi,] = rnorm(n = samples,
                                           mean = data$SR_nogeoloc[rowi],
                                           sd = data$SR_nogeoloc_sd[rowi])

  # make sure it is under the upper limit
  success = FALSE
  while (!success) {
    UL_replace = randomised_nogeoloc_table[rowi,] > data$SR_nogeoloc_UL[rowi]
    if (any(UL_replace)){
      randomised_nogeoloc_table[rowi, UL_replace] = rnorm(n = length(which(UL_replace)),
                                                          mean = data$SR_nogeoloc[rowi],
                                                          sd = data$SR_nogeoloc_sd[rowi])
    } else{ success = TRUE}
  }

  # make sure it is above the lower limit
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


#-----------------------------------------------------

# GENERATE THE SHORTFALLS
for (coli in 1:samples){
  randomised_shortfalls_table[,coli] = (normalize_fixed(randomised_unknown_table[,coli],0,max(data$SR_unknown,na.rm=T)) +
                                          normalize_fixed(randomised_nogeoloc_table[,coli],0,max(data$SR_nogeoloc,na.rm=T)))
}



# CALCULATE BENEFIT FOR ALL these randomly generated benefits
for (scenario in 1:9){

  assign(paste0("randomised_benefit_table_",scenario,"_S"),
         (randomised_shortfalls_table/2 + get(paste0("weight_",scenario,"_S"))))

  # rank each column of randomised benefits in table
  temp_out = matrix(NA, nrow = length(MEAN), ncol= samples)
  temp_in = get(paste0("randomised_benefit_table_",scenario,"_S"))
  for (coli in 1:samples){
    temp_out[,coli] = rank( - temp_in[,coli], na.last = "keep", ties.method = "first")
  }
  assign(paste0("randomised_rank_table_",scenario,"_S"), temp_out)

  # calculate the mean benefit per country
  mean_benefit = NA
  for (rowi in 1:length(MEAN)){
    mean_benefit = c(mean_benefit, mean(get(paste0("randomised_benefit_table_",scenario,"_S"))[rowi,],na.rm=T))
  }
  data[,paste0("mean_randomised_benefit_scenario_",scenario,"_S")] = mean_benefit[-1]



  # calculate the mean rank per country
  mean_rank = NA
  for (rowi in 1:length(MEAN)){
    mean_rank = c(mean_rank, mean(get(paste0("randomised_rank_table_",scenario,"_S"))[rowi,],na.rm=T))
  }
  data[,paste0("mean_randomised_rank_scenario_",scenario,"_S")] = mean_rank[-1]

  # calculate spearkman's rank correlation
  to_keep = which(!is.na(data[,paste0("benefit_",scenario,"_S")]) &
                    !is.na(data[,paste0("mean_randomised_benefit_scenario_",scenario,"_S")]))
  spearman = NA
  spearman_significance = NA
  for (runi in 1:samples){
    out = cor.test(data[to_keep, paste0("benefit_",scenario,"_S")],
                   get(paste0("randomised_benefit_table_",scenario,"_S"))[to_keep,runi],
                   method="spearman")
    spearman = c(spearman,out$estimate)
    spearman_significance = c(spearman_significance,out$p.value)
  }
  assign(paste0("spearman_",scenario,"_S"), spearman[-1])
  assign(paste0("spearman_significance_",scenario,"_S"), spearman_significance[-1])

  # calculate kendall's rank correlation
  kendall = NA
  kendall_significance = NA
  for (runi in 1:samples){

    out = cor.test(data[to_keep, paste0("benefit_",scenario,"_S")],
                   get(paste0("randomised_benefit_table_",scenario,"_S"))[to_keep,runi],
                   method="kendall")
    kendall = c(kendall,out$estimate)
    kendall_significance = c(kendall_significance,out$p.value)
  }
  assign(paste0("kendall_",scenario,"_S"), kendall[-1])
  assign(paste0("kendall_significance_",scenario,"_S"), kendall_significance[-1])

  # save(get(paste0("spearman_",scenario,"_S")),
  #      get(paste0("spearman_significance_",scenario,"_S")),
  #      get(paste0("kendall_",scenario,"_S")),
  #      get(paste0("kendall_significance_",scenario,"_S")),
  #      get(paste0("randomised_benefit_table_",scenario,"_S")),
  #      get(paste0("randomised_rank_table_",scenario,"_S")),
  #      file = paste0(basepath, "randomisation_outputs_",scenario,"_S.RData"))
  save(list = c(paste0("spearman_",scenario,"_S"),
                paste0("spearman_significance_",scenario,"_S"),
                paste0("kendall_",scenario,"_S"),
                paste0("kendall_significance_",scenario,"_S"),
                paste0("randomised_benefit_table_",scenario,"_S"),
                paste0("randomised_rank_table_",scenario,"_S")),
       file = paste0(basepath, "randomisation_outputs_",scenario,"_S.RData"))



}




# to_keep = which(!is.na(data[,paste0("Rank_scenario_",scenario,"_S")]) &
#                   !is.na(data[,paste0("mean_randomised_rank_scenario_",scenario,"_S")]))
# wilcox.test(data[to_keep, paste0("Rank_scenario_",scenario,"_S")],
#             round(data[to_keep,paste0("mean_randomised_rank_scenario_",scenario,"_S")]))
#
# wilcox.test(data[to_keep, paste0("benefit_",scenario,"_S")],
#             round(data[to_keep,paste0("mean_randomised_benefit_scenario_",scenario,"_S")]))
#
# wilcox.test(data[to_keep, paste0("benefit_",scenario,"_S")],
#             data[to_keep,paste0("benefit_",scenario,"_M")])
#
# wilcox.test(data[to_keep, paste0("Rank_scenario_",scenario,"_M")],
#             data[to_keep,paste0("Rank_scenario_",scenario,"_S")])
#

#==================================================
# Make a boxplot for each scenario
#==================================================
cutoff = 50

for (scenario in 1:9){
  # get the order
  index = data[,paste0("benefit_",scenario,"_S")]
  sort_col = order(-index)
  to_plot = get(paste0("randomised_benefit_table_",scenario,"_S"))
  to_plot = to_plot[sort_col,1:samples]

  # vercotise for plotting
  data3 = fortify.zoo(zoo(to_plot), melt = TRUE)
  data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
  # data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
  data3$Mean = index[data3$Index][sort_col]  # data$shortfalls_norm_index[data3$Index][sort_col] #st_drop_geometry(tdwg3$benefit_1_S)[data3$Index][sort_col]
  data3 = data3[data3$Index <=cutoff,]

  png(paste0(basepath,"random_benefit_scenario_",scenario,"_S.png"),
      width=25, height=20, units="cm", res = 300)
  par(mar=c(7.1, 4.1, 2.1, 2.1), mfrow=c(1,1))
  boxplot(Value~Index, data = data3, xlab="", ylab="Expected benefit", #main= "summed normalised",
          ylim=c(min(data3$Value),2),
          las=2, cex=0, lty=1,xaxt="n")#, names = data3$Label[1:99], cex.names=0.5)

  axis(1, at=1:cutoff,labels = data3$Label[1:cutoff], cex.axis=0.6, las=2)

  points(data3$Index[1:cutoff], data3$Mean[1:cutoff],pch=19, col="brown")
  dev.off()

  # ## use a white border of size 0.5 unit to separate the tiles
  # gg<- ggplot(data3, aes(x=as.factor(Index), y=Value, fill=Index, color=Index), show.legend = FALSE) +
  #   # geom_tile(color="white", size=0.01) +
  #   # geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  #   geom_boxplot(outlier.shape=NA) +
  #   scale_x_discrete(labels= unique(data3$Label)) +
  #   ylab("Rank") +
  #   xlab("Botanical Country") +
  #   coord_flip() +
  #   scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  #   scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  #   theme_classic() +
  #   geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  #   theme(legend.position = "none")
  #
  #
  # gg

  #==================================================
  # plot rank randomisation
  #==================================================
  # get the order
  index = data[,paste0("Rank_scenario_",scenario,"_S")]
  sort_col = order(index)
  to_plot = get(paste0("randomised_rank_table_",scenario,"_S"))
  to_plot = to_plot[sort_col,1:samples]

  # vercotise for plotting
  data3 = fortify.zoo(zoo(to_plot), melt = TRUE)
  data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
  # data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
  data3$Mean = index[data3$Index][sort_col]  # data$shortfalls_norm_index[data3$Index][sort_col] #st_drop_geometry(tdwg3$benefit_1_S)[data3$Index][sort_col]
  data3 = data3[data3$Index <=cutoff,]

  png(paste0(basepath,"random_rank_scenario_",scenario,"_S.png"),
      width=20, height=25, units="cm", res = 300)
  par(mar=c(5.1, 7.1, 4.1, 2.1), mfrow=c(1,1))
  boxplot(Value~Index, data = data3, xlab="Rank", ylab="", #main= "summed normalised",
          # ylim=c(min(data3$Value),2),
          horizontal = T,
          las=1, cex=0, lty=1,yaxt="n")#, names = data3$Label[1:99], cex.names=0.5)

  axis(2, at=1:cutoff,labels = data3$Label[1:cutoff], cex.axis=0.6, las=2)

  points(data3$Index[1:cutoff], data3$Mean[1:cutoff],pch=19, col="brown")
  dev.off()

}


########################################################################
########################################################################
###   SCALED
########################################################################
########################################################################

samples = 10000
# scenario = 1
MEAN = data$SR_unknown_sc

# create random tables to fill in
randomised_unknown_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_nogeoloc_table = matrix(NA, nrow = length(MEAN), ncol= samples)
randomised_shortfalls_table = matrix(NA, nrow = length(MEAN), ncol= samples)


# generate random benefits within the expected bounds


# GENERATE RANDOM LINNEAN SHORTFALL
complete = which(!is.na(data$SR_unknown_sc))
for (rowi in complete){

  # generate a random number
  randomised_unknown_table[rowi,] = rnorm(n = samples,
                                          mean = data$SR_unknown_sc[rowi],
                                          sd = data$SR_unknown_sd_sc[rowi])
  # make sure it is under the upper limit
  success = FALSE
  while (!success) {
    UL_replace = randomised_unknown_table[rowi,] > data$SR_unknown_UL_sc[rowi]
    if (any(UL_replace)){
      randomised_unknown_table[rowi, which(UL_replace)] = rnorm(n = length(which(UL_replace)),
                                                                mean = data$SR_unknown_sc[rowi],
                                                                sd = data$SR_unknown_sd_sc[rowi])
    } else{ success = TRUE}
  }

  # make sure it is above the lower limit
  success = FALSE
  while (!success) {
    LL_replace = randomised_unknown_table[rowi,] < data$SR_unknown_LL_sc[rowi]
    if (any(LL_replace)){
      randomised_unknown_table[rowi, LL_replace] = rnorm(n = length(which(LL_replace)),
                                                         mean = st_drop_geometry(data$SR_unknown_sc)[rowi],
                                                         sd = st_drop_geometry(data$SR_unknown_sd_sc)[rowi])
    } else{ success = TRUE}
  }
}


# GENERATE RANDOM WALLACEAN SHORTFALL
complete = which(!is.na(data$SR_nogeoloc_sc))
for (rowi in complete){

  # generate a random number
  randomised_nogeoloc_table[rowi,] = rnorm(n = samples,
                                           mean = data$SR_nogeoloc_sc[rowi],
                                           sd = data$SR_nogeoloc_sd_sc[rowi])

  # make sure it is under the upper limit
  success = FALSE
  while (!success) {
    UL_replace = randomised_nogeoloc_table[rowi,] > data$SR_nogeoloc_UL_sc[rowi]
    if (any(UL_replace)){
      randomised_nogeoloc_table[rowi, UL_replace] = rnorm(n = length(which(UL_replace)),
                                                          mean = data$SR_nogeoloc_sc[rowi],
                                                          sd = data$SR_nogeoloc_sd_sc[rowi])
    } else{ success = TRUE}
  }

  # make sure it is above the lower limit
  success = FALSE
  while (!success) {
    LL_replace = randomised_nogeoloc_table[rowi,] < data$SR_nogeoloc_LL_sc[rowi]
    if (any(LL_replace)){
      randomised_nogeoloc_table[rowi, LL_replace] = rnorm(n = length(which(LL_replace)),
                                                          mean = data$SR_nogeoloc_sc[rowi],
                                                          sd = data$SR_nogeoloc_sd_sc[rowi])
    } else{ success = TRUE}
  }
}


#-----------------------------------------------------

# GENERATE THE SHORTFALLS
for (coli in 1:samples){
  randomised_shortfalls_table[,coli] = (normalize_fixed(randomised_unknown_table[,coli],0,max(data$SR_unknown_sc,na.rm=T)) +
                                          normalize_fixed(randomised_nogeoloc_table[,coli],0,max(data$SR_nogeoloc_sc,na.rm=T)))
}



# CALCULATE BENEFIT FOR ALL these randomly generated benefits
for (scenario in 1:9){

  assign(paste0("randomised_benefit_table_",scenario,"_S_sc"),
         (randomised_shortfalls_table/2 + get(paste0("weight_",scenario,"_S"))))

  # rank each column of randomised benefits in table
  temp_out = matrix(NA, nrow = length(MEAN), ncol= samples)
  temp_in = get(paste0("randomised_benefit_table_",scenario,"_S_sc"))
  for (coli in 1:samples){
    temp_out[,coli] = rank( - temp_in[,coli], na.last = "keep", ties.method = "first")
  }
  assign(paste0("randomised_rank_table_",scenario,"_S_sc"), temp_out)

  # calculate the mean benefit per country
  mean_benefit = NA
  for (rowi in 1:length(MEAN)){
    mean_benefit = c(mean_benefit, mean(get(paste0("randomised_benefit_table_",scenario,"_S_sc"))[rowi,],na.rm=T))
  }
  data[,paste0("mean_randomised_benefit_scenario_",scenario,"_S_sc")] = mean_benefit[-1]



  # calculate the mean rank per country
  mean_rank = NA
  for (rowi in 1:length(MEAN)){
    mean_rank = c(mean_rank, mean(get(paste0("randomised_rank_table_",scenario,"_S_sc"))[rowi,],na.rm=T))
  }
  data[,paste0("mean_randomised_rank_scenario_",scenario,"_S_sc")] = mean_rank[-1]



  # calculate spearkman's rank correlation
  to_keep = which(!is.na(data[,paste0("benefit_",scenario,"_S_sc")]) &
                    !is.na(data[,paste0("mean_randomised_benefit_scenario_",scenario,"_S_sc")]))
  spearman = NA
  spearman_significance = NA
  for (runi in 1:samples){
    out = cor.test(data[to_keep, paste0("benefit_",scenario,"_S_sc")],
                   get(paste0("randomised_benefit_table_",scenario,"_S_sc"))[to_keep,runi],
                   method="spearman")
    spearman = c(spearman,out$estimate)
    spearman_significance = c(spearman_significance,out$p.value)
  }
  assign(paste0("spearman_",scenario,"_S_sc"), spearman[-1])
  assign(paste0("spearman_significance_",scenario,"_S_sc"), spearman_significance[-1])

  # calculate kendall's rank correlation
  kendall = NA
  kendall_significance = NA
  for (runi in 1:samples){

    out = cor.test(data[to_keep, paste0("benefit_",scenario,"_S_sc")],
                   get(paste0("randomised_benefit_table_",scenario,"_S_sc"))[to_keep,runi],
                   method="kendall")
    kendall = c(kendall,out$estimate)
    kendall_significance = c(kendall_significance,out$p.value)
  }
  assign(paste0("kendall_",scenario,"_S_sc"), kendall[-1])
  assign(paste0("kendall_significance_",scenario,"_S_sc"), kendall_significance[-1])

  # save(get(paste0("spearman_",scenario,"_S_sc")),
  #      get(paste0("spearman_significance_",scenario,"_S_sc")),
  #      get(paste0("kendall_",scenario,"_S_sc")),
  #      get(paste0("kendall_significance_",scenario,"_S_sc")),
  #      get(paste0("randomised_benefit_table_",scenario,"_S_sc")),
  #      get(paste0("randomised_rank_table_",scenario,"_S_sc")),
  #      file = paste0(basepath, "randomisation_outputs_",scenario,"_S_sc.RData"))
  save(list = c(paste0("spearman_",scenario,"_S_sc"),
                paste0("spearman_significance_",scenario,"_S_sc"),
                paste0("kendall_",scenario,"_S_sc"),
                paste0("kendall_significance_",scenario,"_S_sc"),
                paste0("randomised_benefit_table_",scenario,"_S_sc"),
                paste0("randomised_rank_table_",scenario,"_S_sc")),
       file = paste0(basepath, "randomisation_outputs_",scenario,"_S_sc.RData"))

}



#
# wilcox.test(data[to_keep, paste0("benefit_",scenario,"_S_sc")],
#             data[to_keep,paste0("benefit_",scenario,"_M")])
#
# wilcox.test(data[to_keep, paste0("Rank_scenario_",scenario,"_M")],
#             data[to_keep,paste0("Rank_scenario_",scenario,"_S_sc")])
#

#==================================================
# Make a boxplot for each scenario
#==================================================
cutoff = 50

for (scenario in 1:9){
  # get the order
  index = data[,paste0("benefit_",scenario,"_S_sc")]
  sort_col = order(-index)
  to_plot = get(paste0("randomised_benefit_table_",scenario,"_S_sc"))
  to_plot = to_plot[sort_col,1:samples]

  # vercotise for plotting
  data3 = fortify.zoo(zoo(to_plot), melt = TRUE)
  data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
  # data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S_sc")])[paste0("benefit_",scenario,"_S_sc")][data3$Index][sort_col]
  data3$Mean = index[data3$Index][sort_col]  # data$shortfalls_norm_index[data3$Index][sort_col] #st_drop_geometry(tdwg3$benefit_1_S)[data3$Index][sort_col]
  data3 = data3[data3$Index <=cutoff,]

  png(paste0(basepath,"random_benefit_scenario_",scenario,"_S_sc.png"),
      width=25, height=20, units="cm", res = 300)
  par(mar=c(7.1, 4.1, 2.1, 2.1), mfrow=c(1,1))
  boxplot(Value~Index, data = data3, xlab="", ylab="Expected benefit", #main= "summed normalised",
          ylim=c(min(data3$Value),2),
          las=2, cex=0, lty=1,xaxt="n")#, names = data3$Label[1:99], cex.names=0.5)

  axis(1, at=1:cutoff,labels = data3$Label[1:cutoff], cex.axis=0.6, las=2)

  points(data3$Index[1:cutoff], data3$Mean[1:cutoff],pch=19, col="brown")
  dev.off()

  # ## use a white border of size 0.5 unit to separate the tiles
  # gg<- ggplot(data3, aes(x=as.factor(Index), y=Value, fill=Index, color=Index), show.legend = FALSE) +
  #   # geom_tile(color="white", size=0.01) +
  #   # geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  #   geom_boxplot(outlier.shape=NA) +
  #   scale_x_discrete(labels= unique(data3$Label)) +
  #   ylab("Rank") +
  #   xlab("Botanical Country") +
  #   coord_flip() +
  #   scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  #   scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  #   theme_classic() +
  #   geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  #   theme(legend.position = "none")
  #
  #
  # gg

  #==================================================
  # plot rank randomisation
  #==================================================
  # get the order
  index = data[,paste0("Rank_scenario_",scenario,"_S_sc")]
  sort_col = order(index)
  to_plot = get(paste0("randomised_rank_table_",scenario,"_S_sc"))
  to_plot = to_plot[sort_col,1:samples]

  # vercotise for plotting
  data3 = fortify.zoo(zoo(to_plot), melt = TRUE)
  data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
  # data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S_sc")])[paste0("benefit_",scenario,"_S_sc")][data3$Index][sort_col]
  data3$Mean = index[data3$Index][sort_col]  # data$shortfalls_norm_index[data3$Index][sort_col] #st_drop_geometry(tdwg3$benefit_1_S)[data3$Index][sort_col]
  data3 = data3[data3$Index <=cutoff,]

  png(paste0(basepath,"random_rank_scenario_",scenario,"_S_sc.png"),
      width=20, height=25, units="cm", res = 300)
  par(mar=c(5.1, 7.1, 4.1, 2.1), mfrow=c(1,1))
  boxplot(Value~Index, data = data3, xlab="Rank", ylab="", #main= "summed normalised",
          # ylim=c(min(data3$Value),2),
          horizontal = T,
          las=1, cex=0, lty=1,yaxt="n")#, names = data3$Label[1:99], cex.names=0.5)

  axis(2, at = 1:cutoff, labels = data3$Label[1:cutoff], cex.axis=0.6, las=2)

  points(data3$Index[1:cutoff], data3$Mean[1:cutoff], pch=19, col="brown")
  dev.off()

}


#######################################################################################


print("Spearman rank correlations")
for (scenario in 1:9){
  print(summary(get(paste0("spearman_",scenario,"_S"))))
}

print("Scaled spearman rank correlations")
for (scenario in 1:9){
  print(summary(get(paste0("spearman_",scenario,"_S_sc"))))
}


#####################################################
# Save
tdwg3 = tdwg3 %>% left_join(data)

save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data_randomisation.RData"))
write.csv(st_drop_geometry(tdwg3), paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/variables_table_randomisation.csv"))
