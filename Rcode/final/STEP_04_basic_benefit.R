#PC1 Lower is richer
#PC2 lower has more protection
# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
library(plotly)
library(tidyverse)
library(truncnorm)
library(sf)

# load the data
load("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData")

data =  st_drop_geometry(tdwg3)

#######################################################################################################
#
#                        FUNCTIONS
#
#######################################################################################################
normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}
normalize_fixed <- function(x, min_val = 0, max_val = 1000) {
  return ((x - min_val) / (max_val - min_val))
}

create_table <- function(samples = 100, darkspots, cumul=T){
  # create random table
  random_ranks_table = matrix(rnorm(length(darkspots)* samples, 0, 2), nrow = length(darkspots), ncol= samples)
  random_benefits_table = matrix(NA, nrow = length(darkspots), ncol= samples)

  for (coli in 1:samples){
    # rank the random numbers in each column
    random_ranks_table[,coli] = rank(-random_ranks_table[,coli], na.last = "keep", ties.method = "first")
    for (rowi in 1:length(darkspots)){
      # use the rank to look up the matching shortfall
      random_benefits_table[rowi,coli] = darkspots[random_ranks_table[rowi,coli]]
    }
    # random_benefits_table[,coli] = cumsum( random_benefits_table[,coli])
  }
  random_benefits_table = ifelse(is.na(random_benefits_table), 0, random_benefits_table)

  if(cumul == TRUE){
    #transform into a cumulative sum
    for (coli in 1:samples){
      random_benefits_table[,coli] = cumsum( random_benefits_table[,coli])
    }
  }
  return(random_benefits_table)
}

############################################################################################
# SCENARIOS
############################################################################################


# 1) Baseline
# 2) low income group
# 3) high income group
# 4) low protection
# 5) high rotection
# 6) low income group & low protection
# 7) low income group & high protection
# 8) high income group & low protection
# 9) high income group & high protection



############################################################################################
#  weigting as a SUM
############################################################################################

# Prioritise areas with the most species left to discover
weight_1_S = rep(1, length(data$SR_unknown))

# prioritise areas with the most species in low income group places to discover
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




########################################################################
########################################################################
###   Estimate benefits and Ranks for median upper and lower
########################################################################
########################################################################


# am not normalising because UL and LL themselves are normalise relative to each other, and don't want to loose that

for (scenario in 1:9){

  #--------------------------------
  # UNSCALED / SUM
  data[,paste0("benefit_",scenario,"_S")] = ((normalize_fixed(data$SR_nogeoloc,0,max(data$SR_nogeoloc,na.rm=T)) +
                                               normalize_fixed(data$SR_unknown,0,max(data$SR_unknown,na.rm=T)))/2 +
                                               get(paste0("weight_",scenario,"_S")))
    # normalise(data$shortfalls_norm_index) + get(paste0("weight_",scenario,"_S"))
    #normalise(normalize_fixed(data$SR_nogeoloc) + normalize_fixed(data$SR_unknown)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S")]),
                                                   na.last = "keep", ties.method = "first")

  #--------------------------------
  # SCALED / SUM
  data[,paste0("benefit_",scenario,"_S_sc")] = ((normalize_fixed(data$SR_nogeoloc_sc,0,max(data$SR_nogeoloc_sc,na.rm=T)) +
                                                  normalize_fixed(data$SR_unknown_sc,0,max(data$SR_unknown_sc,na.rm=T)))/2 +
                                                  get(paste0("weight_",scenario,"_S")))
    #normalise(normalise(data$SR_nogeoloc_sc) + normalise(data$SR_unknown_sc)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S_sc")]),
                                                        na.last = "keep", ties.method = "first")
}

# merge the data
tdwg3 = tdwg3 %>% left_join(data)




#####################################################
# Save

# save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/REV_app_data.RData"))
save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))
write.csv(st_drop_geometry(tdwg3), paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/variables_table.csv"))


# save to shp but note that long data file names don't save properly so best to load app_data.R
st_write(tdwg3, 'C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/model_outputs.shp',layer = NULL, delete_layer = TRUE)



