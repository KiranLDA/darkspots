library(tidyverse)
library(vdemdata)
library(rworldmap)
library(sf)

basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"

# functions
normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}


#Load the tdwg shp which is the base for everything
tdwg3 <- st_read(dsn = paste0(basepath, "REVISION_1/level3"),
                 layer = "level3")


# map country codes to tdwg
wgsrpd_mapping = read.csv(paste0(basepath,"country_tdwg3_map_KD.csv"))
wgsrpd_mapping$ISO_code[is.na(wgsrpd_mapping$ISO_code)] <-"NA"


tdwg3 <- tdwg3 %>%
  left_join(wgsrpd_mapping[c("LEVEL3_COD","COUNTRY","ISO_code")])


# Add descriptions and discoveries from LiteRate
discoveries = read.csv(paste0(basepath,"skyline_model/v6/Species_sampling_rates_all.csv"))#Discovery_rates.csv"))
descriptions = read.csv(paste0(basepath,"skyline_model/v6/Species_description_rates_all.csv"))#Description_rates.csv"))
left_to_sample = read.csv(paste0(basepath,"skyline_model/v6/Species_to_be_sampled_all.csv"))


# Get mean over last 5 years
my.mean = function(x) ifelse( !all(is.na(x)), mean(x, na.rm=T), NA)

discoveries[, "mean"] <- apply(discoveries[, (ncol(discoveries)-21):ncol(discoveries)],1,#c("X2018","X2019","X2020","X2021","X2022")], 1,
                               my.mean)
descriptions[, "mean"] <- apply(descriptions[, (ncol(descriptions)-21):ncol(descriptions)],1,#c("X2018","X2019","X2020","X2021","X2022")], 1,
                                my.mean)

discoveries = discoveries[,c("X", "mean")]
descriptions = descriptions[,c("X", "mean")]
left_to_sample = left_to_sample[,c("X","X2021")]

colnames(discoveries) = c("LEVEL3_COD", "discoveries")
colnames(descriptions) = c("LEVEL3_COD", "descriptions")
colnames(left_to_sample) = c("LEVEL3_COD", "spp_left")

tdwg3 =  tdwg3 %>%
  left_join(discoveries)
tdwg3 =  tdwg3 %>%
  left_join(descriptions)
tdwg3 =  tdwg3 %>%
  left_join(left_to_sample)




##################################################
## Get biodiversity data

path = paste0(basepath,"wcvp_descriptions_discoveries/")
files  = list.files(path, full.names=TRUE)
tot_exp_spp_wcvp = NA
left_spp_wcvp = NA
LEVEL3_COD = NA
for (id in 1:length(files)){
  tempo = read.csv(files[id], sep="\t")
  tot_exp_spp_wcvp = c(tot_exp_spp_wcvp,nrow(tempo))
  left_spp_wcvp = c(left_spp_wcvp,nrow(tempo[tempo$time_ofcollection == 2022,]))
  LEVEL3_COD = c(LEVEL3_COD, substr(files[id],  nchar(files[id])-6, nchar(files[id])-4))
}

bio_data = data.frame(LEVEL3_COD,
                      tot_exp_spp_wcvp,
                      left_spp_wcvp)
bio_data = bio_data[-1,]

tdwg3 =  tdwg3 %>%
  left_join(bio_data)




# add  LAt Long
sf_use_s2(FALSE)
df <- tdwg3 %>% st_centroid() %>% mutate(Long = sf::st_coordinates(.)[,1],
                                         Lat = sf::st_coordinates(.)[,2])# %>% as.data.frame()

tdwg3 <- tdwg3 %>% left_join(as.data.frame(df)[c("LEVEL3_COD", "Long","Lat")])




####################################################################
#load in SDG data
tdwg3 = tdwg3 %>% mutate(ISO_code_3 = MazamaSpatialUtils::iso2ToIso3(tdwg3$ISO_code))


SDG_goals = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/spatial/DARKSPOTS/country_year_sdg_metrics.csv")
SDG_goals = SDG_goals[,1:24]

# subset 2021 for now
SDG_goals = SDG_goals[SDG_goals$Year == 2021,]
colnames(SDG_goals)[colnames(SDG_goals) == "Country.Code.ISO3"] = "ISO_code_3"

tdwg3 =  tdwg3 %>%
  left_join(SDG_goals)



#################################################################
# load in Ian's simulations

shortfalls_raw <- read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REV_shortfalls_not_rescaled_predictions_with_uncertainty_bounds.csv")
shortfalls_raw <- read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/old_REV_shortfalls_not_rescaled_predictions_with_uncertainty_bounds.csv")


outliers= c("NWG","BOR","ECU", "BOL","BZN","PAN")

# remove the old columns that weren't working
shortfalls_raw = shortfalls_raw[,c(1:5,10:13)]


# RESCALE THE DATA
# write the function
SAR <- function(x,y, ref_area_km2=10000){ # Species-Area-Regression
  # glm
  mod <- stats::glm(x ~ log(y), family=poisson(link="log"), start = c(0.5, 0.5))
  # non linear regression
  mod2 <- stats::nls(x ~ c*y^z, start=list(c=exp(coef(mod)[1]), z=coef(mod)[2]))
  z=coef(mod2)[2] # scaling area exponent
  (x*ref_area_km2^z)/(y^z)
}



areas = read.csv(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/", "twdg3_land_area.csv"))

# rescale the shortfalls
shortfalls_rescaled = shortfalls_raw
cols = colnames(shortfalls_raw)[-1]

######################
# Rescale together

# linnean
vec = c(shortfalls_rescaled[,"SR_unknown_LL"],
        shortfalls_rescaled[,"SR_unknown_sd"],
        shortfalls_rescaled[,"SR_unknown"],
        shortfalls_rescaled[,"SR_unknown_UL"])
vec_a = c(areas$land_area,areas$land_area,areas$land_area,areas$land_area)
complete = !is.na(vec)
vec_sc = vec
vec_sc[complete] = SAR(vec[complete], vec_a[complete])
df1 = matrix(vec_sc,ncol=4)
colnames(df1) = c("SR_unknown_LL_sc", "SR_unknown_sd_sc","SR_unknown_sc","SR_unknown_UL_sc")

# wallacean
vec = c(shortfalls_rescaled[,"SR_nogeoloc_LL"],
        shortfalls_rescaled[,"SR_nogeoloc_sd"],
        shortfalls_rescaled[,"SR_nogeoloc"],
        shortfalls_rescaled[,"SR_nogeoloc_UL"])
vec_a = c(areas$land_area,areas$land_area,areas$land_area,areas$land_area)
complete = !is.na(vec)
vec_sc = vec
vec_sc[complete] = SAR(vec[complete], vec_a[complete])
df2 = matrix(vec_sc,ncol=4)
colnames(df2) = c("SR_nogeoloc_LL_sc", "SR_nogeoloc_sd_sc","SR_nogeoloc_sc","SR_nogeoloc_UL_sc")

# together
df= cbind(df1,df2)
replace_cols = c("SR_unknown_LL_sc", "SR_unknown_sd_sc","SR_unknown_sc","SR_unknown_UL_sc",
                 "SR_nogeoloc_LL_sc", "SR_nogeoloc_sd_sc","SR_nogeoloc_sc","SR_nogeoloc_UL_sc")
colnames(shortfalls_rescaled) = paste(colnames(shortfalls_rescaled),"sc",sep="_")
colnames(shortfalls_rescaled)[1] = "LEVEL3_COD"
for (coli in replace_cols){
  shortfalls_rescaled[,coli] = df[,coli]
}


### Only normalise the raw data

`%+%` <- function(x, y, na.rm=TRUE)  mapply(sum, x, y, MoreArgs = list(na.rm = na.rm))

df1 = c(shortfalls_raw$SR_unknown_LL,
        shortfalls_raw$SR_unknown_sd,
        shortfalls_raw$SR_unknown,
        shortfalls_raw$SR_unknown_UL)
df1 = matrix(scales::rescale((df1), c(0, 1)) , ncol= 4)
colnames(df1) = c("SR_unknown_norm_LL", "SR_unknown_norm_SD","SR_unknown_norm_median","SR_unknown_norm_UL")

df2 = c(shortfalls_raw$SR_nogeoloc_LL,
        shortfalls_raw$SR_nogeoloc_sd,
        shortfalls_raw$SR_nogeoloc,
        shortfalls_raw$SR_nogeoloc_UL)
df2 = matrix(scales::rescale((df2), c(0, 1)) , ncol= 4)
colnames(df2) = c("SR_nogeoloc_norm_LL", "SR_nogeoloc_norm_SD","SR_nogeoloc_norm_median","SR_nogeoloc_norm_UL")

df = matrix(df1 %+% df2, ncol= 4)
df = replace(df, df==0, NA)
colnames(df) = c("shortfalls_norm_index_LL", "shortfalls_norm_index_SD","shortfalls_norm_index_median","shortfalls_norm_index_UL")
shortfalls_raw =  cbind(shortfalls_raw,df1,df2,df)

# put the non-normalised metric there

shortfalls_raw$SR_nogeoloc_norm = scales::rescale(shortfalls_raw$SR_nogeoloc, c(0, 1))
shortfalls_raw$SR_unknown_norm = scales::rescale(shortfalls_raw$SR_unknown, c(0, 1))
shortfalls_raw$shortfalls_norm_index = shortfalls_raw$SR_nogeoloc_norm %+% shortfalls_raw$SR_unknown_norm


### Only normalise the rescaled data
df1 = c(shortfalls_rescaled$SR_unknown_LL_sc,
        shortfalls_rescaled$SR_unknown_sd_sc,
        shortfalls_rescaled$SR_unknown_sc,
        shortfalls_rescaled$SR_unknown_UL_sc)
df1 = matrix(scales::rescale((df1), c(0, 1)) , ncol= 4)
colnames(df1) = c("SR_unknown_norm_LL_sc", "SR_unknown_norm_SD_sc","SR_unknown_norm_median_sc","SR_unknown_norm_UL_sc")

df2 = c(shortfalls_rescaled$SR_nogeoloc_LL_sc,
        shortfalls_rescaled$SR_nogeoloc_sd_sc,
        shortfalls_rescaled$SR_nogeoloc_sc,
        shortfalls_rescaled$SR_nogeoloc_UL_sc)
df2 = matrix(scales::rescale((df2), c(0, 1)) , ncol= 4)
colnames(df2) = c("SR_nogeoloc_norm_LL_sc", "SR_nogeoloc_norm_SD_sc","SR_nogeoloc_norm_median_sc","SR_nogeoloc_norm_UL_sc")

df = matrix(df1 %+% df2, ncol= 4)
df = replace(df, df==0, NA)
colnames(df) = c("shortfalls_norm_index_LL_sc", "shortfalls_norm_index_SD_sc","shortfalls_norm_index_median_sc","shortfalls_norm_index_UL_sc")
shortfalls_rescaled =  cbind(shortfalls_rescaled,df1,df2,df)

# merge the data
tdwg3 =  tdwg3 %>%
  left_join(shortfalls_raw)
tdwg3 =  tdwg3 %>%
  left_join(shortfalls_rescaled)


save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))
