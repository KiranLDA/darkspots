library(tidyverse)
library(vdemdata)
library(rworldmap)
library(sf)

# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"

normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}


#Load the tdwg shp which is the base for everything
tdwg3 <- st_read(dsn = paste0(basepath, "REVISION_1/level3"),
                        layer = "level3")


# PROJ <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#
# sf_use_s2(FALSE)
# tdwg3 = st_transform(tdwg3, crs = PROJ)



travel_times <- st_read(dsn = paste0(basepath, "tdwg_l3_travel_times"),
                        layer = "tdwg_l3_travel_times")
herbaria <- st_read(dsn = paste0(basepath, "tdwg_l3_instit_from_coords"),
                               layer = "tdwg_l3_instit_from_coords")





# OLD with rgdal
# tdwg3 <- rgdal::readOGR(dsn = paste0(basepath, "level3"),
#                         layer = "level3")
# travel_times <- rgdal::readOGR(dsn = paste0(basepath, "tdwg_l3_travel_times"),
#                                layer = "tdwg_l3_travel_times")
# herbaria <- rgdal::readOGR(dsn = paste0(basepath, "tdwg_l3_instit_from_coords"),
#                            layer = "tdwg_l3_instit_from_coords")


# load the coutry-tdwg3 table match
# load("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/darkspots_results_plots/wgsrpd_mapping.RData")



# # map country codes to tdwg
# country_codes = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/darkspots_results_plots/country_mapping_KD_short.csv")
# # make sure namibia is N
# country_codes$ISO_code[is.na(country_codes$ISO_code)] <-"NA"
# country_codes$COUNTRY=NA
# for (row in 1:length(country_codes$ISO_code)){
#   country_codes$COUNTRY[row] = isoToName(country_codes$ISO_code[row])
# }
#
#
# write.csv(country_codes, "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/darkspots_results_plots/country_tdwg3_map_KD.csv")
# wgsrpd_mapping = country_codes

wgsrpd_mapping = read.csv(paste0(basepath,"country_tdwg3_map_KD.csv"))
wgsrpd_mapping$ISO_code[is.na(wgsrpd_mapping$ISO_code)] <-"NA"


# map country codes to tdwg
# wgsrpd_mapping = wgsrpd_mapping[-which(duplicated(wgsrpd_mapping[c("LEVEL3_COD")])),]
# tdwg3@data = base::merge(tdwg3@data, wgsrpd_mapping[c("LEVEL3_COD","COUNTRY","ISO_code")], by= "LEVEL3_COD")
# tdwg3@data =  tdwg3@data %>%
#   left_join(herbaria@data[c("LEVEL3_COD", "num_instit")])
# tdwg3@data =  tdwg3@data %>%
#   left_join(travel_times@data[c("LEVEL3_COD",  "Prop_6hrs" , "Perc_6hrs")])


tdwg3 <- tdwg3 %>%
  left_join(wgsrpd_mapping[c("LEVEL3_COD","COUNTRY","ISO_code")])

tdwg3 <- tdwg3 %>%
  left_join(st_drop_geometry(herbaria[c("LEVEL3_COD", "num_instit")]))

tdwg3 <- tdwg3 %>%
  left_join(st_drop_geometry(travel_times[c("LEVEL3_COD",  "Prop_6hrs" , "Perc_6hrs")]))




# write.csv(test,"C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/darkspots_results_plots/country_lest_test.csv")
# Add descriptions and discoveries from Daniele
discoveries = read.csv(paste0(basepath,"skyline_model/v6/Species_sampling_rates_all.csv"))#Discovery_rates.csv"))
descriptions = read.csv(paste0(basepath,"skyline_model/v6/Species_description_rates_all.csv"))#Description_rates.csv"))
left_to_sample = read.csv(paste0(basepath,"skyline_model/v6/Species_to_be_sampled_all.csv"))

# #complement the new with the old
# discoveries_old = read.csv(paste0(basepath,"skyline_model/v4/Species_sampling_rates_all.csv"))#Discovery_rates.csv"))
# descriptions_old = read.csv(paste0(basepath,"skyline_model/v4/Species_description_rates_all.csv"))#Description_rates.csv"))
# left_to_sample_old = read.csv(paste0(basepath,"skyline_model/v4/Species_to_be_sampled_all.csv"))
#
# # write.csv(left_to_sample_old$X[!(left_to_sample_old$X %in% left_to_sample$X)],
# #           paste0(basepath,"REVISION_1/missing_data.csv"))
#
# discoveries = rbind(discoveries, discoveries_old[!(discoveries_old$X %in% discoveries$X),])
# descriptions = rbind(descriptions, descriptions_old[!(descriptions_old$X %in% descriptions$X),])
# left_to_sample = rbind(left_to_sample, left_to_sample_old[!(left_to_sample_old$X %in% left_to_sample$X),])


# # get the last year
# discoveries = discoveries[,c("X", "X19")]
# descriptions = descriptions[,c("X", "X19")]
#


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

# old gdal
# tdwg3@data =  tdwg3@data %>%
#   left_join(discoveries)
# tdwg3@data =  tdwg3@data %>%
#   left_join(descriptions)
# tdwg3@data =  tdwg3@data %>%
#   left_join(left_to_sample)



#VDEM data:
# vdem_data = vdemdata::vdem[c("country_name", "year",
#                              "e_peaveduc", "e_peedgini", #education
#                              "e_area", # land area
#                              "e_cow_exports", "e_cow_imports", # imports and exports
#                              "e_gdp", "e_gdppc", #gdp
#                              "e_miinflat", #inflation
#                              "e_pop", # population
#                              "e_total_resources_income_pc", #natural resource production per capita
#                              "e_miferrat", #fertility rate
#                              "e_mipopula", #population total
#                              "e_miurbani", #urbanisation rate
#                              "e_miurbpop", #urban population
#                              "e_pelifeex", #life expectancy
#                              "e_peinfmor", # infant mortality
#                              "e_pematmor", #maternity mortality
#                              "v2cldmovew",# Freedom of domestic movement for women (C) (
#                              "v2cldmovem", #  Freedom of domestic movement for men
#                              "v2cafres", #  Freedom to research and teach
#                              "e_civil_war", # number of civil wars
#                              "e_miinteco", # international armed conflict
#                              "e_miinterc", # internal armed conflict
#                              "e_pt_coup" #coups d'etat
# )]
#
#
#
# #sumarise vdem accross years
# vdem_data_sumary = data.frame(vdem_data[-which(duplicated(vdem_data[c("country_name")])),])
# vdem_data_sumary = vdem_data_sumary[ , !(names(vdem_data_sumary) %in% "year")]
# vdem_data_sumary[, 2:ncol(vdem_data_sumary)] = NA
#
# for(country in vdem_data_sumary$country_name){
#   # country = "Afghanistan"
#   table = vdem_data[which(vdem_data$country_name == country), ]
#   for (col_idx in 2:ncol(vdem_data_sumary)){
#     # col_idx = 22
#     subtable  = table[,c(1,2, (col_idx+1))]
#     subtable = subtable[complete.cases(subtable), ]
#     subtable = subtable[order(subtable$year, decreasing = TRUE),]
#     if (colnames(subtable)[3] %in% c("e_civil_war", "e_miinteco", "e_miinterc")){
#       # sum all conflicts over last 20 years
#       vdem_data_sumary[which(vdem_data_sumary$country_name == country), col_idx] = sum(subtable[1:20,3])
#     } else {
#       # get most recent data
#       vdem_data_sumary[which(vdem_data_sumary$country_name == country), col_idx] = subtable[1,3]
#
#     }
#   }
# }
#
#
# #joing the vdem data to the tdwg3 data
# colnames(vdem_data_sumary)[colnames(vdem_data_sumary) == "country_name"] = "COUNTRY"
# tdwg3@data =  tdwg3@data %>%
#     left_join(vdem_data_sumary)


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

# old gdal
# tdwg3@data =  tdwg3@data %>%
#   left_join(bio_data)




# # load the sustainable development codes
# SDG = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/spatial/DARKSPOTS/Data_Extract_From_Sustainable_Development_Goals_(SDGs)/aceb7e8c-e68b-4563-b925-08cd7d0c651b_Data.csv")
#
#
# # poverty_vars <- vdemdata::extract_vdem(label_pattern = "poverty")
#
#
#
#
# variables = c("Annual deforestation (% of change) - (for online table only)",
#               "Battle-related deaths (number of people)",
#               "Terrestrial protected areas (% of total land area)",
#               "Researchers in R&D (per million people)",
#               "Prevalence of moderate or severe food insecurity in the population (%)",
#               "Plant species (higher), threatened",
#               "Multidimensional poverty headcount ratio (% of total population)")
#
# toplot = "Battle-related deaths (number of people)"
#
#
# SDG_subset = SDG[SDG[,"Series.Name"] %in% variables,
#                  c("ï..Country.Name","Series.Name",
#                    "X2015..YR2015.","X2016..YR2016.",
#                    "X2017..YR2017.","X2018..YR2018.",
#                    "X2019..YR2019.","X2020..YR2020.")]
#
# my.max = function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
#
# for (var in variables){
#   SDG_subset = SDG[SDG[,"Series.Name"] %in% var,
#                    c("ï..Country.Name","Series.Name",
#                      "X2015..YR2015.","X2016..YR2016.",
#                      "X2017..YR2017.","X2018..YR2018.",
#                      "X2019..YR2019.","X2020..YR2020.")]
#   SDG_subset[, "max"] <- apply(SDG_subset[, c("X2015..YR2015.","X2016..YR2016.",
#                                               "X2017..YR2017.","X2018..YR2018.",
#                                               "X2019..YR2019.","X2020..YR2020.")], 1,
#                                my.max)
#   table = SDG_subset[,c("ï..Country.Name", "max")]
#     table = table[-which(duplicated(table[c("ï..Country.Name")])),]
#   }
#
#   colnames(table) = c("COUNTRY", var)
#   tdwg3@data =  tdwg3@data %>%
#     left_join(table)
#
# }

# df = tdwg3 %>%
#   dplyr::mutate(Long = sf::st_centroid(.)[,1],
#                 Lat = sf::st_centroid(.)[,2])

sf_use_s2(FALSE)
# add the coordinates
df <- tdwg3 %>% st_centroid() %>% mutate(Long = sf::st_coordinates(.)[,1],
                                         Lat = sf::st_coordinates(.)[,2])# %>% as.data.frame()

tdwg3 <- tdwg3 %>% left_join(as.data.frame(df)[c("LEVEL3_COD", "Long","Lat")])





# tdwg3@data$Lat <- coordinates(tdwg3)[,1]
# tdwg3@data$Long <- coordinates(tdwg3)[,2]




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


# shortfalls <- read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/shortfalls_rescaled_predictions_with uncertainty_bounds.csv")

# colnames(shortfalls_rescaled) = paste(colnames(shortfalls_rescaled),"sc",sep="_")
# colnames(shortfalls_rescaled)[1] = "LEVEL3_COD"

shortfalls_raw <- read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REV_shortfalls_not_rescaled_predictions_with_uncertainty_bounds.csv")


# shortfalls <- read.csv(paste0(basepath,"shortfalls_normalised_darkhot_2022_12_13.csv"))
# shortfalls <- read.csv(paste0(basepath,"shortfalls_not_rescaled_normalised_darkhot_2023_01_16.csv"))

# tdwg3 =  tdwg3 %>%
#   left_join(shortfalls_rescaled)

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

# rescale the shortfalls
shortfalls_rescaled = shortfalls_raw
cols = colnames(shortfalls_raw)[-1]

# rescale independently
# for(coli in cols){
#   complete_rows = which(!is.na(shortfalls_rescaled[, coli]))
#   shortfalls_rescaled[complete_rows, coli] = SAR(shortfalls_rescaled[complete_rows, coli], areas$land_area[complete_rows])
# }


# rescale together
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
#overwrite the old data
colnames(shortfalls_rescaled) = c("LEVEL3_COD", "SR_unknown_LL_sc", "SR_unknown_sd_sc",
                                  "SR_unknown_median_sc","SR_unknown_UL_sc", "SR_nogeoloc_LL_sc",
                                  "SR_nogeoloc_sd_sc","SR_nogeoloc_median_sc","SR_nogeoloc_UL_sc")
# shortfalls_rescaled[, cols] = sweep(shortfalls_raw[,cols], 1, (st_area(tdwg3)/1e10), FUN = "/" )


#

### Log and normalise the raw data
# linnean
# df1 = normalise(log(shortfalls_raw[c("SR_unknown_LL","SR_unknown_sd", "SR_unknown","SR_unknown_UL")]+1))
# colnames(df1) = c("SR_unknown_lognorm_LL", "SR_unknown_lognorm_SD","SR_unknown_lognorm","SR_unknown_lognorm_UL")
#
# # wallacean
# df2 = normalise(log(shortfalls_raw[c("SR_nogeoloc_LL", "SR_nogeoloc_sd","SR_nogeoloc","SR_nogeoloc_UL")]+1))
# colnames(df2) = c("SR_nogeoloc_lognorm_LL", "SR_nogeoloc_lognorm_SD","SR_nogeoloc_lognorm","SR_nogeoloc_lognorm_UL")
#
# # calculate index
# df = df1 + df2
# colnames(df) = c("shortfalls_lognorm_index_LL", "shortfalls_lognorm_index_SD","shortfalls_lognorm_index","shortfalls_lognorm_index_UL")
# shortfalls_raw =  cbind(shortfalls_raw,df1,df2,df)



# #### Log and normalise the data rescaled data
# # linnean
# df1 = normalise(log(shortfalls_rescaled[c("SR_unknown_LL_sc","SR_unknown_sd_sc", "SR_unknown_sc","SR_unknown_UL_sc")]+1))
# colnames(df1) = c("SR_unknown_lognorm_LL_sc", "SR_unknown_lognorm_SD_sc","SR_unknown_lognorm_sc","SR_unknown_lognorm_UL_sc")
#
# # wallacean
# df2 = normalise(log(shortfalls_rescaled[c("SR_nogeoloc_LL_sc", "SR_nogeoloc_sd_sc","SR_nogeoloc_sc","SR_nogeoloc_UL_sc")]+1))
# colnames(df2) = c("SR_nogeoloc_lognorm_LL_sc", "SR_nogeoloc_lognorm_SD_sc","SR_nogeoloc_lognorm_sc","SR_nogeoloc_lognorm_UL_sc")
#
# # calculate index
# df = df1 + df2
# colnames(df) = c("shortfalls_lognorm_index_LL_sc", "shortfalls_lognorm_index_SD_sc","shortfalls_lognorm_index_sc","shortfalls_lognorm_index_UL_sc")
# shortfalls_rescaled =  cbind(shortfalls_rescaled, df1,df2,df)

### Only normalise the raw data
df1 = normalise(shortfalls_raw[c("SR_unknown_LL","SR_unknown_sd", "SR_unknown","SR_unknown_UL")])
colnames(df1) = c("SR_unknown_norm_LL", "SR_unknown_norm_SD","SR_unknown_norm","SR_unknown_norm_UL")

df2 = normalise(shortfalls_raw[c("SR_nogeoloc_LL", "SR_nogeoloc_sd","SR_nogeoloc","SR_nogeoloc_UL")])
colnames(df2) = c("SR_nogeoloc_norm_LL", "SR_nogeoloc_norm_SD","SR_nogeoloc_norm","SR_nogeoloc_norm_UL")

df = df1 + df2
colnames(df) = c("shortfalls_norm_index_LL", "shortfalls_norm_index_SD","shortfalls_norm_index","shortfalls_norm_index_UL")
shortfalls_raw =  cbind(shortfalls_raw,df1,df2,df)


### Only normalise the rescaled data
df1 = normalise((shortfalls_rescaled[c("SR_unknown_LL_sc","SR_unknown_sd_sc", "SR_unknown_median_sc","SR_unknown_UL_sc")]))
colnames(df1) = c("SR_unknown_norm_LL_sc", "SR_unknown_norm_SD_sc","SR_unknown_norm_median_sc","SR_unknown_norm_UL_sc")

df2 = normalise((shortfalls_rescaled[c("SR_nogeoloc_LL_sc", "SR_nogeoloc_sd_sc","SR_nogeoloc_median_sc","SR_nogeoloc_UL_sc")]))
colnames(df2) = c("SR_nogeoloc_norm_LL_sc", "SR_nogeoloc_norm_SD_sc","SR_nogeoloc_norm_median_sc","SR_nogeoloc_norm_UL_sc")

df = df1 + df2
colnames(df) = c("shortfalls_norm_index_LL_sc", "shortfalls_norm_index_SD_sc","shortfalls_norm_index_median_sc","shortfalls_norm_index_UL_sc")
shortfalls_rescaled =  cbind(shortfalls_rescaled,df1,df2,df)


shortfalls_rescaled_ind <- read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REV_shortfalls_rescaled_predictions_with_uncertainty_bounds.csv")

shortfalls_rescaled$SR_unknown_sc = shortfalls_rescaled_ind$SR_unknown
shortfalls_rescaled$SR_nogeoloc_sc = shortfalls_rescaled_ind$SR_unknown
shortfalls_rescaled$shortfalls_norm_index_sc = shortfalls_rescaled_ind$shortfalls_norm_index



tdwg3 =  tdwg3 %>%
  left_join(shortfalls_raw)
tdwg3 =  tdwg3 %>%
  left_join(shortfalls_rescaled)



# shortfalls_normalised <- read.csv("C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/shortfalls_normalised.csv")
#
# tdwg3@data =  tdwg3@data %>%
#   left_join(shortfalls_normalised)


# darkhot <- read.csv("C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/shortfalls_normalised_darkhot.csv")

# tdwg3@data =  tdwg3@data %>%
  # left_join(darkhot)




save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))


# save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData"))
# save(tdwg3, file = paste0(basepath, "app_data.RData"))
# write.csv(tdwg3@data, paste0(basepath, "variables_table.csv"))



