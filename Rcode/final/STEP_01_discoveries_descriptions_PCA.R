library("ggpubr")
library(ggbiplot)
library(ggfortify)
library(tidyverse)
library(tidymodels) # for the fit() function
library(plotly)

# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"
load(paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))

# Add descriptions and discoveries from Daniele
discoveries = read.csv(paste0(basepath,"skyline_model/v5/Species_sampling_rates_all.csv"))#Discovery_rates.csv"))
descriptions = read.csv(paste0(basepath,"skyline_model/v5/Species_description_rates_all.csv"))#Description_rates.csv"))
left_to_sample = read.csv(paste0(basepath,"skyline_model/v5/Species_to_be_sampled_all.csv"))

#complement the new with the old
discoveries_old = read.csv(paste0(basepath,"skyline_model/v4/Species_sampling_rates_all.csv"))#Discovery_rates.csv"))
descriptions_old = read.csv(paste0(basepath,"skyline_model/v4/Species_description_rates_all.csv"))#Description_rates.csv"))
left_to_sample_old = read.csv(paste0(basepath,"skyline_model/v4/Species_to_be_sampled_all.csv"))

discoveries = rbind(discoveries, discoveries_old[!(discoveries_old$X %in% discoveries$X),])
descriptions = rbind(descriptions, descriptions_old[!(descriptions_old$X %in% descriptions$X),])
left_to_sample = rbind(left_to_sample, left_to_sample_old[!(left_to_sample_old$X %in% left_to_sample$X),])

window = 30

discoveries$max_value = 0
discoveries$max_year = 0
discoveries$time_diff = 0

z = as.numeric(discoveries[1,(2:(ncol(discoveries) - 3))])
start_year = 2022-(ncol(discoveries)-3-1-1)
years = start_year:2022
par(mfrow=c(5,1), mar=c(4,4,1,1))
for (roooow in 1:nrow(discoveries)){

  z= as.numeric(discoveries[roooow,(2:(ncol(discoveries) - 3))])
  # plot(years, z , type="l",  main=discoveries[roooow,1])

  x = zoo::rollapply(z, window,mean,   fill = NA, partial = TRUE)# fill="extend"
  discoveries$max_value[roooow] = max(x,na.rm=TRUE)
  discoveries$max_year[roooow] =  years[which(x == discoveries$max_value[roooow] )[1]]
  discoveries$time_diff[roooow] = 2022-discoveries$max_year[roooow]
  plot(years, x , type="l",xlab="year", ylab= "discovery rate", main=discoveries[roooow,1])
  abline(v=discoveries$max_year[roooow] , col="red")
}

descriptions$max_value = 0
descriptions$max_year = 0
descriptions$time_diff = 0
# par(mfrow=c(5,1))
for (roooow in 1:nrow(descriptions)){

  z= as.numeric(descriptions[roooow,(2:(ncol(descriptions) - 3))])
  # plot(years, z , type="l",  main=descriptions[roooow,1])

  x = zoo::rollapply(z, window,mean,   fill = NA, partial = TRUE)# fill="extend"
  descriptions$max_value[roooow] = max(x,na.rm=TRUE)
  descriptions$max_year[roooow] =  years[which(x == descriptions$max_value[roooow] )[1]]
  descriptions$time_diff[roooow] = 2022-descriptions$max_year[roooow]
  plot(years, x , type="l",xlab="year", ylab= "descriptioin rate",
       main=descriptions[roooow,1])
  abline(v=descriptions$max_year[roooow] , col="red")
}


######################################
### DO 20 year difference
######################################

for (roooow in 1:nrow(discoveries)){
  discoveries$y30_diff[roooow] =  ( mean(discoveries$X2010[roooow],
                                         discoveries$X2011[roooow],
                                         discoveries$X2012[roooow],
                                         discoveries$X2013[roooow],
                                         discoveries$X2014[roooow],
                                         discoveries$X2015[roooow],
                                         discoveries$X2016[roooow],
                                         discoveries$X2017[roooow],
                                         discoveries$X2018[roooow],
                                         discoveries$X2019[roooow])-
                                      mean(discoveries$X1970[roooow],
                                           discoveries$X1971[roooow],
                                           discoveries$X1972[roooow],
                                           discoveries$X1973[roooow],
                                           discoveries$X1974[roooow],
                                           discoveries$X1975[roooow],
                                           discoveries$X1976[roooow],
                                           discoveries$X1977[roooow],
                                           discoveries$X1978[roooow],
                                           discoveries$X1979[roooow]))
}



for (roooow in 1:nrow(descriptions)){
  descriptions$y30_diff[roooow] =   (mean(descriptions$X2010[roooow],
                                         descriptions$X2011[roooow],
                                         descriptions$X2012[roooow],
                                         descriptions$X2013[roooow],
                                         descriptions$X2014[roooow],
                                         descriptions$X2015[roooow],
                                         descriptions$X2016[roooow],
                                         descriptions$X2017[roooow],
                                         descriptions$X2018[roooow],
                                         descriptions$X2019[roooow])-
                                       mean(descriptions$X1970[roooow],
                                         descriptions$X1971[roooow],
                                         descriptions$X1972[roooow],
                                         descriptions$X1973[roooow],
                                         descriptions$X1974[roooow],
                                         descriptions$X1975[roooow],
                                         descriptions$X1976[roooow],
                                         descriptions$X1977[roooow],
                                         descriptions$X1978[roooow],
                                         descriptions$X1979[roooow])
                                     )
}

##########################################
## 2010s mean
##########################################

for (roooow in 1:nrow(discoveries)){
  discoveries$y2010[roooow] = mean(discoveries$X2010[roooow],
                                          discoveries$X2011[roooow],
                                          discoveries$X2012[roooow],
                                          discoveries$X2013[roooow],
                                          discoveries$X2014[roooow],
                                          discoveries$X2015[roooow],
                                          discoveries$X2016[roooow],
                                          discoveries$X2017[roooow],
                                          discoveries$X2018[roooow],
                                          discoveries$X2019[roooow])
}



for (roooow in 1:nrow(descriptions)){
  descriptions$y2010[roooow] = mean(descriptions$X2010[roooow],
                                           descriptions$X2011[roooow],
                                           descriptions$X2012[roooow],
                                           descriptions$X2013[roooow],
                                           descriptions$X2014[roooow],
                                           descriptions$X2015[roooow],
                                           descriptions$X2016[roooow],
                                           descriptions$X2017[roooow],
                                           descriptions$X2018[roooow],
                                           descriptions$X2019[roooow])
}

##########################################
## 1980 mean
##########################################

for (roooow in 1:nrow(discoveries)){
  discoveries$y1980[roooow] = mean(discoveries$X1980[roooow],
                                      discoveries$X1981[roooow],
                                      discoveries$X1982[roooow],
                                      discoveries$X1983[roooow],
                                      discoveries$X1984[roooow],
                                      discoveries$X1985[roooow],
                                      discoveries$X1986[roooow],
                                      discoveries$X1987[roooow],
                                      discoveries$X1988[roooow],
                                      discoveries$X1989[roooow])
}



for (roooow in 1:nrow(descriptions)){
  descriptions$y1980[roooow] = mean(descriptions$X1980[roooow],
                                       descriptions$X1981[roooow],
                                       descriptions$X1982[roooow],
                                       descriptions$X1983[roooow],
                                       descriptions$X1984[roooow],
                                       descriptions$X1985[roooow],
                                       descriptions$X1986[roooow],
                                       descriptions$X1987[roooow],
                                       descriptions$X1988[roooow],
                                       descriptions$X1989[roooow])
}


##########################################
## 1950 mean
##########################################

for (roooow in 1:nrow(discoveries)){
  discoveries$y1950[roooow] = mean(discoveries$X1950[roooow],
                                      discoveries$X1951[roooow],
                                      discoveries$X1952[roooow],
                                      discoveries$X1953[roooow],
                                      discoveries$X1954[roooow],
                                      discoveries$X1955[roooow],
                                      discoveries$X1956[roooow],
                                      discoveries$X1957[roooow],
                                      discoveries$X1958[roooow],
                                      discoveries$X1959[roooow])
}



for (roooow in 1:nrow(descriptions)){
  descriptions$y1950[roooow] = mean(descriptions$X1950[roooow],
                                       descriptions$X1951[roooow],
                                       descriptions$X1952[roooow],
                                       descriptions$X1953[roooow],
                                       descriptions$X1954[roooow],
                                       descriptions$X1955[roooow],
                                       descriptions$X1956[roooow],
                                       descriptions$X1957[roooow],
                                       descriptions$X1958[roooow],
                                       descriptions$X1959[roooow])
}




#####################################
#### DO PCA NOW
#####################################

tdwg3$spp_discovered =  (tdwg3$tot_exp_spp_wcvp-tdwg3$left_spp_wcvp)
tdwg3$prop_discovered =  tdwg3$spp_discovered/ tdwg3$tot_exp_spp_wcvp
tdwg3$prop_remaining =  tdwg3$left_spp_wcvp/ tdwg3$tot_exp_spp_wcvp
prop_spp = tdwg3$left_spp_wcvp / tdwg3$tot_exp_spp_wcvp


tdwg3 =  tdwg3 %>%
  left_join(data.frame(LEVEL3_COD = discoveries$X,
                       discoveries_time_diff = discoveries$time_diff,
                       discoveries_max_value = discoveries$max_value,
                       discoveries_max_year = discoveries$max_year,
                       discoveries_y30_diff = discoveries$y30_diff,
                       discoveries_y2010 = discoveries$y2010,
                       discoveries_y1980= discoveries$y1980,
                       discoveries_y1950 = discoveries$y1950))

tdwg3 =  tdwg3 %>%
  left_join(data.frame(LEVEL3_COD = descriptions$X,
                       descriptions_time_diff = descriptions$time_diff,
                       descriptions_max_value = descriptions$max_value,
                       descriptions_max_year = descriptions$max_year,
                       descriptions_y30_diff = descriptions$y30_diff,
                       descriptions_y2010 = descriptions$y2010,
                       descriptions_y1980 = descriptions$y1980,
                       descriptions_y1950 = descriptions$y1950))


#prop_spp,
variable_list = c( "discoveries", "descriptions")

goal_list = c(#"Prop_6hrs", "num_instit",
              "Goal.1.Score", "Goal.2.Score",
              "Goal.3.Score", "Goal.4.Score",
              "Goal.5.Score", "Goal.6.Score",
              "Goal.7.Score", "Goal.8.Score",
              "Goal.9.Score", "Goal.10.Score",
              "Goal.11.Score", "Goal.12.Score",
              "Goal.13.Score", "Goal.14.Score",
              "Goal.15.Score", "Goal.16.Score",
              "Goal.17.Score")

rows = complete.cases(as.data.frame( st_drop_geometry(tdwg3)[,goal_list]))
sdg.pca <- prcomp(st_drop_geometry(tdwg3)[rows,goal_list], center = TRUE,scale. = TRUE)

tdwg3$PC1 = NA
tdwg3$PC1[rows] = sdg.pca$x[, 1]
tdwg3$PC2 = NA
tdwg3$PC2[rows] = sdg.pca$x[, 2]
tdwg3$PC3 = NA
tdwg3$PC3[rows] = sdg.pca$x[, 3]


subset = st_drop_geometry(tdwg3)[rows,]
rownames(subset) = st_drop_geometry(tdwg3)[rows,"LEVEL3_COD"]

autoplot(sdg.pca, data = subset,
         colour = 'Income.Group',#'discoveries_max_year', #  "discoveries_time_diff",  #'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.size =5,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour='Income.Group',#"black",
         label.alpha=0.5,
         label.position=position_jitter(width=0.012,height=0.012),
         # shape= FALSE,
         shape=16,
         alpha=0.7,
         size=2,
         label.size = 4,
         lwd=4) +
  theme_bw()

ggsave(paste0(basepath, "REVISION_1/FigS1_PCA_income.pdf"),
       width = 18, height = 15, units = "cm")
ggsave(paste0(basepath, "REVISION_1/FigS1_PCA_income.png"),
       width = 18, height = 15, units = "cm", bg="white")


#####################################################################################
#####################################################################################
####              UNSCALED
#####################################################################################
#####################################################################################


################################################################
### NORMALISED


#----------------------------------------------------------------------------
#   Linnean "SR_unknown"
#----------------------------------------------------------------------------
tdwg3$SR_unknown_norm_yearly_log = log2(normalise(tdwg3$SR_unknown)/28)
tdwg3$discoveries_y2010_log = log2(tdwg3$discoveries_y2010)


a <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log", y = "discoveries_y2010_log",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
          xlab = "Species left to be described (log)",
          ylab = "Description rate across 2010s (log)", main="a.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -20
  )
a
ggsave(paste0(basepath, "REVISION_1/linnean_model_comparison.pdf"), width = 10, height = 10, units = "cm")
ggsave(paste0(basepath, "REVISION_1/linnean_model_comparison.png"), width = 10, height = 10, units = "cm", bg="white")



#----------------------------------------------------------------------------
###### Wallacean "SR_nogeolocalisation"
#----------------------------------------------------------------------------

tdwg3$SR_nogeoloc_norm_yearly_log =log2(normalise(tdwg3$SR_nogeoloc)/28)
tdwg3$descriptions_y2010_log = log2(tdwg3$descriptions_y2010)


b<-ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log", y = "descriptions_y2010_log",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
          xlab = "Species left to be geolocated (log)",
          ylab = "Geolocation rate across 2010s (log)", main="b.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )
b
ggsave(paste0(basepath, "REVISION_1/wallacean_model_comparison.pdf"), width = 10, height = 10, units = "cm")
ggsave(paste0(basepath, "REVISION_1/wallacean_model_comparison.png"), width = 10, height = 10, units = "cm", bg="white")

ggarrange(a,b, ncol = 2, nrow = 1)
ggsave(paste0(basepath, "REVISION_1/linnean_wallacean_model_comparison.pdf"), width = 20, height = 10, units = "cm")
ggsave(paste0(basepath, "REVISION_1/linnean_wallacean_model_comparison.png"), width = 20, height = 10, units = "cm", bg="white")


################################################################
### comparing over years
################################################################

#
#----------------------------------------------------------------------------
#      Linnean "SR_unknown"
#----------------------------------------------------------------------------
# normalise(tdwg3$SR_unknown) = scale(tdwg3$SR_unknown, center=F)
tdwg3$SR_unknown_norm_yearly_log = log2(normalise(tdwg3$SR_unknown)/28)

#2010

tdwg3$discoveries_y2010_log = log2(tdwg3$discoveries_y2010)


c <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log", y = "discoveries_y2010_log",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
          xlab = "Species left to be described (log)",
          ylab = "Description rate across 2010s (log)", main="c.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -20
  )


#1980

tdwg3$discoveries_y1980_log = log2(tdwg3$discoveries_y1980)


b <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log", y = "discoveries_y1980_log",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
               xlab = "Species left to be described (log)",
               ylab = "Description rate across 1980s (log)", main="b.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -20
  )


#1950

tdwg3$discoveries_y1950_log = log2(tdwg3$discoveries_y1950)


a <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log", y = "discoveries_y1950_log",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
               xlab = "Species left to be described (log)",
               ylab = "Description rate across 1950s (log)", main="a.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -20
  )


#----------------------------------------------------------------------------
###### Wallacean "SR_nogeolocalisation"
#----------------------------------------------------------------------------

# normalise(tdwg3$SR_nogeoloc) = scale(tdwg3$SR_nogeoloc, center=F)
tdwg3$SR_nogeoloc_norm_yearly_log =log2(normalise(tdwg3$SR_nogeoloc)/28)

#2010

tdwg3$descriptions_y2010_log = log2(tdwg3$descriptions_y2010)


f <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log", y = "descriptions_y2010_log",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
          xlab = "Species left to be geolocated (log)",
          ylab = "Geolocation rate across 2010s (log)", main="f.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )


#1980

tdwg3$descriptions_y1980_log = log2(tdwg3$descriptions_y1980)


e <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log", y = "descriptions_y1980_log",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
               xlab = "Species left to be geolocated (log)",
               ylab = "Geolocation rate across 1980s (log)", main="e.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )

#1980

tdwg3$descriptions_y1950_log = log2(tdwg3$descriptions_y1950)


d <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log", y = "descriptions_y1950_log",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
               xlab = "Species left to be geolocated (log)",
               ylab = "Geolocation rate across 1950s (log)", main="d.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )


ggarrange(a,b,c,
          d,e,f, ncol = 3, nrow = 2)

ggsave(paste0(basepath, "REVISION_1/sky_tine2event_comparison_50_80_10.pdf"),
       width = 30, height = 20, units = "cm")
ggsave(paste0(basepath, "REVISION_1/sky_tine2event_comparison_50_80_10.png"),
       width = 30, height = 20, units = "cm", bg="white")

#####################################################################################
#####################################################################################
#### RESCALED
#####################################################################################
#####################################################################################
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

################################################################
### NORMALISED

#----------------------------------------------------------------------------
#       Linnean "SR_unknown"
#----------------------------------------------------------------------------

tdwg3$SR_unknown_norm_yearly_log_sc = log2(normalise(tdwg3$SR_unknown_sc/28))
complete_rows = !is.na(tdwg3$discoveries_y2010)
tdwg3$discoveries_y2010_log_sc = tdwg3$discoveries_y2010
tdwg3$discoveries_y2010_log_sc[complete_rows] = log2(SAR(tdwg3$discoveries_y2010[complete_rows],
                                       areas$land_area[complete_rows]))



a <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log_sc", y = "discoveries_y2010_log_sc",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
          xlab = "Species left to be described (log)",
          ylab = "Description rate across 2010s (log)", main="a.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )
a

ggsave(paste0(basepath, "REVISION_1/linnean_model_comparison_sc.pdf"), width = 10, height = 10, units = "cm")
ggsave(paste0(basepath, "REVISION_1/linnean_model_comparison_sc.png"), width = 10, height = 10, units = "cm", bg="white")



#----------------------------------------------------------------------------
#     Wallacean "SR_nogeolocalisation"
#----------------------------------------------------------------------------
#

tdwg3$SR_nogeoloc_norm_yearly_log_sc =log2(normalise(tdwg3$SR_nogeoloc_sc/28))
# tdwg3$descriptions_y2010_log = log2(tdwg3$descriptions_y2010)
complete_rows = !is.na(tdwg3$descriptions_y2010)
tdwg3$descriptions_y2010_log_sc = tdwg3$descriptions_y2010
tdwg3$descriptions_y2010_log_sc[complete_rows] = log2(SAR(tdwg3$descriptions_y2010[complete_rows],
                                                         areas$land_area[complete_rows]))

b <-ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log_sc", y = "descriptions_y2010_log_sc",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
          xlab = "Species left to be geolocated (log)",
          ylab = "Geolocation rate across 2010s (log)", main="b.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )
b
ggsave(paste0(basepath, "REVISION_1/wallacean_model_comparison_sc.pdf"), width = 10, height = 10, units = "cm")
ggsave(paste0(basepath, "REVISION_1/wallacean_model_comparison_sc.png"), width = 10, height = 10, units = "cm",bg="white")

ggarrange(a,b, ncol = 2, nrow = 1)
ggsave(paste0(basepath, "REVISION_1/linnean_wallacean_model_comparison_sc.pdf"), width = 20, height = 10, units = "cm")
ggsave(paste0(basepath, "REVISION_1/linnean_wallacean_model_comparison_sc.png"), width = 20, height = 10, units = "cm", bg="white")


################################################################
### comparing over years
################################################################

#
#----------------------------------------------------------------------------
#       Linnean "SR_unknown"
#----------------------------------------------------------------------------
# normalise(tdwg3$SR_unknown) = scale(tdwg3$SR_unknown, center=F)
tdwg3$SR_unknown_norm_yearly_log_sc = log2(normalise(tdwg3$SR_unknown_sc/28))

#2010

# tdwg3$discoveries_y2010_log = log2(tdwg3$discoveries_y2010)
complete_rows = !is.na(tdwg3$discoveries_y2010)
tdwg3$discoveries_y2010_log_sc = tdwg3$discoveries_y2010
tdwg3$discoveries_y2010_log_sc[complete_rows] = log2(SAR(tdwg3$discoveries_y2010[complete_rows],
                                                         areas$land_area[complete_rows]))


c <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log_sc", y = "discoveries_y2010_log_sc",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
               xlab = "Species left to be described (log)",
               ylab = "Description rate across 2010s (log)", main="c.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -25
  )


#1980

# tdwg3$discoveries_y1980_log = log2(tdwg3$discoveries_y1980)
complete_rows = !is.na(tdwg3$discoveries_y1980)
tdwg3$discoveries_y1980_log_sc = tdwg3$discoveries_y1980
tdwg3$discoveries_y1980_log_sc[complete_rows] = log2(SAR(tdwg3$discoveries_y1980[complete_rows],
                                                         areas$land_area[complete_rows]))


b <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log_sc", y = "discoveries_y1980_log_sc",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
               xlab = "Species left to be described (log)",
               ylab = "Description rate across 1980s (log)", main="b.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -25
  )


#1950

# tdwg3$discoveries_y1950_log = log2(tdwg3$discoveries_y1950)
complete_rows = !is.na(tdwg3$discoveries_y1950)
tdwg3$discoveries_y1950_log_sc = tdwg3$discoveries_y1950
tdwg3$discoveries_y1950_log_sc[complete_rows] = log2(SAR(tdwg3$discoveries_y1950[complete_rows],
                                                         areas$land_area[complete_rows]))

a <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_unknown_norm_yearly_log_sc", y = "discoveries_y1950_log_sc",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
               xlab = "Species left to be described (log)",
               ylab = "Description rate across 1950s (log)", main="a.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -25
  )


#----------------------------------------------------------------------------
#        Wallacean "SR_nogeolocalisation"
#----------------------------------------------------------------------------

tdwg3$SR_nogeoloc_norm_yearly_log_sc =log2(normalise(tdwg3$SR_nogeoloc_sc/28))

#2010

complete_rows = !is.na(tdwg3$descriptions_y2010)
tdwg3$descriptions_y2010_log_sc = tdwg3$descriptions_y2010
tdwg3$descriptions_y2010_log_sc[complete_rows] = log2(SAR(tdwg3$descriptions_y2010[complete_rows],
                                                       areas$land_area[complete_rows]))

f <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log_sc", y = "descriptions_y2010_log_sc",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
               xlab = "Species left to be geolocated (log)",
               ylab = "Geolocation rate across 2010s (log)", main="f.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -25
  )


#1980

complete_rows = !is.na(tdwg3$descriptions_y1980)
tdwg3$descriptions_y1980_log_sc = tdwg3$descriptions_y1980
tdwg3$descriptions_y1980_log_sc[complete_rows] = log2(SAR(tdwg3$descriptions_y1980[complete_rows],
                                                       areas$land_area[complete_rows]))


e <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log_sc", y = "descriptions_y1980_log_sc",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
               xlab = "Species left to be geolocated (log)",
               ylab = "Geolocation rate across 1980s (log)", main="e.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -25
  )

#1950

complete_rows = !is.na(tdwg3$descriptions_y1950)
tdwg3$descriptions_y1950_log_sc = tdwg3$descriptions_y1950
tdwg3$descriptions_y1950_log_sc[complete_rows] = log2(SAR(tdwg3$descriptions_y1950[complete_rows],
                                                       areas$land_area[complete_rows]))

d <- ggscatter(st_drop_geometry(tdwg3)[rows,], x = "SR_nogeoloc_norm_yearly_log_sc", y = "descriptions_y1950_log_sc",
               add = "reg.line", conf.int = FALSE,
               # yscale = "log2",
               # xscale = "log2",
               cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
               xlab = "Species left to be geolocated (log)",
               ylab = "Geolocation rate across 1950s (log)", main="d.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -25
  )


ggarrange(a,b,c,
          d,e,f, ncol = 3, nrow = 2)

ggsave(paste0(basepath, "REVISION_1/sky_tine2event_comparison_50_80_10_sc.pdf"),
       width = 30, height = 20, units = "cm")
ggsave(paste0(basepath, "REVISION_1/sky_tine2event_comparison_50_80_10_sc.png"),
       width = 30, height = 20, units = "cm", bg="white")


save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))

