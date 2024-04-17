library("ggpubr")
library(ggbiplot)
library(ggfortify)
library(tidyverse)
library(tidymodels) # for the fit() function
library(plotly)

# basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"
basepath = "C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/"



# Add descriptions and discoveries from Daniele
discoveries = read.csv(paste0(basepath,"skyline_model/v4/Species_sampling_rates_all.csv"))#Discovery_rates.csv"))
descriptions = read.csv(paste0(basepath,"skyline_model/v4/Species_description_rates_all.csv"))#Description_rates.csv"))
left_to_sample = read.csv(paste0(basepath,"skyline_model/v4/Species_to_be_sampled_all.csv"))

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



# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
load(paste0(basepath, "app_data.RData"))

# save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData"))
# # write.csv(tdwg3@data, paste0(basepath, "variables_table.csv"))
#
tdwg3@data$spp_discovered =  (tdwg3@data$tot_exp_spp_wcvp-tdwg3@data$left_spp_wcvp)
tdwg3@data$prop_discovered =  tdwg3@data$spp_discovered/ tdwg3@data$tot_exp_spp_wcvp
tdwg3@data$prop_remaining =  tdwg3@data$left_spp_wcvp/ tdwg3@data$tot_exp_spp_wcvp
prop_spp = tdwg3@data$left_spp_wcvp / tdwg3@data$tot_exp_spp_wcvp





tdwg3@data =  tdwg3@data %>%
  left_join(data.frame(LEVEL3_COD = discoveries$X,
                       discoveries_time_diff = discoveries$time_diff,
                       discoveries_max_value = discoveries$max_value,
                       discoveries_max_year = discoveries$max_year,
                       discoveries_y30_diff = discoveries$y30_diff,
                       discoveries_y2010 = discoveries$y2010,
                       discoveries_y1980= discoveries$y1980,
                       discoveries_y1950 = discoveries$y1950))


tdwg3@data =  tdwg3@data %>%
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

rows= complete.cases(tdwg3@data[,goal_list])
sdg.pca <- prcomp(tdwg3@data[rows,goal_list], center = TRUE,scale. = TRUE)

tdwg3@data$PC1 = NA
tdwg3@data$PC1[rows] = sdg.pca$x[, 1]
tdwg3@data$PC2 = NA
tdwg3@data$PC2[rows] = sdg.pca$x[, 2]
tdwg3@data$PC3 = NA
tdwg3@data$PC3[rows] = sdg.pca$x[, 3]


subset =tdwg3@data[rows,]
rownames(subset) = tdwg3@data[rows,"LEVEL3_COD"]

autoplot(sdg.pca, data = subset,
         colour = 'Income.Group', #"discoveries_time_diff",   'discoveries_max_year', # # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.7,
         size=12,
         label.size = 4,
         lwd=4
)+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() #+
  # scale_colour_continuous()
  # scale_color_gradient2(midpoint=1930, low="yellow", mid="purple",
  #                       high="black", space ="Lab" )

autoplot(sdg.pca, data = subset,
         colour = "blue",#'discoveries_max_year', #  "discoveries_time_diff",  #'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.size =5,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour="black",
         label.alpha=0.3,
         label.position=position_jitter(width=0.012,height=0.012),
         # shape= FALSE,
         shape=16,
         alpha=0.7,
         size=2,
         label.size = 4,
         lwd=4
)+
  # position_jitter(width = NULL, height = NULL, seed = NA) +
  # geom_jitter()+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw()# +
  # scale_color_gradient2(midpoint=1930, low="yellow", mid="purple",
                        # high="black", space ="Lab" )


autoplot(sdg.pca, data = subset,
         colour = 'Income.Group',,#'discoveries_max_year', #  "discoveries_time_diff",  #'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         loadings.colour = "black",
         loadings.label.colour = "black",
         loadings.size =5,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour='Income.Group',#"black",
         label.alpha=0.3,
         label.position=position_jitter(width=0.012,height=0.012),
         # shape= FALSE,
         shape=16,
         alpha=0.7,
         size=2,
         label.size = 4,
         lwd=4
)+
  # position_jitter(width = NULL, height = NULL, seed = NA) +
  # geom_jitter()+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw()# +
# scale_color_gradient2(midpoint=1930, low="yellow", mid="purple",
# high="black", space ="Lab" )




autoplot(sdg.pca, data = subset,
         x = 1,    # PC2
         y = 3,
         colour = "discoveries_time_diff",  #'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.1,
         size=12,
         label.size = 2,
         lwd=4
)+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() +
  scale_color_gradient2(midpoint=100, low="black", mid="purple",
                        high="yellow", space ="Lab" )


autoplot(sdg.pca, data = subset,
         x = 2,    # PC2
         y = 3,
         colour = "discoveries_time_diff",  #'Income.Group', # num_instit, discoveries, descriptions, spp_left, tot_exp_spp_wcvp, left_spp_wcvp
         loadings = TRUE,
         loadings.label = TRUE,
         # loadings.colour = 'black',
         label = TRUE,#"Country",
         label.colour="black",
         # shape= FALSE,
         shape=16,
         alpha=0.1,
         size=12,
         label.size = 2,
         lwd=4
)+
  # geom_point(alpha=0.05, size=10,)+
  theme_bw() +
  scale_color_gradient2(midpoint=100, low="black", mid="purple",
                        high="yellow", space ="Lab" )

#
# plotly::plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
#         type="scatter",mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_time_diff,
#         marker = list(size=~discoveries_time_diff*0.2, sopacity = 0.5))#
#
#
# plotly::plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
#                 type="scatter",mode = "markers",
#                 text = ~paste("TDWG: ", LEVEL3_COD),
#                 color = ~descriptions_time_diff,
#                 marker = list(size=~descriptions_time_diff*0.1,
#                               opacity = 0.5))
#
#
# ##########################################################################################
#
# ### PCA again discovery/description
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC1 ~ discoveries_time_diff, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
#                max(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('discoveries_time_diff')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC1')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~discoveries_time_diff, y = ~PC1,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~discoveries_time_diff, y = ~PC1,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
#
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC1 ~ descriptions_time_diff, data = tdwg3@data[rows,])
#
#
# x_range <- seq(min(tdwg3@data[rows,]$descriptions_time_diff, na.rm=TRUE),
#                max(tdwg3@data[rows,]$descriptions_time_diff, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('descriptions_time_diff')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC1')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~descriptions_time_diff, y = ~PC1,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~descriptions_time_diff, y = ~PC1,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC2 ~ discoveries_time_diff, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
#                max(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('discoveries_time_diff')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC2')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~discoveries_time_diff, y = ~PC2,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~discoveries_time_diff, y = ~PC2,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
#
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC2 ~ descriptions_time_diff, data = tdwg3@data[rows,])
#
#
# x_range <- seq(min(tdwg3@data[rows,]$descriptions_time_diff, na.rm=TRUE),
#                max(tdwg3@data[rows,]$descriptions_time_diff, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('descriptions_time_diff')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC2')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~descriptions_time_diff, y = ~PC2,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~descriptions_time_diff, y = ~PC2,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC3 ~ discoveries_time_diff, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
#                max(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('discoveries_time_diff')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC3')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~discoveries_time_diff, y = ~PC3,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~discoveries_time_diff, y = ~PC3,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
#
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC3 ~ descriptions_time_diff, data = tdwg3@data[rows,])
#
#
# x_range <- seq(min(tdwg3@data[rows,]$descriptions_time_diff, na.rm=TRUE),
#                max(tdwg3@data[rows,]$descriptions_time_diff, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('descriptions_time_diff')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC3')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~descriptions_time_diff, y = ~PC3,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~descriptions_time_diff, y = ~PC3,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
# ##########################################################################################
#
# ### PCA against  time2event
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC1 ~ SR_shortfalls, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC1')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~PC1,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~PC1,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC2 ~ SR_shortfalls, data = tdwg3@data[rows,])
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC2')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~PC2,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~PC2,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(PC3 ~ SR_shortfalls, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('PC3')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~PC3,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~PC3,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#





#################################################################################
### skyline vs time2event
#################################################################################


#
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(descriptions_time_diff ~ SR_shortfalls, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('descriptions_time_diff')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~descriptions_time_diff,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~descriptions_time_diff,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(discoveries_time_diff ~ SR_shortfalls, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('discoveries_time_diff')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~discoveries_time_diff,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~discoveries_time_diff,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
#
# # descriptions_y2010
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(descriptions_y2010 ~ SR_shortfalls, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('descriptions_y2010')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~descriptions_y2010,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~descriptions_y2010,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#
#
# # discoveries_y2010
#
#
# lm_model <- linear_reg() %>%
#   set_engine('lm') %>%
#   set_mode('regression') %>%
#   fit(discoveries_y2010 ~ SR_shortfalls, data = tdwg3@data[rows,])
#
#
#
#
# x_range <- seq(min(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                max(tdwg3@data[rows,]$SR_shortfalls, na.rm=TRUE),
#                length.out = 100)
# x_range <- matrix(x_range, nrow=100, ncol=1)
# xdf <- data.frame(x_range)
# colnames(xdf) <- c('SR_shortfalls')
#
# ydf <- lm_model %>% predict(xdf)
#
# colnames(ydf) <- c('discoveries_y2010')
# xy <- data.frame(xdf, ydf)
#
# fig <- plot_ly(tdwg3@data[rows,], x = ~SR_shortfalls, y = ~discoveries_y2010,
#                type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
# fig <- fig %>% add_trace(data = xy, x = ~SR_shortfalls, y = ~discoveries_y2010,
#                          name = 'Regression Fit', mode = 'lines', alpha = 1)
# fig
#

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

ggscatter(tdwg3@data[rows,], x = "SR_shortfalls", y = "descriptions_y2010",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SR_shortfalls", ylab = "descriptions_y2010")
shapiro.test(tdwg3@data[rows,]$SR_shortfalls)
shapiro.test(tdwg3@data[rows,]$descriptions_y2010)
ggpubr::ggqqplot(tdwg3@data[rows,]$SR_shortfalls)
ggpubr::ggqqplot(tdwg3@data[rows,]$descriptions_y2010)

ggscatter(tdwg3@data[rows,], x = "SR_shortfalls", y = "descriptions_y30_diff",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SR_shortfalls", ylab = "descriptions_y30_diff")

#
# ggscatter(tdwg3@data[rows,], x = "SR_shortfalls", y = "descriptions_max_year",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "SR_shortfalls", ylab = "descriptions_max_year")
#
# ggscatter(tdwg3@data[rows,], x = "SR_shortfalls", y = "discoveries_y30_diff",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "SR_shortfalls", ylab = "discoveries_y30_diff")
#
# ggscatter(tdwg3@data[rows,], x = "SR_shortfalls", y = "discoveries_max_year",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "SR_shortfalls", ylab = "discoveries_max_year")



#----------------------------------------------------------------------------
#####Linnean "SR_unknown"
#----------------------------------------------------------------------------
tdwg3@data$SR_unknown_norm_yearly_log = log2(tdwg3@data$SR_unknown_norm/28)
tdwg3@data$discoveries_y2010_log = log2(tdwg3@data$discoveries_y2010)


ggscatter(tdwg3@data[rows,], x = "SR_unknown_norm_yearly_log", y = "discoveries_y2010_log",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", #cor.coef.name="tau",# rr.label="tau",
          xlab = "Species left to be described (log)",
          ylab = "Description rate across 2010s (log)", main="A.") +#(a) Linnean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )
ggsave(paste0(basepath, "linnean_model_comparison.pdf"), width = 10, height = 10, units = "cm")

# ggscatter(tdwg3@data[rows,], x = "SR_unknown_yearly", y = "discoveries_y1980",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "kendall",#cor.coef.name="tau",
#           xlab = "SR_unknown", ylab = "discoveries_y1980", main= "Linnean 1980s")
#
#
# ggscatter(tdwg3@data[rows,], x = "SR_unknown_yearly", y = "discoveries_y1950",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "kendall",#cor.coef.name="tau",
#           xlab = "SR_unknown", ylab = "discoveries_y1950", main= "Linnean 1950s")
#
#
# ggscatter(tdwg3@data[rows,], x = "SR_unknown_yearly", y = "discoveries_y30_diff",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "kendall",
#           xlab = "SR_unknown", ylab = "discoveries_y30_diff")

# ggscatter(tdwg3@data[rows,], x = "SR_unknown", y = "descriptions_y2010",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "SR_unknown", ylab = "descriptions_y2010")

#----------------------------------------------------------------------------
###### Wallacean "SR_nogeolocalisation"
#----------------------------------------------------------------------------
#
# ggscatter(tdwg3@data[rows,], x = "SR_nogeolocalisation", y = "discoveries_y2010",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "SR_nogeolocalisation", ylab = "discoveries_y2010")
tdwg3@data$SR_nogeoloc_norm_yearly_log =log2(tdwg3@data$SR_nogeoloc_norm/28)
tdwg3@data$descriptions_y2010_log = log2(tdwg3@data$descriptions_y2010)


ggscatter(tdwg3@data[rows,], x = "SR_nogeoloc_norm_yearly_log", y = "descriptions_y2010_log",
          add = "reg.line", conf.int = FALSE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = FALSE, cor.method = "kendall", cor.coef.name="tau",
          xlab = "Species left to be geolocated (log)",
          ylab = "Geolocation rate across 2010s (log)", main="B.") +  # Wallacean shortfall")
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),cor.coef.name = "tau",
    label.x = -16
  )
ggsave(paste0(basepath, "wallacean_model_comparison.pdf"), width = 10, height = 10, units = "cm")


# ggscatter(tdwg3@data[rows,], x = "SR_nogeolocalisation_yearly", y = "descriptions_y1980",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "kendall",
#           xlab = "SR_nogeolocalisation", ylab = "descriptions_y1980", main="Wallacean 1980s")
#
# ggscatter(tdwg3@data[rows,], x = "SR_nogeolocalisation_yearly", y = "descriptions_y1950",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "kendall",
#           xlab = "SR_nogeolocalisation", ylab = "descriptions_y1950", main="Wallacean 1950s")
#
#
# ggscatter(tdwg3@data[rows,], x = "SR_nogeolocalisation_yearly", y = "descriptions_y30_diff",
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "kendall",
#           xlab = "SR_nogeolocalisation", ylab = "descriptions_y30_diff")


#----------------------------------------------------------------------------
###### peak discovery/description year against PCs
#----------------------------------------------------------------------------

# tdwg3@data$SR_nogeolocalisation_yearly_log =log2(tdwg3@data$SR_nogeolocalisation/20)
# tdwg3@data$descriptions_y2010_log = log2(tdwg3@data$descriptions_y2010)


ggscatter(tdwg3@data[rows,], x = "PC1", y = "discoveries_max_year",
          add = "reg.line", conf.int = TRUE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1",
          ylab = "year of peak discovery", main="(a)")

ggscatter(tdwg3@data[rows,], x = "PC2", y = "discoveries_max_year",
          add = "reg.line", conf.int = TRUE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC2",
          ylab = "year of peak discovery", main="(b)")



ggscatter(tdwg3@data[rows,], x = "PC1", y = "descriptions_max_year",
          add = "reg.line", conf.int = TRUE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1",
          ylab = "year of peak description", main="(c)")

ggscatter(tdwg3@data[rows,], x = "PC2", y = "descriptions_max_year",
          add = "reg.line", conf.int = TRUE,
          # yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC2",
          ylab = "year of peak description", main="(d)")


#----------------------------------------------------------------------------
######  30 difference against PCs
#----------------------------------------------------------------------------



a <- ggscatter(tdwg3@data[rows,], x = "PC1", y = "descriptions_y30_diff",
          add = "reg.line", conf.int = TRUE,
          yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1",
          ylab = "descriptions_y30_diff", main="(a)")

b <- ggscatter(tdwg3@data[rows,], x = "PC2", y = "descriptions_y30_diff",
          add = "reg.line", conf.int = TRUE,
          yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC2",
          ylab = "descriptions_y30_diff", main="(b)")

c <- ggscatter(tdwg3@data[rows,], x = "PC1", y = "discoveries_y30_diff",
          add = "reg.line", conf.int = TRUE,
          yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1",
          ylab = "discoveries_y30_diff", main="(c)")

d <- ggscatter(tdwg3@data[rows,], x = "PC2", y = "discoveries_y30_diff",
          add = "reg.line", conf.int = TRUE,
          yscale = "log2",
          # xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC2",
          ylab = "discoveries_y30_diff", main="(d)")
ggarrange(a,b,c,d, ncol = 2, nrow = 2)

#################################################################

tdwg3@data$PC1_PC2 =tdwg3@data$PC1 * tdwg3@data$PC2


a <- ggscatter(tdwg3@data[rows,], x = "PC1_PC2", y = "discoveries_y30_diff",
          add = "reg.line", conf.int = TRUE,
          yscale = "log2",
          xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1*PC2",
          ylab = "discoveries_y30_diff", main="(a)")

b <- ggscatter(tdwg3@data[rows,], x = "PC1_PC2", y = "descriptions_y30_diff",
          add = "reg.line", conf.int = TRUE,
          yscale = "log2",
          xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1*PC2",
          ylab = "descriptions_y30_diff", main="(b)")

c <- ggscatter(tdwg3@data[rows,], x = "PC1_PC2", y = "discoveries_max_year",
          add = "reg.line", conf.int = TRUE,
          # yscale = "log2",
          xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1*PC2",
          ylab = "discoveries_max_year", main="(c)")

d <- ggscatter(tdwg3@data[rows,], x = "PC1_PC2", y = "descriptions_max_year",
          add = "reg.line", conf.int = TRUE,
          # yscale = "log2",
          xscale = "log2",
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "PC1*PC2",
          ylab = "descriptions_max_year", main="(d)")

# ggplot(iris, aes(x = Petal.Width, y = Sepal.Length)) +
#   geom_point() +
#   stat_smooth(method = "lm", col = "red")
ggarrange(a,b,c,d, ncol = 2, nrow = 2)



################################################################
###  USE a linear regression
df = tdwg3@data[rows,]

lm_fit <- lm(discoveries_max_year ~ PC1 + PC2, data=df)
summary(lm_fit)

lm_fit <- lm(descriptions_max_year ~ PC1 + PC2, data=df)
summary(lm_fit)


lm_fit <- lm(discoveries_y30_diff ~ PC1 + PC2, data=df)
summary(lm_fit)

lm_fit <- lm(descriptions_y30_diff ~ PC1 + PC2, data=df)
summary(lm_fit)




#
# predicted_df <- data.frame(df_pred = predict(lm_fit, df), PC2=df$PC2)
#
# # this is the predicted line of multiple linear regression
# ggplot(data = df, aes(x = descriptions_max_year, y = PC2)) +
#   geom_point(color='blue') +
#   geom_line(color='red',data = predicted_df, aes(x=df_pred, y=PC2))
# # this is the predicted line of multiple linear regression
# ggplot(data = df, aes(x = descriptions_max_year, y = PC2)) +
#   geom_point(color='blue') +
#   geom_line(color='red',data = predicted_df, aes(x=df_pred, y=PC2))
