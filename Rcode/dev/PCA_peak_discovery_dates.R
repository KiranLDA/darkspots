
library(ggbiplot)
library(ggfortify)
library(tidyverse)


basepath = "C:/Users/kdh10kg/Documents/github/darkspots_shiny/prep/"


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
par(mfrow=c(5,1))
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
  discoveries$y30_diff[roooow] =  (mean(discoveries$X1970[roooow],
                                        discoveries$X1971[roooow],
                                        discoveries$X1972[roooow],
                                        discoveries$X1973[roooow],
                                        discoveries$X1974[roooow],
                                        discoveries$X1975[roooow],
                                        discoveries$X1976[roooow],
                                        discoveries$X1977[roooow],
                                        discoveries$X1978[roooow],
                                        discoveries$X1979[roooow]) -
                                      mean(discoveries$X2010[roooow],
                                           discoveries$X2011[roooow],
                                           discoveries$X2012[roooow],
                                           discoveries$X2013[roooow],
                                           discoveries$X2014[roooow],
                                           discoveries$X2015[roooow],
                                           discoveries$X2016[roooow],
                                           discoveries$X2017[roooow],
                                           discoveries$X2018[roooow],
                                           discoveries$X2019[roooow]))
}



for (roooow in 1:nrow(descriptions)){
  descriptions$y30_diff[roooow] =  (mean(descriptions$X1970[roooow],
                                         descriptions$X1971[roooow],
                                         descriptions$X1972[roooow],
                                         descriptions$X1973[roooow],
                                         descriptions$X1974[roooow],
                                         descriptions$X1975[roooow],
                                         descriptions$X1976[roooow],
                                         descriptions$X1977[roooow],
                                         descriptions$X1978[roooow],
                                         descriptions$X1979[roooow]) -
                                      mean(descriptions$X2010[roooow],
                                           descriptions$X2011[roooow],
                                           descriptions$X2012[roooow],
                                           descriptions$X2013[roooow],
                                           descriptions$X2014[roooow],
                                           descriptions$X2015[roooow],
                                           descriptions$X2016[roooow],
                                           descriptions$X2017[roooow],
                                           descriptions$X2018[roooow],
                                           descriptions$X2019[roooow]))
}


#####################################
#### DO PCA NOW
#####################################



load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")

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
                       discoveries_y30_diff = discoveries$y30_diff))


tdwg3@data =  tdwg3@data %>%
  left_join(data.frame(LEVEL3_COD = descriptions$X,
                       descriptions_time_diff = descriptions$time_diff,
                       descriptions_max_value = descriptions$max_value,
                       descriptions_max_year = descriptions$max_year,
                       descriptions_y30_diff = descriptions$y30_diff))





#prop_spp,
variable_list = c( "discoveries", "descriptions")

goal_list = c("Prop_6hrs", "num_instit",
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



subset =tdwg3@data[rows,]
rownames(subset) = tdwg3@data[rows,"LEVEL3_COD"]

autoplot(sdg.pca, data = subset,
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
  scale_color_gradient2(midpoint=30, low="black", mid="purple",
                        high="yellow", space ="Lab" )





#
# plot_ly(data = tdwg3@data[rows,],
#         x = ~PC1,#~discoveries_time_diff,
#         y = ~PC2, type="scatter", mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_time_diff,
#         marker = list(size = ~discoveries_max_value*1000, opacity = 0.5))
#

















# plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
#         type="scatter", mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~descriptions_max_year,# size=10,
#         marker = list(size = ~descriptions_max_year/100, opacity = 0.5))

# plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
#         type="scatter", mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~descriptions_time_diff,
#         marker = list(size=~descriptions_time_diff*0.1, opacity = 0.5))

plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
        type="scatter", mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~descriptions_max_value,
        marker = list(size=~descriptions_max_value*0.2, opacity = 0.5))


plot_ly(data = tdwg3@data[rows,], x = ~log(descriptions_max_value), y = ~PC2,
        type="scatter", mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~descriptions_max_value,
        marker = list(size=~descriptions_max_value*0.2, opacity = 0.5))


plot_ly(data = tdwg3@data[rows,], x = ~log(descriptions_max_value), y = ~PC1,
        type="scatter", mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~descriptions_max_value,
        marker = list(size=~descriptions_max_value*0.2, opacity = 0.5))




plot_ly(data = tdwg3@data[rows,], x = ~(descriptions_max_value), y = ~PC1,
        type="scatter", mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~descriptions_max_value,
        marker = list(size=~descriptions_max_value*0.2, opacity = 0.5))


# library(tidyverse)
library(tidymodels) # for the fit() function
library(plotly)
# data(tips)

# y <- tips$tip
# X <- tips$total_bill

lm_model <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression') %>%
  fit(PC1 ~ descriptions_max_value, data = tdwg3@data[rows,])




x_range <- seq(min(tdwg3@data[rows,]$descriptions_max_value, na.rm=TRUE),
               max(tdwg3@data[rows,]$descriptions_max_value, na.rm=TRUE),
               length.out = 100)
x_range <- matrix(x_range, nrow=100, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('descriptions_max_value')

ydf <- lm_model %>% predict(xdf)

colnames(ydf) <- c('PC1')
xy <- data.frame(xdf, ydf)

fig <- plot_ly(tdwg3@data[rows,], x = ~descriptions_max_value, y = ~PC1,
               type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
fig <- fig %>% add_trace(data = xy, x = ~descriptions_max_value, y = ~PC1,
                         name = 'Regression Fit', mode = 'lines', alpha = 1)
fig







lm_model <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression') %>%
  fit(PC2 ~ descriptions_max_value, data = tdwg3@data[rows,])

x_range <- seq(min(tdwg3@data[rows,]$descriptions_max_value, na.rm=TRUE),
               max(tdwg3@data[rows,]$descriptions_max_value, na.rm=TRUE),
               length.out = 100)
x_range <- matrix(x_range, nrow=100, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('descriptions_max_value')

ydf <- lm_model %>% predict(xdf)

colnames(ydf) <- c('PC2')
xy <- data.frame(xdf, ydf)

fig <- plot_ly(tdwg3@data[rows,], x = ~descriptions_max_value, y = ~PC2,
               type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
fig <- fig %>% add_trace(data = xy, x = ~descriptions_max_value, y = ~PC2,
                         name = 'Regression Fit', mode = 'lines', alpha = 1)
fig





#
# plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2, type="scatter",
#         mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_max_year,
#         marker = list(size=~discoveries_max_year*0.01, opacity = 0.5))

plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2,
        type="scatter",mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~discoveries_time_diff,
        marker = list(size=~discoveries_time_diff*0.15, opacity = 0.5))


plot_ly(data = tdwg3@data[rows,], x =~log(discoveries_time_diff), y =  ~PC2, type="scatter",
        mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~discoveries_time_diff,
        marker = list(size=10,#~discoveries_time_diff*PC1,
                      opacity = 0.5))


plot_ly(data = tdwg3@data[rows,], x =~log(discoveries_time_diff), y =  ~PC1, type="scatter",
        mode = "markers",
        text = ~paste("TDWG: ", LEVEL3_COD),
        color = ~discoveries_time_diff,
        marker = list(size=10,#~discoveries_time_diff*PC1,
                      opacity = 0.5))






#
# library(reshape2) # to load tips data
# library(tidyverse)
library(tidymodels) # for the fit() function
# library(plotly)
# data(tips)

# y <- tips$tip
# X <- tips$total_bill

lm_model <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression') %>%
  fit(PC1 ~ discoveries_time_diff, data = tdwg3@data[rows,])




x_range <- seq(min(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
               max(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
               length.out = 100)
x_range <- matrix(x_range, nrow=100, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('discoveries_time_diff')

ydf <- lm_model %>% predict(xdf)

colnames(ydf) <- c('PC1')
xy <- data.frame(xdf, ydf)

fig <- plot_ly(tdwg3@data[rows,], x = ~discoveries_time_diff, y = ~PC1,
               type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
fig <- fig %>% add_trace(data = xy, x = ~discoveries_time_diff, y = ~PC1,
                         name = 'Regression Fit', mode = 'lines', alpha = 1)
fig



lm_model <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression') %>%
  fit(PC2 ~ discoveries_time_diff, data = tdwg3@data[rows,])

x_range <- seq(min(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
               max(tdwg3@data[rows,]$discoveries_time_diff, na.rm=TRUE),
               length.out = 100)
x_range <- matrix(x_range, nrow=100, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('discoveries_time_diff')

ydf <- lm_model %>% predict(xdf)

colnames(ydf) <- c('PC2')
xy <- data.frame(xdf, ydf)

fig <- plot_ly(tdwg3@data[rows,], x = ~discoveries_time_diff, y = ~PC2,
               type = 'scatter', alpha = 0.65, mode = 'markers', name = 'Tips')
fig <- fig %>% add_trace(data = xy, x = ~discoveries_time_diff, y = ~PC2,
                         name = 'Regression Fit', mode = 'lines', alpha = 1)
fig




# plot_ly(data = tdwg3@data[rows,], x = ~PC1, y = ~PC2, type="scatter",
#         mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_max_value,
#         marker = list(size=~discoveries_max_value*1000, opacity = 0.5))
#
#


#
#
#
# plot_ly(data = tdwg3@data[rows,],
#         x = ~discoveries_max_value,
#         y = ~Goal.9.Score, type="scatter",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_max_value, size=20)
#
#
# plot_ly(data = tdwg3@data[rows,],
#         x = ~discoveries_max_value,
#         y = ~Goal.15.Score, type="scatter",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_time_diff, size=20)
#
#
# plot_ly(data = tdwg3@data[rows,],
#         x = ~PC2,#~discoveries_time_diff,
#         y = ~PC1, type="scatter",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~descriptions_time_diff, size=~descriptions_time_diff)
#
#
#
# plot_ly(data = tdwg3@data[rows,],
#         x = ~PC2,#~discoveries_time_diff,
#         y = ~PC1, type="scatter",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~descriptions_y30_diff,
#         marker = list(size = ~descriptions_y30_diff, opacity = 0.8))
#
#
#
# plot_ly(data = tdwg3@data[rows,],
#         x = ~PC2,#~discoveries_time_diff,
#         y = ~PC1, type="scatter", mode = "markers",
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~discoveries_y30_diff, size=~discoveries_y30_diff)
#
#

