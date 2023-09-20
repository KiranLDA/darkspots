#PC1 Lower is richer
#PC2 lower has more protection
# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
library(plotly)
library(tidyverse)
library(truncnorm)

normalise <- function(x){(x - min(x,na.rm=T))/ ((max(x,na.rm=T) - min(x,na.rm=T)) + 0.01)}
load("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData")




# data =  tdwg3_PCA@data
data =  st_drop_geometry(tdwg3)

# data$SR_shortfalls = data$SR_nogeoloc + data$SR_unknown
# data$shortfalls_norm_index = scale(data$SR_shortfalls, center=F)
#
# data$SR_shortfalls_UL = data$SR_nogeoloc_UL + data$SR_unknown_UL
# data$shortfalls_norm_index_UL = scale(data$SR_shortfalls_UL, center=F)
#
# data$SR_shortfalls_LL = data$SR_nogeoloc_LL + data$SR_unknown_LL
# data$shortfalls_norm_index_LL = scale(data$SR_shortfalls_LL, center=F)
#
#
# data$collections_y2010 = data$descriptions_y2010 + data$discoveries_y2010
# data$collections = data$descriptions# * data$discoveries

###########################
# calculate Upper and Lower limits and scale
# moved this to 1st page
# df1 = normalise(log(st_drop_geometry(data)[c("SR_unknown_LL","SR_unknown_sd", "SR_unknown","SR_unknown_UL")]+1))
# df2 = normalise(log(st_drop_geometry(data)[c("SR_nogeoloc_LL", "SR_nogeoloc_sd","SR_nogeoloc","SR_nogeoloc_UL")]+1))
# df = df1 + df2
# colnames(df) = c("shortfalls_norm_index_LL", "shortfalls_norm_index_SD","shortfalls_norm_index_Med","shortfalls_norm_index_UL")
# df = cbind(st_drop_geometry(data)["LEVEL3_COD"],df)
#
# data = data %>%
#   left_join(df)
#
#
# df1 = normalise(log(st_drop_geometry(data)[c("SR_unknown_LL_sc", "SR_unknown_sd_sc","SR_unknown_sc","SR_unknown_UL_sc")]+1))
# df2 = normalise(log(st_drop_geometry(data)[c("SR_nogeoloc_LL_sc","SR_nogeoloc_sd_sc", "SR_nogeoloc_sc","SR_nogeoloc_UL_sc")]+1))
# df = df1 + df2
# colnames(df) = c("shortfalls_norm_index_LL_sc","shortfalls_norm_index_SD_sc", "shortfalls_norm_index_M_sc","shortfalls_norm_index_UL_sc")
# df = cbind(st_drop_geometry(data)["LEVEL3_COD"],df)
#
# data = data %>%
#   left_join(df)
# #





#######################################################################################################
#
#                        FUNCTIONS
#
#######################################################################################################

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


plot_CE_sum <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  plot(-1,-1,lwd=2, type="l",pch=19, xlim=c(0,1.01),ylim=c(0,1.01),
       xlab = "Cumulative Cost",
       ylab = "Cumulative Expected Benefit")

  polygon(c(c(0,normalise(1:length(to_keep))),
            c(rev(normalise(1:length(to_keep))),0)),
          c(c(0,normalise(cumsum(sort(normalise(normalise(UL[to_keep]) + weight[to_keep]),
                                      decreasing=TRUE, na.last = T)))),
            c(rev(normalise(cumsum(sort(normalise(normalise(LL[to_keep]) + weight[to_keep]),
                                        decreasing=TRUE, na.last = T)))), 0)),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 100),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 100))

  lines(c(0,normalise(1:length(to_keep))),
        c(0,normalise(cumsum(sort(normalise(normalise(mean[to_keep]) + weight[to_keep]),
                                  decreasing=TRUE, na.last = T)))),
        lwd=1, lty=2, col="chocolate3")

  random_benefits_table <- cbind(create_table(darkspots=mean),
                                 create_table(darkspots=LL),
                                 create_table(darkspots=UL))

  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] =  summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }

  polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]))),
            c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
          c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])])),
            c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(c(0,normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
        c(0,normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
        lwd=1, lty=3, col= "grey")

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  polygon(c(c(0, normalise(1:length(which(!is.na(UL_old))))),
            c(rev(normalise(1:length(which(!is.na(LL_old))))), 0)),
          c(c(0,normalise(cumsum(UL_old[which(!is.na(UL_old))]))),
            c(rev(normalise(cumsum(LL_old[which(!is.na(LL_old))]))),0)),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 100),
          border=NA)

  lines(c(0,normalise(1:length(mean_old[which(!is.na(mean_old))]))),
        c(0,normalise(cumsum(mean_old[which(!is.na(mean_old))]))),
        lwd=1, lty=1, col= "darkblue")
  if(add_legend == T){
    legend("bottomright",
           legend=c("Random",
                    "Past collections",
                    "Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2)
    )

  }
}




plot_CE_hist_sum <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  plot(-1,-1,lwd=2, type="l",pch=19, xlim=c(0,1.01),ylim=c(0,1.01),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=F),
                                 create_table(darkspots=LL,cumul=F),
                                 create_table(darkspots=UL,cumul=F))

  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  polygon(c(c(0,normalise(1:length(which(!is.na(UL_old))))),
            c(rev(normalise(1:length(which(!is.na(LL_old))))),0)),
          c(c(0,normalise((UL_old[which(!is.na(UL_old))]))),
            c(rev(normalise((LL_old[which(!is.na(LL_old))]))),0)),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(c(0,normalise(1:length(mean_old[which(!is.na(mean_old))]))),
        c(0,normalise((mean_old[which(!is.na(mean_old))]))),
        lwd=1, lty=1, col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                               max = 255, alpha = 100))

  polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]))),
            c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
          c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])])),
            c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(c(normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
        c(normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(c(normalise(1:length(to_keep))),
            c(rev(normalise(1:length(to_keep))))),
          c(c(normalise((sort(normalise(normalise(UL[to_keep]) + weight[to_keep]),
                              decreasing=TRUE, na.last = T)))),
            c(rev(normalise((sort(normalise(normalise(LL[to_keep]) + weight[to_keep]),
                                  decreasing=TRUE, na.last = T)))))),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(c(normalise(1:length(to_keep))),
        c(normalise((sort(normalise(normalise(mean[to_keep]) + weight[to_keep]), decreasing=TRUE, na.last = T)))),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("topright",
           legend=c("Random",
                    "Past collections",
                    "Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2)
    )

  }
}



plot_CE_multiply <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  plot(-1,-1,lwd=2, type="l",pch=19, xlim=c(0,1.01),ylim=c(0,1.01),
       xlab = "Cumulative Cost",
       ylab = "Cumulative Expected Benefit")

  polygon(c(c(0,normalise(1:length(to_keep))),
            c(rev(normalise(1:length(to_keep))),0)),
          c(c(0,normalise(cumsum(sort(UL[to_keep] * weight[to_keep],
                                      decreasing=TRUE, na.last = T)))),
            c(rev(normalise(cumsum(sort(LL[to_keep] * weight[to_keep],
                                        decreasing=TRUE, na.last = T)))), 0)),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 100),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 100))
  random_benefits_table <- cbind(create_table(darkspots=mean),
                                 create_table(darkspots=LL),
                                 create_table(darkspots=UL))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] =  summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }

  polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]))),
            c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
          c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])])),
            c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(c(0,normalise(1:length(to_keep))),
        c(0,normalise(cumsum(sort(mean[to_keep] * weight[to_keep],
                                  decreasing=TRUE, na.last = T)))),
        lwd=1, lty=2, col="chocolate3")

  lines(c(0,normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
        c(0,normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
        lwd=1, lty=3, col= "grey")

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  polygon(c(c(0,normalise(1:length(which(!is.na(UL_old))))),
            c(rev(normalise(1:length(which(!is.na(LL_old))))),0)),
          c(c(0,normalise(cumsum(UL_old[which(!is.na(UL_old))]))),
            c(rev(normalise(cumsum(LL_old[which(!is.na(LL_old))]))),0)),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 100),
          border=NA)

  lines(c(0,normalise(1:length(mean_old[which(!is.na(mean_old))]))),
        c(0,normalise(cumsum(mean_old[which(!is.na(mean_old))]))),
        lwd=1, lty=1, col= "darkblue")
  if(add_legend == T){
    legend("bottomright",
           legend=c("Random",
                    "Past collections",
                    "Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2)
    )

  }
}




plot_CE_hist_multiply <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  plot(-1,-1,lwd=2, type="l",pch=19, xlim=c(0,1.01),ylim=c(0,1.01),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=F),
                                 create_table(darkspots=LL,cumul=F),
                                 create_table(darkspots=UL,cumul=F))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  polygon(c(c(0,normalise(1:length(which(!is.na(UL_old))))),
            c(rev(normalise(1:length(which(!is.na(LL_old))))),0)),
          c(c(0,normalise((UL_old[which(!is.na(UL_old))]))),
            c(rev(normalise((LL_old[which(!is.na(LL_old))]))),0)),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(c(0,normalise(1:length(mean_old[which(!is.na(mean_old))]))),
        c(0,normalise((mean_old[which(!is.na(mean_old))]))),
        lwd=1, lty=1, col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                               max = 255, alpha = 100))

  polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]))),
            c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
          c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])])),
            c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(c(normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
        c(normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(c(normalise(1:length(to_keep))),
            c(rev(normalise(1:length(to_keep))))),
          c(c(normalise((sort(UL[to_keep] * weight[to_keep],
                              decreasing=TRUE, na.last = T)))),
            c(rev(normalise((sort(LL[to_keep] *  weight[to_keep],
                                  decreasing=TRUE, na.last = T)))))),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(c(normalise(1:length(to_keep))),
        c(normalise((sort(mean[to_keep] * weight[to_keep], decreasing=TRUE, na.last = T)))),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("topright",
           legend=c("Random",
                    "Past collections",
                    "Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2)
    )

  }
}

plot_raw_CE_hist_multiply <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=F),
                                 create_table(darkspots=LL,cumul=F),
                                 create_table(darkspots=UL,cumul=F))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  summary_random_benefits_table = ifelse(summary_random_benefits_table<0,0,summary_random_benefits_table)

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  plot(-1,-1,lwd=2, type="l",pch=19,
       xlim=c(0,length(which(!is.na(UL_old)))),
       ylim=c(0,max(UL* weight, UL_old* weight, LL* weight, LL_old* weight, mean* weight, mean_old* weight,na.rm=T )),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  polygon(c(c(0,1:length(which(!is.na(UL_old)))),
            c(rev(1:length(which(!is.na(LL_old)))),0)),
          c(c(0,UL_old[which(!is.na(UL_old))]),
            c(rev(LL_old[which(!is.na(LL_old))]),0)),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(1:length(mean_old[which(!is.na(mean_old))]),
        mean_old[which(!is.na(mean_old))],
        lwd=1, lty=1,
        col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                 max = 255, alpha = 100))

  polygon(c(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          c(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])],
            rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])],
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(1:length(to_keep),
            rev(1:length(to_keep))),
          c(sort(UL[to_keep] * weight[to_keep],decreasing=TRUE, na.last = T),
            rev(sort(LL[to_keep] *  weight[to_keep], decreasing=TRUE, na.last = T))),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(1:length(to_keep),
        sort(mean[to_keep] * weight[to_keep], decreasing=TRUE, na.last = T),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("topright",legend=c("Random","Past collections","Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2))
  }
}


plot_raw_CE_multiply <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=T),
                                 create_table(darkspots=LL,cumul=T),
                                 create_table(darkspots=UL,cumul=T))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  summary_random_benefits_table = ifelse(summary_random_benefits_table<0,0,summary_random_benefits_table)

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  plot(-1,-1,lwd=2, type="l",pch=19,
       xlim=c(0,length(which(!is.na(UL_old)))),
       ylim=c(0,1.01),
       # ylim=c(0,max(UL* weight, UL_old* weight, LL* weight, LL_old* weight, mean* weight, mean_old* weight,na.rm=T )),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  polygon(c(0,1:length(which(!is.na(UL_old))),
            rev(1:length(which(!is.na(LL_old)))),0),
          c(0,
            normalise(cumsum(UL_old[which(!is.na(UL_old))])),
            normalise(rev(cumsum(LL_old[which(!is.na(LL_old))]))),
            0),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(c(0,1:length(mean_old[which(!is.na(mean_old))])),
        normalise(cumsum(c(0,mean_old[which(!is.na(mean_old))]))),
        lwd=1, lty=1,
        col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                 max = 255, alpha = 100))

  polygon(c(0,1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])),0),
          c(0,
            normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])),
            0),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(0,1:length(to_keep),rev(1:length(to_keep)),0),
          c(0,
            normalise(cumsum(sort(normalise(UL[to_keep]) * weight[to_keep],decreasing=TRUE, na.last = T))),
            normalise(rev(cumsum(sort(normalise(LL[to_keep]) * weight[to_keep], decreasing=TRUE, na.last = T)))),
            0
          ),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(c(0,1:length(to_keep)),
        c(0,normalise(cumsum(sort(normalise(mean[to_keep]) * weight[to_keep], decreasing=TRUE, na.last = T)))),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("bottomright",legend=c("Random","Past collections","Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2))
  }
}


plot_raw_CE_hist_multiply <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=F),
                                 create_table(darkspots=LL,cumul=F),
                                 create_table(darkspots=UL,cumul=F))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  summary_random_benefits_table = ifelse(summary_random_benefits_table<0,0,summary_random_benefits_table)

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  plot(-1,-1,lwd=2, type="l",pch=19,
       xlim=c(0,length(which(!is.na(UL_old)))),
       ylim=c(0,max(UL* weight, UL_old* weight, LL* weight, LL_old* weight, mean* weight, mean_old* weight,na.rm=T )),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  polygon(c(c(0,1:length(which(!is.na(UL_old)))),
            c(rev(1:length(which(!is.na(LL_old)))),0)),
          c(c(0,UL_old[which(!is.na(UL_old))]),
            c(rev(LL_old[which(!is.na(LL_old))]),0)),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(1:length(mean_old[which(!is.na(mean_old))]),
        mean_old[which(!is.na(mean_old))],
        lwd=1, lty=1,
        col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                 max = 255, alpha = 100))

  polygon(c(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          c(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])],
            rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])],
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(1:length(to_keep),
            rev(1:length(to_keep))),
          c(sort(UL[to_keep] * weight[to_keep],decreasing=TRUE, na.last = T),
            rev(sort(LL[to_keep] *  weight[to_keep], decreasing=TRUE, na.last = T))),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(1:length(to_keep),
        sort(mean[to_keep] * weight[to_keep], decreasing=TRUE, na.last = T),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("topright",legend=c("Random","Past collections","Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2))
  }
}

plot_raw_CE_sum <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=T),
                                 create_table(darkspots=LL,cumul=T),
                                 create_table(darkspots=UL,cumul=T))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  summary_random_benefits_table = ifelse(summary_random_benefits_table<0,0,summary_random_benefits_table)

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  plot(-1,-1,lwd=2, type="l",pch=19,
       xlim=c(0,length(which(!is.na(UL_old)))),
       ylim=c(0,1.01),
       # ylim=c(0,max(UL* weight, UL_old* weight, LL* weight, LL_old* weight, mean* weight, mean_old* weight,na.rm=T )),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  polygon(c(0,1:length(which(!is.na(UL_old))),
            rev(1:length(which(!is.na(LL_old)))),0),
          c(0,
            normalise(cumsum(UL_old[which(!is.na(UL_old))])),
            normalise(rev(cumsum(LL_old[which(!is.na(LL_old))]))),
            0),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(c(0,1:length(mean_old[which(!is.na(mean_old))])),
        normalise(cumsum(c(0,mean_old[which(!is.na(mean_old))]))),
        lwd=1, lty=1,
        col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                 max = 255, alpha = 100))

  polygon(c(0,1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])),0),
          c(0,
            normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])),
            0),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(0,1:length(to_keep),rev(1:length(to_keep)),0),
          c(0,
            normalise(cumsum(sort(normalise(UL[to_keep] + weight[to_keep]),decreasing=TRUE, na.last = T))),
            normalise(rev(cumsum(sort(normalise(LL[to_keep] + weight[to_keep]), decreasing=TRUE, na.last = T)))),
            0
          ),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(c(0,1:length(to_keep)),
        c(0,normalise(cumsum(sort(normalise(mean[to_keep] + weight[to_keep]), decreasing=TRUE, na.last = T)))),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("bottomright",legend=c("Random","Past collections","Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2))
  }
}


plot_raw_CE_hist_sum <- function(mean, UL, LL, weight, collections, add_legend=F){

  to_keep = which(!is.na(weight) & !is.na(UL) & !is.na(LL) & !is.na(mean))

  random_benefits_table <- cbind(create_table(darkspots=mean,cumul=F),
                                 create_table(darkspots=LL,cumul=F),
                                 create_table(darkspots=UL,cumul=F))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(mean), ncol=3 )
  for (rowi in 1:length(mean)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] = summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  summary_random_benefits_table = ifelse(summary_random_benefits_table<0,0,summary_random_benefits_table)

  ranked_collections = rank(-collections, na.last = "keep", ties.method = "first")
  mean_old = mean[ranked_collections]
  UL_old = UL[ranked_collections]
  LL_old = LL[ranked_collections]

  plot(-1,-1,lwd=2, type="l",pch=19,
       xlim=c(0,length(which(!is.na(UL_old)))),
       ylim=c(0,1.01),
       # ylim=c(0,max(UL* weight, UL_old* weight, LL* weight, LL_old* weight, mean* weight, mean_old* weight,na.rm=T )),
       xlab = "Normalised Rank",
       ylab = "Expected Benefit")

  polygon(c(1:length(which(!is.na(UL_old))),
            rev(1:length(which(!is.na(LL_old))))),
          normalise(c(UL_old[which(!is.na(UL_old))],
                      rev(LL_old[which(!is.na(LL_old))]))),
          col=rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                  max = 255, alpha = 50), border=NA)

  lines(1:length(mean_old[which(!is.na(mean_old))]),
        normalise(mean_old[which(!is.na(mean_old))]),
        lwd=1, lty=1,
        col= rgb(col2rgb("darkblue")[1,1], col2rgb("darkblue")[2,1],col2rgb("darkblue")[3,1],
                 max = 255, alpha = 100))

  polygon(c(1:length(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])]),
            rev(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          c(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,3])],
            rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          col=rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100),
          border=NA)

  lines(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]),
        lwd=1, lty=3, col= rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 100))

  polygon(c(1:length(to_keep),
            rev(1:length(to_keep))),
          c(sort(normalise(UL[to_keep] + weight[to_keep]),decreasing=TRUE, na.last = T),
            rev(sort(normalise(LL[to_keep] + weight[to_keep]), decreasing=TRUE, na.last = T))),
          col=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                  alpha = 170),
          border=rgb(col2rgb("chocolate3")[1,1], col2rgb("chocolate3")[2,1],col2rgb("chocolate3")[3,1], max = 255,
                     alpha = 170))

  lines(1:length(to_keep),
        sort(normalise(mean[to_keep] + weight[to_keep]), decreasing=TRUE, na.last = T),
        lwd=2, lty=1, col="chocolate3")

  if(add_legend == T){
    legend("topright",legend=c("Random","Past collections","Optimised"),
           col= c("grey","darkblue","chocolate3"),
           lty=c(3,1,2))
  }
}

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



############################################################################################
#  SCENARIOS with upper and lower limits with Weights
############################################################################################
#
# # Prioritise areas with the most species left to discover
# weight_1_M = rep(1, length(data$shortfalls_norm_index))
#
# # prioritise areas with the most species in poor places to discover
# weight_2_M = normalise(data$PC1) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
#
# # prioritise areas with the most species in rich places to discover
# weight_3_M = (1-(normalise(data$PC1))) #(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
#
# #Prioritise areas with most species and biggest potential for biodiversity loss PC2
# weight_4_M = normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
#
# #Prioritise areas with most species and smallest potential for biodiversity loss PC2
# weight_5_M =  (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
#
# # prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
# weight_6_M = normalise(normalise(data$PC1) * (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
#
# # prioritise areas with the most species in poor places to discover with protection
# weight_7_M = normalise(normalise(data$PC1) * (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
#
# # prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
# weight_8_M = normalise((1-normalise(data$PC1)) * (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
#
# # prioritise areas with the most species in rich places to discover with protection
# weight_9_M = normalise((1-normalise(data$PC1)) * (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
#


########################################################################
########################################################################
###   Estimate benefits and Ranks for median upper and lower
########################################################################
########################################################################



# am not normalising because UL and LL themselves are normalise relative to each other, and don't want to loose that

# for (scenario in 1:9){

  #----------------------------------------------------------------
  #    Median
  #----------------------------------------------------------------

  # #--------------------------------
  # # UNSCALED / SUM
  # data[,paste0("benefit_",scenario,"_S")] = normalise(normalise(data$SR_nogeoloc) + normalise(data$SR_unknown)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  # data[,paste0("Rank_scenario_",scenario,"_S")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S")]),
  #                                                       na.last = "keep", ties.method = "first")
  #
  # # #--------------------------------
  # # # UNSCALED / MULTIPLY
  # # data[,paste0("benefit_",scenario,"_M")] = data$shortfalls_norm_index * get(paste0("weight_",scenario,"_M"))
  # # data[,paste0("Rank_scenario_",scenario,"_M")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_M")]),
  # #                                                  na.last = "keep", ties.method = "first")
  # #
  # # #--------------------------------
  # # # SCALED / SUM
  # # data[,paste0("benefit_",scenario,"_S_sc")] = data$shortfalls_norm_index_sc + get(paste0("weight_",scenario,"_S"))
  # # data[,paste0("Rank_scenario_",scenario,"_S_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S_sc")]),
  # #                                                  na.last = "keep", ties.method = "first")
  # #
  # # #--------------------------------
  # # # SCALED / MULTIPLY
  # # data[,paste0("benefit_",scenario,"_M_sc")] = data$shortfalls_norm_index_sc * get(paste0("weight_",scenario,"_M"))
  # # data[,paste0("Rank_scenario_",scenario,"_M_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_M_sc")]),
  # #                                                       na.last = "keep", ties.method = "first")
  #
  # #----------------------------------------------------------------
  # #    Upper Limit
  # #----------------------------------------------------------------
  #
  # #--------------------------------
  # # UNSCALED / SUM
  # data[,paste0("benefit_",scenario,"_UL_S")] = normalise(normalise(data$SR_nogeoloc_UL) + normalise(data$SR_unknown_UL)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index_UL + get(paste0("weight_",scenario,"_S"))
  # data[,paste0("Rank_scenario_",scenario,"_UL_S")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_UL_S")]),
  #                                                          na.last = "keep", ties.method = "first")
  #
  # # #--------------------------------
  # # # UNSCALED / MULTIPLY
  # # data[,paste0("benefit_",scenario,"_UL_M")] = data$shortfalls_norm_index_UL * get(paste0("weight_",scenario,"_M"))
  # # data[,paste0("Rank_scenario_",scenario,"_UL_M")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_UL_M")]),
  # #                                                       na.last = "keep", ties.method = "first")
  # #
  # # #--------------------------------
  # # # SCALED / SUM
  # # data[,paste0("benefit_",scenario,"_UL_S_sc")] = data$shortfalls_norm_index_UL_sc + get(paste0("weight_",scenario,"_S"))
  # # data[,paste0("Rank_scenario_",scenario,"_UL_S_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_UL_S_sc")]),
  # #                                                        na.last = "keep", ties.method = "first")
  # #
  # # #--------------------------------
  # # # SCALED / MULTIPLY
  # # data[,paste0("benefit_",scenario,"_UL_M_sc")] = data$shortfalls_norm_index_UL_sc * get(paste0("weight_",scenario,"_M"))
  # # data[,paste0("Rank_scenario_",scenario,"_UL_M_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_UL_M_sc")]),
  # #                                                          na.last = "keep", ties.method = "first")
  # #
  #
  # #----------------------------------------------------------------
  # #    Lower Limit
  # #----------------------------------------------------------------
  #
  # #--------------------------------
  # # UNSCALED / SUM
  # data[,paste0("benefit_",scenario,"_LL_S")] = normalise(normalise(data$SR_nogeoloc_LL) + normalise(data$SR_unknown_LL)) + get(paste0("weight_",scenario,"_S"))# + get(paste0("weight_",scenario,"_S"))
  # data[,paste0("Rank_scenario_",scenario,"_LL_S")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_LL_S")]),
  #                                                          na.last = "keep", ties.method = "first")

  # #--------------------------------
  # # UNSCALED / MULTIPLY
  # data[,paste0("benefit_",scenario,"_LL_M")] = data$shortfalls_norm_index_LL * get(paste0("weight_",scenario,"_M"))
  # data[,paste0("Rank_scenario_",scenario,"_LL_M")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_LL_M")]),
  #                                                          na.last = "keep", ties.method = "first")
  #
  # #--------------------------------
  # # SCALED / SUM
  # data[,paste0("benefit_",scenario,"_LL_S_sc")] = data$shortfalls_norm_index_LL_sc + get(paste0("weight_",scenario,"_S"))
  # data[,paste0("Rank_scenario_",scenario,"_LL_S_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_LL_S_sc")]),
  #                                                             na.last = "keep", ties.method = "first")
  #
  # #--------------------------------
  # # SCALED / MULTIPLY
  # data[,paste0("benefit_",scenario,"_LL_M_sc")] = data$shortfalls_norm_index_LL_sc * get(paste0("weight_",scenario,"_M"))
  # data[,paste0("Rank_scenario_",scenario,"_LL_M_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_LL_M_sc")]),
  #                                                             na.last = "keep", ties.method = "first")

# }



old = read.csv("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/old_REV_shortfalls_not_rescaled_predictions_with_uncertainty_bounds.csv")


# cbind(old$SR_unknown_LL,
#       st_drop_geometry(data$SR_unknown_LL))

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
# randomised_benefits_table = matrix(NA, nrow = length(MEAN), ncol= samples)
# randomised_benefits_table_sc = matrix(NA, nrow = length(MEAN), ncol= samples)

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
  # UL_replace = randomised_unknown_table[rowi,] > st_drop_geometry(data$SR_unknown_UL)[rowi]
  # randomised_unknown_table[rowi, UL_replace] = st_drop_geometry(data$SR_unknown_UL)[rowi]
  #
  # LL_replace = randomised_unknown_table[rowi,] < st_drop_geometry(data$SR_unknown_LL)[rowi]
  # randomised_unknown_table[rowi, LL_replace] = st_drop_geometry(data$SR_unknown_LL)[rowi]
complete = which(!is.na(old$SR_nogeoloc))
for (rowi in complete){

  randomised_nogeoloc_table[rowi,] = rnorm(n = samples,
                                           mean = data$SR_nogeoloc[rowi],
                                           sd = data$SR_nogeoloc_sd[rowi])

  success = FALSE
  while (!success) {
    UL_replace = randomised_nogeoloc_table[rowi,] > old$SR_nogeoloc_UL[rowi]
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

  # UL_replace = randomised_nogeoloc_table[rowi,] > st_drop_geometry(data$SR_nogeoloc_UL)[rowi]
  # randomised_nogeoloc_table[rowi, UL_replace] = st_drop_geometry(data$SR_nogeoloc_UL)[rowi]
  #
  # LL_replace = randomised_nogeoloc_table[rowi,] < st_drop_geometry(data$SR_nogeoloc_LL)[rowi]
  # randomised_nogeoloc_table[rowi, LL_replace] = st_drop_geometry(data$SR_nogeoloc_LL)[rowi]

  # randomised_unknown_table[rowi,] = rtruncnorm(n = samples,
  #                                              a = st_drop_geometry(data$SR_unknown_LL)[rowi],
  #                                              b = st_drop_geometry(data$SR_unknown_UL)[rowi],
  #                                              mean = st_drop_geometry(old$SR_unknown)[rowi],
  #                                              sd = st_drop_geometry(data$SR_unknown_sd)[rowi])
  # randomised_nogeoloc_table[rowi,] = rtruncnorm(n = samples,
  #                                               a = st_drop_geometry(data$SR_nogeoloc_LL)[rowi],
  #                                               b = st_drop_geometry(data$SR_nogeoloc_UL)[rowi],
  #                                               mean = st_drop_geometry(old$SR_nogeoloc)[rowi],
  #                                               sd = st_drop_geometry(data$SR_nogeoloc_sd)[rowi])
  # randomised_benefits_table[rowi,] = rtruncnorm(n = samples,
  #                                               a = st_drop_geometry(data$shortfalls_norm_index_LL)[rowi],
  #                                               b = st_drop_geometry(data$shortfalls_norm_index_UL)[rowi],
  #                                               mean = st_drop_geometry(data$shortfalls_norm_index)[rowi],
  #                                               sd = st_drop_geometry(data$shortfalls_norm_index_SD)[rowi])
  # randomised_benefits_table_sc[rowi,] = rtruncnorm(n = samples,
  #                                               a = st_drop_geometry(data$shortfalls_norm_index_LL_sc)[rowi],
  #                                               b = st_drop_geometry(data$shortfalls_norm_index_UL_sc)[rowi],
  #                                               mean = st_drop_geometry(data$shortfalls_norm_index_sc)[rowi],
  #                                               sd = st_drop_geometry(data$shortfalls_norm_index_SD_sc)[rowi])
}

for (coli in 1:samples){
  randomised_shortfalls_table[,coli] = normalise(normalise(randomised_unknown_table[,coli]) +
    normalise(randomised_nogeoloc_table[,coli]))
  # randomised_shortfalls_table[,coli] = normalise(randomised_unknown_table[,coli]) +
  #                                                  normalise(randomised_nogeoloc_table[,coli])

}

# randomised_shortfalls_table = normalise(randomised_shortfalls_table)

# randomised_shortfalls_table = normalise(normalise(randomised_unknown_table) + normalise(randomised_nogeoloc_table))

# for (coli in 1:samples){
#   randomised_benefit_table[,coli] = normalise(normalise(randomised_unknown_table[,coli]) +
#                                                    normalise(randomised_nogeoloc_table[,coli]))
#
# }



for (scenario in 1:9){

  # CALCULATE BENEFIT FOR ALL these randomly generated benefits

  #--------------------------------
  # UNSCALED / SUM
  # assign(paste0("randomised_benefit_table_",scenario,"_S"), sweep(randomised_shortfalls_table, 1, get(paste0("weight_",scenario,"_S")), FUN = "+" ))
  # assign(paste0("randomised_benefit_table_",scenario,"_S"), normalise(normalise(randomised_nogeoloc_table) + normalise(randomised_unknown_table)) + get(paste0("weight_",scenario,"_S")))
  assign(paste0("randomised_benefit_table_",scenario,"_S"), (randomised_shortfalls_table + get(paste0("weight_",scenario,"_S"))))

  # #--------------------------------
  # # UNSCALED / MULTIPLY
  # assign(paste0("benefit_table_",scenario,"_M"), sweep(randomised_benefits_table, 1, get(paste0("weight_",scenario,"_M")), FUN = "*" ))
  #
  # #--------------------------------
  # # UNSCALED / SUM
  # assign(paste0("benefit_table_",scenario,"_S_sc"), sweep(randomised_benefits_table_sc, 1, get(paste0("weight_",scenario,"_S")), FUN = "+" ))
  #
  # #--------------------------------
  # # UNSCALED / MULTIPLY
  # assign(paste0("benefit_table_",scenario,"_M_sc"), sweep(randomised_benefits_table_sc, 1, get(paste0("weight_",scenario,"_M")), FUN = "*" ))

  # Rank these benefits and get random rank for global table

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


  # #--------------------------------
  # # UNSCALED / MULTIPLY
  # temp_out = matrix(NA, nrow = length(MEAN), ncol= samples)
  # temp_in = get(paste0("benefit_table_",scenario,"_M"))
  #
  # # rank each column of randomised benefits in table
  # for (coli in 1:samples){
  #   temp_out[,coli] = rank( - temp_in[,coli], na.last = "keep", ties.method = "first")
  # }
  # assign(paste0("rank_table_",scenario,"_M"), temp_out)
  #
  # # calculate the mean rank per country
  # mean_rank = NA
  # for (rowi in 1:length(MEAN)){
  #   mean_rank = c(mean_rank, mean(get(paste0("rank_table_",scenario,"_M"))[rowi,],na.rm=T))
  # }
  # data[,paste0("Rank_rand_scenario_",scenario,"_M")] = mean_rank[-1]
  #
  # #--------------------------------
  # # UNSCALED / SUM
  # temp_out = matrix(NA, nrow = length(MEAN), ncol= samples)
  # temp_in = get(paste0("benefit_table_",scenario,"_S_sc"))
  #
  # # rank each column of randomised benefits in table
  # for (coli in 1:samples){
  #   temp_out[,coli] = rank( - temp_in[,coli], na.last = "keep", ties.method = "first")
  # }
  # assign(paste0("rank_table_",scenario,"_S_sc"), temp_out)
  #
  # # calculate the mean rank per country
  # mean_rank = NA
  # for (rowi in 1:length(MEAN)){
  #   mean_rank = c(mean_rank, mean(get(paste0("rank_table_",scenario,"_S_sc"))[rowi,],na.rm=T))
  # }
  # data[,paste0("Rank_rand_scenario_",scenario,"_S_sc")] = mean_rank[-1]
  #
  # #--------------------------------
  # # UNSCALED / MULTIPLY
  # temp_out = matrix(NA, nrow = length(MEAN), ncol= samples)
  # temp_in = get(paste0("benefit_table_",scenario,"_M_sc"))
  #
  # # rank each column of randomised benefits in table
  # for (coli in 1:samples){
  #   temp_out[,coli] = rank( - temp_in[,coli], na.last = "keep", ties.method = "first")
  # }
  # assign(paste0("rank_table_",scenario,"_M_sc"), temp_out)
  #
  # # calculate the mean rank per country
  # mean_rank = NA
  # for (rowi in 1:length(MEAN)){
  #   mean_rank = c(mean_rank, mean(get(paste0("rank_table_",scenario,"_M_sc"))[rowi,],na.rm=T))
  # }
  # data[,paste0("Rank_rand_scenario_",scenario,"_M_sc")] = mean_rank[-1]

}


# tdwg3 = tdwg3 %>% left_join(data)



#
# ord <- order(data$Rank_scenario_2_UL_M)
# plot(data$Rank_scenario_2_UL_M[ord], data$benefit_2_UL_M[ord], type= "l", lty=2)
# ord <- order(data$Rank_scenario_2_M)
# lines(data$Rank_scenario_2_M[ord], data$benefit_2_M[ord], type= "l")
# ord <- order(data$Rank_scenario_2_LL_M)
# lines(data$Rank_scenario_2_LL_M[ord], data$benefit_2_LL_M[ord], type= "l", lty=2)
#
#
# ord <- order(data$Rank_scenario_2_UL_S)
# plot(data$Rank_scenario_2_UL_S[ord], data$benefit_2_UL_S[ord], type= "l", lty=2)
# ord <- order(data$Rank_scenario_2_S)
# lines(data$Rank_scenario_2_S[ord], data$benefit_2_S[ord], type= "l")
# ord <- order(data$Rank_scenario_2_LL_S)
# lines(data$Rank_scenario_2_LL_S[ord], data$benefit_2_LL_S[ord], type= "l", lty=2)
#



#########################################################################################################
# Use mann-whitney U test to compare scenarios
#########################################################################################################
# par(mfrow=c(2,1))
# to_keep = which( !is.na(data$SR_shortfalls) & !is.na(data$PC1) &!is.na(data$PC2))
# coul <- RColorBrewer::brewer.pal(10, "RdYlGn")
# coul <- rev(colorRampPalette(coul)(length(to_keep)+1))
# rank = rank(-data$SR_shortfalls[to_keep], na.last = "keep", ties.method = "first")
#
# plot(data$PC1[to_keep], data$PC2[to_keep],
#      pch=19,cex=2, xlab="PC1", ylab="PC2",
#      col = coul[rank])
#
# rank2= rank(cumsum(sort(data$SR_shortfalls[to_keep],decreasing=TRUE, na.last = T)),
#             na.last = "keep", ties.method = "first")
#
# plot(1:length(to_keep),
#      c(cumsum(sort(data$SR_shortfalls[to_keep],decreasing=TRUE, na.last = T))),
#      pch=19,cex=2, type="o", xlab="Rank", ylab="Cumulative benefit",
#      col = coul[rank2])
# par(mfrow=c(1,1))

# to_keep = which(!is.na(data$Rank_scenario_1_S))
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_2_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_3_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_4_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_5_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_6_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_7_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_8_S[to_keep])
# wilcox.test(data$Rank_scenario_1_S[to_keep], data$Rank_scenario_9_S[to_keep])

# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_2[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_3[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_4[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_5[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_6[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_7[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_8[to_keep])
# wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_9[to_keep])
#
#
# wilcox.test(data$Rank_CE_9[to_keep],data$Rank_scenatio_9[to_keep])
#
# par(mfrow=c(1,1))
# plot(data$Rank_scenario_9_S[to_keep],
#      data$benefit_9_S[to_keep], pch=19)
# points(data$Rank_CE_9_S[to_keep],
#      data$CE_9_S[to_keep], col="brown", pch=19)
#
# text(data$Rank_scenario_9_S[to_keep]+5,
#      data$benefit_9_S[to_keep]+0.01,
#      data$LEVEL3_COD[to_keep], cex=0.3, adj = 0,  srt=20)
#
# text(data$Rank_CE_9_S[to_keep]+5,
#      data$CE_9_S[to_keep]+0.01,
#      data$LEVEL3_COD[to_keep], cex=0.3, adj = 0, col="brown", srt=20)
#




#####################################################
# Save


# save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/REV_app_data.RData"))
# save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))
# write.csv(st_drop_geometry(tdwg3), paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/variables_table.csv"))
#
# # rgdal::writeOGR(obj=tdwg3,
# #                 dsn="C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp",
# #                 layer="darkspots",
# #                 driver="ESRI Shapefile")
#
#
# # library(raster)
# # save to shp but note that long data file names don't save properly so best to load app_data.R
# st_write(tdwg3, 'C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/model_outputs.shp',layer = NULL, delete_layer = TRUE)
# # st_write(tdwg3, filename=paste0(basepath, "/REVISION_1/model_outputs.shp"), delete_layer = TRUE)
#

















library(zoo)

scenario = 1

#get the order
sort_col = order(-st_drop_geometry(tdwg3)[,paste0("benefit_",scenario,"_S")])
to_plot = get(paste0("randomised_benefit_table_",scenario,"_S"))
# rownames(to_plot) = st_drop_geometry(tdwg3$LEVEL3_NAM)
to_plot = to_plot[sort_col,1:samples]
# keep only the top 100


data3= fortify.zoo(zoo(to_plot), melt = TRUE)
data3$Label = st_drop_geometry(tdwg3$LEVEL3_NAM)[data3$Index][sort_col]
# data3$Mean = st_drop_geometry(tdwg3[paste0("benefit_",scenario,"_S")])[paste0("benefit_",scenario,"_S")][data3$Index][sort_col]
data3$Mean = st_drop_geometry(tdwg3$benefit_1_S)[data3$Index][sort_col]


# data3 = data3[data3$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
                       fill=Index, color = Index),
            show.legend = FALSE) +
  # geom_tile(color="white", size=0.01) +
  geom_boxplot(aes(middle = median(Value)), outlier.shape=NA)+#, coef = 100) +
  geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  # geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_x_discrete(labels= unique(data3$Label)) +
  xlab("Ranked botanical country") +
  ylab("Expected benefit given model uncertainty") +
  # coord_flip() +
  # ylim(0,2.5) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")


gg #+ geom_point(aes(x=as.factor(Index), y=Mean, color= "red"))

ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_benefit_plot.pdf",
       width = 30, height = 12, units = "cm")

# top 100
data3 = data3[data3$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
                       fill=Index),
            show.legend = FALSE) +
  # geom_tile(color="white", size=0.01) +
  geom_boxplot(aes(middle = median(Value)), outlier.shape=NA, coef = 1000) +
  geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  # geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_x_discrete(labels= unique(data3$Label)) +
  xlab("Ranked botanical country") +
  ylab("Expected benefit given model uncertainty") +
  # coord_flip() +
  # ylim(0,2.5) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")


gg #+ geom_point(aes(x=as.factor(Index), y=Mean, color= "red"))

ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_benefit_plot_top100.pdf",
       width = 25, height = 15, units = "cm")



## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data3, aes(x=as.factor(Index), y=Value,
                       fill=Index, color=Index),
            show.legend = FALSE) +
  # geom_tile(color="white", size=0.01) +
  # geom_boxplot(aes(middle = median(Value)), outlier.shape=NA, coef = 1000) +
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  geom_jitter(shape=19, position=position_jitter(0.2),size=0.005) +
  geom_point(aes(x=as.factor(Index), y=Mean), color= "red") +
  scale_x_discrete(labels= unique(data3$Label)) +
  xlab("Ranked botanical country") +
  ylab("Expected benefit given model uncertainty") +
  # coord_flip() +
  # ylim(0,2.5) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(axis.text.x=element_text(angle=90,hjust=1),
        legend.position = "none")


gg #+ geom_point(aes(x=as.factor(Index), y=Mean, color= "red"))

ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_benefit_jitter_top100.pdf",
       width = 25, height = 15, units = "cm")




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


data2 = data2[data2$Index <=100,]

## use a white border of size 0.5 unit to separate the tiles
gg<- ggplot(data2, aes(x=as.factor(Index), y=Value, fill=Index), show.legend = FALSE) +
  # geom_tile(color="white", size=0.01) +
  geom_point(aes(x=as.factor(Index), y=Mean, color= "red")) +
  geom_boxplot(outlier.shape=NA, coef = 100) +
  scale_x_discrete(labels= unique(data2$Label)) +
  ylab("Rank") +
  xlab("Botanical Country") +
  coord_flip() +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  # scale_color_gradientn(colours=rev(RColorBrewer::brewer.pal(7, "YlGn")))+
  theme_classic() +
  theme(legend.position = "none")


gg

ggsave("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/randomised_rank_plot_top100.pdf",
       width = 15, height = 25, units = "cm")





