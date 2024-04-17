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

##########################
# calculate Upper and Lower limits and scale
# moved this to 1st page
# df1 = normalise(log(st_drop_geometry(data)[c("SR_unknown_LL","SR_unknown_sd", "SR_unknown","SR_unknown_UL")]+1))
# df2 = normalise(log(st_drop_geometry(data)[c("SR_nogeoloc_LL", "SR_nogeoloc_sd","SR_nogeoloc","SR_nogeoloc_UL")]+1))
# df = df1 + df2
# colnames(df) = c("shortfalls_norm_index_LL", "shortfalls_norm_index_SD","shortfalls_norm_index_M","shortfalls_norm_index_UL")
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
#





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

for (scenario in 1:9){

  #--------------------------------
  # UNSCALED / SUM
  data[,paste0("benefit_",scenario,"_S")] = normalise(normalise(data$SR_nogeoloc) + normalise(data$SR_unknown)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S")]),
                                                   na.last = "keep", ties.method = "first")

  #--------------------------------
  # SCALED / SUM
  data[,paste0("benefit_",scenario,"_S_sc")] = normalise(normalise(data$SR_nogeoloc_sc) + normalise(data$SR_unknown_sc)) + get(paste0("weight_",scenario,"_S"))#data$shortfalls_norm_index + get(paste0("weight_",scenario,"_S"))
  data[,paste0("Rank_scenario_",scenario,"_S_sc")] <- rank(-st_drop_geometry(data[,paste0("benefit_",scenario,"_S_sc")]),
                                                        na.last = "keep", ties.method = "first")

}





tdwg3 = tdwg3 %>% left_join(data)




#####################################################
# Save


# save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/REV_app_data.RData"))
save(tdwg3, file = paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/REV_app_data.RData"))
write.csv(st_drop_geometry(tdwg3), paste0("C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/variables_table.csv"))

# rgdal::writeOGR(obj=tdwg3,
#                 dsn="C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp",
#                 layer="darkspots",
#                 driver="ESRI Shapefile")


# library(raster)
# save to shp but note that long data file names don't save properly so best to load app_data.R
st_write(tdwg3, 'C:/Users/kdh10kg/OneDrive - The Royal Botanic Gardens, Kew/darkspots/prep/REVISION_1/model_outputs.shp',layer = NULL, delete_layer = TRUE)
# st_write(tdwg3, filename=paste0(basepath, "/REVISION_1/model_outputs.shp"), delete_layer = TRUE)


