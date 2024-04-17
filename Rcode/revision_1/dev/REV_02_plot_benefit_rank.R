#PC1 Lower is richer
#PC2 lower has more protection
# load("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/app_data.RData")
library(plotly)
library(tidyverse)
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

normalise <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}



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

############################################################
#
# expected CE based on my gut
#
############################################################

# 1) Aucun "cout"
# 2) Pauvrete
# 3) Richesse
# 4) Pas de protection
# 5) Protection
# 6) Pauvrete et pas de protection
# 7) Pauvrete et protection
# 8) Richesse et pas de protection
# 9) Richesse et protection



par(mfrow=c(3,3))


# Prioritise areas with the most species left to discover
# data$benefit_1 = data$shortfalls_norm_index
data$CE_1 = normalise(data$shortfalls_norm_index)# data$SR_shortfalls #(1-normalise(data$SR_shortfalls))
data$CE_1 = normalise(data$CE_1)
data$Rank_CE_1 <- rank(-data$CE_1, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_1,
                     cost=1,
                     CE=data$CE_1,
                     rank = data$Rank_CE_1,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(to_plot$rank),
     normalise(to_plot$CE),
     type="o", pch=19,# xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(to_plot$rank)+0.01)[1:21],
     (normalise(to_plot$CE))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)


# prioritise areas with the most species in poor places to discover
# data$benefit_2 = data$shortfalls_norm_index + normalise(data$PC1)
data$CE_2 = normalise(data$shortfalls_norm_index) * normalise(data$PC1) #(1-normalise(data$SR_shortfalls))
data$CE_2 = normalise(data$CE_2)
data$Rank_CE_2 <- rank(-data$CE_2, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_2,
                     cost=1,
                     CE=data$CE_2,
                     rank = data$Rank_CE_2,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(to_plot$rank),
     normalise(to_plot$CE),
     type="o", pch=19,# xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(to_plot$rank)+0.01)[1:21],
     (normalise(to_plot$CE))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# prioritise areas with the most species in rich places to discover
# data$benefit_3 =data$shortfalls_norm_index + (1-(normalise(data$PC1)))
data$CE_3 = normalise(data$shortfalls_norm_index) * normalise(max(data$PC1,na.rm=T)-data$PC1)#(1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
data$CE_3 = normalise(data$CE_3)
data$Rank_CE_3 <- rank(-data$CE_3, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_3,
                     cost=1,
                     CE=data$CE_3,
                     rank = data$Rank_CE_3,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(to_plot$rank),
     normalise(to_plot$CE),
     type="o", pch=19,# xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(to_plot$rank)+0.01)[1:21],
     (normalise(to_plot$CE))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
# data$benefit_4 = (data$shortfalls_norm_index) + normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$CE_4 = normalise(data$shortfalls_norm_index) * normalise(data$PC2)#(1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
data$CE_4 = normalise(data$CE_4)
data$Rank_CE_4 <- rank(-data$CE_4, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_4,
                     cost=1,
                     CE=data$CE_4,
                     rank = data$Rank_CE_4,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(to_plot$rank),
     normalise(to_plot$CE),
     type="o", pch=19,# xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(to_plot$rank)+0.01)[1:21],
     (normalise(to_plot$CE))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
# data$benefit_6 = data$shortfalls_norm_index + normalise(normalise(data$PC1)+  (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$CE_6 = normalise(data$shortfalls_norm_index) * normalise(data$PC1 * data$PC2)
data$CE_6 = normalise(data$CE_6)
data$Rank_CE_6 <- rank(-data$CE_6, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_6,
                     cost=1,
                     CE=data$CE_6,
                     rank = data$Rank_CE_6,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(to_plot$rank),
     normalise(to_plot$CE),
     type="o", pch=19,# xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(to_plot$rank)+0.01)[1:21],
     (normalise(to_plot$CE))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
# data$benefit_8 = data$shortfalls_norm_index + normalise((1-normalise(data$PC1))+  (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$CE_8 = normalise(data$shortfalls_norm_index) * normalise((max(data$PC1,na.rm=T)-data$PC1) * data$PC2)
data$CE_8 = normalise(data$CE_8)
data$Rank_CE_8 <- rank(-data$CE_8, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_8,
                     cost=1,
                     CE=data$CE_8,
                     rank = data$Rank_CE_8,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)



#Prioritise areas with most species and smallest potential for biodiversity loss PC2
# data$benefit_5 =  (data$shortfalls_norm_index) + (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$CE_5 = normalise(data$shortfalls_norm_index) * normalise(max(data$PC2,na.rm=T)-data$PC2)#(1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
data$CE_5 = normalise(data$CE_5)
data$Rank_CE_5 <- rank(-data$CE_5, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_5,
                     cost=1,
                     CE=data$CE_5,
                     rank = data$Rank_CE_5,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)


# prioritise areas with the most species in poor places to discover with protection
# data$benefit_7 = data$shortfalls_norm_index + normalise(normalise(data$PC1)+  (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$CE_7 = normalise(data$shortfalls_norm_index) * normalise(data$PC1 *
                                                     (max(data$PC2,na.rm=T)-data$PC2))
data$CE_7 = normalise(data$CE_7)
data$Rank_CE_7 <- rank(-data$CE_7, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_7,
                     cost=1,
                     CE=data$CE_7,
                     rank = data$Rank_CE_7,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)


# prioritise areas with the most species in rich places to discover with protection
# data$benefit_9 = data$shortfalls_norm_index + normalise((1-normalise(data$PC1)) +  (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$CE_9 = normalise(data$shortfalls_norm_index) * normalise((max(data$PC1,na.rm=T)-data$PC1) *
                                                     (max(data$PC2,na.rm=T)-data$PC2))
data$CE_9 = normalise(data$CE_9)
data$Rank_CE_9 <- rank(-data$CE_9, na.last = "keep", ties.method = "first")

# plot
to_plot = data.frame(benefit = data$CE_9,
                     cost=1,
                     CE=data$CE_9,
                     rank = data$Rank_CE_9,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)





########################################################################
### OLD prioritisation
########################################################################

# 1) Aucun "cout"
# 2) Pauvrete
# 3) Richesse
# 4) Pas de protection
# 5) Protection
# 6) Pauvrete et pas de protection
# 7) Pauvrete et protection
# 8) Richesse et pas de protection
# 9) Richesse et protection

# Prioritise areas with the most species left to discover
data$benefit_1 = normalise(data$shortfalls_norm_index)#(1-normalise(data$SR_shortfalls))
data$benefit_1 = normalise(data$benefit_1)
data$Rank_scenatio_1 <- rank(-data$benefit_1, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places to discover
data$benefit_2 = normalise(data$shortfalls_norm_index) + normalise(data$PC1)#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_2 = normalise(data$benefit_2)
data$Rank_scenatio_2 <- rank(-data$benefit_2, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places to discover
data$benefit_3 =normalise(data$shortfalls_norm_index) + (1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
data$benefit_3 = normalise(data$benefit_3)
data$Rank_scenatio_3 <- rank(-data$benefit_3, na.last = "keep", ties.method = "first")

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
data$benefit_4 = (normalise(data$shortfalls_norm_index)) + normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$benefit_4 = normalise(data$benefit_4)
data$Rank_scenatio_4 <- rank(-data$benefit_4, na.last = "keep", ties.method = "first")

#Prioritise areas with most species and smallest potential for biodiversity loss PC2
data$benefit_5 =  (normalise(data$shortfalls_norm_index)) + (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$benefit_5 = normalise(data$benefit_5)
data$Rank_scenatio_5 <- rank(-data$benefit_5, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
data$benefit_6 = normalise(data$shortfalls_norm_index) + normalise(normalise(data$PC1) + (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_6 = normalise(data$benefit_6)
data$Rank_scenatio_6 <- rank(-data$benefit_6, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places to discover with protection
data$benefit_7 = normalise(data$shortfalls_norm_index) + normalise(normalise(data$PC1) + (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_7 = normalise(data$benefit_7)
data$Rank_scenatio_7 <- rank(-data$benefit_7, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
data$benefit_8 = normalise(data$shortfalls_norm_index) + normalise((1-normalise(data$PC1)) + (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_8 = normalise(data$benefit_8)
data$Rank_scenatio_8 <- rank(-data$benefit_8, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places to discover with protection
data$benefit_9 = normalise(data$shortfalls_norm_index) + normalise((1-normalise(data$PC1)) + (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_9 = normalise(data$benefit_9)
data$Rank_scenatio_9 <- rank(-data$benefit_9, na.last = "keep", ties.method = "first")


##############################################################################
##############################################################################
# Plot the OLD benefits
##############################################################################
##############################################################################

# plot
to_plot = data.frame(benefit = data$benefit_1,
                     cost=1,
                     CE=data$benefit_1,
                     rank = data$Rank_scenatio_1,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

#
#
# plot(c(0,to_plot$rank),
#      c(0,cumsum(to_plot$CE)),
#      type="o", pch=19,
#      xlab = "Countries ranked",
#      ylab = "Cumulative Darkspot index")
# text(c(0,to_plot$rank+10)[1:21],
#      (c(0,cumsum(to_plot$CE)))[1:21],
#      labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)
#
#
#
# plot(to_plot$rank,
#      1-to_plot$CE,
#      ylab  = "Normalised darkspot index",
#      xlab = "Ranked Countries",
#      type= "o",
#      ylim=c(0,1.05),
#      pch=19)
# text((to_plot$rank+7)[1:20],
#      to_plot$CE[1:20],
#      labels=to_plot$tdwg[1:20], cex=0.8, adj = 0)
#
#
# barplot(to_plot$CE,
#         names.arg=to_plot$tdwg,
#         ylab  = "Darkspot index",
#         xlab = "Ranked Countries",
#         las = 3,
#         ylim=c(0,1.1),
#         # yaxt = 'n',
#         cex.names = 0.5)
# text((to_plot$rank+10)[1:20],
#      to_plot$CE[1:20],
#      labels=to_plot$tdwg[1:20], cex=0.8, adj = 0)
#


# plot
to_plot = data.frame(benefit = data$benefit_2,
                     cost=1,
                     CE=data$benefit_2,
                     rank = data$Rank_scenatio_2,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_3,
                     cost=1,
                     CE=data$benefit_3,
                     rank = data$Rank_scenatio_3,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_4,
                     cost=1,
                     CE=data$benefit_4,
                     rank = data$Rank_scenatio_4,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_6,
                     cost=1,
                     CE=data$benefit_6,
                     rank = data$Rank_scenatio_6,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_8,
                     cost=1,
                     CE=data$benefit_8,
                     rank = data$Rank_scenatio_8,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_5,
                     cost=1,
                     CE=data$benefit_5,
                     rank = data$Rank_scenatio_5,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_7,
                     cost=1,
                     CE=data$benefit_7,
                     rank = data$Rank_scenatio_7,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

# plot
to_plot = data.frame(benefit = data$benefit_9,
                     cost=1,
                     CE=data$benefit_9,
                     rank = data$Rank_scenatio_9,
                     tdwg = data$LEVEL3_NAM)
to_plot = to_plot[order(as.numeric(to_plot$rank)),]
plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19, xlim=c(0,0.1), ylim=c(0,0.4),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)

#################################################################################################
# Create a table of ranks
#################################################################################################

samples = 10000

#create table to randomise darkspots and therefore benefits nd ranks
darkspot_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= samples)
benefit_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= samples)
rank_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= samples)

rownames(darkspot_table) = data$LEVEL3_NAM
rownames(benefit_table) = data$LEVEL3_NAM
rownames(rank_table) = data$LEVEL3_NAM

# create ranndom numbers of species within predictions
for (rowi in 1:length(data$LEVEL3_NAM)){
  darkspot_table[rowi,] = rnorm(samples, mean = data$SR_shortfalls[rowi], sd = (data$SR_shortfalls_UL[rowi] - data$SR_shortfalls[rowi])/3)
}

darkspot_table = ifelse(darkspot_table<0,0,round(darkspot_table))

#estimate benefits and rank them
for (coli in 1:samples){
  benefit_table[,coli] = normalise(darkspot_table[,coli])
  rank_table[,coli] = rank(-benefit_table[,coli], na.last = "keep", ties.method = "first")
}


par(mfrow=c(1,1))
# find the benefit for each rank and sort them in a new table and keep track of the country
ranked_benefits_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= samples)
ranked_country_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= samples)
plot(-1,-1, xlim=c(0,1.01),ylim=c(0,1.01),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
for (coli in 1:samples){
  for (rowi in 1:length(data$LEVEL3_NAM)){
    rank = as.numeric(rank_table[rowi,coli])
    ranked_benefits_table[rank,coli] = benefit_table[rowi,coli]
    ranked_country_table[rank,coli] = names(benefit_table[rowi,coli])
  }
  lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(ranked_benefits_table[,coli])]))),
        c(0,normalise(cumsum(ranked_benefits_table[!is.na(ranked_benefits_table[,coli]),coli]))), col= "grey")
}
lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls)]))),
      c(0,normalise(cumsum(sort(data$SR_shortfalls[!is.na(data$SR_shortfalls)],
                                decreasing=TRUE, na.last = T)))),lwd=1)


##################################################################
# Create a table of cumulative benefits over random ranks
##################################################################

# create random table
random_ranks_table = matrix(rnorm(length(data$LEVEL3_NAM)* samples, 0, 2), nrow = length(data$LEVEL3_NAM), ncol= samples)
random_benefits_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= samples)

for (coli in 1:samples){
  # rank the random numbers in each column
  random_ranks_table[,coli] = rank(-random_ranks_table[,coli], na.last = "keep", ties.method = "first")
  for (rowi in 1:length(data$LEVEL3_NAM)){
    # use the rank to look up the matching shortfall
    random_benefits_table[rowi,coli] = data$SR_shortfalls[random_ranks_table[rowi,coli]]
  }
  # random_benefits_table[,coli] = cumsum( random_benefits_table[,coli])
}
random_benefits_table = ifelse(is.na(random_benefits_table), 0, random_benefits_table)

#transform into a cumulative sum
for (coli in 1:samples){
  random_benefits_table[,coli] = cumsum( random_benefits_table[,coli])
}

#summary statistics for random samples for plotting
summary_random_benefits_table = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol=3 )
for (rowi in 1:length(data$LEVEL3_NAM)){
  summary_dta = summary(random_benefits_table[rowi,])
  summary_random_benefits_table[rowi,1] =  summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
  summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
  summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
}



##################################################################
# Now plot upper and lower limit
##################################################################
plot(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls)]))),
      c(0,normalise(cumsum(sort(data$SR_shortfalls[!is.na(data$SR_shortfalls)], decreasing=TRUE, na.last = T)))),
     lwd=2, type="l",pch=19,
     xlim=c(0,1.01),ylim=c(0,1.01),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
# lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls_UL)]))),
#       c(0,normalise(cumsum(sort(data$SR_shortfalls_UL[!is.na(data$SR_shortfalls_UL)],
#                                 decreasing=TRUE, na.last = T)))),lwd=1, lty=2)
# lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls_LL)]))),
#       c(0,normalise(cumsum(sort(data$SR_shortfalls_LL[!is.na(data$SR_shortfalls_LL)],
#                                 decreasing=TRUE, na.last = T)))),lwd=1, lty=2)

polygon(c(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls_LL)]))),
          c(rev(normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls_LL)])))),0),
        c(c(0,normalise(cumsum(sort(data$SR_shortfalls_UL[!is.na(data$SR_shortfalls_UL)],
                                    decreasing=TRUE, na.last = T)))),
          c(rev(normalise(cumsum(sort(data$SR_shortfalls_LL[!is.na(data$SR_shortfalls_LL)],
                                      decreasing=TRUE, na.last = T))))),0), col="grey", border="grey")


polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
        c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,1])])),
          c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0), col="grey", border="grey")


lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_shortfalls)]))),
      c(0,normalise(cumsum(sort(data$SR_shortfalls[!is.na(data$SR_shortfalls)], decreasing=TRUE, na.last = T)))),
      lwd=3, lty=1)
lines(c(0,normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
      c(0,normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
      lwd=3, lty=1)
text(0.9,0.7,"Random")
text(0.2,0.8,"Optimised")


# Linnean
plot(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_unknown)]))),
     c(0,normalise(cumsum(sort(data$SR_unknown[!is.na(data$SR_unknown)],
                               decreasing=TRUE, na.last = T)))),
     lwd=2, type="l",pch=19,
     xlim=c(0,1.01),ylim=c(0,1.01),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
# lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_unknown_UL)]))),
#       c(0,normalise(cumsum(sort(data$SR_unknown_UL[!is.na(data$SR_unknown_UL)],
#                                 decreasing=TRUE, na.last = T)))),lwd=1, lty=2)
# lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_unknown_LL)]))),
#       c(0,normalise(cumsum(sort(data$SR_unknown_LL[!is.na(data$SR_unknown_LL)],
#                                 decreasing=TRUE, na.last = T)))),lwd=1, lty=2)
polygon(c(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_unknown_LL)]))),
          c(rev(normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_unknown_LL)])))),0),
        c(c(0,normalise(cumsum(sort(data$SR_unknown_UL[!is.na(data$SR_unknown_UL)],
                                    decreasing=TRUE, na.last = T)))),
          c(rev(normalise(cumsum(sort(data$SR_unknown_LL[!is.na(data$SR_unknown_LL)],
                                    decreasing=TRUE, na.last = T))))),0), col="cornflowerblue", border="cornflowerblue")

lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_unknown)]))),
      c(0,normalise(cumsum(sort(data$SR_unknown[!is.na(data$SR_unknown)], decreasing=TRUE, na.last = T)))),
      lwd=2, lty=1)


#Wallacean
plot(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_nogeoloc)]))),
     c(0,normalise(cumsum(sort(data$SR_nogeoloc[!is.na(data$SR_nogeoloc)], decreasing=TRUE, na.last = T)))),
     lwd=2, type="l",pch=19,
     xlim=c(0,1.01),ylim=c(0,1.01),
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
# lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_nogeoloc_UL)]))),
#       c(0,normalise(cumsum(sort(data$SR_nogeoloc_UL[!is.na(data$SR_nogeoloc_UL)],
#                                 decreasing=TRUE, na.last = T)))),lwd=1, lty=2)
# lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_nogeoloc_LL)]))),
#       c(0,normalise(cumsum(sort(data$SR_nogeoloc_LL[!is.na(data$SR_nogeoloc_LL)],
#                                 decreasing=TRUE, na.last = T)))),lwd=1, lty=2)

polygon(c(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_nogeoloc_LL)]))),
          c(rev(normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_nogeoloc_LL)])))),0),
        c(c(0,normalise(cumsum(sort(data$SR_nogeoloc_UL[!is.na(data$SR_nogeoloc_UL)],
                                    decreasing=TRUE, na.last = T)))),
          c(rev(normalise(cumsum(sort(data$SR_nogeoloc_LL[!is.na(data$SR_nogeoloc_LL)],
                                      decreasing=TRUE, na.last = T))))),0), col="brown", border="brown")

lines(c(0,normalise(1:length(data$LEVEL3_NAM[!is.na(data$SR_nogeoloc)]))),
      c(0,normalise(cumsum(sort(data$SR_nogeoloc[!is.na(data$SR_nogeoloc)], decreasing=TRUE, na.last = T)))),
      lwd=3, lty=1)


# ###############################
# # plot UL and mean together.
#
#
#
# #create a table summarising the benefits
# summary_benefits = matrix(NA, nrow = length(data$LEVEL3_NAM), ncol= 3)
# colnames = c("1st Qu.","Mean","3rd Qu.")
# for (rowi in 1:length(data$LEVEL3_NAM)){
#   summary_benefits[rowi,1] = summary(ranked_benefits_table[rowi,])[2]
#   summary_benefits[rowi,2] = summary(ranked_benefits_table[rowi,])[4]
#   summary_benefits[rowi,3] = summary(ranked_benefits_table[rowi,])[5]
# }
#
#
# par(mfrow=c(1,1))
# plot(c(0,normalise(cumsum(summary_benefits[,3]))), lty=2, type="l")
# lines(c(0,normalise(cumsum(summary_benefits[,2]))), pch=19,type="o",lwd=0.5)
# lines(c(0,normalise(cumsum(summary_benefits[,1]))), lty=2)
#

############################################################################################
#  SCENARIOS with upper and lower limits with sums
############################################################################################

# Prioritise areas with the most species left to discover
benefit_1_S = rep(1, length(data$benefit_1))

# prioritise areas with the most species in poor places to discover
benefit_2_S = normalise(data$PC1) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover
benefit_3_S = (1-(normalise(data$PC1))) #(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
benefit_4_S = normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

#Prioritise areas with most species and smallest potential for biodiversity loss PC2
benefit_5_S =  (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
benefit_6_S = normalise(normalise(data$PC1) + (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in poor places to discover with protection
benefit_7_S = normalise(normalise(data$PC1) + (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
benefit_8_S = normalise((1-normalise(data$PC1)) + (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover with protection
benefit_9_S = normalise((1-normalise(data$PC1)) + (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))


##############################
#  PLOT
##############################

par(mfrow=c(3,3))

shade_col  = rgb(col2rgb("grey30")[1,1], col2rgb("grey30")[2,1],col2rgb("grey30")[3,1], max = 255, alpha = 75)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
            collections = data$descriptions,
            weight = benefit_1_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_2_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_3_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_4_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_6_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_8_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_5_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_7_S)

plot_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_9_S)



##############################################

par(mfrow=c(3,3))
plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
                 collections = data$descriptions,
                 weight = benefit_1_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_2_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_3_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_4_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_6_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_8_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_5_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_7_S)

plot_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_9_S)


############################################################################################
#  SCENARIOS with upper and lower limits with Weights
############################################################################################

# Prioritise areas with the most species left to discover
benefit_1_W = rep(1, length(data$benefit_1))

# prioritise areas with the most species in poor places to discover
benefit_2_W = normalise(data$PC1) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover
benefit_3_W = (1-(normalise(data$PC1))) #(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
benefit_4_W = normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

#Prioritise areas with most species and smallest potential for biodiversity loss PC2
benefit_5_W =  (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
benefit_6_W = normalise(normalise(data$PC1) * (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in poor places to discover with protection
benefit_7_W = normalise(normalise(data$PC1) * (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
benefit_8_W = normalise((1-normalise(data$PC1)) * (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover with protection
benefit_9_W = normalise((1-normalise(data$PC1)) * (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

##############################
#  PLOT
##############################

par(mfrow=c(3,3))

shade_col  = rgb(col2rgb("grey30")[1,1], col2rgb("grey30")[2,1],col2rgb("grey30")[3,1], max = 255, alpha = 75)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
                 collections = data$descriptions,
                 weight = benefit_1_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_2_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_3_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_4_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_6_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_8_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_5_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_7_W)

plot_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_9_W)



########################################################################################################################

par(mfrow=c(3,3))
plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
                      collections = data$descriptions,
                      weight = benefit_1_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_2_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_3_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_4_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_6_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_8_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_5_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_7_W)

plot_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_9_W)

#########################################################################################################

############################################################################################
#  SCENARIOS with upper and lower limits with sums
############################################################################################

# Prioritise areas with the most species left to discover
benefit_1_S = rep(1, length(data$benefit_1))

# prioritise areas with the most species in poor places to discover
benefit_2_S = normalise(data$PC1) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover
benefit_3_S = (1-(normalise(data$PC1))) #(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
benefit_4_S = normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

#Prioritise areas with most species and smallest potential for biodiversity loss PC2
benefit_5_S =  (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
benefit_6_S = normalise(normalise(data$PC1) + (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in poor places to discover with protection
benefit_7_S = normalise(normalise(data$PC1) + (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
benefit_8_S = normalise((1-normalise(data$PC1)) + (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover with protection
benefit_9_S = normalise((1-normalise(data$PC1)) + (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))


##############################
#  PLOT
##############################

par(mfrow=c(3,3))

shade_col  = rgb(col2rgb("grey30")[1,1], col2rgb("grey30")[2,1],col2rgb("grey30")[3,1], max = 255, alpha = 75)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
            collections = data$descriptions,
            weight = benefit_1_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_2_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_3_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_4_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_6_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_8_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_5_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_7_S)

plot_raw_CE_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
            collections = data$descriptions,
            weight = benefit_9_S)



##############################################

par(mfrow=c(3,3))
plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
                 collections = data$descriptions,
                 weight = benefit_1_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_2_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_3_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_4_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_6_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_8_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_5_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_7_S)

plot_raw_CE_hist_sum(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_9_S)


############################################################################################
#  SCENARIOS with upper and lower limits with Weights
############################################################################################

# Prioritise areas with the most species left to discover
benefit_1_W = rep(1, length(data$benefit_1))

# prioritise areas with the most species in poor places to discover
benefit_2_W = normalise(data$PC1) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover
benefit_3_W = (1-(normalise(data$PC1))) #(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
benefit_4_W = normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

#Prioritise areas with most species and smallest potential for biodiversity loss PC2
benefit_5_W =  (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
benefit_6_W = normalise(normalise(data$PC1) * (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in poor places to discover with protection
benefit_7_W = normalise(normalise(data$PC1) * (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
benefit_8_W = normalise((1-normalise(data$PC1)) * (normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

# prioritise areas with the most species in rich places to discover with protection
benefit_9_W = normalise((1-normalise(data$PC1)) * (1-normalise(data$PC2))) #(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))

##############################
#  PLOT
##############################

par(mfrow=c(3,3))

shade_col  = rgb(col2rgb("grey30")[1,1], col2rgb("grey30")[2,1],col2rgb("grey30")[3,1], max = 255, alpha = 75)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
                 collections = data$descriptions,
                 weight = benefit_1_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_2_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_3_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_4_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_6_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_8_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_5_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_7_W)

plot_raw_CE_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                 collections = data$descriptions,
                 weight = benefit_9_W)



########################################################################################################################

par(mfrow=c(3,3))
plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL, add_legend=T,
                      collections = data$descriptions,
                      weight = benefit_1_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_2_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_3_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_4_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_6_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_8_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_5_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_7_W)

plot_raw_CE_hist_multiply(mean = data$SR_shortfalls,UL = data$SR_shortfalls_UL,LL = data$SR_shortfalls_LL,
                      collections = data$descriptions,
                      weight = benefit_9_W)

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

to_keep = which(!is.na(data$Rank_scenatio_1))
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_2[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_3[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_4[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_5[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_6[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_7[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_8[to_keep])
wilcox.test(data$Rank_scenatio_1[to_keep], data$Rank_scenatio_9[to_keep])

wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_2[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_3[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_4[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_5[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_6[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_7[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_8[to_keep])
wilcox.test(data$Rank_CE_1[to_keep],data$Rank_CE_9[to_keep])


wilcox.test(data$Rank_CE_9[to_keep],data$Rank_scenatio_9[to_keep])

par(mfrow=c(1,1))
plot(data$Rank_scenatio_9[to_keep],
     data$benefit_9[to_keep], pch=19)
points(data$Rank_CE_9[to_keep],
     data$CE_9[to_keep], col="brown", pch=19)

text(data$Rank_scenatio_9[to_keep]+5,
     data$benefit_9[to_keep]+0.01,
     data$LEVEL3_COD[to_keep], cex=0.3, adj = 0,  srt=20)

text(data$Rank_CE_9[to_keep]+5,
     data$CE_9[to_keep]+0.01,
     data$LEVEL3_COD[to_keep], cex=0.3, adj = 0, col="brown", srt=20)


#########################################################################################################
data$rank_diff_1_2 = data$Rank_scenatio_1 - data$Rank_scenatio_2 #-
data$rank_diff_1_3 = data$Rank_scenatio_1 - data$Rank_scenatio_3 #- data$Rank_scenatio_1
data$rank_diff_1_4 = data$Rank_scenatio_1 - data$Rank_scenatio_4 #- data$Rank_scenatio_1
data$rank_diff_1_5 = data$Rank_scenatio_1 - data$Rank_scenatio_5 #- data$Rank_scenatio_1
data$rank_diff_1_6 = data$Rank_scenatio_1 - data$Rank_scenatio_6 #- data$Rank_scenatio_1
data$rank_diff_1_7 = data$Rank_scenatio_1 - data$Rank_scenatio_7 #- data$Rank_scenatio_1
data$rank_diff_1_8 = data$Rank_scenatio_1 - data$Rank_scenatio_8 #- data$Rank_scenatio_1
data$rank_diff_1_9 = data$Rank_scenatio_1 - data$Rank_scenatio_9 #- data$Rank_scenatio_1



data$PC1_normalised = normalise(data$PC1)
data$PC2_normalised = normalise(data$PC2)
data$PC3_normalised = normalise(data$PC3)


##############################################################
##### CUMULATIVWE RAW BENEFIT
#################################################################

# Combined benefit
benefit = data$SR_nogeoloc * data$SR_unknown
to_plot = data.frame(benefit = benefit,
                     rank = rank(-benefit, na.last = "keep", ties.method = "first"),
                     tdwg = data$LEVEL3_COD)

to_plot = to_plot[order(as.numeric(to_plot$rank)),]

plot(c(0,to_plot$rank),
     c(0,cumsum(to_plot$benefit)),
     type="o", pch=19)

# Linnean
benefit = data$SR_nogeoloc
to_plot = data.frame(benefit = benefit,
                     rank = rank(-benefit, na.last = "keep", ties.method = "first"),
                     tdwg = data$LEVEL3_COD)

to_plot = to_plot[order(as.numeric(to_plot$rank)),]

plot(c(0,to_plot$rank),
     c(0,cumsum(to_plot$benefit)),
     type="o", pch=19, col= "cornflowerblue")



# Wallacean
benefit = data$SR_unknown
to_plot = data.frame(benefit = benefit,
                     rank = rank(-benefit, na.last = "keep", ties.method = "first"),
                     tdwg = data$LEVEL3_COD)

to_plot = to_plot[order(as.numeric(to_plot$rank)),]

plot(c(0,to_plot$rank),
      c(0,cumsum(to_plot$benefit)),
      type="o", pch=19, col= "brown")


#####################################
# PC1
# Combined benefit
benefit = data$benefit_1#(data$SR_nogeoloc * data$SR_unknown *(data$PC1+3))
cost = 1#(scale(-data$PC1, scale=T, center = F) + 3)
CE = benefit/cost
to_plot = data.frame(benefit = benefit,
                     cost=cost,
                     CE=CE,
                     rank = rank(-CE, na.last = "keep", ties.method = "first"),
                     tdwg = data$LEVEL3_NAM)

to_plot = to_plot[order(as.numeric(to_plot$rank)),]


plot(normalise(c(0,to_plot$rank)),
     normalise(c(0,cumsum(to_plot$CE))),
     type="o", pch=19,
     xlab = "Cumulative Cost",
     ylab = "Cumulative Expected Benefit")
text((normalise(c(0,(to_plot$rank)))+0.01)[1:21],
     (normalise(c(0,cumsum(to_plot$CE))))[1:21],
     labels=c("", to_plot$tdwg[1:20]), cex=0.8, adj = 0)


plot(normalise(c(0,cumsum(to_plot$cost))),
     normalise(c(0,cumsum(to_plot$benefit))),
     type="o", pch=19, col="grey40")

plot(normalise(to_plot$cost),
     normalise(to_plot$benefit),pch=19, col="grey40")


# Linnean
benefit = data$SR_nogeoloc/ (scale(data$PC1, scale=T, center = F) + 3)
to_plot = data.frame(benefit = benefit,
                     rank = rank(-benefit, na.last = "keep", ties.method = "first"),
                     tdwg = data$LEVEL3_COD)

to_plot = to_plot[order(as.numeric(to_plot$rank)),]

plot(c(0,to_plot$rank),
     c(0,cumsum(to_plot$benefit)),
     type="o", pch=19, col= "cornflowerblue")



# Wallacean
benefit = data$SR_unknown/ (scale(data$PC1, scale=T, center = F) + 3)
to_plot = data.frame(benefit = benefit,
                     rank = rank(-benefit, na.last = "keep", ties.method = "first"),
                     tdwg = data$LEVEL3_COD)

to_plot = to_plot[order(as.numeric(to_plot$rank)),]

plot(c(0,to_plot$rank),
     normalise(c(0,cumsum(to_plot$benefit))),
     type="o", pch=19, col= "brown")


plot(normalise(c(0, cumsum(scale(data$PC1, scale=T, center = F) + 3))))

####





data$SR_shortfalls = data$SR_nogeoloc * data$SR_unknown
# data$shortfalls_norm_index = scale(data$SR_shortfalls, center=F)

data$SR_shortfalls_UL = data$SR_nogeoloc_UL + data$SR_unknown_UL
data$shortfalls_norm_index_UL = scale(data$SR_shortfalls_UL, center=F)

to_plot = cbind(data$Rank_scenatio_2_UL,
                (1-data$benefit_2_UL),
                data$LEVEL3_COD)


to_plot = to_plot[order(as.numeric(to_plot[,1])),]

plot(to_plot[,1],to_plot[,2],type="o", pch=19)




#########################################################################
# UPPER LIMIT
#########################################################################

data$SR_shortfalls = data$SR_nogeoloc + data$SR_unknown
data$shortfalls_norm_index = scale(data$SR_shortfalls, center=F)

data$SR_shortfalls_UL = data$SR_nogeoloc_UL + data$SR_unknown_UL
data$shortfalls_norm_index_UL = scale(data$SR_shortfalls_UL, center=F)

to_plot = cbind(data$Rank_scenatio_2_UL,
               (1-data$benefit_2_UL),
               data$LEVEL3_COD)


to_plot = to_plot[order(as.numeric(to_plot[,1])),]

plot(to_plot[,1],to_plot[,2],type="o", pch=19)




# Prioritise areas with the most species left to discover
data$benefit_1_UL = data$shortfalls_norm_index_UL#(1-normalise(data$SR_shortfalls))
data$benefit_1_UL = normalise(data$benefit_1_UL)
data$Rank_scenatio_1_UL <- rank(-data$benefit_1_UL, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places to discover
data$benefit_2_UL = data$shortfalls_norm_index_UL + normalise(data$PC1)#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_2_UL = normalise(data$benefit_2_UL)
data$Rank_scenatio_2_UL <- rank(-data$benefit_2_UL, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places to discover
data$benefit_3_UL =data$shortfalls_norm_index_UL + (1-(normalise(data$PC1)))#(1-normalise(data$SR_shortfalls)) + ((normalise(data$PC1)))
data$benefit_3_UL = normalise(data$benefit_3_UL)
data$Rank_scenatio_3_UL <- rank(-data$benefit_3_UL, na.last = "keep", ties.method = "first")

#Prioritise areas with most species and biggest potential for biodiversity loss PC2
data$benefit_4_UL = (data$shortfalls_norm_index_UL) + normalise(data$PC2) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$benefit_4_UL = normalise(data$benefit_4_UL)
data$Rank_scenatio_4_UL <- rank(-data$benefit_4_UL, na.last = "keep", ties.method = "first")


#Prioritise areas with most species and smallest potential for biodiversity loss PC2
data$benefit_5_UL =  (data$shortfalls_norm_index_UL) + (1-normalise(data$PC2)) #(1-(normalise(data$PC2))) + (1-normalise(data$SR_shortfalls))
data$benefit_5_UL = normalise(data$benefit_5_UL)
data$Rank_scenatio_5_UL <- rank(-data$benefit_5_UL, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places and biggest potential for biodiversity loss PC2
data$benefit_6_UL = data$shortfalls_norm_index_UL + normalise(normalise(data$PC1)+  (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_6_UL = normalise(data$benefit_6_UL)
data$Rank_scenatio_6_UL <- rank(-data$benefit_6_UL, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in poor places to discover with protection
data$benefit_7_UL = data$shortfalls_norm_index_UL + normalise(normalise(data$PC1)+  (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_7_UL = normalise(data$benefit_7_UL)
data$Rank_scenatio_7_UL <- rank(-data$benefit_7_UL, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places and biggest potential for biodiversity loss PC2
data$benefit_8_UL = data$shortfalls_norm_index_UL + normalise((1-normalise(data$PC1))+  (normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_8_UL = normalise(data$benefit_8_UL)
data$Rank_scenatio_8_UL <- rank(-data$benefit_8_UL, na.last = "keep", ties.method = "first")

# prioritise areas with the most species in rich places to discover with protection
data$benefit_9_UL = data$shortfalls_norm_index_UL + normalise((1-normalise(data$PC1)) +  (1-normalise(data$PC2)))#(1-normalise(data$SR_shortfalls)) + (1-(normalise(data$PC1)))
data$benefit_9_UL = normalise(data$benefit_9_UL)
data$Rank_scenatio_9_UL <- rank(-data$benefit_9_UL, na.last = "keep", ties.method = "first")


##############################################################


# data$rank_diff_1_2 = data$Rank_scenatio_1 - data$Rank_scenatio_2 #-
# data$rank_diff_1_3 = data$Rank_scenatio_1 - data$Rank_scenatio_3 #- data$Rank_scenatio_1
# data$rank_diff_1_4 = data$Rank_scenatio_1 - data$Rank_scenatio_4 #- data$Rank_scenatio_1
# data$rank_diff_1_5 = data$Rank_scenatio_1 - data$Rank_scenatio_5 #- data$Rank_scenatio_1
# data$rank_diff_1_6 = data$Rank_scenatio_1 - data$Rank_scenatio_6 #- data$Rank_scenatio_1
# data$rank_diff_1_7 = data$Rank_scenatio_1 - data$Rank_scenatio_7 #- data$Rank_scenatio_1
# data$rank_diff_1_8 = data$Rank_scenatio_1 - data$Rank_scenatio_8 #- data$Rank_scenatio_1
# data$rank_diff_1_9 = data$Rank_scenatio_1 - data$Rank_scenatio_9 #- data$Rank_scenatio_1





#PLOT THE LOT
#
# t <- list(
#   family = "sans serif",
#   size = 12,
#   color = toRGB("grey50"))
#
# plot_ly(data = data, x = ~Rank_scenatio_2, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_2, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise lower income group regions') %>%
#   add_text(textfont = t, textposition = "top")
#
#
# plot_ly(data = data, x = ~Rank_scenatio_3, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_3, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise higher income group  regions') %>%
#   add_text(textfont = t, textposition = "top")
#
#
# plot_ly(data = data, x = ~Rank_scenatio_4, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_4, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise less protected regions') %>%
#   add_text(textfont = t, textposition = "top")
#
#
# plot_ly(data = data, x = ~Rank_scenatio_5, y = ~Rank_scenatio_1,
#         text = ~LEVEL3_NAM,
#         color = ~rank_diff_1_5, size = 3) %>%
#   add_markers() %>%
#   layout(title = 'Prioritise lower income regions with least protection') %>%
#   add_text(textfont = t, textposition = "top")


s2 <- ggplot(data = data, aes(x = Rank_scenatio_2, y = Rank_scenatio_1,
        label = LEVEL3_COD,#LEVEL3_NAM,
        color = rank_diff_1_2), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 2", y ="Rank scenario 1")+
  theme_bw()

s2
ggsave(paste0(basepath, "REVISION_1/Rank_1_2.pdf"), width = 20, height = 16, units = "cm")




s3 <- ggplot(data = data, aes(x = Rank_scenatio_3, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_3), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 3", y ="Rank scenario 1")+
  theme_bw()

s3
ggsave(paste0(basepath, "REVISION_1/Rank_1_3.pdf"), width = 20, height = 16, units = "cm")




s4 <- ggplot(data = data, aes(x = Rank_scenatio_4, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_4), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 4", y ="Rank scenario 1")+
  theme_bw()

s4
ggsave(paste0(basepath, "REVISION_1/Rank_1_4.pdf"), width = 20, height = 16, units = "cm")



s5 <- ggplot(data = data, aes(x = Rank_scenatio_5, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_5), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 5", y ="Rank scenario 1")+
  theme_bw()

s5
ggsave(paste0(basepath, "REVISION_1/Rank_1_5.pdf"), width = 20, height = 16, units = "cm")





s6 <- ggplot(data = data, aes(x = Rank_scenatio_6, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_6), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 6", y ="Rank scenario 1")+
  theme_bw()

s6
ggsave(paste0(basepath, "REVISION_1/Rank_1_6.pdf"), width = 20, height = 16, units = "cm")





s7 <- ggplot(data = data, aes(x = Rank_scenatio_7, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_7), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 7", y ="Rank scenario 1")+
  theme_bw()

s7
ggsave(paste0(basepath, "REVISION_1/Rank_1_7.pdf"), width = 20, height = 16, units = "cm")




s8 <- ggplot(data = data, aes(x = Rank_scenatio_8, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_8), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 8", y ="Rank scenario 1")+
  theme_bw()

s8
ggsave(paste0(basepath, "REVISION_1/Rank_1_8.pdf"), width = 20, height = 16, units = "cm")






s9 <- ggplot(data = data, aes(x = Rank_scenatio_9, y = Rank_scenatio_1,
                              label = LEVEL3_COD,#LEVEL3_NAM,
                              color = rank_diff_1_9), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  # geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(11, "RdYlBu")) +
  # gradient_color_brewer(palette="Dark2") +
  geom_text() +
  labs(color = "Rank change", x = "Rank scenario 9", y ="Rank scenario 1")+
  theme_bw()

s9
ggsave(paste0(basepath, "REVISION_1/Rank_1_9.pdf"), width = 20, height = 16, units = "cm")

##############################################################
# Find countries that are consistently top of prioritisation
#sun the ranks
data$Rank_sum = (data$Rank_scenatio_1+
                    data$Rank_scenatio_2+
                    data$Rank_scenatio_3+
                    data$Rank_scenatio_4+
                    data$Rank_scenatio_5+
                    data$Rank_scenatio_6+
                    data$Rank_scenatio_7+
                    data$Rank_scenatio_8+
                    data$Rank_scenatio_9)

# rank the ranks
data$Rank_sum_ranked = rank(data$Rank_sum, na.last = "keep", ties.method = "first")
data$Rank_sum_ranked


#sun the absolute rank differences
data$Rank_diff_sum = (abs(data$rank_diff_1_2)+
                    abs(data$rank_diff_1_3)+
                    abs(data$rank_diff_1_4)+
                    abs(data$rank_diff_1_5)+
                    abs(data$rank_diff_1_6)+
                    abs(data$rank_diff_1_7)+
                    abs(data$rank_diff_1_8)+
                    abs(data$rank_diff_1_9))

# rank the ranks
data$Rank_diff_sum_ranked = rank(data$Rank_diff_sum, na.last = "keep", ties.method = "first")
data$Rank_diff_sum_ranked


# #PLOT THE LOT
# plot_ly(data = data, x = ~Rank_scenatio_3, y = ~Rank_scenatio_1,
#         text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~rank_diff_1_3, size = 3) %>%
#   layout(title = 'Prioritise Poor regions')
#
#
# #PLOT THE LOT
# plot_ly(data = data, x = ~Rank_scenatio_4, y = ~Rank_scenatio_1,
#        text = ~paste("TDWG: ", LEVEL3_COD),
#         color = ~rank_diff_1_4, size = 3) %>%
#   layout(title = 'Prioritise regions with less protection')

######

#Save PCA and Ranks

tdwg3@data = data

save(tdwg3, file = paste0("C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/REV_app_data.RData"))
save(tdwg3, file = paste0(basepath, "REVISION_1/app_data.RData"))
write.csv(tdwg3@data, paste0(basepath, "REVISION_1/variables_table.csv"))

# rgdal::writeOGR(obj=tdwg3,
#                 dsn="C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/model_outputs.shp",
#                 layer="darkspots",
#                 driver="ESRI Shapefile")


library(raster)
shapefile(tdwg3, filename='C:/Users/kdh10kg/Documents/github/darkspots_shiny/darkspots_shiny/REVISION_1/model_outputs.shp', overwrite=TRUE)
shapefile(tdwg3, filename=paste0(basepath, "/REVISION_1/model_outputs.shp"), overwrite=TRUE)


