create_table <- (samples = 100, darkspots){
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

  #transform into a cumulative sum
  for (coli in 1:samples){
    random_benefits_table[,coli] = cumsum( random_benefits_table[,coli])
  }

  return(random_benefits_table)
}









plot_CE <- function(mean, UL, LL, weight,
                    shade_col  = rgb(col2rgb("grey")[1,1], col2rgb("grey")[2,1],col2rgb("grey")[3,1], max = 255, alpha = 50)){
  plot(-1,-1,lwd=2, type="l",pch=19, xlim=c(0,1.01),ylim=c(0,1.01),
       xlab = "Cumulative Cost",
       ylab = "Cumulative Expected Benefit")
  polygon(c(c(0,normalise(1:length(which(!is.na(weight) & !is.na(UL))))),
            c(rev(normalise(1:length(which(!is.na(weight) & !is.na(LL))))),0)),
          c(c(0,normalise(cumsum(sort(UL[which(!is.na(weight) & !is.na(UL))]*
                                        weight[which(!is.na(weight) & !is.na(UL))],
                                      decreasing=TRUE, na.last = T)))),
            c(rev(normalise(cumsum(sort(LL[which(!is.na(weight) & !is.na(LL))]*
                                          weight[which(!is.na(weight) & !is.na(LL))],
                                        decreasing=TRUE, na.last = T)))), 0)), col=shade_col, border=NA)
  random_benefits_table <- cbind(create_table(samples,mean),
                                 create_table(samples,LL),
                                 create_table(samples,UL))
  #summary statistics for random samples for plotting
  summary_random_benefits_table = matrix(NA, nrow = length(darkspots), ncol=3 )
  for (rowi in 1:length(darkspots)){
    summary_dta = summary(random_benefits_table[rowi,])
    summary_random_benefits_table[rowi,1] =  summary_dta[4] + 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[2] # 1st quartile
    summary_random_benefits_table[rowi,2] = summary_dta[4] # mean
    summary_random_benefits_table[rowi,3] = summary_dta[4] - 2.66*sum(abs(diff(random_benefits_table[rowi,])))/length(random_benefits_table[rowi,]) #summary_dta[5] # 3rd quartile
  }
  polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
            c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
          c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,1])])),
            c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0), col=shade_col, border=NA)

  lines(c(0,normalise(1:length(which(!is.na(benefit_2_W) & !is.na(mean))))),
        c(0,normalise(cumsum(sort(mean[which(!is.na(benefit_2_W) & !is.na(mean))]*
                                    benefit_2_W[which(!is.na(benefit_2_W) & !is.na(mean))],
                                  decreasing=TRUE, na.last = T)))),
        lwd=3, lty=1)
  lines(c(0,normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
        c(0,normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
        lwd=3, lty=1)
  text(0.9,0.7,"Random")
  text(0.2,0.8,"Optimised")

}


# scneario 2

polygon(c(c(0,normalise(1:length(which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls_UL))))),
          c(rev(normalise(1:length(which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls_LL))))),0)),
        c(c(0,normalise(cumsum(sort(data$SR_shortfalls_UL[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls_UL))]*
                                      benefit_2_W[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls_UL))],
                                    decreasing=TRUE, na.last = T)))),
          c(rev(normalise(cumsum(sort(
            data$SR_shortfalls_LL[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls_LL))]*
              benefit_2_W[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls_LL))],
            decreasing=TRUE, na.last = T)))), 0)),
        col=shade_col, border=NA)

polygon(c(c(0,normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),
          c(rev(normalise(1:length(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])])))),0),
        c(c(0,normalise(summary_random_benefits_table[,3][!is.na(summary_random_benefits_table[,1])])),
          c(rev(normalise(summary_random_benefits_table[,1][!is.na(summary_random_benefits_table[,1])]))),0), col=shade_col, border=NA)

lines(c(0,normalise(1:length(data$LEVEL3_NAM[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls))]))),
      c(0,normalise(cumsum(sort(data$SR_shortfalls[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls))]*
                                  benefit_2_W[which(!is.na(benefit_2_W) & !is.na(data$SR_shortfalls))],
                                decreasing=TRUE, na.last = T)))),
      lwd=3, lty=1)
lines(c(0,normalise(1:length(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])]))),
      c(0,normalise(summary_random_benefits_table[,2][!is.na(summary_random_benefits_table[,2])])),
      lwd=3, lty=1)
text(0.9,0.7,"Random")
text(0.2,0.8,"Optimised")
