library(dplyr)
filename="NWC.csv"#new caledonia
filename = "GAB.csv"#gabon
filename = "BGM.csv"#belgium
filename = "FRA.csv"#france
filename = "CLM.csv"#colombia
filename = "SWI.csv" #switzerland

dta = read.csv(paste0("C:/Users/kdh10kg/Documents/github/darkspots/data/tdwg_data/",filename),
               sep=",", header=TRUE)

#randomly allocate year to unknown data (need to discuss)
dta[is.na(dta$year),"year"] = sample(min(dta$year,na.rm=T):max(dta$year,na.rm=T), length(dta[is.na(dta$year),"year"]), replace=TRUE)

# count number of species per year
new_spp_per_yr = dta %>% count(year)

#plot
par(mfrow=c(1,1))
par(mfrow=c(2,2))
plot(new_spp_per_yr$year, new_spp_per_yr$n, xlab="Year", ylab="Number of Species Discovered", pch=19, type="o")
plot(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), xlab="Year", ylab="Cumulative Number of Species", pch=19, type="o")
# par(mfrow=c(1,1))


#Randomise the years (think this is incorrect and can ignore)
plot(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), xlab="Year", ylab="Randomised collection year", pch=19, type="o")
for (i in 1:200){
  new_spp_per_yr = transform( new_spp_per_yr, n = sample(n))#nc_shuffled %>% count(year)
  lines(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), col="brown")
}
new_spp_per_yr = dta %>% count(year)
points(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), pch=19, type="o")

#Fit a logistic curve to the data
x = new_spp_per_yr$year
y = cumsum(new_spp_per_yr$n)
fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data = data.frame(x, y))
summary(fit)
out=predict(fit, newdata = data.frame(x = seq(1600, 2500, length.out = 900)))
plot(y ~ x, xlim= c(min(x),2500), ylim=c(0,(max(out,max(cumsum(new_spp_per_yr$n)))+100)), xlab="Year", ylab="Cumulative number of species", pch=19)
lines(seq(1600, 2500, length.out = 900),
      out, col="brown",lwd=2)
abline(h=max(out), col="blue")
abline(v= 1600 + which(out > round(max(out))*0.99)[1], col="blue")
title(paste0(round(max(out)*0.99)," spp in ", (1600 + which(out > round(max(out))*0.99)[1])))

