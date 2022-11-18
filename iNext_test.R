 # install.packages("iNEXT")
## import packages
library(iNEXT)
library(ggplot2)
library(dplyr)
# library(vegan)
# library(BiodiversityR)



filename="CLM.csv"
filename="NWC.csv"
##############################################
dta = read.csv(paste0("C:/Users/kdh10kg/Documents/github/darkspots/data/tdwg_data/",filename),
               sep=",", header=TRUE)
#randomly allocate year to unknown data
dta[is.na(dta$year),"year"] = sample(min(dta$year,na.rm=T):max(dta$year,na.rm=T), length(dta[is.na(dta$year),"year"]), replace=TRUE)
new_spp_per_yr = dta %>% count(year)

plot(new_spp_per_yr$year, new_spp_per_yr$n, xlab="Year", ylab="Number of Species Discovered", pch=19, type="o")
plot(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), xlab="Year", ylab="Cumulative Number of Species", pch=19, type="o")

for (i in 1:200){
  new_spp_per_yr = transform( new_spp_per_yr, n = sample(n))#nc_shuffled %>% count(year)
  lines(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), col="brown")
}
new_spp_per_yr = dta %>% count(year)
points(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), pch=19, type="o")


x = new_spp_per_yr$year
y = cumsum(new_spp_per_yr$n)
plot(y ~ x, xlim= c(min(x),2500), ylim=c(0,20000))
fit <- nls(y ~ SSlogis(x, Asym, xmid, scal), data = data.frame(x, y))

summary(fit)
lines(seq(1700, 2500, length.out = 800),
      predict(fit, newdata = data.frame(x = seq(1700, 2500, length.out = 800))), col="brown")



######################################################
#now reformat in a way that vegan likes
for_vegan = matrix(0,nrow=nrow(new_spp_per_yr), ncol=sum(new_spp_per_yr$n))
for (rowi in 1:(nrow(new_spp_per_yr)-1)){
  for_vegan[rowi,#:nrow(new_spp_per_yr),
            c(1,cumsum(new_spp_per_yr$n))[rowi]:cumsum(new_spp_per_yr$n)[rowi]] = 1
  # for_vegan[(rowi+1):nrow(new_spp_per_yr),
  #           c(1,cumsum(new_spp_per_yr$n))[rowi]:cumsum(new_spp_per_yr$n)[rowi]] = rbinom(1,1,0.01)
}
dune= t(for_vegan)
##############################################

# data(ant)
# str(ant)
#
for_iNext = as.incfreq(for_vegan)
# for_iNext[1] = (length(for_iNext)-1)
# for_iNext = list()
# for_iNext$CLM = c(length(as.numeric(new_spp_per_yr$n)),as.numeric(cumsum(new_spp_per_yr$n)))

t <- seq(1, 7000, by=10)
out.inc <- iNEXT(for_iNext, q=0, datatype="incidence_freq", size=t)

# Sample‐size‐based R/E curves
ggiNEXT(out.inc, type=1, color.var="site") +
  theme_bw(base_size = 18) +
  theme(legend.position="none")
