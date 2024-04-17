# install.packages("red")
library(vegan)
library(dplyr)

#load in new caledonia data
dta = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/NewCaledonia.txt",
              sep="\t", header=TRUE)

new_spp_per_yr = dta %>% count(year)
plot(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), xlab="Year", ylab="Number of Speciees", pch=19, type="o")

# randomise
# nc_shuffled = transform( nc, year = sample(year))
# nc_shuflled = nc
# nc_shuffled$year = sample(1750:2020, nrow(nc), replace=T)
for (i in 1:2000){
  new_spp_per_yr = transform( new_spp_per_yr, n = sample(n))#nc_shuffled %>% count(year)
  lines(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), col="brown")
}
new_spp_per_yr = dta %>% count(year)
points(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), pch=19, type="o")

###################################################
#load in new caledonia data
dta = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/Belgium.txt",
              sep="\t", header=TRUE)

new_spp_per_yr = dta %>% count(year)
plot(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), xlab="Year", ylab="Number of Speciees", pch=19, type="o")

##############################################
dta = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/tdwg_data/NWC.csv",
              sep=",", header=TRUE)
#randomly allocate year to unknown data
dta[is.na(dta$year),"year"] = sample(min(dta$year,na.rm=T):max(dta$year,na.rm=T), length(dta[is.na(dta$year),"year"]), replace=TRUE)
new_spp_per_yr = dta %>% count(year)
plot(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), xlab="Year", ylab="Number of Speciees", pch=19, type="o")

for (i in 1:200){
  new_spp_per_yr = transform( new_spp_per_yr, n = sample(n))#nc_shuffled %>% count(year)
  lines(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), col="brown")
}
new_spp_per_yr = dta %>% count(year)
points(new_spp_per_yr$year, cumsum(new_spp_per_yr$n), pch=19, type="o")




#now reformat in a way that vegan likes
for_vegan = matrix(0,nrow=nrow(new_spp_per_yr), ncol=sum(new_spp_per_yr$n))
for (rowi in 1:nrow(new_spp_per_yr)){
  for_vegan[rowi:nrow(new_spp_per_yr),
            c(1,cumsum(new_spp_per_yr$n))[rowi]:cumsum(new_spp_per_yr$n)[rowi]] = 1#rbinom(1,1,0.8)
}
dune= t(for_vegan)
###########

df <- lapply(ncol(for_vegan),function(i)specaccum(for_vegan[,1:i], method="random"))
plot(df[[1]])
for (i in 2:5) plot(df[[i]],add=T, col=i)


################################################




# Dependences
library("BiodiversityR")
library("vegan")

# Loading data
data(BCI)

# Using vegan package
sp1 <- specaccum(for_vegan , method = "exact")
sp2 <- specaccum(for_vegan, method = "random")

# Using BiodiversityR package
sp3 <- accumresult(for_vegan, method = "exact")
sp4 <- accumresult(for_vegan, method = "random")

# Comparing results using plots
par(mfrow=c(1,2))
plot(sp1, col = "black", lwd = 3, xlab = "Samples", ylab = "Richness",
     main = "exact")
plot(sp3, col = "red", xlab = "Samples", ylab = "Richness", add = TRUE)
plot(sp2, col = "black", lwd = 3, xlab = "Samples", ylab = "Richness",
     main = "random")
plot(sp4, col = "red", xlab = "Samples", ylab = "Richness", add = TRUE)
legend("bottomright", c("vegan","BiodiversityR"), col = c("black","red"), lty = 1)
par(mfrow=c(1,1))


# Estimators
sp1_pool <- poolaccum(for_vegan, permutations = 1000)
plot(sp1_pool)


# Plot manipulating lattice
plot(sp1_pool, display = "chao", col = "black",  auto.key = FALSE,
     grid = F, strip = FALSE, xlab = "Sample",
     par.settings = list(axis.line = list(col = 0)),
     scales = list(col=1, tck=c(1,0)),
     panel = function(...){
       lims <- current.panel.limits()
       panel.xyplot(...)
       panel.abline(h=lims$ylim[1], v=lims$xlim[1])
     })



# Plot manipulating lattice
plot(sp1_pool, display = "S", col = "black",  auto.key = FALSE,
     grid = F, strip = FALSE, xlab = "Sample",
     par.settings = list(axis.line = list(col = 0)),
     scales = list(col=1, tck=c(1,0)),
     panel = function(...){
       lims <- current.panel.limits()
       panel.xyplot(...)
       panel.abline(h=lims$ylim[1], v=lims$xlim[1])
     })


#######################################################
# rarify?

dta = for_vegan#new_spp_per_yr$n
S <- specnumber(dta)
(raremax <- min(rowSums(dta))) #min(dta))#
Srare <- rarefy(dta, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(dta, step = 20, sample = raremax, col = "blue", cex = 0.6)





data(BCI)
S <- specnumber(BCI)
(raremax <- min(rowSums(BCI)))
Srare <- rarefy(BCI, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)

################################################################################

sp1 <- specaccum(for_vegan)
sp2 <- specaccum(for_vegan, "random")
sp2
summary(sp2)
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")
## Fit Lomolino model to the exact accumulation
mod1 <- fitspecaccum(sp1, "lomolino")
coef(mod1)
fitted(mod1)
plot(sp1)
## Add Lomolino model using argument 'add'
plot(mod1, add = TRUE, col=2, lwd=2)
## Fit Arrhenius models to all random accumulations
mods <- fitspecaccum(sp2, "arrh")
plot(mods, col="hotpink")
boxplot(sp2, col = "yellow", border = "blue", lty=1, cex=0.3, add= TRUE)
## Use nls() methods to the list of models
sapply(mods$models, AIC)

###############################################################################
plot(specaccum(for_vegan))
plot(diversity(for_vegan,index = "simpson"))
plot(diversity(for_vegan,index = "shannon"))


spAbund <- rowSums(dune)  #gives the number of individuals found in each plot
spAbund # view observations per plot
raremin <- min(rowSums(dune))  #rarefaction uses the smallest number of observations per sample to extrapolate the expected number if all other samples only had that number of observations
raremin # view smallest # of obs (site 17)
## [1] 15
sRare <- rarefy(dune, raremin) # now use function rarefy
sRare #gives an "expected"rarefied" number of species (not obs) if only 15 individuals were present

rarecurve(dune, col = "blue") # produces rarefaction curves # squares are site numbers positioned at observed space. To "rarefy" a larger site, follow the rarefaction curve until the curve corresponds with the lesser site obs. This gives you rarefied species richness




df <- lapply(c(1,21,41,61,81),function(i)specaccum(BCI[,seq(i,i+19)], method="random"))
plot(df[[1]])
for (i in 2:5) plot(df[[i]],add=T, col=i)
