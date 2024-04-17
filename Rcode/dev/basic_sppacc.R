# install.packages("red")
library(red)
library(vegan)

wcvp_acc_names = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/WCVP_acc_names_dateBasio_RS.txt", sep=" ", header=TRUE)
wcvp_dist = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/wcvp_distribution.txt", sep="|", header=TRUE)
gbif_eg = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/sampOcc_1945_2020/sampOcc_1945.txt", sep="\t", header=TRUE)

plot(0, 0, xlim=c(0,10000), ylim=c(0,2000), xlab= "AOO", ylab = "Number of occurences")
for(i in 1:length(unique(gbif_eg["species"])$species)){
  spp = unique(gbif_eg["species"])$species[i]
  #eooo = red::eoo(gbif_eg[gbif_eg["species"] == spp, c("decimalLongitude","decimalLatitude")])
  aooo = red::aoo(gbif_eg[gbif_eg["species"] == spp, c("decimalLongitude","decimalLatitude")])
  points(aooo,
         length(gbif_eg["species"][gbif_eg["species"] == spp]), pch=19)

}



plot(0, 0, xlim=c(0,1000000), ylim=c(0,500), xlab= "AOO", ylab = "Number of occurences")
for(i in 1:length(unique(gbif_eg["species"])$species)){
  spp = unique(gbif_eg["species"])$species[i]
  eooo = red::eoo(gbif_eg[gbif_eg["species"] == spp, c("decimalLongitude","decimalLatitude")])
  #aooo = red::aoo(gbif_eg[gbif_eg["species"] == spp, c("decimalLongitude","decimalLatitude")])
  points(eooo,
         length(gbif_eg["species"][gbif_eg["species"] == spp]), pch=19)

}

# species accumulation curve
gbif_eg = read.csv("C:/Users/kdh10kg/Documents/github/darkspots/data/sampOcc_1945_2020/sampOcc_1945.txt", sep="\t", header=TRUE)
spp_list = unique(gbif_eg["species"])$species
yearly_spp_cumsum = 0
yearly_spp_cumsum = c(yearly_spp_cumsum, length(spp_list))
# plot(1945,length(spp_list), xlim=c(1945,2020), ylim=c(0,1000), xlab= "year",ylab="# species" , pch=19)
plot(gbif_eg$decimalLongitude,gbif_eg$decimalLatitude, pch="+")
for (year in 1946:2020){
  gbif_eg = read.csv(paste0("C:/Users/kdh10kg/Documents/github/darkspots/data/sampOcc_1945_2020/sampOcc_",year,".txt"), sep="\t", header=TRUE)
  new_spp_list = unique(gbif_eg["species"])$species
  spp_list = unique(c(spp_list, new_spp_list))
  yearly_spp_cumsum = c(yearly_spp_cumsum, length(spp_list))
  # points(year,length(spp_list), pch=19)
  points(gbif_eg$decimalLongitude,gbif_eg$decimalLatitude, pch="+")
}
plot(1944:2020, yearly_spp_cumsum,
     xlim=c(1944,2020), ylim=c(0,1000),
     xlab= "year",ylab="# species", pch=19, type="o")



# determine slope?
flattest_pts = which(abs(diff(yearly_spp_cumsum))==min(abs(diff(yearly_spp_cumsum))) )  [1]
points(1944+flattest_pts, yearly_spp_cumsum[flattest_pts],
       pch=19, col="red")





data(BCI)
mod <- c("arrhenius", "gleason", "gitay", "lomolino", "asymp", "gompertz",
         "michaelis-menten", "logis", "weibull")
extraps <- matrix(NA, 100, length(mod))
colnames(extraps) <- mod
for(i in 1:nrow(extraps)) {
  ## use the same accumulation for all nls models
  m <- specaccum(BCI[sample(50,25),], "exact")
  for(p in mod) {
    ## need try because some nls models can fail
    tmp <-  try(predict(fitspecaccum(m, p), newdata=50))
    if(!inherits(tmp, "try-error")) extraps[i,p] <- tmp
  }
}
