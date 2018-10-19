##########################################################################
##  Footprint Calculus with diagonalized Direct Intensity
##########################################################################

rm(list=ls())
datapath = "W:/WU/Projekte/SRU-Projekte/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.4/parsed/"
library(reshape2)
library(data.table)

nrreg = 49    # number of regions
nrsec = 200   # number of IO sectors
nrfd = 7      # number of final demand categories
years = c(2015)   # years considered in the analysis

load(paste0(datapath,"Q.codes.RData"))
load(paste0(datapath,"Y.codes.RData"))
load(paste0(datapath,"pxp/IO.codes.RData"))
Product.codes <- read.csv("Product_Codes.csv")

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}


##########################################################################
# Calculate Footprint
##########################################################################
year = 2015
for(year in years){
  cat(paste0("\nyear ",year,": "))
  load(file=paste0(datapath,"pxp/",year,"_L.RData"))
  load(file=paste0(datapath,"pxp/",year,"_Y.RData"))
  load(file=paste0(datapath,"pxp/",year,"_x.RData"))
  load(file=paste0(datapath,"pxp/",year,"_Extension.RData"))
  
  # prepare extensions
  extension <- as.data.frame(E[Q.codes$Compartment=="Land",])
  extension.codes <- Q.codes[Q.codes$Compartment=="Land",]
  extension[1,] <- colSums(extension[1:13,]) # Cropland
  extension[2,] <- colSums(extension[16:18,]) # Pasture
  extension[3,] <- colSums(extension[14,]) # Forestry
  extension <- extension[1:3,]
  extension[4,] <- colSums(E[Q.codes$Compartment=="Blue.consumption",])
  extension[5,] <- colSums(E[Q.codes$Compartment=="Emissions",] * Q.codes[Q.codes$Compartment=="Emissions","GWP"])
  
  extension <- t(extension) / x
  # delete Nan and Inf values resulting from division by 0
  extension[extension=="NaN"] <- 0
  extension[extension=="Inf"] <- 0
  extension <- as.matrix(extension)
  
  # prepare final demand data
  colnames(Y) <- Y.Codes$`Region Name`
  Y <- agg(Y)
  
  try(data.table::fwrite(list("Source_country","Product","Agriculture","Non-Food","Food products","Hotels and restaurants",
                              "Education and health","Food waste","Country","Indicator"), 
                         file=paste0("./output/Nordic_Footprints_Detail_",year,".csv"), row.names = FALSE, append = FALSE))
  
  regions <- c("DK","FI","SE","NO")
  # region = "FI"
  for(region in regions){
    cat(paste0(region,", "))
    # ee = 3
    for(ee in 1:ncol(extension)){
      # Calculate Multiplier Matrix
      MP <- L * extension[,ee]
      
      # Calculate Footprint
      FP <- as.matrix(t(t(MP) * Y[,region]))
      
      # Aggregate final demand products
      colnames(FP) <- rep(Product.codes$Food, nrreg)
      FP <- agg(FP)
      FP <- cbind(data.frame(Source_country = IO.codes$Country.Code, Product = IO.codes$Product.Name), as.data.frame(FP))
      FP <- FP[rowSums(FP[,-(1:2)]) > 0, ]
      
      # Country, Environmental indicator, Footprint
      FP$Country <- region
      FP$Indicator <- c("Cropland","Grassland","Forest","Blue Water Consumption","Global Warming Potential")[ee]
      
      # write results
      try(data.table::fwrite(FP, file=paste0("./output/Nordic_Footprints_Detail_",year,".csv"), row.names = FALSE, append = TRUE))
      
    }
  }
}
