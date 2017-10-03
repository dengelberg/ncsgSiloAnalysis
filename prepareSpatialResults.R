##This function processess reusltFileSpatial for a single year

prepareResults <- function(year, prestoScn, sensitivity, value){
  
#Setting directory for inputs and outputs
  
setwd("C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_Full/ScenariosReuslts_Raw/")
destination <- "C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_Full/R_Analysis/"

## Original Version Preset Parameters
#year <- 2040
#prestoScn <- "lco"
#sensitivity <- "AOC"
#value <- "1.25"

#Continue to establish destination directory

destination <- paste(destination, sensitivity, sep = "")
if(!dir.exists(destination)){
  dir.create(file.path(destination))
}
destination <- paste(destination, "/SILO_", prestoScn, "_", sensitivity, "_", value, sep = "")
if(!dir.exists(destination)){
  dir.create(file.path(destination))
}


#Reading resultFileSpatial and regions sheet

setwd(paste("./", sensitivity, "_", value, sep = ""))
setwd(paste("./SILO_", prestoScn, "_", sensitivity, "_", value, sep = ""))
spatialInput <- paste("./resultFileSpatial_", as.character(year), ".csv", sep = "")
spatial <- read.csv(spatialInput)
regions <- read.csv("C:/Users/dengelbe/Documents/R/SILO_Analysis/regions cheat sheet.csv")

#Intermidary steps to calclate total households by race and averageHousingPrice

spatial$year <- year
spatial$white <- spatial$shWhite * spatial$households
spatial$black <- spatial$shBlack * spatial$households
spatial$hispanic <- spatial$shHispanic * spatial$households
spatial$other <- spatial$shOther * spatial$households

spatial$housingTotal <- spatial$avePrice * spatial$households

#Adding regional classification to each SMZ

for(i in 1:nrow(spatial)){
        for(j in 1:nrow(regions)){
                if(regions$Start.SMZ[j] <= spatial$Year[i] && spatial$Year[i] <= regions$End.SMZ[j]){
                        spatial$state[i] <- as.character(regions$State[j])
                        spatial$county[i] <- as.character(regions$County.Name[j])
                        spatial$region[i] <- as.character(regions$Region[j])
                        spatial$subregion[i] <- as.character(regions$Subregion[j])
                }
        }
}

#Creating state, county, region, and subregion tables

sTable(year, spatial, destination)
cTable(year, spatial, destination, regions)
rTable(year, spatial, destination)
srTable(year, spatial, destination)
#PFAIntersect(year, spatial, destination)
#spatial
}


##This function performs the same tasks as the previous function but is use for results when SILO is run alone without integrated model
##In the future the two functions should be combined

prepareResultsSiloSen <- function(year, sensitivity, value){
  
  setwd("C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_SILO/normalized_runs/")
  destination <- "C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_SILO/normalized_runs/"
  
  ## Original Version Preset Parameters
  #year <- 2040
  #prestoScn <- "lco"
  #sensitivity <- "AOC"
  #value <- "1.25"
  #destination <- paste(destination, sensitivity, sep = "")

  destination <- paste(destination, "SILO_", sensitivity, "_", value, sep = "")
  if(!dir.exists(destination)){
    dir.create(file.path(destination))
  }
  
  setwd(paste("./silo_", sensitivity, "_", value, sep = ""))
  spatialInput <- paste("./resultFileSpatial_", as.character(year), ".csv", sep = "")
  spatial <- read.csv(spatialInput)
  
  
  
  regions <- read.csv("C:/Users/dengelbe/Documents/R/SILO_Analysis/regions cheat sheet.csv")
  spatial$year <- year
  spatial$white <- spatial$shWhite * spatial$households
  spatial$black <- spatial$shBlack * spatial$households
  spatial$hispanic <- spatial$shHispanic * spatial$households
  spatial$other <- spatial$shOther * spatial$households
  
  spatial$housingTotal <- spatial$avePrice * spatial$households
  
  for(i in 1:nrow(spatial)){
    for(j in 1:nrow(regions)){
      if(regions$Start.SMZ[j] <= spatial$Year[i] && spatial$Year[i] <= regions$End.SMZ[j]){
        spatial$state[i] <- as.character(regions$State[j])
        spatial$county[i] <- as.character(regions$County.Name[j])
        spatial$region[i] <- as.character(regions$Region[j])
        spatial$subregion[i] <- as.character(regions$Subregion[j])
      }
    }
  }
  
  sTable(year, spatial, destination)
  cTable(year, spatial, destination, regions)
  rTable(year, spatial, destination)
  srTable(year, spatial, destination)
  #PFAIntersect(year, spatial, destination)
  #spatial
}


##Unfinished function to calculate the total population inside and outside priority funding areas

PFAIntersect <- function(year, spatial, destination){
  pfa_Intersect <- read.csv("C:/Users/dengelbe/Documents/NCSG/PRESTO/Sensitivity_Full/PFA_Intersect.csv")
  for(i in 1:nrow(spatial)){
    for(j in 1:nrow(pfa_Intersect)){
      if(spatial[i,1] == pfa_Intersect$SMZ[j]){
        spatial$pfaShare <- pfa_Intersect$PERCENTAGE[j]
        spatial$hhInPFA <- round(spatial$pfaShare * spatial$households, digits = 0)
        spatial$hhOutPFA <- round((1 - spatial$pfaShare) * spatial$households, digits = 0)
      }
    }
  }
  spatial
}


        