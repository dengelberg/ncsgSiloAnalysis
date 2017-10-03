##This function summarizes resultsFileSpatial to the region level

rTable <- function(year, spatial, destination){

#Creates table to populate  
  
regionTable <- data.frame(unique(spatial$region))
regionTable <- data.frame(sort(regionTable$unique.spatial.region.))
names(regionTable) <- "Region"

#Summarize accessibilities

regionTable$autoAccessibility <- tapply(spatial$autoAccessibility, spatial$region, mean)
regionTable$transitAccessibility <- tapply(spatial$transitAccessibility, spatial$region, mean)

#Summarize pop, emp, hh

regionTable$population <- tapply(spatial$population, spatial$region, sum)
regionTable$employment <- tapply(spatial$jobs, spatial$region, sum)
regionTable$households <- tapply(spatial$households, spatial$region, sum)

#Summarize hh incomes

regionTable$hh0_20 <- tapply(spatial$hhInc_.20000, spatial$region, sum)
regionTable$hh20_40 <- tapply(spatial$hhInc_.20000.1, spatial$region, sum)
regionTable$hh40_60 <- tapply(spatial$hhInc_.40000, spatial$region, sum)
regionTable$hh60 <- tapply(spatial$hhInc_.60000, spatial$region, sum)

#Summarize dweling types

regionTable$dd_SFD <- tapply(spatial$dd_SFD, spatial$region, sum)
regionTable$dd_SFA <- tapply(spatial$dd_SFA, spatial$region, sum)
regionTable$dd_MF234 <- tapply(spatial$dd_MF234, spatial$region, sum)
regionTable$dd_MF5plus <- tapply(spatial$dd_MF5plus, spatial$region, sum)
regionTable$dd_MH <- tapply(spatial$dd_MH, spatial$region, sum)

#Average dwelling price

totalPrice <- tapply(spatial$housingTotal, spatial$region, sum)
regionTable$averagePrice <- totalPrice/regionTable$households

#summarizes county land available

regionTable$availLand <- tapply(spatial$availLand, spatial$region, sum)

#Summarize hh by race

regionTable$white <- tapply(spatial$white, spatial$region, sum)
regionTable$black <- tapply(spatial$black, spatial$region, sum)
regionTable$hispanic <- tapply(spatial$hispanic, spatial$region, sum)
regionTable$other <- tapply(spatial$other, spatial$region, sum)

#writes rile to output directory

write.csv(regionTable, file = paste(destination, "/SILO_Region_Summary_", year, ".csv", sep = ""))
}