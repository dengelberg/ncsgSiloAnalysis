##This function summarizes resultsFileSpatial to the subregion level

srTable <- function(year, spatial, destination){
  
#Creates table to populate  
  
subregionTable <- data.frame(unique(spatial$subregion))
subregionTable <- data.frame(sort(subregionTable$unique.spatial.subregion.))
names(subregionTable) <- "Subregion"

#Summarize accessibilities

subregionTable$autoAccessibility <- tapply(spatial$autoAccessibility, spatial$subregion, mean)
subregionTable$transitAccessibility <- tapply(spatial$transitAccessibility, spatial$subregion, mean)

#Summarize pop, emp, hh

subregionTable$population <- tapply(spatial$population, spatial$subregion, sum)
subregionTable$employment <- tapply(spatial$jobs, spatial$subregion, sum)
subregionTable$households <- tapply(spatial$households, spatial$subregion, sum)

#Summarize hh incomes

subregionTable$hh0_20 <- tapply(spatial$hhInc_.20000, spatial$subregion, sum)
subregionTable$hh20_40 <- tapply(spatial$hhInc_.20000.1, spatial$subregion, sum)
subregionTable$hh40_60 <- tapply(spatial$hhInc_.40000, spatial$subregion, sum)
subregionTable$hh60 <- tapply(spatial$hhInc_.60000, spatial$subregion, sum)

#Summarize dweling types

subregionTable$dd_SFD <- tapply(spatial$dd_SFD, spatial$subregion, sum)
subregionTable$dd_SFA <- tapply(spatial$dd_SFA, spatial$subregion, sum)
subregionTable$dd_MF234 <- tapply(spatial$dd_MF234, spatial$subregion, sum)
subregionTable$dd_MF5plus <- tapply(spatial$dd_MF5plus, spatial$subregion, sum)
subregionTable$dd_MH <- tapply(spatial$dd_MH, spatial$subregion, sum)

#Average dwelling price

totalPrice <- tapply(spatial$housingTotal, spatial$subregion, sum)
subregionTable$averagePrice <- totalPrice/subregionTable$households

#summarizes county land available

subregionTable$availLand <- tapply(spatial$availLand, spatial$subregion, sum)

#Summarize hh by race

subregionTable$white <- tapply(spatial$white, spatial$subregion, sum)
subregionTable$black <- tapply(spatial$black, spatial$subregion, sum)
subregionTable$hispanic <- tapply(spatial$hispanic, spatial$subregion, sum)
subregionTable$other <- tapply(spatial$other, spatial$subregion, sum)

#writes rile to output directory

write.csv(subregionTable, file = paste(destination, "/SILO_SubRegion_Summary_", year, ".csv", sep = ""))
}