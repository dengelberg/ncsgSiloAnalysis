##This function summarizes resultsFileSpatial to the county level

cTable <- function(year, spatial, destination, regions){

#Creates table to populate  
  
countyTable <- data.frame(unique(spatial$county))
countyTable <- data.frame(sort(countyTable$unique.spatial.county.))
names(countyTable) <- "County"

#Summarize accessibilities

countyTable$autoAccessibility <- tapply(spatial$autoAccessibility, spatial$county, mean)
countyTable$transitAccessibility <- tapply(spatial$transitAccessibility, spatial$county, mean)

#Summarize pop, emp, hh

countyTable$population <- tapply(spatial$population, spatial$county, sum)
countyTable$employment <- tapply(spatial$jobs, spatial$county, sum)
countyTable$households <- tapply(spatial$households, spatial$county, sum)

#Summarize hh incomes

countyTable$hh0_20 <- tapply(spatial$hhInc_.20000, spatial$county, sum)
countyTable$hh20_40 <- tapply(spatial$hhInc_.20000.1, spatial$county, sum)
countyTable$hh40_60 <- tapply(spatial$hhInc_.40000, spatial$county, sum)
countyTable$hh60 <- tapply(spatial$hhInc_.60000, spatial$county, sum)

#Summarize dweling types

countyTable$dd_SFD <- tapply(spatial$dd_SFD, spatial$county, sum)
countyTable$dd_SFA <- tapply(spatial$dd_SFA, spatial$county, sum)
countyTable$dd_MF234 <- tapply(spatial$dd_MF234, spatial$county, sum)
countyTable$dd_MF5plus <- tapply(spatial$dd_MF5plus, spatial$county, sum)
countyTable$dd_MH <- tapply(spatial$dd_MH, spatial$county, sum)

#Average dwelling price

totalPrice <- tapply(spatial$housingTotal, spatial$county, sum)
countyTable$averagePrice <- totalPrice/countyTable$households

#summarizes county land available

countyTable$availLand <- tapply(spatial$availLand, spatial$county, sum)

#Summarize hh by race

countyTable$white <- tapply(spatial$white, spatial$county, sum)
countyTable$black <- tapply(spatial$black, spatial$county, sum)
countyTable$hispanic <- tapply(spatial$hispanic, spatial$county, sum)
countyTable$other <- tapply(spatial$other, spatial$county, sum)

#Addens stea, region, and subregion of county

for(i in 1:nrow(countyTable)){
        for(j in 1:nrow(regions)){
                if(regions$County.Name[j] == countyTable$County[i]){
                        countyTable$state[i] <- as.character(regions$State[j])
                        countyTable$region[i] <- as.character(regions$Region[j])
                        countyTable$subregion[i] <- as.character(regions$Subregion[j])
                }
        }
}

#writes rile to output directory

write.csv(countyTable, file = paste(destination, "/SILO_County_Summary_", year, ".csv", sep = ""))
}