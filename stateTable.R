##This function summarizes resultsFileSpatial to the state level

sTable <- function(year, spatial, destination){
  
#Creates table to populate  
  
stateTable <- data.frame(unique(spatial$state))
stateTable <- data.frame(sort(stateTable$unique.spatial.state.))
names(stateTable) <- "State"

#Summarize accessibilities

stateTable$autoAccessibility <- tapply(spatial$autoAccessibility, spatial$state, mean)
stateTable$transitAccessibility <- tapply(spatial$transitAccessibility, spatial$state, mean)

#Summarize pop, emp, hh

stateTable$population <- tapply(spatial$population, spatial$state, sum)
stateTable$employment <- tapply(spatial$jobs, spatial$state, sum)
stateTable$households <- tapply(spatial$households, spatial$state, sum)

#Summarize hh incomes

stateTable$hh0_20 <- tapply(spatial$hhInc_.20000, spatial$state, sum)
stateTable$hh20_40 <- tapply(spatial$hhInc_.20000.1, spatial$state, sum)
stateTable$hh40_60 <- tapply(spatial$hhInc_.40000, spatial$state, sum)
stateTable$hh60 <- tapply(spatial$hhInc_.60000, spatial$state, sum)

#Summarize dweling types

stateTable$dd_SFD <- tapply(spatial$dd_SFD, spatial$state, sum)
stateTable$dd_SFA <- tapply(spatial$dd_SFA, spatial$state, sum)
stateTable$dd_MF234 <- tapply(spatial$dd_MF234, spatial$state, sum)
stateTable$dd_MF5plus <- tapply(spatial$dd_MF5plus, spatial$state, sum)
stateTable$dd_MH <- tapply(spatial$dd_MH, spatial$state, sum)

#Average dwelling price

totalPrice <- tapply(spatial$housingTotal, spatial$state, sum)
stateTable$averagePrice <- totalPrice/stateTable$households

#summarizes county land available

stateTable$availLand <- tapply(spatial$availLand, spatial$state, sum)

#Summarize hh by race

stateTable$white <- tapply(spatial$white, spatial$state, sum)
stateTable$black <- tapply(spatial$black, spatial$state, sum)
stateTable$hispanic <- tapply(spatial$hispanic, spatial$state, sum)
stateTable$other <- tapply(spatial$other, spatial$state, sum)

#writes rile to output directory

write.csv(stateTable, file = paste(destination, "/SILO_State_Summary_", year, ".csv", sep = ""))
}