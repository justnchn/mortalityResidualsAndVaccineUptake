allCauseMortality2223 <- read.delim("~/Desktop/Research/Provisional Mortality Statistics, 2018 through Last Month.txt") %>%
  select(-c("Notes", "Residence.State.Code", "Year", "Month", "Population", "Crude.Rate")) %>%
  rename("State" = "Residence.State", "Year" = "Year.Code")
allCauseMortality2223 <- allCauseMortality2223[complete.cases(allCauseMortality2223), ] %>%
  mutate(Month = substr(Month.Code, 6, 7)) %>%
  left_join(vaccineUptakeStates, by = c("State", "Year", "Month"))
allCauseMortality2223 <- allCauseMortality2223[complete.cases(allCauseMortality2223), ]

circulatoryMortality2223 <- read.delim("~/Desktop/Research/Provisional Mortality Statistics, 2018 through Last Month Circulatory.txt") %>%
  select(-c("Notes", "Residence.State.Code", "Year", "Month", "Population", "Crude.Rate")) %>%
  rename("State" = "Residence.State", "Year" = "Year.Code")
circulatoryMortality2223 <- circulatoryMortality2223[complete.cases(circulatoryMortality2223), ] %>%
  mutate(Month = substr(Month.Code, 6, 7)) %>%
  left_join(vaccineUptakeStates, by = c("State", "Year", "Month"))
circulatoryMortality2223 <- circulatoryMortality2223[complete.cases(circulatoryMortality2223), ]

covidDeath2223 <- read.csv("~/Desktop/Research/Excess_Deaths_Associated_with_COVID-19.csv") %>%
  subset(select = c("Week.Ending.Date", "State", "Observed.Number", "Year", "Outcome"))

totalDeath <- covidDeath2223[covidDeath2223$Outcome == "All causes", ]
nonCovidDeath <- covidDeath2223[covidDeath2223$Outcome != "All causes", ]

covidDeathCalculated <- left_join(totalDeath, nonCovidDeath, by = c("Week.Ending.Date", "State", "Year"))
covidDeathCalculated$Death <- covidDeathCalculated$Observed.Number.x - covidDeathCalculated$Observed.Number.y
covidDeathCalculated <- subset(covidDeathCalculated, select = c("Week.Ending.Date", "State", "Year", "Death"))

population2022 <- readxl::read_excel("~/Desktop/Research/population2022.xlsx")
population2022$State <- sub("^\\.", "", population2022$State)

allCauseMortality2223WithPopulation <- left_join(allCauseMortality2223, population2022, by = c("State", "Year"))
circulatoryMortality2223WithPopulation <- left_join(circulatoryMortality2223, population2022, by = c("State", "Year"))

allCauseMortalityWithCovidDeaths <- left_join(allCauseMortality2223WithPopulation, covidDeathStateTotals, by = c("Month.Code", "State" = "state"))
circulatoryMortalityWithCovidDeaths <- left_join(circulatoryMortality2223WithPopulation, covidDeathStateTotals, by = c("Month.Code", "State" = "state"))

write.csv(allCauseMortalityWithCovidDeaths, "~/Desktop/Research/stateAllCauseMortality2023.csv")
write.csv(circulatoryMortalityWithCovidDeaths, "~/Desktop/Research/stateCirculatoryMortality2023.csv")
