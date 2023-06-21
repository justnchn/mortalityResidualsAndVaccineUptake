library(dplyr)
library(ggplot2)

testStates <- vaccineUptakeStates[vaccineUptakeStates$Year == 2021,]
testStates <- testStates[trimws(testStates$Month) == "12", ]
sumSeriesComplete <- sum(testStates$Series_Complete_18_to_65)

testCounties <- vaccineUptakeCounties %>%
  mutate(month = format(Date, "%m")) %>%
  mutate(year = format(Date, "%y")) %>%
  group_by(FIPS, month, year) %>%
  filter(Date == max(Date)) %>%
  ungroup()

testCounties <- testCounties[testCounties$year == 21,]
testCounties <- testCounties[trimws(testCounties$month) == "12", ]
sumSeriesCompleteCounties <- sum(testCounties$Series_Complete_18_to_65)

df = resultCirculatory %>%
  mutate(deaths_100k = Deaths/AGE18TO65*100000)
head(df)
ggplot(df, aes(x = Month.Code, y = Series_Complete_18_to_65, group = County)) + geom_line()

df %>% group_by(County, Year) %>% mutate(max_comp = max(Series_Complete_18_to_65)) %>%
  dplyr::select(AGE18TO65, max_comp) %>%
  unique() %>%
  group_by(Year) %>%
  summarize(sum(AGE18TO65)/1e6, sum(max_comp)/1e6)

testShotsAdministered <- read.csv("~/Desktop/Research/COVID-19_Vaccinations_in_the_United_States_County.csv") %>%
  subset(select = c("Date", "Recip_State", "Administered_Dose1_Recip", "Series_Complete_Yes", "Booster_Doses"))
testShotsAdministered[is.na(testShotsAdministered)] <- 0
testShotsAdministered$Administered <- testShotsAdministered$Administered_Dose1_Recip + testShotsAdministered$Series_Complete_Yes + testShotsAdministered$Booster_Doses
testShotsAdministered <- testShotsAdministered[testShotsAdministered$Date == "03/15/2023", ]

# checking allCause death totals
allCauseNonResidualDeaths <- sum(allCauseAllYearsFiltered[allCauseAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
allCauseResidualDeaths <- sum(allCauseCountyResiduals[allCauseCountyResiduals$Month.Code == "2021/12", ]$Deaths)
allCauseDeathTotal <- sum(allCauseStates[allCauseStates$Month.Code == "2021/12", ]$Deaths)
allCauseResultDeathTotal <- sum(result[result$Month.Code == "2021/12", ]$Deaths)

# checking circulatory death totals
circulatoryNonResidualDeaths <- sum(circulatoryAllYearsFiltered[circulatoryAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
circulatoryResidualDeaths <- sum(circulatoryResiduals[circulatoryResiduals$Month.Code == "2021/12", ]$Deaths)
circulatoryDeathTotal <- sum(circulatoryCountyWithResiduals[circulatoryCountyWithResiduals$Month.Code == "2021/12", ]$Deaths)
circulatoryResultDeathTotal <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Deaths)

# checking vaccineUptake values in result as compared to vaccineUptakeStates and original vaccineUptakeCounties
stateAdministeredDose1 <- sum(vaccineUptakeStates[vaccineUptakeStates$Date == "12/31/2021", ]$Administered_Dose1_18_to_65)
stateSeriesComplete <- sum(vaccineUptakeStates[vaccineUptakeStates$Date == "12/31/2021", ]$Series_Complete_18_to_65)
unfilteredCountiesAdministeredDose1 <- sum(vaccineUptakeCounties[vaccineUptakeCounties$Date == "12/31/2021", ]$Administered_Dose1_18_to_65)
unfilteredCountiesSeriesComplete <- sum(vaccineUptakeCounties[vaccineUptakeCounties$Date == "12/31/2021", ]$Series_Complete_18_to_65)

# checking values for allCause and circulatory vaccineUptake
countyAllCauseAdministeredDose1 <- sum(result[result$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
countyAllCauseSeriesComplete <- sum(result[result$Month.Code == "2021/12", ]$Series_Complete_18_to_65)
countyCirculatoryAdministeredDose1 <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
countyCirculatorySeriesComplete <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Series_Complete_18_to_65)
countyAllCausePreMergeAdministered <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
countyAllCausePreMergeSeriesComplete <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
countyCirculatoryPreMergeAdministered <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
countyCirculatoryPreMergeSeriesComplete <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)

# checking population values for allCause and circulatory result
countyAllCauseNonResidualPopulation <- sum(countyPopulationAllCauseNonResidual[countyPopulationAllCauseNonResidual$YEAR == "2021", ]$AGE18TO65)
countyAllCauseResidualPopulation <- sum(countyPopulationAllCauseResiduals[countyPopulationAllCauseResiduals$YEAR == "2021", ]$AGE18TO65)
allCauseResultPopulaton <- sum(result[result$Month.Code == "2021/12", ]$AGE18TO65)
countyCirculatoryNonResidualPopulation <- sum(countyPopulationCirculatoryNonResidual[countyPopulationCirculatoryNonResidual$YEAR == "2021", ]$AGE18TO65)
countyCirculatoryResidualPopulation <- sum(countyPopulationCirculatoryResiduals[countyPopulationCirculatoryResiduals$YEAR == "2021", ]$AGE18TO65)
circulatoryResultPopulaton <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$AGE18TO65)

# checking covid death and cases values for allCause result
allCauseNonResidualCovidDeaths <- sum(covidDeathAllYearsAllCauseNonResidual[covidDeathAllYearsAllCauseNonResidual$Month.Code == "2021/12", ]$deaths)
allCauseNonResidualCovidCases <- sum(covidDeathAllYearsAllCauseNonResidual[covidDeathAllYearsAllCauseNonResidual$Month.Code == "2021/12", ]$cases)
allCauseResidualCovidDeaths <- sum(covidDeathAllCauseResiduals[covidDeathAllCauseResiduals$Month.Code == "2021/12", ]$covid_deaths)
allCauseResidualCovidCases <- sum(covidDeathAllCauseResiduals[covidDeathAllCauseResiduals$Month.Code == "2021/12", ]$covid_cases)
allCauseTotalCovidDeaths <- sum(covidDeathAllYearsAllCauseWithResidual[covidDeathAllYearsAllCauseWithResidual$Month.Code == "2021/12", ]$covid_deaths)
allCauseTotalCovidCases <- sum(covidDeathAllYearsAllCauseWithResidual[covidDeathAllYearsAllCauseWithResidual$Month.Code == "2021/12", ]$covid_cases)
resultTotalCovidDeaths <- sum(resultWithCovidDeaths[resultWithCovidDeaths$Month.Code == "2021/12", ]$covid_deaths)
resultTotalCovidCases <- sum(resultWithCovidDeaths[resultWithCovidDeaths$Month.Code == "2021/12", ]$covid_cases)

# checking covid death and cases values for circulatory result
circulatoryNonResidualCovidDeaths <- sum(covidDeathAllYearsCirculatoryNonResidual[covidDeathAllYearsCirculatoryNonResidual$Month.Code == "2021/12", ]$deaths)
circulatoryNonResidualCovidCases <- sum(covidDeathAllYearsCirculatoryNonResidual[covidDeathAllYearsCirculatoryNonResidual$Month.Code == "2021/12", ]$cases)
circulatoryResidualCovidDeaths <- sum(covidDeathCirculatoryResiduals[covidDeathCirculatoryResiduals$Month.Code == "2021/12", ]$covid_deaths)
circulatoryResidualCovidCases <- sum(covidDeathCirculatoryResiduals[covidDeathCirculatoryResiduals$Month.Code == "2021/12", ]$covid_cases)
circulatoryTotalCovidDeaths <- sum(covidDeathAllYearsCirculatoryWithResidual[covidDeathAllYearsCirculatoryWithResidual$Month.Code == "2021/12", ]$covid_deaths)
circulatoryTotalCovidCases <- sum(covidDeathAllYearsCirculatoryWithResidual[covidDeathAllYearsCirculatoryWithResidual$Month.Code == "2021/12", ]$covid_cases)
circulatoryResultTotalCovidDeaths <- sum(resultCirculatoryWithCovidDeaths[resultCirculatoryWithCovidDeaths$Month.Code == "2021/12", ]$covid_deaths)
circulatoryResultTotalCovidCases <- sum(resultCirculatoryWithCovidDeaths[resultCirculatoryWithCovidDeaths$Month.Code == "2021/12", ]$covid_cases)
