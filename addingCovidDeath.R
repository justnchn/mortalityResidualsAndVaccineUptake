# checking allCause death totals
allCauseNonResidualDeaths <- sum(allCauseAllYearsFiltered[allCauseAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
allCauseResidualDeaths <- sum(allCauseCountyResiduals[allCauseCountyResiduals$Month.Code == "2021/12", ]$Deaths)
allCauseDeathTotal <- sum(allCauseStates[allCauseStates$Month.Code == "2021/12", ]$Deaths)
allCauseResultDeathTotal <- sum(result[result$Month.Code == "2021/12", ]$Deaths)

# checking circulatory death totals 
circulatoryNonResidualDeaths <- sum(circulatoryAllYearsFiltered[circulatoryAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
circulatoryResidualDeaths <- sum(circulatoryCountyResiduals[circulatoryCountyResiduals$Month.Code == "2021/12", ]$Deaths)
circulatoryDeathTotal <- sum(circulatoryStates[circulatoryStates$Month.Code == "2021/12", ]$Deaths)
circulatoryResultDeathTotal <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Deaths)

# checking allCause vaccine uptake
allCauseNonResidualAdminsteredDose1 <- sum(allCauseNonResidual[allCauseNonResidual$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
allCauseNonResidualSeriesComplete <- sum(allCauseNonResidual[allCauseNonResidual$Date == "2021-12-31", ]$Series_Complete_18_to_65)
allCauseResidualAdminsteredDose1 <- sum(allCauseUptakeResiduals[allCauseUptakeResiduals$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
allCauseResidualSeriesComplete <- sum(allCauseUptakeResiduals[allCauseUptakeResiduals$Date == "2021-12-31", ]$Series_Complete_18_to_65)
allCauseAdminsteredDose1Total <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
allCauseSeriesCompleteTotal <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
resultAdministeredDose1 <- sum(result[result$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
resultSeriesComplete <- sum(result[result$Month.Code == "2021/12", ]$Series_Complete_18_to_65)

# checking circulatory vaccine uptake
circulatoryNonResidualAdminsteredDose1 <- sum(circulatoryNonResidual[circulatoryNonResidual$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatoryNonResidualSeriesComplete <- sum(circulatoryNonResidual[circulatoryNonResidual$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatoryResidualAdminsteredDose1 <- sum(circulatoryUptakeResiduals[circulatoryUptakeResiduals$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatoryResidualSeriesComplete <- sum(circulatoryUptakeResiduals[circulatoryUptakeResiduals$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatoryAdminsteredDose1Total <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatorySeriesCompleteTotal <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatoryAdministeredDose1 <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
circulatorySeriesComplete <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Series_Complete_18_to_65)

# checking allCause population totals
allCauseNonResidualPopulation <- sum(countyPopulationAllCauseNonResidual[countyPopulationAllCauseNonResidual$YEAR == "2021", ]$AGE18TO65)
allCauseResidualPopulation <- sum(countyPopulationAllCauseResiduals[countyPopulationAllCauseResiduals$YEAR == "2021", ]$AGE18TO65)
allCauseTotalPopulation <- sum(countyPopulationAllYearsWithAllCauseResiduals[countyPopulationAllYearsWithAllCauseResiduals$YEAR == "2021", ]$AGE18TO65)
allCauseResultPopulation <- sum(result[result$Month.Code == "2021/12", ]$AGE18TO65)

# checking circulatory population totals
circulatoryNonResidualPopulation <- sum(countyPopulationCirculatoryNonResidual[countyPopulationCirculatoryNonResidual$YEAR == "2021", ]$AGE18TO65)
circulatoryResidualPopulation <- sum(countyPopulationCirculatoryResiduals[countyPopulationCirculatoryResiduals$YEAR == "2021", ]$AGE18TO65)
circulatoryTotalPopulation <- sum(countyPopulationAllYearsWithCirculatoryResiduals[countyPopulationAllYearsWithCirculatoryResiduals$YEAR == "2021", ]$AGE18TO65)
circulatoryResultPopulation <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$AGE18TO65)

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



