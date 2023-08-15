# checking allCause death totals
allCauseNonResidualDeaths <- sum(allCauseAllYearsFiltered[allCauseAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
allCauseResidualDeaths <- sum(allCauseCountyResiduals[allCauseCountyResiduals$Month.Code == "2021/12", ]$Deaths)
allCauseDeathTotal <- sum(allCauseStates[allCauseStates$Month.Code == "2021/12", ]$Deaths)
allCauseResultDeathTotal <- sum(result[result$Month.Code == "2021/12", ]$Deaths)
assertthat::are_equal(allCauseNonResidualDeaths + allCauseResidualDeaths, allCauseResultDeathTotal, allCauseDeathTotal)

# checking circulatory death totals 
circulatoryNonResidualDeaths <- sum(circulatoryAllYearsFiltered[circulatoryAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
circulatoryResidualDeaths <- sum(circulatoryCountyResiduals[circulatoryCountyResiduals$Month.Code == "2021/12", ]$Deaths)
circulatoryDeathTotal <- sum(circulatoryStates[circulatoryStates$Month.Code == "2021/12", ]$Deaths)
circulatoryResultDeathTotal <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Deaths)
assertthat::are_equal(circulatoryResidualDeaths + circulatoryNonResidualDeaths, circulatoryDeathTotal, circulatoryResultDeathTotal)

# checking allCause administered dose 1 totals
allCauseNonResidualAdminsteredDose1 <- sum(allCauseNonResidual[allCauseNonResidual$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
allCauseResidualAdminsteredDose1 <- sum(allCauseUptakeResiduals[allCauseUptakeResiduals$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
allCauseAdminsteredDose1Total <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
resultAdministeredDose1 <- sum(result[result$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
assertthat::are_equal(allCauseNonResidualAdminsteredDose1 + allCauseResidualAdminsteredDose1, allCauseAdminsteredDose1Total, resultAdministeredDose1)

# checking allCause seriesComplete totals
allCauseNonResidualSeriesComplete <- sum(allCauseNonResidual[allCauseNonResidual$Date == "2021-12-31", ]$Series_Complete_18_to_65)
allCauseResidualSeriesComplete <- sum(allCauseUptakeResiduals[allCauseUptakeResiduals$Date == "2021-12-31", ]$Series_Complete_18_to_65)
allCauseSeriesCompleteTotal <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
resultSeriesComplete <- sum(result[result$Month.Code == "2021/12", ]$Series_Complete_18_to_65)
assertthat::are_equal(allCauseNonResidualSeriesComplete + allCauseResidualSeriesComplete, allCauseSeriesCompleteTotal, resultSeriesComplete)

# checking circulatory administered dose 1 totals
circulatoryNonResidualAdminsteredDose1 <- sum(circulatoryNonResidual[circulatoryNonResidual$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatoryResidualAdminsteredDose1 <- sum(circulatoryUptakeResiduals[circulatoryUptakeResiduals$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatoryAdminsteredDose1Total <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatoryAdministeredDose1 <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
assertthat::are_equal(circulatoryNonResidualAdminsteredDose1 + circulatoryResidualAdminsteredDose1, circulatoryAdminsteredDose1Total, circulatoryAdministeredDose1)

# checking circulatory series complete totals
circulatoryNonResidualSeriesComplete <- sum(circulatoryNonResidual[circulatoryNonResidual$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatoryResidualSeriesComplete <- sum(circulatoryUptakeResiduals[circulatoryUptakeResiduals$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatorySeriesCompleteTotal <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatorySeriesComplete <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Series_Complete_18_to_65)
assertthat::are_equal(circulatoryNonResidualSeriesComplete + circulatoryResidualSeriesComplete, circulatorySeriesCompleteTotal, circulatorySeriesComplete)

# checking allCause population totals
allCauseNonResidualPopulation <- sum(countyPopulationAllCauseNonResidual[countyPopulationAllCauseNonResidual$YEAR == "2021", ]$AGE18TO65)
allCauseResidualPopulation <- sum(countyPopulationAllCauseResiduals[countyPopulationAllCauseResiduals$YEAR == "2021", ]$AGE18TO65)
allCauseTotalPopulation <- sum(countyPopulationAllYearsWithAllCauseResiduals[countyPopulationAllYearsWithAllCauseResiduals$YEAR == "2021", ]$AGE18TO65)
allCauseResultPopulation <- sum(result[result$Month.Code == "2021/12", ]$AGE18TO65)
assertthat::are_equal(allCauseNonResidualPopulation + allCauseResidualPopulation, allCauseTotalPopulation, allCauseResultPopulation)

# checking circulatory population totals
circulatoryNonResidualPopulation <- sum(countyPopulationCirculatoryNonResidual[countyPopulationCirculatoryNonResidual$YEAR == "2021", ]$AGE18TO65)
circulatoryResidualPopulation <- sum(countyPopulationCirculatoryResiduals[countyPopulationCirculatoryResiduals$YEAR == "2021", ]$AGE18TO65)
circulatoryTotalPopulation <- sum(countyPopulationAllYearsWithCirculatoryResiduals[countyPopulationAllYearsWithCirculatoryResiduals$YEAR == "2021", ]$AGE18TO65)
circulatoryResultPopulation <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$AGE18TO65)
assertthat::are_equal(circulatoryNonResidualPopulation + circulatoryResidualPopulation, circulatoryTotalPopulation, circulatoryResultPopulation)

# checking allCause covid death totals
allCauseNonResidualCovidDeaths <- sum(covidDeathAllYearsAllCauseNonResidual[covidDeathAllYearsAllCauseNonResidual$Month.Code == "2021/12", ]$deaths)
allCauseResidualCovidDeaths <- sum(covidDeathAllCauseResiduals[covidDeathAllCauseResiduals$Month.Code == "2021/12", ]$covid_deaths)
allCauseTotalCovidDeaths <- sum(covidDeathAllYearsAllCauseWithResidual[covidDeathAllYearsAllCauseWithResidual$Month.Code == "2021/12", ]$covid_deaths)
resultTotalCovidDeaths <- sum(resultWithCovidDeaths[resultWithCovidDeaths$Month.Code == "2021/12", ]$covid_deaths)
assertthat::are_equal(allCauseNonResidualCovidDeaths + allCauseResidualCovidDeaths, allCauseTotalCovidDeaths, resultTotalCovidDeaths)

# checking allCause covid case totals
allCauseNonResidualCovidCases <- sum(covidDeathAllYearsAllCauseNonResidual[covidDeathAllYearsAllCauseNonResidual$Month.Code == "2021/12", ]$cases)
allCauseResidualCovidCases <- sum(covidDeathAllCauseResiduals[covidDeathAllCauseResiduals$Month.Code == "2021/12", ]$covid_cases)
allCauseTotalCovidCases <- sum(covidDeathAllYearsAllCauseWithResidual[covidDeathAllYearsAllCauseWithResidual$Month.Code == "2021/12", ]$covid_cases)
resultTotalCovidCases <- sum(resultWithCovidDeaths[resultWithCovidDeaths$Month.Code == "2021/12", ]$covid_cases)
assertthat::are_equal(allCauseNonResidualCovidCases + allCauseResidualCovidCases, allCauseTotalCovidCases, resultTotalCovidCases)

# checking circulatory covid death totals
circulatoryNonResidualCovidDeaths <- sum(covidDeathAllYearsCirculatoryNonResidual[covidDeathAllYearsCirculatoryNonResidual$Month.Code == "2021/12", ]$deaths)
circulatoryResidualCovidDeaths <- sum(covidDeathCirculatoryResiduals[covidDeathCirculatoryResiduals$Month.Code == "2021/12", ]$covid_deaths)
circulatoryTotalCovidDeaths <- sum(covidDeathAllYearsCirculatoryWithResidual[covidDeathAllYearsCirculatoryWithResidual$Month.Code == "2021/12", ]$covid_deaths)
circulatoryResultTotalCovidDeaths <- sum(resultCirculatoryWithCovidDeaths[resultCirculatoryWithCovidDeaths$Month.Code == "2021/12", ]$covid_deaths)
assertthat::are_equal(circulatoryNonResidualCovidDeaths + circulatoryResidualCovidDeaths, circulatoryTotalCovidDeaths, circulatoryResultTotalCovidDeaths)

# checking circulatory covid case totals
circulatoryNonResidualCovidCases <- sum(covidDeathAllYearsCirculatoryNonResidual[covidDeathAllYearsCirculatoryNonResidual$Month.Code == "2021/12", ]$cases)
circulatoryResidualCovidCases <- sum(covidDeathCirculatoryResiduals[covidDeathCirculatoryResiduals$Month.Code == "2021/12", ]$covid_cases)
circulatoryTotalCovidCases <- sum(covidDeathAllYearsCirculatoryWithResidual[covidDeathAllYearsCirculatoryWithResidual$Month.Code == "2021/12", ]$covid_cases)
circulatoryResultTotalCovidCases <- sum(resultCirculatoryWithCovidDeaths[resultCirculatoryWithCovidDeaths$Month.Code == "2021/12", ]$covid_cases)
assertthat::are_equal(circulatoryNonResidualCovidCases + circulatoryResidualCovidCases, circulatoryTotalCovidCases, circulatoryResultTotalCovidCases)


