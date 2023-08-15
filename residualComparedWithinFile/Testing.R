# checking allCause death totals
allCauseNonResidualDeaths <- sum(allCauseAllYearsFiltered[allCauseAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
allCauseResidualDeaths <- sum(allCauseResidualsMerging[allCauseResidualsMerging$Month.Code == "2021/12", ]$Deaths)
allCauseDeathTotal <- sum(allCauseStates[allCauseStates$Month.Code == "2021/12", ]$Deaths)
allCauseResultDeathTotal <- sum(result[result$Month.Code == "2021/12", ]$Deaths)
assertthat::are_equal(allCauseNonResidualDeaths+allCauseResidualDeaths, allCauseResultDeathTotal)

# checking circulatory death totals
circulatoryNonResidualDeaths <- sum(circulatoryAllYearsFiltered[circulatoryAllYearsFiltered$Month.Code == "2021/12", ]$Deaths)
circulatoryResidualDeaths <- sum(circulatoryResidualsMerging[circulatoryResidualsMerging$Month.Code == "2021/12", ]$Deaths)
circulatoryDeathTotal <- sum(circulatoryCountyWithResiduals[circulatoryCountyWithResiduals$Month.Code == "2021/12", ]$Deaths)
circulatoryResultDeathTotal <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Deaths)
assertthat::are_equal(circulatoryNonResidualDeaths+circulatoryResidualDeaths, circulatoryResultDeathTotal)

# checking values for allCause administered dose 1
allCauseNonResidualAdminsteredDose1 <- sum(allCauseUptakeCounties[allCauseUptakeCounties$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
allCauseResidualAdministeredDose1 <- sum(vaccineUptakeResiduals[vaccineUptakeResiduals$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
countyAllCauseAdministeredDose1 <- sum(result[result$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
countyAllCausePreMergeAdministered <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
assertthat::are_equal(allCauseNonResidualAdminsteredDose1 + allCauseResidualAdministeredDose1, countyAllCauseAdministeredDose1, countyAllCausePreMergeAdministered)

# checking values for allCause series complete
allCauseNonResidualSeriesComplete <- sum(allCauseUptakeCounties[allCauseUptakeCounties$Date == "2021-12-31", ]$Series_Complete_18_to_65)
allCauseNonResidualSeriesComplete <- sum(vaccineUptakeResiduals[vaccineUptakeResiduals$Date == "2021-12-31", ]$Series_Complete_18_to_65)
countyAllCauseSeriesComplete <- sum(result[result$Month.Code == "2021/12", ]$Series_Complete_18_to_65)
countyAllCausePreMergeSeriesComplete <- sum(vaccineUptakeWithAllCauseResidualsCut[vaccineUptakeWithAllCauseResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
assertthat::are_equal(allCauseNonResidualSeriesComplete + allCauseNonResidualSeriesComplete, countyAllCauseSeriesComplete, countyAllCausePreMergeSeriesComplete)

# checking values for circulatory administered dose 1
circulatoryNonResidualAdminsteredDose1 <- sum(circulatoryUptakeCounties[circulatoryUptakeCounties$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
circulatoryResidualAdministeredDose1 <- sum(vaccineUptakeResiduals[vaccineUptakeResiduals$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
countyCirculatoryAdministeredDose1 <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Administered_Dose1_18_to_65)
countyCirculatoryPreMergeAdministered <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Administered_Dose1_18_to_65)
assertthat::are_equal(circulatoryNonResidualAdminsteredDose1 + circulatoryResidualAdministeredDose1, countyCirculatoryAdministeredDose1, countyCirculatoryPreMergeAdministered)

# checking values for circulatory series complete
circulatoryNonResidualSeriesComplete <- sum(circulatoryUptakeCounties[circulatoryUptakeCounties$Date == "2021-12-31", ]$Series_Complete_18_to_65)
circulatoryResidualSeriesComplete <- sum(vaccineUptakeResiduals[vaccineUptakeResiduals$Date == "2021-12-31", ]$Series_Complete_18_to_65)
countyCirculatorySeriesComplete <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$Series_Complete_18_to_65)
countyCirculatoryPreMergeSeriesComplete <- sum(vaccineUptakeWithCirculatoryResidualsCut[vaccineUptakeWithCirculatoryResidualsCut$Date == "2021-12-31", ]$Series_Complete_18_to_65)
assertthat::are_equal(circulatoryNonResidualAdminsteredDose1 + circulatoryResidualAdministeredDose1, countyCirculatoryAdministeredDose1, countyCirculatoryPreMergeAdministered)

# checking population values for allCause result
countyAllCauseNonResidualPopulation <- sum(countyPopulationAllCauseNonResidual[countyPopulationAllCauseNonResidual$YEAR == "2021", ]$AGE18TO65)
countyAllCauseResidualPopulation <- sum(countyPopulationAllCauseResiduals[countyPopulationAllCauseResiduals$YEAR == "2021", ]$AGE18TO65)
allCauseResultPopulaton <- sum(result[result$Month.Code == "2021/12", ]$AGE18TO65)
assertthat::are_equal(countyAllCauseNonResidualPopulation + countyAllCauseResidualPopulation, allCauseResultPopulaton)

# checking population values for circulatory result
countyCirculatoryNonResidualPopulation <- sum(countyPopulationCirculatoryNonResidual[countyPopulationCirculatoryNonResidual$YEAR == "2021", ]$AGE18TO65)
countyCirculatoryResidualPopulation <- sum(countyPopulationCirculatoryResiduals[countyPopulationCirculatoryResiduals$YEAR == "2021", ]$AGE18TO65)
circulatoryResultPopulaton <- sum(resultCirculatory[resultCirculatory$Month.Code == "2021/12", ]$AGE18TO65)
assertthat::are_equal(countyAllCauseNonResidualPopulation + countyAllCauseResidualPopulation, allCauseResultPopulaton)

# checking covid death values for allCause result
allCauseNonResidualCovidDeaths <- sum(covidDeathAllYearsAllCauseNonResidual[covidDeathAllYearsAllCauseNonResidual$Month.Code == "2021/12", ]$deaths)
allCauseResidualCovidDeaths <- sum(covidDeathAllCauseResiduals[covidDeathAllCauseResiduals$Month.Code == "2021/12", ]$covid_deaths)
allCauseTotalCovidDeaths <- sum(covidDeathAllYearsAllCauseWithResidual[covidDeathAllYearsAllCauseWithResidual$Month.Code == "2021/12", ]$covid_deaths)
resultTotalCovidDeaths <- sum(resultWithCovidDeaths[resultWithCovidDeaths$Month.Code == "2021/12", ]$covid_deaths)
assertthat::are_equal(allCauseNonResidualCovidDeaths+allCauseResidualCovidDeaths, allCauseTotalCovidDeaths, resultTotalCovidDeaths)

# checking covid cases values for allCause result
allCauseNonResidualCovidCases <- sum(covidDeathAllYearsAllCauseNonResidual[covidDeathAllYearsAllCauseNonResidual$Month.Code == "2021/12", ]$cases)
allCauseResidualCovidCases <- sum(covidDeathAllCauseResiduals[covidDeathAllCauseResiduals$Month.Code == "2021/12", ]$covid_cases)
allCauseTotalCovidCases <- sum(covidDeathAllYearsAllCauseWithResidual[covidDeathAllYearsAllCauseWithResidual$Month.Code == "2021/12", ]$covid_cases)
resultTotalCovidCases <- sum(resultWithCovidDeaths[resultWithCovidDeaths$Month.Code == "2021/12", ]$covid_cases)
assertthat::are_equal(allCauseNonResidualCovidCases+allCauseResidualCovidCases, allCauseTotalCovidCases, resultTotalCovidCases)

# checking covid death values for circulatory result
circulatoryNonResidualCovidDeaths <- sum(covidDeathAllYearsCirculatoryNonResidual[covidDeathAllYearsCirculatoryNonResidual$Month.Code == "2021/12", ]$deaths)
circulatoryResidualCovidDeaths <- sum(covidDeathCirculatoryResiduals[covidDeathCirculatoryResiduals$Month.Code == "2021/12", ]$covid_deaths)
circulatoryTotalCovidDeaths <- sum(covidDeathAllYearsCirculatoryWithResidual[covidDeathAllYearsCirculatoryWithResidual$Month.Code == "2021/12", ]$covid_deaths)
circulatoryResultTotalCovidDeaths <- sum(resultCirculatoryWithCovidDeaths[resultCirculatoryWithCovidDeaths$Month.Code == "2021/12", ]$covid_deaths)
assertthat::are_equal(circulatoryNonResidualCovidDeaths+circulatoryResidualCovidDeaths, circulatoryTotalCovidDeaths, circulatoryResultTotalCovidDeaths)

# checking covid death and cases values for circulatory result
circulatoryNonResidualCovidCases <- sum(covidDeathAllYearsCirculatoryNonResidual[covidDeathAllYearsCirculatoryNonResidual$Month.Code == "2021/12", ]$cases)
circulatoryResidualCovidCases <- sum(covidDeathCirculatoryResiduals[covidDeathCirculatoryResiduals$Month.Code == "2021/12", ]$covid_cases)
circulatoryTotalCovidCases <- sum(covidDeathAllYearsCirculatoryWithResidual[covidDeathAllYearsCirculatoryWithResidual$Month.Code == "2021/12", ]$covid_cases)
circulatoryResultTotalCovidCases <- sum(resultCirculatoryWithCovidDeaths[resultCirculatoryWithCovidDeaths$Month.Code == "2021/12", ]$covid_cases)
assertthat::are_equal(circulatoryNonResidualCovidCases+circulatoryResidualCovidCases, circulatoryTotalCovidCases, circulatoryResultTotalCovidCases)

