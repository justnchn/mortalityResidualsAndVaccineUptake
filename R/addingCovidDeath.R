# read in covid death data
covidDeath2020 <- read.csv("~/Desktop/Research/us-counties-2020.csv")
covidDeath2021 <- read.csv("~/Desktop/Research/us-counties-2021.csv")

# bind the two datasets and create Month.Code column formatted as yyyy/mm
covidDeathAllYears <- rbind(covidDeath2020, covidDeath2021)
covidDeathAllYears$Month.Code <- format(as.Date(covidDeathAllYears$date), "%Y/%m")

# filtering the dataset such that for each county, only the most recent entry in a given yyyy/mm remains
covidDeathAllYears <- covidDeathAllYears %>% group_by(Month.Code, fips) %>%
  filter(date == max(date)) %>%
  ungroup()

# filtering dataset to find all cause residual and non residuial totals and compiling state totals for each
covidDeathAllYearsAllCauseFiltered <- covidDeathAllYears[covidDeathAllYears$fips %in% allCauseAllYears$County.Code, ]
covidDeathAllYearsAllCauseResidualMerging <- subset(covidDeathAllYearsAllCauseFiltered, select = c(Month.Code, state, cases, deaths)) %>%
  group_by(Month.Code, state) %>%
  summarize(across(cases:deaths, sum))
covidDeathAllYearsAllCauseNonResidualCounties <- covidDeathAllYears[covidDeathAllYears$fips %in% result$County.Code, ]
covidDeathAllYearsAllCauseNonResidual <- covidDeathAllYearsAllCauseNonResidualCounties %>%
  subset(select = c(Month.Code, state, cases, deaths)) %>%
  group_by(Month.Code, state) %>%
  summarize(across(cases:deaths, sum))

# calculating covid death resiudals
covidDeathAllCauseResiduals <- left_join(covidDeathAllYearsAllCauseResidualMerging, covidDeathAllYearsAllCauseNonResidual, by = c("Month.Code", "state"))
covidDeathAllCauseResiduals[is.na(covidDeathAllCauseResiduals)] <- 0
covidDeathAllCauseResiduals$covid_deaths <- covidDeathAllCauseResiduals$deaths.x - covidDeathAllCauseResiduals$deaths.y
covidDeathAllCauseResiduals$covid_cases <- covidDeathAllCauseResiduals$cases.x - covidDeathAllCauseResiduals$cases.y
covidDeathAllCauseResiduals <- changeToStateAbbrev(covidDeathAllCauseResiduals) %>%
  rename(state = state_abbreviations)
covidDeathAllCauseResiduals$fips <- covidDeathAllCauseResiduals$state
covidDeathAllCauseResiduals <- covidDeathAllCauseResiduals %>%
  subset(select = c(Month.Code, fips, state, covid_deaths, covid_cases))

# filtering dataset to find residual and non residual totals for circulatory counties and compiling totals
covidDeathAllYearsCirculatoryFiltered <- covidDeathAllYears[covidDeathAllYears$fips %in% circulatoryAllYears$County.Code, ]
covidDeathAllYearsCirculatoryResidualMerging <- subset(covidDeathAllYearsCirculatoryFiltered, select = c(Month.Code, state, cases, deaths)) %>%
  group_by(Month.Code, state) %>%
  summarize(across(cases:deaths, sum))
covidDeathAllYearsCirculatoryNonResidualCounties <- covidDeathAllYears[covidDeathAllYears$fips %in% resultCirculatory$County.Code, ]
covidDeathAllYearsCirculatoryNonResidual <- covidDeathAllYearsCirculatoryNonResidualCounties %>%
  subset(select = c(Month.Code, state, cases, deaths)) %>%
  group_by(Month.Code, state) %>%
  summarize(across(cases:deaths, sum))

# calculating covid death resiudals
covidDeathCirculatoryResiduals <- left_join(covidDeathAllYearsCirculatoryResidualMerging, covidDeathAllYearsCirculatoryNonResidual, by = c("Month.Code", "state"))
covidDeathCirculatoryResiduals[is.na(covidDeathCirculatoryResiduals)] <- 0
covidDeathCirculatoryResiduals$covid_deaths <- covidDeathCirculatoryResiduals$deaths.x - covidDeathCirculatoryResiduals$deaths.y
covidDeathCirculatoryResiduals$covid_cases <- covidDeathCirculatoryResiduals$cases.x - covidDeathCirculatoryResiduals$cases.y
covidDeathCirculatoryResiduals <- changeToStateAbbrev(covidDeathCirculatoryResiduals) %>%
  rename(state = state_abbreviations)
covidDeathCirculatoryResiduals$fips <- covidDeathCirculatoryResiduals$state
covidDeathCirculatoryResiduals <- covidDeathCirculatoryResiduals %>%
  subset(select = c(Month.Code, fips, state, covid_deaths, covid_cases))

# changing columns in all cause county death dataframe and binding with residual totals
covidDeathAllYearsAllCauseWithResidual <- covidDeathAllYearsAllCauseNonResidualCounties %>%
  rename(covid_cases = cases, covid_deaths = deaths) %>%
  subset(select = c(Month.Code, fips, state, covid_deaths, covid_cases)) %>%
  rbind(covidDeathAllCauseResiduals) %>%
  select(-state)

# changing columns in circulatory county death dataframe and binding with residual totals
covidDeathAllYearsCirculatoryWithResidual <- covidDeathAllYearsCirculatoryNonResidualCounties %>%
  rename(covid_cases = cases, covid_deaths = deaths) %>%
  subset(select = c(Month.Code, fips, state, covid_deaths, covid_cases)) %>%
  rbind(covidDeathCirculatoryResiduals) %>%
  select(-state)


# left joining the covid death totals with result dataframes
resultWithCovidDeaths <- result %>%
  left_join(covidDeathAllYearsAllCauseWithResidual, by = c("County.Code" = "fips", "Month.Code"))
resultWithCovidDeaths$covid_deaths[is.na(resultWithCovidDeaths$covid_deaths)] <- 0
resultWithCovidDeaths$covid_cases[is.na(resultWithCovidDeaths$covid_cases)] <- 0

resultCirculatoryWithCovidDeaths <- resultCirculatory %>%
  left_join(covidDeathAllYearsCirculatoryWithResidual, by = c("County.Code" = "fips", "Month.Code"))
resultCirculatoryWithCovidDeaths$covid_deaths[is.na(resultCirculatoryWithCovidDeaths$covid_deaths)] <- 0
resultCirculatoryWithCovidDeaths$covid_cases[is.na(resultCirculatoryWithCovidDeaths$covid_cases)] <- 0

# exporting circulatory results and circulatory vaccine uptake
write.csv(resultWithCovidDeaths, "~/Desktop/Research/allCauseWithCovidDeaths.csv")
write.csv(resultCirculatoryWithCovidDeaths, "~/Desktop/Research/circulatoryWithCovidDeaths.csv")

covidDeathStateTotals <- read.csv("~/Desktop/Research/us-states.csv")
covidDeathStateTotals$Month.Code <- format(as.Date(covidDeathStateTotals$date), "%Y/%m")
covidDeathStateTotals <- covidDeathStateTotals %>%
  group_by(Month.Code, state) %>%
  filter(date == max(date))

allCauseStatesWithCovidDeath <- allCauseStatesWithVaccineUptake %>%
  left_join(covidDeathStateTotals, by = c("Month.Code", "State" = "state"))
allCauseStatesWithCovidDeath[is.na(allCauseStatesWithCovidDeath)] <- 0

circulatoryStatesWithCovidDeath <- circulatoryStatesWithVaccineUptake %>%
  left_join(covidDeathStateTotals, by = c("Month.Code", "State" = "state"))
circulatoryStatesWithCovidDeath[is.na(circulatoryStatesWithCovidDeath)] <- 0

write.csv(allCauseStatesWithCovidDeath, "~/Desktop/Research/allCauseStatesWithCovidDeaths.csv")
write.csv(circulatoryStatesWithCovidDeath, "~/Desktop/Research/circulatoryWithCovidDeaths.csv")
