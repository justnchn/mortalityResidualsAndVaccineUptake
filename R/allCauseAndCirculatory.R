# Reading in allCause county and state datasets from files
allCauseCounties1819 <- read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality (County + Month, 2018-2019).txt")
allCauseCounties2021 <- read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality (County + Month, 2020-2021).txt")
allCauseStates <- subset(read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality--State and Month.txt"), select =
                           -c(Notes, Census.Region, Census.Region.Code, State.Code,
                              Month, Population, Crude.Rate))

# Modifying allCauseStates dataset
allCauseStates <- allCauseStates %>%
  filter(!is.na(Month.Code) & grepl("^\\d{4}/\\d{2}$", Month.Code)) %>%
  removeEmptyRows("Month.Code")

# merging the two allCause sets, removing all counties that do not have data for each month in the dataset
allCauseAllYears <- rbind(allCauseCounties1819, allCauseCounties2021)
allCauseAllYears <- allCauseAllYears[complete.cases(allCauseAllYears), ]

allCauseAllYearsFiltered <- subset(allCauseAllYears, select = -c(Notes, Census.Region, Census.Region.Code, Month)) %>%
  removeMissingCounties() %>%
  filter(County.Code %in% vaccineFIPS)

# Summing all counties in each state by month and subtracting values from state dataset to obtain allCauseResidual
allCauseSums <- calculateDeathsStateMonth(allCauseAllYearsFiltered)
allCauseResidualComparison <- calculateDeathsStateMonth(allCauseAllYears)
allCauseResiduals <- calculateResidual(allCauseResidualComparison, allCauseSums)

# Merging allCauseResiduals with allCauseCounties
allCauseResidualsMerging <- subset(allCauseResiduals, select = -c(Deaths.x, Deaths.y)) %>%
  rename(County = State, Deaths = Residual)
allCauseCountyWithResiduals <-
  rbind(allCauseAllYearsFiltered, allCauseResidualsMerging)

# Modifying to include columns for State and Year and add County.Code values to state residuals
allCauseCountyWithResiduals$State <-
  gsub(".*,\\s+", "", allCauseCountyWithResiduals$County)
allCauseCountyWithResiduals$County.Code <-
  ifelse(is.na(allCauseCountyWithResiduals$County.Code),
         allCauseCountyWithResiduals$State, allCauseCountyWithResiduals$County.Code)
allCauseCountyWithResiduals <-
  changeToStateNames(allCauseCountyWithResiduals)
allCauseCountyWithResiduals$State <-
  ifelse(allCauseCountyWithResiduals$State == "",
         allCauseCountyWithResiduals$County, allCauseCountyWithResiduals$State)
allCauseCountyWithResiduals$Year <-
  substr(allCauseCountyWithResiduals$Month.Code, 1, 4)
allCauseCountyWithResiduals$Month <-
  substr(allCauseCountyWithResiduals$Month.Code, 6, 7)

# reading in county and state circulatory datasets
circulatory1819 <- read.delim("~/Desktop/Research/Mortality Residuals/Circulatory Disease Mortality (County + Month, 2018-2019).txt")
circulatory2021 <- read.delim("~/Desktop/Research/Mortality Residuals/Circulatory Disease Mortality (County + Month, 2020-2021).txt")
circulatoryStates <- subset(read.delim("~/Desktop/Research/Mortality Residuals/Circulatory Disease Mortality (State + Month).txt"),
                            select = -c(Notes, Census.Region, Census.Region.Code, State.Code,
                                        Month, Population, Crude.Rate))
circulatoryStates <- circulatoryStates %>%
  filter(!is.na(Month.Code) & grepl("^\\d{4}/\\d{2}$", Month.Code))

# merging the two circulatory sets, removing all counties that do not have data for each month in the dataset
circulatoryAllYears <- rbind(circulatory1819, circulatory2021)
circulatoryAllYears <- circulatoryAllYears[complete.cases(circulatoryAllYears), ]
circulatoryAllYearsFiltered <- removeMissingCounties(circulatoryAllYears) %>%
  subset(select = -c(Notes, Census.Region, Census.Region.Code, Month))
circulatoryAllYearsFiltered <- circulatoryAllYearsFiltered[circulatoryAllYearsFiltered$County.Code %in% vaccineFIPS,]

# calculating ciruclatory residuals
circulatorySums <- calculateDeathsStateMonth(circulatoryAllYearsFiltered)
circulatoryResidualComparison <- calculateDeathsStateMonth(circulatoryAllYears)
circulatoryResiduals <- calculateResidual(circulatoryResidualComparison, circulatorySums)

# merging circulatory counties and state residuals
circulatoryResidualsMerging <- subset(circulatoryResiduals, select = -c(Deaths.x, Deaths.y)) %>%
  rename(County = State, Deaths = Residual)
circulatoryCountyWithResiduals <-
  rbind(circulatoryAllYearsFiltered, circulatoryResidualsMerging)

# Modifying to include columns for State and Year and add County.Code values to state residuals
circulatoryCountyWithResiduals$State <-
  gsub(".*,\\s+", "", circulatoryCountyWithResiduals$County)
circulatoryCountyWithResiduals$County.Code <-
  ifelse(is.na(circulatoryCountyWithResiduals$County.Code),
         circulatoryCountyWithResiduals$State, circulatoryCountyWithResiduals$County.Code)
circulatoryCountyWithResiduals <-
  changeToStateNames(circulatoryCountyWithResiduals)
circulatoryCountyWithResiduals$State <-
  ifelse(circulatoryCountyWithResiduals$State == "",
         circulatoryCountyWithResiduals$County, circulatoryCountyWithResiduals$State)
circulatoryCountyWithResiduals$Year <-
  substr(circulatoryCountyWithResiduals$Month.Code, 1, 4)
circulatoryCountyWithResiduals$Month <-
  substr(circulatoryCountyWithResiduals$Month.Code, 6, 7)


