# Reading in allCause county and state datasets from files
allCauseCounties1819 <- read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality (County + Month, 2018-2019).txt")
allCauseCounties2021 <- read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality (County + Month, 2020-2021).txt")
allCauseStates <- subset(read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality--State and Month.txt"), select =
                           -c(Notes, Census.Region, Census.Region.Code, State.Code,
                              Month, Population, Crude.Rate))

# merging the two allCause sets, removing all counties that do not have data for each month in the dataset
allCauseAllYears <- rbind(allCauseCounties1819, allCauseCounties2021)
allCauseAllYears <- removeMissingCounties(allCauseAllYears)
allCauseAllYears <- allCauseAllYears[allCauseAllYears$County.Code %in% vaccineFIPS,]
allCauseAllYears <- subset(allCauseAllYears, select = -c(Notes, Census.Region, Census.Region.Code, Month))

# Summing all counties in each state by month and subtracting values from state dataset to obtain allCauseResidual
allCauseSums <- calculateDeathsStateMonth(allCauseAllYears)
allCauseSums <- changeToStateNames(allCauseSums)
allCauseStates <- removeEmptyRows(allCauseStates, "Month.Code")
allCauseResiduals <- calculateResidual(allCauseStates, allCauseSums)

# Merging allCauseResiduals with allCauseCounties
allCauseResidualsMerging <- subset(allCauseResiduals, select = -c(Deaths.x, Deaths.y))
allCauseResidualsMerging <- rename(allCauseResidualsMerging, County = State, Deaths = Residual)
allCauseCountyWithResiduals <- rbind(allCauseAllYears, allCauseResidualsMerging)
allCauseCountyWithResiduals$State <- gsub(".*,\\s+", "", allCauseCountyWithResiduals$County)
allCauseCountyWithResiduals$County.Code <- ifelse(is.na(allCauseCountyWithResiduals$County.Code), allCauseCountyWithResiduals$State, allCauseCountyWithResiduals$County.Code)
allCauseCountyWithResiduals <- changeToStateNames(allCauseCountyWithResiduals)
allCauseCountyWithResiduals$State <- ifelse(allCauseCountyWithResiduals$State == ""
                                            , allCauseCountyWithResiduals$County, allCauseCountyWithResiduals$State)
allCauseCountyWithResiduals$Year <- substr(allCauseCountyWithResiduals$Month.Code, 1, 4)

# reading in county and state circulatory datasets
circulatory1819 <- read.delim("~/Desktop/Research/Mortality Residuals/Circulatory Disease Mortality (County + Month, 2018-2019).txt")
circulatory2021 <- read.delim("~/Desktop/Research/Mortality Residuals/Circulatory Disease Mortality (County + Month, 2020-2021).txt")
circulatoryStates <- subset(read.delim("~/Desktop/Research/Mortality Residuals/Circulatory Disease Mortality (State + Month).txt"),
                            select = -c(Notes, Census.Region, Census.Region.Code, State.Code,
                                        Month, Population, Crude.Rate))

# merging the two circulatory sets, removing all counties that do not have data for each month in the dataset
circulatoryAllYears <- rbind(circulatory1819, circulatory2021)
circulatoryAllYears <- removeMissingCounties(circulatoryAllYears)
circulatoryAllYears <- circulatoryAllYears[circulatoryAllYears$County.Code %in% vaccineFIPS,]

# calculating ciruclatory residuals
circulatorySums <- calculateDeathsStateMonth(circulatoryAllYears)
circulatorySums <- changeToStateNames(circulatorySums)
circulatoryStates <- removeEmptyRows(circulatoryStates, "Month.Code")
circulatoryResiduals <- calculateResidual(circulatoryStates, circulatorySums)

# merging circulatory counties and state residuals
circulatoryAllYears <- subset(circulatoryAllYears, select =
                                -c(Notes, Census.Region, Census.Region.Code,
                                   Month))
circulatoryResidualsMerging <- subset(circulatoryResiduals, select = -c(Deaths.x, Deaths.y))
circulatoryResidualsMerging <- rename(circulatoryResidualsMerging, County = State, Deaths = Residual)
circulatoryResidualsMerging$County.Code <- circulatoryResidualsMerging$County
circulatoryAllYears$County.Code <- as.character(circulatoryAllYears$County.Code)
circulatoryCountyWithResiduals <- rbind(circulatoryAllYears, circulatoryResidualsMerging)
circulatoryCountyWithResiduals$State <- gsub(".*,\\s+", "", circulatoryCountyWithResiduals$County)
circulatoryCountyWithResiduals$County.Code <- ifelse(is.na(circulatoryCountyWithResiduals$County.Code), circulatoryCountyWithResiduals$State, circulatoryCountyWithResiduals$County.Code)
circulatoryCountyWithResiduals <- changeToStateNames(circulatoryCountyWithResiduals)
circulatoryCountyWithResiduals$State <- ifelse(circulatoryCountyWithResiduals$State == ""
                                               , circulatoryCountyWithResiduals$County, circulatoryCountyWithResiduals$State)
circulatoryCountyWithResiduals$Year <- substr(circulatoryCountyWithResiduals$Month.Code, 1, 4)


