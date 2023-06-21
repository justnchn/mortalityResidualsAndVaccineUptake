library(dplyr)

# Reading in allCause county and state datasets from files
allCauseCounties1819 <- read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality (County + Month, 2018-2019).txt")
allCauseCounties2021 <- read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality (County + Month, 2020-2021).txt")
allCauseStates <- subset(read.delim("~/Desktop/Research/Mortality Residuals/All Cause Mortality--State and Month.txt"), select =
                           -c(Notes, Census.Region, Census.Region.Code, State.Code,
                              Month, Population, Crude.Rate))

# loading in vaccineUptake states set
vaccineUptake <- read.csv("~/Desktop/Research/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")
vaccineUptake$Administered_Dose1_18_to_65 = vaccineUptake$Administered_Dose1_Recip_18Plus - vaccineUptake$Administered_Dose1_Recip_65Plus
vaccineUptake$Series_Complete_18_to_65 = vaccineUptake$Series_Complete_18Plus - vaccineUptake$Series_Complete_65Plus
vaccineUptakeStates <- subset(vaccineUptake, select = c(Date, Location, Administered_Dose1_18_to_65, Series_Complete_18_to_65)) %>%
  rename(Recip_State = Location) %>%
  removeNonStates()
vaccineUptakeStates$Date <- as.Date(vaccineUptakeStates$Date, format = "%m/%d/%Y")

# Cleaning vaccine uptake data at the county level
vaccineUptakeCounties <- read.csv("~/Desktop/Research/COVID-19_Vaccinations_in_the_United_States_County.csv")
# calculating administered dose 1 for 18 to 65 year olds
vaccineUptakeCounties$Administered_Dose1_18_to_65 <- ifelse(
  is.na(vaccineUptakeCounties$Administered_Dose1_Recip_18Plus) | is.na(vaccineUptakeCounties$Administered_Dose1_Recip_65Plus),
  0,
  vaccineUptakeCounties$Administered_Dose1_Recip_18Plus - vaccineUptakeCounties$Administered_Dose1_Recip_65Plus
)
# Calculating series complete for 18-65 year olds
vaccineUptakeCounties$Series_Complete_18_to_65 <- ifelse(
  is.na(vaccineUptakeCounties$Series_Complete_18Plus) | is.na(vaccineUptakeCounties$Series_Complete_65Plus),
  0,
  vaccineUptakeCounties$Series_Complete_18Plus - vaccineUptakeCounties$Series_Complete_65Plus
)
# Subsetting relevant columns and removing nonstates from the data
vaccineUptakeCounties <- subset(vaccineUptakeCounties, select = c(Date, Recip_State, FIPS, Administered_Dose1_18_to_65, Series_Complete_18_to_65)) %>%
  removeNonStates()
vaccineUptakeCounties$Date <- as.Date(vaccineUptakeCounties$Date, format = "%m/%d/%Y")
vaccineFIPS <- vaccineUptakeCounties %>% group_by(FIPS) %>% summarize(months = n_distinct(format(Date, "%m-%Y")))
vaccineFIPS <- vaccineFIPS[vaccineFIPS$months == "28",]
vaccineFIPS <- vaccineFIPS$FIPS

# Modifying allCauseStates dataset
allCauseStates <- allCauseStates %>%
  filter(!is.na(Month.Code) & grepl("^\\d{4}/\\d{2}$", Month.Code)) %>%
  removeEmptyRows("Month.Code")

# merging the two allCause sets, removing all counties that do not have data for each month in the dataset
allCauseAllYears <- rbind(allCauseCounties1819, allCauseCounties2021)
allCauseAllYears <- allCauseAllYears[complete.cases(allCauseAllYears), ]

# removing counties that do not have entries in all months or entries in the vaccine set from allcause counties and calculating totals
allCauseAllYearsFiltered <- subset(allCauseAllYears, select = -c(Notes, Census.Region, Census.Region.Code, Month)) %>%
  removeMissingCounties()
allCauseAllYearsFiltered <- allCauseAllYearsFiltered[allCauseAllYearsFiltered$County.Code %in% vaccineFIPS, ]
allCauseCountySums <- calculateDeathsStateMonth(allCauseAllYearsFiltered) %>% 
  changeToStateNames()

# merging the non residual and state totals together to find the residual
allCauseCountyResiduals <- left_join(allCauseStates, allCauseCountySums, by = c("State", "Month.Code")) 
allCauseCountyResiduals[is.na(allCauseCountyResiduals)] <- 0
allCauseCountyResiduals$Deaths <- allCauseCountyResiduals$Deaths.x - allCauseCountyResiduals$Deaths.y
allCauseCountyResiduals <- subset(allCauseCountyResiduals, select = -c(Deaths.x, Deaths.y))
allCauseCountyResiduals <- rename(allCauseCountyResiduals, County = State)

# bind non residual counties with residual state totals
allCauseAllYearsFiltered$County.Code <- as.character(allCauseAllYearsFiltered$County.Code)
allCauseCountyWithResiduals <- rbind(allCauseAllYearsFiltered, allCauseCountyResiduals)

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
  filter(!is.na(Month.Code) & grepl("^\\d{4}/\\d{2}$", Month.Code)) %>%
  removeEmptyRows("Month.Code")

# merging the two circulatory sets, removing all counties that do not have data for each month in the dataset
circulatoryAllYears <- rbind(circulatory1819, circulatory2021)
circulatoryAllYears <- circulatoryAllYears[complete.cases(circulatoryAllYears), ]

# removing counties that do not have entries in all months or entries in the vaccine set from allcause counties and calculating totals
circulatoryAllYearsFiltered <- subset(circulatoryAllYears, select = -c(Notes, Census.Region, Census.Region.Code, Month)) %>%
  removeMissingCounties()
circulatoryAllYearsFiltered <- circulatoryAllYearsFiltered[circulatoryAllYearsFiltered$County.Code %in% vaccineFIPS, ]
circulatoryCountySums <- calculateDeathsStateMonth(circulatoryAllYearsFiltered) %>% 
  changeToStateNames()

# merging the non residual and state totals together to find the residual
circulatoryCountyResiduals <- left_join(circulatoryStates, circulatoryCountySums, by = c("State", "Month.Code")) 
circulatoryCountyResiduals[is.na(circulatoryCountyResiduals)] <- 0
circulatoryCountyResiduals$Deaths <- circulatoryCountyResiduals$Deaths.x - circulatoryCountyResiduals$Deaths.y
circulatoryCountyResiduals <- subset(circulatoryCountyResiduals, select = -c(Deaths.x, Deaths.y))
circulatoryCountyResiduals <- rename(circulatoryCountyResiduals, County = State)

# bind non residual counties with residual state totals
circulatoryAllYearsFiltered$County.Code <- as.character(circulatoryAllYearsFiltered$County.Code)
circulatoryCountyWithResiduals <- rbind(circulatoryAllYearsFiltered, circulatoryCountyResiduals)

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



