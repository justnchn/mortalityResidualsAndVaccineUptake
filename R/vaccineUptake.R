library(lubridate)

# Cleaning vaccine uptake data at state level
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

# Creating vaccineFIPS vector to denote all counties that are have a data point for each month in this time range
vaccineUptakeCounties$Date <- as.Date(vaccineUptakeCounties$Date, format = "%m/%d/%Y")
vaccineFIPS <- vaccineUptakeCounties %>% group_by(FIPS) %>% summarize(months = n_distinct(format(Date, "%m-%Y")))
vaccineFIPS <- vaccineFIPS[vaccineFIPS$months == "28",]
vaccineFIPS <- vaccineFIPS$FIPS

# finding allCause residual values for vaccineUptakeCounties
# subset vaccineUptakeCounties by filtering out counties that are not represented in the allCause mortality data and do not have values in all months
allCauseUptakeCounties <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% allCauseCountyWithResiduals$County.Code,]
allCauseUptakeCounties <- allCauseUptakeCounties[allCauseUptakeCounties$FIPS %in% vaccineFIPS,]

# obtain nonresidual vaccineUptake by summing allCauseUptakeCounties
vaccineUptakeResidualMerging <- subset(allCauseUptakeCounties, select = -FIPS)
vaccineUptakeResidualMerging <- vaccineUptakeResidualMerging %>%
  group_by(Recip_State, Date) %>%
  summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65), Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))
# obtain total vaccineUptake by filtering out vaccineUptakeCounties by the county codes in allCauseAllYears
vaccineUptakeCountiesMerging <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% allCauseAllYears$County.Code,]
vaccineUptakeCountiesMerging <- vaccineUptakeCountiesMerging %>%
  group_by(Recip_State, Date) %>% summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65),Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))

# calculate vaccineUptakeResidual by subtracting nonresidual vaccineUptake from total vaccineUptake
vaccineUptakeResiduals <- left_join(vaccineUptakeResidualMerging, vaccineUptakeCountiesMerging, by = c("Recip_State", "Date"))
vaccineUptakeResiduals <- vaccineUptakeResiduals %>%
  mutate(Administered_Dose1_18_to_65.y = ifelse(is.na(Administered_Dose1_18_to_65.y), 0, Administered_Dose1_18_to_65.y), Administered_Dose1_18_to_65 = Administered_Dose1_18_to_65.y - Administered_Dose1_18_to_65.x) %>%
  mutate(Series_Complete_18_to_65.y = ifelse(is.na(Series_Complete_18_to_65.y), 0, Series_Complete_18_to_65.y),
         Series_Complete_18_to_65 = Series_Complete_18_to_65.y - Series_Complete_18_to_65.x) %>%
  filter(Series_Complete_18_to_65 != 0)
vaccineUptakeResiduals <- subset(vaccineUptakeResiduals, select = c(Date, Recip_State, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
vaccineUptakeResiduals$FIPS <- vaccineUptakeResiduals$Recip_State

# merge vaccineUptakeCounties with the residuals and create month/year columns
vaccineUptakeWithAllCauseResiduals <- rbind(allCauseUptakeCounties, vaccineUptakeResiduals) %>%
  mutate(Year = year(Date), Month = month(Date, label = FALSE)) %>%
  mutate(Year = as.character(Year)) %>% mutate(Month = as.character(Month))
# filter vaccineUptakeWithAllResiduals to only include the last entry in a given month/year combo for each county and residual
vaccineUptakeWithAllCauseResidualsCut <- vaccineUptakeWithAllCauseResiduals %>%
  mutate(month = format(Date, "%Y-%m")) %>%
  group_by(FIPS, month) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  subset(select = -c(month))
vaccineUptakeWithAllCauseResidualsCut$Month <- as.numeric(vaccineUptakeWithAllCauseResidualsCut$Month)
vaccineUptakeWithAllCauseResidualsCut$Month <- sprintf("%02d", vaccineUptakeWithAllCauseResidualsCut$Month)

# merge allCause mortality dataset with allCause vaccineUptake
result <- allCauseCountyWithResiduals %>%
  left_join(vaccineUptakeWithAllCauseResidualsCut, by = c("County.Code" = "FIPS", "Year", "Month")) %>%
  select(County, County.Code, Administered_Dose1_18_to_65, Series_Complete_18_to_65, Month.Code, Deaths, Month, Year)
result[is.na(result)] <- 0

# export result csv
write.csv(result, "~/Desktop/Research/allCauseWithVaccineUptake.csv")
write.csv(vaccineUptakeWithAllCauseResiduals, "~/Desktop/Research/allCauseUptakeResiduals.csv")


# finding circulatory residual values for vaccineUptakeCounties
# subset vaccineUptakeCounties by filtering out counties that are not represented in the cirulatory mortality data and do not have values in all months
circulatoryUptakeCounties <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% circulatoryCountyWithResiduals$County.Code,]
circulatoryUptakeCounties <- circulatoryUptakeCounties[circulatoryUptakeCounties$FIPS %in% vaccineFIPS,]

# calculating administered dose 1 and series complete values for total circulatory counties
circulatoryVaccineUptakeResidualMerging <- subset(circulatoryUptakeCounties, select = -FIPS) %>%
  group_by(Recip_State, Date) %>%
  summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65), Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))
# calculating administered dose 1 and series complete values for non residual circulatory counties
circulatoryCountiesMerging <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% circulatoryAllYears$County.Code,] %>%
  group_by(Recip_State, Date) %>%
  summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65),Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))

# calculating ciruclatoryVaccineUptakeResiduals by subtracting total circulatory values and non residual circulatory values
circulatoryVaccineUptakeResiduals <- left_join(circulatoryVaccineUptakeResidualMerging, circulatoryCountiesMerging, by = c("Recip_State", "Date")) %>%
  mutate(Administered_Dose1_18_to_65.y = ifelse(is.na(Administered_Dose1_18_to_65.y), 0, Administered_Dose1_18_to_65.y), Administered_Dose1_18_to_65 = Administered_Dose1_18_to_65.y - Administered_Dose1_18_to_65.x) %>%
  mutate(Series_Complete_18_to_65.y = ifelse(is.na(Series_Complete_18_to_65.y), 0, Series_Complete_18_to_65.y), Series_Complete_18_to_65 = Series_Complete_18_to_65.y - Series_Complete_18_to_65.x)
circulatoryVaccineUptakeResiduals <- subset(circulatoryVaccineUptakeResiduals, select = c(Date, Recip_State, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
circulatoryVaccineUptakeResiduals$FIPS <- circulatoryVaccineUptakeResiduals$Recip_State

# merging vaccineUptakeResiduals with circulatory vaccine uptake
vaccineUptakeWithCirculatoryResiduals <- rbind(circulatoryUptakeCounties, circulatoryVaccineUptakeResiduals) %>%
  mutate(Year = year(Date), Month = month(Date, label = FALSE)) %>%
  mutate(Year = as.character(Year)) %>% mutate(Month = as.character(Month))

# trimming data to only include most recent entry in all month/year combos for all counties and residuals
vaccineUptakeWithCirculatoryResidualsCut <- vaccineUptakeWithCirculatoryResiduals %>%
  mutate(month = format(Date, "%Y-%m")) %>%
  group_by(FIPS, month) %>%
  filter(Date == max(Date)) %>%
  ungroup()

# modifying month column for merge
vaccineUptakeWithCirculatoryResidualsCut$Month <- as.numeric(vaccineUptakeWithCirculatoryResidualsCut$Month)
vaccineUptakeWithCirculatoryResidualsCut$Month <- sprintf("%02d", vaccineUptakeWithCirculatoryResidualsCut$Month)

# merging circulatory mortality set with circulatory vaccine uptake
resultCirculatory <- circulatoryCountyWithResiduals %>%
  left_join(vaccineUptakeWithCirculatoryResidualsCut, by = c("County.Code" = "FIPS", "Year", "Month")) %>%
  select(County, County.Code, Administered_Dose1_18_to_65, Series_Complete_18_to_65, Month.Code, Deaths, Month, Year)
resultCirculatory[is.na(resultCirculatory)] <- 0

# exporting circulatory results and circulatory vaccine uptake
write.csv(resultCirculatory, "~/Desktop/Research/circulatoryWithVaccineUptake.csv")
write.csv(vaccineUptakeWithCirculatoryResiduals, "~/Desktop/Research/vaccineUptakeWithCirculatoryResiduals.csv")

