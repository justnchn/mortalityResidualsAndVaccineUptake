library(lubridate)

# Cleaning vaccine uptake data at state level
vaccineUptake <- read.csv("~/Desktop/Research/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")
vaccineUptake$Administered_Dose1_18_to_65 = vaccineUptake$Administered_Dose1_Recip_18Plus - vaccineUptake$Administered_Dose1_Recip_65Plus
vaccineUptake$Series_Complete_18_to_65 = vaccineUptake$Series_Complete_18Plus - vaccineUptake$Series_Complete_65Plus
vaccineUptakeStates <- subset(vaccineUptake, select = c(Date, Location, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
vaccineUptakeStates <- subset(vaccineUptake, select = c(Date, Location, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
vaccineUptakeStates <- rename(vaccineUptakeStates, Recip_State = Location)
vaccineUptakeStates <- removeNonStates(vaccineUptakeStates)

# Cleaning vaccine uptake data at the county level
vaccineUptakeCounties <- read.csv("~/Desktop/Research/COVID-19_Vaccinations_in_the_United_States_County.csv")
vaccineUptakeCounties$Administered_Dose1_18_to_65 <- ifelse(
  is.na(vaccineUptakeCounties$Administered_Dose1_Recip_18Plus) | is.na(vaccineUptakeCounties$Administered_Dose1_Recip_65Plus),
  0,
  vaccineUptakeCounties$Administered_Dose1_Recip_18Plus - vaccineUptakeCounties$Administered_Dose1_Recip_65Plus
)
vaccineUptakeCounties$Series_Complete_18_to_65 <- ifelse(
  is.na(vaccineUptakeCounties$Series_Complete_18Plus) | is.na(vaccineUptakeCounties$Series_Complete_65Plus),
  0,
  vaccineUptakeCounties$Series_Complete_18Plus - vaccineUptakeCounties$Series_Complete_65Plus
)
vaccineUptakeCounties <- subset(vaccineUptakeCounties, select = c(Date, Recip_State, FIPS, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
vaccineUptakeCounties <- removeNonStates(vaccineUptakeCounties)

# Creating vaccineFIPS vector to denote all counties that are have a data point for each month in this time range
vaccineUptakeCounties$Date <- as.Date(vaccineUptakeCounties$Date, format = "%m/%d/%Y")
vaccineUptakeStates$Date <- as.Date(vaccineUptakeStates$Date, format = "%m/%d/%Y")
vaccineFIPS <- vaccineUptakeCounties %>% group_by(FIPS) %>% summarize(months = n_distinct(format(Date, "%m-%Y")))
vaccineFIPS <- vaccineFIPS[vaccineFIPS$months == "28",]
vaccineFIPS <- vaccineFIPS$FIPS

# finding residual values for vaccineUptakeCounties
vaccineUptakeCounties <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS != "UNK",]
vaccineUptakeCounties <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% allCauseCountyWithResiduals$County.Code,]
vaccineUptakeResidualMerging <- subset(vaccineUptakeCounties, select = -FIPS)
vaccineUptakeResidualMerging <- vaccineUptakeResidualMerging %>% group_by(Recip_State, Date) %>% summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65),
                                                                                                           Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))
vaccineUptakeResiduals <- left_join(vaccineUptakeStates, vaccineUptakeResidualMerging, by = c("Recip_State", "Date"))
vaccineUptakeResiduals <- vaccineUptakeResiduals %>% mutate(Administered_Dose1_18_to_65.y = ifelse(is.na(Administered_Dose1_18_to_65.y), 0, Administered_Dose1_18_to_65.y),
                    Administered_Dose1_18_to_65 = Administered_Dose1_18_to_65.x - Administered_Dose1_18_to_65.y) %>%
  mutate(Series_Complete_18_to_65.y = ifelse(is.na(Series_Complete_18_to_65.y), 0, Series_Complete_18_to_65.y),
            Series_Complete_18_to_65 = Series_Complete_18_to_65.x - Series_Complete_18_to_65.y) %>%
  filter(Series_Complete_18_to_65 != 0)
vaccineUptakeResiduals <- subset(vaccineUptakeResiduals, select = c(Date, Recip_State, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
vaccineUptakeResiduals$FIPS <- vaccineUptakeResiduals$Recip_State
vaccineUptakeWithAllCauseResiduals <- rbind(vaccineUptakeCounties, vaccineUptakeResiduals)


# extract year and month from df2's Month.Code column
allCauseCountyWithResiduals <- allCauseCountyWithResiduals %>%
  mutate(Year = substr(Month.Code, 1, 4),
         Month = substr(Month.Code, 6, 7))

vaccineUptakeWithAllCauseResiduals <- vaccineUptakeWithAllCauseResiduals %>%
  mutate(Year = year(Date),
         Month = month(Date, label = FALSE))
vaccineUptakeWithAllCauseResiduals <- vaccineUptakeWithALlCauseResiduals %>%
  mutate(Year = as.character(Year)) %>% mutate(Month = as.character(Month))

vaccineUptakeWithAllCauseResidualsCut <- vaccineUptakeWithAllCauseResiduals %>%
  mutate(month = format(Date, "%Y-%m")) %>%
  group_by(FIPS, month) %>%
  filter(Date == max(Date)) %>%
  ungroup()

vaccineUptakeWithAllCauseResidualsCut <- subset(vaccineUptakeWithAllCauseResidualsCut, select = -c(month))
vaccineUptakeWithAllCauseResidualsCut$Month <- as.numeric(vaccineUptakeWithAllCauseResidualsCut$Month)
vaccineUptakeWithAllCauseResidualsCut$Month <- sprintf("%02d", vaccineUptakeWithAllCauseResidualsCut$Month)

# merge df1 and df2 based on FIPS, Recip_State, Year, and Month columns
result <- allCauseCountyWithResiduals %>%
  left_join(vaccineUptakeWithAllCauseResidualsCut, by = c("County.Code" = "FIPS", "Year", "Month"))

# select the desired columns
result <- result %>%
  select(County, County.Code, Administered_Dose1_18_to_65, Series_Complete_18_to_65, Month.Code, Deaths)
