library(lubridate)

# summing totals for non-residual counties
allCauseNonResidualCounties <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% allCauseCountyWithResiduals$County.Code, ]
allCauseNonResidual <- allCauseNonResidualCounties %>%
    subset(select = -FIPS) %>%
    group_by(Recip_State, Date) %>%
    summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65), Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))

allCauseUptakeResiduals <- left_join(vaccineUptakeStates, allCauseNonResidual, by = c("Recip_State", "Date"))
allCauseUptakeResiduals[is.na(allCauseUptakeResiduals)] <- 0

# getting allCause residuals by subtracting state and county totals
allCauseUptakeResiduals <- allCauseUptakeResiduals %>%
  mutate(Administered_Dose1_18_to_65.y = ifelse(is.na(Administered_Dose1_18_to_65.y), 0, Administered_Dose1_18_to_65.y), Administered_Dose1_18_to_65 = Administered_Dose1_18_to_65.x - Administered_Dose1_18_to_65.y) %>%
  mutate(Series_Complete_18_to_65.y = ifelse(is.na(Series_Complete_18_to_65.y), 0, Series_Complete_18_to_65.y),
         Series_Complete_18_to_65 = Series_Complete_18_to_65.x - Series_Complete_18_to_65.y) %>%
  filter(Series_Complete_18_to_65 != 0)
allCauseUptakeResiduals <- subset(allCauseUptakeResiduals, select = c(Date, Recip_State, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
allCauseUptakeResiduals[allCauseUptakeResiduals < 0] <- 0
allCauseUptakeResiduals <- rename(allCauseUptakeResiduals, State = Recip_State)
allCauseUptakeResiduals <- changeToStateNames(allCauseUptakeResiduals)
allCauseUptakeResiduals <- rename(allCauseUptakeResiduals, Recip_State = State)
allCauseUptakeResiduals$FIPS <- allCauseUptakeResiduals$Recip_State

# merge vaccineUptakeCounties with the residuals and create month/year columns
vaccineUptakeWithAllCauseResiduals <- rbind(allCauseNonResidualCounties, allCauseUptakeResiduals) %>%
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

result <- allCauseCountyWithResiduals %>%
  left_join(vaccineUptakeWithAllCauseResidualsCut, by = c("County.Code" = "FIPS", "Year", "Month")) %>%
  select(County, County.Code, Administered_Dose1_18_to_65, Series_Complete_18_to_65, Month.Code, Deaths, Month, Year)
result[is.na(result)] <- 0

circulatoryNonResidualCounties <- vaccineUptakeCounties[vaccineUptakeCounties$FIPS %in% circulatoryCountyWithResiduals$County.Code, ] 
circulatoryNonResidual <- circulatoryNonResidualCounties %>%
  subset(select = -FIPS)
circulatoryNonResidual <- circulatoryNonResidual %>%
  group_by(Recip_State, Date) %>%
  summarise(Administered_Dose1_18_to_65 = sum(Administered_Dose1_18_to_65), Series_Complete_18_to_65 = sum(Series_Complete_18_to_65))

circulatoryUptakeResiduals <- left_join(vaccineUptakeStates, circulatoryNonResidual, by = c("Recip_State", "Date"))
circulatoryUptakeResiduals[is.na(circulatoryUptakeResiduals)] <- 0


# getting circulatory residuals by subtracting stte and county totals
circulatoryUptakeResiduals <- circulatoryUptakeResiduals %>%
  mutate(Administered_Dose1_18_to_65.y = ifelse(is.na(Administered_Dose1_18_to_65.y), 0, Administered_Dose1_18_to_65.y), Administered_Dose1_18_to_65 = Administered_Dose1_18_to_65.x - Administered_Dose1_18_to_65.y) %>%
  mutate(Series_Complete_18_to_65.y = ifelse(is.na(Series_Complete_18_to_65.y), 0, Series_Complete_18_to_65.y),
         Series_Complete_18_to_65 = Series_Complete_18_to_65.x - Series_Complete_18_to_65.y) %>%
  filter(Series_Complete_18_to_65 != 0)
circulatoryUptakeResiduals <- subset(circulatoryUptakeResiduals, select = c(Date, Recip_State, Administered_Dose1_18_to_65, Series_Complete_18_to_65))
circulatoryUptakeResiduals <- rename(circulatoryUptakeResiduals, State = Recip_State)
circulatoryUptakeResiduals <- changeToStateNames(circulatoryUptakeResiduals)
circulatoryUptakeResiduals$FIPS <- circulatoryUptakeResiduals$State
circulatoryUptakeResiduals <- rename(circulatoryUptakeResiduals, Recip_State = State)
circulatoryUptakeResiduals[circulatoryUptakeResiduals < 0] <- 0

# merge vaccineUptakeCounties with the residuals and create month/year columns
vaccineUptakeWithCirculatoryResiduals <- rbind(circulatoryNonResidualCounties, circulatoryUptakeResiduals) %>%
  mutate(Year = year(Date), Month = month(Date, label = FALSE)) %>%
  mutate(Year = as.character(Year)) %>% mutate(Month = as.character(Month))

# filter vaccineUptakeWithCirculatoryResiduals to only include the last entry in a given month/year combo for each county and residual
vaccineUptakeWithCirculatoryResidualsCut <- vaccineUptakeWithCirculatoryResiduals %>%
  mutate(month = format(Date, "%Y-%m")) %>%
  group_by(FIPS, month) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  subset(select = -c(month))
vaccineUptakeWithCirculatoryResidualsCut$Month <- as.numeric(vaccineUptakeWithCirculatoryResidualsCut$Month)
vaccineUptakeWithCirculatoryResidualsCut$Month <- sprintf("%02d", vaccineUptakeWithCirculatoryResidualsCut$Month)

resultCirculatory <- circulatoryCountyWithResiduals %>%
  left_join(vaccineUptakeWithCirculatoryResidualsCut, by = c("County.Code" = "FIPS", "Year", "Month")) %>%
  select(County, County.Code, Administered_Dose1_18_to_65, Series_Complete_18_to_65, Month.Code, Deaths, Month, Year)
resultCirculatory[is.na(resultCirculatory)] <- 0
