# Rename columns and format dates
vaccineUptakeStates <- vaccineUptakeStates %>%
  rename("State" = "Recip_State") %>%
  changeToStateNames() %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Year = year(Date),
         Month = month(Date, label = FALSE),
         month = format(Date, "%Y-%m")) %>%
  group_by(State, month) %>%
  filter(Date == max(Date)) %>%
  ungroup() %>%
  select(-c(month, Date)) %>%
  mutate(Month = ifelse(nchar(Month) == 1, paste0("0", Month), Month))

circulatoryStates <- circulatoryStates %>%
  mutate(Year = substr(Month.Code, 1, 4),
         Month = substr(Month.Code, 6, 7))

allCauseStates <- allCauseStates %>%
  mutate(Year = substr(Month.Code, 1, 4),
         Month = substr(Month.Code, 6, 7))

# Join with vaccine uptake data and fill missing values
circulatoryStatesWithVaccineUptake <- circulatoryStates %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(vaccineUptakeStates, by = c("State", "Year", "Month")) %>%
  replace(is.na(.), 0)

allCauseStatesWithVaccineUptake <- allCauseStates %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(vaccineUptakeStates, by = c("State", "Year", "Month")) %>%
  replace(is.na(.), 0)

# Read and clean population data
statePopulation1819 <- read.delim("~/Desktop/Research/Single-Race Population Estimates 2010-2020 by State and Single-Year Age.txt")
statePopulation2021 <- read.delim("~/Desktop/Research/Single-Race Population Estimates 2020-2021 by State and Single-Year Age.txt")

statePopulationAllYears <- rbind(statePopulation1819, statePopulation2021) %>%
  subset(select = -c(Notes, States.Code, Yearly.July.1st.Estimates.Code)) %>%
  rename(State = States, Year = Yearly.July.1st.Estimates) %>%
  filter(complete.cases(.)) %>%
  mutate(Year = as.character(Year))

# Join with population data
allCauseStatesWithVaccineUptake <- allCauseStatesWithVaccineUptake %>%
  left_join(statePopulationAllYears %>%
              mutate(Year = as.double(Year)), by = c("State", "Year"))

circulatoryStatesWithVaccineUptake <- circulatoryStatesWithVaccineUptake %>%
  left_join(statePopulationAllYears %>%
              mutate(Year = as.double(Year)), by = c("State", "Year"))

# Write output to CSV files
write.csv(circulatoryStatesWithVaccineUptake, "~/Desktop/Research/circulatoryStatesWithVaccineUptake.csv")
write.csv(allCauseStatesWithVaccineUptake, "~/Desktop/Research/allCauseStatesWithVaccineUptake.csv")

# Set working directory to the directory where the CSV files are located
setwd("~/Desktop/Research/2010-2019 County Census by State")

# Get a list of all CSV files in the directory
files <- list.files(pattern = "\\.csv$")
countyPopulation2021 <- read.csv("~/Desktop/Research/cc-est2021-agesex-all.csv")

# Read in all CSV files and store them in a list of data frames
tables <- lapply(files, read.csv, header = TRUE)

# Combine all tables into one dataframe and filter out unnecessary columns and years
countyPopulationAllYears <- do.call(rbind , tables) %>%
  filter(YEAR %in% c(11, 12)) %>%
  rbind(countyPopulation2021) %>%
  subset(select = c(STNAME, CTYNAME, YEAR, AGE18PLUS_TOT, AGE65PLUS_TOT)) %>%
  filter(YEAR %in% c(2, 3, 11, 12))

# Rename year 11 to 2018 and year 12 to 2019
countyPopulationAllYears$YEAR <- countyPopulationAllYears$YEAR %>%
  ifelse(. == 11, 2018, .) %>%
  ifelse(. == 12, 2019, .) %>%
  ifelse(. == 2, 2020, .) %>%
  ifelse(. == 3, 2021, .)


# Calculate the number of people between 18 and 65 years old
countyPopulationAllYears$AGE18TO65 <- countyPopulationAllYears$AGE18PLUS_TOT - countyPopulationAllYears$AGE65PLUS_TOT

# Keep only relevant columns
countyPopulationAllYears <- subset(countyPopulationAllYears, select = c("STNAME", "CTYNAME", "YEAR", "AGE18TO65"))

# read in countyFips dataset
countyFips <- read.csv("~/Desktop/Research/state_and_county_fips_master.csv") %>%
  na.omit()

# Create a data frame with all combinations of fips and YEAR values for merging
countyFipsMerging <- expand.grid(fips = unique(countyFips$fips), YEAR = 2018:2021)
countyFipsMerging <- merge(countyFipsMerging, countyFips[, c("fips", "name", "state")], by = "fips") %>%
  rename(State = state) %>%
  changeToStateNames()
colnames(countyFipsMerging)[2:4] <- c("YEAR", "CTYNAME", "STNAME")

# merge the fips with the population dataframe
countyPopulationAllYears <- merge(countyPopulationAllYears, countyFipsMerging, by = c("CTYNAME", "STNAME", "YEAR"), all.x = TRUE)
countyPopulationAllYears$fips <- as.character(countyPopulationAllYears$fips)
countyPopulationAllYears$YEAR <- as.character(countyPopulationAllYears$YEAR)

# calculating allCause residuals for 18-65 population
countyPopulationAllCauseNonResidualCounties <- countyPopulationAllYears[countyPopulationAllYears$fips %in% result$County.Code, ]
countyPopulationAllCauseNonResidual <- countyPopulationAllCauseNonResidualCounties %>%
  select(-c(CTYNAME, fips)) %>%
  group_by(STNAME, YEAR) %>%
  summarise(AGE18TO65 = sum(AGE18TO65))
countyPopulationAllCauseResidualTotal <- countyPopulationAllYears[countyPopulationAllYears$fips %in% allCauseAllYears$County.Code, ] %>%
  select(-c(CTYNAME, fips)) %>%
  group_by(STNAME, YEAR) %>%
  summarise(AGE18TO65 = sum(AGE18TO65))
countyPopulationAllCauseResiduals <- countyPopulationAllCauseResidualTotal %>%
  left_join(countyPopulationAllCauseNonResidual, by = c("STNAME", "YEAR"))
countyPopulationAllCauseResiduals[is.na(countyPopulationAllCauseResiduals)] <- 0
countyPopulationAllCauseResiduals$AGE18TO65 <- countyPopulationAllCauseResiduals$AGE18TO65.x - countyPopulationAllCauseResiduals$AGE18TO65.y
countyPopulationAllCauseResiduals <- countyPopulationAllCauseResiduals %>%
  left_join(state_lookup, by = c("STNAME" = "State")) %>%
  rename(fips = state_abbreviations) %>%
  subset(select = -c(AGE18TO65.x, AGE18TO65.y))
countyPopulationAllCauseResiduals$CTYNAME <- countyPopulationAllCauseResiduals$fips

# calculating circulatory residuals for 18-65 population
countyPopulationCirculatoryNonResidualCounties <- countyPopulationAllYears[countyPopulationAllYears$fips %in% resultCirculatory$County.Code, ]
countyPopulationCirculatoryNonResidual <- countyPopulationCirculatoryNonResidualCounties %>%
  select(-c(CTYNAME, fips)) %>%
  group_by(STNAME, YEAR) %>%
  summarise(AGE18TO65 = sum(AGE18TO65))
countyPopulationCirculatoryResidualTotal <- countyPopulationAllYears[countyPopulationAllYears$fips %in% circulatoryAllYears$County.Code, ] %>%
  select(-c(CTYNAME, fips)) %>%
  group_by(STNAME, YEAR) %>%
  summarise(AGE18TO65 = sum(AGE18TO65))
countyPopulationCirculatoryResiduals <- countyPopulationCirculatoryResidualTotal %>%
  left_join(countyPopulationCirculatoryNonResidual, by = c("STNAME", "YEAR"))
countyPopulationCirculatoryResiduals[is.na(countyPopulationCirculatoryResiduals)] <- 0
countyPopulationCirculatoryResiduals$AGE18TO65 <- countyPopulationCirculatoryResiduals$AGE18TO65.x - countyPopulationCirculatoryResiduals$AGE18TO65.y
countyPopulationCirculatoryResiduals <- countyPopulationCirculatoryResiduals %>%
  left_join(state_lookup, by = c("STNAME" = "State")) %>%
  rename(fips = state_abbreviations) %>%
  subset(select = -c(AGE18TO65.x, AGE18TO65.y))
countyPopulationCirculatoryResiduals$CTYNAME <- countyPopulationCirculatoryResiduals$fips

# bind residuals with countyPopulation
countyPopulationAllYearsWithAllCauseResiduals <- rbind(countyPopulationAllCauseNonResidualCounties, countyPopulationAllCauseResiduals)
countyPopulationAllYearsWithCirculatoryResiduals <- rbind(countyPopulationCirculatoryNonResidualCounties, countyPopulationCirculatoryResiduals)

# merge county population data with the result dataframes
result <- result %>% left_join(countyPopulationAllYearsWithAllCauseResiduals, by = c("County.Code" = "fips", "Year" = "YEAR"))
result$AGE18TO65[is.na(result$AGE18TO65)] <- 0
resultCirculatory <- resultCirculatory %>% left_join(countyPopulationAllYearsWithCirculatoryResiduals, by = c("County.Code" = "fips", "Year" = "YEAR"))
resultCirculatory$AGE18TO65[is.na(resultCirculatory$AGE18TO65)] <- 0

write.csv(result, "~/Desktop/Research/allCauseWithVaccineUptakeAndPopulation.csv")
write.csv(resultCirculatory, "~/Desktop/Research/circulatoryWithVaccineUptakeAndPopulation.csv")
