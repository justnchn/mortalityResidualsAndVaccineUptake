library(dplyr)

# function to remove counties that do not have entries for all months in the dataset
removeMissingCounties <- function(df) {
  dr <- df %>%
    group_by(County, County.Code) %>%
    summarise(n_months = n_distinct(Month.Code)) %>%
    # filter out counties that do not have 48 month entries
    filter(n_months == 48) %>%
    select(-n_months) %>%
    inner_join(df, by = c("County", "County.Code"))
}

# function to sum deaths for each state in a given month
calculateDeathsStateMonth <- function(df) {
  # extract state and month from county and month columns
  df$State <- gsub(".*,\\s+", "", df$County)

  # group data by state and month and sum deaths
  summarizedData <- df %>%
    group_by(State, Month.Code) %>%
    summarize(Deaths = sum(Deaths))

  return(summarizedData)
}

state_lookup <- data.frame(
  state_abbreviations = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN",
                          "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                          "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
                          "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
            "Colorado", "Connecticut", "District of Columbia", "Delaware", "Florida", "Georgia",
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
            "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
            "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
            "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
            "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
            "Washington", "West Virginia", "Wisconsin", "Wyoming")
)


# Function that changes state abbreviations in counties to the full state name
changeToStateNames <- function(df) {

  df <- left_join(df, state_lookup, by = c("State" = "state_abbreviations")) %>%
    mutate(State = ifelse(is.na(State.y), "", State.y)) %>%
    select(-State.y)
}



# function that removes any rows that have empty values in a given column
removeEmptyRows <- function(df, colName) {
  df[!is.na(df[[colName]]) & df[[colName]] != "", ]
}

# function that calculates the state residuals by grouping the sets by state and month
calculateResidual <- function(df1, df2) {
  df <- left_join(df1, df2, by = c("State", "Month.Code"))
  df <- df %>% mutate(Deaths.y = ifelse(is.na(Deaths.y), 0, Deaths.y),
                      Residual = Deaths.x - Deaths.y) %>%
    filter(Residual != 0)
}

# function that sums the deaths in all states by year and inserts a Cause column
sumDeathsByYear <- function(df, cause) {
  # Convert Month column to Date object with year format only
  df$Year <- substr(df$Month.Code, 1, 4)
  # Sum Deaths by State and Year
  df_summary <- aggregate(df$Deaths, by=list(df$State, df$Year), sum)
  names(df_summary) <- c("State", "Year", "Deaths")

  # Add Cause column with specified value
  df_summary$Cause <- cause

  return(df_summary)
}

# function that sums the deaths in each year by cause and adds it to the dataset
insertUsAggregate <- function(df) {
  df_total <- df %>%
    group_by(Year, Cause) %>%
    summarise(Deaths = sum(Deaths)) %>%
    ungroup()
  df_total <- df_total %>%
    mutate(State = "US") %>%
    select(State, everything())
  df_total <- df_total %>%
    select(State, Year, Deaths, Cause)
  rbind(df, df_total)
}

merge_counties <- function(df, fips_codes) {
  # Extract rows with matching FIPS codes
  matching_counties <- df[df$FIPS %in% fips_codes, ]

  # Extract rows without matching FIPS codes
  non_matching_counties <- df[!df$FIPS %in% fips_codes, ]

  # Group non-matching counties by state and date, and sum the doses administered
  non_matching_counties <- non_matching_counties %>%
    group_by(Date, Recip_State) %>%
    summarise(Administered_Dose1_Recip_18Plus = sum(Administered_Dose1_Recip_18Plus, na.rm = TRUE),
              Administered_Dose1_Recip_65Plus = sum(Administered_Dose1_Recip_65Plus, na.rm = TRUE)) %>%
    ungroup()

  non_matching_counties$FIPS <- NA
  non_matching_counties$Recip_County <- NA

  # Combine matching and non-matching counties
  combined_counties <- rbind(matching_counties, non_matching_counties)

  return(combined_counties)
}

removeNonStates <- function(df) {
  state_abbreviations = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN",
                          "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
                          "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
                          "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  df[df$Recip_State %in% state_abbreviations, ]
}

