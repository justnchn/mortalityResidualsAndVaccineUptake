# Set working directory to the directory where the CSV files are located
setwd("~/Desktop/Research/2010-2019 County Census by State")

# Get a list of all CSV files in the directory
files <- list.files(pattern = "\\.csv$")

# Read in all CSV files and store them in a list of data frames
tables <- lapply(files, read.csv, header = TRUE)

combined.df <- do.call(rbind , tables) %>%
  subset(select = c(STNAME, CTYNAME, YEAR, AGE18PLUS_TOT, AGE65PLUS_TOT)) %>%


countyFips <- read.delim("~/Desktop/Research/state_and_county_fips_master.csv")

