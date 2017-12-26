library("dplyr")
library("lubridate")
library("ggplot2")

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using
# the base plotting system, make a plot showing the total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.

pmPerY <- df2 %>%
  dplyr::filter(Pollutant == "PM25-PRI") %>%
  select(Emissions, year) %>%
  group_by(year) %>%
  summarise(sum(Emissions))

plot(pmPerY, main="All Emissions of PM2.5 from 99-08", xlab="Year", ylab = "Total Emissions (all types)")
png(filename="plot1.png")
dev.off()