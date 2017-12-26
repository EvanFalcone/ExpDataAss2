library("dplyr")
library("lubridate")
library("ggplot2")

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

pmPerBmore <- df2 %>%
  dplyr::filter(Pollutant == "PM25-PRI", fips == "24510") %>%
  select(Emissions, year) %>%
  group_by(year) %>%
  summarise(sum(Emissions))

deltaEmiss <- data.frame(diff(as.matrix(pmPerBmore)))

plot(pmPerBmore, main="Baltimore City Emissions of PM2.5 from 99-08", xlab="Year", ylab = "Total Emissions (all types)")
png(filename="plot2.png")
dev.off()

# Emissions decreased from '99-'02 by 820.2643 units
# Emissions increased from '02-'05 by 637.4383 units
# Emissions decreased from '05-'08 by 1229.0726 units
# Emissions decreased from '99-08 (overall) by 1411.899 units