## NATIONAL EMISSIONS INVENTORY ANALYSIS:
setwd("/Users/estevanfalcone/CODING PRACTICE PROJECTS/R Projects/")
library("dplyr")
library("lubridate")
library("ggplot2")

## |------------------------------------------------------------------------------------|

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

## |------------------------------------------------------------------------------------|

## QUESTIONS TO ANSWER:

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using
# the base plotting system, make a plot showing the total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

# 3. Of the four sources indicated by types (point, nonpoint, onroad, nonroad) variable, which
# of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 to make a plot for this question.

# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
# sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

## |------------------------------------------------------------------------------------|

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using
# the base plotting system, make a plot showing the total PM2.5 emission from all sources
# for each of the years 1999, 2002, 2005, and 2008.

head(df2)

pmPerY <- df2 %>%
  dplyr::filter(Pollutant == "PM25-PRI") %>%
    select(Emissions, year) %>%
      group_by(year) %>%
        summarise(sum(Emissions))

plot(pmPerY, main="All Emissions of PM2.5 from 99-08", xlab="Year", ylab = "Total Emissions (all types)")
png(filename="plot1.png")
dev.off()

## |------------------------------------------------------------------------------------|

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

## |------------------------------------------------------------------------------------|

# 3. Of the four sources indicated by types (point, nonpoint, onroad, nonroad) variable, which
# of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 to make a plot for this question.

pmBmoreType <- df2 %>%
  dplyr::filter(Pollutant == "PM25-PRI", fips == "24510") %>%
  select(Emissions, type, year) %>%
  group_by(type, year) %>%
  summarise(sum(Emissions))

ggplot(pmBmoreType, aes(factor(year), pmBmoreType$`sum(Emissions)`, fill = type)) +
  geom_bar(stat = "Identity") +
    facet_grid(.~type, scales = "free", space = "free") +
      labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) +
        labs(title=expression("PM"[2.5]*" Emissions, Baltimore '99-'08 by Type"))

png(filename="plot3.png")
dev.off()

# How to use diff() with a non-numeric column?
# deltaEmiss <- data.frame(diff(as.matrix(pmBmoreType)))

## All but type 'POINT' have decreased overall from '99-'08

## |------------------------------------------------------------------------------------|

# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library("stringr")
# df1$SCC.Level.One <- str_to_title(df1$SCC.Level.One)
# df1$SCC.Level.Three <- str_to_title(df1$SCC.Level.Three)
coalComb <- df1 %>% 
   dplyr::filter(SCC.Level.One %like% "Combustion" & SCC.Level.Three %like% "Coal")
ccDf2 <- subset(df2, df2$SCC %in% coalComb$SCC) %>% 
  select(Emissions, type, year) %>% 
  group_by(type, year) %>% 
  summarise(sum(Emissions))

ggplot(ccDf2, aes(factor(year), ccDf2$`sum(Emissions)`, fill = type)) +
  geom_bar(stat = "Identity") +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) +
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions across US '99-'08"))

png(filename="plot4.png")
dev.off()

## |------------------------------------------------------------------------------------|

# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

onRoadBmore <- df2 %>% 
  dplyr::filter(fips == "24510" & type == "ON-ROAD") %>% 
  select(Emissions, fips, year) %>% 
  group_by(fips, year) %>% 
  summarise(sum(Emissions))

qplot(year, `sum(Emissions)`, data = onRoadBmore, geom = "line") +
  xlab("year") +
  ylab(expression("Total PM"[2.5]*" Emission (10^5 Tons)")) +
  labs(title=expression("Motor Vehicle-Related Emissions in Baltimore City '99-'08"))

png(filename="plot5.png")
dev.off()

## |------------------------------------------------------------------------------------|

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor
# vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater
# changes over time in motor vehicle emissions?

onRoadLa <- df2 %>% 
  dplyr::filter(fips == "06037" & type == "ON-ROAD") %>% 
  select(Emissions, fips, year) %>% 
  group_by(fips, year) %>% 
  summarise(sum(Emissions))

compare <- rbind(onRoadBmore, onRoadLa)

ggplot(compare, aes(factor(year), `sum(Emissions)`, fill = fips)) +
  geom_bar(stat = "Identity") +
  facet_grid(. ~ fips) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) +
  labs(title=expression("Motor Vehicle Emissions - LA (06037) and Baltimore City (24510) '99-'08"))

png(filename="plot6.png")
dev.off()