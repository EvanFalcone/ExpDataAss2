library("dplyr")
library("lubridate")
library("ggplot2")

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

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

## All but type 'POINT' have decreased overall from '99-'08