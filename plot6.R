library("dplyr")
library("lubridate")
library("ggplot2")

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor
# vehicle sources in Los Angeles County, California (fips == 06037). Which city has seen greater
# changes over time in motor vehicle emissions?

onRoadBmore <- df2 %>% 
  dplyr::filter(fips == "24510" & type == "ON-ROAD") %>% 
  select(Emissions, fips, year) %>% 
  group_by(fips, year) %>% 
  summarise(sum(Emissions))

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