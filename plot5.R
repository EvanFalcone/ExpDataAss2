library("dplyr")
library("lubridate")
library("ggplot2")

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

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