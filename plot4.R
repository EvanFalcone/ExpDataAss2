library("dplyr")
library("lubridate")
library("ggplot2")

## IMPORT both .rds files:

# Create the data frame as 'df':
df1 <- readRDS("Source_Classification_Code.rds")
df2 <- readRDS("summarySCC_PM25.rds")

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