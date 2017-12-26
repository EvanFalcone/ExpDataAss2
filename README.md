# ExpDataAss2
National Emissions Inventory Analysis

Fine particulate matter (PM2.5) is an ambient air pollutant for which there is strong evidence that it is harmful to human health. In the United States, the Environmental Protection Agency (EPA) is tasked with setting national ambient air quality standards for fine PM and for tracking the emissions of this pollutant into the atmosphere. Approximatly every 3 years, the EPA releases its database on emissions of PM2.5. This database is known as the National Emissions Inventory (NEI). You can read more information about the NEI at the EPA National Emissions Inventory web site.

For each year and for each type of PM source, the NEI records how many tons of PM2.5 were emitted from that source over the course of the entire year. The data that you will use for this assignment are for 1999, 2002, 2005, and 2008.

Review criterialess 
For each question

Does the plot appear to address the question being asked?
Is the submitted R code appropriate for construction of the submitted plot?
Dataless 
The data for this assignment are available from the course web site as a single zip file. The zip file contains two files:

PM2.5 Emissions Data (``𝚜𝚞𝚖𝚖𝚊𝚛𝚢𝚂𝙲𝙲_𝙿𝙼𝟸𝟻.𝚛𝚍𝚜``): This file contains a data frame with all of the PM2.5 emissions data for 1999, 2002, 2005, and 2008. For each year, the table contains number of tons of PM2.5 emitted from a specific type of source for the entire year. 

``𝚏𝚒𝚙𝚜``: A five-digit number (represented as a string) indicating the U.S. county . 
``𝚂𝙲𝙲``: The name of the source as indicated by a digit string (see source code classification table) . 
``𝙿𝚘𝚕𝚕𝚞𝚝𝚊𝚗𝚝``: A string indicating the pollutant . 
``𝙴𝚖𝚒𝚜𝚜𝚒𝚘𝚗𝚜``: Amount of PM2.5 emitted, in tons . 
``𝚝𝚢𝚙𝚎``: The type of source (point, non-point, on-road, or non-road) . 
``𝚢𝚎𝚊𝚛``: The year of emissions recorded . 
Source Classification Code Table (``𝚂𝚘𝚞𝚛𝚌𝚎_𝙲𝚕𝚊𝚜𝚜𝚒𝚏𝚒𝚌𝚊𝚝𝚒𝚘𝚗_𝙲𝚘𝚍𝚎.𝚛𝚍𝚜``): This table provides a mapping from the SCC digit strings in the Emissions table to the actual name of the PM2.5 source. The sources are categorized in a few different ways from more general to more specific and you may choose to explore whatever categories you think are most useful. For example, source “10100101” is known as “Ext Comb /Electric Gen /Anthracite Coal /Pulverized Coal”.

You can read each of the two files using the ``𝚛𝚎𝚊𝚍𝚁𝙳𝚂()`` function in R. For example, reading in each file can be done with the following code: 

``NEI <- readRDS("summarySCC_PM25.rds")`` . 
``SCC <- readRDS("Source_Classification_Code.rds")``
