##Load the packages we will be using and the data
library(dplyr)
library(ggplot)
library(tidyverse)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI                                           ##to keep raw data untouched
new_NEI$yeardate<- as.factor(NEI$year)

##Graph 4: How have emissions from coal combusion related sources changed from 1999-2008?

coal<-SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector), ]         ##finds all intances of "Fuel Comb.....Coal" in SCC$E!.Sector column, returns a df of those rows
coal.scc<-unique(coal$SCC)                                  ##find unique values/instances from the coal$SCC column
coal.nei<-subset(NEI, SCC %in% coal.scc)                  ##takes the values from coal$SCC and matches to the NEI table
coal.nei<- coal.nei %>% group_by(type, year) %>% summarize(Total= sum(Emissions))       ##create a chart to find total emission for each group and year
coal.nei.total<- coal.nei %>% group_by(year) %>% summarize(Total= sum(Total)) %>% mutate(type="TOTAL")      ##finds the total for all types by the year
coal.nei.all<-bind_rows(coal.nei, coal.nei.total) 
coal.nei.all$type<- factor(coal.nei.all$type, levels=c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT"))
g4<- ggplot(coal.nei.all, aes(x=factor(year), y=Total, fill=type)) +
  geom_bar(stat="identity") +
  facet_grid(.~type)+
  xlab("Year") +
  ylab("Total Tons of Emissions")
dev.copy(png,file="plot4.png")
dev.off()

##Using the graph, we can see that Non-point emissions increased in 2002, but ultimately decreased in 2008. Overall, comparing with 1999, the emissions have decreased
##The point bar graph also shows a similar pattern, but the difference between 1999 to 2008 is significantly much more of a drastic decrease.
##Combining the two, we see a decrease in total Tons of Emissions from both Point and Non-point types from 1999 to 2008.
