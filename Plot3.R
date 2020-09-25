##Load the packages we will be using and the data
library(dplyr)
library(tidyverse)
library(ggplot)
library(gridExtra)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI                                           ##to keep raw data untouched
new_NEI$yeardate<- as.factor(NEI$year)

##Graph3: Of the four types of sources, which of them have decreased in emissions in Baltimore City from 1999-2008? Which have increased?
baltimore<-new_NEI %>% filter(fips=="24510")
sum3<- baltimore %>% group_by(type,yeardate) %>% summarize(Total=sum(Emissions))
p32<- sum3 %>% ggplot(aes(yeardate, Total, group=type, color=type)) + 
  geom_point() +geom_line()+
  xlab("YEAR") +
  ylab("PM2.5 Emission")
p32
dev.copy(png, file="plot3.png")
dev.off()


##Using the graph, we can see that in Baltimore City from 1999-2008, Non-point, Non-road, and on-road have overall increased while  point has overall descreased their emissions.
