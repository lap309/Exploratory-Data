##Load the packages we will be using and the data
library(dplyr)
library(tidyverse)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI                                           ##to keep raw data untouched
new_NEI$yeardate<- as.factor(NEI$year)



##Graph2: Have total emissions decreased from Baltimore City Maryland?

baltimore<-new_NEI %>% filter(fips=="24510")
sum2<- baltimore %>% group_by(yeardate) %>% summarize(Total=sum(Emissions))
plot(sum2$yeardate, sum2$Total, type="l", xlab="Year", ylab="Total Emissions in Baltimore City")
dev.copy(png, file="plot2.png")
dev.off()
