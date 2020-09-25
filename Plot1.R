##Load the packages we will be using and the data
library(dplyr)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI                                           ##to keep raw data untouched
new_NEI$yeardate<- as.factor(NEI$year)

##Graph1: Have total emissions decreased in the US?
sum1<-new_NEI %>% group_by(yeardate) %>% summarize(n=n(), Total=sum(Emissions))
plot(sum1$yeardate, sum1$Total, type="l", xlab="Year", ylab="Total Emissions in US")
dev.copy(png,file="plot1.png")
dev.off()

##Using the graph, we can see that US emissions of PM2.5 have decreased since 1999
