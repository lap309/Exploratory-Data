##Load the packages we will be using and the data
library(dplyr)
library(ggplot2)
library(tidyverse)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI                                           ##to keep raw data untouched
new_NEI$yeardate<- as.factor(NEI$year)

##Graph5: How have emissions from motor vehicels changed from 1999-2008 in Baltimore City?

baltimore<-new_NEI %>% filter(fips=="24510")
mv<- SCC[grep("Mobile.*Vehicle", SCC$EI.Sector), ]
mv.scc<-unique(mv$SCC)
mv.nei<- subset(baltimore, SCC %in% mv.scc)
mv.nei<-merge(x=mv.nei, y=mv[, c("SCC","EI.Sector", "SCC.Level.Two", "SCC.Level.Three")], by="SCC")

vtab<- mv.nei %>% group_by(year, SCC.Level.Two) %>% summarize(Total=sum(Emissions))
totaltab<- vtab %>% group_by(year) %>% summarize(Total=sum(Total)) %>% mutate(SCC>Level.Two="Total")
vtab<-rbind(vtab, totaltab)
g5<-ggplot(vtab, aes(x=as.factor(year), y=Total,fill=as.factor(SCC.Level.Two))) +
  geom_bar(stat="identity") +
  facet_grid(.~SCC.Level.Two)+
  xlab("Year")+
  ylab("Total PM2.5 Emissions")+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(title="Vehicle Source"))
g5  
dev.copy(png, file="plot5.png")
dev.off()

##Using the graph, we can see that generally speaking, all emissions from highway vehicles running on gasoline and diesel have decreased overtime
##Thus we can see that total emissions from motor vehicles have descreased since 1999.
