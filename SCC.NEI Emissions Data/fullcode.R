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

##Graph2: Have total emissions decreased from Baltimore City Maryland?
##library(dplyr, tidyverse)
##NEI<-readRDS("summarySCC_PM25.rds")
##SCC <- readRDS("Source_Classification_Code.rds")
##new_NEI<- NEI                                           ##to keep raw data untouched
##new_NEI$yeardate<- as.factor(NEI$year)
baltimore<-new_NEI %>% filter(fips=="24510")
sum2<- baltimore %>% group_by(yeardate) %>% summarize(Total=sum(Emissions))
plot(sum2$yeardate, sum2$Total, type="l", xlab="Year", ylab="Total Emissions in Baltimore City")
dev.copy(png, file="plot2.png")
dev.off()

##Graph3:
library(gridExtra)
library(ggplot2)
##library(dplyr)
##NEI<-readRDS("summarySCC_PM25.rds")
##SCC <- readRDS("Source_Classification_Code.rds")
##new_NEI<- NEI                                           ##to keep raw data untouched
##new_NEI$yeardate<- as.factor(NEI$year)
baltimore<-new_NEI %>% filter(fips=="24510")
sum3<- baltimore %>% group_by(type,yeardate) %>% summarize(Total=sum(Emissions))
p32<- sum3 %>% ggplot(aes(yeardate, Total, group=type, color=type)) + 
  geom_point() +geom_line()+
  xlab("YEAR") +
  ylab("PM2.5 Emission")
p32
dev.copy(png, file="plot3.png")
dev.off()

##Graph 4
##library(dplyr, ggplot2, tidyverse)
##NEI<-readRDS("summarySCC_PM25.rds")
##SCC <- readRDS("Source_Classification_Code.rds")
##new_NEI<- NEI                                           ##to keep raw data untouched
##new_NEI$yeardate<- as.factor(NEI$year)
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

##Graph5
##library(dplyr, ggplot2, tidyverse)
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

##Graph6
##library(dplyr, tidyverse)
##library(dplyr)
##NEI<-readRDS("summarySCC_PM25.rds")
##SCC <- readRDS("Source_Classification_Code.rds")
##new_NEI<- NEI                                           ##to keep raw data untouched
##new_NEI$yeardate<- as.factor(NEI$year)

baltimore<- new_NEI %>% filter(fips=="24510")
mv<- SCC[grep("Mobile.*Vehicle", SCC$EI.Sector), ]
mv.scc<-unique(mv$SCC)
mv.nei<- subset(baltimore, SCC %in% mv.scc)

mv.nei<-merge(x=mv.nei, y=mv[, c("SCC","EI.Sector", "SCC.Level.Two", "SCC.Level.Three")], by="SCC")

vtab<- mv.nei %>% group_by(year, SCC.Level.Two) %>% summarize(Total=sum(Emissions))
totaltab<- vtab %>% group_by(year) %>% summarize(Total=sum(Total)) %>% mutate(SCC>Level.Two="Total")
vtab<-rbind(vtab, totaltab) %<% mutate(Location="Baltimore City")
g5<-ggplot(vtab, aes(x=as.factor(year), y=Total,fill=as.factor(SCC.Level.Two))) +
  geom_bar(stat="identity") +
  facet_grid(.~SCC.Level.Two)+
  xlab("Year")+
  ylab("Total PM2.5 Emissions")+
  theme(legend.position="bottom")+
  guides(fill=guide_legend(title="Vehicle Source"))

LA<- new_NEI %>% filter(fips=="06037")
##mv<- SCC[grep("Mobile.*Vehicle", SCC$EI.Sector), ]
##mv.scc<-unique(mv$SCC)                              We can use the same ones from Baltimore because its the same subset
mv.la<- subset(LA, SCC %in% mv.scc)                   ##Use the subset from SCC onto LA now

mv.la<-merge(x=mv.la, y=mv[, c("SCC","EI.Sector", "SCC.Level.Two", "SCC.Level.Three")], by="SCC")

latab<- mv.la %>% group_by(year, SCC.Level.Two) %>% summarize(Total=sum(Emissions))
latotal<- latab %>% group_by(year) %>% summarize(Total=sum(Total)) %>% 
  mutate(SCC.Level.Two="Total")
latab<-rbind(latab, latotal) %>% mutate(Location="LA County")
all<-rbind(vtab,latab)

all$year<-as.factor(all$year)
all$source<-as.factor(all$SCC.Level.Two)
all$Location<-as.factor(all$Location)

g6<- ggplot(data=all, aes(x=year, y=Total, group=Location, col=Location ))+
  geom_point() +
  geom_line() +
  facet_grid(SCC.Level.Two~.)+
  theme(legend.position="bottom")+
  ylab("PM2.5 Emission Totals")

dev.copy(png, file="plot6.png")
dev.off()
