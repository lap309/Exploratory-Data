##Load the packages we will be using and the data
library(dplyr)
library(tidyverse)
library(ggplot2)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI                                           ##to keep raw data untouched
new_NEI$yeardate<- as.factor(NEI$year)

##Graph5
baltimore<-new_NEI %>% filter(fips=="24510")
mv<- SCC[grep("Mobile.*Vehicle", SCC$EI.Sector), ]
mv.scc<-unique(mv$SCC)
mv.nei<- subset(baltimore, SCC %in% mv.scc)
mv.nei<-merge(x=mv.nei, y=mv[, c("SCC","EI.Sector", "SCC.Level.Two", "SCC.Level.Three")], by="SCC")

vtab<- mv.nei %>% group_by(year, SCC.Level.Two) %>% summarize(Total=sum(Emissions))
totaltab<- vtab %>% group_by(year) %>% summarize(Total=sum(Total)) %>% mutate(SCC>Level.Two="Total")
vtab<-rbind(vtab, totaltab)

##Graph6: Compare emissions form motor vehicles in Baltimore City and LA. Which city has seen greater changes over time?
LA<- new_NEI %>% filter(fips=="06037")
##mv<- SCC[grep("Mobile.*Vehicle", SCC$EI.Sector), ]
##mv.scc<-unique(mv$SCC)                              We can use the same ones from Baltimore because its the same subset
mv.la<- subset(LA, SCC %in% mv.scc)                   ##Use the subset from SCC onto LA now

mv.la<-merge(x=mv.la, y=mv[, c("SCC","EI.Sector", "SCC.Level.Two", "SCC.Level.Three")], by="SCC")

latab<- mv.la %>% group_by(year, SCC.Level.Two) %>% summarize(Total=sum(Emissions))
latotal<- latab %>% group_by(year) %>% summarize(Total=sum(Total)) %>% 
  mutate(SCC.Level.Two="Total")
latab<-rbind(latab, latotal) %>% mutate(Location="LA County")


all<-rbind(vtab,latab)        ##Connect the tables from Baltimore and from LA to answer problem 6

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


##Using the graph we created, we see that LA has a higher emission rate than Baltimore. And over time, Baltimores emissions have not changed very much.
##Contrastingly, we see LA's emissions have changed more, both increasing and decerasing over time.
##Highway vehicles running on gasoline has decreased their emissions since 1999 but highway vehicels running on diesel have increased their emission.
##Between the two, LA's emissions have changed more overtime, and have increased a little bit since 1999 while Baltimore's emissions have decreased since 1999.



