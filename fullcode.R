##Load the packages we will be using and the data
library(dplyr)
NEI<-readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
new_NEI<- NEI
new_NEI$yeardate<- as.factor(NEI$year)

##Graph1
sum1<-new_NEI %>% group_by(yeardate) %>% summarize(n=n(), count=sum(Emissions))
g1<-plot(sum1$yeardate, sum1$count, type="l", xlab="Year", )
dev.copy(png,file="plot1.png")
dev.off()

##Graph2: Total emissions from PM2.5 for Maryland
p2<-new_NEI %>% filter(fips=="24510")
sum2<- p2 %>% group_by(yeardate) %>% summarize(count=sum(Emissions))
g2<- plot(sum2$yeardate, sum2$count, type="l", xlab="Year")
dev.copy(png, file="plot2.png")
dev.off()

##Graph3:
library(gridExtra)
library(ggplot2)
sum3<- p2 %>% group_by(type,yeardate) %>% summarize(count=sum(Emissions))
p32<- sum3 %>% ggplot(aes(yeardate, count, group=type, color=type)) + geom_point() +geom_line()
dev.copy(png, file="plot2.png")
dev.off()

##Graph 4
library(tidyverse)
scc.coal <- SCC[grep("Fuel Comb.*Coal", SCC$EI.Sector),  ];
scc.coal.list <- unique(scc.coal$SCC);
nei.coal <- subset(NEI, SCC %in% scc.coal.list);
nei.coal <- nei.coal %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions))
nei.coal.total <- nei.coal %>% group_by(year) %>% summarize(Annual.Total = sum(Annual.Total)) %>% mutate(type = "TOTAL");
nei.coal <- nei.coal %>% select(Annual.Total, type, year);
nei.coal <- bind_rows(nei.coal, nei.coal.total);
nei.coal$type <- factor(nei.coal$type, levels = c("TOTAL", "ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")); # Re-order factor levels to they plot in the order we wish
ggplot(nei.coal, aes(x = factor(year), y = Annual.Total, fill = type)) +
  geom_bar(stat = "identity") +
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
  ggtitle(expression(atop("Total Tons of PM"[2.5]*" Emissions in the United States", paste("from Coal Combustion-Related Sources")))) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Dark2") +
  guides(fill = FALSE)

##Graph5
baltimore<-new_NEI %>% filter(fips=="24510")

##Graph6
baltimore<-new_NEI %>% filter(fips=="24510")
LA<- new_NEI %>% filter(fips=="06037")
