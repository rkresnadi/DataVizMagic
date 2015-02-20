library(ggplot2)
library(sqldf)
library("lubridate")

#Set workging directory

setwd("/Users/kresnadi/Documents/Northwestern/predict 490/GroupProject")

chi_inspections <- read.csv(file = "CHI_Food_Inspections.csv")
attach(chi_inspections)
unique(chi_inspections$Facility.Type)

chi_inspections.tmp <- subset(chi_inspections, subset = (Facility.Type == "Restaurant"))
ggplot.object <- ggplot(data=chi_inspections.tmp) + aes(x=Results, fill = Results) + geom_bar()   + labs(y = "Number of Inspections") + ggtitle("Restaurant Inspection Results")
print(ggplot.object)

pdf(file = "Restaurant_Inspection_Results.pdf", width = 11, height = 8.5)
print(ggplot.object)
dev.off()

#NYC dataset

nyc_inspections <- read.csv(file = "NYC_Food_Inspections.csv")

colnames(nyc_inspections) <- gsub("\\.","_",colnames(nyc_inspections))
nyc_inspections$date <- as.Date(nyc_inspections$INSPECTION_DATE,"%m/%d/%Y")
nyc_inspections$yr <- year(nyc_inspections$date)
nyc_inspections$mo <- month(nyc_inspections$date)
nyc_inspections$yrmo <- paste(nyc_inspections$yr,nyc_inspections$mo, sep = "-")

head(nyc_inspections)
attach(nyc_inspections)


aggdata <-aggregate(nyc_inspections, by=list(CUISINE_DESCRIPTION,CRITICAL_FLAG), 
                    FUN=length)
aggdataTotCuisine <-aggregate(nyc_inspections, by=list(CUISINE_DESCRIPTION), 
                    FUN=length)
aggdatacombine <- merge(aggdata[,1:3],aggdataTotCuisine[,1:2], by="Group.1")
aggdatacombine$pct <-aggdatacombine$CAMIS.x / aggdatacombine$CAMIS.y
attach(aggdatacombine)
aggdatacombine <- aggdatacombine[order(Group.1,Group.2),]

aggbyyrbiz <- sqldf("select yr as year, CAMIS, count(*) as mycount from nyc_inspections where yrmo <> '1900-1' and CRITICAL_FLAG = 'Critical' and 
                      yr in (2010,2011,2012,2013,2014)  group by year , CAMIS")

aggbyyrmean <-sqldf("select year, avg(mycount) as avgcount from aggbyyrbiz group by year")

myplot <- ggplot(aggdatacombine, aes(x=Group.1, y=pct, fill=Group.2)) + geom_bar(stat="identity") + coord_flip() + labs(x="Cuisine") + labs(y="Critical Flag Percentages")
myplot1 <- ggplot(aggbyyrmean, aes(x=year, y=avgcount)) + geom_line() + labs(y="Avg. Critical Violations")

# print to pdf file for use in many applications
pdf(file = "NYCViolations.pdf", width = 8.5, height = 11)
print(myplot)
print(myplot1)
dev.off()



