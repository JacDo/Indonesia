library(dplyr)

download.file(url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-01-2020.csv",destfile="data/tmp/03-01-2020.csv")
download.file(url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-15-2020.csv",destfile="data/tmp/03-15-2020.csv")
download.file(url="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-31-2020.csv",destfile="data/tmp/03-31-2020.csv")

d <-read.csv("data/tmp/03-01-2020.csv")
sum(d$Confirmed)
i<-which(d$Confirmed!=0)
length(unique(d[,"Country.Region"]))

d <-read.csv("data/tmp/03-15-2020.csv")                
sum(d$Confirmed)
i<-which(d$Confirmed!=0)
length(unique(d[,"Country.Region"]))

d <-read.csv("data/tmp/03-31-2020.csv")                
sum(d$Confirmed)
i<-which(d$Confirmed!=0)
length(unique(d[,"Country_Region"]))



head(d)
names(d)
# i<-which(d$Confirmed!=0)
# table(d$Confirmed)
# length(i)
# 
# table(d$Country_Region)
# names(d)
# table(d$Admin2)
# unique(d$Admin2)
# i<-which(is.na(d$Admin2) & d$Country_Region=="US")
# d[i,]
# i<-which(d$Country_Region=="Canada")
# d[i,]

d <- d%>% group_by(Country.Region) %>% summarise(Confirmed=sum(Confirmed)) %>% data.frame()
nrow(d)
table(d$Confirmed)
