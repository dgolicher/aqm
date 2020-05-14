

jhdata<-function(){
  require(lubridate)
  require(tidyverse)
  require(RCurl)
  require(ggplot2) 
  library(DT)
  library(aqm)
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  
  Confirmed<-d
  
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  
  Deaths<-d
  require(tidyverse)
  require(lubridate)
  URL <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  data <- read.csv(text = URL, check.names = F)
  
  pivot_longer(data,cols=5:dim(data)[2],names_to = "Date") ->d
  names(d)<-c("Province","Country","Lat","Long","Date","NCases")
  
  d$Date<-as.Date(d$Date,format="%m/%d/%y")
  Recovered<-d
  
  
  Confirmed %>% group_by(Country, Date) %>% summarise(NCases=sum(NCases)) -> confirmed_country
  Deaths %>% group_by(Country, Date) %>% summarise(NDeaths=sum(NCases)) -> deaths_country
  Recovered %>% group_by(Country, Date) %>% summarise(NRecovered=sum(NCases)) -> recovered_country
  
  confirmed_country %>% left_join(deaths_country,by = c("Country", "Date")) %>% left_join(recovered_country, by = c("Country", "Date")) -> by_country
  by_country%>%arrange(Date) %>% mutate(New_cases = NCases - lag(NCases, default = first(NCases)), NActive=NCases-NDeaths-NRecovered) -> by_country
  by_country
}



