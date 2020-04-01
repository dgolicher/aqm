jhdata<-function()
{

  require(lubridate)
  require(tidyverse)
  require(RCurl)
  require(ggplot2)
  
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


demographics<-function()
{
  require(tidyverse)
  require(lubridate)
  china<-read.csv("https://www.populationpyramid.net/api/pp/156/2019/?csv=true")
  UK<-read.csv("https://www.populationpyramid.net/api/pp/826/2019/?csv=true")
  italy<-read.csv("https://www.populationpyramid.net/api/pp/380/2019/?csv=true")
  usa<-read.csv("https://www.populationpyramid.net/api/pp/840/2019/?csv=true")
 
  f<-function(x) x %>% separate(Age, into = c("Low", "High"), 
  convert=TRUE) %>% group_by(Low) %>% arrange(Low) %>% summarise(tot=sum(M+F)) %>% 
    mutate(percent=round(100*tot/sum(tot),2)) %>% mutate(cumpercent=cumsum(percent))
  
  d1<-data.frame(country="UK",f(UK) )
  d2<-data.frame(country="china",f(china) )
  d3<-data.frame(country="italy",f(italy) )
  d4<-data.frame(country="usa",f(usa) )
  d<-rbind(d1,d2,d3,d4)
  d
}





