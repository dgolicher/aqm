dt<-function(x) DT::datatable(x, 
                              filter = "top",                         
                              extensions = c('Buttons'), options = list(
                                dom = 'Blfrtip',
                                buttons = c('copy', 'csv', 'excel'), colReorder = TRUE
                              ))

clean <-function(x)as.numeric(gsub("[^0-9.-]","",as.character(x)))

f_met<-function(nm="hurn",skip=7)
{
  URL<-sprintf("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/%sdata.txt",nm) 
  d<-read.table(URL,skip=skip, skipNul = TRUE, fill=TRUE,flush=TRUE)
  names(d)<- c("Year", "Month", "tmax", "tmin",  "af","rain", "sun") 
  d$Year<-clean(d$Year)
  d$tmin<-clean(d$tmin)
  d$tmax<-clean(d$tmax)
  d$af<-clean(d$af)
  d$rain<-clean(d$rain)
  d$sun<-clean(d$sun)
  d$date<-as.Date( paste(d$Year,d$Month , 15 , sep = "/" )  , format = "%Y/%m/%d" )
  d$month<-lubridate:::month(d$date,label=TRUE)
  d$station<-nm
  d
}

