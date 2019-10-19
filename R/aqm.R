#' Adds files to the users current home directory from the aqm extdata directory
#'
#' Useful for demonstrating reading and saving data
#' 
#' @param fl file to add. Run aqm_data() to see options
#'
#' @return
#' @export
#'
#' @examples aqm_data()
#' 
#' 
add_file<-function(fl="sleep.csv"){
  
  a<-system.file("extdata", fl, package = "aqm")
  system(sprintf("cp %s %s ",a,fl))
}

aqm_data<-function(){
  dir(system.file("extdata",package = "aqm"))
}

#' Quick data table
#' Wrapper to datatable that adds buttons
#'
#' @param d A dataframe
#'
#' @return
#' @export
#'
#' @examples
dt<-function(d) {DT::datatable(d, 
                              filter = "top",                         
                              extensions = c('Buttons'), options = list(
                                dom = 'Blfrtip',
                                buttons = c('copy', 'csv', 'excel'), colReorder = TRUE
                              ))}

#' Quick clean
#' Strips out non numeric characters
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
clean <-function(x){as.numeric(gsub("[^0-9.-]","",as.character(x)))}


#' Quickly forms dynamite plot
#'
#' @param g0 ggplot object with x and y aesthetics set
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
dynamite<-function(g0, ...){g0 +stat_summary(fun.y=mean,geom="bar", ...) +stat_summary(fun.data=mean_cl_normal,geom="errorbar", ...)}

#' Quick confidence intervals
#'
#' @param g0 ggplot with x and y aesthetics set
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
ci<-function(g0, ...){g0 +stat_summary(fun.y=mean,geom="point",...) +stat_summary(fun.data=mean_cl_normal,geom="errorbar", ...)}

#' Download historical monthly records from met office
#'
#' @param nm Name of the station. Defaults to Hurn
#' @param skip  Number of lines to skip. Defaults to 7
#'
#' @return
#' @export
#'
#' @examples
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


#' rand_likert: Form a vector of likert responses with given probabilities
#'
#' @param n Number of responses required
#' @param p A vector of probabilities. Do not have to sum to 1
#'
#' @return A vector of Likert responses
#' @export
#'
#' @examples
#' 
#' q1<-rand_likert(n=100,p=c(1,1,1,1,1)) # Equal probabilites
#' table(q1)
#' 
rand_likert<-function(n=1000, p=c(1,2,3,5,10))
{
  ## Form a vector of responses
  lscale<-c("Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
  ## Sample 1000 times with replacement and unequal probability
  lik<-sample(lscale,n,replace=TRUE,prob=p)
  ## Turn the response into an ordered factor
  lik<-factor(lik,lscale,ordered=TRUE) 
  lik
}


Xpairs<-function (...) {
  require(mgcv)
  panel.line<-function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                        cex = 1, ...) 
  {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      md <- gam(y ~ s(x,k=3))
    xx <- seq(min(x), max(x), length = 100)
    yy <- predict(md, data.frame(x = xx), se = TRUE, type = "response")
    lines(xx, yy$fit, col = 1)
    lines(xx, yy$fit + 2 * yy$se.fit, col = 3, lty = 2)
    lines(xx, yy$fit - 2 * yy$se.fit, col = 2, lty = 2)
  }
  
  panel.hist<-function (x, ...) 
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
  }
  
  panel.cor<-function (x, y, digits = 2, prefix = "", cex.cor) 
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) 
      cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r * 1.2)
  }
  
  pairs(..., lower.panel = panel.line, upper.panel = panel.cor,  diag.panel = panel.hist)
}

cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

ablines<-function(mod,type="confidence",...){
  a<-names(mod$model)[2] 
  b<-paste("newdata <- data.frame(",a,"=seq(min(",a,"),max(",a,"),l=100))",sep="") 
  eval(parse(text=b))
  a<-predict(mod,newdata,interval=type)
  matlines(newdata[,1],a,...)
}

addRsq<-function(mod,pos="topleft",digits=3){
  a<-round(summary(mod)$adj.r.squared,digits) 
  a<-bquote(R[adj]^2 == .(a)) 
  legend(pos,legend=a,bty="n") 
}

ablines2<-function (mod, ...)
{
  a <- names(mod$model)[2]
  b <- paste("newdata <- data.frame(", a, "=seq(min(", a, "),max(",
             a, "),l=100))", sep = "")
  eval(parse(text = b))
  a <- predict(mod, newdata, type="response",se=TRUE)
  a$upper=a$fit+a$se.fit*2
  a$lower=a$fit-a$se.fit*2
  a<-data.frame(a$fit,a$upper,a$lower)
  matlines(newdata[, 1], a,...)
}


# dune2.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.env.txt', row.names = 1)
# dune2.traits <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.traits.txt', row.names = 1)
# dune2.ell <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/dune2.ell.txt', row.names = 1)
# 
# dune_traits<-dune2.traits
# dune_traits$species<-row.names(dune_traits)
# dune_traits<-dune_traits[,6:1]
# save(dune_traits,file="data/dune_traits.rda")
# write.csv(dune_traits,"inst/extdata/dune_traits.csv",row.names=FALSE)

# library(vegan)
# library(tidyr)
# data(dune)
# # 
# dune$quad<-as.numeric(row.names(dune))
# # 
# dune %>%
#    pivot_longer(cols=-quad,names_to="species",values_to = "abundance")  %>% 
#   filter(abundance>0) -> dune_long
# 
# save(dune_long,file="data/dune_long.rda")
# write.csv(dune_long,"inst/extdata/dune_long.csv",row.names=FALSE)

# data("dune.env")
# dune.env$quad<-1:20
# dim(dune.env)
# dune_env<-dune.env[,6:1]
# 
# save(dune_env,file="data/dune_env.rda")
# write.csv(dune_env,"inst/extdata/dune_env.csv",row.names=FALSE)

# data("dune.taxon")
# dune.taxon$species<-row.names(dune.taxon)
# names(dune.taxon)<-tolower(names(dune.taxon))
# dune_taxon<-dune.taxon[,c(6,1:5)]
# save(dune_taxon,file="data/dune_taxon.rda")
# write.csv(dune_taxon,"inst/extdata/dune_taxon.csv",row.names=FALSE)
