#~******************************************************************************
#~* PURPOSE: A set of basic functions for handling DAILY time series, including
#~* read/write utilities & basic stats (seasonnal, annual, interannual stats)
#~* 
#~* For the moment, NA are not properly handled => these functions are devoted 
#~* to the treatment of RECTANGULAR and COMPLETE datasets (typically, reanalyses 
#~* or model outputs), and are therefore unlikely suitable for station data...
#~******************************************************************************
#~* PROGRAMMER: Ben Renard, Irstea Lyon
#~******************************************************************************
#~* CREATED/MODIFIED: Created 17/03/2014
#~******************************************************************************
#~* CONTENTS
#~*		1. read.table_NoDate: Read a data matrix where the time variable is not
#~*		   explicit, but first date is known                
#~*		2. GetStat.season: General function for computing seasonal statistics
#~*		   (where "season" is user-defined), annually or interannually
#~*		3. XXX:
#~******************************************************************************
#~* REF.:
#~*
#~*
#~******************************************************************************
#~* 2DO LIST: Treatment of missing values - completely ignored so far.
#~*
#~*
#~******************************************************************************
#~* COMMENTS: Because these functions are devoted to DAILY series, they are 
#~* mostly based on data frames combined with the dplyr package. zoo objects  
#~* would probably be better for for subdaily and/or irregular series
#~******************************************************************************


read.table_NoDate<-function(file,firstDate,step='daily',...){
#^******************************************************************************
#^* PURPOSE: Read a data matrix where the time variable is not explicit, but
#^* first date and time step are known
#^******************************************************************************
#^* PROGRAMMER: Ben Renard, Irstea Lyon
#^******************************************************************************
#^* CREATED/MODIFIED: Created 17/03/2014
#^******************************************************************************
#^* IN
#^*		1. file [string], path to data file
#^*		2. firstDate [date], date corresponding to the first row
#^*		3. step [string], time stepping
#^*		4. ..., any other argument to be passed to function read.table
#^* OUT
#^*   A data frame where the first column is named "Time" and contains the date,
#^**	  and other columns contain the data read in the file
#^******************************************************************************
#^* REF.:
#^******************************************************************************
#^* 2DO LIST: implement other time stepping (weekly, monthly, etc.)
#^******************************************************************************
#^* COMMENTS:
#^******************************************************************************
X=read.table(file=file,...)
n=nrow(X)
if(step=='daily'){time=firstDate+0:(n-1)
} else{time=matrix(NA,n,1)}
z=cbind(Time=time,X)
return(z)}

GetStat.season<-function(X,funk=mean,season=1:12,interannual=F,do.plot=F,lwd=1){
#^******************************************************************************
#^* PURPOSE: Compute seasonal stat
#^******************************************************************************
#^* PROGRAMMER: Ben Renard, Irstea Lyon
#^******************************************************************************
#^* CREATED/MODIFIED: Created 17/03/2014
#^******************************************************************************
#^* IN
#^*		1. X [data frame], with one column named 'Time'
#^*		2. funk [function], function to apply on all data of the season
#^*		3. season [matrix], definition of season (e.g. DJF is c(12,1,2); whole 
#^*                       civil year is 1:12; hydro-year is c(9:12,1:8)).
#^*                       see "COMMENTS" below for multi-season usage
#^*		4. interannual [logical]. If TRUE, a single interannual value (for each
#^*                             variable); if FALSE, one value for each year
#^*		5. do.plot [logical]. If TRUE, the resulting data frame is ggplotted
#^*		6. lwd [numeric], line width for plotting
#^* OUT
#^*   A data frame aggregated according to funk/season, annually or interannually
#^******************************************************************************
#^* REF.:
#^******************************************************************************
#^* 2DO LIST:
#^******************************************************************************
#^* COMMENTS: usage of 'season': the number of columns give the length (in month)
#^*           of each season. The number of rows give the number of seasons each 
#^*           year. Examples:          
#^*           season = [12 1 2] => winter stat for each year
#^*           season = [12 1 2 
#^*                     3 4 5 
#^*                     6 7 8 
#^*                     9 10 11] => time series of seasonal stat (4 seasons for each year)
#^*           season = [9 10 11 12 1 2 3 4 5 6 7 8] => annual stat (non-civil year)
#^*           season = transpose([9 10 11 12 1 2 3 4 5 6 7 8]) => time series of monthly stat (12 values per year)
#^*           Convention: 1st month of the season determines the year
#^*           (i.e. december 1960 to february 1961 is DJF 1960)
#^******************************************************************************
if(!is.matrix(season)){z=GetStat.season.engine(X,funk,season,interannual,do.plot);return(z)}
ns=dim(season)[1]
if(ns>1) {z=GetStat.season.engine(X,funk,season[1,],interannual,do.plot=F)
} else {z=GetStat.season.engine(X,funk,season[1,],interannual,do.plot,lwd)}
if(ns==1){return(z)}
z=cbind(season=1,season.name=Num2Str.season(season[1,]),z)
for(i in 2:ns){z0=GetStat.season.engine(X,funk,season[i,],interannual,do.plot=F)
  z0=cbind(season=i,season.name=Num2Str.season(season[i,]),z0)
  z=rbind(z,z0)}
if(do.plot){
  require(ggplot2)
  zm=melt(z,id.vars=c('season','season.name','Year'))
  s.names=Num2Str.season(season)
  p=ggplot(zm)
  if(interannual) {
    p=p+geom_line(aes(x=season,y=value),lwd=lwd)+
      facet_grid(variable~.,scales="free")+scale_x_discrete(breaks=1:ns,labels=s.names)
  } else {
  p=p+geom_line(aes(x=Year,y=value),lwd=lwd)+
      facet_grid(variable~season,scales="free",labeller=mylabel(s.names))
  }
  print(p)
}
return(z)}

mylabel<-function(s.names){
  # function to label the facet_grids
  function(variable,value){
    if(variable=='season'){return(s.names[value])
    } else {return(label_value(variable, value))}
  }  
}

GetStat.season.engine<-function(X,funk=mean,season=1:12,interannual=F,do.plot=F,lwd=1){
#^******************************************************************************
#^* PURPOSE: Compute seasonal stat
#^******************************************************************************
#^* PROGRAMMER: Ben Renard, Irstea Lyon
#^******************************************************************************
#^* CREATED/MODIFIED: Created 17/03/2014
#^******************************************************************************
#^* IN
#^*		1. X [data frame], with one column named 'Time'
#^*		2. funk [function], function to apply on all data of the season
#^*		3. season [vector], definition of season (e.g. DJF is c(12,1,2); whole civil year is 1:12; hydro-year is c(9:12,1:8))
#^*		4. interannual [logical]. If TRUE, a single interannual value (for each variable); if FALSE, one value for each year
#^*		5. do.plot [logical]. If TRUE, ggplot the resulting data frame
#^*		6. lwd [numeric], line width for plotting
#^* OUT
#^*   A data frame aggregated according to funk and season
#^******************************************************************************
#^* REF.:
#^******************************************************************************
#^* 2DO LIST:
#^******************************************************************************
#^* COMMENTS: this is a subfunction not meant to be used outside of this file
#^* You should rather use GetStat.season, which provides a more general interface
#^******************************************************************************
require(dplyr);require(reshape2)
if(!'Time'%in%names(X)) {warning('GetStat.season:Fatal:One column of X should be named "Time" and contain dates');return(NULL)}
Z=mutate(X,
  dummyMonth=as.numeric(format(Time,'%m')),
  dummySeason=dummyMonth%in%season,
  dummyLag=as.numeric(dummyMonth<season[1]),
  Year=if(interannual){0} else {as.numeric(format(Time,'%Y'))-dummyLag})
Z=filter(Z,dummySeason)
Z=select(Z,-c(Time,dummyMonth:dummyLag))
Zm=melt(Z,id.vars='Year')
z=Zm%.%group_by(Year,variable)%.%summarize(N=n(),value=funk(value))
if(do.plot){
  require(ggplot2)
  p=ggplot(z)+geom_line(aes(x=Year,y=value),lwd=lwd)+
    facet_grid(variable~.,scales="free",)
  print(p)
}
return(dcast(z,Year+N~variable))}

Num2Str.season<-function(x){
#^******************************************************************************
#^* PURPOSE: transforms [12 1 2] into string 'DJF'
#^******************************************************************************
#^* PROGRAMMER: Ben Renard, Irstea Lyon
#^******************************************************************************
#^* CREATED/MODIFIED: Created 18/03/2014
#^******************************************************************************
#^* IN
#^*		1. x [matrix], season matrix as numerics
#^* OUT
#^*   [string] season matrix as string
#^******************************************************************************
sl=c('J','F','M','A','M','J','J','A','S','O','N','D')
if(!is.matrix(x)){return(paste(sl[x],sep='',collapse=''))}
ns=dim(x)[1];z=c()
for (i in 1:ns){z=c(z,paste(sl[x[i,]],sep='',collapse=''))}
return(z)}