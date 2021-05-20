#***************************************************************************----
# Climatology ----

#' Monthly stats
#'
#' Compute interannual monthly statistics from a time series
#'
#' @param x data frame, should contain at least two columns named 'month' and 'value'.
#' @param stat function, statistics to be applied.
#' @param ... other arguments passed to function stat.
#' @return A vector of size 12 containing the monthly statistics
#' @examples
#' data(nottem) # 	Average Monthly Temperatures (F) at Nottingham, 1920-1939
#' DF=data.frame(year=rep(1920:1939,each=12),month=1:12,value=nottem)
#' getMonthlyStat(DF)
#' getMonthlyStat(DF,stat=quantile,probs=0.75)
#' @export
getMonthlyStat <- function(x,stat=mean,...){
  out <- rep(NA,12)
  for(i in 1:12){
    mask <- x$month==i & !is.na(x$value)
    out[i] <- stat(x$value[mask],...)
  }
  return(out)
}

#***************************************************************************----
# Change of temporal resolution ----

#' Daily to monthly.
#'
#' Transform a daily time series into a monthly one.
#'
#' @param x data frame, should contain at least three columns named 'day', 'month' and 'value'.
#' @param stat function, statistics to be applied.
#' @param ... other arguments passed to function stat.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item value
#' }
#' @examples
#' DF=cbind(getCalendar(2020),value=rnorm(366))
#' dailyToMonthly(DF)
#' dailyToMonthly(DF,stat=max)
#' dailyToMonthly(DF,stat=quantile,probs=0.75)
#' @export
dailyToMonthly <- function(x,stat=mean,...){
  nIn <- NROW(x)
  nOut <- (x$year[nIn]-x$year[1]+1)*12
  out <- data.frame(year=integer(nOut),month=integer(nOut),value=numeric(nOut))
  k <- 0
  for(year in x$year[1]:x$year[nIn]){
    dim <- daysInMonth(1:12,isLeapYear(year))
    for(i in 1:12){
      mask <- x$year==year & x$month==i & !is.na(x$value)
      value <- ifelse(sum(mask)==dim[i],stat(x$value[mask],...),NA)
      k <- k+1
      out[k,] <- c(year,i,value)
    }
  }
  return(out)
}

#***************************************************************************----
# Variable extraction ----

#' Extract annual variables
#'
#' Extraction of annual variables (mean, extrema, sd, quantiles, durations...)
#' from a daily or monthly time series.
#'
#' @param x data frame, should contain at least two columns named 'month' and 'value'.
#'    If x has a column named 'day', it is interpreted as a daily series.
#' @param hydroYear integer vector, hydro-year definition.
#' @param variables character vector, requested variables.
#'   Available: 'mean','max','min','total','sd'.
#' @param probs numeric vector, probabilities (non-exceedance) for requested quantiles.
#' @param lowT numeric vector, low thresholds for requested below-threshold durations.
#' @param highT numeric vector, high thresholds for requested above-threshold durations.
#' @param events integer vector, with same size as x$value, and typically
#'     resulting from a call to getOTevents: definition of events. This function returns
#'     the annual number of events, and the mean, max, min and duration of each of them.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item variable
#'   \item value
#' }
#' @examples
#' DF=cbind(getCalendar(2001:2020),value=rnorm(7305))
#' # returns nothing - you need to request something!
#' V=getAnnualVariables(DF)
#' # annual mean/max
#' V=getAnnualVariables(DF,variables=c('mean','max'))
#' # seasonal mean/max
#' V=getAnnualVariables(DF,hydroyear=c(12,1,2),variables=c('mean','max'))
#' # seasonal quantiles
#' V=getAnnualVariables(DF,hydroyear=c(12,1,2),probs=c(0.25,0.5,0.75))
#' # seasonal durations below / above threshold
#' V=getAnnualVariables(DF,hydroyear=c(12,1,2),lowT=c(-1,0),highT=1)
#' # Events
#' events=getOTevents(DF$value,threshold=1,minIEduration=5,IEminval=0)
#' V=getAnnualVariables(DF,hydroyear=c(12,1,2),events=events)
#' # Many variables at once
#' V=getAnnualVariables(DF,hydroyear=c(12,1,2),
#'                      variables=c('mean','max','min','total','sd'),
#'                      probs=seq(0.1,0.9,0.1),
#'                      lowT=c(-3,-2,1,0),
#'                      highT=c(0,1,2,3))
#' @export
getAnnualVariables <- function(x,hydroyear=1:12,
                      variables=NULL,
                      probs=NULL,
                      lowT=NULL,
                      highT=NULL,
                      events=NULL
                      ){
  out=c()
  isDaily = 'day' %in% names(x)
  # add hydrological year to daily series
  DF=cbind(x,hydroyear=getHydroYear(x$month,x$year,hydroyear))
  y0 <- min(DF$hydroyear,na.rm=TRUE)
  yn <- max(DF$hydroyear,na.rm=TRUE)
  # number of days in hydroyear - leap years are ignored
  nd <- sum(daysInMonth(months=hydroyear))
  for(year in y0:yn){
    m0 <- !is.na(DF$value) & (DF$month %in% hydroyear) & (DF$hydroyear==year)
    isComplete = ifelse(isDaily,sum(m0)>=nd,sum(m0)==length(hydroyear))
    if(isComplete){
      z=DF$value[m0]
      # Basic variables
      if(length(variables)>0 & !is.null(variables)){
        for(j in 1:length(variables)){
          val=switch(variables[j],
                     mean=mean(z),max=max(z),min=min(z),
                     total=sum(z),sd=sd(z),
                     NA)
          out=rbind(out,data.frame(year=year,variable=variables[j],value=val))
        }
      }
      # Quantiles
      if(length(probs)>0 & !is.null(probs)){
        for(j in 1:length(probs)){
          val=as.numeric(quantile(z,probs[j]))
          out=rbind(out,data.frame(year=year,variable=paste0('q',probs[j]),value=val))
        }
      }
      # Below-threshold durations
      if(length(lowT)>0 & !is.null(lowT)){
        for(j in 1:length(lowT)){
          val=sum(z<=lowT[j])/sum(!is.na(z))
          out=rbind(out,data.frame(year=year,variable=paste0('below',j),value=val))
        }
      }
      # Above-threshold durations
      if(length(highT)>0 & !is.null(highT)){
        for(j in 1:length(highT)){
          val=sum(z>=highT[j])/sum(!is.na(z))
          out=rbind(out,data.frame(year=year,variable=paste0('above',j),value=val))
        }
      }
      # events
      if(!is.null(events)){
        zevents=events[m0] # events for the current hydroyear
        totalD=sum(zevents>0)
        if(totalD==0){
          out=rbind(out,data.frame(year=year,variable='eventsN',value=0))
        } else{
          ievents=min(zevents[zevents>0]):max(zevents)
          N=length(ievents)
          out=rbind(out,data.frame(year=year,variable='eventsN',value=N))
          for(j in 1:N){
            m1=(zevents==ievents[j])
            zz=z[m1]
            out=rbind(out,data.frame(year=year,variable='eventsD',value=sum(m1)))
            out=rbind(out,data.frame(year=year,variable='eventsMean',value=mean(zz)))
            out=rbind(out,data.frame(year=year,variable='eventsMax',value=max(zz)))
            out=rbind(out,data.frame(year=year,variable='eventsMin',value=min(zz)))
          }
        }
      }
    }
  }
  return(out)
}

#' Define over-threshold events
#'
#' Definition of over-threshold events, i.e continuous runs of
#' over-threshold values, with successive runs being potentially
#' merged according to some independence constraints.
#' NA's are treated as below-threshold values
#'
#' @param x numeric vector, values.
#' @param threshold numeric, threshold value.
#' @param minIEduration numeric, minimum inter-event duration: two runs separated by
#'     less than minIEduration steps are merged.
#' @param IEminval numeric, minimum inter-event value: if the series does not go
#'     below IEminval between two events, they are merged.
#' @return An integer vector with same size as x, containing the index of
#' over-threshold events (and 0 for values below the threshold).
#' @examples
#' x <- as.numeric(arima.sim(n=365,list(ar=c(0.9))))
#' # No independence constraint
#' events <- getOTevents(x=x,threshold=2)
#' plot(x,type='l',col='gray')
#' for(i in 1:max(events)){foo=x;foo[events!=i]=NA;points(foo,col=i,pch=19)}
#' # At least 5 time steps between events
#' events <- getOTevents(x=x,threshold=2,minIEduration=5)
#' plot(x,type='l',col='gray')
#' for(i in 1:max(events)){foo=x;foo[events!=i]=NA;points(foo,col=i,pch=19)}
#' # At least 5 time steps between events and values go back below 0
#' events <- getOTevents(x=x,threshold=2,minIEduration=5,IEminval=0)
#' plot(x,type='l',col='gray')
#' for(i in 1:max(events)){foo=x;foo[events!=i]=NA;points(foo,col=i,pch=19)}
#' @export
getOTevents <- function(x,threshold,minIEduration=0,IEminval=threshold){
  # replace NA's by -Inf
  X <- x;X[is.na(x)] <- (-Inf)
  # get runs of threshold exceedance
  mask <- (X>threshold)
  xs <- rle(mask)
  n <- length(xs$lengths)
  indices <- c(0,cumsum(xs$lengths))
  # determine which events should be merged according to constraints
  merger <- rep(FALSE,n)
  if(n>1){
    for(i in 2:n){
      if(xs$values[i]==TRUE){ # threshold excess
        IEduration <- xs$lengths[i-1] # duration of previous inter-event (IE)
        indx <- (indices[i-1]+1):indices[i] # indices of previous IE
        IEmin <- min(X[indx]) # min value reached during previous IE
        # merge if IE too short or min IE value too high
        merger[i] <- (IEduration < minIEduration | IEmin >IEminval)
      }
    }
  }
  # Compute the vector of event indices (0 for IE or NA)
  eventIndx <- rep(0,NROW(X))
  k <- 0
  for(i in 1:n){
    if(xs$values[i]==TRUE){ # threshold excess
      indx <- (indices[i]+1):indices[i+1] # indices of current event
      if(!merger[i]){k <- k+1} # increment counter unless event should be merged with previous one
      eventIndx[indx] <- k
    }
  }
  return(eventIndx)
}
