#***************************************************************************----
# Utilities for calendar months ----

#' Number of days in a calendar month
#'
#' Return the number of days in given calendar months
#'
#' @param months integer vector, months (between 1 and 12).
#' @param isLeap logical, if TRUE, all Februaries in vector months will have 29 days.
#' @return A vector with same size as months containing the corresponding number of days.
#' @examples
#' daysInMonth()
#' @export
daysInMonth <- function(months=1:12,isLeap=FALSE){
  mdur=c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(isLeap) mdur[2]=mdur[2]+1
  return(mdur[months])
}

#' Month name
#'
#' Return the name of calendar months
#'
#' @param months integer vector, months (between 1 and 12).
#' @param nchar positive integer, month names are trimmed to nchar characters.
#' @return A character vector with same size as months containing the corresponding names.
#' @examples
#' monthName()
#' monthName(nchar=1)
#' monthName(nchar=3)
#' @export
monthName <- function(months=1:12,nchar=NULL){
  out <- c('January','February','March',
           'April','May','June','July',
           'August','September','October',
           'November','December')
  if(!is.null(nchar)) {out <- substr(out,1,nchar)}
  return(out[months])
}

#***************************************************************************----
# Utilities for calendar years ----

#' Hydrological Year
#'
#' Compute hydrological (or meteorological or whatever) year
#'
#' @param months integer vector, months (between 1 and 12).
#' @param years integer vector, same size as months, years.
#' @param hydroYear integer vector, hydro-year definition.
#' @return A vector with same size as years containing the corresponding hydro-year.
#' @examples
#' getHydroYear(months=1:12,years=rep(2020,12),hydroYear=c(9,10,11))
#' getHydroYear(months=1:12,years=rep(2020,12),hydroYear=c(12,1,2))
#' @export
getHydroYear <- function(months,years,hydroYear=1:12){
  hyears <- years
  # months before beginning of the hydroYear: civil year minus one
  mask <- (months<hydroYear[1])
  hyears[mask] <- years[mask]-1
  # month not in hydroYear: NA
  mask <- months %in% hydroYear
  hyears[!mask] <- NA
  return(hyears)
}

#' Leap Year
#'
#' Are these leap years?
#'
#' @param years integer vector.
#' @return A logical vector with same size as years.
#' @examples
#' isLeapYear(c(1900,2000,2010,2011,2012))
#' @export
isLeapYear <- function(years){
  return((years%%4==0 & years%%100!=0) | (years%%400==0))
}

#' Get calendar
#'
#' Get calendar for given civil year(s)
#'
#' @param years integer.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item day
#'   \item month
#'   \item year
#'   \item weekday
#' }
#' @examples
#' getCalendar(2020)
#' @export
getCalendar <- function(years){
  dname <- c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday')
  out <- data.frame(day=integer(),month=integer(),year=integer(),
                    weekday=character(),stringsAsFactors=FALSE)
  for(year in years){
    dim <- daysInMonth(1:12,isLeapYear(year))
    for(i in 1:12){
      wd <- as.POSIXlt(as.Date(paste0(year,'-',i,'-',1:dim[i])))$wday
      out=rbind(out,data.frame(day=1:dim[i],
                               month=rep(i,dim[i]),
                               year=rep(year,dim[i]),
                               weekday=dname[wd+1]
                               ))
    }
  }
  return(out)
}
