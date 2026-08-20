#***************************************************************************----
# Spectral Analysis, Fourier etc.  ----

#' Periodogram
#'
#' Compute a periodogram from a daily time series
#'
#' @param x numeric vector, data, assumed to represent daily values.
#'     WARNING: the time step is assumed to be daily and constant across the whole series.
#'     This implies in particular that time steps with missing values should not be removed,
#'     but should rather be assigned a NA value. NA values will be internally replaced with
#'     the mean of x, which is probably a poor approach when many values are missing.
#' @return A periodogram object, which is just a data frame with columns: frequency, period_days, period_years and power.
#' @examples
#' # 40 years of time steps
#' x=1:(365.25*40)
#' # Mix of 3 frequencies
#' y=sin(2*pi*x/365.25)+0.7*sin(2*pi*x/(0.5*365.25))+0.5*sin(2*pi*x/(6*365.25))
#' plot(x,y,type='l')
#' # Get periodogram and plot it
#' per=getPeriodogram(y)
#' plot(per$period_year,per$power,log='x',type='l')
#' @export
#' @importFrom TSA periodogram
getPeriodogram <- function(x){
  # Missing values not handled and are just replaced with the mean
  x[is.na(x)]=mean(x,na.rm=TRUE)
  # Compute periodogram using TSA package
  per=TSA::periodogram(x,log='no',plot=FALSE)
  # Get frequencies and periods
  freq=per$freq
  days=1/freq # period in days
  years=days/365.25 # period in years
  # Assemble data frame and return
  out=data.frame(frequency=freq,period_days=days,period_years=years,power=per$spec)
  class(out) <- c('periodogram','data.frame')
  return(out)
}

#' Plot Periodogram
#'
#' Plot a periodogram resulting from a call to function getPeriodogram
#'
#' @param x periodogram object, periodogram computed by function [getPeriodogram()].
#' @param verticalLines numeric vector, periods (in years) where vertical lines are drawn
#' @param ... arguments passed to other methods
#' @return a ggplot of the periodogram.
#' @examples
#' # 40 years of time steps
#' x=1:(365.25*40)
#' # Mix of 3 frequencies
#' y=sin(2*pi*x/365.25)+0.7*sin(2*pi*x/(0.5*365.25))+0.5*sin(2*pi*x/(6*365.25))
#' plot(x,y,type='l')
#' # Get periodogram and plot it
#' per=getPeriodogram(y)
#' plot(per,verticalLines=c(0.5,1,6))
#' @export
#' @import ggplot2
plot.periodogram <- function(x,verticalLines=c(0.5,1,5,10),...){
  vlines=data.frame(x=verticalLines)
  g=ggplot()+
    geom_vline(data=vlines,aes(xintercept=.data$x,col=factor(.data$x)))+
    scale_color_brewer('period [year]',palette='Pastel1')+
    geom_line(data=x,aes(.data$period_years,.data$power))+
    scale_x_log10()+
    labs(x='Period [year]',y='Power')+
    theme_bw()+theme(panel.grid=element_blank())
  return(g)
}
