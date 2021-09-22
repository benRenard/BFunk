#***************************************************************************----
# Utilities for reading daily data ----

#' Read Oz RHN data
#'
#' Read daily data in the format used by the BoM for
#' the Australian Hydrometric Reference Network
#'
#' @param f character string, file.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item day
#'   \item value
#'   \item qcode (quality code)
#' }
#' @examples
#' #
#' @export
read_OzRefNetwork_Q_D <- function(f){
  # f: datafile
  X <- utils::read.table(f,skip=16,header=F,sep=',')
  DF <- data.frame(year=as.numeric(substr(X[,1],1,4)),
                   month=as.numeric(substr(X[,1],6,7)),
                   day=as.numeric(substr(X[,1],9,10)),
                   value=X[,4],qcode=X[,3])
  return(DF)
}

#' Read Oz RHN2 data
#'
#' Read daily data in the format used by the BoM for
#' the Australian Hydrometric Reference Network v2 (467 sites)
#'
#' @param f character string, file.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item day
#'   \item value
#'   \item qcode (quality code)
#' }
#' @examples
#' #
#' @export
read_OzRefNetwork_Q2_D <- function(f){
  # f: datafile
  X <- utils::read.table(f,skip=27,header=F,sep=',')
  DF <- data.frame(year=as.numeric(substr(X[,1],1,4)),
                   month=as.numeric(substr(X[,1],6,7)),
                   day=as.numeric(substr(X[,1],9,10)),
                   value=X[,2],qcode=X[,3])
  return(DF)
}

#' Read Oz ACORN-SAT v2 temperature data
#'
#' Read daily data in the format used by the BoM for
#' the Australian CLimate Observation Reference Network
#'
#' @param f character string, file.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item day
#'   \item value
#' }
#' @examples
#' #
#' @export
read_OzRefNetwork_T_D <- function(f){
  # f: datafile
  X <- utils::read.table(f,skip=2,header=F,sep=',')
  DF <- data.frame(year=as.numeric(substr(X[,1],1,4)),
                   month=as.numeric(substr(X[,1],6,7)),
                   day=as.numeric(substr(X[,1],9,10)),
                   value=X[,2])
  return(DF)
}

#' Read Oz daily precipitation data
#'
#' Read daily data in the format used by the BoM for
#' the Australian Climate Observation Reference Network
#'
#' @param f character string, file.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item day
#'   \item value
#' }
#' @examples
#' #
#' @export
read_OzRefNetwork_P_D <- function(f){
  # f: datafile
  X <- utils::read.table(f,skip=1,header=F)
  DF <- data.frame(year=as.numeric(substr(X[,1],1,4)),
                   month=as.numeric(substr(X[,1],5,6)),
                   day=as.numeric(substr(X[,1],7,8)),
                   value=X[,2])
  # Missing values
  mask <- DF$value>9999
  DF$value[mask] <- NA
  return(DF)
}

#***************************************************************************----
# Utilities for reading monthly data ----

#' Read Oz monthly data
#'
#' Read monthly data in the format used by the BoM for
#' the Australian Climate Observation Reference Network
#'
#' @param f character string, file.
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item value
#' }
#' @examples
#' #
#' @export
read_OzRefNetwork_M <- function(f){
  # f: datafile
  X <- utils::read.table(f,skip=1,header=F)
  DF <- data.frame(year=as.numeric(substr(X[,1],1,4)),
                   month=as.numeric(substr(X[,1],5,6)),
                   value=X[,3])
  # Missing values
  mask <- DF$value>9999
  DF$value[mask] <- NA
  return(DF)
}

#' Read tabular data
#'
#' Read monthly data in a tabular format (year then 12 columns).
#' Only the first 13 columns are considered, anything on the right of the
#' 13th (e.g. an annual value in some datasets) is ignored.
#'
#' @param f character string, file.
#' @param ... other arguments passed to function read.table
#' @return A data frame with the following columns:
#' \enumerate{
#'   \item year
#'   \item month
#'   \item value
#' }
#' @examples
#' #
#' @export
read_tabular_M <- function(f,...){
  # f: datafile
  X <- utils::read.table(f,...)[,1:13]
  DF <- c()
  for(i in 1:NROW(X)){
    DF <- rbind(DF,
            data.frame(year=X[i,1],month=1:12,
                       value=as.numeric(X[i,1+(1:12)])))
  }
  return(DF)
}

