#***************************************************************************----
# Comparing files & folders ----

#' Directory Diff
#'
#' Get a diff between a master and a copy directories, i.e. list files
#' and folders that have been added, removed, sized-changed or
#' os-modified in the last X hours.
#'
#' @param master.dir character string, master directory
#' @param copy.dir character string, copy directory
#' @param hours numeric, modificaton time span
#' @param fileOnly logical, list only files or subfolders as well?
#' @return A list with the following fields:
#' \enumerate{
#'   \item new, list of new files (in master but not in copy)
#'   \item removed, list of removed files (in copy but not in master)
#'   \item sizeChanged, list of size-changed files (in both copy and master but with different sizes)
#'   \item modified, list of modified files (according to OS-specific 'time of last modif', unclear what this does exactly...)
#' }
#' @examples
#' #
#' @export
dirDiff <- function(master.dir,copy.dir,hours=24,fileOnly=TRUE){
  begin=proc.time()
  message(paste('MASTER dir is:',master.dir))
  message(paste('COPY dir is:',copy.dir))
  # list files
  go=proc.time()
  master.files=list.files(master.dir,recursive=TRUE,include.dirs=!fileOnly)
  message(paste('Listing files in MASTER dir [s]:',(proc.time()-go)[3]))
  message(paste('Found',NROW(master.files),'files'))
  go=proc.time()
  copy.files=list.files(copy.dir,recursive=TRUE,include.dirs=!fileOnly)
  message(paste('Listing files in COPY dir [s]:',(proc.time()-go)[3]))
  message(paste('Found',NROW(copy.files),'files'))
  # Get new/removed files
  common=master.files[(master.files %in% copy.files)]
  new=file.path(master.dir,master.files[!(master.files %in% copy.files)])
  removed= file.path(copy.dir,copy.files[!(copy.files %in% master.files)])
  # Get file sizes and get modified sizes
  go=proc.time()
  master.sizes=file.size(file.path(master.dir,common))
  message(paste('Getting file sizes in MASTER dir [s]:',(proc.time()-go)[3]))
  go=proc.time()
  copy.sizes=file.size(file.path(copy.dir,common))
  message(paste('Getting file sizes in COPY dir [s]:',(proc.time()-go)[3]))
  changed=file.path(master.dir,common[master.sizes != copy.sizes])
  # Get files modified in the last XXX hours
  go=proc.time()
  master.modif=file.mtime(file.path(master.dir,master.files))
  message(paste('Getting file modif times in MASTER dir [s]:',(proc.time()-go)[3]))
  now=Sys.time()
  dt=as.double(difftime(now,master.modif,units="hours"))
  modified=file.path(master.dir,master.files[dt<=hours])
  message(paste('Total time [s]:',(proc.time()-begin)[3]))
  return(list(new=new,removed=removed,sizeChanged=changed,modified=modified))
}
