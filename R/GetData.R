#***************************************************************************----
# Utilities for getting data (downloading in particular) ----

#' HydroPortail batch downloader
#'
#' Download HydroPortail data from
#' https://data.ofb.fr/catalogue/data-eaufrance/eng/catalog.search#/metadata/cae2a6c7-4429-4c89-9da2-44ada7735520
#'
#' @param saveTo character string, destination folder
#' @param what character string, stations or sites ?
#' @param url_prefix character string, prefix of the download URL
#' @param url_id character string, id of the download URL
#' @param url_fsuffix character string, suffix of the downloaded files (without extension)
#' @return nothing - just write files to disk.
#' @examples
#' dest=tempdir()
#' downloadData_HP(saveTo=dest,url_id='',url_fsuffix='_09')
#' @export
downloadData_HP <- function(saveTo,what=c('stations','sites'),
                            url_prefix='https://bnum.din.gouv.fr/mdrive/index.php/s/',
                            url_id=c('AcdANJytrNy7Hrs','qCcoDTJ9gaALQc2','Y2oAEKmf7BPExyR','RL4eqXnmL3ciZGy','J79dFNWbbZFi6CK','JABp59e9tTySeRR','SaT3wXMkMnCwwrd'),
                            url_fsuffix=c('_09','_AE','_FJ','_PT','_UZ','_KN','_O')){
  w=match.arg(what)
  urls=paste0(url_prefix,url_id,'/download/',w,url_fsuffix,'.tar')
  for (i in 1:length(urls)){
    # Download
    cmd=paste0('wget --quiet --show-progress --progress=bar:force:noscroll --directory-prefix=',saveTo,' ',urls[i])
    system(cmd)
  }
}

#' HydroPortail QJ getter
#'
#' Extract daily streamflow files from the tar files dowloaded with downloadData_HP
#'
#' @param tarDir character string, directory containing the HydroPortail tar files
#' @param saveTo character string, destination folder where QJ files will be saved
#' @param subfolder character string, subfolder of saveTo where QJ files will be saved
#' @return nothing - just write files to disk.
#' @examples
#' dest=tempdir()
#' downloadData_HP(saveTo=dest,url_id='',url_fsuffix='_09')
#' extractQJfilesFromHP(tarDir=dest)
#' @export
extractQJfilesFromHP <- function(tarDir,saveTo=tarDir,subfolder='QJ'){
  if(!exists(file.path(saveTo,subfolder))){
    dir.create(file.path(saveTo,subfolder))
  }
  files=list.files(path=tarDir,pattern='*.tar')
  if(length(files)==0) return()
  for(i in 1:length(files)){
    tarfile=file.path(tarDir,files[i])
    cmd=paste0('tar -tvf ',tarfile)
    foo=system(cmd,intern=TRUE)
    if(length(foo)==0) next
    ix=grep(pattern='debitsjournaliers',x=foo)
    for(j in 1:length(ix)){
      QJfile=strsplit(foo[ix[j]],split=' ')
      QJfile=QJfile[[1]][length(QJfile[[1]])]
      id=strsplit(QJfile,split='/')[[1]][2]
      # Extract QJ tar
      cmd=paste0('tar --extract',' --directory ',saveTo,' --file=',tarfile,' ',QJfile,' --strip-components=2')
      system(cmd)
      # Extract QJ csv from QJ tar
      cmd=paste0('gzip -d',' ',file.path(saveTo,'debitsjournaliers.csv.gz'))
      system(cmd)
      # Rename
      file.rename(file.path(saveTo,'debitsjournaliers.csv'),
                  file.path(saveTo,subfolder,paste0('QJ_',id,'.csv')))
    }
  }
}
