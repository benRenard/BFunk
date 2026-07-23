#***************************************************************************----
# Utilities for getting data (downloading in particular) ----
saveTo='/home/benjamin.renard/Desktop/HydroPortail/July2026'

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
#' downloadHydroPortailData(saveTo=dest,url_id='',url_fsuffix='_09')
#' @export
downloadHydroPortailData <- function(saveTo,what=c('stations','sites'),
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
#
#
# # Extract
# list.
# cmd=paste0('tar -ztf ',saveTo,urls[i])
# system(cmd)
# cmd=paste0('tar --extract --file=',tarfile,' ',year,'/',ncfile,' --directory "" --strip-components=1')
