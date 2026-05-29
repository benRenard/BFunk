#***************************************************************************----
# Tools to get info on released tools (e.g. download counts) ----

#' Github downloads
#'
#' Count the number of downloads from GitHub.
#'
#' @param url string, Github API URL to retrieve the desired releases.
#'     Should look something like 'https://api.github.com/repos/repo-name/releases'
#' @return A data frame containing the downloads count for all assets from all releases found in the provided URL.
#' @examples
#' DF=countGithubDownloads('https://api.github.com/repos/BaRatin-tools/BaRatinAGE/releases')
#' plot(DF$published,DF$downloads)
#' @importFrom rjson fromJSON
#' @export
countGithubDownloads <- function(url){
  foo=tryCatch(readLines(url,warn=FALSE),error=function(e){NULL})
  if(is.null(foo)){
    mess=paste0('Error fetching the requested url: ',url)
    stop(mess,call.=FALSE)
  }
  if(length(foo)>1){foo=paste(foo,collapse='')}
  js=rjson::fromJSON(foo)
  if(is.list(js[[1]])){ # multiple releases
    Nr=length(js)
  } else { # single release
    js=list(js)
    Nr=1
  }
  out=data.frame()
  for(i in 1:Nr){
    rel=js[[i]]
    nam=rel$name
    dat=as.POSIXct(rel$published_at,tz='UTC',format='%Y-%m-%dT%H:%M:%S')
    for(j in 1:length(rel$assets)){
      ass=rel$assets[[j]]
      out=rbind(out,data.frame(name=nam,published=dat,asset=ass$name,downloads=ass$download_count))
    }
  }
  return(out)
}
