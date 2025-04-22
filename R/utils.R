#***************************************************************************----
# Miscellaneous utilities ----

#' QR code
#'
#' Get QRcode associated with a URL as a ggplot
#'
#' @param url character string, URL
#' @param colorBkg color, background color
#' @param colorTile color, tile color
#' @return A ggplot
#' @examples
#' getQRcode('https://globxblog.github.io/')
#' @export
#' @importFrom qrcode qr_code
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @import ggplot2
getQRcode <-function(url,colorBkg='white',colorTile='black',alpha=1){
  M=qrcode::qr_code(url)
  n=NCOL(M)
  DF=pivot_longer(as.data.frame(M),cols=all_of(1:n))
  DF=cbind(DF,row=rep(1:n,n),col=rep(n:1,each=n))
  g=ggplot(DF)+geom_raster(aes(row,col,fill=value),alpha=alpha)+
    scale_fill_manual(values=c(colorBkg,colorTile),guide=NULL)+
    coord_equal()+theme_void()+
    theme(plot.background=element_rect(fill=colorBkg,color=colorBkg),legend.position='none')
  return(g)
}
