#~******************************************************************************
#~* PURPOSE OF THIS MODULE:a fancy representation of scatterplot matrixes, with scatterplots
#~*          on the lower diagonal, color-coded correlations on the upper diagonal
#~*          and either names or an histogram on the diagonal
#~******************************************************************************
#~* PROGRAMMER: Benjamin Renard, Irstea Lyon
#~******************************************************************************
#~* CREATED/MODIFIED: 25/11/2016
#~******************************************************************************
#~* CONTENTS
#~*    1. FancyScatterplotMatrix: Main function.
#~*    2. plotBkg: internal function, do not use.
#~******************************************************************************
#~* REF.: 
#~******************************************************************************
#~* 2DO LIST: 
#~******************************************************************************
#~* COMMENTS: 
#~******************************************************************************

FancyScatterplotMatrix<-function(df,
                                 diag="name",diag.cex=1,hist.col="gray",
                                 pch=19,smooth=F,pt.col="black",
                                 cor.cex=1,colors=c("blue","white","red"),stretch=1,mincor=0.2,
                                 show.axis=F,nbin = 64){
  #^******************************************************************************
  #^* PURPOSE: a fancy representation of scatterplot matrixes, with scatterplots
  #^*          on the lower diagonal, color-coded correlations on the upper diagonal
  #^*          and either names or an histogram on the diagonal
  #^******************************************************************************
  #^* PROGRAMMER: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 25/11/2016
  #^******************************************************************************
  #^* IN
  #^*    1. df, data frame. Numeric values only. 
  #^*    2. diag: what to plot on the diagonal (either "name" or "hist")
  #^*    3. diag.cex: cex for the names on the diagonal
  #^*    4. hist.col: colour of the histograms
  #^*    5. pch: pch of the points in the lower-diagonal scatterplots
  #^*    6. smooth: smooth the scatterplots? True recommended for very large datasets
  #^*    7. pt.col: colour of the points
  #^*    8. cor.cex: cex for the correlation values in the upper diagonal
  #^*    9. colors: several colors defining the colorscheme for correlations
  #^*    10. stretch: stretching factor for the color scheme, see comments below
  #^*    11. mincor: minimum correlation represented by the colorscheme, see comments below
  #^*    12. show.axis, show ticks on the axes?
  #^* OUT
  #^*    1. Nothing - just a plot
  #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* 2DO LIST: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^*    1. stretch: when Stretch=1, the first color corresponds to a correlation
  #^*       of -1 and the last color to a correlation of 1.
  #^*       When Strecth<1, the colorscheme is "compacted", i.e. the first and last 
  #^*       colors will be reached for correlations smaller than 1 (in absolute value).
  #^*       Conversely when Strecth>1, the colorscheme is "stretched", i.e. the first 
  #^*       and last colors will never be reached. This results in a "paler" 
  #^*       colorscheme, which is useful for datasets with low correlations.
  #^*    2. mincor: if |cor|<mincor, the cell will remain at the color in the middle
  #^*       of the colorscheme (i.e. white with the default colors). This is useful 
  #^*       if one does not whish to show smallish correlations.
  #^******************************************************************************
  
  # preliminaries
  p=ncol(df)
  name=names(df)
  M=cor(df)
  colormap=colorRamp(colors)
  minval=-1*stretch;maxval=stretch
  par(mfrow=c(p,p),mar=c(0,0,0,0),oma=1+c(1,1,0,0))
  for (i in 1:p){
    for (j in 1:p){
      #-----------------------------------------------------
      # DIAGONAL
      if(i==j) {
        if(diag=="hist") {
          hist(df[,i],col=hist.col,main="",xaxt='no',yaxt='no')          
        } else {
          plot(NA,xlim=c(0,1),ylim=c(0,1),xaxt='no',yaxt='no')
          text(0.5,0.5,name[i],font=2,cex=diag.cex)
        }
        plotBkg(bkg=NA,border="black")
      } else {
        #-----------------------------------------------------
        # UPPER HALF
        if (i<j) {
          plot(NA,xlim=c(0,1),ylim=c(0,1),xaxt='no',yaxt='no')
          # map correlation value into [minval,maxval]
          if(M[i,j]<minval){u=0}
          else if(M[i,j]>maxval) {u=1}
          else {u=(M[i,j]-minval)/(maxval-minval)}
          if(abs(M[i,j])<mincor) {u=0.5}
          plotBkg(bkg=rgb(colormap(u),maxColorValue=255),border="black")
          text(0.5,0.5,sprintf("%.2f",round(M[i,j],2)),font=2,cex=cor.cex)
        } else {
          #-----------------------------------------------------
          # LOWER HALF
          if(smooth){
            smoothScatter(df[,c(i,j)],nrpoints=0,nbin = nbin,
                          colramp = colorRampPalette(c("white", pt.col)),
                          xaxt='no',yaxt='no')
          } else {
            plot(df[,c(i,j)],pch=pch,col=pt.col,xaxt='no',yaxt='no')
          }
          if(show.axis){
            if(i==p) {axis(1)}
            if(j==1) {axis(2)}          
          }
        }
      }
    }
  }
}

plotBkg<-function(bkg="yellow",border="black"){
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bkg,border=border)  
}

