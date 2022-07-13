foo=read.table('referenceRegions.csv',sep=',')
SREXregions=vector('list',NROW(foo))
for(i in 1:length(SREXregions)){
  pol=data.frame()
  for (j in 5:NCOL(foo)){
    if(as.character(foo[i,j])!=''){
      z=read.table(text=as.character(foo[i,j]))
      pol=rbind(pol,as.numeric(z))
    }
  }
  names(pol)=c('lon','lat')
  SREXregions[[i]]=list(name=foo$V1[i],ID=foo$V2[i],originalIndx=foo$V3[i],
                        use=foo$V4[i],polygon=pol)
}
names(SREXregions) <- foo$V2
save(SREXregions,file='SREXregions.RData')

# Verify
library(ggplot2)
g=BFunk::getWorldMap()
for(i in 1:NROW(SREXregions)){
  pol=SREXregions[[i]]$polygon
  g=g+geom_polygon(data=pol,aes(lon,lat),fill='white',alpha=0.5,color='red')+
    annotate('text',x=mean(pol$lon),y=mean(pol$lat),label=SREXregions[[i]]$ID)
}
g
