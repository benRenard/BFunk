# SREX / AR5
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
                        use=foo$V4[i],polygon=pol,color='white')
}
names(SREXregions) <- foo$V2
# America
h=0.5
SREXregions[[1]]$color=hsv(h,1,1)
SREXregions[[2]]$color=hsv(h,0.5,1)
h=h+0.06
SREXregions[[3]]$color=hsv(h,1,0.5)
SREXregions[[4]]$color=hsv(h,1,1)
SREXregions[[5]]$color=hsv(h,0.5,1)
h=h+0.08
SREXregions[[6]]$color=hsv(h,1,1)
SREXregions[[27]]$color=hsv(h,0.5,1)
h=h+0.08
SREXregions[[7]]$color=hsv(h,0.5,1)
SREXregions[[8]]$color=hsv(h,1,1)
h=h+0.06
SREXregions[[9]]$color=hsv(h,1,0.5)
SREXregions[[10]]$color=hsv(h,1,1)
# EUROPE
h=0.44
SREXregions[[11]]$color=hsv(h,1,0.5)
SREXregions[[12]]$color=hsv(h,1,0.75)
SREXregions[[13]]$color=hsv(h,1,1)
# Africa
h=0.2
SREXregions[[14]]$color=hsv(h,1,0.5)
SREXregions[[15]]$color=hsv(h,1,1)
SREXregions[[16]]$color=hsv(h,0.5,1)
h=h+0.06
SREXregions[[17]]$color=hsv(h,1,1)
# North Asia
h=0.85
SREXregions[[18]]$color=hsv(h,0.75,1)
h=h+0.06
SREXregions[[19]]$color=hsv(h,0.5,1)
SREXregions[[20]]$color=hsv(h,1,0.75)
SREXregions[[21]]$color=hsv(h,1,1)
SREXregions[[22]]$color=hsv(h,0.75,1)
# South Asia
h=0
SREXregions[[23]]$color=hsv(h,1,0.5)
SREXregions[[24]]$color=hsv(h,1,1)
# Oceania
h=0.06
SREXregions[[25]]$color=hsv(h,1,0.75)
SREXregions[[26]]$color=hsv(h,0.5,1)

save(SREXregions,file='SREXregions.RData')

# AR6
foo=read.table('IPCC-WGI-reference-regions-v4_coordinates.csv',sep=',',header=TRUE)
AR6regions=vector('list',NROW(foo))
for(i in 1:length(AR6regions)){
  pol=data.frame()
  for (j in 5:NCOL(foo)){
    if(as.character(foo[i,j])!=''){
      z=read.table(text=as.character(foo[i,j]),sep='|')
      pol=rbind(pol,as.numeric(z))
    }
  }
  names(pol)=c('lon','lat')
  AR6regions[[i]]=list(name=foo$Reference.region.name[i],ID=foo$Acronym[i],
                       originalIndx=i,use=foo$Surface[i],polygon=pol,color='white')
}
names(AR6regions) <- foo$Acronym
# America
h=0.5
AR6regions[[1]]$color=hsv(h,1,0.5)
AR6regions[[2]]$color=hsv(h,1,1)
AR6regions[[3]]$color=hsv(h,0.5,1)
h=h+0.06
AR6regions[[4]]$color=hsv(h,1,0.5)
AR6regions[[5]]$color=hsv(h,1,1)
AR6regions[[6]]$color=hsv(h,0.5,1)
h=h+0.08
AR6regions[[7]]$color=hsv(h,1,0.5)
AR6regions[[8]]$color=hsv(h,1,1)
AR6regions[[9]]$color=hsv(h,0.5,1)
h=h+0.08
AR6regions[[10]]$color=hsv(h,1,0.5)
AR6regions[[11]]$color=hsv(h,1,0.75)
AR6regions[[12]]$color=hsv(h,1,1)
AR6regions[[13]]$color=hsv(h,0.5,1)
h=h+0.06
AR6regions[[14]]$color=hsv(h,1,0.5)
AR6regions[[15]]$color=hsv(h,1,1)
AR6regions[[16]]$color=hsv(h,0.5,1)
# EUROPE
h=0.44
AR6regions[[17]]$color=hsv(h,1,0.5)
AR6regions[[18]]$color=hsv(h,1,0.75)
AR6regions[[19]]$color=hsv(h,1,1)
AR6regions[[20]]$color=hsv(h,0.5,1)
# Africa
h=0.2
AR6regions[[21]]$color=hsv(h,1,0.5)
AR6regions[[22]]$color=hsv(h,1,0.75)
AR6regions[[23]]$color=hsv(h,1,1)
AR6regions[[24]]$color=hsv(h,0.75,1)
AR6regions[[25]]$color=hsv(h,0.5,1)
h=h+0.06
AR6regions[[26]]$color=hsv(h,1,0.5)
AR6regions[[27]]$color=hsv(h,1,1)
AR6regions[[28]]$color=hsv(h,0.5,1)
# North Asia
h=0.85
AR6regions[[29]]$color=hsv(h,1,0.5)
AR6regions[[30]]$color=hsv(h,1,0.75)
AR6regions[[31]]$color=hsv(h,1,1)
AR6regions[[32]]$color=hsv(h,0.75,1)
AR6regions[[33]]$color=hsv(h,0.5,1)
h=h+0.06
AR6regions[[34]]$color=hsv(h,1,0.5)
AR6regions[[35]]$color=hsv(h,1,0.75)
AR6regions[[36]]$color=hsv(h,1,1)
AR6regions[[37]]$color=hsv(h,0.75,1)
AR6regions[[38]]$color=hsv(h,0.5,1)
# South Asia
h=0
AR6regions[[39]]$color=hsv(h,1,0.5)
AR6regions[[40]]$color=hsv(h,1,1)
# Oceania
h=0.06
AR6regions[[41]]$color=hsv(h,1,0.5)
AR6regions[[42]]$color=hsv(h,1,0.75)
AR6regions[[43]]$color=hsv(h,1,1)
AR6regions[[44]]$color=hsv(h,0.75,1)
AR6regions[[45]]$color=hsv(h,0.5,1)

save(AR6regions,file='AR6regions.RData')

# Verify
library(ggplot2)
g=BFunk::getWorldMap()
# g=ggplot()+theme_void()
what=SREXregions #AR6regions
for(i in 1:NROW(what)){
  pol=what[[i]]$polygon
  g=g+geom_polygon(data=pol,aes(lon,lat),fill=what[[i]]$color,alpha=0.89,color='black')+
    annotate('text',x=mean(pol$lon),y=mean(pol$lat),
             label=paste0(what[[i]]$originalIndx,': ',what[[i]]$ID))
}
g
