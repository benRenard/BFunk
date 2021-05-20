require(ggplot2)
require(maps)
world = map_data("world")

grid.expand<-function(lon,lat,z=NULL){
  # expand vectors of nLon longitudes and nLat latitudes into
  # a Nlon*Nlat dataframe composing a regular grid.
  # If the Nlon*Nlat matrix z is provided, it's expanded and added 
  # to the dataframe.
  foo=merge(lon,lat)
  if(is.null(z)){
    DF=data.frame(lon=foo[,1],lat=foo[,2])
  } else {
    DF=data.frame(lon=foo[,1],lat=foo[,2],
                  z=matrix(z,length(lon)*length(lat),1))
  }
  return(DF)
}

#----------------------------------------------------------------------
# "addLayer" functions: add a layer to an existing ggplot
addWorld<-function(map,xlim=c(-180,180),ylim=c(-80,80),col="black"){
  # Add World contours to an existing map
  map=map+geom_polygon(data=world,aes(long,lat,group=group),fill="transparent",color=col,size=0.3)+
    coord_cartesian(xlim=xlim,ylim=ylim)
  return(map)
}

addSurface<-function(map,lon,lat,z,
                   col=c("red","yellow","blue"),
                   lim=c(min(z),max(z))){
  # Get a surface layer, based on an expanded grid (lon,lat) and values z
  foo=data.frame(lon=lon,lat=lat,z=z)
  map=map+geom_tile(data=foo,aes(x=lon,y=lat,fill=z))+
  scale_fill_gradientn(colours=col,limits=lim)
}

addPoints<-function(map,lon,lat,fill="black",
                    col=c("red","yellow","blue"),
                    fill.lim=c(min(fill),max(fill)),
                    shape='circle',size=4){
  # Get a layer made of points, with aes: fill (and later: / size / type)
  pch=switch(shape,circle=21,triangle=24,square=22)
  foo=data.frame(lon=lon,lat=lat,fill=fill)
  if(length(fill)==1){
    map=map+geom_point(data=foo,aes(x=lon,y=lat),shape=pch,size=size,fill=fill)
  } else{
    map=map+geom_point(data=foo,aes(x=lon,y=lat,fill=fill),shape=pch,size=size)+
    scale_fill_gradientn(colours=col,limits=fill.lim)
  }
}