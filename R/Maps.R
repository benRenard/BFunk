#***************************************************************************----
# World mapping utilities ----

#' Get bounding box
#'
#' Get bounding box from a geographical description
#'
#' @param bbox character, bounding box as a geographical description.
#'     Available:
#' \enumerate{
#'   \item 'All','World' (Antartica removed for the latter)
#'   \item Continents: 'Africa','America','Asia', Europe', 'Oceania'.
#'   \item Continental regions: 'SouthAmerica', 'CentralAmerica',
#'       'NorthAmerica', 'WesternEurope', 'Mediterranean'.
#'   \item Oceans: 'Atlantic','Indian','Pacific'.
#'   \item Oceanic regions: 'NorthAtlantic','SouthAtlantic','IndianPacific'.
#'   \item Countries: 'Australia','NewZealand','France'.
#' }
#' @return a vector of size 4 (minlon, maxlon, minlat, maxlat), or NULL
#'     if the region is not available.
#' @examples
#' getBBox('SouthAtlantic')
#' getBBox('Europe')
#' getBBox('Mediterranean')
#' getBBox('NewZealand')
#' @export
getBBox <- function(bbox='World'){
  full=c(-180,190.3,-85.2,83.6) # following ggplot2's map_data("world")
  bb <- switch(bbox,
               All=full,
               World=c(-180,full[2],-58,full[4]), # remove Antartic
               # CONTINENTS
               Africa=c(-21,53,-36,38),
               America=c(-172,-12,-58,full[4]),
               Asia=c(26,full[2],-13,full[4]),
               Europe=c(-25,42,35.5,72),
               Oceania=c(112,225,-48,25),
               # CONTINENTAL REGIONS
               SouthAmerica=c(-85,-33,-58,14),
               CentralAmerica=c(-118,-60,6,35),
               NorthAmerica=c(-170,-50,23,72),
               WesternEurope=c(-12,20,35.5,60),
               Mediterranean=c(-7,42,29,48),
               # OCEANS
               Atlantic=c(-82,25,-58,full[4]),
               Indian=c(25,140,-58,30),
               Pacific=c(118,292,-58,64),
               # OCEANIC REGIONS
               NorthAtlantic=c(-82,0,10,full[4]),
               SouthAtlantic=c(-70,25,-58,10),
               IndianPacific=c(25,292,-58,64),
               # COUNTRIES
               Australia=c(112,155,-45,-10),
               NewZealand=c(166,179,-48,-34),
               France=c(-5,9.8,41.25,51.4),
               NULL)
  return(bb)
}

#' World map
#'
#' Create a World Map as a ggplot object
#'
#' @param fill fill color.
#' @param borderColor border color.
#' @param borderWidth numeric, border width.
#' @param region character, bounding box as a geographical description
#' @param theme ggplot theme
#' @param addFrame logical, add a frame (mostly useful when theme=theme_void())
#' @return a ggplot object
#' @examples
#' getWorldMap(region='SouthAtlantic')
#' getWorldMap(region='Europe')
#' getWorldMap(region='Mediterranean',borderColor='gray50')
#' getWorldMap(region='NewZealand')
#' @export
#' @import ggplot2
getWorldMap <- function(fill='gray20',borderColor=fill,borderWidth=0.1,
                        region='World',theme=theme_void(),addFrame=T
                        ){
  bb <- getBBox(region)
  if(is.null(bb)) bb <- getBBox('All')
  mlat=0.5*(bb[3]+bb[4])

  world = map_data("world")
  if(any(bb>180)){
    w2 <- world
    w2$long=w2$long+360
    w2$group=w2$group+max(world$group)
    world=rbind(world,w2)
  }
  g=ggplot()
  g=g+geom_polygon(data=world,aes(long,lat,group=group),
                   fill=fill,color=borderColor,size=borderWidth)
  g=g+coord_fixed(ratio=getAspectRatio(mlat),xlim=bb[1:2],ylim=bb[3:4],expand=FALSE)
  g=g+theme
  if(addFrame) g=g+theme(panel.border=element_rect(fill=NA))
  return(g)
}

#***************************************************************************----
# Geodesy ----

#' Haversine distance
#'
#' Compute the haversine distance between two (lon,lat) points.
#' Uses Salvador's script in this page:
#' \url{https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula}
#'
#' @param pt1,pt2 numeric vectors of size at least 2 starting with (lon,lat)
#'   coordinates. If a third coordinate is present (e.g. elevation), it
#'   will be ignored.
#' @param R numeric, Earth's radius in km.
#' @return a numeric giving the distance between the two points in km
#' @examples
#' pt1=c(-32.917,151.75) #Newcastle, AU
#' pt2=c(43.2964,5.37) #Marseille, FR
#' haversine(pt1,pt2)
#' @export
haversine <- function(pt1,pt2,R=6371){
  p1=as.numeric(pt1)
  p2=as.numeric(pt2)
  toRad=pi/180
  diff=(p1-p2)*toRad
  d=0.5-cos(diff[2])/2+cos(p1[2]*toRad)*cos(p2[2]*toRad)*(1-cos(diff[1]))/2
  d=2*R*asin(sqrt(d))
  return(d)
}

#' Compute y/x Aspect Ratio
#'
#' Compute the y/x aspect ratio for a map in lon-lat, centered around a given latitude.
#' The ratio is computed as the ratio of distances traveled with a 1 degree move equator-ward and eastward.
#' @param lat numeric, latitude for which the ratio is sought.
#' @param ... other arguments passed to function haversine.
#' @return a numeric giving the y/x aspect ratio
#' @examples
#' getAspectRatio(0)
#' getAspectRatio(50)
#' getAspectRatio(-50)
#' @export
getAspectRatio <- function(lat,...){2
  if(lat>=0){dlat=-1} else {dlat=1}
  dy=haversine(c(0,lat),c(0,lat+dlat),...)
  dx=haversine(c(0,lat),c(1,lat),...)
  r=dy/dx
  return(r)
}
