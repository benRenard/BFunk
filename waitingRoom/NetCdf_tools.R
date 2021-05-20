require(RNetCDF)

Monthly_GetSeasonalAverage<-function(ncfile,date0,season,start,end,
                                     indx.t=0,indx.lon=3,indx.lat=2,indx.var=4,
                                     lim.lon=c(-180,180),lim.lat=c(-90,90),
                                     subsampling=c(1,1)){
  # Prelimnaries and basic reading
  nc <- open.nc(ncfile)
  Nt=dim.inq.nc(nc,indx.t)$length
  dates=seq(date0, by = "month", length.out = Nt)
  months=as.POSIXlt(dates)$mon+1
  years=as.POSIXlt(dates)$year+1900
  lat <- var.get.nc(nc,indx.lat)
  lon <- var.get.nc(nc,indx.lon)
  nlon=length(lon)
  nlat=length(lat)
  if(any(lon>180)){
    nu=lon[lon>180]-360
    lon[lon>180]=nu
  }
  
  # Restrict to year of interest
  indx0=which(dates==start)
  indxn=which(dates==end)
  Nt=indxn-indx0+1
  var <- var.get.nc(nc,indx.var,start=c(1,1,indx0),count=c(nlon,nlat,Nt))
  dates=dates[indx0:indxn]
  months=months[indx0:indxn]
  years=years[indx0:indxn]
  
  # subsampling
  var=var[seq(1,length(lon),subsampling[1]),seq(1,length(lat),subsampling[2]),]
  lon=lon[seq(1,length(lon),subsampling[1])]
  lat=lat[seq(1,length(lat),subsampling[2])]
  nlon=length(lon)
  nlat=length(lat)
  
  # Put large negative values to missing (sea ice), and get MV mask
  var[var< -100]<-NA
  mvmask=apply(is.na(var),c(1,2),any)
  
  # Compute seasonal averages
  ny=years[length(years)]-years[1]+1
  saverage=array(NA,c(nlon,nlat,ny))
  for(i in 1:ny){
    mask1=( years == (years[1]+i-1) )
    mask2=is.na(years)
    for(j in 1:length(season)){mask2= mask2 | (months==season[j])}
    mask=mask1&mask2
    saverage[,,i]=apply(var[,,mask],c(1,2),mean)
  }
  
  # unfold to matrix format
  result=matrix(NA,ny,sum(!mvmask))
  xy=matrix(NA,sum(!mvmask),2)
  k=0
  for(i in 1:nlon){
    for(j in 1:nlat){
      if(!mvmask[i,j]){
        if( lon[i]>=lim.lon[1] & lon[i]<=lim.lon[2] & lat[j]>=lim.lat[1] & lat[j]<=lim.lat[2] ){
          k=k+1
          result[,k]=saverage[i,j,]
          xy[k,]=c(lon[i],lat[j])
        }
      }
    }
  }  
  return(list(w=result[,1:k],xy=xy[1:k,]))
}