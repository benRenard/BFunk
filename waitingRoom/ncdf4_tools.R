require(ncdf4)

Monthly_GetSeasonalAverage<-function(ncfile,date0,season,start,end,
                                     id.t="time",id.lon="lon",id.lat="lat",id.var=NA,
                                     lim.lon=c(-180,180),lim.lat=c(-90,90),
                                     subsampling=c(1,1)){
  # Prelimnaries and basic reading
  nc <- nc_open(ncfile)
  time<-ncvar_get(nc,id.t)
  Nt=length(time)
  dates=seq(date0, by = "month", length.out = Nt)
  months=as.POSIXlt(dates)$mon+1
  years=as.POSIXlt(dates)$year+1900
  lat <- ncvar_get(nc,id.lat)
  lon <- ncvar_get(nc,id.lon)
  nlon=length(lon)
  nlat=length(lat)
  if(any(lon>180)){
    nu=lon[lon>180]-360
    lon[lon>180]=nu
  }
  
  # Restrict to year of interest
  if(any(dates==start)){
    indx0=which(dates==start)
  } else {indx0=1}
  if(any(dates==end)){
    indxn=which(dates==end)
  } else {indxn=length(dates)}
  Nt=indxn-indx0+1
  hasP=F
  for(i in 1:nc$ndims){
    if(nc$dim[[i]]$name=="P"){hasP=T}
  }
  for(i in 1:nc$ndims){
    if(nc$dim[[i]]$name=="depth"){hasP=T}
  }
  if(nc$ndims==4 & hasP){#X,Y,P,T with a single pressure level
    st=c(1,1,1,indx0);co=c(nlon,nlat,1,Nt)
  } else{
    st=c(1,1,indx0);co=c(nlon,nlat,Nt)
  }

  var <- ncvar_get(nc,id.var,start=st,count=co)
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
  # define hydro-year
  Hyear=years
  Hyear[months<season[1]]=Hyear[months<season[1]]-1
  for(m in 1:12){
    if(all(m!=season)){Hyear[months==m]=NA}
  }
  y1=min(Hyear,na.rm=T)
  yn=max(Hyear,na.rm=T)
  ny=yn-y1+1
  saverage=array(NA,c(nlon,nlat,ny))
  # compute average  
  for(i in 1:ny){
     saverage[,,i]=apply(var[,,Hyear==(y1+i-1) & !is.na(Hyear)],c(1,2),mean)
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