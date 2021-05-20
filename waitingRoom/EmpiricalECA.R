# Functions for performing Explanatory Component Analysis (ECA) in its "empirical" version

EmpECA.estim<-function(Y,Phi){
	#-----------------------------------------------------------------------------------
	# STEP 0: preliminaries
	#-----------------------------------------------------------------------------------
  # Create output list and initialize it
  OUT=list(Tau=NA,Tau.hat=NA,Omega=NA,Y=Y,Ytilde=NA,Phi=Phi)
  # Check sizes
  Ns=length(Phi[1,]);Nt=length(Phi[,1]);Nx=length(Y[1,])
  if(length(Y[,1])!=Nt){warning('Size mismatch [Y,Phi]');return(OUT)}
	#-----------------------------------------------------------------------------------
	# STEP 1: Extract temporal pattern from Y
	#-----------------------------------------------------------------------------------
  # Apply Normal score transformation
  OUT$Ytilde=matrix(NA,Nt,Nx);
  for(i in 1:Nx){OUT$Ytilde[,i]=RandomizedNormalScore(Y[,i])}
    # Extract temporal pattern
  OUT$Tau=matrix(NA,Nt,1);
  Ones=matrix(1,1,Nx)
  OUT$Tau=rowMeans(OUT$Ytilde, na.rm = TRUE) # This assumes independence, conditionnal on tau(t)
  
  # Non-indep case below:
#  # Get correlation matrix and invert it
#  Sigma=cor(OUT$Ytilde,use="na.or.complete")
#  SigmaInv=tryCatch(solve(Sigma),error=function(e){NA})
#  if(any(is.na(SigmaInv))){warning('Cannot invert SigmaY - proceeding by replacing SigmaY by Id, results might be biased')}
#  
#  #for(i in 1:Nt){Z=matrix(OUT$Ytilde[i,],Nx,1);Z[is.na(Z)]=0;OUT$Tau[i]=Ones%*%SigmaInv%*%Z}
#  #OUT$Tau=OUT$Tau/sum(SigmaInv)
##  for(i in 1:Nt){
#    Z=matrix(OUT$Ytilde[i,],Nx,1)
#    mask=!is.na(Z)
#    Sigma_slim=Sigma[mask,mask]    
#    SigmaInv_slim=tryCatch(solve(Sigma_slim),error=function(e){NA})
#    if(any(is.na(SigmaInv))){
#      warning('Cannot invert SigmaY - proceeding by replacing SigmaY by Id, results might be biased')
#      SigmaInv_slim=diag(1,sum(mask),sum(mask))
#    }
#    Z_slim=Z[mask]
#    Ones_slim=matrix(1,1,sum(mask))
#    OUT$Tau[i]=Ones_slim%*%SigmaInv_slim%*%Z_slim
#    OUT$Tau[i]=OUT$Tau[i]/sum(SigmaInv_slim)
#  }

  #-----------------------------------------------------------------------------------
	# STEP 2: Extract associate spatial pattern from Phi
	#-----------------------------------------------------------------------------------
  OUT$Omega=matrix(NA,Ns,1);
  for(i in 1:Ns){OUT$Omega[i]=sum(Phi[,i]*OUT$Tau)}
  OUT$Omega=OUT$Omega/sum(OUT$Tau^2)
 	#-----------------------------------------------------------------------------------
	# STEP 3: Re-estimate Tau (->Tau.hat) based on Phi rather than on Y - for comparing tau/tau.hat
	#-----------------------------------------------------------------------------------
  OUT$Tau.hat=matrix(NA,Nt,1);
  for (i in 1:Nt){OUT$Tau.hat[i]=GetTauFromPhi(Phi[i,],OUT$Omega)}
return(OUT)}

#----------------------------------------------------------------------------------------------------
# Plots from the output of function EmpECA
EmpECA.plot<-function(eca,LatLon,lay=c(2,ceiling(length(eca$Y[1,])/2))){
  require(maps);require(ggplot2);require(corrplot)
  Ns=length(eca$Phi[1,]);Nt=length(eca$Phi[,1]);Nx=length(eca$Y[1,])
  w=matrix(NA,Ns,1);w=eca$Omega;#w[abs(Omega)<0.3]=0
  DF=data.frame(cbind(lon=LatLon$lon,lat=LatLon$lat,w=data.frame(w)))
  DF$lon[DF$lon>180]= -360+DF$lon[DF$lon>180]
  world = map_data("world")
  # Visualize data
  epsi=matrix(NA,Nt,Nx);
  for(i in 1:Nt){epsi[i,]=eca$Ytilde[i,]-eca$Tau[i]}
  X11(title='ECA - Hydrologic data',width=12, height=4.5);par(mfrow=c(1,3))
  matplot(eca$Y,type='b',lty=1,pch=19,xlab='Time\n(a)',ylab='Y',main='Real world')
  matplot(eca$Ytilde,type='b',lty=1,pch=19,xlab='Time\n(b)',ylab='Ytilde',main='Gaussian world')
  matplot(epsi,type='b',lty=1,pch=19,xlab='Time\n(c)',ylab='epsilon',main='Residuals')
  # plot spatial pattern on world map
  map=ggplot(world,aes(long,lat))+
    geom_point(data=DF, aes(x = lon, y = lat, fill=w,shape=21),size=5,colour=NA)+
    scale_fill_gradient2(name = '', low="blue", mid='white',high="red",midpoint=median(w))+
    geom_polygon(aes(group = group), fill = "transparent",color = "black", size = .3)
  X11(title='ECA - Spatial pattern',width=7, height=4.5);
  print(map+ opts(title="Most Explanatory Component",panel.background = theme_rect(fill = "transparent")))
  # Temporal pattern
  X11(title='ECA - Temporal pattern (Tau) extracted from Y',width=7, height=4.5);plot(eca$Tau,type='b',xlab='Time',ylab='Temporal pattern');
  for (i in 1:Nx){points(eca$Ytilde[,i],pch=19,col="grey")}
  lines(eca$Tau,type='b',col='black',pch=19,lwd=3)
  # Check conditional independence
  Sigma=cor(eca$Ytilde,use="na.or.complete")
  Sigma.epsi=cor(epsi,use="na.or.complete")
  X11(title='ECA - Correlations in Y',width=10, height=5.1);par(mfrow=c(1,2))
  corrplot(Sigma,method="ellipse",title='Correlation matrix \nRaw Ytilde data',addtextlabel="no",mar=c(0,0,3,0))
  corrplot(Sigma.epsi,method="ellipse",title='Correlation matrix \nConditional on Tau',addtextlabel="no",mar=c(0,0,3,0))
  # compare with reconstructed temporal pattern
  X11(title='ECA - Temporal pattern extracted from Y vs. Temporal pattern reconstructed from Phi',width=5, height=5)
  plot(eca$Tau,eca$Tau.hat,xlab='Temporal pattern Tau',ylab='Reconstructed TauHat',pch=19);
  mini=min(min(eca$Tau),min(eca$Tau.hat));maxi=max(max(eca$Tau),max(eca$Tau.hat))
  lines(c(mini,maxi),c(mini,maxi),col='red')
  X11(title='ECA - Temporal pattern: extracted from Y (gray) vs. reconstructed from Phi (black)',width=15, height=4.5);
  plot(eca$Tau,type='b',col='gray',pch=19,lwd=1,ylim=c(-3,3),xlab='Time',ylab='Temporal pattern Tau')
  lines(eca$Tau.hat,type='b',col='black',pch=19,lwd=3)     
  # scatterplots Y vs. temporal patterns
  X11(title='ECA - Y vs. Temporal pattern extracted from Y',width=29, height=7);par(mfrow=lay,oma=1*c(1,1,1,1),mar=4*c(1,1,0,0)+0.1,mgp=0.5*c(3,1,0),tcl=0.1);
  for (i in 1:Nx){mask=!is.na(eca$Y[,i]);plot(eca$Tau[mask],eca$Y[mask,i],pch=19,xlab='Temporal pattern Tau',ylab=paste('Site',i))}
  X11(title='ECA - Y vs. Temporal pattern reconstructed from Phi',width=29, height=7);par(mfrow=lay,oma=1*c(1,1,1,1),mar=4*c(1,1,0,0)+0.1,mgp=0.5*c(3,1,0),tcl=0.1);
  for (i in 1:Nx){mask=!is.na(eca$Y[,i]);plot(eca$Tau.hat[mask],eca$Y[mask,i],pch=19,xlab='Reconstructed Tau.hat',ylab=paste('Site',i))}
  # Merged scatterplots (All sites together in Gaussian space)
  X11(title='ECA - Y vs. Temporal patterns (Gaussian space)',width=7, height=4);par(mfrow=c(1,2));
  plot(c(-4,4),c(-4,4),type='l',col='red',xlab='Temporal pattern Tau',ylab='All sites together (Gaussian space)')
  for (i in 1:Nx){mask=!is.na(eca$Ytilde[,i]);points(eca$Tau[mask],eca$Ytilde[mask,i],pch=19)}
  plot(c(-4,4),c(-4,4),type='l',col='red',xlab='Reconstructed Tau.hat',ylab='All sites together (Gaussian space)')
  for (i in 1:Nx){mask=!is.na(eca$Ytilde[,i]);points(eca$Tau.hat[mask],eca$Ytilde[mask,i],pch=19)}
}
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
EmpECA.predict<-function(eca,Phi,Y=NULL,DoPlot=FALSE,lay=c(2,ceiling(length(eca$Y[1,])/2))){
  # Create output list and initialize it
  OUT=list(Tau.hat=NA,Tau.hat.tilde=NA,Y=Y,Ytilde=NA,Phi=Phi)
  # Reconstruct Tau.hat.tilde (in Gaussian space)
  Nt=length(Phi[,1]);Nx=length(eca$Y[1,])
  OUT$Tau.hat.tilde=matrix(NA,Nt,1);
  for (i in 1:Nt){OUT$Tau.hat.tilde[i]=GetTauFromPhi(Phi[i,],eca$Omega)}
  #if(!is.null(Y)){
    # Get Ytilde in Gaussian space
    OUT$Ytilde=matrix(NA,Nt,Nx)
    for(i in 1:Nx){OUT$Ytilde[,i]=approx(eca$Y[,i],eca$Ytilde[,i],Y[,i])$y}
  #}
  # Send Tau.hat.tilde to real space
  OUT$Tau.hat=matrix(NA,Nt,Nx);
  for (i in 1:Nx){OUT$Tau.hat[,i]=approx(eca$Ytilde[,i],eca$Y[,i],OUT$Tau.hat.tilde)$y}
  if(DoPlot==TRUE){if(is.null(Y)){warning('No Y data to compare with prediction - plot aborted');return(OUT)}
    Nx=length(Y[1,]);Nt=length(Y[,1])
    if(length(Phi[,1])!=Nt){warning('Size mismatch [Y,Phi]');return(OUT)}
    if(length(eca$Y[1,])!=Nx){warning('Size mismatch: number of sites used in estimation and prediction differ');return(OUT)}
    # scatterplots Ytilde vs. temporal patterns (Gaussian Space)
    X11(title='Prediction in Gaussian space',width=29, height=7);par(mfrow=lay,oma=1*c(1,1,1,1),mar=4*c(1,1,0,0)+0.1,mgp=0.5*c(3,1,0),tcl=0.1);
    for (i in 1:Nx){
      mask=!is.na(OUT$Ytilde[,i]);plot(OUT$Tau.hat.tilde[mask],OUT$Ytilde[mask,i],pch=19,xlab='Temporal pattern Tau',ylab=paste('Site',i,'(Gaussian space)'))
      mini=min(min(OUT$Tau.hat.tilde[mask]),min(OUT$Ytilde[mask,i]));maxi=max(max(OUT$Tau.hat.tilde[mask]),max(OUT$Ytilde[mask,i]))
      lines(c(mini,maxi),c(mini,maxi),col='red')
    }
    # Regional scatterplots Ytilde vs. temporal patterns (Gaussian Space), all sites together
    X11(title='Prediction in Gaussian space, all sites merged together',width=5, height=5);
    plot(c(-4,4),c(-4,4),type='l',col='red',xlab='Temporal pattern Tau',ylab='All sites together (Gaussian space)')
    for (i in 1:Nx){
      mask=!is.na(OUT$Ytilde[,i]);
      points(OUT$Tau.hat.tilde[mask],OUT$Ytilde[mask,i],pch=19)
    }
    # scatterplots Ytilde vs. temporal patterns (Real Space)
    X11(title='Prediction in real space',width=29, height=7);par(mfrow=lay,oma=1*c(1,1,1,1),mar=4*c(1,1,0,0)+0.1,mgp=0.5*c(3,1,0),tcl=0.1);
    for (i in 1:Nx){
      mask=(!is.na(OUT$Tau.hat[,i]))&(!is.na(OUT$Y[,i]));plot(OUT$Tau.hat[mask,i],OUT$Y[mask,i],pch=19,xlab='Temporal pattern Tau',ylab=paste('Site',i,'(Real space)'))
      mini=min(min(OUT$Tau.hat[mask,i]),min(OUT$Y[mask,i]));maxi=max(max(OUT$Tau.hat[mask,i]),max(OUT$Y[mask,i]))
      lines(c(mini,maxi),c(mini,maxi),col='red')
    }
  }
return(OUT)}
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
# Retrieve the temporal signal from the climate field Phi rather than Hydro-obs Y
# Enables prediction!
GetTauFromPhi<-function(Phi,Omega){TauHat=sum(Phi*Omega)/sum(Omega^2);return(TauHat)}
#----------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------
# Normal score transformation. Ties are randomized, NAs are preserved
RandomizedNormalScore<-function(x){p=(rank(x,na.last="keep",ties.method="random")-0.5)/sum(!is.na(x))
  z=qnorm(p);return(z)}
#----------------------------------------------------------------------------------------------------