#~******************************************************************************
#~* PURPOSE OF THIS MODULE: A general version of the Mann-Kendall test, enabling
#~*                         various dependence assumptions
#~******************************************************************************
#~* PROGRAMMER: Benjamin Renard, Irstea Lyon
#~******************************************************************************
#~* CREATED/MODIFIED: Modified 17/11/2014
#~******************************************************************************
#~* CONTENTS
#~*    1. GeneralMannKendall: Main function, MK test with several dependence assumptions
#~* Other functions below are useful for GeneralMannKendall but are not really meant to be 
#~* used outside of this module:
#~*    2. GetMKStat: Get MK statistics and Sen's trend estimate
#~*    3. GetTiesCorrection: correction of MK stat. variance for ties
#~*    4. GetAR1Correction: correction of MK stat. variance for AR(1) dependence assumption
#~*    5. EstimateHurst: Estimation of the Hurst coefficient
#~*    6. HurstLkh: Likelihood function to maximize to get Hurst coefficient
#~*    7. RandomizedNormalScore: Randomized Normal Score transformation
#~******************************************************************************
#~* REF.: Hamed, Rao, 1998. A modified Mann-Kendall trend test for autocorrelated data. J. Hydrol., 204(1-4): 182-196.
#~*       Hamed, 2008. Trend detection in hydrologic data: The Mann–Kendall trend test under the scaling hypothesis. J. Hydrol., 349(3-4): 350-363.
#~******************************************************************************
#~* 2DO LIST: 
#~*
#~*
#~******************************************************************************
#~* COMMENTS: See below for important comments on GeneralMannKendall function
#~*
#~*
#~******************************************************************************
                                                      
GeneralMannKendall<-function(X,level=0.1,dep.option='INDE',DoDetrending=TRUE){
#^******************************************************************************
#^* PURPOSE: A general version of the Mann-Kendall test, enabling
#^*          various dependence assumptions
#^******************************************************************************
#^* PROGRAMMER: Benjamin Renard, Irstea Lyon
#^******************************************************************************
#^* CREATED/MODIFIED: Modified 17/11/2014
#^******************************************************************************
#^* IN
#^*    1. X, data (vector). IMPORTANT: assumes X is regularly-spaced. 
#^*    2. level of the test (between 0 and 1, default 0.1)
#^*    3. dep.option, option for handling temporal dependence. Available:
#^*              (i)  'INDE' [default], assume independence (i.e. the standard MK test)
#^*              (ii) 'AR1', assumes AR1 short-term dependence structure (i.e. Hamed and Rao's version of the MK test)
#^*              (iii)'LTP', assume long-term persistence (i.e. Hamed's version of the MK test)
#^*    4. DoDetrending, [only used for dep.option==LTP] do detrending before estimating Hurst coefficient (default=TRUE as recommended in Hamed's paper)
#^* OUT
#^*    1. A list with the following fields:
#^*              (i)  H: logical, reject (true) or do not reject (false) H0
#^*              (ii) P: p-value of the test
#^*              (iii)STAT: test statistics
#^*              (iv) TREND: trend estimate (using Sen's slope estimate) 
#^*              (v)  DEP: dependence estimate (=0 if dep.option='INDE', =lag-1 autocorrelation if dep.option='AR1', =Hurst coefficient if dep.option='LTP' 
#^******************************************************************************
#^* REF.: Hamed, Rao, 1998. A modified Mann-Kendall trend test for autocorrelated data. J. Hydrol., 204(1-4): 182-196.
#^*       Hamed, 2008. Trend detection in hydrologic data: The Mann–Kendall trend test under the scaling hypothesis. J. Hydrol., 349(3-4): 350-363.
#^******************************************************************************
#^* 2DO LIST: 
#^******************************************************************************
#^* COMMENTS: 
#^*    1. Handling of ties: Specific formula exist for INDE and AR1, but the LTP case is trickier.
#^*       Hammed's paper is unclear on how to handle ties, especially at the step of Hurst coefficient estimation.
#^*       There is a normal-score transformation at this step, and one needs to decide how to assign a rank to ties.
#^*       What is implemented below is the option ties.method="random", i.e. the rank is randomized for ties.
#^*       This is not, strictly speaking, correct because this randomization impacts the dependence structure.
#^*       However synthetic runs suggest it works OK.
#^*    2. Computational efficiency: Likely poor for case dep.option='LTP'
#^*       There is a 4-level loop which leads to a n^4 algorithm. 
#^*       I attempted to vectorize this loop but it didn't improve things.
#^*       => Expect significant running times for dep.option='LTP' when size(X)>50...
#^*       (orders of magnitude: 1s for n=30, 10s for n=50, 2-3 minutes for n=100)
#^*       On the other hand both options INDE and AR1 are very fast.
#^******************************************************************************
	
	#-----------------------------------------------------------------------------------
	# STEP 0: preliminaries
	#-----------------------------------------------------------------------------------
	# Create output list and initialize it
	OUT=list(H=NA,P=NA,STAT=NA,TREND=NA,DEP=NA)
	# Check dep.option is valid
	if(!((dep.option=='INDE')|(dep.option=='AR1')|(dep.option=='LTP'))){warning('Unknown dep.option');return(OUT)}
	# Remove Nas from X to create NA-free vector Z
	Z=X[!is.na(X)];n=length(Z)
	# Don't even try if less than 3 non-missing values
	if(n<3){warning('less than 3 non-missing values');return(OUT)}
	# Get basic MK stat + Sen's trend estimate
	get.MK.basics=GetMKStat(X)
	MK=get.MK.basics$stat;OUT$TREND=get.MK.basics$trend
	
	#-----------------------------------------------------------------------------------
	# CASE 1: 'INDE' or 'AR1'
	#-----------------------------------------------------------------------------------
	if((dep.option=='INDE')|(dep.option=='AR1')){
		# Compute basic variance
		var0=((n*(n-1)*(2*n+5)))/18
		# Compute ties correction and get ties-corrected variance
		var1=var0-GetTiesCorrection(Z)
		if(is.na(var1)){warning('NA variance');return(OUT)}
		if(var1<=0){warning('negative variance');return(OUT)}
		# Compute autocorrelation correction if dep.option=='AR1'
		if(dep.option=='AR1'){
			AR1.correction=GetAR1Correction(X)
			correction=AR1.correction$correction
			OUT$DEP=AR1.correction$lag1
			}
		else{correction=1;OUT$DEP=0}
		MKvar=var1*correction
		if(MKvar<=0){warning('negative variance');return(OUT)}
	}
	
	#-----------------------------------------------------------------------------------
	# CASE 2: 'LTP'
	#-----------------------------------------------------------------------------------
	if(dep.option=='LTP'){
		# Estimate Hurst Coeff
		Hu=EstimateHurst(X,DoDetrending,OUT$TREND)
		OUT$DEP=Hu
    # Get autocorrelation function
		lambda=0:n
		C=0.5*(abs(lambda+1)^(2*Hu)-2*abs(lambda)^(2*Hu)+abs(lambda-1)^(2*Hu))
		# Compute variance of MK using the monstrous 4-level loop...
		var0=0
		for(j in 2:n){
			for(i in 1:(j-1)){
				for(l in 2:n){
					for(k in 1:(l-1)){
					num=C[abs(j-l)+1]-C[abs(i-l)+1]-C[abs(j-k)+1]+C[abs(i-k)+1]
					den=sqrt( (2-2*C[abs(i-j)+1]) * (2-2*C[abs(k-l)+1]) )
					var0=var0+asin(num/den)
					}
				}
			}
		}
		var1=(2/pi)*var0
		if(is.na(var1)){warning('NA variance');return(OUT)}
		if(var1<=0){warning('negative variance');return(OUT)}
		# bias correction
		a0=(1.0024*n-2.5681)/(n+18.6693)
		a1=(-2.2510*n+157.2075)/(n+9.2245)
		a2=(15.3402*n-188.6140)/(n+5.8917)
		a3=(-31.4258*n+549.8599)/(n-1.1040)
		a4=(20.7988*n-419.0402)/(n-1.9248)
		B=a0+a1*Hu+a2*Hu^2+a3*Hu^3+a4*Hu^4
		MKvar=var1*B
		if(MKvar<=0){warning('negative variance');return(OUT)}	
	}

	#-----------------------------------------------------------------------------------
	# FINAL STEP: Get test statistics, significance, pval, etc.
	#-----------------------------------------------------------------------------------
	# Final test statistics
	if(MK>0){stat=(MK-1)/sqrt(MKvar)}
	else if(MK<0){stat=(MK+1)/sqrt(MKvar)}
	else{stat=MK/sqrt(MKvar)}
	OUT$STAT=stat
	# p-val (2-sided test)
	OUT$P=2*pnorm(-1*abs(stat),mean=0,sd=1)
	# decision
	OUT$H=(OUT$P<level)
return(OUT)}

GetMKStat<-function(X){
#~******************************************************************************
#~* PURPOSE: Get MK stat and Sen's trend estimate
#~******************************************************************************
#~ IN:  1. X, data vector
#~ OUT: A list, with components: 1. stat, MK statistics; 2/ trend, Sen's estimate
#~******************************************************************************
	n=length(X);count.p=0;count.m=0;k=0
	slope.list=matrix(NA,((n-1)*n)/2,1)
	for(j in 2:n){for(i in 1:(j-1)){
		k=k+1
		if( (!is.na(X[j])) & (!is.na(X[i])) ){
			slope.list[k]=(X[j]-X[i])/(j-i)
			if(X[j]>X[i]){count.p=count.p+1}
			else if (X[j]<X[i]){count.m=count.m+1}
		}
	}}
	stat=count.p-count.m
	trend=median(slope.list[!is.na(slope.list)])
return(list(stat=stat,trend=trend))}

GetTiesCorrection<-function(Z){
#~******************************************************************************
#~* PURPOSE: Get correction for ties
#~******************************************************************************
#~ IN:  1. Z, data vector (without NAs)
#~ OUT: 1. the correction factor for the variance of MK stat
#~******************************************************************************
	n=length(Z)
	w=matrix(NA,n,1);tie=matrix(NA,n,1);v=matrix(NA,n,1)
	for(i in 1:n){w[i]=sum(Z==Z[i])} # counts how many times each value is duplicated
	for(i in 1:n){
		tie[i]=sum(w==i)/i # create a vector containing the number of ties of extent i
		v[i]=tie[i]*i*(i-1)*(2*i+5) # save contribution of i-ties to correction
	}
return(sum(v)/18)}

GetAR1Correction<-function(Z){
#~******************************************************************************
#~* PURPOSE: Get correction for AR(1)-like dependence
#~******************************************************************************
#~ IN:  1. Z, data vector
#~ OUT: A list with components:
#~      1. lag1, estimated lag-1 correlation coefficient
#~      2. correction, the correction factor for the variance of MK stat
#~******************************************************************************
	n=length(Z)
	w=matrix(NA,n-2,1)
	# Compute lag-1 coefficient
	Z0=Z[!is.na(Z)];m=mean(Z0)
	x=Z[1:(n-1)];y=Z[2:n]
	mask=(!is.na(x))&(!is.na(y))
	lag1=sum((x[mask]-m)*(y[mask]-m))/sum((Z0-m)^2)
	#Compute correction
	for(i in 1:(n-2)){w[i]=(n-i)*(n-i-1)*(n-i-2)*((lag1)^(i))} # save contribution of lag i to correction
	correction=1+(2/(n*(n-1)*(n-2)))*sum(w)
return(list(lag1=lag1,correction=correction))}

EstimateHurst<-function(Z,DoDetrending,trend){
#~******************************************************************************
#~* PURPOSE: Get correction for AR(1)-like dependence
#~******************************************************************************
#~ IN:  1. Z, data vector
#~      2. DoDetrending, detrend data before estimating Hurst?
#~      3. trend, trend value (only used if detrending required)
#~ OUT: 1. Estimated value of the hurst coefficient
#~******************************************************************************
	n=length(Z)
	# Detrend if requested
	if(DoDetrending) {Y=Z-trend*(1:n)}
	# Transform to normal-score - Note that ties.method="random", might affect autocorrelation! but Hamed's paper is unclear on how to treat ties at this step
	W=RandomizedNormalScore(Y)
	Max.Lkh=optimize(f=HurstLkh,interval=c(0.5,1),W,maximum=TRUE)
	H=Max.Lkh$maximum
return(H)}

HurstLkh<-function(H,x){
#~******************************************************************************
#~* PURPOSE: Compute the likelihood function to be maximized for estimating H
#~******************************************************************************
#~ IN:  1. H, hurst coeff. value
#~      2. x, data sample
#~ OUT: 1. log-likelihood
#~******************************************************************************
	n=length(x)
	# Compute Cn(H)
	CnH=matrix(NA,n,n)
	for (i in 1:n){
		for (j in 1:n){
			l=abs(i-j);
			CnH[i,j]=0.5*(abs(l+1)^(2*H)-2*(abs(l)^(2*H))+ abs(l-1)^(2*H));
		}
	}
	mask=!is.na(x)
	m=sum(mask)
	v0=qnorm((1:m)/(m+1))
	g0=var(v0)
	L=-0.5*log(det(CnH[mask,mask]))-(t(x[mask])%*%solve(CnH[mask,mask])%*%x[mask])/(2*g0)
return(L)}

RandomizedNormalScore<-function(x){
#~******************************************************************************
#~* PURPOSE: Randomized Normal Score transformation
#~******************************************************************************
#~ IN:  1. x, original series
#~ OUT: 1. normal-scored series
#~******************************************************************************
  # empirical frequencies
  p=(rank(x,ties.method="random",na.last="keep"))/(1+sum(!is.na(x)))
  # Normal quantile
  z=qnorm(p)
return(z)}


# Functions below are not used - naive and unsuccessful try at speeding up the 4-level loop by using embedded sapply.
kFunk<-function(k,j,i,l,C){
	num=C[abs(j-l)+1]-C[abs(i-l)+1]-C[abs(j-k)+1]+C[abs(i-k)+1]
	den=sqrt( (2-2*C[abs(i-j)+1]) * (2-2*C[abs(k-l)+1]) )
	z=asin(num/den)
return(z)}

lFunk<-function(l,j,i,C){
	z=sum(sapply(1:(l-1),kFunk,j,i,l,C))
return(z)}

iFunk<-function(i,j,C,n){
	z=sum(sapply(2:n,lFunk,j,i,C))
return(z)}

jFunk<-function(j,C,n){
	z=sum(sapply(1:(j-1),iFunk,j,C,n))
return(z)}

ijklFunk<-function(C,n){
	z=sum(sapply(2:n,jFunk,C,n))
return(z)}

DummyFunk<-function(C,n){
		var0=0
		for(j in 2:n){
			for(i in 1:(j-1)){
				for(l in 2:n){
					for(k in 1:(l-1)){
					num=C[abs(j-l)+1]-C[abs(i-l)+1]-C[abs(j-k)+1]+C[abs(i-k)+1]
					den=sqrt( (2-2*C[abs(i-j)+1]) * (2-2*C[abs(k-l)+1]) )
					var0=var0+asin(num/den)
					}
				}
			}
		}
return(var0)}