dress<-function(Q,spag,ref,doPlot=F,mu.inflate=1,sigma.inflate=1){
  #^******************************************************************************
  #^* PURPOSE: dressing of a discharge time series Q, with residuals looking
  #^*          similar to the ones between a spaghetti spag (typically from BaRatin)
  #^*          and a reference ref (typically BaRatin Maxpost)
  #^******************************************************************************
  #^* PROGRAMMER: Benjamin Renard, Irstea Lyon
  #^******************************************************************************
  #^* CREATED/MODIFIED: 25/04/2017
  #^******************************************************************************
  #^* IN
  #^*    1. [real vector] Q, the naked times series to be dressed, size N
  #^*    2. [real vector] spag, one spaghetti, size M
  #^*    3. [real vector] ref, the reference spaghetti (typically the maxpost), size M
  #^*    4. [Logical]     doPlot, do plot?
  #^*    5. [real]        mu.inflate, factor inflating the mean of the residuals
  #^*    6. [real]        sigma.inflate, factor inflating the sdev of the residuals
  #^* OUT
  #^*    1. [real vector] Qout, the dressed times series, size N
   #^******************************************************************************
  #^* REF.: 
  #^******************************************************************************
  #^* 2DO: 
  #^******************************************************************************
  #^* COMMENTS: 
  #^******************************************************************************
  
  # define data frame containing xplanatory variable x and residuals res
  df=data.frame(x=log(ref),res=spag-ref)
  # apply gamlss model assuming residuals are Gaussian, 
  # with both mean (mu) and sdev (sigma) being regressed against the 
  # covariate using nonparametric splines
  w=gamlss(res~pb(x),sigma.formula=~pb(x),family=NO(),data=df)
  # predict mu and sigma given Q
  par=predictAll(w,newdata=data.frame(x=log(Q)),data=df)
  # sample new residuals
  res=mu.inflate*par$mu+sigma.inflate*par$sigma*rnorm(length(par$mu))
  # dress Q
  Qout=Q+res
  # plot
  if(doPlot){
    centiles.pred(w,xname="x",xvalues=df$x,data=df,plot=T)
    points(df$x,df$res,pch=19,cex=0.5)
  }
  return(Qout)
}