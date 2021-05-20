require(kohonen)

getGroup<-function(data,k,standardize=F,ylab="value"){  
  # standardize
  X=as.matrix(data)
  n=length(X[,1])
  if(standardize){
    for(i in 1:n){X[i,]=X[i,]/sum(X[i,])}
  }

  # apply kohonen
  w=som(X,grid = somgrid(k,1, "hexagonal"))
  z=predict(w,trainY=X)
  group=z$unit.classif

  # plot
  par(mfrow=c(1,k))
  for(i in 1:k){
    matplot(t(X[group==i,]),col="lightgray",type="l",
            ylim=c(0,max(X)),xlab="month",ylab=ylab,main=paste("cluster",i))
    m=apply(X[group==i,],2,mean)
    lines(m,col="red",lwd=2,type="b",pch=19)
  }
  return(group)
}
