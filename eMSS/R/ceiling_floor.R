
ceiling_floor<-function(pvalue,lpvalue)   #####Calculate upper and lower bounds####
{
  ceiling<-quantile(sort(pvalue),0.1)
  for(i in 1:length(pvalue)){if(pvalue[i]<ceiling)pvalue[i]<-ceiling}
  f0<-sum(-2*log(pvalue))/length(pvalue)
  floorvalue<-seq(f0,1.25*f0,length=11)
  sizefloor<-length(floorvalue)
  A<-matrix(0,lpvalue,sizefloor)
  B<-matrix(0,1,sizefloor)
  for(i in 1:ncol(A))
  {
    A[,i]<-pvalue
    floor<-exp(floorvalue[i]/(-2))
    aa<-theta.MSS(A[,i],floor)
    n<-length(A[,i])
    ex<-excursion(n,aa)
    if(ex$stnewin==0)ex$stnewin<-lpvalue
    B[1,i]<-ex$stMSS/(ex$stnewin-ex$ststartpos)
    if(mean(aa[[1]])>0)B[1,i]=0
  }
  maxB=order(B,decreasing=TRUE)[1]
  if(max(B)==0)stop("All default floor can not satisfy MSS condition")
  Floor<-exp(floorvalue[maxB]/(-2))
  MSS(pvalue,Floor,Step=1)
  return(list(suitable_ceiling=ceiling,suitable_floor=exp(floorvalue[maxB]/(-2))))
}

