
theta.MSS <- function(datapvalue,Floor){
  if(min(datapvalue)<0|max(datapvalue)>1)stop("P-value should be between 0 and 1")
  pvalue<-datapvalue
  M<-NULL
  for(j in 1:length(pvalue)){M[j]<-(-2*log(pvalue[j])-(-2*log(Floor)))}
  return(list(M))
}
