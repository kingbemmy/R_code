
MSS<-function(permu,Floor,Step = 1,beginning=NA,graph = TRUE)
{
  if(Step!=1&&Step!=2&&Step!=3)stop("Step should be 1,2or3")
  datapvalue<-permu
  theta.MSS(datapvalue,Floor)
  aa<-theta.MSS(datapvalue,Floor)
  n<-length(datapvalue)
  excursion(n,aa)
  ex<-excursion(n,aa)
  if(graph == TRUE){
    if(is.na(beginning)==1){
      startex<-ex$ststartpos[1][[1]]
      if(startex==0)startex<-1
      endex<-ex$stmaxpos[1][[1]]
      stnewinex<-ex$stnewin[1][[1]]
      if(is.na(ex$ndMSS[1][[1]])!=1&&ex$ndMSS[1][[1]]!=0)
      {
        ndstartex<-ex$ndstartpos[1][[1]]
        if(ndstartex==0)ndstartex<-1
        ndendex<-ex$ndmaxpos[1][[1]]
        ndnewinex<-ex$ndnewin[1][[1]]
      }
      if(Step==1)
      {
        if(is.na(ex$rdMSS[1][[1]])!=1&&ex$rdMSS[1][[1]]!=0)
        {
          rdstartex<-ex$rdstartpos[1][[1]]
          if(rdstartex==0)rdstartex<-1
          rdendex<-ex$rdmaxpos[1][[1]]
          rdnewinex<-ex$rdnewin[1][[1]]
        }
      }
      x <- c(as.numeric(as.character(aa[1][[1]])))
      y <- cumsum(x)
      plot(y,type="o", lwd = 1.5,xlab = "marker",ylab = "score")
      points(startex,y[startex],col=2,cex=1.5,pch=24,bg="red")
      points(endex,y[endex],col=2,cex=1.5,pch=25,bg="red")
      if(is.na(ex$ndMSS[1][[1]])!=1&&ex$ndMSS[1][[1]]!=0){
        points(ndstartex,y[ndstartex],col=2,cex=1.5,pch=24,bg="blue")
        points(ndendex,y[ndendex],col=2,cex=1.5,pch=25,bg="blue")
      }
      if(Step==1){
        if(is.na(ex$rdMSS[1][[1]])!=1&&ex$rdMSS[1][[1]]!=0)
        {
          points(rdstartex,y[rdstartex],col=2,cex=1.5,pch=24,bg="green")
          points(rdendex,y[rdendex],col=2,cex=1.5,pch=25,bg="green")
        }
      }
    }

    if(is.na(beginning)==0){
      #beginning<-beginning
      startex<-beginning+ex$ststartpos[1][[1]]-1
      if(ex$ststartpos[1][[1]]==0)startex<-beginning
      endex<-beginning+ex$stmaxpos[1][[1]]-1
      stnewinex<-beginning+ex$stnewin[1][[1]]-1
      if(is.na(ex$ndMSS[1][[1]])!=1&&ex$ndMSS[1][[1]]!=0)
      {
        ndstartex<-beginning+ex$ndstartpos[1][[1]]-1
        if(ex$ndstartpos[1][[1]]==0)ndstartex<-beginning
        ndendex<-beginning+ex$ndmaxpos[1][[1]]-1
        ndnewinex<-beginning+ex$ndnewin[1][[1]]-1
      }
      x <- c(as.numeric(as.character(aa[1][[1]])))
      region<-beginning:(beginning+(length(x)-1))
      y <- cumsum(x)
      plot(region,y,type="o", lwd = 1.5,xlab = "marker",ylab = "score")
      points(startex,y[startex-beginning+1],col=2,cex=1.5,pch=24,bg="red")
      points(endex,y[endex-beginning+1],col=2,cex=1.5,pch=25,bg="red")
      if(is.na(ex$ndMSS[1][[1]])!=1&&ex$ndMSS[1][[1]]!=0){
        points(ndstartex,y[ndstartex-beginning+1],col=2,cex=1.5,pch=24,bg="blue")
        points(ndendex,y[ndendex-beginning+1],col=2,cex=1.5,pch=25,bg="blue")
      }
    }


  }
  return(list(MSSoutput=ex))
}

