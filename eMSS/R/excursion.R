
excursion <- function(n,aa){
  x <- c(0,as.numeric(as.character(aa[1][[1]])))
  y <- cumsum(x)
  min.y <- cummin(y)
  detect.y <- min.y[2:(n+1)] - min.y[1:n]
  detect.y <- paste(ifelse(detect.y==0,1,0),sep="",collapse="")
  increas.region <- gregexpr("1{1,}",detect.y)
  start.point <- increas.region[[1]]
  end.point <- increas.region[[1]] + attr(increas.region[[1]],"match.length") + 1
  excursion.group <- mapply(function(start,end){if(end>length(x)) end <- length(x)
  temp.y <- y[start:end]
  excursion <- max(temp.y)-temp.y[1]
  max.index <- min(which(temp.y==max(temp.y)))
  c(excursion,start-1,start-1+max.index-1)} , start.point , end.point)
  if(excursion.group[2,1]==0&&excursion.group[3,1]==1)excursion.group[,1]=0
  orderMSS<-order(excursion.group[1,],decreasing=TRUE)[1:3]
  orderMSS1<-orderMSS+1
  stnewin=0
  ndnewin=0
  rdnewin=0
  ifelse(orderMSS1[1]>ncol(excursion.group),stnewin<-length(x)-1,stnewin<-excursion.group[2,orderMSS1[1]])
  ifelse(orderMSS1[2]>ncol(excursion.group),ndnewin<-length(x)-1,ndnewin<-excursion.group[2,orderMSS1[2]])
  ifelse(orderMSS1[3]>ncol(excursion.group),rdnewin<-length(x)-1,rdnewin<-excursion.group[2,orderMSS1[3]])
  return(list(stMSS=excursion.group[1,orderMSS[1]] , ststartpos=excursion.group[2,orderMSS[1]] , stmaxpos=excursion.group[3,orderMSS[1]]
              ,ndMSS=excursion.group[1,orderMSS[2]] , ndstartpos=excursion.group[2,orderMSS[2]] , ndmaxpos=excursion.group[3,orderMSS[2]]
              ,rdMSS=excursion.group[1,orderMSS[3]] , rdstartpos=excursion.group[2,orderMSS[3]] , rdmaxpos=excursion.group[3,orderMSS[3]]
              ,stnewin=stnewin,ndnewin=ndnewin,rdnewin=rdnewin))
}
