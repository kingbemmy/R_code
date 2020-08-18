
ReAnalyse<-function(output,P,re=NA,order,Ceiling,Floor,Step,gene,lgene=lgene){
  output<-output
  if(Step!=2&&Step!=3)stop("ReAnalyse step should be 2 or 3")
  if(Step==2){
    pvalue<-P
    if(order==1){newx<-pvalue[output$Output[2,1]:output$Output[6,1],]
    beginning<-output$Output[2,1]
    }
    if(order==2){newx<-pvalue[output$Output[9,1]:output$Output[13,1],]
    beginning<-output$Output[9,1]
    }
    if(order==3){newx<-pvalue[output$Output[16,1]:output$Output[20,1],]
    beginning<-output$Output[16,1]
    }

    dis<-newx
    if(length(dis)==0)
    {
      return(list(floor=0,"1 and 2 number"=0,"rank_largest"=0,"rank_second"=0,Output=0))
      stop
    }

    Ceiling<-Ceiling #minimum non zero value
    for(i in 1:length(dis)){if(dis[i]<Ceiling)dis[i]<-Ceiling}
    f0<-sum(-2*log(dis))/length(dis)
    floorvalue<-seq(f0,1.25*f0,length=11)
    sizefloor<-length(floorvalue)
    A<-matrix(0,length(newx),sizefloor)
    B<-matrix(0,1,sizefloor)
    for(i in 1:ncol(A))
    {
      A[,i]<-dis
      floor<-exp(floorvalue[i]/(-2))
      aa<-theta.MSS(A[,i],floor)
      n<-length(A[,i])
      ex<-excursion(n,aa)
      if(ex$stnewin==0)ex$stnewin<-lgene
      B[1,i]<-ex$stMSS/(ex$stnewin-ex$ststartpos)
      if(mean(aa[[1]])>0)B[1,i]=0
    }
    maxB=order(B,decreasing=TRUE)[1]
    if(max(B)==0)stop("All default floor can not satisfy MSS condition")
    Floor<-exp(floorvalue[maxB]/(-2))
  }

  if(Step==3){
    pvalue<-P
    if(order==1){
      redo_first_1<-re
      newx<-pvalue[redo_first_1$Output[2]:redo_first_1$Output[6],]
      beginning<-redo_first_1$Output[2]
      f0<-redo_first_1$floor
    }
    if(order==2){
      redo_second_1<-re
      if(is.na(redo_second_1$Output[2])==1)
      {
        return(list(floor=0,"1 and 2 number"=0,"rank_largest"=0,"rank_second"=0,Output=0))
        stop
      }
      newx<-pvalue[redo_second_1$Output[2]:redo_second_1$Output[6],]
      beginning<-redo_second_1$Output[2]
      f0<-redo_second_1$floor
    }
    if(order==3){
      redo_third_1<-re
      if(is.na(redo_third_1$Output[2])==1)
      {
        return(list(floor=0,"1 and 2 number"=0,"rank_largest"=0,"rank_second"=0,Output=0))
        stop
      }
      newx<-pvalue[redo_third_1$Output[2]:redo_third_1$Output[6],]
      beginning<-redo_third_1$Output[2]
      f0<-redo_third_1$floor
    }
    dis<-newx
    if(length(dis)==0)
    {
      return(list(floor=0,"1 and 2 number"=0,"rank_largest"=0,"rank_second"=0,Output=0))
      stop
    }
    Ceiling<-Ceiling  #minimum non zero value
    for(i in 1:length(newx))if(newx[i]<Ceiling)newx[i]<-Ceiling
    select<-matrix(0,4,1)
    rownames(select)<-c("MSS score","startpos","endpos","rank")
    repeat{
      f0<-0.1*f0
      if(f0<newx[2]&&table(newx)[1]<2){
        f0<-10*f0
        break}
      if(f0<Ceiling){
        f0<-10*f0
        break}
      if(f0<min(newx)){
        f0<-10*f0
        break}

      showMSS<-MSS(newx,f0,graph = FALSE)
      select[1,1]<-showMSS$MSSoutput$stMSS
      select[2,1]<-beginning+showMSS$MSSoutput$ststartpos-1
      if(select[2,1]<=0)select[2,1]=1
      select[3,1]<-beginning+showMSS$MSSoutput$stnewin-1

      select[4,]<-rank(-select[1,])
      newx<-pvalue[select[2,1]:select[3,1],]
      for(i in 1:length(newx))if(newx[i]<Ceiling)newx[i]<-Ceiling
      beginning<-select[2,1]
    }
    Floor<-f0
  }


  OUT(newx,Ceiling,Floor,Step,gene,beginning)
}

