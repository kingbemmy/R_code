
OUT<-function(pvalue,Ceiling,Floor,Step,gene,beginning=NA)
{
  if(Step!=1&&Step!=2&&Step!=3)stop("Step should be 1,2 or 3")
  if(Step==1 && is.na(beginning)==0)stop("when Step=1,beginning should be NA")
  ldata<-length(pvalue)
  for(i in 1:length(pvalue)){if(pvalue[i]<Ceiling)pvalue[i]<-Ceiling}

  if(Step==1){
    Output<-matrix(0,21,1)
    rownames(Output)<-c("1stMSS","1ststartloc","1ststartpos","1stmaxloc","1stmaxpos","1stnewin","1stnewpos",
                        "2ndMSS","2ndstartloc","2ndstartpos","2ndmaxloc","2ndmaxpos","2ndnewin","2ndnewpos",
                        "3rdMSS","3rdstartloc","3rdstartpos","3rdmaxloc","3rdmaxpos","3rdnewin","3rdnewpos")
  }

  if(Step!=1){
    Output<-matrix(0,14,1)
    rownames(Output)<-c("1stMSS","1ststartloc","1ststartpos","1stmaxloc","1stmaxpos","1stnewin","1stnewpos",
                        "2ndMSS","2ndstartloc","2ndstartpos","2ndmaxloc","2ndmaxpos","2ndnewin","2ndnewpos")
  }

  if(Step==1){
    showMSS<-MSS(pvalue,Floor,graph = FALSE)
    for(i in 1:1){
      Output[1,1]<-showMSS$MSSoutput$stMSS
      if(is.na(Output[1,1])==1)next
      if(Output[1,1]==0)next
      Output[2,1]<-showMSS$MSSoutput$ststartpos
      if(Output[2,1]==0)Output[2,1]<-1
      Output[3,1]<-gene[Output[2,1]]
      Output[4,1]<-showMSS$MSSoutput$stmaxpos
      Output[5,1]<-gene[Output[4,1]]
      Output[6,1]<-showMSS$MSSoutput$stnewin
      Output[7,1]<-gene[Output[6,1]]

      Output[8,1]<-showMSS$MSSoutput$ndMSS
      if(is.na(Output[8,1])==1)next
      if(Output[8,1]==0)next
      Output[9,1]<-showMSS$MSSoutput$ndstartpos
      if(is.na(Output[9,1])==1)next
      if(Output[9,1]==0)Output[9,1]<-1
      Output[10,1]<-gene[Output[9,1]]
      Output[11,1]<-showMSS$MSSoutput$ndmaxpos
      Output[12,1]<-gene[Output[11,1]]
      Output[13,1]<-showMSS$MSSoutput$ndnewin
      Output[14,1]<-gene[Output[13,1]]

      Output[15,1]<-showMSS$MSSoutput$rdMSS
      if(is.na(Output[15,1])==1)next
      if(Output[15,1]==0)next
      Output[16,1]<-showMSS$MSSoutput$rdstartpos
      if(is.na(Output[16,1])==1)next
      if(Output[16,1]==0)Output[16,1]<-1
      Output[17,1]<-gene[Output[16,1]]
      Output[18,1]<-showMSS$MSSoutput$rdmaxpos
      Output[19,1]<-gene[Output[18,1]]
      Output[20,1]<-showMSS$MSSoutput$rdnewin
      Output[21,1]<-gene[Output[20,1]]
    }
    MSS(pvalue,Floor)
  }

  if(Step!=1){
    showMSS<-MSS(pvalue,Floor,graph = FALSE)
    for(i in 1:1){
      Output[1,1]<-showMSS$MSSoutput$stMSS
      if(Output[1,1]==0)break
      Output[2,1]<-beginning+showMSS$MSSoutput$ststartpos-1
      if(is.na(Output[2,1])==1)break
      if(Output[2,1]==0)Output[2,1]<-1
      Output[3,1]<-gene[Output[2,1]]
      Output[4,1]<-beginning+showMSS$MSSoutput$stmaxpos-1
      Output[5,1]<-gene[Output[4,1]]
      Output[6,1]<-beginning+showMSS$MSSoutput$stnewin-1
      Output[7,1]<-gene[Output[6,1]]

      Output[8,1]<-showMSS$MSSoutput$ndMSS
      if(is.na(Output[8,1])==1)break
      if(Output[8,1]==0)break
      Output[9,1]<-beginning+showMSS$MSSoutput$ndstartpos-1
      if(is.na(Output[9,1])==1)break
      if(Output[9,1]==0)Output[9,1]<-1
      Output[10,1]<-gene[Output[9,1]]
      Output[11,1]<-beginning+showMSS$MSSoutput$ndmaxpos-1
      Output[12,1]<-gene[Output[11,1]]
      Output[13,1]<-beginning+showMSS$MSSoutput$ndnewin-1
      Output[14,1]<-gene[Output[13,1]]
    }
    MSS(pvalue,Floor,Step=2)
  }

  options(scipen=99)
  if(Step==1){
    return(list(Output=Output,"Output1"=Output[,1]))
  }else{
    return(list(floor=Floor,Output=Output[,1]))
  }
}
