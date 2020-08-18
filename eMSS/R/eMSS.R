eMSS<-function(p){
  if(dir.exists("eMSS out")==0){dir.create("eMSS out")}
  gene<-matrix(1:length(p),length(p),1)
  found<-ceiling_floor(p,length(p))
  Ceiling<-found$suitable_ceiling
  Floor<-found$suitable_floor

  png(filename ="./eMSS out/Original output.png",width = 1920, height = 1080)
  output<-OUT(p,Ceiling,Floor,Step=1,gene)
  dev.off()

  #----Take out each eMSS interval for re-analysis----#
  #----Reanalyze eMSS intervals----#
  png(filename ="./eMSS out/redo_first_1.png",width = 1920, height = 1080)
  redo_first_1<-ReAnalyse(output,p,re=NA,order=1,Ceiling,Floor,Step=2,gene)
  dev.off()

  png(filename ="./eMSS out/redo_second_1.png",width = 1920, height = 1080)
  redo_second_1<-ReAnalyse(output,p,re=NA,order=2,Ceiling,Floor,Step=2,gene)
  dev.off()

  png(filename ="./eMSS out/redo_third_1.png",width = 1920, height = 1080)
  redo_third_1<-ReAnalyse(output,p,re=NA,order=3,Ceiling,Floor,Step=2,gene)
  dev.off()

  #----Limit approaching upper bound to lower bound----#

  png(filename ="./eMSS out/redo_first_2.png",width = 1920, height = 1080)
  redo_first_2<-ReAnalyse(output,p,re=redo_first_1,order=1,Ceiling,Floor,Step=3,gene)
  dev.off()

  png(filename ="./eMSS out/redo_second_2.png",width = 1920, height = 1080)
  redo_second_2<-ReAnalyse(output,p,re=redo_second_1,order=2,Ceiling,Floor,Step=3,gene)
  dev.off()

  png(filename ="./eMSS out/redo_third_2.png",width = 1920, height = 1080)
  redo_third_2<-ReAnalyse(output,p,re=redo_third_1,order=3,Ceiling,Floor,Step=3,gene)
  dev.off()

  final<-matrix(0,6,3)
  colnames(final)<-c("1st_MSS","2nd_MSS","3rd_MSS")
  rownames(final)<-c("Marker_pos","Markers","Refined pos","Markers","Final pos","Markers")
  if(is.na(redo_first_1$Output[2])==1){redo_first_1$Output[2]=0;redo_first_1$Output[4]=0}
  if(is.na(redo_first_2$Output[2])==1){redo_first_2$Output[2]=0;redo_first_2$Output[4]=0}
  if(is.na(redo_second_1$Output[2])==1){redo_second_1$Output[2]=0;redo_second_1$Output[4]=0}
  if(is.na(redo_second_2$Output[2])==1){redo_second_2$Output[2]=0;redo_second_2$Output[4]=0}
  if(is.na(redo_third_1$Output[2])==1){redo_third_1$Output[2]=0;redo_third_1$Output[4]=0}
  if(is.na(redo_third_2$Output[2])==1){redo_third_2$Output[2]=0;redo_third_2$Output[4]=0}
  final[1,1]<-paste(output$Output1[3],output$Output1[5],sep = "-")
  final[1,2]<-paste(output$Output1[10],output$Output1[12],sep = "-")
  final[1,3]<-paste(output$Output1[17],output$Output1[19],sep = "-")
  final[2,1]<-output$Output1[4]-output$Output1[2]+1
  final[2,2]<-output$Output1[11]-output$Output1[9]+1
  final[2,3]<-output$Output1[18]-output$Output1[16]+1
  final[3,1]<-paste(redo_first_1$Output[3],redo_first_1$Output[5],sep = "-")
  final[3,2]<-paste(redo_second_1$Output[3],redo_second_1$Output[5],sep = "-")
  final[3,3]<-paste(redo_third_1$Output[3],redo_third_1$Output[5],sep = "-")
  final[4,1]<-redo_first_1$Output[4]-redo_first_1$Output[2]+1
  final[4,2]<-redo_second_1$Output[4]-redo_second_1$Output[2]+1
  final[4,3]<-redo_third_1$Output[4]-redo_third_1$Output[2]+1
  final[5,1]<-paste(redo_first_2$Output[3],redo_first_2$Output[5],sep = "-")
  final[5,2]<-paste(redo_second_2$Output[3],redo_second_2$Output[5],sep = "-")
  final[5,3]<-paste(redo_third_2$Output[3],redo_third_2$Output[5],sep = "-")
  final[6,1]<-redo_first_2$Output[4]-redo_first_2$Output[2]+1
  final[6,2]<-redo_second_2$Output[4]-redo_second_2$Output[2]+1
  final[6,3]<-redo_third_2$Output[4]-redo_third_2$Output[2]+1

  picture<-paste("All pictures are saved in",paste(getwd(),"/eMSS out",sep = ""))
  return(list("eMSS"=final,"message:"=picture))

}
