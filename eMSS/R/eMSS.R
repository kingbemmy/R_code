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
  redo_first_1<-ReAnalyse(output,p,re=NA,1,Floor,Step=2,gene,Ceiling)
  dev.off()

  png(filename ="./eMSS out/redo_second_1.png",width = 1920, height = 1080)
  redo_second_1<-ReAnalyse(output,p,re=NA,2,Floor,Step=2,gene,Ceiling)
  dev.off()

  png(filename ="./eMSS out/redo_third_1.png",width = 1920, height = 1080)
  redo_third_1<-ReAnalyse(output,p,re=NA,3,Floor,Step=2,gene,Ceiling)
  dev.off()

  #----Limit approaching upper bound to lower bound----#

  png(filename ="./eMSS out/redo_first_2.png",width = 1920, height = 1080)
  redo_first_2<-ReAnalyse(output,p,re=redo_first_1,1,Floor,Step=3,gene,Ceiling)
  dev.off()

  png(filename ="./eMSS out/redo_second_2.png",width = 1920, height = 1080)
  redo_second_2<-ReAnalyse(output,p,re=redo_second_1,2,Floor,Step=3,gene,Ceiling)
  dev.off()

  png(filename ="./eMSS out/redo_third_2.png",width = 1920, height = 1080)
  redo_third_2<-ReAnalyse(output,p,re=redo_third_1,3,Floor,Step=3,gene,Ceiling)
  dev.off()

  picture<-paste("All pictures are saved in",paste(getwd(),"/eMSS out",sep = ""))
  return(list("Ceiling"=Ceiling,"Floor"=Floor,"original output"=output,
              "first refinement"=redo_first_1,"second refinement"=redo_second_1,"third refinement"=redo_third_1,
              "first final"=redo_first_2,"second final"=redo_second_2,"third final"=redo_third_2,
              "message:"=picture))

}
