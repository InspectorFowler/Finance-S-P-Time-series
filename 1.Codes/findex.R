findex<-function(data,list){
  
  x<-names(data)[-1]
  list[,1]<-as.character(list[,1])
  list[,2]<-as.character(list[,2])
  list[,3]<-as.character(list[,3])
  output<-data.frame(matrix(NA,nrow=length(x),ncol=3))
  names(output)<-c("Company","Symbol","Industry")
  y<-paste(list[,1],list[,3],sep=".")
  y<-gsub(" ",".",y)
  y<-gsub("[^[:alnum:]]", ".",y)
  y<-cbind(y,as.character(list[,2]))
  names(y)<-c("Company","Symbol")
  for (i in 1:length(x)){
    if(sum(grepl(x[i],y[,1],fixed=TRUE))==1){
      index<-as.numeric(grep(x[i],y[,1]))
      output[i,2]<-list[index,2]
      output[i,1]<-list[index,1]
      output[i,3]<-list[index,3]
    }
  }
  output[1,2]<-list[1,2]
  output[1,1]<-list[1,1]
  output[1,3]<-list[1,3]
  return(output)
}