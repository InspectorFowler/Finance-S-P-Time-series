aggregate<-function(dates,data){
  out.data<-granger.data<-data.frame(matrix(NA, nrow=nrow(dates), ncol=2))
  names(out.data)<-c("Date","Value")
  out.data[,1]<-dates
  for (i in 1:nrow(dates)){
    for (j in 1:nrow(data)){
      if (format(dates[i,1],"%m")==format(data[j,1],"%m")&
          format(dates[i,1],"%Y")==format(data[j,1],"%Y")){
        out.data[i,2]<-data[j,2]
      }
    }
  }
  write.csv(out.data,file=
              "C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/output.csv",
            row.names=FALSE)
}