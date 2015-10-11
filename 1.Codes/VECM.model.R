VECM.model<-function(cluster.data,pred.data,real.data,lagvalue,rvalue=1,aheadv,dir=TRUE){
    options(warn=-1)
    suppressMessages(library(tsDyn))
    suppressMessages(library(forecast))
    write.table(real.data,"C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/VECM.Predict.csv",
                sep=",",row.names=FALSE)
    write.table(real.data,"C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/VECM.Direction.csv",
                sep=",",row.names=FALSE)
    for (i in 2:ncol(cluster.data)){
        pred<-data.frame(matrix(NA,nrow=aheadv,ncol=(ncol(pred.data))))
        pred[,1]<-real.data[,1]
        names(pred)<-names(pred.data)
        names(pred)[1]<-as.character(paste("Cluster-",(i-1),sep=""))
        for (j in 2:ncol(pred.data)){
            model<-VECM(cbind(cluster.data[,i],pred.data[,j]),lag=lagvalue,r=rvalue)
            x<-tryCatch(predict(model,n.ahead=aheadv),error=function(e) NULL)
            if (length(x)==4){
                pred[1,j]<-x[1,2]
                pred[2,j]<-x[2,2]  
            }
        }
        write.table(pred,"C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/VECM.Predict.csv",sep=",",
                    row.names=FALSE,append=TRUE)
        if (dir==TRUE){
          direction<-abs(sign(pred[,-1])+sign(real.data[,-1]))
          direction<-cbind(real.data[,1],direction)
          names(direction)[1]<-as.character(paste("Cluster-",(i-1),sep=""))
          write.table(direction,"C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/VECM.Direction.csv",sep=",",
                      row.names=FALSE,append=TRUE)
        } 
    }
    options(warn=0)
}