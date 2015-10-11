VECM.model.ex.stock<-function(cluster.data,pred.data,ex.data,lagvalue=1,rvalue=1,aheadv=2){
    
    # All Input datasets in PERCENTAGE FORMAT
    # cluster.data : Single cluster data frame with date column
    # Pred.data : Single Prediction stock with date column
    # aheadv : Number of days in future the prediction is made
    
    options(warn=-1)
    suppressMessages(library(tsDyn))
    suppressMessages(library(forecast))
    
    pred.data[,1]<-as.Date(pred.data[,1])
    pred<-data.frame(matrix(NA,nrow=aheadv,ncol=(ncol(pred.data))))
    names(pred)<-names(pred.data)
    model<-VECM(cbind(cluster.data[,2],pred.data[,2],ex.data[,-1]),lag=lagvalue,r=rvalue)
    x<-tryCatch(predict(model,n.ahead=aheadv),error=function(e) NULL)
    if (length(x)==(aheadv*(ncol(ex.data)+1))){
      for (i in 1:aheadv){
        pred[i,1]<-pred.data[nrow(pred.data),1]+i
        pred[i,2]<-x[i,2]
      }
    }
    options(warn=0)
    return(pred)
}