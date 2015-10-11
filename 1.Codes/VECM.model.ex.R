VECM.model.ex<-function(data,ex.data,lagvalue=1,rvalue=1,aheadv=2){
    
    # All Input datasets in PERCENTAGE FORMAT
    # data :In percentage format
    # aheadv : Number of days in future the prediction is made

    options(warn=-1)
    suppressMessages(library(tsDyn))
    suppressMessages(library(forecast))
    
    data[,1]<-as.Date(data[,1])
    cluster.pred<-data.frame(matrix(NA,nrow=aheadv,ncol=(ncol(data))))
    names(cluster.pred)<-names(data)
    for (i in 2:ncol(data)){
      model<-VECM(cbind(data[,i],ex.data[,-1]),lag=lagvalue,r=rvalue)
      x<-tryCatch(predict(model,n.ahead=aheadv),error=function(e) NULL)
      if (length(x)==(aheadv*ncol(ex.data))){
        for (j in 1:aheadv){
          cluster.pred[j,i]<-x[j,1]
          cluster.pred[j,1]<-data[nrow(data),1]+j
        }
      }
    }
    
    options(warn=0)
    return(cluster.pred)
}