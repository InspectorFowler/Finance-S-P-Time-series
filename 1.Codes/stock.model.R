stock.model<-function(data.daily,Index.daily,Pred.daily,Ex.daily,Real.daily
                      ,dist.method="COR",clust.method="ward.D",
                      clusters=10,charts=FALSE,lag=1,rval=1,future=2){

  # Supress warning messages
  options(warn=-1)
  
  # System Save Path and source Path
  save.path<-'C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/'
  source.path<-'C:/Users/ParikshitVerma/Downloads/Finance Project/1.Codes/'
  
  #----------------------------------------------------------------------------------#
  # Creating prcnt change data files
  #----------------------------------------------------------------------------------#
  
  source(paste(source.path,'convert.R',sep=''))
  data.daily.prcnt<-convert(data.daily,dformat="%Y-%m-%d")
  Index.daily.prcnt<-convert(Index.daily,dformat="%m/%d/%Y")
  Pred.daily.prcnt<-convert(Pred.daily,dformat="%Y-%m-%d")
  Real.daily.prcnt<-convert(Real.daily,dformat="%m/%d/%Y")
  Ex.daily.prcnt<-convert(Ex.daily,dformat="%m/%d/%Y")
  
  #----------------------------------------------------------------------------------#
  # Finding the correlated clusters for each filtered out stock
  #----------------------------------------------------------------------------------#

  source(paste(source.path,'clusterindex.R',sep=''))
  source(paste(source.path,'hcluster.R',sep=''))
  source(paste(source.path,'correlation.R',sep=''))
  
  # Finding clusters, cluster index and the correlation matrix
  clusters<-hcluster(data.daily.prcnt,dist.method,clust.method,clusters)
  minidex<-clusterindex(clusters,data.daily.prcnt,dformat="%m/%d/%Y",divisor=0.145,Prcnt=FALSE,Std=FALSE)
  correlation.matrix<-correlation(minidex,Pred.daily.prcnt,charts)
  
  # Writing the clusterindex and correlation files to system
  write.csv(minidex,file=paste(save.path,'cluster.index.csv',sep=""),row.names=FALSE)
  
  write.csv(correlation.matrix,file=paste(save.path,'correlation.matrix.csv',sep=""),row.names=FALSE)
  
  # Storing the highest correlated index of each stock in a dataframe
  correlation.index<-data.frame(Company=character(ncol(correlation.matrix)-1),
                                MaxIndex=numeric(ncol(correlation.matrix)-1))
  correlation.index[,1]<-names(correlation.matrix)[-1]
  for (i in (2:ncol(correlation.matrix))){
    correlation.index[i-1,2]<-which.max(correlation.matrix[,i])
  }
  write.csv(correlation.index,file=paste(save.path,'correlation.index.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Predicting future values 
  #----------------------------------------------------------------------------------#
  
  source(paste(source.path,'VECM.model.ex.stock.R',sep=''))
  
  final.pred<-data.frame(matrix(NA,nrow=future,ncol=ncol(Pred.daily.prcnt)))
  names(final.pred)<-names(Pred.daily.prcnt)
  for (i in 1:future) final.pred[i,1]<-Pred.daily.prcnt[nrow(Pred.daily.prcnt),1]+i
  for (i in 2:ncol(Pred.daily.prcnt)){
    final.pred[,i]<-VECM.model.ex.stock(minidex[,c(1,(correlation.index[i-1,2]+1))],
                                       Pred.daily.prcnt[,c(1,i)],Ex.daily.prcnt,rvalue=rval
                                       ,lagvalue=lag,aheadv=future)[,2]
  }
  
  # Writing to system
  write.csv(final.pred,file=paste(save.path,'Reg.Stock.Prediction.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Comparing with Real stock values
  #----------------------------------------------------------------------------------#
  
  direction<-abs(sign(final.pred[,-1])+sign(Real.daily.prcnt[,-1]))/2
  direction<-cbind(Real.daily.prcnt[,1],direction)
  names(direction)<-names(final.pred)
  
  #Writing file to system
  write.csv(direction,file=paste(save.path,'Directional.Result.csv',sep=""),row.names=FALSE)
  
  Net.result<-data.frame(matrix(NA,nrow=future,ncol=2))
  names(Net.result)<-c('Date','Sum')
  Net.result[,1]<-direction[,1]
  Net.result[,2]<-rowSums(direction[,-1])
  return(Net.result)
  
  #Resetting warning signs for the rest of R session
  options(warn=0)
}