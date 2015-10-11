stock.model.double<-function(data.daily,Index.daily,Pred.daily,Ex.daily,Real.daily
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
  # Filtering out SnP500 effect from the Stock.daily
  #----------------------------------------------------------------------------------#
  
  source(paste(source.path,'spregression.R',sep=''))
  reg.daily.prcnt<-spregression(Index.daily.prcnt,data.daily.prcnt)
  reg.coeff.daily<-spregression(Index.daily.prcnt,data.daily.prcnt,TRUE)
  reg.Pred.daily.prcnt<-spregression(Index.daily.prcnt,Pred.daily.prcnt)
  reg.coeff.Pred.daily<-spregression(Index.daily.prcnt,Pred.daily.prcnt,TRUE)
  
  #Writing the regression coefficients to system
  write.csv(reg.Pred.daily.prcnt,file=paste(save.path,'Reg.Pred.daily1.csv',sep=""),row.names=FALSE)
  write.csv(reg.coeff.Pred.daily,file=paste(save.path,'Regression.Coefficients1.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Finding the correlated clusters for each filtered out stock
  #----------------------------------------------------------------------------------#

  source(paste(source.path,'clusterindex.R',sep=''))
  source(paste(source.path,'hcluster.R',sep=''))
  source(paste(source.path,'correlation.R',sep=''))
  
  # Finding clusters, cluster index and the correlation matrix
  clusters<-hcluster(reg.daily.prcnt,dist.method,clust.method,clusters)
  minidex<-clusterindex(clusters,reg.daily.prcnt,dformat="%m/%d/%Y",divisor=0.145,Prcnt=FALSE,Std=FALSE)
  correlation.matrix<-correlation(minidex,reg.Pred.daily.prcnt,charts)
  
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
  # Filtering out cluster Index effect from the filtered stock data
  #----------------------------------------------------------------------------------#
  
  # Creating an empty dataframe for second filtered stock data and its coefficients
  reg2.Pred.daily.prcnt<-data.frame(matrix(NA,nrow=nrow(reg.Pred.daily.prcnt),ncol=ncol(reg.Pred.daily.prcnt)))
  names(reg2.Pred.daily.prcnt)<-names(reg.Pred.daily.prcnt)
  reg2.Pred.daily.prcnt[,1]<-reg.Pred.daily.prcnt[,1]
  
  reg2.coeff.Pred.daily<-data.frame(matrix(NA,nrow=nrow(reg.coeff.Pred.daily),ncol=ncol(reg.coeff.Pred.daily)))
  names(reg2.coeff.Pred.daily)<-names(reg.coeff.Pred.daily)
  reg2.coeff.Pred.daily[,1]<-reg.coeff.Pred.daily[,1]
  
  for (i in 2:ncol(reg.Pred.daily.prcnt)){
    reg2.Pred.daily.prcnt[,i]<-spregression(minidex[,c(1,(correlation.index[i-1,2]+1))],reg.Pred.daily.prcnt[,c(1,i)])[,2]
    reg2.coeff.Pred.daily[i-1,c(2,3)]<-spregression(minidex[,c(1,(correlation.index[i-1,2]+1))],reg.Pred.daily.prcnt[,c(1,i)],TRUE)[1,c(2:3)]
  }
  
  #Writing the regression coefficients to system
  write.csv(reg2.Pred.daily.prcnt,file=paste(save.path,'Reg.Pred.daily2.csv',sep=""),row.names=FALSE)
  write.csv(reg2.coeff.Pred.daily,file=paste(save.path,'Regression.Coefficients2.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Finding the Causal clusters for each double filtered out stock using Grangers test
  #----------------------------------------------------------------------------------#
  
  # Running Granger Test
  source(paste(source.path,'granger.test.R',sep=''))
  granger.data<-granger.test(reg2.Pred.daily.prcnt,minidex)
  
  #Storing the lowest p-value index for each stock in dataframe
  granger.index<-data.frame(Company=character(nrow(granger.data)),
                            MinIndex=numeric(nrow(granger.data)))
  granger.index[,1]<-granger.data[,1]
  for (i in (1:nrow(granger.data))){
    granger.index[i,2]<-which.min(granger.data[i,])-1
  }
  
  # Writing to System
  write.csv(granger.index,file=paste(save.path,'granger.index.csv',sep=""),row.names=FALSE)
  
  write.csv(granger.data,file=paste(save.path,'granger.data.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Predicting future values for the cluster indices
  #----------------------------------------------------------------------------------#
  
  source(paste(source.path,'VECM.model.ex.R',sep=''))
  cluster.pred<-VECM.model.ex(minidex,Ex.daily.prcnt,lagvalue=lag,rvalue=rval,aheadv=future)
  
  # Writing to system
  write.csv(cluster.pred,file=paste(save.path,'cluster.Prediction.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Predicting future values for double filtered stock data using the granger causal cluster
  #----------------------------------------------------------------------------------#
  
  source(paste(source.path,'VECM.model.ex.stock.R',sep=''))
  
  reg2.pred<-data.frame(matrix(NA,nrow=future,ncol=ncol(reg2.Pred.daily.prcnt)))
  names(reg2.pred)<-names(reg2.Pred.daily.prcnt)
  for (i in 1:future) reg2.pred[i,1]<-reg2.Pred.daily.prcnt[nrow(reg2.Pred.daily.prcnt),1]+i
  for (i in 2:ncol(reg2.Pred.daily.prcnt)){
    reg2.pred[,i]<-VECM.model.ex.stock(minidex[,c(1,(granger.index[i-1,2]+1))],
                                       reg2.Pred.daily.prcnt[,c(1,i)],Ex.daily.prcnt,rvalue=rval
                                       ,lagvalue=lag,aheadv=future)[,2]
  }
  
  # Writing to system
  write.csv(reg2.pred,file=paste(save.path,'Reg.Stock.Prediction.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Predicting future values for SnP 500 Index
  #----------------------------------------------------------------------------------#
  
  SnP.pred<-VECM.model.ex(Index.daily.prcnt,Ex.daily.prcnt,rvalue=rval,lagvalue=lag,aheadv=future)
  
  # Writing to system
  write.csv(SnP.pred,file=paste(save.path,'SnP.Prediction.csv',sep=""),row.names=FALSE)
  
  #----------------------------------------------------------------------------------#
  # Adding all the predicted values using the regression coefficients
  #----------------------------------------------------------------------------------#
  
  pen.pred<-data.frame(matrix(NA,nrow=nrow(reg2.pred),ncol=ncol(reg2.pred)))
  pen.pred[,1]<-reg2.pred[,1]
  names(pen.pred)<-names(reg2.pred)
  for (i in 2:ncol(reg2.pred)){
    for (j in 1:future){
      pen.pred[j,i]<-((cluster.pred[j,(correlation.index[i-1,2]+1)]*reg2.coeff.Pred.daily[i-1,3])
                      +reg2.coeff.Pred.daily[i-1,2]+reg2.pred[j,i])
    }
  }
  
  final.pred<-data.frame(matrix(NA,nrow=nrow(pen.pred),ncol=ncol(pen.pred)))
  final.pred[,1]<-pen.pred[,1]
  names(final.pred)<-names(pen.pred)
  for (i in 2:ncol(pen.pred)){
    for (j in 1:future){
      final.pred[j,i]<-((SnP.pred[j,2]*reg.coeff.Pred.daily[i-1,3])
                      +reg.coeff.Pred.daily[i-1,2]+pen.pred[j,i])
    }
  }
  
  #Writing file to system
  write.csv(pen.pred,file=paste(save.path,'Penultimate.Prediction.csv',sep=""),row.names=FALSE)
  write.csv(final.pred,file=paste(save.path,'Final.Prediction.csv',sep=""),row.names=FALSE)
  
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