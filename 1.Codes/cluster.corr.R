cluster.corr<-function(stock.data.cluster,stock.data,pred.data,divisor,dist.method,clust.method,clusters,
                       dformat="%Y-%m-%d",Detrend=FALSE,Prcnt=TRUE,Std=FALSE,charts=FALSE,indexseries=FALSE){
    
    # WARNING : USE THE PERCENT CHANGE VERSION OF PRED DATA
    #           Processing parameters only work on the creation of minidex
    #           
    # The code doesn't convert Pred.data to percentage change automatically
  
    # Sourcing the user defined functions
    source('C:/Users/ParikshitVerma/Downloads/Finance Project/1.Codes/clusterindex.R')
    source('C:/Users/ParikshitVerma/Downloads/Finance Project/1.Codes/hcluster.R')
    source('C:/Users/ParikshitVerma/Downloads/Finance Project/1.Codes/correlation.R')
  
    # Creating clusters, mini-indices and finding correlations
    clusters<-hcluster(stock.data.cluster,dist.method,clust.method,clusters,FALSE)
    x<-clusterindex(clusters,stock.data,dformat,divisor,Prcnt,Std)
    y<-correlation(x,pred.data,charts)
    par(mfrow=c(1,1))
    if (indexseries==TRUE){
      write.csv(x,file=
                  "C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/output.csv",
                row.names=FALSE)
      return(x)
    } 
    else{
      write.csv(y,file=
                  "C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/output.csv",
                row.names=FALSE)
      return(y)
    }
}