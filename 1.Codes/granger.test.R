granger.test<-function(stock.data,cluster.index){
  
  # NOTE : Both datasets should be in %age change format
  
  suppressMessages(library(lmtest)) # Granger test
  suppressMessages(library(vars)) # VARselect for optimal lag value
  
  granger.data<-data.frame(matrix(NA, nrow=(ncol(stock.data)-1), ncol=ncol(cluster.index)))
  names(granger.data)<-c("Companies",names(cluster.index)[-1])
  for (i in 2:ncol(stock.data)){
    optimal.lag<-2
    granger.data[i-1,1]<-names(stock.data)[i]
    for (j in 2:ncol(cluster.index)){
      granger.data[i-1,j]<-grangertest(stock.data[,i]~cluster.index[,j],order=optimal.lag)$Pr[2]
    }
  }
  return(granger.data)
}