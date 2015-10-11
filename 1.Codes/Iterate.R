Iterate<-function(data.daily,Index.daily,Pred.daily,Ex.daily,Real.daily
                  ,max.lag){
  x<-stock.model.single(data.daily,Index.daily,Pred.daily,Ex.daily,
                 Real.daily)
  names(x)[2]<-paste('lag',1,sep="-")
  for (i in 2:max.lag){
    y<-stock.model.single(data.daily,Index.daily,Pred.daily,Ex.daily,
                   Real.daily,lag=i)
    x<-cbind(x,y[,2])
    names(x)[1+i]<-paste('lag',i,sep="-")
  }
  return(x)
}