Read.stock<-function(index,days,period,Home=TRUE){
    
  # Creating an empty numeric vector
  x <- vector(mode="numeric")
  
  # Pulling maximum data available for each stock in the given timeframe
  for (i in 1:nrow(index)){
    temp<-nrow(read.csv(paste("http://www.google.com/finance/getprices?i=",
                              period,"&p=",days,"d&f=d,o,h,l,c,v&df=cpct&q=",
                              as.character(index[i,"Symbol"]),sep=""),skip=7,
                        header=FALSE,colClasses=c("character","numeric",rep("NULL",4))))
    x<-c(x,temp)            
  }
    
  # Creating the dataframe for storing all the stock data starting with the first entry
    stock.data<-read.csv(paste("http://www.google.com/finance/getprices?i=",
                               period,"&p=",days,"d&f=d,o,h,l,c,v&df=cpct&q=",
                               as.character(index[1,"Symbol"]),sep=""),skip=7,
                         header=FALSE,colClasses=c("character","numeric",rep("NULL",4)))
    
    stock.data[,3]<-1
    stock.data[,3]<-as.POSIXct(stock.data[,3],origin="1970-01-01")
    
    for (i in 1:nrow(stock.data)){
      if(nchar(stock.data[i,1])==11){
        x<-as.numeric(gsub("[^0-9]","",stock.data[i,1]))
        stock.data[i,3]<-as.POSIXct(x,origin="1970-01-01")
      }
      else stock.data[i,3]<-stock.data[i-1,3]+60
    }
    
    stock.data[,4]<-stock.data[,2]
    stock.data<-stock.data[,-c(1,2)]
    names(stock.data)<-c("Timestamp",as.character(index[1,"Symbol"]))
    
    # Pulling all the remaining stock data from Yahoo! finance
    for (i in 2:nrow(index)){
        dataset<-read.csv(paste("http://www.google.com/finance/getprices?i=",
                                period,"&p=",days,"d&f=d,o,h,l,c,v&df=cpct&q=",
                                as.character(index[i,"Symbol"]),sep=""),skip=7,
                          header=FALSE,colClasses=c("NULL","numeric",rep("NULL",4)))
        names(dataset)<-as.character(index[i,"Symbol"])
        stock.data<-cbind(stock.data,dataset)            
    }
    
    #Renaming headers to full company name
    colnames(stock.data)<-c("Date",as.character(paste(index[,1],index[,3],sep=":")))
    
    #output and writing to system
    if (Home==TRUE){
      write.csv(stock.data,file=
                  "C:/Users/akhilesh/Desktop/Course Materials/ISEN 613/Final Project/2.Excel Data/temp.csv",
                row.names=FALSE)
    }
    else
      write.csv(stock.data,file=
                  "C:/Users/ParikshitVerma/Downloads/Finance Project/2.Excel Data/output.csv",
                row.names=FALSE)
    
    stock.data
}