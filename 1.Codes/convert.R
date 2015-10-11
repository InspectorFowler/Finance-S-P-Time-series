convert<-function(stock,std=FALSE,dformat="%m/%d/%Y"){
    suppressMessages(library(xts)) 
  
    # NOTE : Check the date format before executing code
    names(stock)[1]<-"Date"
    stock$Date<-as.Date(as.character(stock$Date),format=dformat)
    stock<-xts(stock[,-1],stock$Date)
    stock<-(diff(stock)/stock[-nrow(stock),]*100)
    stock<-stock[-1,]
    stock<-data.frame(Date=index(stock),coredata(stock))
    if (std==TRUE){
        
        # Standardizing the dataset if required
        scaled.stock<-scale(stock[,-1])
        scaled.stock<-as.data.frame(scaled.stock)
        scaled.stock<-cbind(stock[,1],scaled.stock)
        stock<-scaled.stock
    }
    return(stock)
}