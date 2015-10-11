clusterindex<-function(cluster.index,stock.data,dformat="%Y-%m-%d",
                       divisor,Prcnt=TRUE,Std=FALSE,charts=FALSE,charts2=FALSE){
    
    suppressMessages(library(ggplot2)) # Pretty plots
    suppressMessages(library(reshape2)) # melting the dataframe
    suppressMessages(library(grid)) # ggplot associate for creating grids
    suppressMessages(library(xts)) # Converting to time series data format and back
    
    # NOTE : Ensure that stock.data here is in daily format
    
    # Creating an empty dataframe to store the mini-indices
    minidex<-data.frame(matrix(0,nrow=nrow(stock.data),ncol=(1+length(unique(cluster.index)))))
    minidex[,1]<-stock.data[,1]
    
    # Loop for matching the companies to their clusters and creating the indices by addition
    for (i in 1:length(unique(cluster.index))){
        for (j in 2:ncol(stock.data)){
            if(sum((which(cluster.index==i)+1)==j)==TRUE)
            minidex[,(i+1)]<-minidex[,(i+1)]+stock.data[,j]
        }
    }
    
    # Scaling the indices by the index divisor
    minidex[,-1]<-minidex[,-1]/divisor
    
    # Modifying the data frame headers
    names(minidex)[1]<-"Date"
    for (i in 2:ncol(minidex)){
        names(minidex)[i]<-as.character(paste("Cluster-",(i-1),sep=""))
    }
    temp.minidex<-minidex
    temp.minidex$Date<-as.Date(as.character(temp.minidex$Date),format=dformat)
    
    # Converting to percentage change
    if (Prcnt==TRUE){
        stock<-minidex
        stock$Date<-as.Date(as.character(stock$Date),format=dformat)
        stock<-xts(stock[,-1],stock$Date)
        stock<-(diff(stock)/stock[-nrow(stock),]*100)
        stock<-stock[-1,]
        stock<-data.frame(Date=index(stock),coredata(stock))
        minidex<-stock
        if (Std==TRUE){
            
            # Standardizing the dataset if required
            scaled.minidex<-scale(minidex[,-1])
            scaled.minidex<-as.data.frame(scaled.minidex)
            scaled.minidex<-cbind(minidex[,1],scaled.minidex)
            minidex<-scaled.minidex
        }
    }
    names(minidex)[1]<-"Date"    
    
    # Plotting the time series data
    if(charts==TRUE){
        temp.minidex<- melt(temp.minidex,id.vars="Date")
        var<-ggplot(temp.minidex)+geom_line(aes(x = Date, y = value, colour = variable))
        print(var)
    }
    
    if(charts2==TRUE){
        vplayout<-function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
        grid.newpage()
        pushViewport(viewport(layout = grid.layout((ncol(minidex)-1),1)))
        for (i in 2:ncol(minidex)){
            dataset<-minidex[,c(1,i)]
            names(dataset)<-c("Date","value")
            var<-ggplot(dataset)+geom_line(aes(x = Date, y = value))
            print(var,vp=vplayout(i-1,1))
        }
    }
    
    # Returning the minidex to the console
    return(minidex)
}