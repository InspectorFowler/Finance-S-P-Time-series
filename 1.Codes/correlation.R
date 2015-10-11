correlation<-function(stock.data,pred.data,charts=FALSE){

    # Calculating cross correlation of prediction stocks with the others
    corr.table<-data.frame(matrix(NA,nrow=(ncol(stock.data)-1),ncol=(ncol(pred.data)-1)))
    corr.table[,1]<-names(stock.data)[2:ncol(stock.data)]
    names(corr.table)[1]<-"Indices"
    par(mfrow=c(5,4))
    for(j in 2:ncol(pred.data)){
        corr<-numeric(ncol(stock.data)-1)
        for (i in 2:ncol(stock.data)){
            y<-ccf(pred.data[,j],stock.data[,i],lag.max=0,type="correlation",plot=FALSE)
            corr[i-1]<-as.numeric(y$acf)
        }
        corr.table[,j]<-corr
        names(corr.table)[j]<-names(pred.data)[j]
        
        if(charts==TRUE){
            # Setting margins and plotting the values
            plot.table<-cbind(corr.table[,1],corr)
            par(mar=c(7,3,4,1)+.1)
            plot(plot.table[,2],type="h",xaxt="n",yaxt="n",ylab="",xlab="",main=names(pred.data)[j],
                 lwd=5,cex.axis=0.8,las=1)
            axis(1,at=1:(ncol(stock.data)-1),tick=FALSE,labels=names(stock.data)[2:ncol(stock.data)],
                 cex.axis=0.8,las=2)
            axis(2,at=seq(0,1,by=0.05),cex.axis=0.8,las=2)
        }
    }
    return(corr.table)
}