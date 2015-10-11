spregression<-function(Index.data,data.daily,coeff=FALSE){
  reg.data<-data.frame(Date=character(nrow(Index.data)))
  reg.data$Date<-Index.data$Date
  coeff.data<-data.frame(Company=character(ncol(data.daily)-1),
                         Intercept=numeric(ncol(data.daily)-1),
                         coef=numeric(ncol(data.daily)-1))
  coeff.data[,1]<-names(data.daily)[-1]
  for (i in 2:ncol(data.daily)){
    fit<-lm(data.daily[,i]~Index.data[,2])
    reg.data[,i]<-as.data.frame(residuals(fit))
    names(reg.data)[i]<-names(data.daily)[i]
    coeff.data[i-1,2]<-summary(fit)$coefficients[1,1]
    coeff.data[i-1,3]<-summary(fit)$coefficients[2,1]
  }
  if (coeff==TRUE) return(coeff.data)
  else return(reg.data)
}