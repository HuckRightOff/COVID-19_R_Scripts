# Jun 23, 2020
library(parallel)

# USA
# starting June 1
y <- c(1790074,1810113,1829792,1850767,1874055,1897436,1915971,1932272,1949452,1969698,1987546,2011968,2037910,2059051,2077358,2100406,2124026,2151108,2182035,2213998,2242093,2268651)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1
daily_change <- growth[length(growth)]
weekly_change <- mean(growth[(length(growth)-6):length(growth)])
plot(growth)

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
a <- seq(1,101,by=.5)
b <- seq(0,1000,by=10)
c <- seq(1,3,by=.05)
parameter_array <- expand.grid(a,b,c)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathgrow_USA <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathgrow_USA)
fit_DG <- (deathgrow_USA[1]*x_forecast+deathgrow_USA[2])^deathgrow_USA[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(max(y),max(y)*5,length=500)
Po <- seq(.1,6,by=.1)
r <- seq(-.2,0.2,by=.01)
parameter_array <- expand.grid(K,Po,r)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathlog_USA <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathlog_USA)
fit_DL <- deathlog_USA[1]/(1+exp(deathlog_USA[2]+deathlog_USA[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_subset[1]
y_shift <- 1800000
y_max <- 2900000-y_shift
y_middle <- 2800000-y_shift
y_limit <- 2800000-y_shift
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DG[-(1:max(x_subset))])-y_shift,space=0,col="red",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DL[-(1:max(x_subset))])-y_shift,space=0,col="darkgreen",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.72,"Next Week:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.66,round(fit_DM[length(fit_DM)-14]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.86,"In 2 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.8,round(fit_DM[length(fit_DM)-7]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1,"In 3 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.94,round(fit_DM[length(fit_DM)]),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:(length(x_forecast)-x_subset[1])+.5,labels=format(as.Date(1:(length(x_forecast)-x_subset[1]+1),origin=Sys.Date()-22),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.5,at=y_axis,labels=y_shift+y_axis)
mtext("United States",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.18,"Weekday-Adj.",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
# text(20.5,y_middle*.875,"Predicted:",cex=1.3)
# text(20.5,y_middle*.825,39197,col="darkorange",cex=2.5)
text(20.5,y_limit*.72,"June 22:",cex=1.3)
text(20.5,y_limit*.66,y[length(y)],cex=2.5)

# India
# starting June 3
y <- c(207615,216919,226770,236657,246628,256611,266598,276583,286579,297535,308993,320922,332424,343091,354065,366946,380532,395048,410461,425282,440215)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1
daily_change <- growth[length(growth)]
weekly_change <- mean(growth[(length(growth)-6):length(growth)])
plot(growth)

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
a <- seq(1,101,by=.5)
b <- seq(0,1000,by=10)
c <- seq(1,3,by=.05)
parameter_array <- expand.grid(a,b,c)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathgrow_INDIA <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathgrow_INDIA)
fit_DG <- (deathgrow_INDIA[1]*x_forecast+deathgrow_INDIA[2])^deathgrow_INDIA[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(max(y),max(y)*5,length=500)
Po <- seq(.1,6,by=.1)
r <- seq(-.2,0.2,by=.01)
parameter_array <- expand.grid(K,Po,r)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathlog_INDIA <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathlog_INDIA)
fit_DL <- deathlog_INDIA[1]/(1+exp(deathlog_INDIA[2]+deathlog_INDIA[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_subset[1]
y_shift <- 200000
y_max <- 860000-y_shift
y_middle <- 800000-y_shift
y_limit <- 800000-y_shift
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DG[-(1:max(x_subset))])-y_shift,space=0,col="red",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DL[-(1:max(x_subset))])-y_shift,space=0,col="darkgreen",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.72,"Next Week:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.66,round(fit_DM[length(fit_DM)-14]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.86,"In 2 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.8,round(fit_DM[length(fit_DM)-7]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1,"In 3 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.94,round(fit_DM[length(fit_DM)]),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:(length(x_forecast)-x_subset[1])+.5,labels=format(as.Date(1:(length(x_forecast)-x_subset[1]+1),origin=Sys.Date()-21),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.75,at=y_axis,labels=y_shift+y_axis)
mtext("India",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.18,"Weekday-Adj.",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
# text(20.5,y_middle*.875,"Predicted:",cex=1.3)
# text(20.5,y_middle*.825,39197,col="darkorange",cex=2.5)
text(20.5,y_limit*.72,"June 23:",cex=1.3)
text(20.5,y_limit*.66,y[length(y)],cex=2.5)

# UK
# starting June 1
y <- c(276332,277985,279856,281661,283311,284868,286194,287399,289140,290143,291409,292950,294375,295889,296857,298136,299251,300469,301815,303110,304331,305289)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1
daily_change <- growth[length(growth)]
weekly_change <- mean(growth[(length(growth)-6):length(growth)])
plot(growth)

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
a <- seq(-10,10,by=.1)
b <- seq(0,1000,by=10)
c <- seq(.5,3,by=.05)
parameter_array <- expand.grid(a,b,c)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathgrow_UK <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathgrow_UK)
fit_DG <- (deathgrow_UK[1]*x_forecast+deathgrow_UK[2])^deathgrow_UK[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(max(y),max(y)*5,length=500)
Po <- seq(-3,3,by=.1)
r <- seq(-.2,0.2,by=.01)
parameter_array <- expand.grid(K,Po,r)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathlog_UK <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathlog_UK)
fit_DL <- deathlog_UK[1]/(1+exp(deathlog_UK[2]+deathlog_UK[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_subset[1]
y_shift <- 270000
y_max <- 347500-y_shift
y_middle <- 340000-y_shift
y_limit <- 340000-y_shift
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DG[-(1:max(x_subset))])-y_shift,space=0,col="red",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DL[-(1:max(x_subset))])-y_shift,space=0,col="darkgreen",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.72,"Next Week:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.66,round(fit_DM[length(fit_DM)-14]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.86,"In 2 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.8,round(fit_DM[length(fit_DM)-7]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1,"In 3 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.94,round(fit_DM[length(fit_DM)]),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:(length(x_forecast)-x_subset[1])+.5,labels=format(as.Date(1:(length(x_forecast)-x_subset[1]+1),origin=Sys.Date()-22),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.75,at=y_axis,labels=y_shift+y_axis)
mtext("United Kingdom",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.18,"Weekday-Adj.",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
# text(20.5,y_middle*.875,"Predicted:",cex=1.3)
# text(20.5,y_middle*.825,39197,col="darkorange",cex=2.5)
text(20.5,y_limit*.72,"June 22:",cex=1.3)
text(20.5,y_limit*.66,y[length(y)],cex=2.5)

# Canada
# starting June 1
y <- c(91705,92410,93085,93726,94335,95057,95699,96244,96652,97125,97530,97945,98410,98787,99147,99467,99853,100220,100629,101019,101337,101637)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1
daily_change <- growth[length(growth)]
weekly_change <- mean(growth[(length(growth)-6):length(growth)])
plot(growth)

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
a <- seq(-10,10,by=.1)
b <- seq(0,1000,by=10)
c <- seq(.5,3,by=.05)
parameter_array <- expand.grid(a,b,c)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathgrow_CANADA <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathgrow_CANADA)
fit_DG <- (deathgrow_CANADA[1]*x_forecast+deathgrow_CANADA[2])^deathgrow_CANADA[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(max(y),max(y)*5,length=500)
Po <- seq(-3,3,by=.1)
r <- seq(-.2,0.2,by=.01)
parameter_array <- expand.grid(K,Po,r)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(NLL <- as.numeric(parLapply(cluster,combinations,fit_fx)))
deathlog_CANADA <- as.numeric(parameter_array[which(NLL==min(NLL,na.rm=T)),]);print(deathlog_CANADA)
fit_DL <- deathlog_CANADA[1]/(1+exp(deathlog_CANADA[2]+deathlog_CANADA[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_subset[1]
y_shift <- 91000
y_max <- 112750-y_shift
y_middle <- 111000-y_shift
y_limit <- 111000-y_shift
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DG[-(1:max(x_subset))])-y_shift,space=0,col="red",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DL[-(1:max(x_subset))])-y_shift,space=0,col="darkgreen",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.72,"Next Week:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.66,round(fit_DM[length(fit_DM)-14]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.86,"In 2 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.8,round(fit_DM[length(fit_DM)-7]),col="darkorange",cex=2.4)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1,"In 3 Weeks:",col="black",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*.94,round(fit_DM[length(fit_DM)]),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:(length(x_forecast)-x_subset[1])+.5,labels=format(as.Date(1:(length(x_forecast)-x_subset[1]+1),origin=Sys.Date()-22),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.75,at=y_axis,labels=y_shift+y_axis)
mtext("Canada",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.18,"Weekday-Adj.",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(length(x_forecast)-x_subset[1]+4.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
# text(20.5,y_middle*.875,"Predicted:",cex=1.3)
# text(20.5,y_middle*.825,39197,col="darkorange",cex=2.5)
text(20.5,y_limit*.72,"June 22:",cex=1.3)
text(20.5,y_limit*.66,y[length(y)],cex=2.5)
