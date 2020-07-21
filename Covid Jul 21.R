# Jul 21, 2020
library(parallel)

# USA
# starting June 30
y <- c(2588017,2640626,2695495,2752704,2803397,2846350,2893083,2944191,3006650,3064093,3131114,3193522,3254486,3312422,3374256,3439626,3509610,3585701,3651090,3714681,3770577)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1;plot(growth)
daily_change <- median(growth[(length(growth)-6):length(growth)])
weekly_change <- mean(growth[(length(growth)-6):length(growth)])

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
y_scale <- seq(0,40,length=25)
x_shift <- seq(0,500,length=25)
power <- seq(1,3,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=25)
parameter_array <- expand.grid(y_scale,x_shift,power,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
casegrow_USA <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(casegrow_USA)
fit_DG <- casegrow_USA[4]+(casegrow_USA[1]*x_forecast+casegrow_USA[2])^casegrow_USA[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(0,(max(y)-min(y_subset))*25,length=100)+max(y)
Po <- seq(-3,3,length=25)
r <- seq(-.2,0.2,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=50)
parameter_array <- expand.grid(K,Po,r,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
caselog_USA <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(caselog_USA)
fit_DL <- caselog_USA[4]+caselog_USA[1]/(1+exp(caselog_USA[2]+caselog_USA[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_shift <- floor(min(y_subset)/100000)*100000
y_max <- 5800000-y_shift
y_middle <- y_max-0
y_limit <- 5500000-y_shift
predicted <- 3518275
date_offset <- 22
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max),density=50)
polygon(c(20,21,21,20),c(0,0,predicted-y_shift,predicted-y_shift),col="darkorange",density=50)
polygon(c(20,21,21,20),c(0,0,predicted-y_shift,predicted-y_shift),density=0)
range1 <- paste(format(as.Date(21,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range2 <- paste(format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range3 <- paste(format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(42,origin=Sys.Date()-date_offset),"%b%d"),sep="")
text(45.25,y_limit*.72,range1,col="black",cex=1.3)
text(45.25,y_limit*.66,paste("+",round(fit_DM[length(fit_DM)-14]-max(y)),sep=""),col="darkorange",cex=2.4)
text(45.25,y_limit*.86,range2,col="black",cex=1.3)
text(45.25,y_limit*.8,paste("+",round(fit_DM[length(fit_DM)-7]-fit_DM[length(fit_DM)-14]),sep=""),col="darkorange",cex=2.4)
text(45.25,y_limit*1,range3,col="black",cex=1.3)
text(45.25,y_limit*.94,paste("+",round(fit_DM[length(fit_DM)]-fit_DM[length(fit_DM)-7]),sep=""),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:41+.5,labels=format(as.Date(1:42,origin=Sys.Date()-date_offset),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.5,at=y_axis,labels=y_shift+y_axis)
mtext("United States",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(45.25,y_limit*1.18,"7-Day Median",cex=1.3)
text(45.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(45.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
text(20.5,y_middle*.58,"Predicted:",cex=1.3)
text(20.5,y_middle*.52,predicted,col="darkorange",cex=2.5)
text(20.5,y_middle*.72,"July 20:",cex=1.3)
text(20.5,y_middle*.66,y[length(y)],cex=2.5)

# India
# starting July 1
y <- c(585493,604641,625544,648315,673165,697413,719665,742417,767296,793802,820916,849553,878254,906752,936181,968876,1003832,1038716,1077618,1118043,1155191)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1;plot(growth)
daily_change <- median(growth[(length(growth)-6):length(growth)])
weekly_change <- mean(growth[(length(growth)-6):length(growth)])

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
y_scale <- seq(0,20,length=25)
x_shift <- seq(0,500,length=25)
power <- seq(1,3,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=25)
parameter_array <- expand.grid(y_scale,x_shift,power,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
casegrow_INDIA <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(casegrow_INDIA)
fit_DG <- casegrow_INDIA[4]+(casegrow_INDIA[1]*x_forecast+casegrow_INDIA[2])^casegrow_INDIA[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(0,(max(y)-min(y_subset))*10,length=100)+max(y)
Po <- seq(-3,3,length=25)
r <- seq(-.2,0.2,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=50)
parameter_array <- expand.grid(K,Po,r,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
caselog_INDIA <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(caselog_INDIA)
fit_DL <- caselog_INDIA[4]+caselog_INDIA[1]/(1+exp(caselog_INDIA[2]+caselog_INDIA[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_shift <- floor(min(y_subset)/100000)*100000
y_max <- 2200000-y_shift
y_middle <- y_max-0
y_limit <- 2000000-y_shift
predicted <- 1039082
date_offset <- 21
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max),density=50)
polygon(c(20,21,21,20),c(0,0,predicted-y_shift,predicted-y_shift),col="darkorange",density=50)
polygon(c(20,21,21,20),c(0,0,predicted-y_shift,predicted-y_shift),density=0)
range1 <- paste(format(as.Date(21,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range2 <- paste(format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range3 <- paste(format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(42,origin=Sys.Date()-date_offset),"%b%d"),sep="")
text(45.25,y_limit*.72,range1,col="black",cex=1.3)
text(45.25,y_limit*.66,paste("+",round(fit_DM[length(fit_DM)-14]-max(y)),sep=""),col="darkorange",cex=2.4)
text(45.25,y_limit*.86,range2,col="black",cex=1.3)
text(45.25,y_limit*.8,paste("+",round(fit_DM[length(fit_DM)-7]-fit_DM[length(fit_DM)-14]),sep=""),col="darkorange",cex=2.4)
text(45.25,y_limit*1,range3,col="black",cex=1.3)
text(45.25,y_limit*.94,paste("+",round(fit_DM[length(fit_DM)]-fit_DM[length(fit_DM)-7]),sep=""),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:41+.5,labels=format(as.Date(1:42,origin=Sys.Date()-date_offset),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.5,at=y_axis,labels=y_shift+y_axis)
mtext("India",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(45.25,y_limit*1.18,"7-Day Median",cex=1.3)
text(45.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(45.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
text(20.5,y_middle*.58,"Predicted:",cex=1.3)
text(20.5,y_middle*.52,predicted,col="darkorange",cex=2.5)
text(20.5,y_middle*.72,"July 21:",cex=1.3)
text(20.5,y_middle*.66,y[length(y)],cex=2.5)

# UK
# starting July 2 (excluding 2 values before a revision 312654,313483)
y <- c(283757,284276,284900,285416,285768,286349,286979,287621,288133,288953,289603,290133,291373,291911,292552,293239,294066,294792,295372)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1;plot(growth)
daily_change <- median(growth[(length(growth)-6):length(growth)])
weekly_change <- mean(growth[(length(growth)-6):length(growth)])

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-18):length(x)]
y_subset <- y[(length(y)-18):length(y)]

# case growth
y_scale <- seq(0,20,length=25)
x_shift <- seq(1000,2000,length=25)
power <- seq(.5,2.5,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=25)
parameter_array <- expand.grid(y_scale,x_shift,power,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
casegrow_UK <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(casegrow_UK)
fit_DG <- casegrow_UK[4]+(casegrow_UK[1]*x_forecast+casegrow_UK[2])^casegrow_UK[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(0,(max(y)-min(y_subset))*5,length=100)+max(y)
Po <- seq(-3,3,length=25)
r <- seq(-.2,0.2,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=50)
parameter_array <- expand.grid(K,Po,r,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
caselog_UK <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(caselog_UK)
fit_DL <- caselog_UK[4]+caselog_UK[1]/(1+exp(caselog_UK[2]+caselog_UK[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_shift <- floor(min(y_subset)/10000)*10000
y_max <- 312500-y_shift
y_middle <- y_max-5000
y_limit <- 310000-y_shift
predicted <- 296422
date_offset <- 22-2
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max),density=50)
ref <- max(y)-y_shift
polygon(c(20,21,21,20)-2,c(ref,ref,predicted-y_shift,predicted-y_shift),col="darkorange",density=50)
polygon(c(20,21,21,20)-2,c(ref,ref,predicted-y_shift,predicted-y_shift),density=0)
range1 <- paste(format(as.Date(21,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range2 <- paste(format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range3 <- paste(format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(42,origin=Sys.Date()-date_offset),"%b%d"),sep="")
text(45.25-2,y_limit*.72,range1,col="black",cex=1.3)
text(45.25-2,y_limit*.66,paste("+",round(fit_DM[length(fit_DM)-14]-max(y)),sep=""),col="darkorange",cex=2.4)
text(45.25-2,y_limit*.86,range2,col="black",cex=1.3)
text(45.25-2,y_limit*.8,paste("+",round(fit_DM[length(fit_DM)-7]-fit_DM[length(fit_DM)-14]),sep=""),col="darkorange",cex=2.4)
text(45.25-2,y_limit*1,range3,col="black",cex=1.3)
text(45.25-2,y_limit*.94,paste("+",round(fit_DM[length(fit_DM)]-fit_DM[length(fit_DM)-7]),sep=""),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:(41-2)+.5,labels=format(as.Date(1:(42-2),origin=Sys.Date()-date_offset),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.5,at=y_axis,labels=y_shift+y_axis)
mtext("United Kingdom",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(45.25-2,y_limit*1.18,"7-Day Median",cex=1.3)
text(45.25-2,y_limit*1.14,"Daily Growth:",cex=1.3)
text(45.25-2,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
text(20.5-2,y_middle*(.58+.25),"Predicted:",cex=1.3)
text(20.5-2,y_middle*(.52+.25),predicted,col="darkorange",cex=2.5)
text(20.5-2,y_middle*(.72+.25),"July 20:",cex=1.3)
text(20.5-2,y_middle*(.66+.25),y[length(y)],cex=2.5)

# Canada
# starting June 30
y <- c(104204,104420,104771,105090,105316,105535,105934,106166,106433,106804,107125,107346,107590,108155,108486,108827,109264,109669,109999,110338,111124)
x <- 1:length(y)
daily <- c(1,y[-1]-y[-length(y)])
weekly <- c();for (i in 1:(length(daily)-6)){weekly <- c(weekly,mean(daily[i:(i+6)]))}
plot(weekly);growth <- weekly[-1]/weekly[-length(weekly)]-1;plot(growth)
daily_change <- median(growth[(length(growth)-6):length(growth)])
weekly_change <- mean(growth[(length(growth)-6):length(growth)])

x_forecast <- 1:(length(y)+21)
par(mfrow=c(1,1),mar=c(2.1,3.1,0.1,2.1),bg='antiquewhite',xpd=T,lwd=1)
plot(y~x)
x_subset <- x[(length(x)-20):length(x)]
y_subset <- y[(length(y)-20):length(y)]

# case growth
y_scale <- seq(0,20,length=25)
x_shift <- seq(0,1000,length=25)
power <- seq(.5,2.5,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=25)
parameter_array <- expand.grid(y_scale,x_shift,power,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-(parameter_array[input,1]*x_subset+parameter_array[input,2])^parameter_array[input,3])^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
casegrow_CANADA <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(casegrow_CANADA)
fit_DG <- casegrow_CANADA[4]+(casegrow_CANADA[1]*x_forecast+casegrow_CANADA[2])^casegrow_CANADA[3]
plot(fit_DG~x_forecast,type="l");points(y_subset~x_subset,pch=16)

# case logistic
K <- seq(0,(max(y)-min(y_subset))*5,length=100)+max(y)
Po <- seq(-3,3,length=25)
r <- seq(-.2,0.2,length=25)
y_shift <- seq(min(y_subset)/2,min(y_subset),length=50)
parameter_array <- expand.grid(K,Po,r,y_shift)
combinations <- 1:nrow(parameter_array)
fit_fx <- function(input){sum((y_subset-parameter_array[input,4]-parameter_array[input,1]/(1+exp(parameter_array[input,2]+parameter_array[input,3]*x_subset)))^2)}
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster,c("x_subset","y_subset","parameter_array"))
system.time(sum_squares <- as.numeric(parLapply(cluster,combinations,fit_fx)))
caselog_CANADA <- as.numeric(parameter_array[which(sum_squares==min(sum_squares,na.rm=T)),]);print(caselog_CANADA)
fit_DL <- caselog_CANADA[4]+caselog_CANADA[1]/(1+exp(caselog_CANADA[2]+caselog_CANADA[3]*x_forecast))
plot(fit_DG~x_forecast,type="l",col="red",lwd=3);lines(fit_DL~x_forecast,col="green",lwd=3);points(y_subset~x_subset,pch=16)
fit_DM <- .5*fit_DG+.5*fit_DL;lines(fit_DM~x_forecast,col="goldenrod1",lwd=3)

# case graph
par(mar=c(4.1,4.1,3.1,7.1),mfrow=c(1,1),bg='antiquewhite',xpd=T,lwd=2)
options(scipen=5)
y_shift <- floor(min(y_subset)/1000)*1000
y_max <- 122000-y_shift
y_middle <- y_max-2000
y_limit <- 120000-y_shift
predicted <- 110305
date_offset <- 22
barplot(y_subset-y_shift,space=0,col="goldenrod1",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max))
par(new=T)
barplot(c(rep(NA,length(y_subset)),fit_DM[-(1:max(x_subset))])-y_shift,space=0,col="darkorange",las=1,yaxt="n",xlim=c(0,length(x_forecast)-x_subset[1]),ylim=c(0,y_max),density=50)
ref <- max(y)-y_shift
polygon(c(20,21,21,20),c(ref,ref,predicted-y_shift,predicted-y_shift),col="darkorange",density=50)
polygon(c(20,21,21,20),c(ref,ref,predicted-y_shift,predicted-y_shift),density=0)
range1 <- paste(format(as.Date(21,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range2 <- paste(format(as.Date(28,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),sep="")
range3 <- paste(format(as.Date(35,origin=Sys.Date()-date_offset),"%b%d"),"-",format(as.Date(42,origin=Sys.Date()-date_offset),"%b%d"),sep="")
text(45.25,y_limit*.72,range1,col="black",cex=1.3)
text(45.25,y_limit*.66,paste("+",round(fit_DM[length(fit_DM)-14]-max(y)),sep=""),col="darkorange",cex=2.4)
text(45.25,y_limit*.86,range2,col="black",cex=1.3)
text(45.25,y_limit*.8,paste("+",round(fit_DM[length(fit_DM)-7]-fit_DM[length(fit_DM)-14]),sep=""),col="darkorange",cex=2.4)
text(45.25,y_limit*1,range3,col="black",cex=1.3)
text(45.25,y_limit*.94,paste("+",round(fit_DM[length(fit_DM)]-fit_DM[length(fit_DM)-7]),sep=""),col="darkorange",cex=2.4)
axis(1,pos=0,hadj=.8,las=3,cex.axis=1.5,at=0:41+.5,labels=format(as.Date(1:42,origin=Sys.Date()-date_offset),"%b%d"),tick=F)
y_axis <- axis(2,pos=0,lwd=5,las=2,cex.axis=1,hadj=20)
axis(2,pos=0,lwd=5,las=2,cex.axis=1.5,at=y_axis,labels=y_shift+y_axis)
mtext("Canada",side=3,line = -0.5,cex=5)
text(-2.5,y_limit*1.14,"COVID",cex=2)
text(-2.5,y_limit*1.08,"Cases",cex=2)
text(45.25,y_limit*1.18,"7-Day Median",cex=1.3)
text(45.25,y_limit*1.14,"Daily Growth:",cex=1.3)
text(45.25,y_limit*1.08,paste(round(daily_change*100,1),"%",sep=""),col="black",cex=2.5)
text(20.5,y_middle*(.58+.15),"Predicted:",cex=1.3)
text(20.5,y_middle*(.52+.15),predicted,col="darkorange",cex=2.5)
text(20.5,y_middle*(.72+.15),"July 20:",cex=1.3)
text(20.5,y_middle*(.66+.15),y[length(y)],cex=2.5)