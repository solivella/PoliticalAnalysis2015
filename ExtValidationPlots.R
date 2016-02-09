########################
## Compare BART predictions
## to QED and Birch's 
## measures.
########################

## Set RNG 
set.seed(831213)

## Get processed data
load("./FullData.RData")

## Get full bart results
load("./FullBart.RData")
all.data$fDataForMerge$BART <-  thisBartFull$yhat.train.mean


## This code just merges the data based on country/year and then plots it.
## It adjusts the placement of the text for legibility
postscript(width=6.5, height=9.75, "Both.ps")         
par(mgp=c(1,0,0), mar=c(4,2,1.5,1), tcl=0, mfrow=c(2,1), xaxt="n", yaxt="n", cex.lab=.8)
## Load & process Birch data 
birch <- read.csv("./ElectoralMalpracticeDataset.csv")
birch <- birch[,c(1,2,9,10)]
birch[birch==9] <- NA
dim(birch)
colnames(birch)<- c("country","year","voting","counting")
colnames(all.data$fDataForMerge)
birch<-birch[!duplicated(birch),]
birch[,1] <- tolower(birch$country)
birch.nelda <- merge(birch,all.data$fDataForMerge, by=c("country", "year"))
birch.nelda<-birch.nelda[!is.na(birch.nelda$counting),]
dim(birch.nelda)
with(birch.nelda, table(BART, counting))
temp<-birch.nelda$BART
temp[birch.nelda$BART>.35 & birch.nelda$counting==3& !is.na(birch.nelda$BART)] = temp[birch.nelda$BART>.35 & birch.nelda$counting==3& !is.na(birch.nelda$BART)]  +c(-.04, 0, .02)
temp[birch.nelda$BART>-.25 & birch.nelda$BART< 0 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)] = temp[birch.nelda$BART>-.25 & birch.nelda$BART< 0 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)]+  c(0,.04,0,.02,0)
temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.26 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)]=temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.26 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)] + c(-.04, -.04 , -0.02, -0.05)
temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.25 & birch.nelda$counting==1 & !is.na(birch.nelda$BART)]= temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.31 & birch.nelda$counting==1 & !is.na(birch.nelda$BART)] +c(-.04, .05, 0, 0.1, -0.01)
cor(birch.nelda$counting, birch.nelda$BART)
dim(birch.nelda) #23 observations
plot(x=birch.nelda$counting
     , y=birch.nelda$BART
     , pch=16
     , xlim=c(.9, 4.1)
     , ylim=c(-.55, 1.2)
     , ylab="BART score"
     , xlab="Electoral malpractice score"
     , col=rgb(0,100,0,50,maxColorValue=255))
cbind(birch.nelda$BART, temp)
title(paste("BART vs. Electoral Malpractice (r="
            , round(cor(birch.nelda$counting, birch.nelda$BART), 2), ")", sep="")
      ,line=.35, cex.main=.9)
mtext(1:4, side=1, at=c(1,2,3,4), cex=.8)
mtext(seq(-.5, 1, by=.5), side=2, at=seq(-.5, 1, by=.5), cex=.8)
text(paste(toupper(birch.nelda$country)
           , birch.nelda$year)
     , x=birch.nelda$counting
     , y=temp, pch=19, cex=.38)
abline(lm(BART~counting, data=birch.nelda), col="gray70")
## Load and process QED data
qed <- read.csv("./qedscores.csv")
colnames(qed)
qed <- qed[-which(is.na(qed[,6])),c(1,2,6:11)]
colnames(qed)[3:6] <- c("ballotfraud","voterfraud", "intimidate", "explicitcheat")
colnames(qed)
qed.nelda <- merge(qed,all.data$fDataForMerge, by=c("country", "year"))
dim(qed.nelda)
qed.nelda$av<-rowMeans(qed.nelda[,c("overall", "extent")], na.rm=TRUE)
qed.nelda$av<-qed.nelda$extent+qed.nelda$overall
cor(qed.nelda$BART, qed.nelda$av)
table(qed.nelda$BART, qed.nelda$av)
levels(qed.nelda$country)[1] <- "S. Africa"
levels(qed.nelda$country)[3]<- "Trinidad"
levels(qed.nelda$country)[10]<- "Bosnia"
levels(qed.nelda$country)[20]<- "Czech/Slovak"
levels(qed.nelda$country)[21]<- "Czech R."
levels(qed.nelda$country)[23]<- "Dominican R."
levels(qed.nelda$country)[24]<- "Eq. Guinea"
levels(qed.nelda$country)[31]<- "Guinea Bissau"
levels(qed.nelda$country)[42]<- "United King."
levels(qed.nelda$country)[43]<- "Sri Lanka"
levels(qed.nelda$country)[51]<- "N. Zealand"
table(qed.nelda$BART, qed.nelda$av)
temp<-qed.nelda$BART
temp[qed.nelda$BART<0 & qed.nelda$av==0]=(mean(temp[qed.nelda$BART<0 & qed.nelda$av==0] ) + (seq(-.2, .2, by=.026)))[order(temp[qed.nelda$BART<0 & qed.nelda$av==0] )]
temp[qed.nelda$BART<.25 & qed.nelda$av==1]=(median(temp[qed.nelda$BART<26 & qed.nelda$av==1] ) + (seq(-.35, .35, by=.03)))[order(temp[qed.nelda$BART<.25 & qed.nelda$av==1] )]
temp[qed.nelda$BART>.3 & qed.nelda$av==2]=(median(temp[qed.nelda$BART>.3 & qed.nelda$av==2])) + c(.02, -.02)
temp[qed.nelda$BART<.1  & qed.nelda$BART>0  & qed.nelda$av==2]=temp[qed.nelda$BART<.1  & qed.nelda$BART>0  & qed.nelda$av==2]+c(0.02, -.02)
length(seq(-.35, .35, by=.03))
## Create PDF with plots in Figure 7
plot(x=qed.nelda$av, y=qed.nelda$BART, pch=16, xlim=c(-.2, 5.1), ylim=c(-.7, 1.4), ylab="BART score", xlab="QED score", col=rgb(0,100,0,50,maxColorValue=255))
abline(lm(BART~av, data=qed.nelda), col="gray70")
text(paste(toupper(qed.nelda$country), qed.nelda$year), x=qed.nelda$av, y=temp, pch=19, cex=.29)
title(paste("BART vs. QED (r=", round(cor(qed.nelda$BART, qed.nelda$av),2), ")", sep=""), line=.35, cex.main=.8)
mtext(0:5, side=1, at=c(0,1,2,3,4,5), cex=.8)
mtext(seq(-.5, 1, by=.5), side=2, at=seq(-.5, 1, by=.5), cex=.8)
dev.off()




## This code just merges the data based on country/year and then plots it.
## It adjusts the placement of the text for legibility
pdf(width=6.5, height=9.75, "Both.pdf")         
par(mgp=c(1,0,0), mar=c(4,2,1.5,1), tcl=0, mfrow=c(2,1), xaxt="n", yaxt="n", cex.lab=.8)
## Load & process Birch data 
birch <- read.csv("./ElectoralMalpracticeDataset.csv")
birch <- birch[,c(1,2,9,10)]
birch[birch==9] <- NA
dim(birch)
colnames(birch)<- c("country","year","voting","counting")
colnames(all.data$fDataForMerge)
birch<-birch[!duplicated(birch),]
birch[,1] <- tolower(birch$country)
birch.nelda <- merge(birch,all.data$fDataForMerge, by=c("country", "year"))
birch.nelda<-birch.nelda[!is.na(birch.nelda$counting),]
dim(birch.nelda)
with(birch.nelda, table(BART, counting))
temp<-birch.nelda$BART
temp[birch.nelda$BART>.35 & birch.nelda$counting==3& !is.na(birch.nelda$BART)] = temp[birch.nelda$BART>.35 & birch.nelda$counting==3& !is.na(birch.nelda$BART)]  +c(-.04, 0, .02)
temp[birch.nelda$BART>-.25 & birch.nelda$BART< 0 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)] = temp[birch.nelda$BART>-.25 & birch.nelda$BART< 0 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)]+  c(0,.04,0,.02,0)
temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.26 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)]=temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.26 & birch.nelda$counting==2 & !is.na(birch.nelda$BART)] + c(-.04, -.04 , -0.02, -0.05)
temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.25 & birch.nelda$counting==1 & !is.na(birch.nelda$BART)]= temp[birch.nelda$BART>-.51 & birch.nelda$BART< -.31 & birch.nelda$counting==1 & !is.na(birch.nelda$BART)] +c(-.04, .05, 0, 0.1, -0.01)
cor(birch.nelda$counting, birch.nelda$BART)
dim(birch.nelda) #23 observations
plot(x=birch.nelda$counting
     , y=birch.nelda$BART
     , pch=16
     , xlim=c(.9, 4.1)
     , ylim=c(-.55, 1.2)
     , ylab="BART score"
     , xlab="Electoral malpractice score"
     , col=rgb(0,100,0,50,maxColorValue=255))
cbind(birch.nelda$BART, temp)
title(paste("BART vs. Electoral Malpractice (r="
            , round(cor(birch.nelda$counting, birch.nelda$BART), 2), ")", sep="")
      ,line=.35, cex.main=.9)
mtext(1:4, side=1, at=c(1,2,3,4), cex=.8)
mtext(seq(-.5, 1, by=.5), side=2, at=seq(-.5, 1, by=.5), cex=.8)
text(paste(toupper(birch.nelda$country)
           , birch.nelda$year)
     , x=birch.nelda$counting
     , y=temp, pch=19, cex=.38)
abline(lm(BART~counting, data=birch.nelda), col="gray70")
## Load and process QED data
qed <- read.csv("./qedscores.csv")
colnames(qed)
qed <- qed[-which(is.na(qed[,6])),c(1,2,6:11)]
colnames(qed)[3:6] <- c("ballotfraud","voterfraud", "intimidate", "explicitcheat")
colnames(qed)
qed.nelda <- merge(qed,all.data$fDataForMerge, by=c("country", "year"))
dim(qed.nelda)
qed.nelda$av<-rowMeans(qed.nelda[,c("overall", "extent")], na.rm=TRUE)
qed.nelda$av<-qed.nelda$extent+qed.nelda$overall
cor(qed.nelda$BART, qed.nelda$av)
table(qed.nelda$BART, qed.nelda$av)
levels(qed.nelda$country)[1] <- "S. Africa"
levels(qed.nelda$country)[3]<- "Trinidad"
levels(qed.nelda$country)[10]<- "Bosnia"
levels(qed.nelda$country)[20]<- "Czech/Slovak"
levels(qed.nelda$country)[21]<- "Czech R."
levels(qed.nelda$country)[23]<- "Dominican R."
levels(qed.nelda$country)[24]<- "Eq. Guinea"
levels(qed.nelda$country)[31]<- "Guinea Bissau"
levels(qed.nelda$country)[42]<- "United King."
levels(qed.nelda$country)[43]<- "Sri Lanka"
levels(qed.nelda$country)[51]<- "N. Zealand"
table(qed.nelda$BART, qed.nelda$av)
temp<-qed.nelda$BART
temp[qed.nelda$BART<0 & qed.nelda$av==0]=(mean(temp[qed.nelda$BART<0 & qed.nelda$av==0] ) + (seq(-.2, .2, by=.026)))[order(temp[qed.nelda$BART<0 & qed.nelda$av==0] )]
temp[qed.nelda$BART<.25 & qed.nelda$av==1]=(median(temp[qed.nelda$BART<26 & qed.nelda$av==1] ) + (seq(-.35, .35, by=.03)))[order(temp[qed.nelda$BART<.25 & qed.nelda$av==1] )]
temp[qed.nelda$BART>.3 & qed.nelda$av==2]=(median(temp[qed.nelda$BART>.3 & qed.nelda$av==2])) + c(.02, -.02)
temp[qed.nelda$BART<.1  & qed.nelda$BART>0  & qed.nelda$av==2]=temp[qed.nelda$BART<.1  & qed.nelda$BART>0  & qed.nelda$av==2]+c(0.02, -.02)
length(seq(-.35, .35, by=.03))
## Create PDF with plots in Figure 7
plot(x=qed.nelda$av, y=qed.nelda$BART, pch=16, xlim=c(-.2, 5.1), ylim=c(-.7, 1.4), ylab="BART score", xlab="QED score", col=rgb(0,100,0,50,maxColorValue=255))
abline(lm(BART~av, data=qed.nelda), col="gray70")
text(paste(toupper(qed.nelda$country), qed.nelda$year), x=qed.nelda$av, y=temp, pch=19, cex=.29)
title(paste("BART vs. QED (r=", round(cor(qed.nelda$BART, qed.nelda$av),2), ")", sep=""), line=.35, cex.main=.8)
mtext(0:5, side=1, at=c(0,1,2,3,4,5), cex=.8)
mtext(seq(-.5, 1, by=.5), side=2, at=seq(-.5, 1, by=.5), cex=.8)
dev.off()



