#!/usr/bin/env Rscript
##########################################
## Script to estimate fraud proxy score
## using IRT model
#########################################

set.seed(831213)

## Load data
nelda <- read.dta("./id & q-wide.dta")
temp.countriesNELDA <- read.csv("./countriesNelda.csv")
temp.countriesNELDA[,1] <- gsub("\\s","",temp.countriesNELDA[,1]) 
names(temp.countriesNELDA)[1] <- "country"
nelda$country <- temp.countriesNELDA[,1]
nelda <- subset(nelda
                ,grepl("-L1$",electionid) #Only first round legislative elections
                ,select=c(country
                          , year
                          , electionid
                          , nelda11
                          , nelda32
                          , nelda34
                          , nelda30
                          , nelda48
                          , nelda49
                          , nelda47
                          ))
nelda <- nelda[order(nelda$country,nelda$year,nelda$electionid),]

## Format data to be used by ltm()
nelda <- apply(nelda,2,recode,recodes="'yes'=1;
               'unclear'=1;
               'no'=0;
               'N/A'=0"
                )  

## Keep first election if first round election 
## was repeated.
nelda <- subset(nelda, !duplicated(nelda[,c(1,2)])) 

colnames(nelda)
colnames(nelda[,4:10])
dim(nelda)

## Estimate full IRT model
jj <- matrix(as.numeric(nelda[,c(4:10)]), ncol=7, byrow=FALSE)
head(jj)
irt.model <- ltm(jj~z1, IRT.param=TRUE)


output <- factor.scores(irt.model, resp.patterns = jj)
newScores <- output$score.dat$z1
fraud.pos <- data.frame(newScores,nelda[,c(1,2)])
names(fraud.pos)[1] <- "fraud.score"


## Alternative IRT scores including only nelda32, nelda48 and nelda49 items
kk <- matrix(as.numeric(nelda[,c(5,8,9)]), ncol=3, byrow=FALSE)
head(kk)
irt.model.kk <- ltm(kk~z1, IRT.param=TRUE)

output.kk <- factor.scores(irt.model.kk, resp.patterns = kk)
newScores <- output.kk$score.dat$z1
fraud.pos$fraud.score.3item <- newScores


