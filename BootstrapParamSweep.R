#!/usr/bin/env Rscript
#####################
# Bootstrap for
# parameter selection
#####################

# Flag to send job to cluster or 
# run in a regular computer (still in parallel,
# using MPI backend)
PegasusRun  <- commandArgs()

if(PegasusRun){
  system("bsub < BSSweep.job")
}else{
#!/usr/bin/env Rscript
#Load libraries
library(plyr)
library(doMPI)
library(BayesTree)

#Set RNG seed
set.seed(831213)

source("./GenErrorAuxFunc.R")

## Create cluster and (implicitly) run workerLoop function;
## Commands before the cluster initialization are run
## by master and workers alike, while commands after
## it are only run by master. 
cl  <- startMPIcluster()
registerDoMPI(cl)




## Get processed data
load("./FullData.RData")
train.data  <- all.data$fData[all.data$fData[,"train"]==1,]
train.countries  <- all.data$countries.full[all.data$fData[,"train"]==1]


##Set nr. of bootstraps
n.boot.samp <- 100

## Create parameter grid, and focus on
## parameter combinations discussed in the BART
## paper by Chipman et al.
bartParams <- expand.grid(
  k=c(1,2)
  ,pow=c(0.5,1)
  ,base=c(0.95,0.99)
  ,sigdf=c(3,10)
  ,sigquant=c(.75,.9)
  ,ntree=c(100,200)
  ,burnin=50e3
  ,niter=5e3
  ,keepevery=10
  ,sampind =1:n.boot.samp
)
bartParams <- bartParams[with(bartParams,which((sigquant==0.75&sigdf==10)|(sigquant>0.75&sigdf==3))),]
bartParams  <- as.matrix(bartParams)

## Store parameter combinations as rownames for later retrieval
rownames(bartParams) <- apply(bartParams,1,paste,collapse="-")

## Create boostrap samples
boot.data  <- new.Boot(n.boot.samp
                          ,data=train.data
                       )
## Estimate BARTs using different parameter combinations
## in parallel
bart.res  <- alply(bartParams
                   ,1
                   ,bartWrap
                   ,data.arg=boot.data$data
                   ,.parallel=TRUE
                   #,.paropts=list(.packages=c("plyr","BayesTree"))                   
)

## Calculate loss functions
boot.err.dis  <- ldply(bart.res,lossGen,lasso=FALSE)

## Retrieve sample number and parameter combinations
boot.err.dis$boot.sample  <- gsub(".*-([0-9]*)$","\\1",boot.err.dis$X1)
boot.err.dis$parameters  <- gsub("(.*)-[0-9]*$","\\1",boot.err.dis$X1)

## Get mean errors across resamples for each parameter combination
boot.errors.ind  <- ddply(boot.err.dis,c("parameters","obs"),function(x)numcolwise(mean)(x))
boot.errors  <- ddply(boot.errors.ind,"parameters",function(x)numcolwise(mean)(x))

## Separate parameter values
boot.errors  <- cbind(do.call(rbind,strsplit(boot.errors$parameters,"-"))[,1:6],boot.errors[,-c(1:2)])
colnames(boot.errors)[1:6]  <- colnames(bartParams)[1:6]

## Get ranks of each parameter combination
## for each loss measure
boot.errors  <- boot.errors[,-11]
rankings <- colwise(rank)(boot.errors[,-c(1:6)])
colnames(rankings) <- outer(colnames(rankings),".rank",FUN=paste,sep="")
boot.errors <- cbind(boot.errors,rankings)

## Calcualate mean rank for each parameter combination
boot.errors$avg.rank <-  rowMeans(rankings)#rowMeans(rankings[,c(2,3,7)])
boot.errors <- boot.errors[order(boot.errors$avg.rank),]

## Output: CSV with parameter combinations, error values, and
## ranks.
write.csv(boot.errors,file="./BSParamSweep.csv",row.names=FALSE)

## Finalize parallel backend
closeCluster(cl)
mpi.finalize()
}





