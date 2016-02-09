#######################
## BART illustration
#######################

## Seed RNG
set.seed(831213)

## If you feed this function a matrix of observed digit distributions, 
## and a vector of expected distributions, it will calcualte the 
## probabilty that it is "fraud", or that the deviation is too large 
## under the null hypothesis/Chi2 distribution
## Note that the default expectation is Benford's law for the fsd
probsCalculator <- function(expectedDigit=c(.301, .176),
                            observedDigit=matrix(c(.9, .8, .1, .9), ncol=2)){
  
  .oneProbCalculator <- function(x){
    myChiSquared <- sum((expectedDigit-x)^2/expectedDigit)
    myDF <- length(expectedDigit)
    return(pchisq(myChiSquared, myDF))
  }
  
  return(aaply(as.matrix(observedDigit), 1, .oneProbCalculator))
  
}

## Function to run BART on fake data, 
## and compare to actual density
bartTest <- function(targetData, sampleSize=350, k=2, beta=2, alpha = 0.95, nTrees=200, niter=1e3, burnin=1e3, complex=FALSE){
  simSampleSize=sampleSize
  n.digits = dim(targetData)[2]
  simulatedInputs <- data.frame(replicate(n.digits,runif(simSampleSize)))
  names(simulatedInputs) <- names(targetData)
  print(head(simulatedInputs))
  if (complex==FALSE){  simulatedFraud <- rbinom(n=simSampleSize, prob=probsCalculator(observedDigit=simulatedInputs), size=1)}
  if(complex==TRUE){ simulatedFraud <- rbinom(n=simSampleSize, prob=probsCalculator2(observedDigit=simulatedInputs), size=1)}
  fraud.bart <- bart(x.train=simulatedInputs
                     ,y.train=simulatedFraud
                     ,x.test=targetData
                     ,k = k
                     ,power = beta
                     ,base = alpha
                     ,nskip=burnin
                     ,ndpost=niter
                     ,ntree=nTrees
                     ,verbose=TRUE
  )
  return(colMeans(pnorm(fraud.bart$yhat.test)))  
}


## make a plot showing the probability density
digit1 <- digit2 <- seq(0,1, by=.05)
possibleDigits <- expand.grid(digit1, digit2)
probFraud <- probsCalculator(observedDigit=possibleDigits)
df <- data.frame(possibleDigits, probFraud,"Actual (Benford)")
colnames(df) <- c("d1", "d2", "probFraud","Type")


## Run vanilla BART
bartProb <- bartTest(subset(df,select=-c(probFraud,Type)))
bartPred <- data.frame(subset(df,select=-c(probFraud,Type)), bartProb,"Predicted (Benford)")
colnames(bartPred) <- c("d1", "d2", "probFraud","Type")     
all.together.benf <- rbind(df,bartPred)


## Can BART pick it up if there is a much less "smooth" probability surface.  
probsCalculator2 <- function(expectedDigit=c(.5, .5),
                             observedDigit=matrix(c(.9, .8, .1, .9), ncol=2)){
  .oneProbCalculator <- function(x){
    mystat <- (((expectedDigit-x)^2)/expectedDigit)*sqrt(prod(x))
    #    mystat <- mystat-(expectedDigit)*prod(x)^2
    mystat <- (mystat/prod((expectedDigit-x)))[1]
    return(pcauchy(mystat))
  }
  return(aaply(as.matrix(observedDigit), 1, .oneProbCalculator))
}
probFraud.complex <- probsCalculator2(observedDigit=possibleDigits)
df.complex <- data.frame(possibleDigits, probFraud.complex, "Actual (complex)")
colnames(df.complex) <- c("d1", "d2", "probFraud","Type")

## BART again, for the more complex surface
bartProb.complex <- bartTest(subset(df.complex,select=-c(probFraud,Type))
                             ,sampleSize = 2e3
                             , k = 0.5 # bigger k generates 
                             # more informative priors (less overfitting). 
                             # Fit seems to be more sensitive to this. than
                             # to beta and alpha.
                             , beta = 4 # increasing either beta or alpha puts
                             # higher prior prob. on bigger trees,
                             # which effectively helps overfit.
                             , alpha = 1
                             , nTrees = 25
                             , niter = 1e3
                             , burnin = 1e3
                             , complex = TRUE)

bartPred.complex <- data.frame(subset(df.complex,select=-c(probFraud,Type)), bartProb.complex,"Predicted (complex)")
colnames(bartPred.complex) <- c("d1", "d2", "probFraud","Type")     
all.together.complex <- rbind(df.complex,bartPred.complex)



## Really all together
all.together <- rbind(all.together.benf,all.together.complex)
postscript("./BartDemo.ps",width=8, height=8)
wireframe(probFraud~d1*d2|Type
          , par.settings= standard.theme("pdf", color=FALSE),
          , xlim=c(0,1)
          ,ylim=c(0,1)
          , shade=TRUE
          ,light.source=c(0,10,10) 
          ,layout=c(2,2)
          ,index.cond=list(c(3,4,1,2))
          , zlab="Fraud"
          ,data=all.together
          ,strip = function(..., bg,par.strip.text){
            strip.default(..., bg="gray80"
                          ,par.strip.text=list(alpha=1
                                               ,cex=1.05
                                               ,col="black"
                                               ,font=2
                          ))})
dev.off()