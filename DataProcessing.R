############################
# Data processing file
# for fraud project
############################


data.preproc  <- function(){
  fDataTrain <- read.csv("./FraudDatasetTrain.csv")
  fDataTrain$train  <- 1
  
  fDataTest <- read.csv("./FraudDatasetTest.csv")
  fDataTest$train  <- 0
  
  fData <- rbind(fDataTrain,fDataTest)
  
  fData$commission  <- as.factor(fData$commission)
  fData$instability  <- as.factor(fData$instability)
  
  
  ##Discretization of five variables (three with most combined missing values, demtenure and gdpchange).
  ##For gdpchange, divide into quartiles
 fData$gdpchange <- factor(cut(fData$gdpchange
                               ,quantile(fData$gdpchange,seq(0,1,1/4),na.rm=TRUE)
                               ,include.lowest=TRUE),exclude=NULL)
  
  ##For demtenure, divide into SortOf democracy, RealDeal democracy, and NA.
  fData$demtenure <- factor(ifelse(fData$demtenure>=10,1,0),exclude=NULL)
  
  ##For polity.score, cut into democracies, anocracies and autocracies
  ## See Marshall and Cole 2011, p. 9.
  fData$lagged.polity <- factor(cut(fData$lagged.polity
                                    ,c(-10,-6,5,10)
                                    ,include.lowest=TRUE),exclude=NULL)
  
  #Create new regime variable based on polity and demtenure
  switch.wrap <- function(x,...){switch(x,...)}
  switchV <- Vectorize(switch.wrap,"x")
  fData$regime <- as.factor(switchV(with(fData,paste(demtenure,lagged.polity,sep="."))
                                    ,"0.[-10,-6]"="Autocracy"
                                    ,"0.(5,10]"="New Democracy"
                                    ,"0.(-6,5]"="Anocracy"
                                    ,"0.NA"="New Democracy"
                                    ,"1.(5,10]"="Old Democracy"
                                    ,"1.(-6,5]"="Anocracy"
                                    ,"1.NA"="Old Democracy"
                                    ,"NA.[-10,-6]" ="Autocracy"
                                    ,"NA.(5,10]" ="New Democracy"
                                    ,"NA.(-6,5]" = "Anocracy"
                                    ,"NA.NA"="NA"
  ))
  
  #For inequality, cut using quartiles
  fData$inequality <- factor(cut(fData$inequality
                                 ,quantile(fData$inequality,seq(0,1,1/4),na.rm=TRUE)
                                 ,include.lowest=TRUE),exclude=NULL)
  #For average magnitude, cut using quartiles
  fData$avgmag <- factor(cut(fData$avgmag
                             ,quantile(fData$avgmag,seq(0,1,1/4),na.rm=TRUE)+c(0,sort(runif(3)),0) # quantiles 0 and .25 are the same.
                             ,include.lowest=TRUE),exclude=NULL)
  
  #Casewise deletion of remaining variables.
  fData <- fData[complete.cases(fData[,-c(which(colnames(fData)=="lagged.fraudscore"))]),]
  fData$uid <- 1:nrow(fData)
  
  fDataForMerge <- fData
  
  
  
  ## Select forensics and context indicators
  fData <- subset(fData,select=c(
                                 Uniform.LastDigit
                                 ,DistanceLastPair
                                 ,Mean.SecondDigit
                                 ,Benford.SecondDigit
                                 ,regime
                                 ,fraud.democracy
                                 ,inequality
                                 ,fract
                                 ,urban
                                 ,turnout
                                 ,avgmag
                                 ,gdpchange
                                 ,commission
                                 ,instability
                                 ,country
                                 ,year
                                 ,train
                            ,uid
  ))
  
  
  #Create forensics-only subset
  fDataForen <- subset(fData,select=c(
                                      Uniform.LastDigit
                                      ,DistanceLastPair
                                      ,Mean.SecondDigit
                                      ,Benford.SecondDigit
                                      ,fraud.democracy 
                                      ,country
                                      ,year
                                      ,uid
                                      ,train
  ))
  
  #Create context-only subset 
  fDataInform <- subset(fData,select=c(
    regime
    ,fraud.democracy
    ,inequality
    ,fract
    ,urban
    ,turnout
    ,avgmag
    ,gdpchange
    ,commission
    ,instability
    ,country
    ,year
    ,uid
    ,train
  ))
  
  
  
  #Get country for block bootrap
  countries.full <- fData$country
  fData <- fData[,-grep("country",colnames(fData))]

  countries.foren <- fDataForen$country
  fDataForen  <- fDataForen[,-grep("country",colnames(fDataForen))]
 
  countries.inform <- fDataInform$country
  fDataInform  <- fDataInform[,-grep("country",colnames(fDataInform))]
  
  #Convert to BART-readable data
  fData <- makeind(fData)
  fDataForen <- makeind(fDataForen)
  fDataInform <- makeind(fDataInform)
  
 #Return list of subsets and country vectors.
  return(list(fData=fData
              ,fDataForen=fDataForen
              ,fDataInform=fDataInform
              ,countries.full=countries.full
              ,countries.foren=countries.foren
              ,countries.inform=countries.inform
              ,fDataForMerge=fDataForMerge))
}
