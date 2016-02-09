#########################################
## Load and analyze block-bootstrap model 
## estimation to assess generalization
## error. 
#########################################

## Load estimation results
load("./GenErrorsBootFull.RData")
load("./GenErrorsBootForen.RData")
load("./GenErrorsBootContext.RData")

## Collect all BART estimations in a list
errors.gen.boot  <- list(full.errors.boot
                    ,foren.errors.boot
                    ,context.errors.boot)


## Obtain mean prediction LOO errors
## as defined by Efron & Tibshirani 1997.
## We focus on mean, as no SE is defined for 
## medians
out.mean  <- laply(1:3,function(x)errors.gen.boot[[x]]$error[c(5,1,2)])
rownames(out.mean) <- c("Full", "Forensics", "Context")
out.mean  <- as.data.frame(out.mean)
out.mean$ConsRank  <- rowMeans(apply(out.mean,2,rank))

## Add standard errors to mean errors
out.se  <- round(laply(1:3,function(x)errors.gen.boot[[x]]$error.se[c(5,1,2)]),2)
out.se  <- as.data.frame(out.se)
out.se$ConsRank  <- NA
rownames(out.se) <- c("SE Full", "SE Forensics", "SE Context")

## Form full table, and print
out  <- rbind(out.mean[1,]
              ,out.se[1,]
              ,out.mean[2,]
              ,out.se[2,]
              ,out.mean[3,]
              ,out.se[3,])
colnames(out) <- c("RMSE", "MAE", "MAPE","ConsRank")
print(round(out,4))

