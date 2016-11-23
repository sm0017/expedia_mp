# K nearest neighbours : Using Jaccard Similarity 
##############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Configure path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path = "/home/smita/MP/dataSet2014.csv"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat = read.csv(path, nrows = 100000)
colnames(dat)

dat$X.1 <- NULL
dat$Unnamed..0 <- NULL
dat$X <- NULL
dat$season = as.factor(dat$season)

mysample <- dat[sample(1:nrow(dat), 50000, replace=FALSE),]

train  = mysample[which(mysample$month < 8),]
test  = mysample[which(mysample$month >= 8 & mysample$is_booking==1),]

nrowTrain = nrow(train) # total rows in train set
nrowtest = nrow(test) # total rows in test set
new.dat = rbind(train, test) # concatenate test and train 

#************************************************************************************
## Accuracy Metric : MAP@K
#************************************************************************************

apk <- function(k, actual, predicted)
{
  score <- 0.0
  cnt <- 0.0
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  score
}

mapk <- function (k, actual, predicted)
{
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}

#####################################################################################
## Model - 1 : Best on the notion that user serching for similar destination
## books similar hotel cluster
#***********************************************************
#####################################################################################

#install.packages("vegan")
library(vegan)


# Calculate Jaccard similarity 
selectVar  = c("srch_destination_id", "hotel_market")
train.var = subset(train, select = selectVar)
test.var = subset(test, select = selectVar)

train.test = rbind(test.var[1,], train.var)


jaccardIndex = vegdist(train.test, method = "jaccard") # this is vector


distances = as.matrix(jaccardIndex) # convert distance matrix of n X n
testDist = distances[(nrowTrain+1):nrow(new.dat), 1:nrowTrain]

