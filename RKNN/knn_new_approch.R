# K nearest neighbours : Using Jaccard Similarity 
##############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Configure path
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
path = "/home/smita/RF_models/final_data_RF4_small.csv"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dat = read.csv(path)
colnames(dat)

dat$X.1 <- NULL
dat$Unnamed..0 <- NULL
dat$X <- NULL
dat$season = as.factor(dat$season)

train  = dat[which(dat$month < 8),]
test  = dat[which(dat$month >= 8),]

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

library(vegan)

# Calculate Jaccard similarity 
selectVar  = c("srch_destination_id", "srch_destination_type_id")
calc.dist = subset(new.dat, select = selectVar)
jaccardIndex = vegdist(calc.dist, method = "jaccard") # this is vector
distances = as.matrix(jaccardIndex) # convert distance matrix of n X n

#We only want distances of test data from the training data:  Hence some manipulation
testDist = distances[(nrowTrain+1):nrow(new.dat), 1:nrowTrain]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We loop through each row of distance matrix, sort the distances
# in ascending order and select the top five nearest neighbour 
# which are close to the each test-record
# We get the hotel_cluster values for those top 5 nearest neighbours 
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outcome = list()

for ( t in 1:nrow(test)){
  
  hotel_clusters = c() 
  
  # start with empty result for hotel_clusters
  sort.dist = order(testDist[t,], decreasing = TRUE)
  sort.dist = sort.dist[1:5]

  # following loop add top 5 hotel_clusters one by one
  
  for (i in 1:length(sort.dist)){ 
      d = new.dat[sort.dist[i],"hotel_cluster"]
      hotel_clusters = append(hotel_clusters,d)
     
  }  
  h1 = list(hotel_clusters)
  outcome = append(outcome, h1)
}

new = list()
for (i in 1:nrow(test)){ 
  d = test$hotel_cluster[i]
  cl = rep(d, 5)
  new = append(new, list(cl))
}

result_dat = data.frame(cbind(outcome, new))
final_model1 <- data.frame(result_dat)
model1.Score = mapk(5, final_model1$new, final_model1$outcome)

rm(jaccardIndex, distances, result_dat)


############################################################
# Model - 2 : Based on hotel-search similarity
#***********************************************************
############################################################

# Calculate Jaccard similarity 
selectVar  = c("hotel_country", "hotel_market")
calc.dist = subset(new.dat, select = selectVar)
jaccardIndex = vegdist(calc.dist, method = "jaccard") # this is vector
distances = as.matrix(jaccardIndex) # convert distance matrix of n X n

#We only want distances of test data from the training data:  Hence some manipulation
testDist = distances[(nrowTrain+1):nrow(new.dat), 1:nrowTrain]

outcome = list()

for ( t in 1:nrow(test)){
  
  hotel_clusters = c() 
  
  # start with empty result for hotel_clusters
  sort.dist = order(testDist[t,])
  sort.dist = sort.dist[1:5]

  # following loop add top 5 hotel clusters one by one
  
  for (i in 1:length(sort.dist)){ 
      d = new.dat[sort.dist[i], "hotel_cluster"]
      hotel_clusters = append(hotel_clusters,d)
     
  }  
  h1 = list(hotel_clusters)
  outcome = append(outcome, h1)
}

new = list()
for (i in 1:nrow(test)){ 
  d = test$hotel_cluster[i]
  cl = rep(d, 5)
  new = append(new, list(cl))
}

result_dat = data.frame(cbind(outcome, new))
final_model2 <- data.frame(result_dat)
model2.Score = mapk(5, final_model2$new, final_model2$outcome)
# Free memory
rm(jaccardIndex, distances, result_dat)

############################################################
# Model - 3 based on user Similarity
#***********************************************************
############################################################

# Calculate Jaccard similarity 
selectVar  = c("user_location_country", "user_location_region", "user_location_city")
calc.dist = subset(new.dat, select = selectVar)
jaccardIndex = vegdist(calc.dist, method = "jaccard") # this is vector
distances = as.matrix(jaccardIndex) # convert distance matrix of n X n

#We only want distances of test data from the training data:  Hence some manipulation
testDist = distances[(nrowTrain+1):nrow(new.dat), 1:nrowTrain]
outcome = list()

for ( t in 1:nrow(test)){
  
  hotel_clusters = c() 
  
  # start with empty result for hotel_clusters
  sort.dist = order(testDist[t,])
  sort.dist = sort.dist[1:5]

  # following loop add top 5 hotel clusters one by one
  
  for (i in 1:length(sort.dist)){ 
      d = new.dat[sort.dist[i], "hotel_cluster"]
      hotel_clusters = append(hotel_clusters,d)
     
  }  
  h1 = list(hotel_clusters)
  outcome = append(outcome, h1)
}

new = list()
for (i in 1:nrow(test)){ 
  d = test$hotel_cluster[i]
  cl = rep(d, 5)
  new = append(new, list(cl))
}

result_dat = data.frame(cbind(outcome, new))
final_model3 <- data.frame(result_dat)
model3.Score = mapk(5, final_model3$new, final_model3$outcome)
# Free memory
rm(jaccardIndex, distances, result_dat)


############################################################
# Model - 4 : Based on user, seasons
#***********************************************************
############################################################

# Calculate Jaccard similarity 
    
selectVar  = c("user_location_country", "user_location_region", "season")
calc.dist = subset(new.dat, select = selectVar)
calc.dist$season = as.numeric(calc.dist$season)
jaccardIndex = vegdist(calc.dist, method = "jaccard") # this is vector
distances = as.matrix(jaccardIndex) # convert distance matrix of n X n

#We only want distances of test data from the training data:  Hence some manipulation
testDist = distances[(nrowTrain+1):nrow(new.dat), 1:nrowTrain]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# We loop through each row of distance matrix ,sort the distances
# in ascending order and select the top five nearest neighbour 
# which are close to the each test-record
# We get the hotel_cluster values for those top 5 nearest neighbours 
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outcome = list()

for ( t in 1:nrow(test)){
  
  hotel_clusters = c() 
  
  # start with empty result for hotel_clusters
  sort.dist = order(testDist[t,])
  sort.dist = sort.dist[1:5]

  # following loop add top 5 hotel clusters one by one
  
  for (i in 1:length(sort.dist)){ 
      d = new.dat[sort.dist[i],"hotel_cluster"]
      hotel_clusters = append(hotel_clusters,d)
     
  }  
  h1 = list(hotel_clusters)
  outcome = append(outcome, h1)
}

new = list()
for (i in 1:nrow(test)){ 
  d = test$hotel_cluster[i]
  cl = rep(d, 5)
  new = append(new, list(cl))
}


result_dat = data.frame(cbind(outcome, new))
final_model4 <- data.frame(result_dat)
model4.Score = mapk(5, final_model4$new, final_model4$outcome)
# Free memory
rm(jaccardIndex, distances, result_dat)

############################################################
# Model - 5 best on similarity between users and destination type
############################################################

# Calculate Jaccard similarity    
selectVar  = c("user_location_region", "srch_destination_type_id")
calc.dist = subset(new.dat, select = selectVar)
jaccardIndex = vegdist(calc.dist, method = "jaccard") # this is vector
distances = as.matrix(jaccardIndex) # convert distance matrix of n X n

#We only want distances of test data from the training data:  Hence some manipulation
testDist = distances[(nrowTrain+1):nrow(new.dat), 1:nrowTrain]

outcome = list()

for ( t in 1:nrow(test)){
  
  hotel_clusters = c() 
  
  # start with empty result for hotel_clusters
  sort.dist = order(testDist[t,])
  sort.dist = sort.dist[1:5]

  # following loop add top 5 hotel clusters one by one
  
  for (i in 1:length(sort.dist)){ 
      d = new.dat[sort.dist[i],"hotel_cluster"]
      hotel_clusters = append(hotel_clusters,d)
     
  }  
  h1 = list(hotel_clusters)
  outcome = append(outcome, h1)
}

new = list()
for (i in 1:nrow(test)){ 
  d = test$hotel_cluster[i]
  cl = rep(d, 5)
  new = append(new, list(cl))
}

result_dat = data.frame(cbind(outcome, new))
final_model5 <- data.frame(result_dat)
model5.Score = mapk(5, final_model5$new, final_model5$outcome)
# Free memory
rm(jaccardIndex, distances, result_dat)

############################################################
# Final Step: Ensemble 5 models and get the final result
#***********************************************************
############################################################
colnames(final_model1) = c("pred_model1", "actual1")
colnames(final_model2) = c("pred_model2", "actual1")
colnames(final_model3) = c("pred_model3", "actual1")
colnames(final_model4) = c("pred_model4", "actual1")
colnames(final_model5) = c("pred_model5", "actual1")

final_result = cbind(final_model1, final_model2, final_model3, final_model4, final_model5)

a <- sapply(final_result$pred_model1, function(x) unlist(x))
b <- sapply(final_result$pred_model2, function(x) unlist(x))
c <- sapply(final_result$pred_model3, function(x) unlist(x))
d <- sapply(final_result$pred_model4, function(x) unlist(x))
e <- sapply(final_result$pred_model5, function(x) unlist(x))

    
common = list()
for(i in 1:rows){
  t <- c(a[,i], b[,i], c[,i], d[,i], e[,i])
  tbl <- sort(table(t), decreasing = TRUE)
  neighbors  <- as.numeric(names(tbl)[1:5])
  common <- append(common, list(neighbors))
}

final_result$combined = common
score.model3 <- mapk(5, final_result$actual, final_result$combined)

#Overall Accuracy by this approch is 2.18% which is very low
