######################################################################################
## Using Hiearchial CLustering to create some more features
######################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Configure followinng variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dataPath <- "/home/smita/RF_models/clean_sample_RF4_23K.csv"

# install vegan package before running below code
library(vegan)
exp_Data <- read.csv(dataPath)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create fetaure using similarity measure based on 
# (srch_destination_id", "srch_destination_type_id")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

selectVar  = c("srch_destination_id", "srch_destination_type_id")
train = subset(exp_Data, select = selectVar)

# create jaccard distance matrix 
jaccardDistance = vegdist(train, method = "jaccard")
cluster.tree = hclust(jaccardDistance)
cluster_id_srchDest = cutree(cluster.tree, k = 10)
exp_Data$cluster_id_srchDest = cluster_id_srchDest

rm(jaccardDistance, cluster.tree, cluster_id_srchDest, train)#free memory

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create fetaure using similarity measure based on 
# ("hotel_country", "hotel_market")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

selectVar  = c("hotel_country", "hotel_market")
train = subset(exp_Data, select = selectVar)

# create jaccard distance matrix 
jaccardDistance = vegdist(train, method = "jaccard")
cluster.tree = hclust(jaccardDistance)
cluster_id_hotel_cluster = cutree(cluster.tree, k = 5)# create the 10 clusters
exp_Data$cluster_id_hotel_cluster = cluster_id_hotel_cluster

rm(jaccardDistance, cluster.tree, cluster_id_hotel_cluster, train)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create fetaure using similarity measure based on 
# ("user_location_region", "user_location_city"", "user_location_country")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

selectVar  = c("user_location_country", "user_location_region", "user_location_city")
train = subset(exp_Data, select = selectVar)

# create jaccard distance matrix 
jaccardDistance = vegdist(train, method = "jaccard")
cluster.tree = hclust(jaccardDistance)
cluster_id_UCRC = cutree(cluster.tree, k = 5)# create the 5 clusters
exp_Data$cluster_id_UCRC = cluster_id_UCRC

rm(jaccardDistance, cluster.tree, cluster_id_UCRC, train)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create fetaure using similarity measure based on 
# ("user_location_country", "user_location_region", "srch_destination_id")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

selectVar  = c("user_location_country", "user_location_region", "srch_destination_id")
train = subset(exp_Data, select = selectVar)

# create jaccard distance matrix 
jaccardDistance = vegdist(train, method = "jaccard")
cluster.tree = hclust(jaccardDistance)
cluster_id_ULSrDest = cutree(cluster.tree, k = 15)# create the 30 gropus  based on this criteria
exp_Data$cluster_id_ULSrDest = cluster_id_ULSrDest

rm(jaccardDistance, cluster.tree, cluster_id_ULSrDest, train)