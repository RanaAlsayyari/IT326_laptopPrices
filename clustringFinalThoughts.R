
library(readr)
clusdataset <- read.csv("C:/Users/kamar/OneDrive/Desktop/new_datasetforDM.csv")
View(clusdataset)


###=========================================================================================
##Clustering
## this clustering algorithm uses the Brute-Force method in determining the correct number of clusters. We try applying a clustering algorithm with different numbers of clusters. Then, we find the number that optimizes the quality of the clustering results

### first since the dataset is mixedtype rather than one-hot-encoding, another approach into keeping all attributes in count -while using kmeans which only accept numeric- is to replace values with there faction from 0 to 1, for nominal data to not have a weight[caused by one-hot-encoding] larger than numeric data that will be normalized.
### keeping in mind that the fractions should be unique as they represent a category.

####1: normalizing numeric attributes
normalize = function(x) {return ((x-min(x)) / (max(x)))} 
clusdataset$Inches = normalize(clusdataset$Inches)
clusdataset$ScreenResolution = normalize(clusdataset$ScreenResolution)
clusdataset$Ram = normalize(clusdataset$Ram)
clusdataset$Weight = normalize(clusdataset$Weight)
clusdataset$MemorySize = normalize(clusdataset$MemorySize)

####2: get the fraction for categorical attributes and assign them
clusdataset %>% 
  group_by( Company ) %>% 
  summarise( percent = n() / nrow( clusdataset ) )
clusdataset$Company <- factor(clusdataset$Company, levels = c("Acer", "Apple", "Asus", "Chuwi", "Dell", "Fujitsu", "Google", "HP", "Huawei","LG", "Lenovo", "MSI", "Mediacom", "Microsoft", "Razer", "Samsung","Toshiba", "Vero", "Xiaomi"), labels = c(0.0790, 0.0161,0.121, 0.00230, 0.228, 0.002303, 0.00230303, 0.210,0.00153,0.0023030303, 0.23, 0.0414,0.00537,0.00460, 0.0053737, 0.00691, 0.0368, 0.00307, 0.0030707))
str(clusdataset)
clusdataset$Company <-as.numeric(as.character(clusdataset$Company))

clusdataset %>% 
  group_by( TypeName ) %>% 
  summarise( percent = n() / nrow( clusdataset ) )
clusdataset$TypeName <- factor(clusdataset$TypeName, levels = c("2 in 1 Convertible", "Gaming", "Netbook", "Notebook", "Ultrabook", "Workstation"), labels = c("0.0929", "0.157 ","0.0192", "0.558 ", "0.150", "0.0223"))
clusdataset$TypeName <-as.numeric(as.character(clusdataset$TypeName))


clusdataset %>% 
  group_by( OpSys ) %>% 
  summarise( percent = n() / nrow( clusdataset ) )
clusdataset$OpSys <- factor(clusdataset$OpSys, levels = c("Android", "Chrome OS", "Linux", "Mac OS X", "No OS", "Windows 10", "Windows 10 S", "Windows 7", "macOS"), labels = c("0.00153", "0.0207 ","0.0476", "0.00614 ", "0.0507", "0.823", "0.0061", "0.0345","0.00998"))
clusdataset$OpSys <-as.numeric(as.character(clusdataset$OpSys))



clusdataset %>% 
  group_by( CPU ) %>% 
  summarise( percent = n() / nrow( clusdataset ) )
clusdataset$CPU <- factor(clusdataset$CPU, levels = c("AMD A-Series", "AMD E-Series", "Intel Atom x", "Intel Celeron", "Intel Core M", "Intel Core i3", "Intel Core i5", "Intel Core i7", "Intel Pentium", "Other"), labels = c("0.0361", "0.00691", "0.00921","0.0675", "0.0146 ", "0.104", "0.325", "0.404", "0.0230","0.00921"))
clusdataset$CPU <-as.numeric(as.character(clusdataset$CPU))



clusdataset %>% 
  group_by( GPU ) %>% 
  summarise( percent = n() / nrow( clusdataset ) )
clusdataset$GPU <- factor(clusdataset$GPU, levels = c("AMD Radeon Graphics", "Intel HD Graphics", "Intel Iris Graphics", "Intel UHD Graphics", "Nvidia GeForce Graphics", "Nvidia Quadro", "Other"), labels = c("0.133", "0.490", "0.0107","0.0522", "0.282 ", "0.0238", "0.00767"))
clusdataset$GPU <-as.numeric(as.character(clusdataset$GPU))

clusdataset %>% 
  group_by( MemoryType ) %>% 
  summarise( percent = n() / nrow( clusdataset ) )
clusdataset$MemoryType <- factor(clusdataset$MemoryType, levels = c(" Flash Storage", " Flash Storage HDD", " HDD", " Hybrid", " SSD", " SSD  SSD"," SSD HDD", " SSD&HDD", " SSD&Hybrid"), labels = c("0.0568", "0.000767", "0.288","0.0077", "0.49153","0.49153", "0.15367","0.15367", "0.00153"))
clusdataset$MemoryType <-as.numeric(as.character(clusdataset$MemoryType))


###remove class label
clusdataset$price <-NULL
###remove ID as it will give over fitting, same for product as there is 618 unique product which is about half the dataset, this mean worst case each product variable existed once except for one, and best case that each attribute existed twice maximum which could lead to an overfitting. 
clusdataset$ID <-NULL
clusdataset$Product <-NULL

###_________________________________________________________________________________

##running the Kmean method with numbers ranging from 3 to 6
set.seed(8953)
clusdataset <- scale(clusdataset)
str(clusdataset)
library(NbClust)
library(cluster)
library(factoextra)


kmeanResults3<-kmeans(clusdataset, 3)
kmeanResults3

fviz_cluster(kmeanResults3, data = clusdataset)


kmeanResults4<-kmeans(clusdataset, 4)
kmeanResults4

fviz_cluster(kmeanResults4, data = clusdataset)



kmeanResults5<-kmeans(clusdataset, 5)
kmeanResults5

fviz_cluster(kmeanResults5, data = clusdataset)


kmeanResults6<-kmeans(clusdataset, 6)
kmeanResults6

fviz_cluster(kmeanResults6, data = clusdataset)

###___________________________________________________________________________________

## looking for the optimal k number:
### 1: direct method: elbow method.
fviz_nbclust(clusdataset, kmeans, method="wss")
### the elbow graph shows the within-cluster-sum-of-squares (wcss), looking at the point that most likely to be the bend (elbow point) "3", we interpret that increasing of k does not lead to a significant reduction in wcss.
### Yet this type of bend could be considered as ambiguous as the plot does not contain a sharp elbow.

###2: direct method: silhouette score method
fviz_nbclust(clusdataset, kmeans, method="silhouette")
###The major difference between elbow and silhouette scores is that elbow only calculates the euclidean distance whereas silhouette takes into account variables such as variance, skewness, high-low differences, etc.
###A silhouette score of one means each data point is unlikely to be assigned to another cluster. the best number of clusters in increasing order are point to one are:3,9,2,10.
###A score close to zero means each data point could be easily assigned to another cluster. by that we know 5,8 are the worst number of cluster.
###now that we have interpreted the silhouette lets take a look at the best number of clusters that fell of our brute force method

kmeanResults9<-kmeans(clusdataset, 9)

fviz_cluster(kmeanResults9, data = clusdataset)
### determining the quality of the cluster by eye, there is high intra cluster similaritiy especially in cluster 4, but an extremely low inter cluster dissimliraty escpecially in cluster 6.

kmeanResults2<-kmeans(clusdataset, 2)
kmeanResults2


fviz_cluster(kmeanResults2, data = clusdataset)
### even with the interference and intrusion of the two clusters, compared to 9, a person can better distinguish between the two clusters. But dividing this complex dataset into two cluster, can one say it's a good approach?


kmeanResults10<-kmeans(clusdataset, 10)
kmeanResults10



fviz_cluster(kmeanResults10, data = clusdataset)
###looking at the clusters in a 2d plot, there is a lot of intrusion between the cluster, and an extremely low inter cluster dissimilarity.
### now that we are left with two optimal points 3, 10, we'll use the gap stat method to have a better understanding.

### 3: statistical method: gap method
fviz_nbclust(clusdataset, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
### the gap_stat method gave a 9 as the optimal.
###now that we are left with 3 distinct optimal points, we'll calculate the precision and recall of three to decide which is better.

###_______________________________________________________________________________
#precision and recall for k=3
cluster_assignments <- c(kmeanResults3$cluster)
ground_truth_labels <- c(dataset$price)



data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall 
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0
  
  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
    # Count the number of items from the same category within the same cluster
    same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
    # Count the total number of items in the same cluster
    total_same_cluster <- sum(data$cluster == cluster)
    
    # Count the total number of items with the same category
    total_same_category <- sum(data$label == label)
    
    # Calculate precision and recall for the current item and add them to the sums
    precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
    recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }
  
  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n
  
  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")



#precision and recall for k=9
cluster_assignments <- c(kmeanResults9$cluster)
ground_truth_labels <- c(dataset$price)




data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall 
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0
  
  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
    # Count the number of items from the same category within the same cluster
    same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
    # Count the total number of items in the same cluster
    total_same_cluster <- sum(data$cluster == cluster)
    
    # Count the total number of items with the same category
    total_same_category <- sum(data$label == label)
    
    # Calculate precision and recall for the current item and add them to the sums
    precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
    recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }
  
  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n
  
  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")



#precision and recall for k=10
cluster_assignments <- c(kmeanResults10$cluster)
ground_truth_labels <- c(dataset$price)




data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall 
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0
  
  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
    # Count the number of items from the same category within the same cluster
    same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
    # Count the total number of items in the same cluster
    total_same_cluster <- sum(data$cluster == cluster)
    
    # Count the total number of items with the same category
    total_same_category <- sum(data$label == label)
    
    # Calculate precision and recall for the current item and add them to the sums
    precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
    recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }
  
  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n
  
  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")


#B-cubed precision is a measure used to evaluate the quality of a clustering algorithm's performance, It provides insights into how well a clustering algorithm assigns data points to the correct clusters or categories .For k=3 the precision is (0.3391458 ) ,for k=9the precision is(0.4292558 ) for k=10 the precision is (0.4079697 ) i noticed that the precision is relativly low for all the k values  .it is because the clusters generated by the algorithm are not well-separated and there is significant overlap, Overlapping clusters make it challenging to assign data points accurately to their true categories, leading to imprecise clustering results.another potential reason is the misclassification of data points into the wrong clusters. When data points are incorrectly assigned to clusters, it reduces the precision because not all data points within a cluster belong to the same true class or category.the clusters generated with k=10 have the highest precision .and the clusters generated with k=3 have the lowest precision .


#B-cubed recall is a measure used to evaluate the quality of a clustering algorithm's performance,It provides insights into how well a clustering algorithm captures all data points belonging to the same true class or category.the recall for k=3 is (0.5428454), for k=9 the recall is (0.2520904 ), for k=10 the recall is (0.2051488).i can see that the recall is low for all the k values.When data points are incorrectly assigned to clusters that do not contain all the data points from the same true class, recall decreases because not all data points are captured. the clusters generated with k=3 have the highest recall.and the clusters generated with k=10 have the lowest recall .

#Average silhouette coeffecient for k=10
avg_sil <- silhouette(kmeanResults10$cluster,dist(clusdataset))
fviz_silhouette(avg_sil)

#Average silhouette coeffecient for k=10
avg_sil <- silhouette(kmeanResults3$cluster,dist(clusdataset))
fviz_silhouette(avg_sil)

#Average silhouette coeffecient for k=10
avg_sil <- silhouette(kmeanResults9$cluster,dist(clusdataset))
fviz_silhouette(avg_sil)

#In a silhouette plot, each data point is represented by a vertical bar. The height of the bar corresponds to the silhouette score of the data point.By comparing the plots , k=9 and k=10 have higher silhouette scores than k=3 but all of them are closer to 0 than 1 A score close to 0 means that the data point is on or very close to the decision boundary between two neighboring clusters.k=9 and k=10 have negative scores.A score close to -1 indicates that the data point is assigned to the wrong cluster and would be better off in another cluster.

km<-kmeans(clusdataset, 9,iter.max =140,algorithm="Lloyd",nstart=100)
twss<-sum(km$withinss)
print(twss)


km<-kmeans(clusdataset, 3,iter.max =140,algorithm="Lloyd",nstart=100)
twss<-sum(km$withinss)
print(twss)


km<-kmeans(clusdataset, 10,iter.max =140,algorithm="Lloyd",nstart=100)
twss<-sum(km$withinss)
print(twss)

#TWCSS represents the sum of the squared distances between each data point in a cluster and the centroid of that cluster. In other words, it measures the variation or dispersion of data points within each cluster.the value of twss for k=3 is  9970.073 ,for k=10 its 5970.864 and for k=9 it is 6271.242(i noticed that the values are high).A large Total Within-Cluster Sum of Squares (TWCSS) suggests that the dataset's clusters are spread out or not particularly compact. A high TWCSS in the context of clustering indicates that the data points within each cluster are spread out over a broader area and that the clusters are not densely packed around their centroids.


###==============================================================================================================================

##Enhancing the quality of clusters:

### now that we have noticed a low precision measure. we will try enhancing the cluster by approaching the data differently.
## using One-Hot-Encoding rather than fragment approach:


clusdataset2 <- read.csv("C:/Users/kamar/OneDrive/Desktop/new_datasetforDM.csv")

normalize = function(x) {return ((x-min(x)) / (max(x)))} 
clusdataset2$Inches = normalize(clusdataset2$Inches)
clusdataset2$ScreenResolution = normalize(clusdataset2$ScreenResolution)
clusdataset2$Ram = normalize(clusdataset2$Ram)
clusdataset2$Weight = normalize(clusdataset2$Weight)
clusdataset2$MemorySize = normalize(clusdataset2$MemorySize)


####2: get the fraction for categorical attributes and assign
clusdataset2 %>% 
  group_by( Company ) %>% 
  summarise( percent = n() / nrow( clusdataset2 ) )
clusdataset2$Company <- factor(clusdataset2$Company, levels = c("Acer", "Apple", "Asus", "Chuwi", "Dell", "Fujitsu", "Google", "HP", "Huawei","LG", "Lenovo", "MSI", "Mediacom", "Microsoft", "Razer", "Samsung","Toshiba", "Vero", "Xiaomi"), labels = c(1, 2,3, 4, 5, 6, 7, 8,9,10, 11, 12,13,14, 15, 16, 17, 18, 19))
str(clusdataset)
clusdataset2$Company <-as.numeric(as.character(clusdataset2$Company))

clusdataset2 %>% 
  group_by( TypeName ) %>% 
  summarise( percent = n() / nrow( clusdataset2 ) )
clusdataset2$TypeName <- factor(clusdataset2$TypeName, levels = c("2 in 1 Convertible", "Gaming", "Netbook", "Notebook", "Ultrabook", "Workstation"), labels = c(1, 2,3, 4, 5, 6))
clusdataset2$TypeName <-as.numeric(as.character(clusdataset2$TypeName))


clusdataset2 %>% 
  group_by( OpSys ) %>% 
  summarise( percent = n() / nrow( clusdataset2 ) )
clusdataset2$OpSys <- factor(clusdataset2$OpSys, levels = c("Android", "Chrome OS", "Linux", "Mac OS X", "No OS", "Windows 10", "Windows 10 S", "Windows 7", "macOS"), labels = c(1, 2,3, 4, 5, 6, 7, 8,9))
clusdataset2$OpSys <-as.numeric(as.character(clusdataset2$OpSys))



clusdataset2 %>% 
  group_by( CPU ) %>% 
  summarise( percent = n() / nrow( clusdataset2 ) )
clusdataset2$CPU <- factor(clusdataset2$CPU, levels = c("AMD A-Series", "AMD E-Series", "Intel Atom x", "Intel Celeron", "Intel Core M", "Intel Core i3", "Intel Core i5", "Intel Core i7", "Intel Pentium", "Other"), labels = c(1, 2,3, 4, 5, 6, 7, 8,9,10))
clusdataset2$CPU <-as.numeric(as.character(clusdataset2$CPU))



clusdataset2 %>% 
  group_by( GPU ) %>% 
  summarise( percent = n() / nrow( clusdataset2 ) )
clusdataset2$GPU <- factor(clusdataset2$GPU, levels = c("AMD Radeon Graphics", "Intel HD Graphics", "Intel Iris Graphics", "Intel UHD Graphics", "Nvidia GeForce Graphics", "Nvidia Quadro", "Other"), labels = c(1, 2,3, 4, 5, 6, 7))
clusdataset2$GPU <-as.numeric(as.character(clusdataset2$GPU))

clusdataset2 %>% 
  group_by( MemoryType ) %>% 
  summarise( percent = n() / nrow( clusdataset2 ) )
clusdataset2$MemoryType <- factor(clusdataset2$MemoryType, levels = c(" Flash Storage", " Flash Storage HDD", " HDD", " Hybrid", " SSD", " SSD  SSD"," SSD HDD", " SSD&HDD", " SSD&Hybrid"), labels = c(1, 2,3, 4, 5, 6, 7, 8,9))
clusdataset2$MemoryType <-as.numeric(as.character(clusdataset2$MemoryType))


###remove class label
clusdataset2$price <-NULL
###remove ID as it will give over fitting, same for product as there is 618 unique product which is about half the dataset, this mean worst case each product variable existed once except for one, and best case that each attribute existed twice maximum which could lead to an overfitting. 
clusdataset2$ID <-NULL
clusdataset2$Product <-NULL

set.seed(8953)
clusdataset2 <- scale(clusdataset2)
str(clusdataset)


skmeanResults2<-kmeans(clusdataset2, 2)
skmeanResults2
fviz_cluster(skmeanResults2, data = clusdataset2)


skmeanResults3<-kmeans(clusdataset2, 3)
skmeanResults3
fviz_cluster(skmeanResults3, data = clusdataset2)


skmeanResults4<-kmeans(clusdataset2, 4)
skmeanResults4
fviz_cluster(skmeanResults4, data = clusdataset2)



skmeanResults5<-kmeans(clusdataset, 5)
skmeanResults5
fviz_cluster(skmeanResults5, data = clusdataset2)


skmeanResults6<-kmeans(clusdataset, 6)
skmeanResults6
fviz_cluster(skmeanResults6, data = clusdataset2)


fviz_nbclust(clusdataset2, kmeans, method="wss")

fviz_nbclust(clusdataset2, kmeans, method="silhouette")


#precision and recall for k=2
cluster_assignments <- c(skmeanResults2$cluster)
ground_truth_labels <- c(dataset$price)

data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall 
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0
  
  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
    # Count the number of items from the same category within the same cluster
    same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
    # Count the total number of items in the same cluster
    total_same_cluster <- sum(data$cluster == cluster)
    
    # Count the total number of items with the same category
    total_same_category <- sum(data$label == label)
    
    # Calculate precision and recall for the current item and add them to the sums
    precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
    recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }
  
  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n
  
  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")



### we see that both elbow and silhouette method agreed on 2 being the optimal point, which gave us a recall of :0.6699069 yet a precision of: 0.2611999 

###====================


###having the precision still low, we will try approaching the data by only selecting core numeric data.

clusdataset3 <- read.csv("C:/Users/kamar/OneDrive/Desktop/new_datasetforDM.csv")

normalize = function(x) {return ((x-min(x)) / (max(x)))} 
clusdataset3$Inches = normalize(clusdataset3$Inches)
clusdataset3$ScreenResolution = normalize(clusdataset3$ScreenResolution)
clusdataset3$Ram = normalize(clusdataset3$Ram)
clusdataset3$Weight = normalize(clusdataset3$Weight)
clusdataset3$MemorySize = normalize(clusdataset3$MemorySize)

###remove class label
clusdataset3$price <-NULL
###remove ID as it will give over fitting, same for product as there is 618 unique product which is about half the dataset, this mean worst case each product variable existed once except for one, and best case that each attribute existed twice maximum which could lead to an overfitting. 
clusdataset3$ID <-NULL
clusdataset3$Product <-NULL
clusdataset3$MemoryType <- NULL
clusdataset3$GPU <- NULL
clusdataset3$CPU <- NULL
clusdataset3$OpSys <- NULL
clusdataset3$TypeName <- NULL
clusdataset3$Company <- NULL

set.seed(8953)
clusdataset3 <- scale(clusdataset3)
str(clusdataset)


tkmeanResults2<-kmeans(clusdataset3, 2)
tkmeanResults2
fviz_cluster(tkmeanResults2, data = clusdataset3)


tkmeanResults3<-kmeans(clusdataset3, 3)
tkmeanResults3
fviz_cluster(tkmeanResults3, data = clusdataset3)


tkmeanResults4<-kmeans(clusdataset3, 4)
tkmeanResults4
fviz_cluster(tkmeanResults4, data = clusdataset3)



tkmeanResults5<-kmeans(clusdataset, 5)
tkmeanResults5
fviz_cluster(tkmeanResults5, data = clusdataset3)


tkmeanResults6<-kmeans(clusdataset, 6)
tkmeanResults6
fviz_cluster(tkmeanResults6, data = clusdataset3)


tkmeanResults8<-kmeans(clusdataset, 8)
tkmeanResults8
fviz_cluster(tkmeanResults6, data = clusdataset3)


fviz_nbclust(clusdataset3, kmeans, method="wss")

fviz_nbclust(clusdataset3, kmeans, method="silhouette")


#precision and recall for k=2
cluster_assignments <- c(tkmeanResults2$cluster)
ground_truth_labels <- c(dataset$price)

data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall 
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0
  
  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
    # Count the number of items from the same category within the same cluster
    same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
    # Count the total number of items in the same cluster
    total_same_cluster <- sum(data$cluster == cluster)
    
    # Count the total number of items with the same category
    total_same_category <- sum(data$label == label)
    
    # Calculate precision and recall for the current item and add them to the sums
    precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
    recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }
  
  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n
  
  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")
###_____________________________

#precision and recall for k=8
cluster_assignments <- c(tkmeanResults8$cluster)
ground_truth_labels <- c(dataset$price)

data <- data.frame(cluster = cluster_assignments, label = ground_truth_labels)

# Function to calculate BCubed precision and recall 
calculate_bcubed_metrics <- function(data) {
  n <- nrow(data)
  precision_sum <- 0
  recall_sum <- 0
  
  for (i in 1:n) {
    cluster <- data$cluster[i]
    label <- data$label[i]
    
    # Count the number of items from the same category within the same cluster
    same_category_same_cluster <- sum(data$label[data$cluster == cluster] == label)
    
    # Count the total number of items in the same cluster
    total_same_cluster <- sum(data$cluster == cluster)
    
    # Count the total number of items with the same category
    total_same_category <- sum(data$label == label)
    
    # Calculate precision and recall for the current item and add them to the sums
    precision_sum <- precision_sum + same_category_same_cluster /total_same_cluster
    recall_sum <- recall_sum + same_category_same_cluster / total_same_category
  }
  
  # Calculate average precision and recall
  precision <- precision_sum / n
  recall <- recall_sum / n
  
  return(list(precision = precision, recall = recall))
}

# Calculate BCubed precision and recall
metrics <- calculate_bcubed_metrics(data)

# Extract precision and recall from the metrics
precision <- metrics$precision
recall <- metrics$recall

# Print the results
cat("BCubed Precision:", precision, "\n")
cat("BCubed Recall:", recall, "\n")
### there is a difference in the optimal point between the elbow and the silhouette method, yet even with a clearer clustering plot, the Precision: 0.2489709 and Recall: 0.569642 for k=2 (elbow optimal), where Precision: 0.3636168 and Recall: 0.2447948 for k=8 (silhouette optimal).

###===================================================================

# conclusion:
### the best precision and recall measure we could get was: for fragment encoding k=10 the recall is (0.2051488) precision is(0.4292558 ) [highest precision], and one-hot-encoding k=2 recall is(0.6699069) precision is(0.2611999 ) [highest recall] . we first well analyse sources of issue of why the BCubed measures were low.
### after we approached the dataset with 2 different encoding methods, we then approached the data by only focusing on numeric data to answer the question: does the problem lie on the encoding? and the answer came out to be that encoding the data and pitting the categorical attributes in count prduced the best outcomes, so what other issues could we discuss?
## a deep study of low outcomes of the BCubed measures:
### First lets look at the Gap-stat method and why the optimal point was not a real optimal, this indicates a fail of the method, so under which conditions are the Gap-Statistics likely to fail? 
## There are three scenarios that could occur:
### Underestimation of clusters, Overestimation of clusters, and In general,
### underestimation: If two or three clusters are very close together and the other clusters are far apart, it tends to underestimate. 
### Overestimation:If all clusters are close to together, it rather overestimates than underestimates.
### In general: Both underestimation and overestimation depend mostly on the randomly initialized centroids. When some of them get omitted due to random unluck, the break in the within-cluster distance forces the gap statistic to produce the optimum cluster earlier.
### and by observing the clustering plot most likely overestimation happened.

## elbow method fail conditions:
### the elbow method might not be the right for the problem. (which most likely happened when we faced an ambiguous bend)
### K-means is highly sensitive to preprocessing. If one attribute is on a much larger scale than the others, it will dominate the output.

###this is a conclusion of the results, and to enhance the result we provide a solution of future work (enhancing the dataset):

# future work:
###in case of imbalanced data, we propose that a resampling approach to the dataset - Clustering Based Oversampling for improved learning-. The essential idea behind the proposed method is to use the distance between a minority class sample and its respective cluster centroid to infer the number of new sample points to be generated for that minority class sample. The proposed algorithm has very less dependence on the technique used for finding cluster centroids and does not effect the majority class learning in any way. It also improves learning from imbalanced data by incorporating the distribution structure of minority class samples in generating of new data samples. The newly generated minority class data is handled in a way as to prevent outlier production and overfitting. 

###==================================================================
