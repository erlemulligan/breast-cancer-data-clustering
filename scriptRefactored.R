install.packages('kernlab')
install.packages('caret')
install.packages('EMT')
install.packages('cluster')
install.packages('XNomial')
library(kernlab)
library(caret)
library(EMT)
library(cluster)
library(XNomial)

dataSource <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data'
dataFilePath <- './data/cancer.data'
dataDirectoryPath <- './data'
 
if (!dir.exists(dataDirectoryPath)) {
  dir.create(dataDirectoryPath)
}
 
if (!file.exists(dataFilePath)) {
  download.file(dataSource, destfile=dataFilePath)
}

### SET FULL/ORIGINAL DATA SET ###
# labels for cancer data variables (column names)
cancerDataLabels <- c(
  'id',
  'diagnosis',
  'radius_mean',
  'texture_mean',
  'perimeter_mean',
  'area_mean',
  'smoothness_mean',
  'compactness_mean',
  'concavity_mean',
  'concave_points_mean',
  'symmetry_mean',
  'fractal_dimension_mean',
  'radius_se',
  'texture_se',
  'perimeter_se',
  'area_se',
  'smoothness_se',
  'compactness_se',
  'concavity_se',
  'concave_points_se',
  'symmetry_se',
  'fractal_dimension_se',
  'radius_worst',
  'texture_worst',
  'perimeter_worst',
  'area_worst',
  'smoothness_worst',
  'compactness_worst',
  'concavity_worst',
  'concave_points_worst',
  'symmetry_worst',
  'fractal_dimension_worst'
) 

# full data set
cancerData <- read.csv(dataFilePath, fileEncoding = "UTF-8", header = FALSE)
names(cancerData) <- cancerDataLabels

cancerData$diagnosis <- factor(cancerData$diagnosis, labels = c(1, 2))

# set data set for numeric data only to use for clustering
cancerClusteringData <- as.matrix(cancerData[,3:32])
### END OF SET FULL/ORIGINAL DATA SET ###

###

### CREATE TEST DATA SAMPLE ###
# set seed for random sample
set.seed(123)

# sample size of random sample
testSampleSize = 25

# generate random indices from cancer data set 
testSampleDataIndex <- sample(1:nrow(cancerData), testSampleSize)

# use random indices to set random sample from cancer data set
testSampleData <- cancerData[testSampleDataIndex, ]

# set test data set for numeric data only to use for clustering
testClusteringData <- as.matrix(testSampleData[,3:32])
### END OF CREATE TEST DATA SAMPLE ###

###

# 50/50 expected values for 2 clusters
expectedValues2Cluster <- c(1/2, 1/2)

# 33.33/33.33/33.33 expected values for 3 clusters
expectedValues3Cluster <- c(1/3, 1/3, 1/3)

### SPECTRAL CLUSTERING ON TEST DATA ###
# spectral clustering with 2 clusters (2 centers)
spectralCluster2 <- specc(testClusteringData, 2)
# exact multinomial test
spectralCluster2.pValue <- xmulti(size(spectralCluster2), expectedValues2Cluster)
# confusion matrix
spectralCluster2.confusionMatrix <- confusionMatrix(factor(spectralCluster2@.Data), testSampleData$diagnosis)
# accuracy
spectralCluster2.accuracy <- spectralCluster2.confusionMatrix$overall['Accuracy']

# spectral clustering with 3 clusters (3 centers)
spectralCluster3 <- specc(testClusteringData, 3)
# exact multinomial test
spectralCluster3.pValue <- xmulti(size(spectralCluster3), expectedValues3Cluster)
### END OF SPECTRAL CLUSTERING ON TEST DATA ###

###

### KMEANS CLUSTERING ON TEST DATA ###
# kmeans clustering with 2 clusters
kmeansCluster2 <- kmeans(testClusteringData, 2)
# exact multinomial test
kmeansCluster2.pValue <- xmulti(kmeansCluster2$size, expectedValues2Cluster) 
# confusion matrix
kmeansCluster2.confusionMatrix <- confusionMatrix(factor(kmeansCluster2$cluster), testSampleData$diagnosis)
# accuracy
kmeansCluster2.accuracy <- kmeansCluster2.confusionMatrix$overall['Accuracy']

# kmeans clustering with 3 clusters
kmeansCluster3 <- kmeans(testClusteringData, 3)
# exact multinomial test
kmeansCluster3.pValue <- xmulti(kmeansCluster3$size, expectedValues3Cluster)
### END OF KMEANS CLUSTERING ON TEST DATA ###

###

### KMEDOIDS CLUSTERING ON TEST DATA ###
# kmedoids clustering with 2 clusters
kmedoidsCluster2 <- pam(testClusteringData, 2)
# exact multinomial test
kmedoidsCluster2.pValue <- xmulti(kmedoidsCluster2$clusinfo[,1], expectedValues2Cluster)
# confusion matrix
kmedoidsCluster2.confusionMatrix <- confusionMatrix(factor(kmedoidsCluster2$cluster), testSampleData$diagnosis)
# accuracy
kmedoidsCluster2.accuracy <- kmedoidsCluster2.confusionMatrix$overall['Accuracy']

# kmedoids clustering with 3 clusters
kmedoidsCluster3 <- pam(testClusteringData, 3)
# exact multinomial test
kmedoidsCluster3.pValue <- xmulti(kmedoidsCluster3$clusinfo[,1], expectedValues3Cluster)
### END OF KMEDOIDS CLUSTERING ON TEST DATA ###

# compare p values and accuracy of clustering methods
clustering.pValues <- c(spectralCluster2.pValue$pProb, kmeansCluster2.pValue$pProb, kmedoidsCluster2.pValue$pProb)
clustering.accuracy <- c(spectralCluster2.accuracy, kmeansCluster2.accuracy, kmedoidsCluster2.accuracy)
clustering.rowNames <- c('spectral', 'kmeans', 'kmedoids')
clustering.colNames <- c('p-value', 'accuracy')

clusteringCompareTable <- as.data.frame(as.table(setNames(clustering.pValues, clustering.accuracy)))
colnames(clusteringCompareTable) <- clustering.colNames
row.names(clusteringCompareTable) <- clustering.rowNames
print(clusteringCompareTable)
