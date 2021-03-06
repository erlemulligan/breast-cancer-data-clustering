# install.packages('kernlab')
# install.packages('caret')
# install.packages('EMT')
# install.packages('cluster')
# install.packages('XNomial')
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

### CREATE TEST DATA SAMPLE (by Bootstrapping samples)###

n=15

spectralCluster2.pValue <- vector(mode="double",length=n)
spectralCluster2.accuracy <- vector(mode="double",length=n)
spectralCluster3.pValue <- vector(mode="double",length=n)
kmeansCluster2.pValue <- vector(mode="double",length=n)
kmeansCluster2.accuracy <- vector(mode="double",length=n)
kmeansCluster3.pValue <- vector(mode="double",length=n)
kmedoidsCluster2.pValue <- vector(mode="double",length=n)
kmedoidsCluster2.accuracy <- vector(mode="double",length=n)
kmedoidsCluster3.pValue <- vector(mode="double",length=n)

for(x in 1:n) {
    
    # set seed for random sample
    set.seed(x+124)
    
    # sample size of random sample
    testSampleSize = 569/n
    
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
    spectralCluster2.test <- xmulti(size(spectralCluster2), expectedValues2Cluster)
    # confusion matrix
    spectralCluster2.confusionMatrix <- confusionMatrix(factor(spectralCluster2@.Data), testSampleData$diagnosis)
    # p-value
    spectralCluster2.pValue[x] <- spectralCluster2.confusionMatrix$overall['AccuracyPValue']
    # accuracy
    spectralCluster2.accuracy[x] <- spectralCluster2.confusionMatrix$overall['Accuracy']
    
    # spectral clustering with 3 clusters (3 centers)
    spectralCluster3 <- specc(testClusteringData, 3)
    # exact multinomial test
    spectralCluster3.test <- xmulti(size(spectralCluster3), expectedValues3Cluster)
    # p-value for spec clustering with 3 clusters
    spectralCluster3.pValue[x] <- spectralCluster3.test$pProb
    ### END OF SPECTRAL CLUSTERING ON TEST DATA ###
    
    ###
    
    ### KMEANS CLUSTERING ON TEST DATA ###
    # kmeans clustering with 2 clusters
    kmeansCluster2 <- kmeans(testClusteringData, 2)
    # exact multinomial test
    kmeansCluster2.test <- xmulti(kmeansCluster2$size, expectedValues2Cluster) 
    # confusion matrix
    kmeansCluster2.confusionMatrix <- confusionMatrix(factor(kmeansCluster2$cluster), testSampleData$diagnosis)
    # p-value
    kmeansCluster2.pValue[x] <- kmeansCluster2.confusionMatrix$overall['AccuracyPValue']
    # accuracy
    kmeansCluster2.accuracy[x] <- kmeansCluster2.confusionMatrix$overall['Accuracy']
    
    # kmeans clustering with 3 clusters
    kmeansCluster3 <- kmeans(testClusteringData, 3)
    # exact multinomial test
    kmeansCluster3.test <- xmulti(kmeansCluster3$size, expectedValues3Cluster)
    # p-value for spec clustering with 3 clusters
    kmeansCluster3.pValue[x] <- kmeansCluster3.test$pProb
    ### END OF KMEANS CLUSTERING ON TEST DATA ###
    
    ###
    
    ### KMEDOIDS CLUSTERING ON TEST DATA ###
    # kmedoids clustering with 2 clusters
    kmedoidsCluster2 <- pam(testClusteringData, 2)
    # exact multinomial test
    kmedoidsCluster2.test <- xmulti(kmedoidsCluster2$clusinfo[,1], expectedValues2Cluster)
    # confusion matrix
    kmedoidsCluster2.confusionMatrix <- confusionMatrix(factor(kmedoidsCluster2$cluster), testSampleData$diagnosis)
    # p-value
    kmedoidsCluster2.pValue[x] <- kmedoidsCluster2.confusionMatrix$overall['AccuracyPValue']
    # accuracy
    kmedoidsCluster2.accuracy[x] <- kmedoidsCluster2.confusionMatrix$overall['Accuracy']
    
    # kmedoids clustering with 3 clusters
    kmedoidsCluster3 <- pam(testClusteringData, 3)
    # exact multinomial test
    kmedoidsCluster3.test <- xmulti(kmedoidsCluster3$clusinfo[,1], expectedValues3Cluster)
    # p-value for spec clustering with 3 clusters
    kmedoidsCluster3.pValue[x] <- kmedoidsCluster3.test$pProb
    ### END OF KMEDOIDS CLUSTERING ON TEST DATA ###
    
  }

# compare p values and accuracy of clustering methods
clustering.pValues <- c(mean(spectralCluster2.pValue), mean(kmeansCluster2.pValue), mean(kmedoidsCluster2.pValue))
clustering.accuracy <- c(mean(spectralCluster2.accuracy), mean(kmeansCluster2.accuracy), mean(kmedoidsCluster2.accuracy))
clustering.rowNames <- c('spectral', 'kmeans', 'kmedoids')
clustering.colNames <- c('accuracy', 'p-value')

clusteringCompareTable <- as.data.frame(as.table(setNames(clustering.pValues, clustering.accuracy)))
colnames(clusteringCompareTable) <- clustering.colNames
row.names(clusteringCompareTable) <- clustering.rowNames
print(clusteringCompareTable)
