pdf("SpectralClusteringPlots.pdf")

# dataSource <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data'
# dataFilePath <- './data/cancer.data'
# dataDirectoryPath <- './data'
# 
# if (!dir.exists(dataDirectoryPath)) {
#   dir.create(dataDirectoryPath)
# }
# 
# if (!file.exists(dataFilePath)) {
#   download.file(dataSource, destfile=dataFilePath)
# }
# 
# cancer.data <- read.csv(dataFilePath, fileEncoding = "UTF-8", header = F)

cancer.data <- read.csv("data.csv",header=TRUE)
cancer.data$diagnosis <- factor(cancer.data$diagnosis, labels = c(2,1))

library(kernlab)

clusteringdata <- as.matrix(cancer.data[,3:32])

##2 clusters

specc1 <- specc(clusteringdata,2)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc1, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("black","red")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("black", "red"),legend=c("Benign","Malignant"))

#hypothesis test with confusion matrix

library(caret)

confusionMatrix(factor(specc1@.Data),cancer.data$diagnosis)

# library(EMT)
# multinomial.test(size(specc1),p=c(.5,.5), n= 569)


##3 clusters
specc2 <- specc(clusteringdata,3)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc2, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("black","red")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("black", "red"),legend=c("Benign","Malignant"))

#hypothesis test with confusion matrix

table(specc2@.Data,cancer.data$diagnosis)


library(EMT)
multiTest <- multinomial.test(size(specc2),c(1/3,1/3,1/3))



##4 clusters
specc3 <- specc(clusteringdata,4)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc3, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("black","red")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("black", "red"),legend=c("Benign","Malignant"))

#hypothesis test with confusion matrix

table(specc3@.Data,cancer.data$diagnosis)

multinomial.test(size(specc3),c(.25,.25,.25,.25))


#kmeans k=2

kmeans2 <- kmeans(clusteringdata,2)
confusionMatrix(factor(kmeans2$cluster),cancer.data$diagnosis)

par(mfrow=c(1,2))
plot(clusteringdata, col=kmeans2$cluster, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("black","red")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("black", "red"),legend=c("Benign","Malignant"))


#kmeans k=3

kmeans3 <- kmeans(clusteringdata,3)
table(factor(kmeans3$cluster),cancer.data$diagnosis)
multinomial.test(kmeans3$size,c(1/3,1/3,1/3))

par(mfrow=c(1,2))
plot(clusteringdata, col=kmeans3$cluster, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("red","black")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("red", "black"),legend=c("Benign","Malignant"))

graphics.off()
