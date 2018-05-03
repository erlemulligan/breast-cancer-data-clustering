pdf("SpectralClusteringPlots.pdf")

dataSource <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data'
dataFilePath <- './data/cancer.data'
dataDirectoryPath <- './data'

if (!dir.exists(dataDirectoryPath)) {
  dir.create(dataDirectoryPath)
}

if (!file.exists(dataFilePath)) {
  download.file(dataSource, destfile=dataFilePath)
}

cancer.data <- read.csv(dataFilePath, fileEncoding = "UTF-8", header = F)


library(kernlab)

clusteringdata <- as.matrix(cancer.data[,3:32])

specc1 <- specc(clusteringdata,2)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc1, main="breast cancer clusters")

plot(cancer.data$V3,cancer.data$V4, col=c("red","black")[cancer.data$V2], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("red", "black"),legend=c("Benign","Malignant"))


specc2 <- specc(clusteringdata,3)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc2, main="breast cancer clusters")

plot(cancer.data$V3,cancer.data$V4, col=c("red","black")[cancer.data$V2], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("red", "black"),legend=c("Benign","Malignant"))


specc3 <- specc(clusteringdata,4)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc3, main="breast cancer clusters")

plot(cancer.data$V3,cancer.data$V4, col=c("red","black")[cancer.data$V2], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("red", "black"),legend=c("Benign","Malignant"))


graphics.off()
