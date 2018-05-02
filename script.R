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

typeof(specc1)
plot(clusteringdata, col=specc1, main="breast cancer clusters")
