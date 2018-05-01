dataSource <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data'
dataFilePath <- './data/cancer.data'
dataDirectoryPath <- './data'

if (!dir.exists(dataDirectoryPath)) {
  dir.create(dataDirectoryPath)
}

if (!file.exists(dataFilePath)) {
  download.file(dataSource, destfile=dataFilePath)
}

cancer.data <- read.csv(dataFilePath, fileEncoding = "UTF-8", header = F)