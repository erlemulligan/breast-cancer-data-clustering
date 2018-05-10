pdf("SpectralClusteringPlots.pdf")

library(kernlab)
library(caret)
library(EMT)

# setwd("~/MSDS 5163/Final Project/breast-cancer-data-clustering")

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


mod_ExactMultinomialTest <- function (observed, prob, size, groups, numEvents) 
{
  pObs = dmultinom(observed, size = size, prob)
  eventMat <- findVectors(groups, size)
  if (nrow(eventMat) != numEvents) 
    stop("Wrong number of events calculated. \n This is probably a bug.")
  eventProb <- apply(eventMat, 1, function(x) dmultinom(x, 
                                                        size = size, prob = prob))
  p.value = sum(eventProb[eventProb <= pObs])
  if (round(sum(eventProb), digits = 2) != 1) 
    stop("Wrong values for probabilities. \n This is probably a bug.")
  head <- paste("\n Exact Multinomial Test, distance measure: p\n\n")
  return(p.value)
  # tab <- as.data.frame(cbind(numEvents, round(pObs, digits = 8), 
  #                            round(p.value, digits = 8)))
  # colnames(tab) <- c("   Events", "   pObs", "   p.value")
  # cat(head)
  # print(tab, row.names = FALSE)
  # invisible(list(id = "Exact Multinomial Test", size = size, 
  #                groups = groups, stat = "lowP", allProb = sort(eventProb, 
  #                                                               decreasing = TRUE), ntrial = NULL, p.value = round(p.value, 
                                                                                                                  # digits = 8)))
}

mod_multinomial.test <- function (observed, prob, useChisq = FALSE, MonteCarlo = FALSE, 
          ntrial = 1e+05, atOnce = 1e+06) 
{
  if (!is.vector(observed, mode = "numeric")) 
    stop(" Observations have to be stored in a vector, e.g.  'observed <- c(5,2,1)'")
  if (!is.vector(prob, mode = "numeric")) 
    stop(" Probabilities have to be stored in a vector, e.g.  'prob <- c(0.25, 0.5, 0.25)'")
  if (round(sum(prob), digits = 1) != 1) 
    stop("Wrong input: sum of probabilities must not deviate from 1.")
  if (length(observed) != length(prob)) 
    stop(" Observations and probabilities must have same dimensions.")
  size <- sum(observed)
  groups <- length(observed)
  numEvents <- choose(size + groups - 1, groups - 1)
  if (MonteCarlo == FALSE) {
    if (useChisq == FALSE) {
      res <- mod_ExactMultinomialTest(observed, prob, size, 
                                  groups, numEvents)
    }
    else {
      res <- mod_ExactMultinomialTestChisquare(observed, prob, 
                                           size, groups, numEvents)
    }
  }
  else {
    if (ntrial < numEvents) {
      cat(" \n WARNING: Number of simulated withdrawels is lower than the number of possible outcomes. \n                This might yield unreliable results!\n\n")
    }
    flush.console()
    if (useChisq == FALSE) {
      res <- MonteCarloMultinomialTest(observed, prob, 
                                       size, groups, numEvents, ntrial, atOnce)
    }
    else {
      res <- MonteCarloMultinomialTestChisquare(observed, 
                                                prob, size, groups, numEvents, ntrial, atOnce)
    }
  }
  invisible(res)
}


cancer.data <- read.csv("data.csv",header=TRUE)
cancer.data$diagnosis <- factor(cancer.data$diagnosis, labels = c(1,2))

clusteringdata <- as.matrix(cancer.data[,3:32])


#perform clustering on n random samples of data and average results
n <- 15
specc1.P.Values <- vector(mode="double",length=n)
specc1.Accuracies <- vector(mode="double",length=n)
specc2.P.Values <- vector(mode="double",length=n)
specc3.P.Values <- vector(mode="double",length=n)

for(x in 1:n) {
  
  set.seed(x+50)
  indices <- sample(nrow(clusteringdata),nrow(clusteringdata)/n)
  clusteringdatasample1 <- clusteringdata[indices, ]
  cancer.data.sample1 <- cancer.data[indices, ]
  specc1 <- specc(clusteringdatasample1,2)
  specc2 <- specc(clusteringdatasample1,3)
  specc3 <- specc(clusteringdatasample1,4)
  #hypothesis test with confusion matrix
  multn.test1 <- mod_multinomial.test(size(specc1),c(.5,.5))
  multn.test2 <- mod_multinomial.test(size(specc2),c(1/3,1/3,1/3))
  multn.test3 <- mod_multinomial.test(size(specc3),c(.25,.25,.25,.25))
  specc1.P.Values[x] <- multn.test1
  specc2.P.Values[x] <- multn.test2
  specc3.P.Values[x] <- multn.test3
  
  confMatrix <- confusionMatrix(factor(specc1@.Data),cancer.data.sample1$diagnosis)
  specc1.Accuracies[x] <- confMatrix$overall['Accuracy']
  
}

specc1.p.value <- mean(specc1.P.Values)
specc2.p.value <- mean(specc2.P.Values)
specc3.p.value <- mean(specc3.P.Values)
specc1.Accuracy <- mean(specc1.Accuracies)
print(specc1.p.value)
print(specc2.p.value)
print(specc3.p.value)
print(specc1.Accuracy)

specc1 <- specc(clusteringdata,2)
par(mfrow=c(1,2))
plot(clusteringdata, col=specc1, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("black","red")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("black", "red"),legend=c("Benign","Malignant"))

#hypothesis test with confusion matrix


confusionMatrix(factor(specc1@.Data),cancer.data$diagnosis)


print(mod_multinomial.test(size(specc1),c(.5,.5)))


##3 clusters
specc2 <- specc(clusteringdata,3)

par(mfrow=c(1,2))
plot(clusteringdata, col=specc2, main="breast cancer clusters")

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("black","red")[cancer.data$diagnosis], main="Actual Clusters")
legend("topright",pch=c(1,1), col=c("black", "red"),legend=c("Benign","Malignant"))

#hypothesis test with confusion matrix

table(specc2@.Data,cancer.data$diagnosis)



multinomial.test(size(specc2),c(1/3,1/3,1/3))



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
