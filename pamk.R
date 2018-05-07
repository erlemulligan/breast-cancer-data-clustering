pdf("pamKPlots.pdf")
library(fpc) 

dataSource <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data'
dataFilePath <- './data/cancer.data'
dataDirectoryPath <- './data'

if (!dir.exists(dataDirectoryPath)) {
  dir.create(dataDirectoryPath)
}

if (!file.exists(dataFilePath)) {
  download.file(dataSource, destfile=dataFilePath)
}

#cancer.data <- read.csv(dataFilePath, fileEncoding = "UTF-8", header = F)
cancer.data <- read.csv(file="data.csv", header = TRUE, sep=",")
cancer.data$diagnosis <- factor(cancer.data$diagnosis, labels = c(2,1))

library(kernlab)
library(cluster)
library(fpc)

bcdata <- as.matrix(cancer.data[,3:32],header=T)
                                                              

pamk.result <- pamk(bcdata)

# how many clusters?
pamk.result$nc

# check clustering against actual 
table(pamk.result$pamobject$clustering, cancer.data[,2])
layout(matrix(c(1,2),1,2))
# two graphs side by side
plot(pamk.result$pamobject,main="Cluster by k-medoid method")
layout(matrix(1))

# now try pam with k=2
library(cluster)
pam.result <- pam(bcdata, 2)
table(pam.result$clustering, cancer.data[,2])


# now try pam with k=3
pam.result <- pam(bcdata, 3)
table(pam.result$clustering, cancer.data[,2])


# Try a confusion matrix

install.packages("caret")
library(caret)

confusionMatrix(factor(pamk.result$pamobject$clustering),cancer.data$diagnosis)

#try multinomial test
## Load the EMT package:
install.packages("EMT")
library(EMT)


## Input data for a three-dimensional case:
observed <- c(356,138)  # observed data: 5 items in category one, 2 items in category two, 1 item in category three
prob <- c(0.6274, 0.3726) 	# model: hypothetical probability that an item falls into category one, two, or three


## Calculate p-value using default options:
out <- multinomial.test(observed, prob)        


## Plot the probabilities for each event:
plotMultinom(out)


## Calculate p-value for the same input using Pearson's chisquare as a distance measure:
out <- multinomial.test(observed, prob, useChisq = TRUE)  

par(mfrow=c(1,2))
plot(bcdata, col=pamk.result$pamobject$clustering, main="Assignment by k-medoids")
legend("topright",pch=c(1,1), col=c("red", "black"),legend=c("Benign","Malignant"))

plot(cancer.data$radius_mean,cancer.data$texture_mean, col=c("red","black")[cancer.data$diagnosis], main="Actual Cluster Assignment by Diagnosis")
legend("topright",pch=c(1,1), col=c("red", "black"),legend=c("Benign","Malignant"))

# plot shows color as well as number of cluster
plotcluster(bcdata,pamk.result$pamobject$clustering)

#Close pdf file ** IMPORTANT!
graphics.off()
