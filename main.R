##### SPAMBASE DATASET #####

##### Session Information #####
sessionInfo()

##### Downloading File #####
if(!file.exists("Data/spambase.data")) {
        download.file(url = "http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data",
                      destfile = "Data/spambase.data")
}

if(!file.exists("Data/spambase.zip")) {
        download.file(url = "http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.zip",
                      destfile = "Data/spambase.zip")
}

#unzip("Data/spambase.zip")
?unzip
##### Reading File #####

#Data File
data_raw <- read.csv("Data/spambase.data", header = F)

#Names File
library(readr)
data_raw_names <- read.delim("Data/spambase.names", header = FALSE)
data_raw_names <- data_raw_names[-(1:30),]
data_raw_names <- as.data.frame(data_raw_names)

library(dplyr)
library(tidyr)
data_raw_names <- data_raw_names %>%
        separate(data_raw_names, c("Variable", "Type"), sep = ":")


#Assigning Name to Dataset
names(data_raw) <- data_names$V1
names(data_raw)[is.na(names(data_raw))] <- "classes"
colnames(data_raw)

##### Dataset Description #####
#Dimension of Dataset
if(!require(base)) install.packages("base")
library(base)
dim(data_raw)

#Structure of Dataset
str(data_raw)

#Names of columns
names(data_raw)

#Viewing Dataset
head(data_raw, n = 10)

#Summary of Dataset
summary(data_raw)


##### Data PreProcessing #####
data <- data_raw

#Checking for missing values
any(is.na(data))

#Number of Missing Values
sum(is.na(data))

#Missing values in each column
sapply(data, FUN = function(x) (sum(is.na(x))))

#Percentage of Missing Values in each column
sapply(data, FUN = function(x) 
        if(any(is.na(x)) == TRUE){ mean(is.na(x))*100 }
)

#Dropping columns with more than 80% Null Values
data <- data[, which(colMeans(!is.na(data)) > 0.8)]

#Dimension of Clean data
dim(data)

#Exploring Spam Class Column
table(data$classes)

#Converting Classes to factors
data$classes <- as.factor(data$classes)
levels(data$classes)

#Renaming levels of Diagnosis Column
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

data$classes <- recode(data$classes,
                         "0" = "Not Spam",
                         "1" = "Spam")

levels(data$classes)

#Total Number of Benign and Malignant
summary(data$classes)

#Percentage of Benign and Malignant
round(prop.table(table(data$classes))*100, 2)

#Summary of make word frequency
summary(data$word_freq_make)

#Normalizing Function
normalize <- function(x) {
        return( (x - min(x))/ (max(x) - min(x)))
}
NormalizeData <- as.data.frame(lapply(data[1:57], normalize))

#Summary of Normalized Data make word frequency
summary(NormalizeData$word_freq_make)

#Clean Data
CleanData <- cbind(data[, 58], NormalizeData)
names(CleanData)[names(CleanData) == "data[, 58]"] <- "class"
CleanData$class <- as.character(CleanData$class)
str(CleanData)


##### Splitting the Data into Train and Test Datasets #####
set.seed(1234)
samp <- sample(nrow(CleanData),0.80*nrow(CleanData))
TrainData <- CleanData[samp,]
TestData <- CleanData[-samp,]

TrainLabels <- TrainData[, 1]
TestLabels <- TestData[, 1]

if(!require(class)) install.packages("class")
library(class)

TestPredict <- knn(train = TrainData[, 2:58], 
                   test = TestData[, 2:58],
                   cl = TrainLabels, k = 1)

if(!require(gmodels)) install.packages("gmodels")
library(gmodels)

CrossTable(x = TestLabels, 
           y = TestPredict,
           prop.chisq = FALSE)

# Z - score Transformation
ZScoreData <- as.data.frame(scale(data[-58]))
summary(ZScoreData)

#Updated Data
UpdateData <- cbind(data[, 58], ZScoreData)
names(UpdateData)[names(UpdateData) == "data[, 58]"] <- "class"
UpdateData$class <- as.character(UpdateData$class)
class(UpdateData$class)

#set.seed(12345)
samp1 <- sample(nrow(UpdateData),0.80*nrow(UpdateData))
TrainData1 <- UpdateData[samp1,]
TestData1 <- UpdateData[-samp1,]

TrainLabels1 <- TrainData[, 1]
TestLabels1 <- TestData[, 1]

if(!require(class)) install.packages("class")
library(class)

TestPredict1 <- knn(train = TrainData1[, 2:31], 
                    test = TestData1[, 2:31],
                    cl = TrainLabels1, k = 3)

if(!require(gmodels)) install.packages("gmodels")
library(gmodels)

CrossTable(x = TestLabels1, 
           y = TestPredict1,
           prop.chisq = FALSE)

