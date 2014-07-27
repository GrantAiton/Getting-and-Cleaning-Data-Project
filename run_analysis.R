#download and extract data files
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "getdata-projectfiles-UCI HAR Dataset.zip")
}
if(!file.exists("UCI HAR Dataset")){
  unzip("getdata-projectfiles-UCI HAR Dataset.zip") 
}

#read in test and train datasets
test <- read.table("./UCI HAR Dataset/test/X_test.txt")
train <- read.table("./UCI HAR Dataset/train/X_train.txt")

#add labels
rawtestlabels <- read.table("./UCI HAR Dataset/test/y_test.txt")
rawtrainlabels <- read.table("./UCI HAR Dataset/train/y_train.txt")
rawtestlabels <- rawtestlabels[,1]
rawtrainlabels <- rawtrainlabels[,1]

#create string of descriptive labels with for loop
testlabels <- NULL
for(i in rawtestlabels) {
  if(i == 1) {
    testlabels <- append(testlabels, "WALKING")
  } else if(i == 2) {
    testlabels <- append(testlabels, "WALKING_UPSTAIRS")
  } else if(i == 3) {
    testlabels <- append(testlabels, "WALKING_DOWNSTAIRS")
  } else if(i == 4) {
    testlabels <- append(testlabels, "SITTING")
  } else if(i == 5) {
    testlabels <- append(testlabels, "STANDING")
  } else if(i == 6) {
    testlabels <- append(testlabels, "LAYING")
  }
}
testlabels <- as.factor(testlabels)

trainlabels <- NULL
for(i in rawtrainlabels) {
  if(i == 1) {
    trainlabels <- append(trainlabels, "WALKING")
  } else if(i == 2) {
    trainlabels <- append(trainlabels, "WALKING_UPSTAIRS")
  } else if(i == 3) {
    trainlabels <- append(trainlabels, "WALKING_DOWNSTAIRS")
  } else if(i == 4) {
    trainlabels <- append(trainlabels, "SITTING")
  } else if(i == 5) {
    trainlabels <- append(trainlabels, "STANDING")
  } else if(i == 6) {
    trainlabels <- append(trainlabels, "LAYING")
  }
}
trainlabels <- as.factor(trainlabels)

test[,562] <- testlabels
train[,562] <- trainlabels

#add subjects
testsubject <- read.table("./UCI HAR Dataset/test/subject_test.txt", colClasses = "factor")
trainsubject <- read.table("./UCI HAR Dataset/train/subject_train.txt", colClasses = "factor")
test[,563] <- testsubject
train[,563] <- trainsubject


#merge data
alldata <- merge(test, train, all = TRUE)

#label columns
cnames <- read.table("./UCI HAR Dataset/features.txt", colClasses = c("character", "character"))
cnames <- cnames[,2]
cnames <- append(cnames, "labels")
cnames <- append(cnames, "subjects")
colnames(alldata) <- cnames

#extract means and standard deviation
cols <- grep("mean|std", colnames(alldata))
allmeansstd <- alldata[,cols]
allmeansstd$labels <- alldata$labels
allmeansstd$subjects <- alldata$subjects

#Uses descriptive activity names to name the activities in the data set

#Appropriately labels the data set with descriptive variable names. 

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
install.packages("reshape2")
library("reshape2")
allmelted <- melt(allmeansstd, id = c("subjects", "labels"), measure.vars = colnames(allmeansstd)[1:79])
bysubjectandlabel <- dcast(allmelted, subjects + labels ~ variable, mean)
write.table(bysubjectandlabel, "bysubjectandlabel.txt")
