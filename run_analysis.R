# This file contains the script used to perform the analysis of the data
# available for the Getting and Cleaning Data Course Project. It comprises
# the following steps:
# 1 - Merges the training and the test sets to create one data set.
# 2 - Extracts only the measurements on the mean and standard deviation for
#     each measurement.
# 3 - Uses descriptive activity names to name the activities in the data set
# 4 - Appropriately labels the data set with descriptive variable names.
# 5 - From the data set in step 4, creates a second, independent tidy data set
#     with the average of each variable for each activity and each subject.


# 1 - Merges the training and the test sets to create one data set.

# Load all the data required:
# - Column names
featureNames <- read.table("./dataset/features.txt")[,2]
# - Activity labels
activityLabels <- read.table("./dataset/activity_labels.txt")[,2]
# - Train data
subjectTrain <- read.table("./dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("./dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("./dataset/train/X_train.txt", header = FALSE)
# - Test data
subjectTest <- read.table("./dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("./dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("./dataset/test/X_test.txt", header = FALSE)
# Merge Train and Test data:
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames)
colnames(activity) <- "activity"
colnames(subject) <- "subject"
# Result of the merge:
fullDataSet <- cbind(features,activity,subject)

# 2 - Extracts only the measurements on the mean and standard deviation for
#     each measurement:
extract_features <- grep("mean|std", names(fullDataSet))
reducedDataSet <- fullDataSet[ , c(extract_features, 562, 563)]

# 3 - Uses descriptive activity names to name the activities in the data set:
reducedDataSet$activity <- as.character(reducedDataSet$activity)
for (i in 1:6){
        reducedDataSet$activity[reducedDataSet$activity == i] <-
                as.character(activityLabels[i])
}
reducedDataSet$activity <- as.factor(reducedDataSet$activity)

# 4 - Appropriately labels the data set with descriptive variable names:

# My criteria to substitute the column names for others more descriptive is
# as follows...
names(reducedDataSet)<-gsub("Acc", "Accelerometer", names(reducedDataSet))
names(reducedDataSet)<-gsub("Gyro", "Gyroscope", names(reducedDataSet))
names(reducedDataSet)<-gsub("BodyBody", "Body", names(reducedDataSet))
names(reducedDataSet)<-gsub("Mag", "Magnitude", names(reducedDataSet))
names(reducedDataSet)<-gsub("^t", "Time", names(reducedDataSet))
names(reducedDataSet)<-gsub("^f", "Frequency", names(reducedDataSet))
names(reducedDataSet)<-gsub("tBody", "TimeBody", names(reducedDataSet))
names(reducedDataSet)<-gsub("-mean()", "Mean", names(reducedDataSet), ignore.case = TRUE)
names(reducedDataSet)<-gsub("-std()", "STD", names(reducedDataSet), ignore.case = TRUE)
names(reducedDataSet)<-gsub("-freq()", "Frequency", names(reducedDataSet), ignore.case = TRUE)
# Making it more coherent:
names(reducedDataSet)<-gsub("angle", "Angle", names(reducedDataSet))
names(reducedDataSet)<-gsub("gravity", "Gravity", names(reducedDataSet))
names(reducedDataSet)<-gsub("activity", "Activity", names(reducedDataSet))
names(reducedDataSet)<-gsub("subject", "Subject", names(reducedDataSet))

# 5 - From the data set in step 4, creates a second, independent tidy data set
#     with the average of each variable for each activity and each subject.
reducedDataSet$Subject <- as.factor(reducedDataSet$Subject)
reducedDataSet <- data.table(reducedDataSet)
tidyData <- aggregate(. ~Subject + Activity, reducedDataSet, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)




