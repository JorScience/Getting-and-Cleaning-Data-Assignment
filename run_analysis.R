# Using the UCI HAR Dataset, which can be downloaded from the following link:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This script was written to achieve the following (not necessarily in this order):
#
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
#
# Note that this script was written to operate from the working directory where either the dataset 
# is already stored or where you want it to be stored, so make sure you set it beforehand. In
# addition, the final, tidy data set will also be saved in this directory.

# If necesarry, download and unzip the dataset
if (!file.exists("UCI HAR Dataset")) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip")
    unzip("data.zip")
    invisible(file.remove("data.zip"))
}

# Check and load required packages
if ("data.table" %in% rownames(installed.packages())) {
      library("data.table")
} else {
      stop("Install the data.table package first before running this script")
}

# Store activity labels and features
activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
features <- read.table("UCI HAR Dataset/features.txt")

# Store the labels and features as characters
activitylabels[,2] <- as.character(activitylabels[,2])
features[,2] <- as.character(features[,2])

# Load the test and train data
testdata <- read.table("UCI HAR Dataset/test/X_test.txt")
testactivities <- read.table("UCI HAR Dataset/test/y_test.txt")
testsubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

traindata <- read.table("UCI HAR Dataset/train/X_train.txt")
trainactivities <- read.table("UCI HAR Dataset/train/y_train.txt")
trainsubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

# Create and merge both datasets, with subject and activity as first two columns respectively, and name
# all columns accordingly
testset <- cbind(testsubjects, testactivities, testdata)
trainset <- cbind(trainsubjects, trainactivities, traindata)

totalset <- rbind(testset,trainset)
colnames(totalset) <- c("subjectID", "activityID", features[,2])
rm("testdata", "testactivities", "testsubjects", "traindata", "trainactivities", "trainsubjects", "testset", "trainset", "features")

# Use descriptive names for the activities in the dataset
totalset$activityID <- factor(totalset$activityID, levels = activitylabels[,1], labels = activitylabels[,2])
rm("activitylabels")

# Subset the dataset to only include measurements on the mean and standard deviation
extractor <- grep(".*mean.|.*std.", names(totalset), value = TRUE)
extractor <- grep(".meanFreq.", extractor, invert = TRUE, value = TRUE)
extractor <- append(names(totalset[, 1:2]), extractor)
selectedset <- totalset[, extractor]
rm("extractor")

# Label the set with descriptive variable names
colnames (selectedset) <- gsub("\\()", "", colnames (selectedset))
colnames (selectedset) <- gsub("(mean)", "Mean", colnames (selectedset))
colnames (selectedset) <- gsub("(std)", "Standard.Deviation", colnames (selectedset))
colnames (selectedset) <- gsub("^f", "frequency", colnames (selectedset))
colnames (selectedset) <- gsub("^t", "time", colnames (selectedset))
colnames (selectedset) <- gsub("(Acc)", "Accelerator", colnames (selectedset))
colnames (selectedset) <- gsub("(Gyro)", "Gyroscope", colnames (selectedset))
colnames (selectedset) <- gsub("(Mag)", "Magnitude", colnames (selectedset))

# Create a second, tidy data set with the average of each variable for each activity and each 
# subject
dt.selectedset <- data.table(selectedset)
meanset <- dt.selectedset[, lapply(.SD, mean), by = c("subjectID", "activityID")]
rm("dt.selectedset")

# Save the newly created dataset for submission
write.table(meanset, "Tidydataset.txt", row.names = FALSE)
