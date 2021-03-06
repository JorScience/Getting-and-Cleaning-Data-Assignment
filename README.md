# Getting and Cleaning Data Assignment 
## run_analysis.R 
Using the UCI HAR Dataset, which can be downloaded from the following link: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
 
This script was written to achieve the following (not necessarily in this order): 
1. Merge the training and the test sets to create one data set; 
2. Extract only the measurements on the mean and standard deviation for each measurement; 
3. Use descriptive activity names to name the activities in the data set; 
4. Appropriately labels the data set with descriptive variable names. 
5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject. 
Note that the script is designed to download the data for you if you do not have it stored locally yet. Hence, make sure you set your working directory before running the script. The script does use the 'data.table' package, however, which you will have to install yourself. If you do not have the package installed yet, it will throw an error. 
## Running the script 
After having set your working directory (using 'setwd()'), save the script to the same directory and run it using the following code:
```
source("run_analysis.R")
``` 
After the script has completed running, you will have three data frames in your environment, namely `meanset`, `selectedset` and `totalset`. In addition, a tidy data set will have been stored to your working directory called 'Tidydataset.txt'. 
## The process 
1. The script checks if you have the required data for the analysis. If not, it downloads and unzips it for you, after which the zip file is removed; 
2. The script checks if you have data.table installed. If you do not have said package installed, it throws an error; 
3. Having succesfully completed steps 1 and 2, the script now reads all necesarry data into R. Specifically, it reads the activity labels, features, test data and train data; 
4. Using the `cbind()` and `rbind()` functions, the script merges the train and test set, including subject ID's and activity ID's as the first two columns. After merging the sets, the merged set is stored as `totalset`, after which any objects which are no longer required are removed; 
5. The script proceeds to give the activities in the data set descriptive names, using the activity labels data; 
6. Then, the dataset is subsetted to only include mean and standard deviation variables, by using the `grep()` function and creating an object called `extractor`, which is subsequently used for subsetting. After the subset is created, it is stored in the environment as `selectedset`  , after which any objects which are no longer required are removed; 
7. Using the subsetted data `selectedset`, the script labels the variables with descriptive names, using the `gsub()` function to clean up the variable names which were taken from the features file. Although this make the variable names more descriptive, do note that this does significantly increase the length of the variable names; 
8. Lastly, the script uses the data.table package to subset the `selectedset` to only include the means of each variable for each subject and each activity. The result is stored in `meanset`, which is also stored in your working directory as 'Tidydataset.txt'. The script then removes any objects it no longer needs. 
## The result 
As indicated in the process section of this readme, running the script will provide you with the following: 
1. `totalset` data frame in R. This is the dataset which resulted from merging the train and test data; 
2. `selectedset` data frame in R. This is the dataset which only includes data on mean and standard deviation variables; 
3. `meanset` data frame in R. This is the dataset which only includes the means of each variable (which are the mean and standard deviation variables of the original dataset) for each subject and each activity; 
4. 'Tidydataset.R' text file in your working directory. This file contains the `meanset` data as a text file.