#install.packages("stringi")
#install.packages("dplyr")
library(stringi)
library(dplyr)

# if directory does not exist, create one
if (!file.exists("~/test/GCD")) dir.create("~/test/GCD")
# set that as working directory
setwd("~/test/GCD")
# specify url from which data is coming and store as variable
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download data to wd, along with date downloaded; unzip the zip archive
download.file(fileUrl, destfile = "./dataset.zip")
dateDownloaded <- date()
dataset_list <- unzip("./dataset.zip") #character vector listing all files in the set

# TASK 1. Merge training and test sets to create one data set

#extract desired data from files, store it as intermediate data frames
trainlabels <- read.table(dataset_list[28]) #("train/y_train.txt") list-item 28
traindata <- read.table(dataset_list[27]) #("train/X_train.txt") list-item 27
testlabels <- read.table(dataset_list[16]) #("test/y_test.txt") list-item 16
testdata <- read.table(dataset_list[15]) #("test/X_test.txt") list-item 15
trainsubj <- read.table(dataset_list[26]) #("train/subject_train.txt") list-item 26
testsubj <- read.table(dataset_list[14]) #("test/subject_test.txt") list-item 14

#combine data frames
all.data <- bind_rows(traindata,testdata)
all.labels <- bind_rows(trainlabels,testlabels)
all.subj <- bind_rows(trainsubj,testsubj)
fullset <- bind_cols(all.subj,all.labels,all.data)
rm(trainlabels,traindata,testlabels,testdata,trainsubj,testsubj,all.data,all.labels, all.subj)

#label columns
features <- read.table("./UCI HAR Dataset/features.txt",stringsAsFactors = F)
colheds <- c("subject","activity", features[,2])
names(fullset) <- colheds
rm(features)
#to check: glimpse(fullset)

# TASK 2. Extracts only the measurements on the mean and standard deviation
# for each measurement.

#identify, locate, and extract columns
meanCols <- grep("mean",colheds)
stdCols <- grep("std",colheds)
main_dataset <- fullset[,c(1:2,c(meanCols,stdCols))]
rm(meanCols,stdCols,fullset)
# to check: glimpse(main_dataset)

# TASK 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("./UCI HAR Dataset/activity_labels.txt", colClasses = "factor")
main_dataset$activity <- as.factor(main_dataset$activity)
levels(main_dataset$activity) <- as.factor(activities$V2)
rm(activities,colheds)
#to check: main_dataset[1:50,1:5]

# TASK 4. Appropriately labels the data set with descriptive variable names.

# Code expands the abbreviations into human-readable words, cleans up confusing and unnecessary duplicated strings within variable names
# (duplications have no identification value), and confirms that all variable names remain unique.

names(main_dataset) <- stri_replace_all_fixed(names(main_dataset),
c("Acc","Gyro","Jerk","Mag","XYZ","()","Freq","tBody","tGravity","fBody","-mean","-std","-X","-Y","-Z"),
c("_acceleration", "_angularVelocity","_jerk","_signalMagnitude","triaxial","",
  "_frequency","time_body","time_gravity","frequency_body","_mean","_std","X","Y","Z"), vectorize_all = FALSE)

names(main_dataset) <- stri_replace_all_fixed(names(main_dataset),"bodyBody","body", vectorize_all = FALSE)
# to check: duplicated(names(main_dataset)) should return FALSE

# TASK 5. From the data set in step 4, creates a second, independent
# tidy data set with the average of each variable for each activity and
# each subject.

tidy_dataset <- main_dataset %>% group_by(activity,subject) %>% summarize_each(funs(mean))
write.table(tidy_dataset, file = "./tidy_dataset.txt", row.name=FALSE)
