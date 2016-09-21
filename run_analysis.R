#########################################################################################################
##            RUN ANALYSIS R                                                                           ##
##             Olivier Aubert                                                                          ##
##                                                                                                     ##
#########################################################################################################

#########################################################################################################
## 1: Merges the training and the test sets to create one data set. 
#########################################################################################################

# Download the dataset then unzip the dataset:
FILE <- "getdata_dataset.zip"
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile="/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI_HAR_DATASET.zip", method="curl")
unzip ("/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI_HAR_DATASET.zip")

# TRAIN
setwd("/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI HAR Dataset/train")
X_train <-  read.table ("X_train.txt")
dim (X_train) 
View(X_train)
y_train <-  read.table ("y_train.txt")
dim (y_train) 
subject_train <-  read.table ("subject_train.txt")
dim (subject_train) 

# TEST
setwd("/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI HAR Dataset/test")
X_test <-  read.table ("X_test.txt")
dim (X_test) 
View(X_test)
y_test <-  read.table ("y_test.txt")
dim (y_test) 
subject_test <-  read.table ("subject_test.txt")
dim (subject_test) 
names (subject_test)

## Rename the database : train sets
setwd("/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI HAR Dataset")
features <- read.table ("features.txt")
colnames (X_train) <- features[,2]
colnames (y_train) <- "activityId"
colnames (subject_train) <- "subjectId"

## Merge the train sets
TRAINSETS <- cbind (y_train, subject_train, X_train)
View (TRAINSETS)

## Rename the database : test sets
setwd("/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI HAR Dataset")
features <- read.table ("features.txt")
colnames (X_test) <- features[,2]
colnames (y_test) <- "activityId"
colnames (subject_test) <- "subjectId"

## Merge the test sets
TESTSETS <- cbind (y_test, subject_test, X_test)
View (TESTSETS)

## Merge the TRAIN and TEST SETS
FINAL_DATABASE <- rbind (TRAINSETS,TESTSETS)
dim (FINAL_DATABASE)

#########################################################################################################
## 2 Extracts only the measurements on the mean and standard deviation for each measurement.
#########################################################################################################
names (FINAL_DATABASE)

COLNAMES <- colnames (FINAL_DATABASE)
FINAL_DATABASE_MEAN_SD <- FINAL_DATABASE[grepl("mean|std|activityId|subjectId", COLNAMES)]
View(FINAL_DATABASE_MEAN_SD)


#########################################################################################################
## 3 Uses descriptive activity names to name the activities in the data set
#########################################################################################################

## Download the descriptive activity names
setwd("/Users/olivieraubert/Desktop/R/EXERCIE_COURSERA/UCI HAR Dataset")
activity_labels <- read.table ("activity_labels.txt")
View (activity_labels)

## Rename the columns 
colnames(activity_labels) <- c("activityId", "activitylabels")

## Add the descriptive activity names
FINAL_DATABASE_MEAN_SD_2<- join (FINAL_DATABASE,activity_labels, by="activityId")
table (FINAL_DATABASE_MEAN_SD_2$activitylabels)


#########################################################################################################
## 4 Appropriately labels the data set with descriptive variable names.
#########################################################################################################

names (FINAL_DATABASE_MEAN_SD_2) <- make.names(names (FINAL_DATABASE_MEAN_SD_2))
names (FINAL_DATABASE_MEAN_SD_2)

## Romove the ()
names (FINAL_DATABASE_MEAN_SD_2) <- gsub ("\\(|\\)", "", names (FINAL_DATABASE_MEAN_SD_2))
names (FINAL_DATABASE_MEAN_SD_2)

## replace the -
names (FINAL_DATABASE_MEAN_SD_2) <- make.names(names (FINAL_DATABASE_MEAN_SD_2))
names (FINAL_DATABASE_MEAN_SD_2)

## Rename to better explain 
# Mean
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("\\.mean",".Mean",names(FINAL_DATABASE_MEAN_SD_2))
# Standardeviation
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("\\.std","Standardeviation",names(FINAL_DATABASE_MEAN_SD_2))
# Frequency
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("Freq\\.","Frequency.",names(FINAL_DATABASE_MEAN_SD_2))
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("Freq$","Frequency",names(FINAL_DATABASE_MEAN_SD_2))
# Meximum
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("max","Maximum",names(FINAL_DATABASE_MEAN_SD_2))
# Minimum
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("min","Minimum",names(FINAL_DATABASE_MEAN_SD_2))
# Acceleration
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("Acc","Acceleration",names(FINAL_DATABASE_MEAN_SD_2))
# Angularacceleration
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("GyroJerk","Angularacceleration",names(FINAL_DATABASE_MEAN_SD_2))
# Angularspeed
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("Gyro","Angularspeed",names(FINAL_DATABASE_MEAN_SD_2))
# Magnitude
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("Mag","Magnitude",names(FINAL_DATABASE_MEAN_SD_2))
#Time
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("^t","Time",names(FINAL_DATABASE_MEAN_SD_2))
#Freq
names(FINAL_DATABASE_MEAN_SD_2) <- gsub("^f","FrequencyDomain.",names(FINAL_DATABASE_MEAN_SD_2))

names (FINAL_DATABASE_MEAN_SD_2)


#########################################################################################################
## 5 From the data set in step 4, creates a second, independent tidy data set with the average of each 
##  variable for each activity and each subject.
#########################################################################################################
TidyData <- aggregate(. ~subjectId + activityId, FINAL_DATABASE_MEAN_SD_2, mean)
TidyData <- TidyData[order(TidyData$subjectId, TidyData$activityId),]
View (TidyData)
## Create the Data
write.table(TydiData, file = "average_variable_for_each_subject.txt", row.name=FALSE)

