library(dplyr)

## 0. Read in data sets

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
Sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
Sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# get variable names
features <- read.table("./UCI HAR Dataset/features.txt")

# get activity labels
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

## 1. Merges the training and the test sets to create one data set.

X_total <- rbind(X_train, X_test)
Y_total <- rbind(Y_train, Y_test)
Sub_total <- rbind(Sub_train, Sub_test)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

extract_features <- var_names[grep("mean\\(\\)|std\\(\\)",var_names[,2]),]
X_total <- X_total[,extract_features[,1]]

## 3. Uses descriptive activity names to name the activities in the data set

colnames(Y_total) <- "Activity"
Y_total$activitylabel <- factor(Y_total$Activity, labels = as.character(act_labels[,2]))
activitylabel <- Y_total[,-1]

## 4. Appropriately labels the data set with descriptive variable names.

colnames(X_total) <- var_names[extract_features[,1],2]

## 5.  From the data set in step 4, creates a second, independent tidy data set with the average of 
##     each variable for each activity and each subject.

colnames(Sub_total) <- "subject"
total <- cbind(X_total, activitylabel, Sub_total)

total_mean <- total %>% 
        group_by(activitylabel, subject) %>% 
        summarize_each(funs(mean))

write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)