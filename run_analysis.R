# This script converts the raw data into tidy data by executing the following steps: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for 
# each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# make sure to have your working directory setup as the one where you'd like the data to 
# be copied into:

# download the raw data and unzip files:
URL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
setInternet2(TRUE)
download.file(URL, "UCI_HAR.zip")
unzip("UCI_HAR.zip")
setwd("./UCI HAR Dataset")
# read-in the files containing the 6 activities and 561 features, convert to tbl_df format:
activities <- tbl_df(read.table("activity_labels.txt"))
features <- tbl_df(read.table("features.txt"))
# create output directory for tidy data file:
if (!file.exists("tidy")){dir.create("tidy")}
setwd("./tidy/")
# remove all special characters from column names in features table:
features_clean <- gsub("\\(", "", features$V2)
features_clean <- gsub("\\)", "", features_clean)
features_clean <- gsub(",", "", features_clean)
features_clean <- gsub("-", "", features_clean)
#read in main data:
xtrain <- tbl_df(read.table("../train/X_train.txt"))
xtest <- tbl_df(read.table("../test/X_test.txt"))
# name the columns in main data:
names(xtrain)<- features_clean
names(xtest)<- features_clean
# column names have to be sweeped of all not allowed characters:
column_names_corr <- make.names(names=names(xtest), unique=TRUE, allow_ = TRUE)
names(xtest) <- column_names_corr 
names(xtrain) <- column_names_corr 
# select only wanted columns containing "mean" and "std":
mean_std<- grep("*[mM][Ee][Aa][Nn]*|*[sS][tT][dD]*", names(xtest), value = T)
xtest2 <- select(xtest, match(mean_std,names(xtest)))
xtrain2 <- select(xtrain, match(mean_std,names(xtrain)))
# read-in the activity IDs for both datasets, rename activity ID column to "activityId":
ytrain <- tbl_df(read.table("../train/y_train.txt"))
names(ytrain) <- "activityId"
ytest <- tbl_df(read.table("../test/y_test.txt"))
names(ytest) <- "activityId"
# read-in the subject IDs for both datasets, rename subject ID column to "subjectId"
subtrain <- tbl_df(read.table("../train/subject_train.txt"))
names(subtrain) <- "subjectId"
subtest <- tbl_df(read.table("../test/subject_test.txt"))
names(subtest) <- "subjectId"
# combine test and training data for main data, activity labels and subjects:
data <- bind_rows(xtrain2, xtest2)
labels <- bind_rows(ytrain, ytest)
subjects <- bind_rows(subtrain, subtest)
labels2 <- labels
# replace the activity IDs in labels with descriptions from activities file 
# (create a new "labels2" data frame):
for (i in 1:length(labels$activityId)){
  labels2$activityId[i] = as.character(activities$V2[labels$activityId[i]])
}
#put main data, activities and subjectIDs into the final data frame:
finalData <- data 
finalData$activity = labels2$activityId
finalData$subjectId = subjects$subjectId
#change the order of columns, activity first, subjectID 2nd, then all measured data:
finalData <- select(finalData, activity, subjectId, tBodyAccmeanX:angleZgravityMean)
#sort by activity, then subjectID
finalData2 <- tbl_df(arrange(finalData, activity, subjectId))
#take averages of all columns, grouped by activities and subjectIDs, write into new data set:
allthemeans<-tbl_df(ddply(finalData2, .(activity, subjectId), colwise(mean)))
allthemeans
write.csv(allthemeans, file = "output.csv")
write.table(allthemeans, file="output.txt", row.names=FALSE) 
