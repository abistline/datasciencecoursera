
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

f = "data/wearable.zip"
if(!file.exists(f)) {
      download.file(url, destfile = f)
} 
if (!file.exists("data/UCI HAR Dataset")) {
      unzip(zipfile = f, exdir = "data")
}

library(dplyr)

# Summon the "features" variable into a 561 x 1 data frame.  Build a vector for
# quickly selecting only the features variables that corresponds to mean and 
# standard deviation
features = as.vector(read.table("data/UCI HAR Dataset/features.txt")[, 2])
mean_std = sort(c(grep("*mean", features), grep("*Mean", features), 
                       grep("*std", features)))

# Summon the sampled "train" values, X_train, into a 7352 x 86 data frame. 
# Summon y_train "activity" variable into a 7352 x 1 data frame.
# Summon the subjTrain "subject" variable into a 7352 x 1 data frame.
# Bind the subject and activity columns to the X_train data frame for a single
# "train" data frame with dimensions 7352 x 88.
X_train = read.table("data/UCI HAR Dataset/train/X_train.txt")[, mean_std]
y_train = read.table("data/UCI HAR Dataset/train/y_train.txt")
subjTrain = read.table("data/UCI HAR Dataset/train/subject_train.txt")
train = cbind(subjTrain, y_train, X_train)

# Summon the sampled "test" values, X_test, into a 2947 x 86 data frame.  
# Summon the y_test "activity" variable into a 2947 x 1 data frame.
# Summon the subjTest "subject" variable into a 2947 x 1 data frame.
# Bind the subject and activity columns to the X_test data frame for a single
# "test" data frame with dimensions 2947 x 88.
X_test = read.table("data/UCI HAR Dataset/test/X_test.txt")[, mean_std]
y_test = read.table("data/UCI HAR Dataset/test/y_test.txt")
subjTest = read.table("data/UCI HAR Dataset/test/subject_test.txt")
test = cbind(subjTest, y_test, X_test)

# Merge the train and test data frames into a single 10299 x 88 data frame
# and name the columns.  
# Note:  Using gsub to remove the "()" and "-" characters from the column names
train_test = rbind(train, test)
cleanFeatures = gsub("\\()", " ", features[mean_std])
cleanFeatures = gsub("-", " ", cleanFeatures)
colnames(train_test) = c("subjectID", "activity", cleanFeatures )

# Summon the activity-labels vector, and replace the numbers in the
# "activity" column with their corresponding labels.  Also re-arranging the
# dataset:  first by subjectID, then by activity.  
action = read.table("data/UCI HAR Dataset/activity_labels.txt")
train_test = mutate(train_test, 
                    activity = factor(as.character(train_test$activity), 
                    labels = as.character(action$V2)))
train_test = arrange(train_test, subjectID, activity)

# Quality Control check on the dimensions of the resulting data frame
# Should be 10299 x 88
print(dim(train_test))

# Create a new tidy dataset of the average value per subject and activity.
# Name all of the columns, except "subjectID" and "activity", 
# "Average of ...(measurement)"
avgMeasurements = dcast(melt(train_test, id = c("subjectID", "activity")),
                     subjectID + activity ~ variable, mean)
newNames = c("subjectID", "activity", sub("", "Average of ", 
                                          names(avgMeasurements[3:88])))
colnames(avgMeasurements) = newNames

# Quality control check on the dimensions of the resulting data frame
# Should be 180 x 88 (30*6 = 180)
print(dim(avgMeasurements))

# Create two new TIDY datasets in .txt format for future analysis
write.table(train_test, file = "data/UCI HAR Dataset/wearable.txt",
            row.names = FALSE)
write.table(avgMeasurements, file = 
                  "data/UCI HAR Dataset/subject_averages.txt",
            row.names = FALSE)


