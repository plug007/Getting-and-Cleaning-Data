xtrain_vlu <- read.table("~/Downloads/UCI HAR Dataset/train/X_train.txt")
xtest_vlu <- read.table("~/Downloads/UCI HAR Dataset/test/X_test.txt")
X <- rbind(xtrain_vlu, xtest_vlu)

strain_vlu <- read.table("~/Downloads/UCI HAR Dataset/train/subject_train.txt")
stest_vlu <- read.table("~/Downloads/UCI HAR Dataset/test/subject_test.txt")
S <- rbind(strain_vlu, stest_vlu)

ytrain_vlu <- read.table("~/Downloads/UCI HAR Dataset/train/y_train.txt")
ytest_vlu <- read.table("~/Downloads/UCI HAR Dataset/test/y_test.txt")
Y <- rbind(ytrain_vlu, ytest_vlu)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("~/Downloads/UCI HAR Dataset/features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_of_good_features]
names(X) <- features[indices_of_good_features, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set

activities <- read.table("~/Downloads/UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "~/Downloads/UCI HAR Dataset/result_set_with_the_averages.txt")