loadAndMergeData <- function () {
  # Load libraries
  library(reshape2)
  library(dplyr)
  
  # init variables about subdirectory, zipfilename and zipfilepath
  subdirectory              <- "./data/"
  zipfilename               <- "UCI HAR Dataset.zip"
  zipfilepath               <- paste0(subdirectory, zipfilename)
  
  # If subdirectory "data" doesn't exists, then create this.
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  # If zip file isn't stored in subdirectory data, it will be downloaded.
  if (!file.exists(zipfilepath)){
    fileURL                 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, zipfilepath, method="curl")
  }  
  
  # Upzip downloaded zip file
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(zipfilepath, exdir = subdirectory) 
  }
  
  # Load "activity labels" & "features" and convert them into character.
  activityLabels            <- read.table(paste0(subdirectory, 
                                                 "UCI HAR Dataset/activity_labels.txt"), 
                                                  col.names=c("activityId", "activity"))
  activityLabels[,2]        <- as.character(activityLabels[,2])
  features                  <- read.table(paste0(subdirectory, 
                                                 "UCI HAR Dataset/features.txt"), 
                                                  col.names=c("featureId", "featureLabel"))
  features[,2]              <- as.character(features[,2])
  
  # Extract only the data about mean and standard deviation
  features.MeanAndStd       <- grep(".*mean.*|.*std.*", features[,2])
  features.MeanAndStd.names <- features[features.MeanAndStd,2]
  
  # Adjust labels into CamelCase notation and without dashs
  features.MeanAndStd.names = gsub('-mean', 'Mean', features.MeanAndStd.names)
  features.MeanAndStd.names = gsub('-std', 'Std', features.MeanAndStd.names)
  features.MeanAndStd.names <- gsub('[-()]', '', features.MeanAndStd.names)
  
  # Load test data
  set.test                  <- read.table(paste0(subdirectory, "UCI HAR Dataset/test/X_test.txt"))
  activities.test           <- read.table(paste0(subdirectory, "UCI HAR Dataset/test/Y_test.txt"))
  subjects.test             <- read.table(paste0(subdirectory, "UCI HAR Dataset/test/subject_test.txt"))
  
  # Load training data
  set.training              <- read.table(paste0(subdirectory, "UCI HAR Dataset/train/X_train.txt"))
  activities.training       <- read.table(paste0(subdirectory, "UCI HAR Dataset/train/Y_train.txt"))
  subjects.training         <- read.table(paste0(subdirectory, "UCI HAR Dataset/train/subject_train.txt"))

  # Merge "subject data" from training and test
  subject.merged            <- rbind(subjects.test, subjects.training)
  # Rename subject identifier column in "subjectId"
  names(subject.merged)     <- "subjectId"
  
  # Merge "set data" from test and training data and select only mean and standard deviation values
  set.merged                <- rbind(set.test, set.training)
  set.merged                <- set.merged [, features.MeanAndStd]
  # Rename column name from selected feature labels
  names(set.merged)         <- features.MeanAndStd.names
  
  # Merge "activity data" from test and training data
  activities.merged         <- rbind(activities.test, activities.training)
  # Rename activity identifier column in "activityId"
  names(activities.merged)  = "activityId"
  # Merge activity data and activity labels
  activities.merged         <- merge(activities.merged, 
                                     activityLabels, 
                                     all.x = TRUE,
                                     by="activityId")
  
  # Merge merged subject data, merged activity data and merged set data
  data.merged               <- cbind(subject.merged, activity = activities.merged$activity, set.merged)
  # Write merged data into the file "tidy_data.txt"
  write.table(data.merged, "merged_data.txt")
  
  # Calculte the average of each variable grouped by activity and subject.
  data.merged <- tbl_df(data.merged)
  data.tidy <- data.merged %>% group_by(subjectId, activity) %>% summarise_each(funs(mean))
  write.table(data.tidy, "tidy_data.txt")
}
