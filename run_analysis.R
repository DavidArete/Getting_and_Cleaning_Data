##1. Merges the training and the test sets to create one data set.

##First of all load all the files.

##Load data files
TrainData <- read.table("train/X_train.txt")
TestData <- read.table("test/X_test.txt")

##Load Subject Data
TrainSubject <- read.table("train/subject_train.txt")
TestSubject <- read.table("test/subject_test.txt")

##load Activity names (column titles)
featuresList <- read.table("features.txt")

##Load Activity information
TestActivity <- read.table("test/y_test.txt")
TrainActivity <- read.table("train/y_train.txt")

##Then merge the data, the subjects(participants), and the code for the activity.
##This first bit of code merges the columns.
##Merge the data, activity and subject information
mergedTrain <- cbind(TrainSubject, TrainActivity, TrainData)
mergedTest <- cbind(TestSubject, TestActivity, TestData)

##Then this next bit merges the rows (basically the test and train data)
##Merge the data using rbind
mergedData <- rbind(mergedTrain, mergedTest)

##Now I need to add the column names.
##When the feaures list was imported in came in as a two column table, what I need is a vector
##First I delete the first column
##Delete the fist column of FeaturesList
featuresList <- featuresList[,2]

##Then turn it into a vecotor
unlist(featuresList)

##Then add this vector as the column titles. However I have to 'manually' add 
##Participant and Activity as the first two column names.
##Add column titles for activity and participants
featuresList <- c("Participant", "Activity", as.character(featuresList))

##Add the activity names as columnnames
colnames(mergedData) <- featuresList

##Create the (big) tidy data file.
write.table(mergedData, file="tidy_Step1.txt", row.name=FALSE, quote=FALSE)

##I think that is question 1 complete. 

##2. Extracts only the measurements on the mean and standard deviation for each measurement. 

##get the index of all the columns that have mean or std in them.
featuresListindex <- grep("mean|std", featuresList)

##Manually add the first two columns (becuase I need to keep them)
featuresListindex <- c(1, 2, featuresListindex)

##update merged data which just the correct columns
mergedData <- mergedData[, featuresListindex]

##Question 2 Done

##3. Uses descriptive activity names to name the activities in the data set

##gsub number for descrption
##1 WALKING
##2 WALKING_UPSTAIRS
##3 WALKING_DOWNSTAIRS
##4 SITTING
##5 STANDING
##6 LAYING

##Load Activity names
ActivityNames <- read.table("activity_labels.txt")

##in column 2 remove the _ and change to character (to allow the switch.
ActivityNames[, 2] = gsub("_", " ", as.character(ActivityNames[, 2]))

##this replaces the numeric code, which is actually a char with the descriptive title 
mergedData[,2] = ActivityNames[mergedData[,2], 2]

##Question 3 finished

##Appropriately labels the data set with descriptive variable names. 
##Make temp version
temp_cols <- names(mergedData)
##remove all the non-characters
temp_cols <- tolower(gsub("[^[:alpha:]]", "", temp_cols))
##now a bunch of others to tidy the column names
temp_cols <- gsub("fbody", "Freq Body ", temp_cols)
temp_cols <- gsub("tbody", "Time Body ", temp_cols)
temp_cols <- gsub("fgravity", "Freq Gravity ", temp_cols)
temp_cols <- gsub("tgravity", "Time Gravity ", temp_cols)
temp_cols <- gsub("gyro", "Gyro ", temp_cols)
temp_cols <- gsub("mean", " Mean ", temp_cols)
temp_cols <- gsub("std", " Std ", temp_cols)
temp_cols <- gsub("freq", "Freq", temp_cols)
temp_cols <- gsub("body", "", temp_cols)
temp_cols <- gsub("acc", " Accelorator ", temp_cols)
temp_cols <- gsub("jerk", " Jerk ", temp_cols)
temp_cols <- gsub("mag", " Mag ", temp_cols)
temp_cols <- gsub(" x", " X", temp_cols)
temp_cols <- gsub(" y", " Y", temp_cols)
temp_cols <- gsub(" z", " Z", temp_cols)
temp_cols <- gsub("  ", " ", temp_cols)

##and now pop them back as column names
colnames(mergedData) <- temp_cols

##Question 4 complete

##From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(reshape2)

##CREATE A temp to do this (perhaps not required, but it helped it make sense in my head to 
##have a smaller data set)
temp_cols <- temp_cols[3:81]

##The following comes almost compeltely from the reshaping data lecture.
##melt the variables into one. 
melt_data <- melt(mergedData, id=c("participant", "activity"), measure.vars=temp_cols)
##and now decast into the mean.
mean_data <- dcast(melt_data, participant + activity ~ variable, mean)

##Create the file
write.table(mean_data, "Tidy_Step5.txt", quote=FALSE, row.names=FALSE)

##Question 5 complete
