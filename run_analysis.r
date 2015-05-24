library(plyr)

# Utils: function add suffix
addSuffix<- function(x, suffix) {
  if (!(x %in% c("Subject","Activity"))) {
    paste(x,suffix, sep="")
  }
  else{
    x
  }
}

setwd("C:/GPYF/Training/Coursera - Data Science/03 - Getting and cleaning data/Week 3")

# Get data
pathfile<-file.path(getwd(),"UCI HAR Dataset")

pathfiletest<-file.path(pathfile, "test")
pathfiletrain<-file.path(pathfile, "train")

xtest<-read.table(file.path(pathfiletest,"X_test.txt"))
ytest<-read.table(file.path(pathfiletest,"Y_test.txt"))
subjecttest<-read.table(file.path(pathfiletest,"subject_test.txt"))

xtrain<-read.table(file.path(pathfiletrain,"X_train.txt"))
ytrain<-read.table(file.path(pathfiletrain,"Y_train.txt"))
subjecttrain<-read.table(file.path(pathfiletrain,"subject_train.txt"))

# Get activity labels 
activitylabels<-read.table(file.path(pathfile,
                                     "activity_labels.txt"),
                           col.names = c("Id", "Activity")
)

# Get features labels
featurelabels<-read.table(file.path(pathfile,
                                    "features.txt"),
                          colClasses = c("character")
)

#1. Merge training and test data to create one dataset.
traindata<-cbind(cbind(xtrain, subjecttrain), ytrain)
testdata<-cbind(cbind(xtest, subjecttest), ytest)
sensordata<-rbind(traindata, testdata)

sensorlabels<-rbind(rbind(featurelabels, c(562, "Subject")), c(563, "Id"))[,2]
names(sensordata)<-sensorlabels

#2. Extract only the measurements on the mean and standard deviation for each measurement.
sensordatameanstd <- sensordata[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(sensordata))]

#3. Use descriptive activity names to name the activities in the data set
sensordatameanstd <- join(sensordatameanstd, activitylabels, by = "Id", match = "first")
sensordatameanstd <- sensordatameanstd[,-1]

#4. Appropriately label the dataset with descriptive names.
names(sensordatameanstd) <- gsub("([()])","",names(sensordatameanstd))
# norm names
names(sensordatameanstd) <- make.names(names(sensordatameanstd))

#5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject 
cleandata<-ddply(sensordatameanstd, c("Subject","Activity"), numcolwise(mean))
# clean up column names
cleandataheaders<-names(cleandata)
cleandataheaders<-sapply(cleandataheaders, addSuffix, ".mean")
names(cleandata)<-cleandataheaders

write.table(cleandata, file = "average_activity_subject.txt", row.name=FALSE)
