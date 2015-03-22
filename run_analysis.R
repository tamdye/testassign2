
##Step 1:  Merge the training and test sets to create on data set
        
        trainFeatures <- read.table("./UCI HAR dataset/train/x_train.txt")
        trainSubjects <- read.table("./UCI HAR dataset/train/subject_train.txt")
        trainActivities <- read.table("./UCI HAR dataset/train/y_train.txt")
        totalTrainData <- cbind(trainSubjects,trainActivities,trainFeatures)

        testFeatures <- read.table("./UCI HAR dataset/test/x_test.txt")
        testSubjects <- read.table("./UCI HAR dataset/test/subject_test.txt")
        testActivities <- read.table("./UCI HAR dataset/test/y_test.txt")
        totalTestData <- cbind(testSubjects,testActivities,testFeatures)

        totalData <- rbind(totalTrainData,totalTestData)
        
##Step 2:  Extract only the measurements on the mean and standard deviation
        
        features <- read.table("./UCI HAR dataset/features.txt")

        meanCols <- grep("mean", features[,2])
        stdCols <- grep("std", features[,2])
        Cols <- c(1,2,sort(c(meanCols,stdCols))+2)  ##added two because Totaldata
                                                    ##has two more columns than
                                                    ##featuresColNames

        extractTotalData <- totalData[,Cols]

##Step 3:  Use descriptive activity names to name the activities in the dataset
        
        activityLabels <- read.table("./UCI HAR dataset/activity_labels.txt")

        mergedData <- merge(extractTotalData,activityLabels,by.x="V1.1",by.y="V1")

        reorderedData <- cbind(mergedData[,1],mergedData[,82],
                  mergedData[,3:81])

##Step 4:  Appropriately label the dataset with descriptive variable names

        variableLabels <- read.csv("./dataLabels.csv")
        tidyLabels <- as.character(variableLabels[,3])

        colnames(reorderedData) = c("subject","activity",tidyLabels)

        library(reshape2)
        FinalData <- melt(reorderedData,id=c("subject","activity"),
                        measure.vars=tidyLabels)

##Step 5:  create a second, independent tidy dataset with the average of each
##variable for each activity and each subject

        library(dplyr)
        groups <- group_by(FinalData,subject,activity,variable)
        sumData <- summarize(groups,avg=mean(value))
        write.table(sumData,"./tidydata.txt",row.name=FALSE)