

#set working directory to the unzipped data location
setwd("/Projects/UCI HAR Dataset");

# Reading data from training files
features     = read.table('./features.txt',header=FALSE); 
actType = read.table('./activity_labels.txt',header=FALSE); 
subTrain = read.table('./train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
yTrain       = read.table('./train/y_train.txt',header=FALSE); 

colnames(actType)  = c('activityId','actType');
colnames(subTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Merge all the traing data
trainingData = cbind(yTrain,subTrain,xTrain);

# Reading data from testing files
subTest = read.table('./test/subject_test.txt',header=FALSE); 
xTest       = read.table('./test/x_test.txt',header=FALSE); 
yTest       = read.table('./test/y_test.txt',header=FALSE); 

colnames(subTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# Merge all the testing data
testData = cbind(yTest,subTest,xTest);


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData);

colNames  = colnames(finalData); 

# ID, mean() & stddev() - TRUE 
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];

finalData = merge(finalData,actType,by='activityId',all.x=TRUE);

colNames  = colnames(finalData); 

# Clean up
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};


colnames(finalData) = colNames;


# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActType  = finalData[,names(finalData) != 'actType'];

tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

tidyData    = merge(tidyData,actType,by='activityId',all.x=TRUE);

# Create tidyData file
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');