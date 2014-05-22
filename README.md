library(reshape);

#Put folder "UCI HAR Dataset" directly under current working directory
#workingdirectory/UCI HAR Dataset has all the input file for this program
#define read input file method
#all input files are from folder "UCI HAR Dataset"
#parameter XFile could be "X_test.txt" or "X_train.txt"
#parameter subjectFile could be "subject_test.txt" or "subject_train.txt"
#parameter yFile could be "X_train.txt" or "X_test.txt"

readInput <- function(XFile,subjectFile,yFile){
	
	XFile <- read.table(XFile);
	subjectFile <- read.table(subjectFile);
	yFile <- read.table(yFile);

	#name columns for "subject_test.txt" or "subject_train.txt"
	names(subjectFile) <- c("subject_label");
	
	#combine three files to get a table with first column as subject id, second column as activity label and rest to be required measurements
	output <- cbind(subjectFile,yFile,XFile);	
	output
	
}

#generate raw output file for test data
XFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt";
subjectFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt";
yFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt";
testOutput <- readInput(XFile, subjectFile, yFile);

#generate raw output file for training data
XFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt";
subjectFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt";
yFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt";
trainOutput <- readInput(XFile, subjectFile, yFile);

#combine both test and training data by row
rawOutput <- rbind(testOutput, trainOutput);

#read activity file "activity_labels.txt"
activityFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt";
activityFile <- read.table(activityFile);

#name the columns for "activity_labels.txt"
names(activityFile) <-c("activity_labels");

#name the second column of rawOutput
names(rawOutput)[2] <- "activity_labels";

#change the activity labels from "1,2,3,4,5,6" to decriptive strings like "SITTING, WALKING, LAYING STANDING..."
rawOutput$activity_labels <- factor(rawOutput$activity_labels, levels = activityFile[,1], labels = activityFile[,2]);

#factorize subject label
rawOutput$subject_label <- factor(rawOutput$subject_label);

#festuresFile is "features.txt"
featuresFile <- "getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt";
featuresFile <- read.table(featuresFile, stringsAsFactors=FALSE);

#rename the columns of features.txt input file
names(featuresFile) <- c("col","name");

#give each column in "subject_test.txt" or "subject_train.txt" the corresponding name in file features.txt
names(rawOutput)[3:dim(rawOutput)[2]] <- featuresFile$name;

#create a two column boolean matrix to indicate wheather each measurement name contains exactly the key word "mean()" or "std()"
booleanMatrix <- sapply(c("mean()", "std()"), grepl, names(rawOutput), fixed = TRUE);
	
#get the union of the two columns above to find all column names than contain key words "mean()" or "std()"
booleanMatrix <- booleanMatrix[,1] | booleanMatrix[,2];

#the first two columns of raw output are subject id and activity label and should be kept
booleanMatrix[1] <- TRUE;
booleanMatrix[2] <- TRUE;
	
#replce "-" with "_" in column names and "-" should not appear in column names
names(rawOutput) <- gsub("-","_",names(rawOutput));
	
#"(" and ")" should be removed from column names
names(rawOutput) <- gsub("\\(", "", names(rawOutput));
names(rawOutput) <- gsub(")", "", names(rawOutput));

#wirte rawoutput to .text file
write.table(rawOutput, "rawoutput.txt",col.names = NA, sep = "\t");

#keep only columns with column names contain key word "names()" or "std()" and the first two columns for subject id and activity label
tidyOutput <- rawOutput[,booleanMatrix];

#Creates a tidy data set with the average of each variable for each activity and each subject
x = melt(tidyOutput, id = c("subject_label", "activity_labels"), measure = names(tidyOutput)[3:dim(tidyOutput)[2]]);
tidyOutput <- cast(x, subject_label  + activity_labels ~ variable, fun = mean);

#append "_mean" to the column names of corresponding columns
names(tidyOutput)[3:dim(finaloutput)[2]] <- paste(names(tidyOutput)[3:dim(tidyOutput)[2]], "mean", sep = "_");

#write the final tidy data file to a .text file
write.table(tidyOutput, "tidyoutput.txt",col.names = NA, sep = "\t");
