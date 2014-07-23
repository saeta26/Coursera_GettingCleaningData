library(data.table) # for fread()

# A function that expresses a given string in camel-case, 
# and removes the characters "(",")",",","-" from the input string.
#
simpleCap <- function(x) {
  s <- strsplit(x, "-")[[1]]
  s1<-paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
  s1<-gsub("^\\s+|\\s+$|\\(|\\)| |,", "", s1)
}

# read in training-data
trainDat<-read.table("train/X_train.txt")
trainLab<-read.table("train/y_train.txt")
trainSubj<-read.table("train/subject_train.txt")

# read in test-data
testDat<-read.table("test/X_test.txt")
testLab<-read.table("test/y_test.txt")
testSubj<-read.table("test/subject_test.txt")
# read the file containing list of feature-names
featList<-fread("features.txt")

#
#1. combine the two datasets col.-wise (vertically)
#
dsDat<-rbind(trainDat, testDat) #table of combined training and test data
dsLab<-rbind(trainLab, testLab) #table of associated activity-labels for training and test
dsSubj<-rbind(trainSubj, testSubj) #table of subjects corresponding to the training- and test-data.

#
#2. From combined dataset, extract features corresponding to means and stds of other features
#    a. first figure out which features correspond to mean and std. of other features.
#       To do this, look for the strings "-mean()" and "-std()" in the feature-names
meanFeatList<-grep("-mean()", featList$V2)
stdFeatList<-grep("-std()", featList$V2)
statFeatList<-sort(c(meanFeatList, stdFeatList)) 
#statFeatList contains list of feature-ids corresponding to mean and std. of other features
#    b. extract features from dsDat that correspond to features listed in statFeatList
dsDatSubset<-dsDat[,statFeatList]

#
#3. Generate list of descriptive text-labels for the activities corresponding 
#   to the data in the combined data-set
#
activityNames<-c("Walking", "WalkingUpstairs", "WalkingDown", "Sitting", "Standing", "Lying")
dsl<-dsLab[[1]] #dsLab is a list of activity-labels read in from the input files Y_{train, text}.txt
descriptiveLabels<-activityNames[dsl]
#length(descriptiveLabels)
#descriptiveLabels is contains descriptive activity-labels for the 10299 rows of the combined dataset

#
#4. generate descriptive names for the features, from the names given in file features.txt
#  To generate the descriptive names, I remove all non-alphanumeric characters from supplied name
#  and represent the resulting text in camel-case format.
#
featureNames<-array()
for (i in seq(length(featList$V2))) {
  n1<-simpleCap(featList$V2[i]) #function simpleCap() is defined at the top of the file.
  featureNames<-append(featureNames, n1)
}
featureNames<-na.omit(featureNames)
#Array 'featureNames' now contains 561 descriptive feature-names, in camelCase text, 
#without any non-alphanumeric characters.
#
#Now assign the generated names to the combined dataset: dsDat
names(dsDat)<-featureNames

#
#5. This task is completed in 3 steps:
# a. group data by activity into 'activity-groups'
# b. in each activity-group, extract data-subset for each subject, and compute mean for this subset.
# c. construct a table of the computed means.
#
numActivities<-6
numSubjects<-30
dfResult<-data.frame(matrix(ncol=561, nrow=0))
#names(dfResult)<-featureNames

for (a in seq(numActivities)) #loop over activities (6 iterations)
{
  actId<-which(dsLab==a) # indices where this activity-label (a) appears in dsLab
  activityGroup<-dsDat[actId,] #subset of data corresponding to this activity
  subjectGroup<-dsSubj[actId,] #subset of subjects corresponding to this activity
  
  for (s in seq(numSubjects)) #for this activity, loop over each subject
  {
    subjId<-which(subjectGroup==s) #indices of subject 's' for activity 'a'
    actSubj<-subjectGroup[subjId]
    dsActSubj<-activityGroup[subjId,]
    actSubjMean<-colMeans(dsActSubj) #mean of data-frame for this activity-subject combination
    dfResult<-rbind(dfResult,actSubjMean) #append mean-vector to existing output data-frame    
  }
}
dfResult<-na.omit(dfResult) #remove any NA rows, if any exist by chance.
#Assign names to the new dataset
names(dfResult)<-featureNames

write.table(dfResult,file="ActivitySubjectMeans.txt", na = 'NA', sep = ' ',
            row.names = F, col.names = F)

write.table(names(dfResult), file="result_features.txt")