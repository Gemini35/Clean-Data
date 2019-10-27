#############################################################
#####PROJECT TIDY DATA#######################################
##BY JULIO C
rm(list=ls())

library(dplyr)
library(RCurl)
library(jpeg)
library(data.table)

# LOAD AGNOSTIC LABELS AND AND SUBJECTS

feature<-fread("./data/features.txt") # contains all different feature testing performed
# each row is a record mapping to an activity and subject
activity_labels<-fread("./data/activity_labels.txt") # labels for each activity will 
#require to be mapped to each record

#  ANALYZE  Train FIRST


subject_train<-fread("./data/subject_train.txt") # this is the subject labels
train.labels<-fread("./data/y_train.txt") # this data gives activity labels
# to each record
train<-fread("./data/X_train.txt") # this is the train data

col.name<-feature$V2 #put feature activity labels in column format
names(train)<-c(col.name) # rename column to labels

#index<-c(which(duplicated(names(train)))) # KEEPING TRASH NOT SURE WHY :)

index<-which(duplicated(names(train)))
#index.df<-as.data.frame(index) #TRASH...KEEP ON ON TRYING..A VACA FOI PRO BREJO
index.mat<-as.matrix(index)
#train.new<-train[,..index.mat] # TRASH...WHAT DA HECK...IT WARN'T DAT!
train[,index.mat]<-NULL # REMOVING DUPLICATE COLLUMMS 
train.stats<-select(train,contains("mean"),contains("std"))# KEEPING ONLY MEAN AND STD

# CLEANING UP ACTIVTY AND SUBJECT

train.labels.activity<-left_join(train.labels,activity_labels, by="V1") #ADDING NAMES TO LABLES
train.labels.activity[,1]<-NULL #REMOOVING THE CODES FOR THE LABLES
train.subj.act<-cbind(train.labels.activity,subject_train) # BINDING THE SUBJ WITH ACTTIVITY

names(train.subj.act)<-c("ACTIVITY","SUBJECT") # rename column to labels activity 
# and subject

####DRUM ROLLS PLEAZ...BINDING ACTIVITY AND SUBJ WITH FEATURE DATA

train.tidy<-cbind(train.subj.act,train.stats) # voila..

#USING "funs" gives warning Warning message: funs() is soft deprecated..
#but still gives results
#train.summary<- train.tidy %>%
# group_by(ACTIVITY,SUBJECT) %>%
#summarise_all(funs(mean))

#USISNG LIST WORKS AS WELL AND NO WARNING MESSAGE ...DONT QUIE UNDERSTAND BUT 
#IT WORKS AND NEED TO MOVE ON

# SUMMARY FOR TRAIN DATA ONLY
train2.summary<- train.tidy %>%
        group_by(ACTIVITY,SUBJECT) %>%
        summarise_all(list(~mean(.)))


#data.merged<-merge(train,test,all = TRUE)
#col.name<-feature.train$V2
#names(data.merged)<-c(col.name)
#noob
#grep('mean',names(data.merged),value=TRUE)

#test<-data.merged %>% select(grep('mean',col.name,value=TRUE))
#test<-data.merged %>% select(contains('mean',names(data.merged)))
#test<-select(data.merged,matches("mean"))



#########################################################################


#START ANALYZING TEST DATA SPARING THE COMMENTS :)

subject_test<-fread("./data/subject_test.txt") # this is the subject labels
test.labels<-fread("./data/y_test.txt") # this data gives activity labels
# to each record
test<-fread("./data/X_test.txt") # this is the train data

col.name<-feature$V2 #put feature activity labels in column format
names(test)<-c(col.name) # rename column to labels

index<-which(duplicated(names(test)))
index.mat<-as.matrix(index)

test[,index.mat]<-NULL # REMOVING DUPLICATE COLLUMMS 
test.stats<-select(test,contains("mean"),contains("std"))# KEEPING ONLY MEAN AND STD

# CLEANING UP ACTIVTY AND SUBJECT

test.labels.activity<-left_join(test.labels,activity_labels, by="V1") #ADDING NAMES TO LABLES
test.labels.activity[,1]<-NULL #REMOOVING THE CODES FOR THE LABLES
test.subj.act<-cbind(test.labels.activity,subject_test) # BINDING THE SUBJ WITH ACTTIVITY

names(test.subj.act)<-c("ACTIVITY","SUBJECT") # rename column to labels activity 
# and subject

test.tidy<-cbind(test.subj.act,test.stats) # voila..


# SUMMARY FOR test DATA ONLY
test2.summary<- test.tidy %>%
        group_by(ACTIVITY,SUBJECT) %>%
        summarise_all(list(~mean(.)))



merge.summary.l<-list(train2.summary,test2.summary)

merge2.summary<-arrange(rbindlist(merge.summary.l, use.names=TRUE),ACTIVITY,SUBJECT)

head(merge2.summary[,1:5],10)

#data.merged<-merge(train,test,all = TRUE)
#col.name<-feature.train$V2
#names(data.merged)<-c(col.name)
#noob
#grep('mean',names(data.merged),value=TRUE)

#test<-data.merged %>% select(grep('mean',col.name,value=TRUE))
#test<-data.merged %>% select(contains('mean',names(data.merged)))
#test<-select(data.merged,matches("mean"))

