README
================

Getting and Cleaning Data Course Project on Coursera
----------------------------------------------------

This is my code for Getting and Cleaning Data Course Project The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(RCurl)
```

    ## Warning: package 'RCurl' was built under R version 3.5.3

    ## Loading required package: bitops

``` r
library(jpeg)
library(data.table)
```

    ## Warning: package 'data.table' was built under R version 3.5.3

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
feature<-fread("./data/features.txt") 
activity_labels<-fread("./data/activity_labels.txt") 
subject_train<-fread("./data/subject_train.txt") 
train.labels<-fread("./data/y_train.txt") 
train<-fread("./data/X_train.txt") 

col.name<-feature$V2 #
names(train)<-c(col.name) 


index<-which(duplicated(names(train)))
index.mat<-as.matrix(index)
train[,index.mat]<-NULL  
train.stats<-select(train,contains("mean"),contains("std"))


train.labels.activity<-left_join(train.labels,activity_labels, by="V1")
train.labels.activity[,1]<-NULL 
train.subj.act<-cbind(train.labels.activity,subject_train)

names(train.subj.act)<-c("ACTIVITY","SUBJECT")

train.tidy<-cbind(train.subj.act,train.stats)


train2.summary<- train.tidy %>%
        group_by(ACTIVITY,SUBJECT) %>%
        summarise_all(list(~mean(.)))



subject_test<-fread("./data/subject_test.txt")
test.labels<-fread("./data/y_test.txt") 
test<-fread("./data/X_test.txt") 

col.name<-feature$V2 
names(test)<-c(col.name) 

index<-which(duplicated(names(test)))
index.mat<-as.matrix(index)

test[,index.mat]<-NULL  
test.stats<-select(test,contains("mean"),contains("std"))


test.labels.activity<-left_join(test.labels,activity_labels, by="V1") 
test.labels.activity[,1]<-NULL 
test.subj.act<-cbind(test.labels.activity,subject_test) 

names(test.subj.act)<-c("ACTIVITY","SUBJECT") 


test.tidy<-cbind(test.subj.act,test.stats) 



test2.summary<- test.tidy %>%
        group_by(ACTIVITY,SUBJECT) %>%
        summarise_all(list(~mean(.)))



merge.summary.l<-list(train2.summary,test2.summary)

merge2.summary<-arrange(rbindlist(merge.summary.l, use.names=TRUE),ACTIVITY,SUBJECT)

## Display only 5 collumns
dim(merge2.summary)
```

    ## [1] 180  88

``` r
head(merge2.summary[,1:5],40)
```

    ##    ACTIVITY SUBJECT tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z
    ## 1    LAYING       1         0.2215982      -0.040513953       -0.11320355
    ## 2    LAYING       2         0.2813734      -0.018158740       -0.10724561
    ## 3    LAYING       3         0.2755169      -0.018955679       -0.10130048
    ## 4    LAYING       4         0.2635592      -0.015003184       -0.11068815
    ## 5    LAYING       5         0.2783343      -0.018304212       -0.10793760
    ## 6    LAYING       6         0.2486565      -0.010252917       -0.13311957
    ## 7    LAYING       7         0.2501767      -0.020441152       -0.10136104
    ## 8    LAYING       8         0.2612543      -0.021228173       -0.10224537
    ## 9    LAYING       9         0.2591955      -0.020526822       -0.10754972
    ## 10   LAYING      10         0.2802306      -0.024294484       -0.11716864
    ## 11   LAYING      11         0.2805930      -0.017659805       -0.10878658
    ## 12   LAYING      12         0.2601134      -0.017520392       -0.10816013
    ## 13   LAYING      13         0.2767164      -0.020440454       -0.10433186
    ## 14   LAYING      14         0.2332754      -0.011342465       -0.08683333
    ## 15   LAYING      15         0.2894757      -0.016629654       -0.11853024
    ## 16   LAYING      16         0.2742272      -0.016610351       -0.10731049
    ## 17   LAYING      17         0.2697801      -0.016846201       -0.10700628
    ## 18   LAYING      18         0.2746916      -0.017393768       -0.10769893
    ## 19   LAYING      19         0.2726537      -0.017142686       -0.10898146
    ## 20   LAYING      20         0.2395079      -0.014440628       -0.10427432
    ## 21   LAYING      21         0.2713255      -0.018423305       -0.10325383
    ## 22   LAYING      22         0.2799597      -0.014262986       -0.11080092
    ## 23   LAYING      23         0.2740380      -0.021655384       -0.10425678
    ## 24   LAYING      24         0.2728505      -0.017355521       -0.10723624
    ## 25   LAYING      25         0.2507918      -0.018894366       -0.10042883
    ## 26   LAYING      26         0.2716459      -0.019189573       -0.10500254
    ## 27   LAYING      27         0.2741025      -0.017986761       -0.10769973
    ## 28   LAYING      28         0.2759135      -0.016753786       -0.10834485
    ## 29   LAYING      29         0.2872952      -0.017196548       -0.10946207
    ## 30   LAYING      30         0.2810339      -0.019449410       -0.10365815
    ## 31  SITTING       1         0.2612376      -0.001308288       -0.10454418
    ## 32  SITTING       2         0.2770874      -0.015687994       -0.10921827
    ## 33  SITTING       3         0.2571976      -0.003502998       -0.09835792
    ## 34  SITTING       4         0.2715383      -0.007163065       -0.10587460
    ## 35  SITTING       5         0.2736941      -0.009900835       -0.10854030
    ## 36  SITTING       6         0.2767785      -0.014591162       -0.11012773
    ## 37  SITTING       7         0.2846746      -0.014610976       -0.12246460
    ## 38  SITTING       8         0.2674915      -0.006725506       -0.10446105
    ## 39  SITTING       9         0.2483267      -0.027016777       -0.07537847
    ## 40  SITTING      10         0.2706121      -0.015042682       -0.10425324

Only displaying the firt 5 collumns since 88 collumns available
---------------------------------------------------------------
