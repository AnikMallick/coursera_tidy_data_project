library(dplyr)
library(tidyr)
library(reshape2)

###4.Appropriately labels the data set with descriptive variable names.
clean_variable_names <- function(x){
    x <- gsub("^t","time",x = x)
    x <- gsub("^f","frequency",x = x)
    x <- gsub("\\(\\)","",x = x)
    x <- gsub("Acc","Accelerometer",x = x)
    x <- gsub("Gyro","Gyro-sensor",x = x)
    x
}

#root path of the data
data_path <- "UCI HAR Dataset"
train_path <- file.path(data_path,"train")
test_path <- file.path(data_path,"test")

#reading the data
df_train_x <- read.table(file.path(train_path,"X_train.txt"),header = FALSE)
df_train_y <- read.table(file.path(train_path,"y_train.txt"),
                         col.names = "activity",header = FALSE)
df_train_sub <- read.table(file.path(train_path,"subject_train.txt"),
                           col.names = "subject",header = FALSE)

df_test_x <- read.table(file.path(test_path,"X_test.txt"),header = FALSE)
df_test_y <- read.table(file.path(test_path,"y_test.txt"),
                        col.names = "activity",header = FALSE)
df_test_sub <- read.table(file.path(test_path,"subject_test.txt"),
                           col.names = "subject",header = FALSE)

###1. Merges the training and the test sets to create one data set.
df <- rbind(cbind(df_train_x,df_train_y,df_train_sub),
            cbind(df_test_x,df_test_y,df_test_sub))

#removing
remove_df <- c("df_train_x","df_train_y","df_train_sub",
               "df_test_x","df_test_y","df_test_sub")
for(d in remove_df) rm(d)

#create backup of df
df_backup <- df

variable_names <- read.table(file.path(data_path,"features.txt"))[,2]

###2.Extracts only the measurements on the mean and 
###standard deviation for each measurement.
cols_with_mean <- which(grepl("mean",variable_names))
cols_name_mean <- variable_names[grepl("mean",variable_names)]
cols_name_mean <- clean_variable_names(cols_name_mean)###4.Appropriately labels the data set with descriptive variable names.

cols_with_std <- which(grepl("std",variable_names))
cols_name_std <- variable_names[grepl("std",variable_names)]
cols_name_std <- clean_variable_names(cols_name_std)###4.Appropriately labels the data set with descriptive variable names.

df_mean <- df[,cols_with_mean]
names(df_mean) <- cols_name_mean
df_std <- df[,cols_with_std]
names(df_std) <- cols_name_std

df_new <- cbind(df_mean,df_std,df[,c("subject","activity")])


###3.Uses descriptive activity names to name the activities in the data set
activities <- read.table(file.path(data_path,"activity_labels.txt"),
                         col.names = c("cat","activity"),header = FALSE)

fun <- function(x) activities[activities$cat == x,]$activity
df_new$activity <- sapply(df_new$activity,fun)

df_backup_new <- df_new
rm("df")
###5.From the data set in step 4, creates a second, independent tidy data set 
###with the average of each variable for each activity and each subject.
df_new <- tbl_df(df_new)

#indes 80 and 81 is subject and activity
df_new %>% 
    melt(id=c("activity","subject"),
         measure.vars =names(df_new)[-c(80,81)]) %>%
    group_by(activity,subject) %>%
    dcast(activity + subject~variable,mean) %>% tbl_df -> df_result

#final result is 
df_result
###saving the file
write.table(df_result, file = "result.txt",row.name=FALSE)
















