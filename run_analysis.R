library(dplyr)

# read the feature names
f <- read.table("features.txt", as.is=TRUE)$V2
# substitute the function call parentheses, as these are not allowed in column names
f <- gsub("\\(", "Of", f)
f <- gsub("\\)", "", f)
# substitute more characters and disambiguate duplicate column names
f <- make.names(f, unique=TRUE)

# read the data and name the columns appropriately
d <- rbind(cbind(read.table("train/X_train.txt", col.names=f),
                 read.table("train/y_train.txt", col.names=c("y")),
                 read.table("train/subject_train.txt", col.names=c("subject"))),
           cbind(read.table("test/X_test.txt", col.names=f),
                 read.table("test/y_test.txt", col.names=c("y")),
                 read.table("test/subject_test.txt", col.names=c("subject"))))

# select just those features that are mean or standard deviation of a measurement
d <- select(d, matches("(\\.meanOf|\\.stdOf)"), y, subject)

# read the activity names
act <- read.table("activity_labels.txt", col.names=c("y", "activity"))

# replace each activity label by the corresponding activity name
d <- left_join(mutate(d, id=as.character(y)) %>% select(-y),
               mutate(act, id=as.character(y)) %>% select(-y), by="id") %>%
    select(-id)

# create an independent data set containing the average of each
# variable for each activity and each subject
d_sum <- group_by(d, subject, activity) %>% summarise_each(funs(mean))

# write out the tidy data set
write.table(d_sum, file="human_activity.txt", row.names=FALSE)
