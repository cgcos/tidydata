## This scritp have two functions, one to create the matrix and calculate its inverse
## and other to get the cache the inverse or ask the first function to calculate it.
library('plyr');library('dplyr')
## READ the test data
body_acc_test_x <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt", sep="",header=F,na.strings="NA",fill=T)
body_acc_test_y <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt", sep="",header=F,na.strings="NA",fill=T)
body_acc_test_z <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt", sep="",header=F,na.strings="NA",fill=T)

body_gyro_test_x <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt", sep="",header=F,na.strings="NA",fill=T)
body_gyro_test_y <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt", sep="",header=F,na.strings="NA",fill=T)
body_gyro_test_z <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt", sep="",header=F,na.strings="NA",fill=T)

total_acc_test_x <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt", sep="",header=F,na.strings="NA",fill=T)
total_acc_test_y <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt", sep="",header=F,na.strings="NA",fill=T)
total_acc_test_z <- read.table("./coursera/project/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt", sep="",header=F,na.strings="NA",fill=T)

# Calculate mean and standard deviation for the test

df_test <- data.frame(mean_body_acc_x=rowMeans(body_acc_test_x),mean_body_acc_y=rowMeans(body_acc_test_y),mean_body_acc_z=rowMeans(body_acc_test_z),
                      mean_gyro_x=rowMeans(body_gyro_test_x),mean_gyro_y=rowMeans(body_gyro_test_y),mean_gyro_z=rowMeans(body_gyro_test_z),
                      mean_total_acc_x=rowMeans(total_acc_test_x),mean_total_acc_y=rowMeans(total_acc_test_y),mean_total_acc_z=rowMeans(total_acc_test_z),
                      sd_body_acc_x=apply(body_acc_test_x,1,sd,na.rm=F),sd_body_acc_y=apply(body_acc_test_y,1,sd,na.rm=F),sd_body_acc_z=apply(body_acc_test_z,1,sd,na.rm=F),
                      sd_gyro_x=apply(body_gyro_test_x,1,sd,na.rm=F),sd_gyro_y=apply(body_gyro_test_y,1,sd,na.rm=F),sd_gyro_z=apply(body_gyro_test_z,1,sd,na.rm=F),
                      sd_total_acc_x=apply(total_acc_test_x,1,sd,na.rm=F),sd_total_acc_y=apply(total_acc_test_y,1,sd,na.rm=F),sd_total_acc_z=apply(total_acc_test_z,1,sd,na.rm=F))
# Load Subject
s <- read.table('./coursera/project/UCI HAR Dataset/test/subject_test.txt', sep="",header=F,na.strings="NA",fill=T)
# Load activity
a <- read.table('./coursera/project/UCI HAR Dataset/test/y_test.txt', sep="",header=F,na.strings="NA",fill=T)
# Create data frame and merge with the former df_test
sujeto_test <- data.frame(s,a)
df_test <- data.frame(s,a,df_test)
names(df_test)[names(df_test == 'V1')] <- 'subject'
names(df_test)[names(df_test == 'V2')] <- 'activity'
## READ the train data
body_acc_train_x <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_acc_x_train.txt", sep="",header=F,na.strings="NA",fill=T)
body_acc_train_y <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_acc_y_train.txt", sep="",header=F,na.strings="NA",fill=T)
body_acc_train_z <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_acc_z_train.txt", sep="",header=F,na.strings="NA",fill=T)

body_gyro_train_x <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt", sep="",header=F,na.strings="NA",fill=T)
body_gyro_train_y <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt", sep="",header=F,na.strings="NA",fill=T)
body_gyro_train_z <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt", sep="",header=F,na.strings="NA",fill=T)

total_acc_train_x <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_gyro_x_train.txt", sep="",header=F,na.strings="NA",fill=T)
total_acc_train_y <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_gyro_y_train.txt", sep="",header=F,na.strings="NA",fill=T)
total_acc_train_z <- read.table("./coursera/project/UCI HAR Dataset/train/Inertial Signals/body_gyro_z_train.txt", sep="",header=F,na.strings="NA",fill=T)

# Calculate mean and standard deviation for the train

df_train <- data.frame(mean_body_acc_x=rowMeans(body_acc_train_x),mean_body_acc_y=rowMeans(body_acc_train_y),mean_body_acc_z=rowMeans(body_acc_train_z),
                       mean_gyro_x=rowMeans(body_gyro_train_x),mean_gyro_y=rowMeans(body_gyro_train_y),mean_gyro_z=rowMeans(body_gyro_train_z),
                       mean_total_acc_x=rowMeans(total_acc_train_x),mean_total_acc_y=rowMeans(total_acc_train_y),mean_total_acc_z=rowMeans(total_acc_train_z),
                       sd_body_acc_x=apply(body_acc_train_x,1,sd,na.rm=F),sd_body_acc_y=apply(body_acc_train_y,1,sd,na.rm=F),sd_body_acc_z=apply(body_acc_train_z,1,sd,na.rm=F),
                       sd_gyro_x=apply(body_gyro_train_x,1,sd,na.rm=F),sd_gyro_y=apply(body_gyro_train_y,1,sd,na.rm=F),sd_gyro_z=apply(body_gyro_train_z,1,sd,na.rm=F),
                       sd_total_acc_x=apply(total_acc_train_x,1,sd,na.rm=F),sd_total_acc_y=apply(total_acc_train_y,1,sd,na.rm=F),sd_total_acc_z=apply(total_acc_train_z,1,sd,na.rm=F))

# Load Subject
s <- read.table('./coursera/project/UCI HAR Dataset/train/subject_train.txt', sep="",header=F,na.strings="NA",fill=T)
# Load activity
a <- read.table('./coursera/project/UCI HAR Dataset/train/y_train.txt', sep="",header=F,na.strings="NA",fill=T)
# Create data frame and merge with the former df_train
sujeto_train <- data.frame(s,a)
df_train <- data.frame(s,a,df_train)
names(df_train)[names(df_train == 'V1')] <- 'subject'
names(df_train)[names(df_train == 'V2')] <- 'activity'

# merge the two data frames
df <- rbind(df_train,df_test)
names(df)[1] <- "subject"
names(df)[2] <- "activity"

# read activity labes
act_labels <- read.table('./coursera/project/UCI HAR Dataset/activity_labels.txt',sep="")
data <- merge(df,act_labels,by.x="activity",by.y="V1",all=T)
data_end <- select(data,subject:V2)
names(data_end)[20] <- "activity"
end <- ddply(data_end, c("subject", "activity"), summarise,
             body_acc_x = mean(mean_body_acc_x),
             body_acc_y = mean(mean_body_acc_y),
             body_acc_z = mean(mean_body_acc_z),
             gyro_x = mean(mean_gyro_x),
             gyro_y = mean(mean_gyro_y),
             gyro_z = mean(mean_gyro_z),
             total_acc_x = mean(mean_total_acc_x),
             total_acc_y = mean(mean_total_acc_y),
             total_acc_z = mean(mean_total_acc_z),
             sd_body_x = mean(sd_body_acc_x),
             sd_body_y = mean(sd_body_acc_y),
             sd_body_z = mean(sd_body_acc_z),
             sd_gyrox = mean(sd_gyro_x),
             sd_gyroy = mean(sd_gyro_y),
             sd_gyroz = mean(sd_gyro_z),
             sd_total_x = mean(sd_total_acc_x),
             sd_total_y = mean(sd_total_acc_y),
             sd_total_z = mean(sd_total_acc_z)
)

write.table(end,file='./coursera/project/UCI HAR Dataset/data.txt',row.names=F)
