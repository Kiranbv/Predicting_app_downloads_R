# This code is to build the model for predicting App downloads from 12 different countries

# change working directory(change accordingly)
getwd()
dir_name = "C:\\Users\\kiran\\Desktop\\apptopia"
setwd(dir_name)

# Reading the training and test files
training_data <- read.csv(file = "train.csv",head=TRUE,sep=",")
testing_data <- read.csv(file = "test.csv",head=TRUE,sep=",")


# Data has both numerical and categorical features and should be preprocessed first
dim_tr <- dim(training_data)
number_rows = dim_tr[1]
number_cols = dim_tr[2]
dim_test = dim(testing_data)
number_col_te = dim_test[2]
number_rows_te = dim_test[1]

# neglecting first 2 labels - app id, date as they would not have any effect on downloads. 
training_data_red <- training_data[,3:number_cols]
testing_data_red <- testing_data[,3:number_col_te]
dim_red_tr <- dim(training_data_red)
dim_red_test <- dim(testing_data_red)


# dealing with factors in the training and test data frame
factor_names_train <- names(Filter(is.factor, training_data_red))
factor_names_test <- names(Filter(is.factor, testing_data_red))
num_factors <- length(factor_names_train)

# coding for the categorical variables
# 1. dummy coding and helmert coding

# training data
num_levels <- length(levels(training_data_red[,"country"]))
contrasts(training_data_red$country)<-contr.treatment(num_levels)
#contrasts(training_data_red$country)<-contr.helmert(num_levels)

num_levels <- length(levels(training_data_red[,"rank_kind"]))
contrasts(training_data_red$rank_kind)<-contr.treatment(num_levels)
#contrasts(training_data_red$rank_kind)<-contr.helmert(num_levels)

num_levels <- length(levels(training_data_red[,"age_restrictions"]))
contrasts(training_data_red$age_restrictions)<-contr.treatment(num_levels)
#contrasts(training_data_red$age_restrictions)<-contr.helmert(num_levels)

num_levels <- length(levels(training_data_red[,"total_estimated_installs"]))
contrasts(training_data_red$total_estimated_installs)<-contr.treatment(num_levels)
#contrasts(training_data_red$total_estimated_installs)<-contr.helmert(num_levels)

num_levels <- length(levels(training_data_red[,"operating_system"]))
contrasts(training_data_red$operating_system)<-contr.treatment(num_levels)
#contrasts(training_data_red$operating_system)<-contr.helmert(num_levels)



# testing data
num_levels <- length(levels(testing_data_red[,"country"]))
contrasts(testing_data_red$country)<-contr.treatment(num_levels)
#contrasts(testing_data_red$country)<-contr.helmert(num_levels)

num_levels <- length(levels(testing_data_red[,"rank_kind"]))
contrasts(testing_data_red$rank_kind)<-contr.treatment(num_levels)
#contrasts(testing_data_red$rank_kind)<-contr.helmert(num_levels)

num_levels <- length(levels(testing_data_red[,"age_restrictions"]))
contrasts(testing_data_red$age_restrictions)<-contr.treatment(num_levels)
#contrasts(testing_data_red$age_restrictions)<-contr.helmert(num_levels)

num_levels <- length(levels(testing_data_red[,"total_estimated_installs"]))
contrasts(testing_data_red$total_estimated_installs)<-contr.treatment(num_levels)
#contrasts(testing_data_red$total_estimated_installs)<-contr.helmert(num_levels)

num_levels <- length(levels(testing_data_red[,"operating_system"]))
contrasts(testing_data_red$operating_system)<-contr.treatment(num_levels)
#contrasts(testing_data_red$operating_system)<-contr.helmert(num_levels)


# extracting feature and ouput vectors from training and testing data
#feature_matrix_training <- training_data_red[,1:dim_red_tr[2]-1]
#output_training <- training_data_red[,dim_red_tr[2]]
#feature_matrix_testing = testing_data_red 

# calling the caret package.
library("caret")

# Preprocessing the training data
pre_process_data<-preProcess(training_data_red[,-13], method = c("center", "scale")) 
training_data1<-predict(pre_process_data, training_data_red)
#testing_data1<-predict(pre_process_data, testing_data_red)

# visualizing the training data
featurePlot(x=training_data1[,1:12],y=training_data1[,13],plot="pairs", auto.key=list(columns=3))

# removing na rows in the training data
training_data1[!complete.cases(training_data1),]
training_data2 <- na.omit(training_data1)

# finding important features using recursive feature estimation

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(downloads~., data=training_data2, method="lasso", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
train_mat <- as.matrix(training_data2[,1:12])
resp_mat <- as.matrix(training_data2[,13])
output <- predict(model,testing_data_red)
