#########################################################################################
###################### MARKETING CAMPAIGN EFFECTIVENESS #################################
#########################################################################################

####################################################################
##### Sec.1 Installing releavnt libraries and importing data #######


# 1.1. Installing the libraries

# Data import and transformation
library(stringr) # String manupulation
library(data.table)
library(dplyr)
library(DataExplorer) 

# Data visualization
library(cowplot) # Combine multiple chart together 
library(ggpubr) # For plotting
library(ggplot2)

# Feature selection
library(corrplot)# Variable correlation
library(Boruta) # Feature selection
library(caret) # Feature selection & sampling

# Machine learning algorithm
library("kernlab") 
library(mlr)

# 1.2. Importing the data
setwd("C:/Users/sm505388/Desktop/Work_Subrata/ML_Office/GL_DT")
Loan_data <- fread(file = "bank-full.csv", sep = ",", stringsAsFactors = TRUE, 
                   header = TRUE, na.strings = c("", "na", "NA", "Null", "NULL", "null"))
Loan_data <- as.data.frame(Loan_data)
str(Loan_data)

# 1.3. Creating a fucntion to check the NA's count for each features
naColumns <- function(i) {
  colnames(i)[unlist(lapply(i, function(i) any(is.na(i))))]
}
naColumns(Loan_data) # There is no NA's

#######################################################################################
############## Sec 2. Exploratory data Analysis (EDA) #################################

plot_bar(Loan_data)

#2.1 Visual Exploration of the data (Bar plot)
plot_bar(Loan_data)
barplot(table(Loan_data$education),main = "Education distribution", col="green",
        ylab="No. of Clients", las=2,cex.names = 0.8,cex.axis = 0.8) 
barplot(table(Loan_data$job),main = "Job distribution", col="red",
        ylab="No. of Clients", las=2,cex.names = 0.8,cex.axis = 0.8) 
barplot(table(Loan_data$marital),main = "Distribution of Marital status", col="Blue",
        ylab="No. of Clients", las=2,cex.names = 0.8,cex.axis = 0.8) 
barplot(table(Loan_data$month),main = "Month wise distribution", col="yellow",
        ylab="No. of Clients", las=2,cex.names = 0.8,cex.axis = 0.8) 

#**The data distribution is not same accross subscription category i.e. Target column
#**Majorly customers completed Secondary education
#**Customer are majorly from Blue-collar job, followed by Management and technician

#2.2 Visual Exploration of the data (Box Plot)
# Distribution of Balance accross job, education & marital status
p <- ggplot(Loan_data, aes(x =job, y = balance)) + 
  geom_boxplot(aes(fill = Target), position = position_dodge(0.5)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
Pic <- p + facet_grid(cols = vars(Target))

p1 <- ggplot(Loan_data, aes(x =education, y = balance)) + 
  geom_boxplot(aes(fill = Target), position = position_dodge(0.5)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
Pic1 <-p1 + facet_grid(cols = vars(Target))

p2 <- ggplot(Loan_data, aes(x =marital, y = balance)) + 
  geom_boxplot(aes(fill = Target), position = position_dodge(0.5)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
Pic2 <-p2 + facet_grid(cols = vars(Target))

plot_grid(Pic,Pic1,Pic2, labels = c("Job vs Balance", "Education vs balance",
                                    "Marital status vs Balance "))
# Distribution of call duration job, education & marital status
p <- ggplot(Loan_data, aes(x =job, y = duration)) + 
  geom_boxplot(aes(fill = Target), position = position_dodge(0.5)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
Pic <- p + facet_grid(cols = vars(Target))

p1 <- ggplot(Loan_data, aes(x =education, y = duration)) + 
  geom_boxplot(aes(fill = Target), position = position_dodge(0.5)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
Pic1 <-p1 + facet_grid(cols = vars(Target))

p2 <- ggplot(Loan_data, aes(x =marital, y = duration)) + 
  geom_boxplot(aes(fill = Target), position = position_dodge(0.5)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
Pic2 <-p2 + facet_grid(cols = vars(Target))

plot_grid(Pic,Pic1,Pic2, labels = c("Job vs duration", "Education vs duration",
                                    "Marital status vs duration"))

#a. There is no significant difference in balance distribution for both the classes of Target column 
# same accross Job, education and Marital status.
#b. Significant difference is present for call duration accross all levels of job, education and marital status
#c.High correlation is present among some of the features like contact, poutcome. We will drop some of those variables

#######################################################################################
############## Sec 3. Data cleaning and feature engineering ###########################

# 3.1) Creating a separate columns for variables job, education, contact & poutcome (Unknown category)
Loan_data$job_unknown <- ifelse(Loan_data$job == "unknown",1,0)
Loan_data$Education_unknown <- ifelse(Loan_data$education == "unknown",1,0)
Loan_data$contact_unknown <- ifelse(Loan_data$contact == "unknown",1,0)
Loan_data$poutcomet_unknown <- ifelse(Loan_data$poutcome == "unknown",1,0)
loan_datacopy<-Loan_data

# 3.2) Converting factor variables to numeric

sapply(Loan_data, class) # checking the class of each variables
category_col = Loan_data %>% select_if(is.factor) %>% colnames()
category_col
Loan_data[category_col] <- lapply(Loan_data[category_col], as.numeric)

# 3.3) Converting values to 0 & 1(from 1 & 2) for default & Target column (point of interest)

Loan_data$default <- ifelse(Loan_data$default ==2,1,0)
Loan_data$housing <- ifelse(Loan_data$housing== 2,1,0)
Loan_data$loan<- ifelse(Loan_data$loan== 2,1,0)
Loan_data$Target <- ifelse(Loan_data$Target == 2,1,0)

# 3.4) Transforming all the dataset into same scale (max-min transformation)

##--------------------------------------------------------------------------------------------
# Loan_data_1$balance <- scale(Loan_data_1$balance) -- For single column transformation
#ztran <- function(x, na.rm = TRUE) {
  #mns <- colMeans(x, na.rm = na.rm)
  #sds <- apply(x, 2, sd, na.rm = na.rm)
  #x <- sweep(x, 2, mns, "-")
  #x <- sweep(x, 2, sds, "/")
  #x
#}
#Loan_data_1 <- ztran(Loan_data)

#** Since value in target column also transforming , we are not using Z transformation. Instead of it 
# we will be using z transformation.
##--------------------------------------------------------------------------------------------

# Max-min transformation 
Mmin_trans <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
Loan_data1 <- as.data.frame(lapply(Loan_data, Mmin_trans))

#######################################################################################
############## Sec 4. Variables selection ############################################

# 4.1 Correlation Analysis 
az <- split(names(Loan_data1), sapply(Loan_data1, function(x){class(x)}))
Loan_date_numeric <- Loan_data1[az$numeric]

corr <- cor(Loan_date_numeric)
corrplot(corr , method = "circle")

#** Some of the variables should drop due to high correlation ( Positive and Negative)

# 4.2 Feature selection
# Performing Boruta search
boruta_features <- Boruta(Target ~., data = na.omit(Loan_data1), doTrace =1)
plot(boruta_features, cex.axis=.7, las=2, xlab="", main="Importance of variables") # Check tentative variables get selected for granted or not.
TentativeFix <- TentativeRoughFix(boruta_features)
important_att <- attStats(TentativeFix)
Impt_att = important_att[important_att$decision != 'Rejected', c('meanImp', 'decision')]
head(Impt_att[order(-Impt_att$meanImp), ])

#**Most important features are duration, poutcome, month, housing, age, day

# ** We will drop some of the less sigficant variables like Pdays, Previous, Contact_unknown, Poutcome_unknown, 
# Education_unknown, job_unknown due very low explanatory capability and presence of high correlation

# 4.3 keeping only the important variables
Final_data <- Loan_data1[,c("age","job","marital","education", "default","balance","housing",
                            "loan","contact","day","month", "duration","campaign", "poutcome",
                            "Target")]
dim(Final_data)


#####################################################################################
############# Sec 5. Spliting the dataset into training and test dataset ############

# Spliting the dataset into Training and test dataset (Using stratified classification technique)
set.seed(123)
split_index <- createDataPartition(Final_data$Target, p = .7, list = FALSE)
train_data <- Final_data[split_index,]
test_data <- Final_data[-split_index,]
dim(train_data)
dim(test_data)

####################################################################################
############# Sec 6. Model Development #############################################

# 6.1 Summary of the dataset
summarizeColumns(train_data)
summarizeColumns(test_data)

train_data$Target <- as.factor(train_data$Target) # Converting the levels of target variables to factor
test_data$Target <- as.factor(test_data$Target)

# 6.2 Task creation (Dataset on which learners learn)
traintask <-makeClassifTask(data=train_data,target = "Target", positive = 1)
testtask <-makeClassifTask(data=test_data,target = "Target")
str(getTaskData(traintask))

# 6.3 We will use different algorithms and assess the model accuracy
# a. QDA (Quadratic Discriminant Analysis)
# b. Logistic regression
# c. Decision tree
# d. Random Forest
# e. Support vector machine
# f. K nearest neighbor (KNN)

# ---------------------------------------------------------------------------------
# a. QDA (Quadratic Discriminant Analysis)
# ---------------------------------------------------------------------------------
qda_learner <- makeLearner("classif.qda", predict.type = "response") # setting up the model parameter
qda_model <- train(qda_learner, traintask) # Model training with test data
qdr_predict<-predict(qda_model, testtask)# Predicting the outcome 

# Assessing the model accuracy (Overall and class level)
model_pred <- qdr_predict$data$response
qda_model_performance <- confusionMatrix(test_data$Target, model_pred)
qda_model_performance

# Overall accuracy is good (86%), but if look into class level accuracy, accuracy of class 1 (point of interest)
# is not upto mark. let's try out some other model.

#--------------------------------------------------------------------------------
# b. Logistic regression
#--------------------------------------------------------------------------------
lr_learner <- makeLearner("classif.logreg", predict.type = "response")
lr_model <- train(lr_learner, traintask) # training the model
predict_log <- predict(lr_model, testtask) # predicting the the Target

model_pred <- predict_log$data$response
lr_model_performance <- confusionMatrix(test_data$Target, model_pred)
lr_model_performance

# No significant improvement in model performance using LR algorithm. 
# ------------------------------------------------------------------------------ 
# c. Decision tree
# ------------------------------------------------------------------------------
Dtree <- makeLearner("classif.rpart", predict.type = "response")
set_cv <-makeResampleDesc("CV", iters=5L) # 5 fold cross validation.

# Setting up the tunable parameters
getParamSet("classif.rpart")
tune_param <-makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50), # minimum number of observations required in a node to split 
  makeIntegerParam("minbucket", lower = 5, upper = 50), # minimum number of observations in the terminal nodes (leaf)
  makeNumericParam("cp", lower = 0.001, upper = 0.2) # cp ~ complexity parameter.lesser the value more chances of overfitting.
)
# Setting up control parameter(Search/control parameter)
control <- makeTuneControlRandom(maxit = 100) # 100 iteration
# Parameters hypertunning
hyptune <- tuneParams(learner = Dtree, resampling = set_cv, par.set = tune_param,
                      task = traintask, control = control, measures = acc)
# Best hyper parameter and cross validation accuracy
hyptune$x
hyptune$y

# Creating Best hyper parameter object
Besthyperparams <- setHyperPars(Dtree, par.vals =hyptune$x )

# train the model using hyperparameters
Dtree_train <- train(Besthyperparams, traintask)
Dpred <- predict(Dtree_train, testtask)

DT_model_pred <- Dpred$data$response
DT_model_accurcy <- confusionMatrix(test_data$Target, DT_model_pred)
DT_model_accurcy

# * Both class level (58.8%) and overall accuracy (89.6%) is much higher compared to previous two models.

# ------------------------------------------------------------------------------ 
# d. Random Forest
# ------------------------------------------------------------------------------
rf <- makeLearner("classif.randomForest", predict.type = "response",
                  par.vals = list(ntree=200, mtry=3)) # model initiation with 200 tree

# Setting up tunable parameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree", lower = 50, upper = 500),
  makeIntegerParam("mtry", lower =3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
# Setting up control parameter(Random search)
rf_search <- makeTuneControlRandom(maxit = 100L)

# Setting up cross validation/resampling (5 folds cross validation)
rf_cv <- makeResampleDesc("CV", iters=5L)

# Hypertuning of the model
hypertune_rf <- tuneParams(learner = rf, task = traintask, par.set = rf_param, 
                           resampling = rf_cv, control = rf_search, measures = acc)

# Best hyper parameter and cross validation accuracy
hypertune_rf$x
hypertune_rf$y

# Train and predict the Target using best hyper parameter

rf_hyperparam <- setHyperPars(rf, par.vals = hypertune_rf$x)
rf_model <- train(rf_hyperparam, traintask)
rfmodel <- predict(rf_model, testtask)

# Model performance
rf_model_pred <- rfmodel$data$response
rf_model_acc <- confusionMatrix(test_data$Target,rf_model_pred )
rf_model_acc

# Both overall (90.2%) and class level accuracy (60.5%) has improved and best in all models
# ------------------------------------------------------------------------------ 
# d. Support vector machine
# ------------------------------------------------------------------------------
svm <- makeLearner("classif.ksvm", predict.type = "response") # model initiation
# Setting up the tunable parameter
svm_param <- makeParamSet(
  makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), # Cost parameter
  makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4))) # RFB (radial basis kernel function)

# Setting up sampling parameter
svm_control <- makeResampleDesc("CV", iters=5L) # 5 fold cross validation

# Setting up the control parameter
svm_search <- makeTuneControlRandom(maxit = 100)

# Tune the model using all the parameters
hyptune <- tuneParams(learner = svm, task = traintask, par.set = svm_param, 
                      resampling = svm_control, control = svm_search, measures = acc)
hyptune$x
hyptune$y # Cross validation accuracy

# Setting best hyper parameters for the model
svm_model <- setHyperPars(svm, par.vals = hyptune$x)
# Train the model with best parameter and predict the Target

svm_train <- train(svm_model, traintask) 
svm_prediction <- predict(svm_train, testtask) # Predicted with test dataset

# Model performance
svm_model_pred <- svm_prediction$data$response
svm_model_acc <- confusionMatrix(test_data$Target, svm_model_pred)
svm_model_acc

# Overall accuracy (89.63%) is good and class level accuracy for our point of interest is highest (64%)
# ------------------------------------------------------------------------------ 
# e. K nearest neighbor (KNN)
# ------------------------------------------------------------------------------
KNN <- makeLearner ("classif.knn", predict.type = "response") # Model initiation
getParamSet("classif.knn")
# Setting up model hyperparameter
KNN_param <- makeParamSet (
  makeIntegerLearnerParam("k", lower = 1L, upper = 7L), # number of neighbors
  makeNumericLearnerParam("l", default = 0L, lower = 0L, upper = 7) # minimum vote for decision
  )

# Setting up sampling parameter
KNN_control <- makeResampleDesc("CV", iters = 5L)

# Setting up control parameter
KNN_search <- makeTuneControlRandom (maxit =100)

# Hypertunning the model

hyptune <- tuneParams(learner = KNN,task = traintask,par.set = KNN_param, 
                      resampling = KNN_control, control = KNN_search, measures = acc)
hyptune$x
hyptune$y 

# Train the model and predicting the Target
KNN_model <- train(KNN, traintask)
KNN_predict <- predict(KNN_model, testtask)

# Model performance
KNN_model_pred <- KNN_predict$data$response
KNN_model_acc <- confusionMatrix (test_data$Target, KNN_model_pred)

KNN_model_acc

# Overall accuracy is decent(86.9%) but class level accuracy (point of interest) is low 42.4%

####################################################################################
#################### Sec 7. Conclusion #############################################

# a. Out of all the model, the performance of Random Forest (RF) and Support vector machine (SVM) is Highest.
# b. We can go for SVM, since the class level accuracy of our point of interest is highest. But before making 
# any conclusion we need check perfromce consistency on out of sample data
# c. Can be explored other techniques like boosting algorithm and check out the model performance.


