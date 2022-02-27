install.packages("dplyr")
install.packages("Metrics")
install.packages("ISLR")
install.packages("randomForest")
install.packages("glm.predict")
install.packages("party")
install.packages("caTools")
install.packages("ROCR")  
install.packages(c("lightgbm", "MLmetrics"))
install.packages("naivebayes")
install.packages("ggfortify")
install.packages("glmnet()")
install.packages("xgboost")
install.packages("party")
install.packages(c("FSelector", "rpart.plot", "xlsx", "data.tree"))
install.packages("carnet",dependencies =TRUE)
install.packages("class")
install.packages("caret")
install.packages("corrplot")
library(corrplot)#visualizing a correlation matrix
library(caret)#
library(party)    #used to create and analyze decision tree
library(glmnet)#fits generalized linear and similar models via penalized maximum likelihood
library(naivebayes)#provides an efficient implementation of the popular Naive Bayes classifier
library(psych)#general purpose toolbox for personality, psychometric theory and experimental psychology
library(fortunes)#a collection of fortunes from the R community
library(dummies)#expands factors, characters and other eligible classes into dummy/indicator variables
library(caret)#carrying out classification and regression tasks
library(caTools)#several basic utility functions
library(ROCR)#evaluating and visualizing the performance of scoring classifiers
library(Metrics)#metrics for regression, time series, binary classification, classification, and information retrieval problems
library(randomForest)#classification and regression based on a forest of trees using random inputs
library(dplyr)#transform and summarize tabular data with rows and columns
library(data.table)#for fast aggregation of large datasets, low latency add/update/remove of columns, quicker ordered joins, and a fast file reader
library(Matrix)#functions to support highly dense or sparse matrices
library(MLmetrics)#a collection of evaluation metrics, including loss, score and utility functions, that measure regression, classification and ranking performance
library(e1071)#functions for latent class analysis, short time Fourier transform, fuzzy clustering, support vector machines, shortest path
library(tidyverse)#data import, tidying, manipulation, visualisation, and programming
library(ggplot2)#system for declaratively creating graphics, based on The Grammar of Graphics
library(caretEnsemble)#used to create ensemble models from such lists of caret models
library(Amelia)#for the multiple imputation of multivariate incomplete data
library(mice)#creating multiple imputations as compared to a single imputation (such as mean) takes care of uncertainty in missing values
library(GGally)#adding several functions to reduce the complexity of combining geometric objects with transformed data
library(rpart)#for building classification and regression trees
library(klaR)#miscellaneous functions for classification and visualization
library(ggfortify)#data Visualization Tools for Statistical Analysis Results=
library(xgboost)# for extreme Gradient Boosting
library(magrittr)#a mechanism for chaining commands with a new forward-pipe operator
library(FSelector)#provides functions for selecting attributes from a given dataset
library(rpart.plot)#plot 'rpart' models
library(xlsx)#read/write/format Excel 2007 and Excel 97/2000/XP/2003 file formats
library(data.tree)#create tree structures from hierarchical data, and traverse the tree in various orders
library(class)#various functions for classification, including k-nearest neighbour
library(pROC)#tools for visualizing, smoothing and comparing receiver operating characteristic(ROC curves).
library(mlbench)#a collection of artificial and real-world machine learning benchmark problems
library(CRANsearcher)#Addin for searching packages in CRAN database based on keywords
library(readr)#provide a fast and friendly way to read rectangular data 
d <- read_csv("data.csv")#import data set
View(d)
head(d)
summary(d)
length(unique(d[["id"]]))#delete the duplicate values or the rows present in dataframe
length(unique(d[["spkid"]]))
length(unique(d[["pdes"]]))
length(unique(d[["full_name"]]))
table(d$pha)#categorical tabulation of data with the variable
table(d$neo)
unique(d[["class"]])
length(unique(d[["class"]]))
d=subset.data.frame(d,select=-c(id,pdes,name,prefix,equinox))#removing useless columns
dimnames(d)#set the dimnames of an object
unique(d)
d=subset.data.frame(d,select=-c(diameter,albedo,diameter_sigma))#removing useless columns
da=data.frame(d)#convert d to data frame
d[!complete.cases(d), ]#mentions which have no missing values in the dataset
na.omit(d)#omit all unnecessary cases from data
sd=subset.data.frame(d,select=-c(spkid,full_name, neo, pha, orbit_id, class))#removing useless columns
nrow(sd)
ncol(sd)
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
} #normalizing the values of data
as.data.frame(lapply(sd[,31], normalize))#applies normalize function to all data values


preprocessParams <- preProcess(sd[,7:31],method=c("range"))#Pre-processing transformation
print(preprocessParams)

transformed <- predict(preprocessParams,sd[,7:31])

summary(transformed)


dummy.data.frame(d)#creates dummy data frame

#transformed<-cbind(dummy.data.frame(da, names=c("class"), sep="_"))
transformed<-cbind(dummy.data.frame(da, names=c("neo"), sep="_"))#splits the column neo into 2 types based on the value
#transformed<-cbind(dummy.data.frame(da, names=c("orbit_id"), sep="_"))




#ya<- revalue(da$pha, c("Y"=1,"N"=0))

#transformed=subset.data.frame(transformed,select=-c(spkid, full_name, neo))
xtabs(~orbit_id+neo,data=transformed)
transformed$pha=ifelse(transformed$neo_Y==1,1,0)#convert 'Y' and 'N' to binary form
transformed=subset.data.frame(transformed,select=-c(neo_N,neo_Y))
#transformed$pha<- map(transformed$pha, c("Y"=1,"N"=0))
set.seed(3456)
trainIndex <- createDataPartition(transformed$pha, p = .7,
                                  list = FALSE,
                                  times = 1)#dividing the data into train and test of 70% and 30% respectively based on pha
x_train <- transformed[ (trainIndex),]
x_test <- transformed[(-trainIndex),]
y_train<-data.frame(x_train$pha)
y_test<-data.frame(x_train$pha)
#x_train$neo<-NULL
#x_test$neo<=NULL



x_dumm<-data.frame(x_train)
x_dumm<- subset(x_dumm,select=-c(full_name,spkid,x_train.pha))
x_train<- subset(x_train,select=-c(full_name,spkid))
x_test<- subset(x_test,select=-c(full_name,spkid))
xt=data.frame(x_test)
#colnames(x_dumm)[32] <- "rms"#rename the column
#colnames(x_train)[32] <- "rms"
#colnames(x_test)[32] <- "rms"
#colnames(xt1)[32] <- "rms"
x_dumm<-x_dumm %>% relocate(H:rms, .before =pha)#relocating the column pha to last
x_train<-x_train %>% relocate(H:rms, .before =pha)
x_test<-x_test %>% relocate(H:rms, .before =pha)
xt<-xt %>% relocate(H:rms, .before =pha)


#1.Logistic Regression
output.lr <- glm(pha ~. ,data =x_dumm, family = "binomial")
summary(output.lr) 

res.lr<-predict(output.lr,x_dumm,type="response")
res.lr
res.lr_ts<-predict(output.lr,xt,type="response")
cm.lr<-table(Actual_Value=x_dumm$pha,Predicted_Value=res.lr)
cm.lr#train data accuracy
(cm.lr[[1,1]]+cm.lr[[2,2]])/sum(cm.lr)
cm.lr_ts<-table(Actual_Value=xt$pha,Predicted_Value=res.lr_ts)
cm.lr_ts#test data accuracy

(cm.lr_ts[[1,1]]+cm.lr_ts[[2,2]])/sum(cm.lr_ts)



#2.Random Forest
library(randomForest)
output.forest <-randomForest(pha ~ .-(epoch+epoch_mjd+H+epoch_cal),data =x_dumm,ntree=64)
output.forest

res.forest<-predict(output.forest,x_dumm)
res.forest_t<-predict(output.forest,xt)
cm.tr<-table(Actual_Value=x_dumm$pha,Predicted_Value=res.forest)
cm.tr
dim(cm.tr)
cm.te<-table(Actual_Value=xt$pha,Predicted_Value=res.forest_t)
cm.te
(cm.te[[1,1]]+cm.te[[2,2]])/sum(cm.te)
plot(output.forest)
sum_tr <- 0
for (i in 1:2){
  for(j in 1:2693){
    if(i!=j){
      sum_tr <-sum_tr+cm.tr[i,j]
    }
  }
}
sum_tr
print(paste("The Accuracy of Random Forest Model is",sum_tr/48999))
print(paste("The Accuracy of Random Forest Model is",100-(49000-sum_tr)/490))#train data accuracy

dim(cm.te)
sum <- 0
for (i in 1:2){
  for(j in 1:1801){
    if(i!=j){
      sum <-sum+cm.te[i,j]
    }
  }
}
sum
print(paste("The Accuracy of Random Forest Model is",sum/20999))#test data accuracy
cm.te$byclass

#3.K-Means Clustering
x_d=x_dumm[,-32]
y_d=x_dumm$pha

set.seed(7146)
kmean_tr <- kmeans(x_d,2,nstart=55,iter.max = 35)
attributes(kmean_tr)
kmean_tr$centers
kmean_tr$size
kt_tr=table(y_d,kmean_tr$cluster)
kt_tr
sum(diag(kt_tr))/ sum(kt_tr)#train data accuracy

x_d_te=xt[,-32]
y_d_te=xt$pha
kmean_te<- kmeans(x_d_te,2,nstart=75,iter.max = 95)
attributes(kmean_te)
kmean_te$centers
kmean_te$size
kt_te=table(y_d_te,kmean_te$cluster)
kt_te
sum(diag(kt_te))/ sum(kt_te)#test data accuracy


#4.Decision Tree
tree<-ctree(pha~.,x_dumm)
tree
plot(tree,type="simple")
p<-predict(tree,x_dumm,type='prob')
t=table(Predicted=p,Actual=x_dumm$pha)
t
(t[[1,1]]+t[[nrow(t),2]])/sum(t)#train data accuracy

p<-predict(tree,xt)
t_te=table(Predicted=p,Actual=xt$pha)
(t_te[[1,1]]+t_te[[nrow(t_te),2]])/sum(t_te)
t_te#test data accuracy


#5.NaiveBayes

x_tr_nb=x_dumm

x_tr_nb$pha[x_tr_nb$pha == 0] <- 'no'#coverting binary data to category
x_tr_nb$pha[x_tr_nb$pha == 1] <- 'yes'

x_te_nb=xt
x_te_nb$pha[x_te_nb$pha == 0] <- 'no'
x_te_nb$pha[x_te_nb$pha == 1] <- 'yes'

model <- naive_bayes(pha ~ ., data =x_tr_nb)
model
plot(model)


x_tr_nb %>% filter(pha=="no") %>% 
  summarize(mean(e), sd(e))


p <- predict(model, x_tr_nb, type= 'class')
(tab <- table(p,x_tr_nb$pha))
sum(diag(tab))/ sum(tab)


p1<- predict(model, x_te_nb)
(tab1 <- table(p1,x_te_nb$pha))
sum(diag(tab1))/ sum(tab1)#test data accuracy
  

