#importing Data----

data<-read.csv("indian_liver_patient.csv")

#Data Preprocessin----
summary(data)
is.na(data$Albumin_and_Globulin_Ratio)
data$Albumin_and_Globulin_Ratio<-ifelse(is.na(data$Albumin_and_Globulin_Ratio)
                                        ,ave(data$Albumin_and_Globulin_Ratio,FUN=function(x) mean(x,na.rm = T))
                                        ,data$Albumin_and_Globulin_Ratio)

data$Gender<-factor(data$Gender,levels = c('Female','Male'),
                    labels = c(0,1))

data<-transform(data,Gender=as.integer(Gender),Dataset=as.integer(Dataset))

N<-which(data$Dataset==0)
Y<-which(data$Dataset==1)
length(N)
length(Y)
set.seed(121)
Up.sampling1<-sample(Y,length(N),replace = TRUE)
data<-data[c(Up.sampling1,N),]
#Splitting dataset into the training set and test set----

#install.packages('caTools')
library(caTools)
set.seed(123)  #to generate random values
split<-sample.split(data$Dataset,SplitRatio = 0.75)
tr_data<-subset(data,split == TRUE)
ts_data<-subset(data,split == FALSE)


#KNN----

tr1_data = tr_data
ts1_data = ts_data
#Normalize
tr1_data[,1:10]<-scale(tr1_data[,1:10])
ts1_data[,1:10]<-scale(ts1_data[,1:10])

#Apply knn
library(class)
classifier2<-knn(train = tr1_data[-11],
                 test = ts1_data[-11],
                 cl = tr1_data[,11],
                 k = 1)

#Confusion Matrix for KNN----
cm2<-table(ts1_data[,11],classifier2)

cat("\nKNN :")

acc2<-sum(diag(cm2)) / sum(cm2)
cat("\nAccuracy of KNN:",acc2*100)
rec2<-cm2[2,2]/sum(cm2[2,])
cat("\nRecall of KNN is:",rec2*100)
spe2<-cm2[1,1]/sum(cm2[1,])
cat("\nSpecificity of KNN is:",spe2*100)
pre2<-cm2[2,2]/sum(cm2[,2])
cat("\nPrecision of KNN is:",pre2*100,"\n")


#RANDOM fOREST----
library("randomForest")

ts3_data<-ts_data
tr3_data<-tr_data
set.seed(345)

#Applying random forest
rfm= randomForest(Dataset ~ .,data = tr3_data,ntree=500)
y_pred3= predict(rfm,ts3_data)
y_pred3<-ifelse(y_pred3>0.5,1,0)

#Confusion Matrix for RF----
cm3<-table(ts3_data[,11],y_pred3)
cat("\nRF :")

acc3<-sum(diag(cm3)) / sum(cm3)
cat("\nAccuracy of RF:",acc3*100)
rec3<-cm3[2,2]/sum(cm3[2,])
cat("\nRecall of RF is:",rec3*100)
spe3<-cm3[1,1]/sum(cm3[1,])
cat("\nSpecificity of RF is:",spe3*100)
pre3<-cm3[2,2]/sum(cm3[,2])
cat("\nPrecision of RF is:",pre3*100,"\n")




