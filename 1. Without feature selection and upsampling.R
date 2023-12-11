library(caTools)
f<-read.csv("Dataset_of_Diabetes.csv")
f<-f[,c(-1,-2)]

#data cleaning----
  #remove duplicate values    
      f<-unique(f) 

  #Fixing and standardizing typos
      #1. Gender
        for(i in 1:length(f$Gender)){
          if (f$Gender[i]=='M'){
            f$Gender[i]=0;
          }
          else{
            f$Gender[i]=1;
          }
        }
      #2. CLASS
        for(i in 1:length(f$CLASS)){
          if (f$CLASS[i]=='N'){
            f$CLASS[i]=1;
          }
          else if(f$CLASS[i]=='P'){
            f$CLASS[i]=2;
          }
          else{
            f$CLASS[i]=3;
          }
        }
      f<-transform(f,Gender=as.integer(Gender),CLASS=as.integer(CLASS));

#dividing into training and testing data----
      set.seed(123)
      split<-sample.split(f,SplitRatio  = 0.7)
      tr_data<-subset(f,split==TRUE)
      ts_data<-subset(f,split==FALSE)

#DECISION TREE----
      library(rpart)
      model2 <- rpart(CLASS ~ . , data=tr_data,method="class")
      
      #plot(model2, uniform=TRUE, main="Classification Tree for Diabetes")
      #text(model2, use.n=TRUE, all=TRUE, cex=.8)
      
      treePred <- predict(model2, ts_data, type = 'class')
      tb1 = table(treePred, ts_data$CLASS)
      cat("\n")
      cat("\nDecison tree :")
      #Accuracy
      accuracy_Test <- sum(diag(tb1)) / sum(tb1)
      cat("\n    Accuracy :",accuracy_Test)
      acc_vector = c(accuracy_Test)
      
  #Precision
      pre91<-(tb1[1,1]/sum(tb1[,1]))
      cat("\n    Precision of Class N :",pre91)
      pre92<-(tb1[2,2]/sum(tb1[,2]))
      cat("\n    Precision of Class P :",pre92)
      pre93<-(tb1[3,3]/sum(tb1[,3]))
      cat("\n    Precision of Class Y :",pre93)
      
      prec_n = c(pre91)
      prec_p = c(pre92)
      prec_y = c(pre93)
  #Recall
      rec91<-(tb1[1,1]/sum(tb1[1,]))
      cat("\n    Recall of Class N :",rec91)
      rec92<-(tb1[2,2]/sum(tb1[2,]))
      cat("\n    Recall of Class P :",rec92)
      rec93<-(tb1[3,3]/sum(tb1[3,]))
      cat("\n    Recall of Class Y :",rec93,"\n")
      
      rec_n = c(rec91)
      rec_p = c(rec92)
      rec_y = c(rec93)

      
#KNN----
      
      tr1_data = tr_data
      ts1_data = ts_data
  #Normalize
      tr1_data[,2:11]<-scale(tr1_data[,2:11])
      ts1_data[,2:11]<-scale(ts1_data[,2:11])
      
  #Apply knn
      library(class)
      y_pred<-knn(train = tr1_data[-12],
                  test = ts1_data[-12],
                  cl = tr1_data[,12],
                  k = 3)
      
  #Confusion Matrix
      cf1<-table(actual=ts1_data$CLASS,predicted = y_pred)
      
      cat("\nKNN :")
      #accuracy
      acc<-(cf1[[1,1]]+cf1[[2,2]]+cf1[[3,3]])/sum(cf1)
      cat("\n    Accuracy :",acc)
      acc_vector = c(acc_vector,acc)
      
      #Precision
      pre1<-(cf1[1,1]/sum(cf1[,1]))
      cat("\n    Precision of Class N :",pre1)
      pre2<-(cf1[2,2]/sum(cf1[,2]))
      cat("\n    Precision of Class P :",pre2)
      pre3<-(cf1[3,3]/sum(cf1[,3]))
      cat("\n    Precision of Class Y :",pre3)

      prec_n = c(prec_n,pre1)
      prec_p = c(prec_p,pre2)
      prec_y = c(prec_y,pre3)
      
      #Recall
      rec1<-(cf1[1,1]/sum(cf1[1,]))
      cat("\n    Recall of Class N :",rec1)
      rec2<-(cf1[2,2]/sum(cf1[2,]))
      cat("\n    Recall of Class P :",rec2)
      rec3<-(cf1[3,3]/sum(cf1[3,]))
      cat("\n    Recall of Class Y :",rec3,"\n")

      rec_n = c(rec_n,rec1)
      rec_p = c(rec_p,rec2)
      rec_y = c(rec_y,rec3)
      
      
#RANDOM fOREST----
      library("randomForest")

      ts2_data<-ts_data
      tr2_data<-tr_data
      set.seed(345)

  #Applying random forest
      rfm= randomForest(CLASS~.,data = tr2_data,ntree=10)
      y_pred1= predict(rfm,ts2_data)

      y_pred1[y_pred1<1.5]=1
      y_pred1[1.5<=y_pred1 & y_pred1<2.5]=2
      y_pred1[2.5<=y_pred1]=3

      ts2_data$y_pred1 = y_pred1
      
  #Confusion Matrix
      cf2<-table(actual=ts2_data$CLASS,predicted = y_pred1)

      cat("\nRANDOM FOREST:")
  #accuracy
      acc2<-(cf2[[1,1]]+cf2[[2,2]]+cf2[[3,3]])/sum(cf2)
      cat("\n    Accuracy:",acc2)
      acc_vector = c(acc_vector,acc2)
      
  #Precision
      pre12<-(cf2[1,1]/sum(cf2[,1]))
      cat("\n    Precision of Class N :",pre12)
      pre22<-(cf2[2,2]/sum(cf2[,2]))
      cat("\n    Precision of Class P :",pre22)
      pre33<-(cf2[3,3]/sum(cf2[,3]))
      cat("\n    Precision of Class Y :",pre33)

      prec_n = c(prec_n,pre12)
      prec_p = c(prec_p,pre22)
      prec_y = c(prec_y,pre33)

      #Recall
      rec12<-(cf2[1,1]/sum(cf2[1,]))
      cat("\n    Recall of Class N :",rec12)
      rec22<-(cf2[2,2]/sum(cf2[2,]))
      cat("\n    Recall of Class P : ",rec22)
      rec33<-(cf2[3,3]/sum(cf2[3,]))
      cat("\n    Recall of Class Y :",rec33)
      cat("\n")      
      cat("\n")
      cat("\n")
      
      rec_n = c(rec_n,rec12)
      rec_p = c(rec_p,rec22)
      rec_y = c(rec_y,rec33)
      
      
# XGBoost----
# library(xgboost)
# classifier<-xgboost(data = as.matrix(tr_data[-12]),label = tr_data$CLASS,nrounds = 10)
# y_pred2<-predict(classifier,newdata = as.matrix(ts_data[,12]))
# cf3<-table(actual=ts_data$CLASS,predicted = y_pred2)
# acc3<-(cf3[[1,1]]+cf3[[2,2]])/sum(cf3)

      
# Visulaisation----
      plot(acc_vector,type="o",col="red",xlab="Algorithms",ylab="Accuracy",xaxt='n',lwd=5,cex.lab=1.5)
      axis(1,at=c(1,2,3),labels=c("DT","KNN","RF"),cex.axis=2)

      
      plot(prec_n,type="o",col="red",xlab="Algorithms",ylab="Precision",ylim=c(0,1),xaxt='n',lwd=5,cex.lab=1.5)
      lines(prec_p, type="o",col="blue",lwd=5)
      lines(prec_y, type="o",col="green",lwd=5)
      legend(1,0.55,legend = c("Class N","Class P","Class Y"),fill = c("red","blue","green"),cex=1)
      axis(1,at=c(1,2,3),labels=c("DT","KNN","RF"),cex.axis=2)
      
      plot(rec_n,type="o",col="red",xlab="Algorithms",ylab="Recall",ylim=c(0,1),xaxt='n',lwd=5,cex.lab=1.5)
      lines(rec_p, type="o",col="blue",lwd=5)
      lines(rec_y, type="o",col="green",lwd=5)
      legend(1,0.55,legend = c("Class N","Class P","Class Y"),fill = c("red","blue","green"),cex=1)
      axis(1,at=c(1,2,3),labels=c("DT","KNN","RF"),cex.axis=2)
      