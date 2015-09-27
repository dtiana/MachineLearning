##Reading the data 
library(caret)
library(doParallel)
registerDoParallel(3)
data_read<-function(path_name){
    data<-read.csv(path_name,header=TRUE,na.strings = NA)
    for (i in 1:7){
        data[1]<-NULL
    }
    return (data)    
}
slicing<-function(data,return_train_value){
    set.seed(3233)
    folds<-createFolds(data$classe,k=5,list=TRUE,returnTrain=return_train_value)
    return (folds)
}
fold_apply<-function(fold,data){
    return<-data[fold,]
}
partition<-function(data){
    train_vector<-createDataPartition(y=data$classe,p=0.7,list=FALSE)
    return (train_vector)
}

clean_data<-function(data){
    index_vector<-vector(mode="logical", length=length(data))
    for (i in 1:length(data)){
        if(any(is.na(data[i]))){
            index_vector[i]<-FALSE
        }else{
            index_vector[i]<-TRUE
            }
    }
    data<-subset(data,select=index_vector)
    v1<-grep("kurtosis",colnames(data))
    v2<-grep("skewness",colnames(data))
    v3<-grep("yaw",colnames(data))
    vfinal<-sort(unique(c(v1,v2,v3)))
    data<-data[-vfinal]
    return (data)
}



data<-data_read("./pml-training.csv")

partition_vector<-partition(data)
train_fold<-data[partition_vector,]
test_fold<-data[-partition_vector,]


train_fold<-clean_data(train_fold)


cor_train_fold<-cor(train_fold[-ncol(train_fold)])
vector_cor_train<-findCorrelation(cor_train_fold)
train_fold<-train_fold[-vector_cor_train]
modelFit<-train(classe ~ ., data = train_fold, method = "rf")

####turning off resampling
#fitControl<- trainControl(method = "none")
#tgrid<- expand.grid(mtry=c(6)) 
#modelFit<-train(classe ~ ., data = train_fold, method = "rf", trControl = fitControl, tuneGrid = tgrid)
####

cf_matrix<-confusionMatrix(data=predict(modelFit,test_fold),reference=test_fold$classe)



Random Forest 

13737 samples
41 predictor
5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Bootstrapped (25 reps) 
Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
Resampling results across tuning parameters:
    
    mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
2    0.9845741  0.9804810  0.002477945  0.003134743
21    0.9842561  0.9800816  0.002364515  0.002988925
41    0.9676362  0.9590550  0.003799445  0.004807399

Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 2.



Confusion Matrix and Statistics

Reference
Prediction    A    B    C    D    E
A 1672    7    0    0    0
B    2 1125    8    0    0
C    0    7 1017   25    0
D    0    0    1  939    3
E    0    0    0    0 1079

Overall Statistics

Accuracy : 0.991           
95% CI : (0.9882, 0.9932)
No Information Rate : 0.2845          
P-Value [Acc > NIR] : < 2.2e-16       

Kappa : 0.9886          
Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9988   0.9877   0.9912   0.9741   0.9972
Specificity            0.9983   0.9979   0.9934   0.9992   1.0000
Pos Pred Value         0.9958   0.9912   0.9695   0.9958   1.0000
Neg Pred Value         0.9995   0.9971   0.9981   0.9949   0.9994
Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
Detection Rate         0.2841   0.1912   0.1728   0.1596   0.1833
Detection Prevalence   0.2853   0.1929   0.1782   0.1602   0.1833
Balanced Accuracy      0.9986   0.9928   0.9923   0.9866   0.9986

