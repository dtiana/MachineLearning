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






