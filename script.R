library(tidyverse)
library(MASS)
library(rpart)
library(naivebayes)
library(class)
library(modelr)
library(caret)
library(tree)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(klaR)
source("r/functions.R")


#####LOAD DATA####
dataset<-read_csv("data/diabetes.txt",col_names=c("timespreg","plaglu",
                                             "diastolic","triceps",
                                             "serum","bmi","diabped",
                                             "age","diabetes"))

####EXPLORATORY ANALYSIS####

##histograms
plotlist<-multiplehist(dataset)
histograms<-multiplot(plotlist = plotlist,cols=3)
###seems to be too many values with 0 in some variables 

##check missing
colSums(is.na(dataset))
colSums(dataset==0)
###triceps and serum have too many missing as 0

##transform diabetes to factor
dataset$diabetes<-factor(dataset$diabetes,levels=c(0,1))
levels(dataset$diabetes)<-c("No","Yes")

##correlation of variables
corr<-as.data.frame(cor(dataset[,-9])) %>%
    rownames_to_column(var = "var1") %>%
    gather(timespreg:age,key = var2,value = value) 

ggplot()+geom_tile(data=corr,aes(x=var1, y=var2,fill=value))

##remove rows with missing values and columns with too many missing values
finaldataset<-dataset[dataset$plaglu!=0,]
finaldataset<-finaldataset[finaldataset$bmi!=0,]
finaldataset<-finaldataset[,c(-4,-5)]

##replace missing values with mean in diastolic
diastolic<-dataset[dataset$diastolic!=0,]
mu<-mean(diastolic$diastolic)
finaldataset<-mutate(finaldataset,diastolic=ifelse(diastolic==0,mu,diastolic))
rm(diastolic)

##standardize variables
normalize<-as.data.frame(scale(finaldataset[,-7]))
normalize$diabetes<-finaldataset$diabetes

####DATASET PARTITION####

##sample partition
set.seed(1234)
trainidx<-createDataPartition(y = finaldataset$diabetes,
                              list = FALSE,p = 0.7)
train<-finaldataset[trainidx,]
notrain<-finaldataset[-trainidx,]

testidx<-createDataPartition(y=notrain$diabetes,list=FALSE,p=0.5)
test<-notrain[testidx,]
validate<-notrain[-testidx,]

##check means
table(train$diabetes)/nrow(train)
table(test$diabetes)/nrow(test)
table(validate$diabetes)/nrow(validate)



####MODELS####

####DECISION TREE#####

##with rpart package

dtree<-rpart(diabetes~.,data=train,method="class")
summary(dtree)

printcp(dtree)

plot(dtree,margin=0.2)
text(dtree,all=TRUE,use.n = TRUE,cex=0.5)

confusionMatrix(data=results$dtree.prune.pred,reference=results$diabetes)

##better plot

rpart.plot(dtree,type=1)



##with tree package

dtree<-tree(diabetes~.,data=partition$train,method = "class")
summary(dtree)
plot(dtree)
text(dtree,pretty=0)
###training misclassifcation = 20.3%

##prediction on test dataset
dtree.pred<-predict(dtree,partition$test,type="class")
results<-partition$test
results$dtree.pred<-dtree.pred
results$dtree.misclass<-ifelse(test$dtree.pred==test$diabetes,FALSE,TRUE)
mean(test$dtree.misclass)
## test missclassifcation = 25.8%


##crossvalidation to find minimum error tree 
cv.dtree<-cv.tree(dtree,FUN=prune.misclass)
cv.dtree
###best tree with 9 nodes

##best model with 9 nodes
dtree.prune<-prune.misclass(dtree,best=9)
summary(dtree.prune)
plot(dtree.prune)
text(dtree.prune,pretty=0)

##prediction on test dataset
dtree.prune.pred<-predict(dtree.prune,partition$test,type="class")
results$dtree.prune.pred<-dtree.prune.pred
results$dtree.prune.misclass<-ifelse(results$dtree.prune.pred==results$diabetes,FALSE,TRUE)
mean(results$dtree.prune.misclass)


#####KNN#####
lista<-list()
missrate<-c()

for (i in 1:25){
    a<-knn(train = partitionnorm$train[,-7],test=partitionnorm$test[,-7],k = i,cl=partitionnorm$train$diabetes)
    lista[[i]]<-a
    missrate[i]<-mean(ifelse(lista[[i]]==partitionnorm$test$diabetes,FALSE,TRUE))
    }
    
vector<-c(1:25)
plot(vector,missrate)
###lowest probability with k=15

#####NAIVE BAYES####

##separate variables in x and y

naivetrain<-partition$train[,-7]
naivelabels<-partition$train$diabetes

naive<-train(naivetrain,naivelabels,"nb",trControl=trainControl(method="cv",number=10))

naive.pred<-predict(naive1,newdata = partition$test,type = "raw")


table(naive.pred,partition$test$diabetes)
###misclass of 25%


