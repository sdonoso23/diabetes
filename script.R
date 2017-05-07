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

##check missing
colSums(dataset==0)
###triceps and serum have too many missing

##histograms

histsplots<-multiplehist(dataset)

a<-multiplot(plotlist = lista,cols=3)

##correlation of variables
cor(dataset)

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
partition<-binary.partition(finaldataset,c(0.70,0.15,0.15),"diabetes")
partitionnorm<-binary.partition(normalize,c(0.70,0.15,0.15),"diabetes")

##check means
mean(partition$train$diabetes)
mean(partition$test$diabetes)
mean(partition$validate$diabetes)
mean(partitionnorm$train$diabetes)
mean(partitionnorm$test$diabetes)
mean(partitionnorm$validate$diabetes)

##transform diabetes variable to factor
partitionnorm$train$diabetes<-as.factor(partition$train$diabetes)
partition$train$diabetes<-as.factor(partition$train$diabetes)


####MODELS####

####DECISION TREE#####

##with rpart package

decisiontree<-rpart(diabetes~.,data=partition$train,method="class")
printcp(decisiontree)

plot(decisiontree)
text(decisiontree,use.n=TRUE,all=TRUE,cex=0.9,)


confusionMatrix(data=results$dtree.prune.pred,reference=results$diabetes)

##better plot
fancyRpartPlot(decisiontree)
fancyRpartPlot(maxtree)


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


