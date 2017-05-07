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
source(functions.R)


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
histograms<-function(dataset,savefolder){
    lista<-list()
    numeric<-map_lgl(dataset,is.numeric)
    newdataset<-dataset[,numeric]
    colnames<-colnames(newdataset)
    n<-length(newdataset)
    dir.create(file.path(getwd(),savefolder), showWarnings = FALSE)

    for(i in 1:n){
        a<-ggplot(mapping=aes(x=newdataset[,i]))+geom_density()+labs(x=colnames[i])    
        ggsave(paste(colnames[i],".jpg",sep=""),path=savefolder,device="jpg")
    }
}

aaa<- function(data,column){
    numeric<-map_lgl(dataset,is.numeric)
    newdataset<-dataset[,numeric]
    a<-ggplot(data=data,mapping=aes_string(x=column))+geom_density()+labs(x=column)
    return(a)
    }

colnames(dataset)
lista<-map(colnames(dataset),~aaa(dataset,.))

lista[[5]]


dataset2<-bind_cols(dataset,dataset,dataset)

lista<-multiplehist(dataset)

multiplot(plotlist = lista,cols=3)

histograms(newdataset,"plots")

##correlation of variables
cor(dataset)

##boxplot
boxplot(dataset$timespreg)
boxplot(dataset$plaglu)
boxplot(dataset$diastolic)
boxplot(dataset$triceps)

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
binary.partition<-function(dataset,pct,variable){
    
    numvar<-which(colnames(dataset)=="variable")
    dataset<-arrange_(dataset,.dots=(paste("desc(",variable,")",sep="")))
    n<-nrow(dataset)
    ones<-sum(dataset[,variable])
    zeros<-nrow(dataset)-ones

    partones<-map(pct,~round(.*ones))
    partzeros<-map(pct,~round(.*zeros))

    suma<-sum(flatten_dbl(partzeros))+sum(flatten_dbl(partones))
    a<-cumsum(flatten_dbl(partones))
    b<-cumsum(flatten_dbl(partzeros))
    
    dataones<-dataset[1:ones,]
    datazeros<-dataset[(ones+1):nrow(dataset),]

    train<-bind_rows(dataones[1:a[1],],datazeros[1:b[1],])
    validate<-bind_rows(dataones[(a[1]+1):a[2],],
                    datazeros[(b[1]+1):b[2],])
    test<-bind_rows(dataones[(a[2]+1):nrow(dataones),],
                    datazeros[(b[2]+1):nrow(datazeros),])
    
    lista<-list(train,validate,test)
    names(lista)<-c("train","validate","test")
    return(lista)
}

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


