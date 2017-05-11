library(tidyverse)
library(MASS)
library(rpart)
library(class)
library(modelr)
library(caret)
library(tree)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(klaR)
library(e1071)
source("r/functions.R")


#####LOAD DATA####
dataset<-read_csv("data/diabetes.txt",col_names=c("timespreg","plaglu",
                                             "diastolic","triceps",
                                             "serum","bmi","diabped",
                                             "age","diabetes"))

####EXPLORATORY ANALYSIS####

##histograms
plotlist<-multiplehist(dataset[,-9])
multiplot(plotlist = plotlist,cols=3)
ggsave("plots/histograms.png",
       multiplot(plotlist = plotlist,cols=3),width=8,height=4)

###seems to be too many values with 0 in some variables 

##check missing
colSums(is.na(dataset))
colSums(dataset==0)
###triceps and serum have too many missing as 0

##transform diabetes to factor
dataset$diabetes<-factor(dataset$diabetes,levels=c(0,1)
                         ,labels = c("No","Yes"))

###outliers
normdata<-as.data.frame(scale(dataset[,-9]))
outliersplot<-normdata %>%
    gather(timespreg:age,key=var,value=value) %>%
    ggplot()+geom_boxplot(aes(x=var,y=value))
outliersplot
ggsave("plots/outliers.jpg",outliersplot,width=8,height=4)

##correlation of variables
corr<-as.data.frame(cor(dataset[,-9])) %>%
    rownames_to_column(var = "var1") %>%
    gather(timespreg:age,key = var2,value = value) 

corrplot<-ggplot(data=corr,aes(x=var1, y=var2,fill=value))+geom_tile()+
    geom_text(aes(label=round(value,2)),color="white")

corrplot
ggsave("plots/corrplot.png",corrplot,width = 8,height = 4)

write_csv(as.data.frame(cor(dataset[,-9])),"output/correlation.csv")


##remove rows with missing values and columns with too many missing values
#remove missing values in plaglu
finaldataset<-dataset[dataset$plaglu!=0,]
#remove missing values in bmi
finaldataset<-finaldataset[finaldataset$bmi!=0,]
#remove columns serum and triceps
finaldataset<-finaldataset[,c(-4,-5)]
#remove outlier in bmi
finaldataset<-finaldataset[finaldataset$bmi!=max(finaldataset$bmi),]
#log transformation for outliers
finaldataset$diabped<-log(finaldataset$diabped)
finaldataset$bmi<-log(finaldataset$bmi)

##count of diabetes
table(finaldataset$diabetes)
county<-ggplot()+geom_bar(data=finaldataset,aes(x=diabetes),stat="count")
county
ggsave("plots/diabetes.png",county,width = 8,height = 4)

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

set.seed(1234)
testidx<-createDataPartition(y=notrain$diabetes,list=FALSE,p=0.5)
test<-notrain[testidx,]
validate<-notrain[-testidx,]

normtrain<-normalize[trainidx,]
normnotrain<-normalize[-trainidx,]
normtest<-normnotrain[testidx,]
normvalidate<-normnotrain[-testidx,]


##check means
table(train$diabetes)/nrow(train)
table(test$diabetes)/nrow(test)
table(validate$diabetes)/nrow(validate)

####MODELS####

####DECISION TREE#####
#define tree with function defaults: 
#gini as splitting criteria, priors defined by sample, 
#loss with value 1
#20 as minimum obs for a node to be splitted
#20/3 as minimum obs in a terminal node
#0.01 as minimum increase of fit for a split to be performed
set.seed(1234)
dtree<-rpart(diabetes~.,data=train,method="class")

#tree predictions
dtree.pred<-predict(dtree,validate,type = "class")
dtree.out<-confusionMatrix(data=dtree.pred,reference = validate$diabetes,positive="Yes")
dtree.out

##print results of 10-fold cross validation
#search for the tree with the lowest xerror and then choose
#the tree with the lowest number of splits within 1 sd
printcp(dtree)

jpeg("plots/cvtree.jpg")
plotcp(dtree)
dev.off()

dtree.prune<-prune(dtree,cp = 0.028)

jpeg("plots/dtreeprune.jpg")
fancyRpartPlot(dtree.prune)
dev.off()

#pruned tree predictions
dtree.prune.pred<-predict(dtree.prune,validate,type="class")
dtree.prune.out<-confusionMatrix(data=dtree.prune.pred,reference = validate$diabetes,positive="Yes")
dtree.prune.out

#####KNN NORMALIZED#####
knn.pred<-list()
knn.missrate<-c()

for (i in 1:25){
    set.seed(1234)
    a<-knn(train = normtrain[,-7],test=normvalidate[-7],k = i,cl=normtrain$diabetes)
    knn.pred[[i]]<-a
    knn.missrate[i]<-mean(ifelse(knn.pred[[i]]==normvalidate$diabetes,FALSE,TRUE))
    }

k<-c(1:25)    
names(knn.pred)<-paste("K=",k,sep="")

knn.plot<-ggplot()+geom_line(aes(x=k,y=knn.missrate))
knn.plot
ggsave("plots/knnplot.jpg",knn.plot,width=8,height=4)

koptimal<-which.min(knn.missrate)
koptimal

###lowest missclassification with k=11
knn.out<-confusionMatrix(data=knn.pred[[koptimal]],reference=normvalidate$diabetes,positive="Yes")
knn.out

#####NAIVE BAYES####

##separate variables in x and y
naivetrain<-train[,-7]
naivelabels<-train$diabetes

set.seed(1234)
naive<-train(naivetrain,naivelabels,"nb",trControl=trainControl(method="cv",number=10))
naive.pred<-predict(naive,newdata = validate)
naive.out<-confusionMatrix(data=naive.pred,reference=validate$diabetes,positive="Yes")
naive.out

#####COMPARISON####

confmodels<-list(dtree.prune.out,
              knn.out,naive.out)
modelnames<-c("Decision Tree Prune","KNN",
              "Naive Bayes")

comp<-comparisondf(confmodels,modelnames)

complot<-ggplot(data=comp,aes(x=Model,y=Accuracy))+geom_col(fill="green4")
complot
ggsave("plots/accuracy.jpg",complot,width=8,height=4)

#####log loss####

###decision tree
tree.pred<-as.data.frame(predict(dtree.prune,validate,type="prob"))
tree.pred$actual<-ifelse(validate$diabetes=="Yes",1,0)
tree.pred$class<-as.data.frame(predict(dtree.prune,validate,type="class"))
tree.pred$prob<-ifelse(tree.pred$class=="Yes",tree.pred$Yes,tree.pred$No)

logloss(tree.pred$prob,tree.pred$actual)

###naive
naive.pred<-as.data.frame(predict(naive,validate,type="prob"))
naive.pred$actual<-ifelse(validate$diabetes=="Yes",1,0)
naive.pred$class<-as.data.frame(predict(naive,validate,type="raw"))
naive.pred$prob<-ifelse(naive.pred$class=="Yes",naive.pred$Yes,naive.pred$No)
naive.pred$newprob<-pmin(pmax(naive.pred$prob,1e-15),1-1e-15)
naive.pred$log<-naive.pred$actual*log(naive.pred$newprob)+(1-naive.pred$actual)*log(1-naive.pred$newprob)


logloss(naive.pred$prob,naive.pred$actual)

###knn

##generate probabilities

knn.prob<-list()

for (i in 1:25){
    a<-knn(train = normtrain[,-7],test=normvalidate[,-7],k = i,cl=normtrain$diabetes,prob = TRUE)
    knn.prob[[i]]<-a
}


names(knn.prob)<-paste("K=",k,sep="")

knn.log<-as.data.frame(knn.prob[[koptimal]])
knn.log$actual<-ifelse(validate$diabetes=="Yes",1,0)
knn.log$class<-knn.prob[[koptimal]]
knn.log$prob<-attr(knn.prob[[koptimal]],"prob")
knn.log

logloss(knn.log$prob,knn.log$actual)


####CARET####

#traincontrol parameters
fitcontrol<-trainControl(method = "repeatedcv",number = 10,repeats=3,classProbs = TRUE,savePredictions = TRUE,returnResamp = "all")

#knn
set.seed(1234)
fitcontrol<-trainControl(method = "repeatedcv",number = 10,repeats=3,classProbs = TRUE,savePredictions = TRUE,returnResamp = "all")
knngrid<-expand.grid(k=c(1:50))

knn.caret<-train(diabetes~.,data=train,method="knn",preProcess=c("center","scale"),
                 trControl=fitcontrol,tuneGrid=knngrid)


knn.caret.pred<-predict(knn.caret,newdata=validate)
knn.caret.out<-confusionMatrix(knn.caret.pred,reference=validate$diabetes,positive="Yes")


###rpart
set.seed(1234)
rpart.caret<-train(diabetes~.,data=train,method="rpart",trControl=fitcontrol,tuneLength=30)
rpart.caret.pred<-predict(rpart.caret,newdata=validate)
rpart.caret.out<-confusionMatrix(rpart.caret.pred,reference=validate$diabetes,positive="Yes")

###naivebayes
set.seed(1234)
naive.caret<-train(diabetes~.,data=train,method="nb",trControl=fitcontrol,tuneLength=30)
naive.caret.pred<-predict(naive.caret,newdata=validate)
naive.caret.out<-confusionMatrix(naive.caret.pred,reference=validate$diabetes,positive="Yes")


##random forest
set.seed(1234)
rf.caret<- train(diabetes~., data=train, method="rf", trControl=fitcontrol,tuneLength=6,
                 preProcess=c("center","scale"))
rf.caret.pred<-predict(rf.caret,newdata=validate)
rf.caret.out<-confusionMatrix(rf.caret.pred,reference=validate$diabetes,positive="Yes")

complist<-list(knn.caret.out,knn.out,rpart.caret.out,dtree.prune.out,
               naive.caret.out,naive.out,rf.caret.out)
carmod<-c("Caret KNN","KNN","Caret Decision Tree","Decision Tree", 
          "Caret Naive Bayes","Naive Bayes","Random Forest Caret")

totalcomp<-comparisondf(complist,carmod)
totalcomp



#model chosen
rpart.caret.pred2<-predict(rpart.caret,newdata=test)
confusionMatrix(rpart.caret.pred2,reference=test$diabetes,positive="Yes")

