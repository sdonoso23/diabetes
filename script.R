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
plotlist<-multiplehist(dataset)
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


##correlation of variables
corr<-as.data.frame(cor(dataset[,-9])) %>%
    rownames_to_column(var = "var1") %>%
    gather(timespreg:age,key = var2,value = value) 

corrplot<-ggplot(data=corr,aes(x=var1, y=var2,fill=value))+geom_tile()+
    geom_text(aes(label=round(value,2)),color="white")

ggsave("plots/corrplot.png",corrplot,width = 8,height = 4)
write_csv(as.data.frame(cor(dataset[,-9])),"correlation.csv")


##remove rows with missing values and columns with too many missing values
finaldataset<-dataset[dataset$plaglu!=0,]
finaldataset<-finaldataset[finaldataset$bmi!=0,]
finaldataset<-finaldataset[,c(-4,-5)]

##count of diabetes
table(finaldataset$diabetes)
county<-ggplot()+geom_bar(data=finaldataset,aes(x=diabetes),stat="count")
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

testidx<-createDataPartition(y=notrain$diabetes,list=FALSE,p=0.5)
test<-notrain[testidx,]
validate<-notrain[-testidx,]

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

dtree<-rpart(diabetes~.,data=train,method="class")
summary(dtree)
plot(dtree,margin=0.2)
text(dtree,all=TRUE,use.n = TRUE,cex=0.5)
dtree.pred<-predict(dtree,validate,type = "class")
confusionMatrix(data=dtree.pred,reference = validate$diabetes,positive="Yes")

##print results of 10-fold cross validation
#search for the tree with the lowest xerror and then choose
#the tree with the lowest number of splits within 1 sd

printcp(dtree)
dtree.prune<-prune(dtree,cp = 0.02)
plot(dtree.prune,margin=0.2)
text(dtree.prune,all=TRUE,use.n = TRUE,cex=0.5)
dtree.prune.pred<-predict(dtree.prune,validate,type="class")
confusionMatrix(data=dtree.prune.pred,reference = validate$diabetes,positive="Yes")

##with tree package
dtree2<-tree(diabetes~.,data=train,method = "class")
summary(dtree2)
plot(dtree2)
text(dtree2,pretty=0)
###training misclassifcation = 20.3%

##prediction on test dataset
dtree2.pred<-predict(dtree2,validate,type="class")
confusionMatrix(data=dtree2.pred,reference=validate$diabetes,positive="Yes")

## test missclassifcation = 25.8%


##crossvalidation to find minimum error tree 
cv.dtree2<-cv.tree(dtree2,FUN=prune.misclass)
cv.dtree2
###best tree with 9 nodes

##best model with 9 nodes
dtree2.prune<-prune.misclass(dtree2,best=3)
summary(dtree.prune)
plot(dtree.prune)
text(dtree.prune,pretty=0)

##prediction on test dataset
dtree2.prune.pred<-predict(dtree.prune2,validate,type="class")
confusionMatrix(data=dtree2.prune.pred,reference=validate$diabetes,positive="Yes")

#####KNN no normalized#####
lista<-list()
missrate<-c()

for (i in 1:25){
    a<-knn(train = train[,-7],test=validate[,-7],k = i,cl=train$diabetes)
    lista[[i]]<-a
    missrate[i]<-mean(ifelse(lista[[i]]==validate$diabetes,FALSE,TRUE))
    }
    
vector<-c(1:25)
plot(vector,missrate)
###lowest probability with k=17
confusionMatrix(data=lista[[17]],reference=validate$diabetes,positive="Yes")



#####NAIVE BAYES####

##separate variables in x and y

naivetrain<-train[,-7]
naivelabels<-train$diabetes

naive<-train(naivetrain,naivelabels,"nb",trControl=trainControl(method="cv",number=10))
naive.pred<-predict(naive,newdata = validate,type = "raw")
confusionMatrix(data=naive.pred,reference=validate$diabetes,positive="Yes")


###LOGIT###

logit<-glm(diabetes~.,data=train,family=binomial(link=logit))
logit.pred<-predict(logit,newdata = validate,type="response")
logit.pred.class<-ifelse(logit.pred>0.5,"Yes","No")
confusionMatrix(data=logit.pred.class,reference=validate$diabetes,positive="Yes")

