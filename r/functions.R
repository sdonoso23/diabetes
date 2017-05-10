library(tidyverse)
library(caret)


###function to save mutiple histograms
hist.dfnumeric<-function(dataset,savefolder){
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

###function to generate list of histograms
multiplehist<-function(data){
    numeric<-map_lgl(data,is.numeric)
    newdata<-data[,numeric]
    aux<- function(data,column){
        a<-ggplot(data=data,mapping=aes_string(x=column))+geom_density()+labs(x=column)
        return(a)
    }
    colnames(newdata)
    lista<-map(colnames(newdata),~aux(newdata,.))
    return(lista)
}

###function to plot multiple ggplots together, found on R Cookbook
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    require(grid)
    
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    if (is.null(layout)) {
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
        print(plots[[1]])
        
    } else {
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
        
        for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
        }
    }
}

###function to divide dataset into train,validate and test maintaining
###classes proportions
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

###function to compare classification models with binary targets
comparisondf<-function(cmlist,modelnames){
    aux<-function(cm){
        lista<-map(cm,as.data.frame)
        a<-list(lista$table[1,3],lista$table[2,3],lista$table[3,3],lista$table[4,3])
        names(a)<-c("TN","FP","FN","TP")
        b<-rownames_to_column(lista$overall) %>%
            spread(rowname,value = ".x[[i]]")
        c<-rownames_to_column(lista$byClass) %>%
            spread(rowname,value = ".x[[i]]")
        d<-bind_cols(a,b,c)
        return(d)
    }
    aaa<-map(cmlist,aux)
    df<-map_df(aaa,bind_rows)
    df<-add_column(df,modelnames,.before="TN") %>%
        rename(Model=modelnames)
    return(df)
}


logloss<-function(pred,actual){
    minvalue<-0.00000000000001
    newpred<-pmax(pmin(pred,1-minvalue),minvalue)
    logloss<--(1/length(newpred))*sum(actual*log(newpred)+(1-actual)*log(1-newpred))
    return(logloss)
}

logloss2<-function(newpred,actual){
    logloss<--(1/length(newpred))*sum(actual*log(newpred)+(1-actual)*log(1-newpred))
    return(logloss)
}
