library(tidyverse)

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