complete<-function(directory,id=1:332){
    ID<-numeric(length(id))
    nobs<-numeric(length(id))	
    i<-1
    while(i<=length(id)){
        if(id[i]<10) dir<-paste(directory,"/00",as.character(id[i]),".csv",sep="")
        else if(id[i]<100) dir<-paste(directory,"/0",as.character(id[i]),".csv",sep="")
        else dir<-paste(directory,"/",as.character(id[i]),".csv",sep="")
        temp1<-read.csv(dir)
        good<-complete.cases(temp1)
        data<-temp1[good,]
        ID[i]<-id[i]
        nobs[i]<-nrow(data)
        i=i+1
    }
    comdata<-cbind(ID,nobs)	
    comdata
}