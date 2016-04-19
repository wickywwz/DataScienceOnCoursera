pollutantmean<-function(directory,pollutant,id=1:332){
    fileid<-id
    if(id[1]<10) dir<-paste(directory,"/00",as.character(id[1]),".csv",sep="")
    else if(id[1]<100) dir<-paste(directory,"/0",as.character(id[1]),".csv",sep="")
    else dir<-paste(directory,"/",as.character(id[1]),".csv",sep="")
    temp1<-read.csv(dir)
    if(length(id)>1){
        i<-2
        while(i<=length(id)){
            if(id[i]<10) dir<-paste(directory,"/00",as.character(id[i]),".csv",sep="")
            else if(id[i]<100) dir<-paste(directory,"/0",as.character(id[i]),".csv",sep="")
            else dir<-paste(directory,"/",as.character(id[i]),".csv",sep="")
            temp2<-read.csv(dir)
            temp2<-rbind(temp1,temp2)
            temp1<-temp2
            i=i+1
        }
    }	
    good<-complete.cases(temp1)
    data<-temp1[good,]
    if(pollutant=="sulfate")
        pollmean<-mean(data$sulfate)
    else
        pollmean<-mean(data$nitrate)
    pollmean
}
