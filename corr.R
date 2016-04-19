corr<-function(directory,threshold=0){
	r<-numeric(332)
	nobs<-numeric(332)	
     	i<-1
	while(i<=332){
		if(i<10) dir<-paste(directory,"/00",as.character(i),".csv",sep="")
		else if(i<100) dir<-paste(directory,"/0",as.character(i),".csv",sep="")
		else dir<-paste(directory,"/",as.character(i),".csv",sep="")
		temp1<-read.csv(dir)
		good<-complete.cases(temp1)
		data<-temp1[good,]
		if(nrow(data)>threshold) r[i]<-cor(data$sulfate,data$nitrate)
		i<-i+1
		}
	better<-is.na(r)
	r<-r[!better]
	r<-r[r!=0]
}