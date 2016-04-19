best<-function(state,outcome){
    ##read outcome data
    outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    a<-subset(outcome_data,State==state)
    
    ##check that state and outcome are valid
    if(nrow(a)==0) stop("invalid state")
    
    if (outcome=="heart attack") x<-11 
    else if (outcome=="heart failure") x<-17
    else if (outcome=="pneumonia") x<- 23 
    else stop("invalid outcome")
    
    ##return hospital name in that state with the lowest 30-day death rate
    a<-subset(a, a[,x]!="Not Available")
    a[,x]<-as.numeric(a[, x])
    b<-a[order(a[,x],a$Hospital.Name),]
    b[1:5,c(2,7,x)]
}