rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ## Check that state and outcome are valid
    if (outcome=="heart attack") x<-11 
    else if (outcome=="heart failure") x<-17
    else if (outcome=="pneumonia") x<- 23 
    else stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    a<-split(outcome_data,outcome_data$State,drop=TRUE)
    ## Return a data frame with the hospital names and the
    i<-1
    hospital<-vector()
    state<-vector()
    y<-data.frame(hospital,state)
    while(i<=length(a)) {
        b<-as.data.frame(a[[i]])
        b<-subset(b, b[,x]!="Not Available")
        b[,x]<-as.numeric(b[, x])
        bb<-b[order(b[,x],b[,2]),]
        if(num=="best") y[i,1]=bb[1,2]
        else if (num=="worst") y[i,1]=bb[nrow(bb),2]
        else y[i,1]=bb[num,2]
        y[i,2]=bb[1,7]
        i<-i+1
        
    }
    y
}




