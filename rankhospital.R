## Takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a 
## hospital in that state for that outcome (num). The function reads 
## the outcome-of-care-measures.csv file and returns a character vector with the name
##of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
        if(!any(state==state.abb)) {
                stop("invalid state")
        }
        if(!any(outcome==c("heart attack","heart failure","pneumonia"))) {
                stop("invalid outcome")
        }
        mydata<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        tot_rows<-as.numeric(nrow(mydata))
        if(!any(num==c("best","worst",1:tot_rows))) {
                stop("invalid num")
        }
        mystate<-subset(mydata,State==state)
        mystate2<-mystate[order(mystate$Hospital.Name),]
        if(num=="best") {
                num<-1
        }
        if(outcome=="heart attack") {
                myoutcome<-subset(mystate2,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available")
                if(num=="worst") {
                        num<-nrow(myoutcome)
                }
                myoutcome2<-transform(myoutcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                myoutcome3<-myoutcome2[order(myoutcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                myoutcome4<-myoutcome3[num,]
        }
        if(outcome=="heart failure") {
                myoutcome<-subset(mystate2,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available")
                if(num=="worst") {
                        num<-nrow(myoutcome)
                }
                myoutcome2<-transform(myoutcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
                myoutcome3<-myoutcome2[order(myoutcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                myoutcome4<-myoutcome3[num,]
        }
        if(outcome=="pneumonia") {
                myoutcome<-subset(mystate2,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available")
                if(num=="worst") {
                        num<-nrow(myoutcome)
                }
                myoutcome2<-transform(myoutcome,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
                myoutcome3<-myoutcome2[order(myoutcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                myoutcome4<-myoutcome3[num,]
        }   
        as.character(myoutcome4[,"Hospital.Name"])
}
