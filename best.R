
## Takes two arguments: the 2-character abbreviated name of a state
## and an outcome name. The function reads the 
## outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 
## 30-day mortality for the specified outcome in that state. 
## The hospital name is the name provided in the Hospital.Name variable. 
## The outcomes can be one of \heart attack", \heart failure", or \pneumonia". 
## Hospitals that do not have data on a particular outcome should be 
## excluded from the set of hospitals when deciding the rankings.

best <- function(state, outcome) {
        if(!any(state==state.abb)) {
                stop("invalid state")
                }
        if(!any(outcome==c("heart attack","heart failure","pneumonia"))) {
                stop("invalid outcome")
                }
        mydata<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        mystate<-subset(mydata,State==state)
        mystate2<-mystate[order(mystate$Hospital.Name),]
        if(outcome=="heart attack") {
                myoutcome<-subset(mystate2,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available")
                myoutcome2<-myoutcome[which.min(myoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                }
        if(outcome=="heart failure") {
                myoutcome<-subset(mystate2,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available")
                myoutcome2<-myoutcome[which.min(myoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                }
        if(outcome=="pneumonia") {
                myoutcome<-subset(mystate2,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available")
                myoutcome2<-myoutcome[which.min(myoutcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                }   
        as.character(myoutcome2[,"Hospital.Name"])
        }

