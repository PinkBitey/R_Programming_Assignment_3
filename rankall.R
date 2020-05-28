## Takes two arguments: an outcome name (outcome) and a hospital ranking (num). 
## The function reads the outcome-of-care-measures.csv file and returns a 
## 2-column data frame containing the hospital in each state that has the 
## ranking specified in num.


rankall <- function(outcome, num = "best") {
        if(!any(outcome==c("heart attack","heart failure","pneumonia"))) {
                stop("invalid outcome")
        }
        
        mydata<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
        tot_rows<-as.numeric(nrow(mydata))
        if(!any(num==c("best","worst",1:tot_rows))) {
                stop("invalid num")
        }
        mydata2<-split.data.frame(mydata,mydata$State)
        mydata3<-lapply(mydata2,function(x) x[order(x$Hospital.Name),])
        if(num=="best") {
                num<-1
        }
        heart.attack<- function(x) {
                myoutcome<-subset(x,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available")
                if(num=="worst") {
                        num<-nrow(myoutcome)
                }
                myoutcome2<-transform(myoutcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
                myoutcome3<-myoutcome2[order(myoutcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                myoutcome4<-myoutcome3[num,]
        }
        heart.failure<- function(x) {
          myoutcome<-subset(x,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available")
          if(num=="worst") {
            num<-nrow(myoutcome)
          }
          myoutcome2<-transform(myoutcome,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
          myoutcome3<-myoutcome2[order(myoutcome2$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
          myoutcome4<-myoutcome3[num,]
        }
        pneumonia<- function(x) {
          myoutcome<-subset(x,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available")
          if(num=="worst") {
            num<-nrow(myoutcome)
          }
          myoutcome2<-transform(myoutcome,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
          myoutcome3<-myoutcome2[order(myoutcome2$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
          myoutcome4<-myoutcome3[num,]
        }
        if(outcome=="heart attack") {
                finaldata<-lapply(mydata3,heart.attack)
                finaldata2<-do.call("rbind",finaldata)
        }
        if(outcome=="heart failure") {
          finaldata<-lapply(mydata3,heart.failure)
          finaldata2<-do.call("rbind",finaldata)
        }
        if(outcome=="pneumonia") {
          finaldata<-lapply(mydata3,pneumonia)
          finaldata2<-do.call("rbind",finaldata)
        }
        finaldata3<-subset(finaldata2,select=c(2,7))
        colnames(finaldata3)<-c("hospital","state")
        finaldata3[colnames(finaldata3)]
}
