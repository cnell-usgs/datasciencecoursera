setwd("/Users/colleennell/Documents/R/assn3")
library(dplyr)

outcomes<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
hospital<-read.csv("hospital-data.csv")

##finding the best hospital in a state
best<-function(state,outcome){ 
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")##read in dataset
  if(state %in% outcome.data$State ==FALSE){
    stop("invalid state")  ##check state and outcome are valid
  }
  if(outcome %in% c("pneumonia","heart attack","heart failure")==FALSE){
    stop("invalid outcome")
  }
  if(state %in% outcome.data$State & outcome %in% c("pneumonia","heart attack","heart failure")){
    state.data<-filter(data, State == state) ##filter to state input
    if(outcome == "pneumonia"){ ##filter data to contain hospital name, and outcome data
      outcome.data<-state.data[,c(2,23)]
    }
    if(outcome == "heart attack"){
      outcome.data<-state.data[,c(2,11)]
    }
    if(outcome == "heart failure"){
      outcome.data<-state.data[,c(2,17)]
    }
    ##return hospital name in that sate with lowest rate
    outcome.data[,2]<-suppressWarnings(as.numeric(outcome.data[,2]))
    rank.rate<-outcome.data[order(outcome.data[,2],outcome.data[,1]),]
    best<-paste(rank.rate$Hospital.Name[1])
  }
  best
}
 
##ranking hospitals by outcome in a state
#returns character vector with the name of the hospital than ranks as specified by num in selected state
rankhospital<-function(state, outcome, num){
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")##read in dataset
  if(state %in% outcome.data$State ==FALSE){
    stop("invalid state")  ##check state and outcome are valid
  }
  if(outcome %in% c("pneumonia","heart attack","heart failure")==FALSE){
    stop("invalid outcome")
  }
  if(state %in% outcome.data$State & outcome %in% c("pneumonia","heart attack","heart failure")){
    state.data<-filter(data, State == state) ##filter to state input
    if(outcome == "pneumonia"){ ##filter data to contain hospital name, and outcome data
      outcome.data<-state.data[,c(2,23)]
    }
    if(outcome == "heart attack"){
      outcome.data<-state.data[,c(2,11)]
    }
    if(outcome == "heart failure"){
      outcome.data<-state.data[,c(2,17)]
    }
  }
  outcome.data[,2]<-suppressWarnings(as.numeric(outcome.data[,2]))
  rank.rate<-outcome.data[order(outcome.data[,2],outcome.data[,1],na.last=T),]
  rank.rate[rank.rate=="Not Available"] = NA
  rank.rate<-na.omit(rank.rate)
  if(num=="best"){
    pick<-1
  }
  if(num=="worst"){
    pick<-length(rank.rate[,2])
  }
  if(is.numeric(num)){
    if(num > length(rank.rate[,2])){
      stop("NA")
    }
    else{pick<-num
    }
  }
  
  ranking<-paste(rank.rate$Hospital.Name[pick])
  ranking
  
}

###ranking hospitals in all states

##write a function that returns a dataframe with

rankall<-function(outcome, num = "best"){
  data<-read.csv("outcome-of-care-measures.csv",colClasses = "character")##read outcome data
  
  if(outcome %in% c("pneumonia","heart attack","heart failure")==FALSE){
    stop("invalid outcome")##check that state, num, and outcome are valid
  }

  if(state %in% data$State ==FALSE){
    stop("invalid state") 
  }
  out<-ifelse(outcome == "pneumonia",23, ifelse(outcome == "heart attack",11,17))
  out.data<-data[,c(2,7,out)]
  out.data$out<-suppressWarnings(as.numeric(out.data$out))
  
  rank.out<-out.data[order(out.data[,3],outcome.data[,1],na.last=T),]
  rank.out<-rank.out[!is.na(rank.out[,3]),]
  
  if(is.character(num)){
    if(num %in% c('best','worst')==FALSE){
      stop('invalid rank')
    }
    if(num == 'best'){
      position<-1
    }
    if(num == 'worst'){
      position<-length(rank.out)
    }
  }
  
  all.states<-sort(unique(data$State))
  
  df.out<-lapply(state.data,hosp.rank,num)
    
  }
  
  
  
  ##return a dataframe with hospital names and state at selected rank
}

ranky<-outcomes[,c(2,7,23)]
ranky[,3]<-as.numeric(ranky[,3])
View(ranky)
str(ranky)

rank.out<-ranky[order(ranky[,3],ranky[,1],na.last=T),]
rank.out<-rank.out[!is.na(rank.out[,3]),]
View(rank.out)
