## function to return hospital name for particular state by given rank
returnhospital <- function(state, outcomeCol,num="best"){
  
  ## Read outcome data
  outcomeData<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ## subset the required data
  reqData<-subset(outcomeData,State==state,select=c(7,2,outcomeCol))
  
  ## convert character data to numeric
  suppressWarnings(reqData[,3]<-as.numeric(reqData[,3]))
  
  ## exclude hospital that do not have data on particular outcome
  reqData<-subset(reqData,!is.na(reqData[,3]))
  
  suppressWarnings(if ((num!="best") & (num!="worst") & (as.numeric(num)>nrow(reqData))) return("NA"))
  
  # order the data
  reqData<-reqData[order(reqData[,1], reqData[,3],reqData[,2]),]
  nr<-nrow(reqData)
  
  Rank<-1:nr
  
  ## Assign rank to data
  reqData<-cbind(reqData,Rank)
  
  if (num=="best"){
    num=min(Rank)
  } else if (num=="worst"){
    num=max(Rank)
  }
  
  ## result hotel name
  hotelName<-reqData[reqData$Rank==num,2]
  
  return(hotelName)
}

## function to return best hospital for given state
best<-function (state, outcome){
  ##Read outcome data
  readData<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ##Assign Column number for outcome
  colNum<-list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  ##check validity of outcome argument
  if (is.null(colNum[[outcome]])) stop("Invalid outcome")
  
  ## check validity of state argument
  if (all(readData$State!=state)) stop("Invalid State")
  
  ##retrun hosptial by state
  df<-returnhospital(state,colNum[[outcome]],"best")
  
  return(df)
}

## function to return hospital name for given ranking and state
rankhospital<-function (state, outcome,num="best"){
  ##Read outcome data
  readData<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ##Assign Column number for outcome
  colNum<-list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  ##check validity of outcome argument
  if (is.null(colNum[[outcome]])) stop("Invalid outcome")
  
  ## check validity of state argument
  if (all(readData$State!=state)) stop("Invalid State")
  
  ##retrun hosptial by state
  df<-returnhospital(state,colNum[[outcome]],num)
  
  return(df)
}


## function to return hospital from all states with given ranking
rankall<-function (outcome,num="best"){
  ##Read outcome data
  readData<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  ##Assign Column number for outcome
  colNum<-list("heart attack"=11,"heart failure"=17,"pneumonia"=23)
  
  #check validity of outcome argument
  if (is.null(colNum[[outcome]])) stop("Invalid outcome")
  
  states<-sort(as.vector(unique(readData[,7])))
  df<-data.frame(row.names=1:length(states),stringsAsFactors=FALSE)
  ##n=states[4]
  for (n in states){
    df<-rbind(df,data.frame(hospital=rankhospital(n,outcome,num),state=n))
  }
  row.names(df)<-states
  #df<-df[order(df$state),]
  return(df)
}
