##function to find the best hospital in state

best <- function(state, outcome){
  
        ##Read outcome data
        outcomeData<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        if (all(outcomeData$State!=state)) stop("Invalid State")

        ##Assign Column number for outcome
        colNum<-list("heart attack"=11,"heart failure"=17,"pneumonia"=23)

        if (is.null(colNum[[outcome]])) stop("Invalid outcome")
        
        ##subset the required data
        reqData<-subset(outcomeData,State==state,select=c(2,colNum[[outcome]]))

        ##convert character data to numeric
        suppressWarnings(reqData[,2]<-as.numeric(reqData[,2]))
        
        ##Assign rank to data
        rankHotel<-rank(reqData[,2],na.last=TRUE,ties.method="min")
        
        ##pick hotels with min Rank
        finalList<-reqData[min(rankHotel)==rankHotel,]
        
        ##sort hotel list by alphabet
        sortFinal<-finalList[order(finalList[,1]),]
        
        ##result hotel name
        hotelName<-sortFinal[1,1]
        
        return(hotelName)
}

