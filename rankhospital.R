# rankhospital
rankhospital<-function(state,outcome,num="best"){
        ## Read outcome data
        data_origin<-read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        col<-{if(outcome=="heart attack"){11}
                else if(outcome=="heart failure"){17}
                else if(outcome=="pneumonia"){23}
                else stop("invalid outcome")}
        if(sum(data_origin$State==state)==0)
                stop("invalid state")
        
        ## Return hospital name in that state with the given rank
        row_id<-which(data_origin$State==state)
        rank_data<-suppressWarnings(as.numeric(data_origin[row_id,col]))
        order<-order(rank_data,data_origin[row_id,2],na.last=NA)
        id<-row_id[order]
        orderdata<-data_origin[id,]
        if(num=="best"){
                orderdata[1,2]
        } else if(num=="worst"){
                orderdata[nrow(orderdata),2]
        } else{
                orderdata[num,2]
        }
}