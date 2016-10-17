# Function to find the best hospital in US
best<-function(state,outcome){
        ## Read outcome data
        data_origin<-read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        ## Check that state and outcome are valid
        col<-{if(outcome=="heart attack"){11}
        else if(outcome=="heart failure"){17}
        else if(outcome=="pneumonia"){23}
                else stop("invalid outcome")}
        if(sum(data_origin$State==state)==0)
                stop("invalid state")
        #Return hospital name in that state with lowest 30-day death
        row_id<-which(data_origin$State==state)
        rank_data<-suppressWarnings(as.numeric(data_origin[row_id,col]))
        id<-which(rank_data==min(rank_data,na.rm = T))
        id<-row_id[id]
        if(length(id)>1){
                sort<-sort(data_origin$Hospital.Name[id])
                return
                sort[1]
        }
        else{
        return 
        data_origin$Hospital.Name[id]
}
        }