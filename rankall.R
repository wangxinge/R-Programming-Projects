# rankall
rankall<-function(outcome,num="best"){
        ## Read outcome data
        data_origin<-read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state and outcome are valid
        col<-{if(outcome=="heart attack"){11}
                else if(outcome=="heart failure"){17}
                else if(outcome=="pneumonia"){23}
                else stop("invalid outcome")}
        
        ## For each state, find the hospital of the given rank
        states<-unique(data_origin$State)
        output<-vector()
        for(i in 1:length(states)){
                state_name<-states[i]
                row_id<-which(data_origin$State==state_name)
                rank_data<-suppressWarnings(as.numeric(data_origin[row_id,col]))
                order<-order(rank_data,data_origin[row_id,2],na.last=NA)
                id<-row_id[order]
                orderdata<-data_origin[id,]
                hospital<- if(num=="best"){
                        orderdata[1,2]
                } else if(num=="worst"){
                        orderdata[nrow(orderdata),2]
                } else{
                        orderdata[num,2]
                }
                output<-append(output,c(hospital,states[i]))
        }
        ## Return a data frame with the hospital names and the state name
        output_frame <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(output_frame)<-c("hospital","state")
        rownames(output_frame)<-output_frame[,2]
        output_frame
}