rankhospital<-function(state, outcome, num="best"){
    
    all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!(state %in% all_data[,7])){
        stop("invalid state")
    } else if(tolower(outcome) =="heart attack") {
        col_no = 11
    } else if (tolower(outcome) == "heart failure"){
        col_no = 17
    } else if ( tolower(outcome) == "pneumonia") {
        col_no = 23
    } else {
        stop("invalid outcome")
    }
    
    ## Retrieve data of the state specified by argument 'state'
    s_data <- all_data[which(all_data[,7]==state),]
    ## Remove rows that do not have data in the column specified by argument 'outcome' 
    s_data2 <- s_data[which(s_data[,col_no]!="Not Available"),]

    # sort the data based on the values in col_no
    s_data2_ordered <- s_data2[order(as.numeric(s_data2[,col_no]), s_data2[,2]), ]
    # Get the hospital names that has the ranking specified by num
    if(num == "best"){
        s_data2_ordered[1, 2] 
    } else if(num == "worst"){
        s_data2_ordered[nrow(s_data2_ordered), 2]
    } else{
        s_data2_ordered[num, 2]
    }
}