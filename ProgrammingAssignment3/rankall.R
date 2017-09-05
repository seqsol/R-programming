rankall <- function(outcome, num="best") {
    all_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(tolower(outcome) =="heart attack") {
        col_no = 11
    } else if (tolower(outcome) == "heart failure"){
        col_no = 17
    } else if ( tolower(outcome) == "pneumonia") {
        col_no = 23
    } else {
        stop("invalid outcome")
    }
    
    states_data <- split( all_data, as.factor(all_data$State))
    res <- lapply(states_data, function(s) {
        s2 <- s[which(s[, col_no] != "Not Available"), ]
        # sort the data based on the values in col_no
        s2_ordered <- s2[order(as.numeric(s2[, col_no]), s2[, 2]), ]
        # Get the hospital names that has the ranking specified by num
        if(num=="best"){
            s2_ordered[1, 2]
        } else if(num == "worst"){
            s2_ordered[nrow(s2_ordered), 2]
        } else {
            s2_ordered[num, 2]
        }
    })
    data.frame(hospital=sapply(res, function(x) x[[1]]), state=names(res))
}