rankhospital <- function(state = "NY", outcome = "pneumonia", num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        r.outcome <- c("heart attack", "heart failure", "pneumonia") #correct set
        par <- function(dat){
                #this will give the vector of columns to retrieve
                pairs <-list(c(2,11), c(2,17), c(2,23))
                a<- r.outcome == dat
                for(i in 1:3) {
                        if(a[i] & !is.na(a[i])) {
                                res<-unlist(pairs[i])
                                break
                        }
                }
                res
        }
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        st<- data$State[!duplicated(data$State)] #this will get you the list of states
        # this indented lines are the argument validation
                if (length(which(state == st)) == 0) {
                        stop("Invalid state")
                } else {
                        print("only valid states will be considered")
                        state <- st[which(state == st)] #subset of correct states
                }
                if (length(which(outcome == r.outcome)) == 0){
                        stop("Invalid outcome")
                } else {
                        print("only valid outcomes will be considered")
                        outcome <- r.outcome[which(outcome == r.outcome)] #subset of correct outcomes
                }
     
        
        c.data <- subset(data, State == state, select = par(outcome)) #subset of data
        c.data[,2] <- as.numeric(c.data[,2]) # turn the set numeric
        c.data <- c.data[order(c.data$Hospital.Name, na.last = NA),] # sorting
        #up to here we just have a sorted clean set of data
        c.data <- cbind(c.data[ !is.na(c.data[2]) == T, ], "rank"= rank(c.data[2], na.last = NA, ties.method = "first")) #added column with rank
        # now we validate num
                if (num == "best" ){
                        c.data[min(c.data[3]) == c.data[3], 1] # gives the best/ min rank
                } else if (num == "worst") {
                        c.data[max(c.data[3]) == c.data[3], 1] # gives the worst / max rank
                } else if (is.numeric(num)) {
                        # we verify is within range
                        limit <- max(c.data[3])
                        if (limit < num) return(NA)
                        c.data[num == c.data[3], 1]
                } else {stop ("invalid argument, best, worst or number is expected")}
}