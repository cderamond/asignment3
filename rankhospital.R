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
                if (num == "best" || num == "worst") {
                        #maybe something
                } else if (is.numeric(num)) {
                        # we verify is within range
                } else {stop ("invalid argument, best, worst or number is expected")}
        
        c.data <- subset(data, State == state, select = par(outcome)) #subset of data
        c.data[,2] <- as.numeric(c.data[,2]) # turn the set numeric
        c.data <- c.data[order(c.data$Hospital.Name, na.last = NA),] # sorting
        #up to here we just have a sorted clean set of data
        

}