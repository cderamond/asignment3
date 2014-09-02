rankall <- function(outcome = "heart attack", num = "best") {
		## Read outcome data
		## Check that state and outcome are valid
		## For each state, find the hospital of the given rank
		## Return a data frame with the hospital names and the
		## (abbreviated) state name
        r.outcome <- c("heart attack", "heart failure", "pneumonia") #correct set
        par <- function(dat){
                #this will give the vector of columns to retrieve
                pairs <-list(c(2, 7, 11), c(2, 7, 17), c(2, 7, 23))
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
        #st<- data$State[!duplicated(data$State)] #this will get you the list of states
        # this indented lines are the argument validation
                if (length(which(outcome == r.outcome)) == 0){
                        stop("Invalid outcome")
                } else {
                        print("only valid outcomes will be considered")
                        outcome <- r.outcome[which(outcome == r.outcome)] #subset of correct outcomes
                }
        c.data <- subset(data, , select = par(outcome)) #subset of data
        c.data[,3] <- as.numeric(c.data[,3]) # turn the set numeric
        c.data <- c.data[!is.na(c.data[3]), ] # erase NAs
        c.data <- split(c.data, c.data[2]) # split 
        c.data <- lapply(c.data, function(x) x <- cbind(x, "rank" = rank(x[3], na.last = NA, ties.method = "first"))) # it ranks and adds the rank column per state
        #c.data 
        if (num == "best" ){
                c.data <- lapply(c.data, function(x) subset(x, x[4] == min(x$rank), 1:2)) # gives the best/ min rank
                c.data <- do.call(rbind, c.data)
                names(c.data) <- c("hospital", "state")
                c.data
        } else if (num == "worst") {
                c.data <- lapply(c.data, function(x) subset(x, x[4] == max(x$rank), 1:2)) # gives the worst / max rank
                c.data <- do.call(rbind, c.data)
                names(c.data) <- c("hospital", "state")
                c.data
        } else if (is.numeric(num)) {
                # we verify is within range
                #limit <- max(c.data[3])
                #if (limit < num) return(NA)
                c.data <- lapply(c.data, function(x) subset(x, x[4] == num, 1:2)) # take the specific one
                c.data <- do.call(rbind, c.data)
                names(c.data) <- c("hospital", "state")
                c.data
        } else {stop ("invalid argument, best, worst or number is expected")}
        
}