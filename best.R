#
#Step one: read outcome data
# Read.csv()
# 
# Step two: Check that state and outcome are valid
# 
# We can establish a variable name=c("heart attack", "heart failure", "pneumonia") to check if "outcome" is one of them. If (length(which(name==outcome))==0) If true stop(). The same with "state"
# 
# Step three: Return hospital name in that state with lowest 30-day death
# 
# When we extract from data with "outcome" in the "state", use min(). 


best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}