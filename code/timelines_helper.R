### Function for residential register

# Function that determines a preferred sequence of moves from of observations
## where some can be treated as certain observations of the family
## and others might be the result of adult children moving by themselves

## dc: deletion cost, i.e. the cost of an independent adult migration
### deletion because we remove that location from the family timeline
## tc: transition cost, i.e. the cost of the whole family migrating
### transition because we keep the new location in the timeline, requiring a move
## I made transition a bit more costly than deletion so that in a situation where
### independent migration and family migration seem equally likely, the independent migration
### scenario is preferred
min_cost_transformation <- function(seq, del, dc = 1, tc = 1.01) {
  
  n <- length(seq)
  
  if (n == 0 | length(table(seq)) == 1 | sum(del == 1) == 0) {return(list(min_cost = 0, optimal_sequence = rep(1,n)))}
  
  # Some occurrences can be deleted, others cannot
  seq_simple <- del * seq
  seq_simple <- seq_simple[!seq_simple == 0]
  
  ns <- length(seq_simple)
  
  lowest_cost <- 100000
  best_seq <- rep(1,n)
  
  last_c <- seq[1] 
  
  l <- expand.grid(rep(list(1:0), ns)) %>% 
    rev() %>% 
    `colnames<-`(paste0("Var",which(del == 1)))
  
  static <- paste0("Var",which(del == 0))
  
  l <- data.frame(matrix(data=1,nrow=nrow(l),ncol=length(static))) %>% 
    `colnames<-`(static) %>% 
    cbind(l) %>% 
    select(str_sort(names(.),numeric=T))
  
  # loop through possible sequences to determine the least "costly" i.e. the most likely
  for (r in 1:nrow(l)) {
    
    row <- as.numeric(l[r,])
    
    cost_del <- sum(row==0)*dc
    
    if (cost_del > lowest_cost) {next}
    
    result <- row * seq
    result <- result[!result == 0]
    
    cost_trans <- 0
    total_cost <- cost_del
    
    if (length(result) == 0 | length(table(result)) == 1) {
      
      if (total_cost < lowest_cost) {
        lowest_cost <- total_cost
        best_seq <- row
      }
      next
    }
    
    last_c <- result[1] 
    flag_next = 0
    
    for (i in 2:length(result)) {
      
      curr_c <- result[i]
      if (curr_c != last_c) {total_cost <- total_cost + tc}
      last_c <- curr_c
      
      if (total_cost >= lowest_cost) {
        flag_next = 1
        break
      }
    }
    
    if (flag_next == 1) {next}
    
    if (total_cost < lowest_cost) {
      lowest_cost <- total_cost
      best_seq <- row
    }
  }
  
  # Return both the minimum cost and the optimal sequence
  return(list(min_cost = lowest_cost, optimal_sequence = best_seq))
}

# Example usage
# sequence <- "ABCABC"
# deletion_cost <- 1
# transition_cost <- 1.1
# result <- min_cost_transformation(sequence, deletion_cost, transition_cost)
# print(paste("Minimum Cost:", result$min_cost))
# print(paste("Optimal Sequence:", result$optimal_sequence))
