
source("hw1_hw2_source.R")

num_pts = 10
# Setting up the runs and the algorithm

for (i in 1:1000){
  generated = data.generate(n=num_pts)
  
  # Use linear regression to find w
  new_mat = as.matrix(cbind(x0=1, generated$data[,1:2]))  
  mat_inv = solve((t(new_mat) %*% new_mat)) %*% t(new_mat)
  w = mat_inv %*% generated$data[,3] # Weights initialized
  
  #w <- c(0,0,0)
  
  # Back to PLA
  input  <-  as.matrix(cbind(1, generated$data[c(1,2)])) # creating the input matrix
  res <- apply(input,1,function(x) t(w)%*%x)  # multiplying transpose of w with each row of input matrix 
  
  k <- 1  # initializing the iterations
  
  while (any(sign(res)!=generated$data$y))    # as long as the sign of all elements of res vector differ from the true 
  {                                           # output y, we perform the PLA algorithm below
    #cat("Iteration:", k, "\n")
    mis <- which(sign(res)!=generated$data$y)  # getting the points for which hypotesis is wrong
    ifelse (length(mis)==1, n <- mis, n <- sample(which(sign(res)!=generated$data$y),1))  # randomly sample one of these points
    w <- w + generated$data$y[n]*input[n,]  # apply PLA, get new weights
    res <- apply(input,1,function(x) t(w)%*%x)  # use new weights to get the new res vector
    k <- k+1  # increase the iteration count
  }
  cat ("Number of iterations to converge: ", k, "\n")
  iterations[i] <- k-1 # store the number of iterations needed in each run
}

# Main results: average of iterations required to converge
mean(iterations)

