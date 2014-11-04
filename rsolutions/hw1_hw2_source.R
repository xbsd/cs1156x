## Function to generate the data (by Chaoping)
data.generate = function(n = 10, ext = 1){ 
  # Generate the points.
  x1 = runif(n, -ext, ext)
  x2 = runif(n, -ext, ext)
  
  # Draw a random line in the area.
  point = runif(2, -ext, ext)
  point2 = runif(2, -ext, ext)
  slope = (point2[2] - point[2]) / (point2[1] - point[1])
  intercept = point[2] - slope * point[1]
  
  # Assign the dependent values.
  y = as.numeric(x1 * slope + intercept > x2) * 2 - 1
  
  # Return the values.
  data = data.frame(x1,x2,y)
  return(list(data = data,slope = slope, intercept = intercept))
}  


##### PLA  #####

# Help function to generate the test points (I used 10000 test points)

data.gen2 <- function(n=10000){
  x1 = runif(n, -1, 1)
  x2 = runif(n, -1, 1)
  data <- data.frame(x1,x2)
  data
}


iterations <- numeric(0)    # initialising the iterations and misclassification probability vectors
probability <- numeric(0)

# Setting up the runs and the algorithm

for (i in 1:2){
  generated  <-  data.generate(n=10)    # generating points (set n=10 or n=100) and target function
  input  <-  as.matrix(cbind(1, generated$data[c(1,2)])) # creating the input matrix
  
  w  <-  c(0,0,0)  # initializing the weights
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
  #cat ("Number of iterations to converge: ", k, "\n")
  iterations[i] <- k-1 # store the number of iterations needed in each run
  
  new.data <- data.gen2()  #  generating the test points for examining out-of-sample performance
  f  <-  as.numeric(new.data$x1 * generated$slope + generated$intercept > new.data$x2) * 2 - 1  # classifying points according to the true function f
  g  <-  as.numeric(new.data$x1 * (-w[2]/w[3]) - w[1]/w[3] > new.data$x2) * 2 - 1  # classifying points according to the hypothesised function g, using the 
  # final weights provided by PLA            
  
  probability[i] <- sum(f!=g)/10000  # store the misclassification errors from each run
}

# Main results: average of iterations and estimated misclassification probability
mean(iterations)
mean(probability)


# Function to plot the points and f and g functions from  one iteration

library(ggplot2)

qplot(x1,x2,col= as.factor(y), data = generated$data) + 
  geom_abline(intercept = generated$intercept, slope = generated$slope) +
  geom_abline(intercept = -w[1]/w[3], slope = -w[2]/w[3], col=3)

# qplot(x1,x2,col= as.factor(y), data = generated$data) + 
#   geom_abline(intercept = generated$intercept, slope = generated$slope)


#g  <-  as.numeric(new.data$x1 * (-w[2]/w[3]) - w[1]/w[3] > new.data$x2) * 2 - 1  # classifying points according to the hypothesised function g, using the 



