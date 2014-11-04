
source("hw1_hw2_source.R")
num_pts = 100
num_iterations = 1000
e_in = 0
generated = data.generate(n=num_pts)


for (i in 1:num_iterations) {
  generated = data.generate(n=num_pts)
  
  new_mat = as.matrix(cbind(x0=1, generated$data[,1:2]))
  
  mat_inv = solve((t(new_mat) %*% new_mat)) %*% t(new_mat)  
  w = mat_inv %*% generated$data[,3]
  h_y = as.vector(sign(t(w) %*% as.matrix(t(as.matrix(new_mat)))))
  
  e_in = e_in + sum(generated$data$y != h_y) / num_pts
  
}

average_e_in = e_in / num_iterations

probability <- numeric(0)

test_points = 1000

for (k in 1:num_iterations){
new.data <- data.gen2(n = test_points)  #  generating the test points for examining out-of-sample performance
f  <-  as.numeric(new.data$x1 * generated$slope + generated$intercept > new.data$x2) * 2 - 1  # classifying points according to the true function f
g  <-  as.numeric(new.data$x1 * (-w[2]/w[3]) - w[1]/w[3] > new.data$x2) * 2 - 1  # classifying points according to the hypothesised function g, using the 
# final weights provided by PLA            

probability[k] <- sum(f!=g)/test_points  # store the misclassification errors from each run
}

cat ("Answer 5: Average e_in is", average_e_in, "and average e_out is", mean(probability))




