
source("hw1_hw2_source.R")

num_pts = 1000
num_iterations = 1000

e_in = 0
generated = data.generate(n=num_pts)

for (i in 1:num_iterations) {
  

new_mat = as.matrix(cbind(x0=1, generated$data[,1:2]))  
f_y = sign(new_mat[,2]**2 + new_mat[,3]**2 - 0.6) # Uses custom f(x) per Q8

noise_indices = sample(num_iterations, 0.10*num_iterations)
f_y[noise_indices] = f_y[noise_indices]*-1

mat_inv = solve((t(new_mat) %*% new_mat)) %*% t(new_mat)  
w = mat_inv %*% generated$data[,3]
h_y = as.vector(sign(t(w) %*% t(new_mat)))

e_in = e_in + sum(f_y != h_y) / num_pts

}

average_e_in = e_in/num_iterations

cat ("Average e in", average_e_in)
