
source("hw1_hw2_source.R")

num_pts = 1000
num_iterations = 1000

final_result = 0
w = 0
e_in = 0
e_out = 0

result_matrix = 0

generated = data.generate(n=num_pts)

for (i in 1:num_iterations) {
  
  new_mat = as.matrix(cbind(x0=1, generated$data[,1:2]))  
  f_y = sign(new_mat[,2]**2 + new_mat[,3]**2 - 0.6) # Uses custom f(x) per Q8
  noise_indices = sample(num_pts, 0.10*num_pts)
  f_y[noise_indices] = f_y[noise_indices]*-1
  
  trans_mat = as.matrix(cbind(x0 = 1, x1 = new_mat[,2], x2 = new_mat[,3], 
                              x1x2 = new_mat[,2]*new_mat[,3], x1sq = new_mat[,2]**2, 
                              x2sq = new_mat[,3]**2))
  
  mat_inv = solve((t(trans_mat) %*% trans_mat)) %*% t(trans_mat)
  w_new = mat_inv %*% f_y
  
  w = w + w_new
  
  h_y = as.vector(sign(t(w_new) %*% t(trans_mat))) 
     
  e_in = e_in + sum(f_y != h_y) / num_pts
  
  # Generate new data
  generated2 = data.generate(n=num_pts)
  new_mat = as.matrix(cbind(x0=1, generated2$data[,1:2]))  
  f_y = sign(new_mat[,2]**2 + new_mat[,3]**2 - 0.6) # Uses custom f(x) per Q8
  noise_indices = sample(num_pts, round(0.10*num_pts))
  f_y[noise_indices] = f_y[noise_indices]*-1
  
  trans_mat2 = as.matrix(cbind(x0 = 1, x1 = new_mat[,2], x2 = new_mat[,3], 
                              x1x2 = new_mat[,2]*new_mat[,3], x1sq = new_mat[,2]**2, 
                              x2sq = new_mat[,3]**2))
  
  h_y = as.vector(sign(t(w_new) %*% t(trans_mat2))) 
  
  dt = data.table(trans_mat2)
  opt_a = sign(-1 - 0.05 * dt$x1 + 0.08 * dt$x2 + 0.13 * dt$x1x2 + 1.5 * dt$x1sq + 1.5 * dt$x2sq)
  opt_b = sign(-1 - 0.05 * dt$x1 + 0.08 * dt$x2 + 0.13 * dt$x1x2 + 1.5 * dt$x1sq + 15 * dt$x2sq)
  opt_c = sign(-1 - 0.05 * dt$x1 + 0.08 * dt$x2 + 0.13 * dt$x1x2 + 15 * dt$x1sq + 1.5 * dt$x2sq)
  opt_d = sign(-1 - 1.5 * dt$x1 + 0.08 * dt$x2 + 0.13 * dt$x1x2 + 0.05 * dt$x1sq + 0.05 * dt$x2sq)
  opt_e = sign(-1 - 0.05 * dt$x1 + 0.08 * dt$x2 + 1.5 * dt$x1x2 + 0.15 * dt$x1sq + 0.15 * dt$x2sq)
  
  option_matrix = rbind(opt_a, opt_b, opt_c, opt_d, opt_e)
  
  h_y_matrix = matrix(rep(h_y,5), nrow=5, ncol=num_pts, byrow=T)
  option_matrix_tf = option_matrix == h_y_matrix
  result_matrix = apply(option_matrix_tf, 1, function(x) sum(x)/num_pts)
  
  final_result = final_result + result_matrix
  
  e_out = e_out + sum(f_y != h_y) / num_pts
  
}

final_result/num_iterations
w/num_iterations
e_in/num_iterations
e_out/num_iterations

print ("Option a has ~ 95% agreement with hypothesis")