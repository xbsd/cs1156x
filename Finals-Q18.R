# Question 18

rm (list = ls())
external_iterations = 1
internal_iterations = 100
n_pts = 100
n_pts_out = 200
k_num = 9

for (iter_ext in c(1:external_iterations)){
  
  result_df = data.frame(k=numeric(), has_empty=logical(), E_in_1 = numeric())
  
  print (paste0("Run ", iter_ext, " ************* "))
  
  for (iter in c(1:internal_iterations)){
    x0 = rep(1, n_pts)
    x1 = runif(n_pts, -1, 1)
    x2 = runif(n_pts, -1, 1)
    y_values = sign(x2 - x1 + 0.25*sin(pi*x1))

    lloyd_df = data.frame(x1=x1, x2=x2)
    
    #K-Means
    
    km = kmeans(lloyd_df, centers = k_num, algorithm = "Lloyd", nstart = k_num, iter.max=100)
    if (0 %in% km$size) {
      has_empty=TRUE
    } else {
      has_empty=FALSE
    }
    
    # Segment 1 - gamma_val = 1.5
    gamma_val = 1.5
    
    new_phi_values = data.frame(bias=rep(1, n_pts))
    
    for (i_row in c(1:nrow(km$centers))) {
      mu_matrix = matrix(rep(km$centers[i_row,], each=n_pts), ncol=2, nrow=n_pts)
      temp_mat = lloyd_df - mu_matrix
      phi_values = apply(temp_mat, 1, function(x) exp(-1*gamma_val*norm(as.matrix(x), "f")^2))
      new_phi_values = cbind(new_phi_values, phi_values)
    }
    
    
    phi_mat = as.matrix(new_phi_values)
    w = solve(t(phi_mat) %*% phi_mat) %*% t(phi_mat) %*% y_values
    
    y_preds_in = sign(phi_mat %*% w)
    correct_in = sum(y_preds_in != y_values)/n_pts    
    correct_in_1  = correct_in
       
    result_df = rbind(result_df,
                      data.frame(k=k_num, has_empty=has_empty, E_in_1 = correct_in_1))
    
    print(paste0("Completed Iteration ", iter))
    
  }
  
  #mean(result_df$correct_out)
  result_df2 <- result_df[result_df$has_empty == FALSE,]
  result_df2  
}

mean(result_df2$E_in_1 == 0.00)





