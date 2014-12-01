# Question 17

rm (list = ls())
external_iterations = 1
internal_iterations = 100
n_pts = 100
n_pts_out = 200
k_num = 9



percent_rbf_beats_svm = data.frame(pct_svm_beats=numeric())

for (iter_ext in c(1:external_iterations)){
  
  result_df = data.frame(k=numeric(), has_empty=logical(), E_in_1 = numeric(), E_in_2 = numeric(),
             E_out_1 = numeric(), E_out_2 = numeric())
  
  print (paste0("Run ", iter_ext, " ************* "))
  
  for (iter in c(1:internal_iterations)){
    x0 = rep(1, n_pts)
    x1 = runif(n_pts, -1, 1)
    x2 = runif(n_pts, -1, 1)
    y_values = sign(x2 - x1 + 0.25*sin(pi*x1))
    
    x0_out = rep(1, n_pts_out)
    x1_out = runif(n_pts_out, -1, 1)
    x2_out = runif(n_pts_out, -1, 1)
    y_values_out = sign(x2_out - x1_out + 0.25*sin(pi*x1_out))
    
    lloyd_df = data.frame(x1=x1, x2=x2)
    lloyd_df_out = data.frame(x1_out=x1_out, x2_out=x2_out)
    

    
    #K-Means
    
    x1_init_cluster = runif(k_num, -1, 1)
    x2_init_cluster = runif(k_num, -1, 1)
    
    km = kmeans(lloyd_df, centers = k_num, algorithm = "Lloyd", nstart = k_num, iter.max=100)
    if (0 %in% km$size) {
      has_empty=TRUE
    } else {
      has_empty=FALSE
    }
    
    # Segment 1 - gamma_val = 1.5
    gamma_val = 1.5

    new_phi_values = data.frame(bias=rep(1, n_pts))
    new_phi_values_out = data.frame(bias=rep(1, n_pts_out))
    
    for (i_row in c(1:nrow(km$centers))) {
      mu_matrix = matrix(rep(km$centers[i_row,], each=n_pts), ncol=2, nrow=n_pts)
      temp_mat = lloyd_df - mu_matrix
      phi_values = apply(temp_mat, 1, function(x) exp(-1*gamma_val*norm(as.matrix(x), "f")^2))
      new_phi_values = cbind(new_phi_values, phi_values)
    }
    
    for (i_row in c(1:nrow(km$centers))) {
      mu_matrix_out = matrix(rep(km$centers[i_row,], each=n_pts_out), ncol=2, nrow=n_pts_out)
      temp_mat_out = lloyd_df_out - mu_matrix_out
      phi_values_out = apply(temp_mat_out, 1, function(x) exp(-gamma_val*norm(as.matrix(x), "f")^2))
      new_phi_values_out = cbind(new_phi_values_out, phi_values_out)
    }
    
    
    phi_mat = as.matrix(new_phi_values)
    w = solve(t(phi_mat) %*% phi_mat) %*% t(phi_mat) %*% y_values
    
    y_preds_in = sign(phi_mat %*% w)
    correct_in = sum(y_preds_in != y_values)/n_pts
        
    phi_mat_out = as.matrix(new_phi_values_out)
    y_preds_out = sign(phi_mat_out %*% w)
    correct_out = sum(y_preds_out != y_values_out)/n_pts_out

    correct_in_1  = correct_in
    correct_out_1 = correct_out

    # Segment 2 - gamma_val = 2
    gamma_val = 2
    
    new_phi_values = data.frame(bias=rep(1, n_pts))
    new_phi_values_out = data.frame(bias=rep(1, n_pts_out))
    
    for (i_row in c(1:nrow(km$centers))) {
      mu_matrix = matrix(rep(km$centers[i_row,], each=n_pts), ncol=2, nrow=n_pts)
      temp_mat = lloyd_df - mu_matrix
      phi_values = apply(temp_mat, 1, function(x) exp(-1*gamma_val*norm(as.matrix(x), "f")^2))
      new_phi_values = cbind(new_phi_values, phi_values)
    }
    
    for (i_row in c(1:nrow(km$centers))) {
      mu_matrix_out = matrix(rep(km$centers[i_row,], each=n_pts_out), ncol=2, nrow=n_pts_out)
      temp_mat_out = lloyd_df_out - mu_matrix_out
      phi_values_out = apply(temp_mat_out, 1, function(x) exp(-gamma_val*norm(as.matrix(x), "f")^2))
      new_phi_values_out = cbind(new_phi_values_out, phi_values_out)
    }
    
    
    phi_mat = as.matrix(new_phi_values)
    w = solve(t(phi_mat) %*% phi_mat) %*% t(phi_mat) %*% y_values
    
    y_preds_in = sign(phi_mat %*% w)
    correct_in = sum(y_preds_in != y_values)/n_pts
    
    phi_mat_out = as.matrix(new_phi_values_out)
    y_preds_out = sign(phi_mat_out %*% w)
    correct_out = sum(y_preds_out != y_values_out)/n_pts_out
    
    correct_in_2  = correct_in
    correct_out_2 = correct_out
    # End Repeat Segment
    
    result_df = rbind(result_df,
                      data.frame(k=k_num, has_empty=has_empty, E_in_1 = correct_in_1, E_in_2 = correct_in_2,
                                 E_out_1 = correct_out_1, E_out_2 = correct_out_2))
        
    print(paste0("Completed Iteration ", iter))
    
  }
  
  #mean(result_df$correct_out)
  result_df2 <- result_df[result_df$has_empty == FALSE,]
  rows_result_df2 <- nrow(result_df2)
  result_df2  
}

result_df2$E_in_diff = result_df2$E_in_2 < result_df2$E_in_1
result_df2$E_out_diff = result_df2$E_out_2 < result_df2$E_out_1

table(result_df2$E_in_diff)
table(result_df2$E_out_diff)


