
library(e1071)
library(data.table)
library(ggplot2)
library(kernlab)
setwd("~/Documents/study//edX/cs1156x/")


x1 = c(1,0,0,-1,0,0,-2)
x2 = c(0,1,-1,0,2,-2,0)
y = c(-1,-1,-1,1,1,1,1)

tr = data.frame(x1=x1, x2=x2, y=y)



model_svm <- svm(y ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                   degree = 2,
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = 1,
                   cost = 1e6)
qplot(x1,x2)
paste0("Number of Support Vectors = ", model_svm$tot.nSV)

# test = data.frame(x1=c(0.5272, 0.9523, 0.5631), x2 = c(0.8712, 0.4783, -0.4928))
# test$bias = 1
# #test$y = c(-1,1,1)
# test = as.matrix(test)
# rbf = rbfdot(sigma = 1)
# 
# kernelMatrix(rbf, x=test)
# kernelPol(rbf, x=test, ,z = as.matrix(c(-1,1,1)))

# Question 13

rm (list = ls())
iterations = 100
n_pts = 100
n_pts_out = 500
k_num = 12
gamma_val = 1.5

result_df = data.frame(k=numeric(), correct_in=numeric(), correct_out=numeric(),
                       correct_in_svm=numeric(), correct_out_svm=numeric())

for (iter in c(1:iterations)){
  x0 = rep(1, n_pts)
  x1 = runif(n_pts, -1, 1)
  x2 = runif(n_pts, -1, 1)
  y_values = sign(x2 - x1 + 0.25*sin(pi*x1))
  
  x0_out = rep(1, n_pts_out)
  x1_out = runif(n_pts_out, -1, 1)
  x2_out = runif(n_pts_out, -1, 1)
  y_values_out = sign(x2_out - x1_out + 0.25*sin(pi*x1_out))
  
  lloyd_df = data.frame(x1=x1, x2=x2)
  lloyd_df_out = data.frame(x0_out = 1, x1_out=x1_out, x2_out=x2_out)
  
  new_phi_values = data.frame(bias=rep(1, n_pts))
  new_phi_values_out = data.frame(bias=rep(1, n_pts))
  
  #K-Means
  km = kmeans(lloyd_df, centers = k_num, algorithm = "Lloyd", nstart = k_num, iter.max=100)
  
  for (i_row in c(1:nrow(km$centers))) {
    mu_matrix = matrix(rep(km$centers[i_row,], each=n_pts), ncol=2, nrow=n_pts)
    temp_mat = lloyd_df - mu_matrix
    phi_values = apply(temp_mat, 1, function(x) exp(-1*gamma_val*norm(as.matrix(x), "f")^2))
    new_phi_values = cbind(new_phi_values, phi_values)
  }
  
  for (i_row in c(1:nrow(km$centers))) {
    mu_matrix_out = matrix(rep(km$centers[i_row,], each=n_pts), ncol=2, nrow=n_pts)
    temp_mat_out = lloyd_df_out - mu_matrix_out
    phi_values_out = apply(temp_mat_out, 1, function(x) exp(-1*gamma_val*norm(as.matrix(x), "f")^2))
    new_phi_values_out = cbind(new_phi_values_out, phi_values_out)
  }
  
  
  phi_mat = as.matrix(new_phi_values)
  w = solve(t(phi_mat) %*% phi_mat) %*% t(phi_mat) %*% y_values
  
  y_preds_in = sign(phi_mat %*% w)
  correct_in = sum(y_preds_in == y_values)/n_pts
  
  
  phi_mat_out = as.matrix(new_phi_values_out)
  y_preds_out = sign(phi_mat_out %*% w)
  correct_out = sum(y_preds_out == y_values_out)/n_pts_out
  
  
  # Start SVM Test
  lloyd_df_svm = cbind(y = y_values, x0 = 1, lloyd_df)
  lloyd_df_svm_out = cbind(x0 = 1, x1=lloyd_df_out$x1_out, x2=lloyd_df_out$x2_out)
  
  
  model_svm <- svm(y ~ ., data = lloyd_df_svm, scale = FALSE, kernel = "radial", 
                   degree = 2,
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = gamma_val,
                   cost = 1e7)
  
  y_svm_preds_in = predict(model_svm, lloyd_df_svm[,-1])
  y_svm_preds_out = predict(model_svm, lloyd_df_svm_out)
  
  correct_in_svm = sum(y_svm_preds_in == y_values)/n_pts
  correct_out_svm = sum(y_svm_preds_out == y_values_out)/n_pts_out
  
  result_df = rbind(result_df,
  data.frame(k=k_num, correct_in=correct_in, correct_out=correct_out,
             correct_in_svm=correct_in_svm, correct_out_svm=correct_out_svm))
  
  print(paste0("Completed Iteration ", iter))
  
  }

mean(result_df$correct_out)

