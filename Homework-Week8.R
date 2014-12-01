 
library(e1071)
library(data.table)

# Initial - Creation of RDS Files, Uncomment and Run only once
# train <- data.table(read.table("http://www.amlbook.com/data/zip/features.train", header = FALSE))
# test <- data.table(read.table("http://www.amlbook.com/data/zip/features.test", header = FALSE))
# setnames(train, c("digit", "symmetry", "intensity"))
# setnames(test, c("digit", "symmetry", "intensity"))
# saveRDS(train, "train_hw8.rds")
# saveRDS(test, "test_hw8.rds")


# Initial - Creation of RDS Files, Uncomment and Run only once
# train <- data.table(read.table("http://www.amlbook.com/data/zip/features.train", header = FALSE))
# test <- data.table(read.table("http://www.amlbook.com/data/zip/features.test", header = FALSE))
# setnames(train, c("digit", "symmetry", "intensity"))
# setnames(test, c("digit", "symmetry", "intensity"))
# saveRDS(train, "train_hw8.rds")
# saveRDS(test, "test_hw8.rds")


# Question 1, 2

rm(list = ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

result = data.table(criteria = character(), error = numeric(),
                    SVs = numeric())

for (i_value in c(1,2,3,4,5,6,7,8,9,0)) {

  
  a_digit = i_value
  tr = copy(train_full)
  tr$digit <- ifelse(tr$digit == a_digit, 1, -1)
  tr$digit <- as.factor(tr$digit)
  
  te = copy(train_full)
  te$digit <- ifelse(te$digit == a_digit, 1, -1)
  te$digit <- as.factor(te$digit)
  
  
model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                 degree = 2,
                 type = "C-classification",
                 coef0 = 1,
                 shrinking = FALSE,
                 gamma = 1,
                 cost = 0.01)

preds = predict(model_svm, te[,-1,with=F])
incorrect = sum(preds != te$digit)
ratio = incorrect/nrow(te)
print (paste0(a_digit," vs all: Error = ", ratio))
result = rbindlist (list (result, data.table(criteria = paste0(a_digit, " vs all"),
                    error = ratio, SVs = model_svm$tot.nSV)))

}

result[order(error)]

# Question 4

abs(result[order(error)][1]$SVs - result[order(error)][10]$SVs)


# Question 5, 6

rm(list = ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

result = data.table(criteria = character(), error = numeric(), C_value = numeric(),
                    SVs = numeric())
  
  
  a_digit = 1
  tr = copy(train_full)
  tr = tr[digit %in% c(1,5),]
  tr$digit <- ifelse(tr$digit == a_digit, 1, -1)
  tr$digit <- as.factor(tr$digit)


  te = copy(train_full)
  te = te[digit %in% c(1,5)]
  te$digit <- ifelse(te$digit == a_digit, 1, -1)
  te$digit <- as.factor(te$digit)

for (c_value in c(0.0001, 0.001, 0.01)) {
#  for (c_value in c(1)) {
    
    
  
  model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                   degree = 2,
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = 1,
                   cost = c_value)
  
  preds = predict(model_svm, te[,-1,with=F])
  incorrect = sum(preds != te$digit)
  ratio = incorrect/nrow(te)
  print (paste0(a_digit," vs 5: C_value = ", c_value, " Error = ", ratio))
  result = rbindlist (list (result, data.table(criteria = paste0(a_digit, " vs 5"),
                                               error = ratio, C_value = c_value, 
                                               SVs = model_svm$tot.nSV)))
  
}

result[order(C_value)]

# With Q = 2 using E_in
# > result[order(C_value)]
# criteria       error C_value SVs
# 1:   1 vs 5 0.008968610   1e-04 236
# 2:   1 vs 5 0.004484305   1e-03  76
# 3:   1 vs 5 0.004484305   1e-02  34
# C_value = 1, Q = 2, E_out = 0.003203075

# With Q = 5 using E_in
# > result[order(C_value)]
# criteria       error C_value SVs
# 1:   1 vs 5 0.004484305   1e-04  26
# 2:   1 vs 5 0.004484305   1e-03  25
# 3:   1 vs 5 0.003843690   1e-02  23
# C_value = 1, Q = 5, E_out = 0.003203075

# [a] C = 0.0001, E_in -- Q = 2: 0.008968610, Q = 5: 0.004484305
# [b] C = 0.001, SVs -- Q = 2: 76, Q = 5: 25
# [c] C = 0.01, E_in -- Q = 2: 0.004484305, Q = 5: 0.003843690
# [d] C = 1, E_out -- Q = 2: 0.003203075, Q = 5: 0.003203075

# Question 7, 8

rm(list = ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

best_result <- data.table(best_C = numeric(), error = numeric())

a_digit = 1
tr = copy(train_full)
tr = tr[digit %in% c(1,5),]
tr$digit <- ifelse(tr$digit == a_digit, 1, -1)
tr$digit <- as.factor(tr$digit)

te = copy(train_full)
te = te[digit %in% c(1,5)]
te$digit <- ifelse(te$digit == a_digit, 1, -1)
te$digit <- as.factor(te$digit)

svmControl = tune.control(nrepeat = 1, sampling = "cross", cross = 10,
                          performances = TRUE)

for (i in c(1:100)) {

set.seed(sample(10000,1))
cv_svm <- tune.svm (digit ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                      degree = 2,
                      type = "C-classification",
                      coef0 = 1,
                      shrinking = FALSE,
                      gamma = 1,
                      cost = c(0.0001, 0.001, 0.01, 0.1, 1))

best_result <- rbindlist(list(best_result,
                              data.table(best_C = cv_svm$best.parameters$cost,
                                         error = cv_svm$best.performance)))
print (paste0("Iteration: ", i))
}

table(best_result$best_C)
mean(best_result$error[best_result$best_C==0.001])


# Question 9

rm(list = ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

result = data.table(criteria = character(), error = numeric(), C_value = numeric(),
                    SVs = numeric())


a_digit = 1
tr = copy(train_full)
tr = tr[digit %in% c(1,5),]
tr$digit <- ifelse(tr$digit == a_digit, 1, -1)
tr$digit <- as.factor(tr$digit)


te = copy(train_full)
te = te[digit %in% c(1,5)]
te$digit <- ifelse(te$digit == a_digit, 1, -1)
te$digit <- as.factor(te$digit)

for (c_value in c(0.01, 1, 100, 1e4, 1e6)) {
  #  for (c_value in c(1)) {
  
  
  
  model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "radial", 
                   degree = 2,
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = 1,
                   cost = c_value)
  
  preds = predict(model_svm, te[,-1,with=F])
  incorrect = sum(preds != te$digit)
  ratio = incorrect/nrow(te)
  print (paste0(a_digit," vs 5: C_value = ", c_value, " Error = ", ratio))
  result = rbindlist (list (result, data.table(criteria = paste0(a_digit, " vs 5"),
                                               error = ratio, C_value = c_value, 
                                               SVs = model_svm$tot.nSV)))
  
}

result[order(error)]

# Question 10

rm(list = ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

result = data.table(criteria = character(), error = numeric(), C_value = numeric(),
                    SVs = numeric())


a_digit = 1
tr = copy(train_full)
tr = tr[digit %in% c(1,5),]
tr$digit <- ifelse(tr$digit == a_digit, 1, -1)
tr$digit <- as.factor(tr$digit)


te = copy(test_full)
te = te[digit %in% c(1,5)]
te$digit <- ifelse(te$digit == a_digit, 1, -1)
te$digit <- as.factor(te$digit)

for (c_value in c(0.01, 1, 100, 1e4, 1e6)) {
  #  for (c_value in c(1)) {
  
  
  
  model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "radial", 
                   degree = 2,
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = 1,
                   cost = c_value)
  
  preds = predict(model_svm, te[,-1,with=F])
  incorrect = sum(preds != te$digit)
  ratio = incorrect/nrow(te)
  print (paste0(a_digit," vs 5: C_value = ", c_value, " Error = ", ratio))
  result = rbindlist (list (result, data.table(criteria = paste0(a_digit, " vs 5"),
                                               error = ratio, C_value = c_value, 
                                               SVs = model_svm$tot.nSV)))
  
}

result[order(error)]





