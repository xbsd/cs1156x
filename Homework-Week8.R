library(e1071)

setwd("~/Documents/study//edX/cs1156x/")

#train <- data.table(read.table("http://www.amlbook.com/data/zip/features.train", header = FALSE))
#test <- data.table(read.table("http://www.amlbook.com/data/zip/features.test", header = FALSE))
#setnames(train, c("digit", "symmetry", "intensity"))
#setnames(test, c("digit", "symmetry", "intensity"))
#saveRDS(train, "train_hw8.rds")
#saveRDS(test, "test_hw8.rds")

train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")


# Question 2, 3, 4 

result <- data.table(criteria=character(), E_in=numeric(), total_SVs=numeric())

for (i in 0:9){
a_digit = i
tr = copy(train_full)
tr$digit = ifelse(tr$digit==a_digit, 1, -1)

te = copy(train_full)
te$digit = ifelse(te$digit==a_digit, 1, -1)

model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                 degree = 2,
                 type = "C-classification",
                 coef0 = 1,
                 shrinking = FALSE,
                 gamma = 1,
                 cost = 0.01)

preds = predict(model_svm, te[,-1,with=F])
print(paste0 (a_digit, " vs all: ", sum(preds != te$digit)/nrow(te)))
result = rbindlist(list(result, data.table(criteria=paste0(a_digit, " vs all"), 
                                  E_in = sum(preds != te$digit)/nrow(te),
                                  total_SVs = model_svm$tot.nSV)))
}

result[order(E_in)]


# Question 5  

rm(list=ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

result <- data.table(criteria=character(), c_value = numeric(), 
                     E_in=numeric(), total_SVs=numeric())

for (c_value in c(0.001, 0.005, 0.01, 0.05, 0.1, 1)){
  a_digit = 1
  tr = copy(train_full)
  tr <- tr[tr$digit %in% c(1,5),]
  tr$digit = ifelse(tr$digit==a_digit, 1, -1)
  
  te = copy(train_full) # Change this for E_in and E_out as needed
                        # For E_in use train_full, for E_out use test_full
  
  te <- te[te$digit %in% c(1,5),]
  te$digit = ifelse(te$digit==a_digit, 1, -1)
  
  model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                   degree = 2,
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = 1,
                   cost = c_value)
  
  preds = predict(model_svm, te[,-1,with=F])
  print(paste0 (a_digit, " vs 5: ", sum(preds != te$digit)/nrow(te)))
  result = rbindlist(list(result, data.table(criteria=paste0(a_digit, " vs 5"), 
                                             C = c_value, E_in = sum(preds != te$digit)/nrow(te),
                                             total_SVs = model_svm$tot.nSV)))
}

result[order(-C)]


# Question 6 

rm(list=ls())
train_full <- readRDS("train_hw8.rds")
test_full <- readRDS("test_hw8.rds")

result <- data.table(criteria=character(), c_value = numeric(), 
                     E_in=numeric(), total_SVs=numeric())

for (c_value in c(0.0001, 0.001, 0.01, 1)){
  a_digit = 1
  tr = copy(train_full)
  tr <- tr[tr$digit %in% c(1,5),]
  tr$digit = ifelse(tr$digit==a_digit, 1, -1)
  
  te = copy(train_full) # Change this for E_in and E_out as needed
                        # For E_in use train_full, for E_out use test_full
  
  te <- te[te$digit %in% c(1,5),]
  te$digit = ifelse(te$digit==a_digit, 1, -1)
  
  model_svm <- svm(digit ~ ., data = tr, scale = FALSE, kernel = "polynomial", 
                   degree = 2, # Change this for different values of Q
                   type = "C-classification",
                   coef0 = 1,
                   shrinking = FALSE,
                   gamma = 1,
                   cost = c_value)
  
  preds = predict(model_svm, te[,-1,with=F])
  print(paste0 (a_digit, " vs 5: ", sum(preds != te$digit)/nrow(te)))
  result = rbindlist(list(result, data.table(criteria=paste0(a_digit, " vs 5"), 
                                             C = c_value, E_in = sum(preds != te$digit)/nrow(te),
                                             total_SVs = model_svm$tot.nSV)))
}

result[order(C)]





