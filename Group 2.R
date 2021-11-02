# load libraries
install.packages("kernlab")
install.packages("dplyr")
install.packages("class")
library(dplyr)
library(kernlab)
library(class)

# load data
data("spam")
df1 = spam[spam["type"] == "spam",]
df2 = spam[spam["type"] == "nonspam",]
df_1 = df1[,1:57]
df_2 = df2[,1:57]

# slice dataframes to aviod repeated sampling
df_11 = df_1[1:500,]
df_12 = df_2[1:500,]
df_13 = df_1[601:1100,]
df_14 = df_2[601:1100,]

# create sampling criteria
n = 100
n2 = 50
NN1 = dim(df1)
NN2 = dim(df2)

# random sample data from df
set.seed(666)
train_spam = sample_n(df_11,n,replace = FALSE)
train_nonspam = sample_n(df_12,n,replace = FALSE)

test_spam = sample_n(df_13,n2)
test_nonspam = sample_n(df_14,n2)

train_rand = rbind(train_spam, train_nonspam)
test_rand = rbind(test_spam, test_nonspam)
train_label = factor(c(rep("spam",n),rep("nonspam",n)))
test_label = factor(c(rep("spam",n2),rep("nonspam",n2)))
# classification using knn, with k=1
kk=1 
set.seed(777)
knn_pred_1=knn(train=train_rand,test=test_rand, cl=train_label,k=kk, prob=FALSE)

# classification using knn, with k=9
kk2=9
set.seed(778)
knn_pred_2=knn(train=train_rand,test=test_rand, cl=train_label,k=kk2, prob=FALSE)

# classification using knn, with k=25
kk3=25 
set.seed(779)
knn_pred_3=knn(train=train_rand,test=test_rand, cl=train_label,k=kk3, prob=FALSE)

summary(knn_pred_1)
summary(knn_pred_2)
summary(knn_pred_3)

# Model accuracy comparison
cm1 = as.matrix(table(Actual = test_label, Predicted = knn_pred_1))
cm1
sum(diag(cm1))/length(test_label)

cm2 = as.matrix(table(Actual = test_label, Predicted = knn_pred_2))
cm2
sum(diag(cm2))/length(test_label)

cm3 = as.matrix(table(Actual = test_label, Predicted = knn_pred_3))
cm3
sum(diag(cm3))/length(test_label)

