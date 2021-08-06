install.packages("class")
library("class")
install.packages("gmodels")
library("gmodels")

#Data Accumulation 
set.seed(121)
data <- read.csv(file.choose())
View(data)

table(data$type)
str(data)
data$type <- as.factor(data$type)
str(data)

#Normalized Value  function
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Normalize Function Calling
data_n <- as.data.frame(lapply(data[2:(ncol(data)-1)], normalize))
View(data_n)

#Dividing data in to parts: Training and Testing 
ind <- sample(2,nrow(data_n),replace=T,prob = c(0.8,0.2))
train <- data_n[ind==1,]
test <- data_n[ind==2,]
train_labels <- data[ind==1,ncol(data)]
test_labels <- data[ind==2,ncol(data)]
View(test_labels)

#KNN Classifier Model
model <- knn(train = train,test = test,cl=train_labels,k=3)

#Model Evaluation
CrossTable(x=test_labels,y=model,prop.chisq = FALSE)
    