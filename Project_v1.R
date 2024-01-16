# load required libraries
library(dplyr)
library(corrplot)
library(ISLR)
library(caret)
library(ROCR)
library(pROC)
library(rpart)
library('rpart.plot')

# read the dataset
train_data <- as.data.frame(read.csv("train.csv", header=TRUE,sep=","))
test_data <- as.data.frame(read.csv("test.csv", header=TRUE,sep=","))

# size of train data
dim(train_data)
cat("Train data contains",nrow(train_data),"rows")
# size of test data
dim(test_data)
cat("Test data contains",nrow(test_data),"rows")

# displays the first few rows
head(train_data)
head(test_data)

# summary statistics of data
summary(train_data)
summary(test_data)

# check the datatypes of the variables in the dataset
str(train_data)
str(test_data)

# checking the dependent Variable
table(train_data$satisfaction)
table(test_data$satisfaction)

# check if there are any nan values in the dataset
sum(is.na(train_data))
sum(is.na(test_data))
sum(is.na(train_data$Arrival.Delay.in.Minutes))
sum(is.na(test_data$Arrival.Delay.in.Minutes))

cleaned_train <- na.omit(train_data)
cleaned_test <- na.omit(test_data)

# check if the null values are deleted
sum(is.na(cleaned_train))
sum(is.na(cleaned_test))

dim(cleaned_train)
dim(cleaned_test)

# check if removal of NA values has affected the shape of data
hist(log(cleaned_train$Arrival.Delay.in.Minutes))
hist(log(train_data$Arrival.Delay.in.Minutes))
hist(log(cleaned_test$Arrival.Delay.in.Minutes))
hist(log(test_data$Arrival.Delay.in.Minutes))

cleaned_train$Inflight.wifi.service = as.factor(cleaned_train$Inflight.wifi.service)
cleaned_train$Departure.Arrival.time.convenient = as.factor(cleaned_train$Departure.Arrival.time.convenient)
cleaned_train$Ease.of.Online.booking = as.factor(cleaned_train$Ease.of.Online.booking) 
cleaned_train$Gate.location = as.factor(cleaned_train$Gate.location)
cleaned_train$Food.and.drink = as.factor(cleaned_train$Food.and.drink)
cleaned_train$Online.boarding = as.factor(cleaned_train$Online.boarding)
cleaned_train$Seat.comfort = as.factor(cleaned_train$Seat.comfort)
cleaned_train$Inflight.entertainment = as.factor(cleaned_train$Inflight.entertainment)
cleaned_train$On.board.service = as.factor(cleaned_train$On.board.service)
cleaned_train$Leg.room.service = as.factor(cleaned_train$Leg.room.service)
cleaned_train$Baggage.handling = as.factor(cleaned_train$Baggage.handling)
cleaned_train$Checkin.service = as.factor(cleaned_train$Checkin.service)
cleaned_train$Inflight.service = as.factor(cleaned_train$Inflight.service)
cleaned_train$Cleanliness = as.factor(cleaned_train$Cleanliness)
cleaned_train$satisfaction = as.factor(cleaned_train$satisfaction)

str(cleaned_train)

cleaned_test$Inflight.wifi.service = as.factor(cleaned_test$Inflight.wifi.service)
cleaned_test$Departure.Arrival.time.convenient = as.factor(cleaned_test$Departure.Arrival.time.convenient)
cleaned_test$Ease.of.Online.booking = as.factor(cleaned_test$Ease.of.Online.booking) 
cleaned_test$Gate.location = as.factor(cleaned_test$Gate.location)
cleaned_test$Food.and.drink = as.factor(cleaned_test$Food.and.drink)
cleaned_test$Online.boarding = as.factor(cleaned_test$Online.boarding)
cleaned_test$Seat.comfort = as.factor(cleaned_test$Seat.comfort)
cleaned_test$Inflight.entertainment = as.factor(cleaned_test$Inflight.entertainment)
cleaned_test$On.board.service = as.factor(cleaned_test$On.board.service)
cleaned_test$Leg.room.service = as.factor(cleaned_test$Leg.room.service)
cleaned_test$Baggage.handling = as.factor(cleaned_test$Baggage.handling)
cleaned_test$Checkin.service = as.factor(cleaned_test$Checkin.service)
cleaned_test$Inflight.service = as.factor(cleaned_test$Inflight.service)
cleaned_test$Cleanliness = as.factor(cleaned_test$Cleanliness)

str(cleaned_test)

numerical_vars <- cleaned_train[, sapply(cleaned_train, is.numeric)]

numerical_vars <- numerical_vars[,-c(1,2)] # remove X and ID column
# Computing correlation matrix
cor_matrix <- cor(numerical_vars)

high_correlation <- findCorrelation(cor_matrix, cutoff = 0.7, verbose = FALSE)
highly_correlated_variable <- colnames(numerical_vars[high_correlation])
cat("The highly correlated variable is:", highly_correlated_variable)

cleaned_train <- cleaned_train[,-c(1,2,24)] # omit X and ID column and highly correlated varible 'Arrival.Delay.in.Minutes
cleaned_test <- cleaned_test[,-c(1,2,24)] # omit X and ID column and highly correlated varible 'Arrival.Delay.in.Minutes

dim(cleaned_train)
head(cleaned_train)

cleaned_train <- cleaned_train %>%
  mutate(satisfaction = ifelse(satisfaction == 'neutral or dissatisfied', 0, 1))

# correlation plot of numeric variable
train.cor <- cor(subset(cleaned_train,select = c( Age, Flight.Distance, Departure.Delay.in.Minutes)))
summary(train.cor)
corrplot(train.cor, order="hclust")
round(train.cor,2)

# box plot
boxplot(x = as.list(numerical_vars))

# Calculate the first and third quartiles and interquartile range (IQR) for Flight.Distance
Q1 <- quantile(cleaned_train$Flight.Distance, 0.25)
Q3 <- quantile(cleaned_train$Flight.Distance, 0.75)
IQR <- Q3 - Q1

# Calculate lower and upper limits for outliers 
lower.limit <- Q1 - 1.5 * IQR
upper.limit <- Q3 + 1.5 * IQR
cat("Lower limit:", lower.limit, "\n")
cat("Upper limit:", upper.limit, "\n")

# Subset the data to remove outliers based on the calculated limits 
train_df <- cleaned_train[cleaned_train$Flight.Distance > lower.limit & cleaned_train$Flight.Distance < upper.limit, ]
cat("Number of outliers removed:", nrow(cleaned_train) - nrow(train_df), "\n")

# Calculate the first and third quartiles and interquartile range (IQR) for Departure.Delay.in.Minutes
Q1 <- quantile(train_df$Departure.Delay.in.Minutes, 0.25)
Q3 <- quantile(train_df$Departure.Delay.in.Minutes, 0.75)
IQR <- Q3 - Q1

# Calculate lower and upper limits for outliers 
lower.limit <- Q1 - 1.5 * IQR
upper.limit <- Q3 + 1.5 * IQR
cat("Lower limit:", lower.limit, "\n")
cat("Upper limit:", upper.limit, "\n")

# Subset the data to remove outliers based on the calculated limits 
train_df2 <- train_df[train_df$Departure.Delay.in.Minutes > lower.limit & train_df$Departure.Delay.in.Minutes < upper.limit, ]
cat("Number of outliers removed:", nrow(train_df) - nrow(train_df2), "\n")

final.train <- train_df2

# Plot the distribution of quality variable 

ggplot(final.train, aes(x = factor(satisfaction))) +
  geom_bar() +
  scale_fill_manual(values = c("#3366CC", "#DC3912")) +
  labs(title = "Distribution of Satisfaction") +
  xlab("Satisfaction") +
  ylab("Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
set.seed(123)
glm.fit <- glm(satisfaction ~ ., data = final.train, family = "binomial")

#Reviewing the model using summary Function
summary(glm.fit)
# from the summary we can see that variables Gender, Flight.Distance, Inflight.wifi.service, Gate.location, Food.and.drink, Seat.comfort, 
# Inflight.entertainment, On.board.service are non-significant variables

importance <- varImp(glm.fit, scale = FALSE)
importance

#retraining the model by removing insignificant variables
glm.fit2 <- glm( satisfaction ~ Customer.Type + Age + Type.of.Travel + Class + 
  Departure.Arrival.time.convenient + Ease.of.Online.booking + Online.boarding  + Leg.room.service +
  Baggage.handling + Checkin.service + Inflight.service +
  Cleanliness + Departure.Delay.in.Minutes, data = final.train, family = "binomial")
summary(glm.fit2)

# make predictions
glm.predict = predict(glm.fit2, type = 'response' , newdata=cleaned_test)
summary(glm.predict)

# Set a threshold
threshold <- 0.5

# Convert predicted probabilities to class labels
glm.predicted_class <- ifelse(glm.predict >= threshold, 'satisfied', 'neutral or dissatisfied ')
glm.predicted_class
# Display the summary of predicted class labels
table(glm.predicted_class)
table(cleaned_test$satisfaction, glm.predict > 0.5)


#Plotting the ROC Curve
ROCRpred <- prediction(glm.predict, cleaned_test$satisfaction)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1),text.adj = c(-0.2,1.7))
# from this plot we can see that potential optimal threshold values can be 0.5, 0.6 and 0.7

# confusion matrix for threshold values
conf_matrix.0.5 <- table(cleaned_test$satisfaction, glm.predict > 0.5)
conf_matrix.0.6 <- table(cleaned_test$satisfaction, glm.predict > 0.6)
conf_matrix.0.7 <- table(cleaned_test$satisfaction, glm.predict > 0.7)

# calculate accuracy by optimum threshold value
glm.accuracy0.5 = sum(diag(conf_matrix.0.5)) / sum(conf_matrix.0.5)
cat("Accuracy with threshold = 0.5:",glm.accuracy0.5) # highest accuracy
glm.accuracy0.6 = sum(diag(conf_matrix.0.6)) / sum(conf_matrix.0.6)
cat("Accuracy with threshold = 0.6:",glm.accuracy0.6)
glm.accuracy0.7 = sum(diag(conf_matrix.0.7)) / sum(conf_matrix.0.7)
cat("Accuracy with threshold = 0.7:",glm.accuracy0.7)

tp <- conf_matrix.0.5[2, 2]  # True positive
tn <- conf_matrix.0.5[1, 1]  # True negative
fp <- conf_matrix.0.5[1, 2]  # False positive
fn <- conf_matrix.0.5[2, 1] # false negative

# calculate recall
glm.recall <- tp / (tp + fn)
glm.recall

# calculate precision
glm.precision <- tp / (tp + fp)
glm.precision


# AUC
glm.roc_curve <- roc(cleaned_test$satisfaction, glm.predict)

# Obtain the AUC
auc_value <- auc(glm.roc_curve)
auc_value


# Decision Tree

tree.fit <- rpart(satisfaction ~ ., 
             method="class", 
             data=cleaned_train,
             control=rpart.control(minsplit=1),
             parms=list(split='information'))
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty =0)

# Plot the tree using better plot function
rpart.plot(tree.fit)

# check significant variables
varImp(tree.fit)

#refit the model using only significant variables
tree.fit1 <- rpart(satisfaction ~ Age+Class+Ease.of.Online.booking+
                     Inflight.entertainment+Inflight.wifi.service+Leg.room.service+Online.boarding+
                     Type.of.Travel,
                  method="class", 
                  data=cleaned_train,
                  control=rpart.control(minsplit=1),
                  parms=list(split='information'))
summary(tree.fit1)
plot(tree.fit1)
text(tree.fit1, pretty =0)

# Plot the tree using better plot function
rpart.plot(tree.fit1)

# pruning the tree to avoid overfitting using cross validation

printcp(tree.fit1) #display crossvalidated error for each tree size
plotcp(tree.fit1) # plotting cv error

#select Complexity parameter (CP) with lowest crossvalidated error 
opt.cp <- tree.fit1$cptable[which.min(tree.fit1$cptable[,"xerror"]),"CP"]
opt.cp #0.01

# pruning the tree
fit.pruned <- prune(tree.fit1, cp=opt.cp)
# plot the pruned tree
rpart.plot(fit.pruned)

# make predictions using pruned tree
tree.predict = predict(fit.pruned, newdata = cleaned_test, type='class')
summary(tree.predict)
rpart.plot(tree.predict)

# Create a confusion matrix
conf.matrix <- table(tree.predict, cleaned_test$satisfaction)

# Calculate accuracy
accuracy <- sum(diag(conf.matrix)) / sum(conf.matrix)

# Display the confusion matrix and accuracy
print(conf.matrix)
cat("Accuracy:", accuracy)

# Calculate recall
recall <- conf.matrix[2, 2] / sum(conf.matrix[2, ])
cat("Recall:", recall)

# Calculate precision
precision <- conf.matrix[2, 2] / sum(conf.matrix[, 2])
cat("Precision:", precision)

#Need to figure out TODO
# AUC
# Calculate the predicted probabilities
probabilities <- predict(fit.pruned, newdata = cleaned_test, type = 'prob')[, "satisfied"]
tree.roc_curve <- roc(cleaned_test$satisfaction, probabilities)
plot(tree.roc_curve)
# Obtain the AUC
auc_value <- auc(tree.roc_curve)
auc_value
