library(ggplot2)
library(cowplot)
install.packages("ROCR")
library(ROCR)

data <- read.csv("C:\\Users\\Hp\\Downloads\\diabetes.csv")
data

str(data)

sample <- sample(c(TRUE,FALSE),nrow(data),replace = TRUE, prob = c(0.2,0.8))
train <- df[sample,]
test <- df[!sample,]

clf <- glm(Outcome~.,data = train, family = "binomial")
clf


# Make predictions on the dataset
predictions <- predict(clf, type = "response")
predictions

summary(clf)

anova(clf,test = "Chisq")

for (i in predictions){
    if (i > 0.5)
   {print("T")}
     else{print("F")}
}


fitted.results <- predict(clf)
fitted.results <- ifelse(fitted.results>0.5,1,0)
error1 <- mean(fitted.results != test$Outcome)
1 - error1
# accuracy of 0.593 not good enough

sample2 <- sample(c(TRUE,FALSE),nrow(data),replace = TRUE, prob = c(0.35,0.65))
train2 <- df[sample2,]
test2 <- df[!sample2,]

clf2 <- glm(Outcome~ .,data = train2, family = "binomial")
clf2

fitted.results2 <- predict(clf2)
fitted.results2 <- ifelse(fitted.results2>0.5,1,0)
error2 <- mean(fitted.results2 != test2$Outcome)
1 - error2
#accuracy of 0.6 still not good enough