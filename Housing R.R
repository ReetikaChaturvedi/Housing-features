# Encoding the orginal file
library(readr)
Clean_GROUP_HOUSING <- read_csv("Desktop/Clean GROUP HOUSING.csv")
Clean.GROUP.HOUSING <- Clean_GROUP_HOUSING
View(Clean.GROUP.HOUSING)
dim(Clean.GROUP.HOUSING)
data_1<- Clean.GROUP.HOUSING
str(data_1)

# Summary Statistics
summary(data_1)
data_1<-Final
summary(Final)

#Linear Regression
# To find regression I would first want to know the plot and correlation to visualize
plot(Final$totalPrice,Final$square,main = "Scatterplot")
cor(Final$totalPrice,Final$square)

#not a strong correlation as per the coefficient
l_model<- lm(formula=Final$totalPrice~Final$square)
l_model


#Multiple Regression
mr<-lm(formula=Clean_GROUP_HOUSING$totalprice~Clean_GROUP_HOUSING$livingroom+Clean_GROUP_HOUSING$bedroom+ Clean_GROUP_HOUSING$bathroom+ Clean_GROUP_HOUSING$kitchen)
plot(mr)


#lasso Regression
# Converting Beijing housing to a big data matrix object
X.bm<- matrix(data = Clean_GROUP_HOUSING, nrow = 1,ncol =17 , byrow = F)
str(X.bm)
dim(X.bm)

x <- model.matrix(totalPrice~.,Clean_GROUP_HOUSING)
y <-Clean_GROUP_HOUSING$totalPrice

train <- sample(1:nrow(x), nrow(x)*.67)
test <- (-train)
y.test <- y[test]

# Checks
dim (x[train,])

length(y[train])
length(y[test])
grid <- 10^seq(10,-2,length=1000) 
lasso.mod <- glmnet(x[train,], y[train],alpha=1,lambda = grid)

dim(coef(lasso.mod ))
summary(lasso.mod) 
par(mfrow=c(1,2))
plot_glmnet(lasso.mod, xvar = "lambda", label = 5)
plot_glmnet(lasso.mod, xvar="dev",label=5) 
lasso.mod$lambda

set.seed(1)
#how to choose best lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out, label=TRUE)

