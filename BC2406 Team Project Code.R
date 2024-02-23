install.packages("caret")
install.packages("car")
install.packages("corrplot")
install.packages("lubridate")
install.packages("tidyr")
install.packages("lmtest")
install.packages("randomForest")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("forecast")
install.packages("ehaGoF")
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(corrplot)
library(caret)
library(car)
library(forecast)
library(rpart)
library(rpart.plot)	
library(randomForest)
library(ehaGoF)
library(tibble)

# common functions
#function to plot line chart of ypredicted & ytrue against date on same graph
plotbothyovertime <- function(ytrue, ypredicted, date, title = "Predicted vs True Y Over Time")
{
  df = data.frame(date = date, ytrue = ytrue, ypredicted = ypredicted);
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = ytrue, color = "True Values")) +
    geom_line(aes(y = ypredicted, color = "Predicted Values")) +
    labs(x = "Date", y = "WTI_Spot Price") +
    scale_color_manual(values = c("True Values" = "blue", "Predicted Values" = "red")) +
    theme_minimal() +
    ggtitle(title) +
    theme(legend.title = element_blank());
}
#function to plot ypredicted against ytrue and visualise residuals
plotypredvstrue <- function(ytrue, ypredicted, title = "Predicted vs True Y with Residuals")
{
  data <- data.frame(ytrue, ypredicted);
  # Create the scatter plot
  scatter_plot <- ggplot(data, aes(x = ytrue, y = ypredicted)) + geom_point(shape = 1, color = "blue") + geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "black") + geom_segment(aes(x = ytrue, xend = ytrue, y = ypredicted, yend = ytrue), linetype = "dotted", color = "red", linewidth = 0.25) + labs(x = "True Y", y = "Predicted Y") + ggtitle(title) + theme_minimal();
  # Display the plot
  print(scatter_plot);
}
#function to calculate RMSE
rmse <- function(ytrue, ypredicted)
{
  residual = ytrue - ypredicted;
  naerror=is.na(residual);
  residual<-subset(residual, naerror==F);
  ans = sqrt(mean(residual^2));
  return(ans);
}
#--------------------------------------------------------------------------
# data cleaning
data <- read.csv("Downloads/WTI Price.csv")
View(data)
data=subset(data, select=-GSCI)
# count number of NA: 904
sum(is.na(data)) 
#remove rows with duplicate date entries, keeping only the first occurrence of each unique date.
data <- data[!duplicated(data$DATE),]
# Convert the DATE column to a Date format
data$DATE <- as.Date(data$DATE, format="%m/%d/%Y")
# Extract month and year from the DATE column
data$month <- month(data$DATE)
data$year <- year(data$DATE)
# Compute monthly means (excluding DATE column)
monthly_means <- data %>% select(-DATE) %>% group_by(month, year) %>% summarize(across(everything(), ~mean(.x, na.rm=TRUE)))
# Join the original data with the monthly means
data_joined <- left_join(data, monthly_means, by=c("month", "year"), suffix = c("", "_mean"))
View(data_joined)
# Replace NA values and outliers using the monthly means
columns_to_replace <- setdiff(names(data), c("DATE", "month", "year"))  #shows columns with missing data
columns_to_replace
for (column in columns_to_replace) 
{
  # Compute IQR and determine bounds for outliers
  Q1 <- quantile(data_joined[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data_joined[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  # Replace missing values and outliers
  data_joined[[column]] <- ifelse(is.na(data_joined[[column]]) | data_joined[[column]] < lower_bound | data_joined[[column]] > upper_bound, data_joined[[paste0(column, "_mean")]], data_joined[[column]])
}
# Drop the mean columns and month, year columns
data_filled <- data_joined %>% select(-matches("_mean"), -month, -year)
#Check if there are any NA, 0 NA values now
any(is.na(data_filled))
#----------------------------------------------------------------------
# Exploratory Data Analysis
# Correlation matrix and heatmap
View(data_filled)
summary(data_filled[, -c(1)]) #Exclude DATE column
str(data_filled)
cor_matrix = cor(data_filled[, -c(1)]) #Exclude date column
corrplot(cor_matrix)
# Extract magnitude of correlations related to WTI_Spot
wti_corr <- cor_matrix["WTI_Spot",]
wti_corr_magnitude = abs(wti_corr)
# Sort correlation magnitudes in decreasing order
sorted_wti_corr <- sort(wti_corr_magnitude, decreasing = TRUE)
sorted_wti_corr = as.data.frame(sorted_wti_corr)
View(sorted_wti_corr)
# Correlation of US_Oil_Demand and US_Oil_Supply with other variables
demand_corr <- cor_matrix["US_Oil_Demand",]
supply_corr <- cor_matrix["US_Oil_Supply",]
View(as.data.frame(demand_corr))
View(as.data.frame(supply_corr))

# Visualizations
# Histograms for numerical variables
par(mfrow = c(3, 4))
for (col in colnames(data_filled)[2:13]) 
{
  hist(data_filled[[col]], main = col, xlab = col, col = "lightblue")
}
par(mfrow = c(1, 1))
# Line chart for WTI_Spot
ggplot(data=data_filled, aes(x=DATE, y=WTI_Spot)) + geom_line() + labs(title="Time Series of WTI Spot Price", x="Date", y="Price")
# Histogram for WTI_Spot
ggplot(data=data_filled, aes(x=WTI_Spot)) + geom_histogram(binwidth=5, fill="blue", color="black", alpha=0.7) + labs(title="Histogram of WTI Spot Price", x="Price", y="Frequency")
# Boxplot for WTI_Spot
ggplot(data=data_filled, aes(y=WTI_Spot)) + geom_boxplot() + labs(title="Boxplot of WTI Spot Price", y="Price")
# Individual box plots for each variable
par(mfrow = c(3, 4))
for(col in colnames(data_filled)[2:13])
{
  boxplot(data_filled[[col]], main = col, col = "lightblue")
}
par(mfrow = c(1, 1))
#Pairwise scatter plots
pairs(data_filled[, c("WTI_Spot", "Inflation_5Y_BE", "DXY", "SP500", "NASDAQ", "DJIA")])
# Plotting trends of major indices over time
ggplot(data=data_filled) + geom_line(aes(x=DATE, y=SP500, color="SP500")) + geom_line(aes(x=DATE, y=NASDAQ, color="NASDAQ")) + geom_line(aes(x=DATE, y=DJIA, color="DJIA")) + labs(title="Trends of Major Indices Over Time", x="Date", y="Value") + theme(legend.title=element_blank())
#--------------------------------------------------------------------------
# data preparation
# set seed for reproducibility and checking
set.seed(2023)
# 70% train-test split
split_index <- createDataPartition(data_filled$WTI_Spot, p = 0.7, list = FALSE)
training_data <- data_filled[split_index, ]
testing_data <- data_filled[-split_index, ]
# get ytrue vectors for training and test sets
ytraintrue = training_data$WTI_Spot
ytesttrue = testing_data$WTI_Spot
#--------------------------------------------------------------------------
#Time Series Analysis (ARIMA)
# ARIMA model for WTI_Spot price
#first prediction
cutoffdate = as.Date("06-30-2020", format="%m-%d-%Y")
cutoffdate1 = as.Date("01-30-2018", format="%m-%d-%Y")
cutoffdate2 = as.Date("12-31-2020", format="%m-%d-%Y")
#second prediction
cutoffdate = as.Date("06-30-2021", format="%m-%d-%Y")
cutoffdate1 = as.Date("01-30-2019", format="%m-%d-%Y")
cutoffdate2 = as.Date("12-31-2021", format="%m-%d-%Y")
#third prediction
cutoffdate = as.Date("06-30-2018", format="%m-%d-%Y")
cutoffdate1 = as.Date("01-30-2017", format="%m-%d-%Y")
cutoffdate2 = as.Date("12-31-2018", format="%m-%d-%Y")
#prepare training data
trainingdata <- data.frame(training_data[,1:2])
trainingdata[,1+ncol(trainingdata)] <- cutoffdate
trainingdata[,1+ncol(trainingdata)] <- cutoffdate1
trainingdata <- subset(trainingdata, trainingdata$DATE < trainingdata$V3)
trainingdata <- subset(trainingdata, trainingdata$DATE > trainingdata$V4)
trainingdata <- trainingdata[,2]
#prepare test data
testingdata <- data.frame(testing_data[,1:2])
testingdata[,1+ncol(testingdata)] <- cutoffdate2
testingdata[,1+ncol(testingdata)] <- cutoffdate
testingdata <- subset(testingdata, testingdata$DATE < testingdata$V3)
testingdata <- subset(testingdata, testingdata$DATE > testingdata$V4)
graphtestingdata <- testingdata
testingdata <- testingdata[,2]
wti_arima <- auto.arima(trainingdata, approximation = F, stepwise = F)
forecasted_values <- forecast(wti_arima, h=183) # forecasting next 6 months
plot(forecasted_values,
     main = "Historical and Predicted prices of WTI", 
     xlab = "Timeline", ylab = "Stock Price ($)", col.main = "darkblue")
RMSE_upper = rmse(testingdata, forecasted_values$upper[,1]) #upperlimit (85%)
RMSE_mean = rmse(testingdata, forecasted_values$mean)
RMSE_lower = rmse(testingdata, forecasted_values$lower[,1]) #lowerlimit (85%)
RMSE_upper; RMSE_mean; RMSE_lower     #printRMSE
checkresiduals(forecasted_values) #check reliability of model
plotbothyovertime(graphtestingdata$WTI_Spot, mean(forecasted_values$mean), graphtestingdata$DATE) #graph plotted to compare testset and predicted mean value
#--------------------------------------------------------------------------
# CART Model for predicting WTI_Spot price
set.seed(2023)
wti_cart <- rpart(WTI_Spot ~ ., data=training_data[, -1], method="anova", control = rpart.control(cp = 0, minsplit=100)) # excluding DATE
plotcp(wti_cart)
# CART pruning via optimal complexity parameter
cverror.Cap = wti_cart$cptable[which.min(wti_cart$cptable[,"xerror"]), "xerror"] + wti_cart$cptable[which.min(wti_cart$cptable[,"xerror"]), "xstd"]
i = 1
j = 4
while(wti_cart$cptable[i,j]>cverror.Cap)
{
  i = i + 1
}
cp.opt = ifelse(i>1,sqrt(wti_cart$cptable[i,1] * wti_cart$cptable[i-1, 1]), 1)
pruned_wti_cart <- prune(wti_cart, cp = cp.opt)
# optimal CART
rpart.plot(pruned_wti_cart)
# ypredicted for train and test sets
ytrainpredictedtree <- predict(pruned_wti_cart, newdata = training_data)
ytestpredictedtree <- predict(pruned_wti_cart, newdata = testing_data)
# RMSE for train and test sets
rmsetraintree = rmse(ytraintrue, ytrainpredictedtree)
rmsetesttree = rmse(ytesttrue, ytestpredictedtree)
# goodness of fit: R-Squared 
rsqtraintree = gofRSq(ytraintrue, ytrainpredictedtree, dgt = 5)
rsqtesttree = gofRSq(ytesttrue, ytestpredictedtree, dgt = 5)
#plot predicted vs true y
plotypredvstrue(ytraintrue,ytrainpredictedtree, "Predicted vs True WTI Price with Residuals for CART train set")
plotypredvstrue(ytesttrue,ytestpredictedtree, "Predicted vs True WTI Price with Residuals for CART test set")
plotbothyovertime(ytraintrue, ytrainpredictedtree, training_data$DATE, "Predicted vs True WTI Price Over Time for CART train set")
plotbothyovertime(ytesttrue, ytestpredictedtree, testing_data$DATE, "Predicted vs True WTI Price Over Time for CART test set")
# view variables actually used in construction of the tree
printcp(pruned_wti_cart)
varsused = pruned_wti_cart$frame$var
varsused = varsused[varsused != "<leaf>"]
unique(varsused)
length(unique(varsused))
# plot variable importance against each xvariable
varimpttree = pruned_wti_cart$variable.importance
varimpttreedf = as.data.frame(varimpttree)
colnames(varimpttreedf) <- c("Importance")
View(varimpttreedf)
ggplot(varimpttreedf, aes(x = rownames(varimpttreedf), y = `Importance`, fill = `Importance`)) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Importance of each X Variable (CART)", x = "Variable", y = "Importance") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "lightblue", high = "darkblue") 
# count number of leaf nodes
leafnodes = pruned_wti_cart$frame[pruned_wti_cart$frame$var == "<leaf>",]
nrow(leafnodes)
# view info for each node by breadth-first traversal order
besttreeframe = pruned_wti_cart$frame
colnames(besttreeframe) <- c("Variable used to split node", "No. of obs", "Sum of weights", "Sum of squared residuals", "Mean Y", "CP value", "ncompete", "nsurrogate")
View(besttreeframe)
#----------------------------------------------------------
#Linear regression model to predict WTI Price
set.seed(2023)
lmfull = lm(WTI_Spot ~ ., data = training_data[,-1]) # exclude date
# find best linear regression model with lowest AIC
lmbest = step(lmfull)
# get generalised AIC value for best linear regression model
extractAIC(lmbest)[2]
# summary statistics of best linear regression model
lmbestsummary = summary(lmbest)
print(lmbestsummary)
# ypredicted for train and test sets
ytrainpredictedlm <- predict(lmbest, newdata = training_data)
ytestpredictedlm <- predict(lmbest, newdata = testing_data)
# RMSE for train and test sets
rmsetrainlm = rmse(ytraintrue, ytrainpredictedlm)
rmsetestlm = rmse(ytesttrue, ytestpredictedlm)
#plot predicted vs true y
plotypredvstrue(ytraintrue,ytrainpredictedlm, "Predicted vs True WTI Price with Residuals for Linear Model train set")
plotypredvstrue(ytesttrue,ytestpredictedlm, "Predicted vs True WTI Price with Residuals for Linear Model test set")
plotbothyovertime(ytraintrue, ytrainpredictedlm, training_data$DATE, "Predicted vs True WTI Price Over Time for Linear Model train set")
plotbothyovertime(ytesttrue, ytestpredictedlm, testing_data$DATE, "Predicted vs True WTI Price Over Time for Linear Model test set")
# view unadjusted r-squared values for training set
lmbestsummary$adj.r.squared
# goodness of fit: R-Squared 
rsqtrainlm = gofRSq(ytraintrue, ytrainpredictedlm, dgt = 5)
rsqtestlm = gofRSq(ytesttrue, ytestpredictedlm, dgt = 5)
# view xvariables actually used, their regression coefficient values and their p-values
View(lmbestsummary$coefficients)
# plot variable importance
varimptlmdf = as.data.frame(varImp(lmbest))
colnames(varimptlmdf) <- c("Importance")
View(varimptlmdf)
ggplot(varimptlmdf, aes(x = rownames(varimptlmdf), y = `Importance`, fill = `Importance`)) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Importance of each X Variable (Linear Model)", x = "Variable", y = "Importance") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "lightblue", high = "darkblue") 
# view total number of coefficients
lmbestsummary$df[1]
# basic linear model diagnostics
par(mfrow = c(2, 2))
plot(lmbest)
par(mfrow = c(1, 1))
# plot pdf & cdf of training set residuals
residualpdf = density(ytraintrue - ytrainpredictedlm)
plot(residualpdf, main='Residual Probability Density Function',xlab='Residuals')
residualcdf = ecdf(ytraintrue - ytrainpredictedlm)
plot(residualcdf,main='Residuals Cumulative Density Function ',xlab='Residuals')
# plot cook's distance and influence plot & view  potentially influential points
plot(lmbest, which = 4)
infl = influencePlot(lmbest)
sortedinfl = infl[order(-infl$CookD),]
View(sortedinfl)
# view GVIF and those with  high adjusted GVIF > 2, and also view xvariables actually used in best linear regression model and their degree of freedom
VIF = vif(lmbest)
vifdf = as.data.frame(VIF)
View(vifdf)
# count number of xvariables actually used in best linear regression model
nrow(vifdf)
#---------------------------------------------------------------
# Random Forest Model for predicting WTI_Spot price
set.seed(2023)
wti_rf <- randomForest(WTI_Spot ~ ., data = training_data[, -1], importance = TRUE) # excluding DATE
print(wti_rf)
# ypredicted for train and test sets
ytrainpredictedrf <- predict(wti_rf, newdata = training_data)
ytestpredictedrf <- predict(wti_rf, newdata = testing_data)
# RMSE for train and test sets
rmsetrainrf = rmse(ytraintrue, ytrainpredictedrf)
rmsetestrf = rmse(ytesttrue, ytestpredictedrf)
# goodness of fit: R-Squared 
rsqtrainrf = gofRSq(ytraintrue, ytrainpredictedrf, dgt = 5)
rsqtestrf = gofRSq(ytesttrue, ytestpredictedrf, dgt = 5)
#plot predicted vs true y
plotypredvstrue(ytraintrue,ytrainpredictedrf, "Predicted vs True WTI Price with Residuals for Random Forest train set")
plotypredvstrue(ytesttrue,ytestpredictedrf, "Predicted vs True WTI Price with Residuals for Random Forest test set")
plotbothyovertime(ytraintrue, ytrainpredictedrf, training_data$DATE, "Predicted vs True WTI Price Over Time for Random Forest train set")
plotbothyovertime(ytesttrue, ytestpredictedrf, testing_data$DATE, "Predicted vs True WTI Price Over Time for Random Forest test set")
# plot variable importance
varimptrfdf1 = as.data.frame(varImp(wti_rf, type = 1))
varimptrfdf2 = as.data.frame(varImp(wti_rf, type = 2))
colnames(varimptrfdf1) <- c("Importance")
colnames(varimptrfdf2) <- c("Importance")
View(varimptrfdf1)
View(varimptrfdf2)
ggplot(varimptrfdf1, aes(x = rownames(varimptrfdf1), y = `Importance`, fill = `Importance`)) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Importance of each X Variable (Random Forest) by Permutation Error", x = "Variable", y = "Importance") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "lightblue", high = "darkblue") 
ggplot(varimptrfdf2, aes(x = rownames(varimptrfdf2), y = `Importance`, fill = `Importance`)) + geom_bar(stat = "identity", width = 0.5) + labs(title = "Importance of each X Variable (Random Forest) by MSE Reduction", x = "Variable", y = "Importance") + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_fill_gradient(low = "lightblue", high = "darkblue") 
#---------------------------------------------------------------
# Overall analysis
rmsetrain = c(rmsetraintree, rmsetrainlm, rmsetrainrf)
rmsetest = c(rmsetesttree, rmsetestlm, rmsetestrf)
rsqtrain = c(rsqtraintree, rsqtrainlm, rsqtrainrf)
rsqtest = c(rsqtesttree, rsqtestlm, rsqtestrf)
models = c("CART", "Linear Model", "Random Forest")
rmsedf = data.frame(Model = models, RMSE_Train = rmsetrain, RMSE_Test = rmsetest)
rsqdf = data.frame(Model = models, R2_Train = rsqtrain, R2_Test = rsqtest)
# compare RMSE and R-Squared values across all models
View(rmsedf)
View(rsqdf)
rmsemetrics <- gather(rmsedf, Metric_Type, Value, -Model)
ggplot(rmsemetrics, aes(x = Model, y = Value, fill = Metric_Type)) + geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + labs(x = "Model", y = "Value") + scale_fill_manual(values = c("RMSE_Train" = "blue", "RMSE_Test" = "red")) + theme_minimal() + ggtitle("Comparison of RMSE Across All Models") + theme(legend.title = element_blank())
rsqmetrics <- gather(rsqdf, Metric_Type, Value, -Model)
ggplot(rsqmetrics, aes(x = Model, y = Value, fill = Metric_Type)) + geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + labs(x = "Model", y = "Value") + scale_fill_manual(values = c("R2_Train" = "blue", "R2_Test" = "red")) + coord_cartesian(ylim = c(0.85, 1)) + scale_y_continuous(breaks = seq(0.85, 1.005, by = 0.01)) + theme_minimal() + ggtitle("Comparison of R-Squared Across All Models") + theme(legend.title = element_blank())
