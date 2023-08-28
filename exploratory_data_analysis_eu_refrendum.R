(rm(list=ls()))

#-------------------------------------------------------------------------------
#load required libraries
library(ggplot2)
library(mgcv)
library(MASS)

#-------------------------------------------------------------------------------
#Read data set - recode -1 as a missing value
ref_data <- read.csv("ReferendumResults.csv", na = -1)

#check for the missing values
apply(is.na(ref_data),2,sum)
#267 missing values are present in the "Leave" column

# structure of data
str(ref_data)
ref_data1 <- na.omit(ref_data)
str(ref_data1)

# summary stats and converting the type of postals
summary(ref_data1$Leave)
ref_data1$Postals <- as.numeric(as.factor(ref_data1$Postals))

#-------------------------------------------------------------------------------
#Exploratory Analysis


# Correlation matrix
corr_matrix <- round(cor(ref_data1[, 4:48]), 2)
# correlation plot
par(mfrow=c(1,1), mar=c(5,5,2,2))
corrplot::corrplot(corr_matrix, type="upper", method="num", tl.col="black", tl.srt=90, tl.cex=0.5)

# Identify correlations with response variable and print out top variables with high correlation
corr_with_response <- corr_matrix[, "Leave"]

high_corr_vars1 <- names(corr_with_response[abs(corr_with_response) > 0.5])
print(high_corr_vars1)

high_corr_vars <- names(corr_with_response[abs(corr_with_response) > 0.35])
print(high_corr_vars)

# Scatterplot matrix of top 8 variables
pairs(ref_data1[, c("Leave", high_corr_vars1)])

# Leave by region
ggplot(ref_data1, aes(x = RegionName, y = Leave)) +
  geom_boxplot(fill = "#69b3a2") +
  ggtitle("Distribution of Leave Votes by Region") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot of Leave by Postals
options(warn = -1)
ggplot(ref_data, aes(x = Postals, y = Leave, fill = Postals)) +
  geom_boxplot() + 
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Boxplot of Leave by Postals", x = "Postals", y = "Leave")

#### percentage of population educated to the equivalent of degree level or above
# to the "Leave" vote
ggplot(data = ref_data1,aes(L4Quals_plus,Leave)) +
  geom_point() +
  geom_smooth(method = "lm")+
  labs(title = "Percentage of Educated people with Leave Votes", x = "%age educated people", y = "Leave")
#wards with more percentage of population having degree level education have lower
#leave vote


###################################################################################
###################################################################################
## MODELING

# Model building and evaluation
model1 <- lm(Leave ~ NVotes + MeanAge+ Households + Residents + L1Quals + L4Quals_plus + HigherOccup + C1C2DE + RegionName, data=ref_data)
summary(model1)
par(mfrow=c(2,2))
plot(model1)



# Linear model with parametric covariates
model1 <- lm(Leave ~ NVotes + MeanAge+ Households + Residents + L1Quals + L4Quals_plus + HigherOccup + C1C2DE + RegionName, data=ref_data)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

# Generalized linear model
model2 <- glm(Leave ~ NVotes + MeanAge+ Households + Residents + L1Quals + L4Quals_plus + HigherOccup + C1C2DE + RegionName, data=ref_data)
summary(model2)
plot(model2)

# Generalized additive model

model3 <- gam(Leave ~ s(Households) + s(Residents) + s(NVotes) + s(MeanAge) + s(L1Quals) + s(L4Quals_plus) + s(HigherOccup) + RegionName, data=ref_data)
summary(model3)
plot(model3)

# Look for interactions
model4 <- lm(Leave ~ NVotes + MeanAge+ Households + Residents + L1Quals + L4Quals_plus + HigherOccup + C1C2DE + RegionName + MeanAge:RegionName, data=ref_data)
summary(model4)


# Determine the significance of the model
anova(model1, model2, model3, model4, test="Chisq")

# model 3 has the lowest p-value so we will select model 3


##################################################################################
## PREDICTION


# extract data for building the model
model_data <- ref_data[!is.na(ref_data$Leave), c("Leave", "NVotes", "MeanAge", "Households", "Residents", "L1Quals", "L4Quals_plus", "HigherOccup", "C1C2DE", "RegionName")]

# build the GAM model
model <- gam(Leave ~ s(Households) + s(NVotes) + s(Residents) + s(MeanAge) + s(L1Quals) + s(L4Quals_plus) + s(HigherOccup) + RegionName, data = model_data)

# extract data for prediction
predict_data <- ref_data[is.na(ref_data$Leave), c("ID", "Leave", "NVotes", "MeanAge", "Households", "Residents", "L1Quals", "L4Quals_plus", "HigherOccup", "C1C2DE", "RegionName")]

# predict the proportion of 'Leave' votes
predicted_probabilities <- predict(model1, predict_data, type = "response")
head(predicted_probabilities)


# calculate the standard deviation of the prediction errors
n_i <- predict_data$NVotes
p_i_hat <- predicted_probabilities
p_i_var <- predict(model1, predict_data, type = "response")^2
y_i_var <- p_i_hat * (1 - p_i_hat) / n_i
sigma_hat <- sqrt(p_i_var + y_i_var)
print(sigma_hat)
mean(sigma_hat)
plot(sigma_hat, ylab = "Standard Deviation", main = "Standard Devation plot")
# add a horizontal line at the mean of y
abline(h = mean(sigma_hat), col = "red")


# saving the predictions in .dat file

# Get the predictions and standard errors in a data frame
pred_df <- data.frame(ID = predict_data$ID, 
                      Predicted_Proportion_Leave = predict(model, newdata = predict_data, se.fit = TRUE)$fit,
                      Standard_Error = predict(model, newdata = predict_data, se.fit = TRUE)$se.fit)
head(pred_df)

# Save the data frame to a text file
write.table(pred_df, file = "predictions.txt", sep = " ", row.names = FALSE, col.names = FALSE)


