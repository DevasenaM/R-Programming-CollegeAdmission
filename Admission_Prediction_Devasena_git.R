#Load data
library(rio)   # to import the csv file
library(tidyverse) #for dplyr

#--------------------------------------------Data Loading and Cleaning-------------------
link = 'Admission_Prediction.csv'

df <- import(link)

#Check the column names
colnames(df)
#Rename variables without spaces.
colnames(df) <- c('Serial No.', 'GRE', 'TOEFL',     
                  'University_Rating', 'SOP','LOR',              
                  'CGPA','Research','Chance_of_Admit')

#Remove the column serial_no, which is not needed for modelling
df <- df[,-c(1)]

#Summary
summary(df)

library(mice)
#A.1 Examine missing values
dev.off()
md.pattern(df,
           rotate.names = T)

#Skimr
library(skimr)
skim(df)

#Checking for any NA's in dataframe
sum(is.na(df))

#For classification, convert the Chance_of_Admit column as 0 & 1, with the mean of .73
df_Logi <- df
df_Logi$Chance_of_Admit = ifelse(df_Logi$Chance_of_Admit >.73,1,0)
summary(df_Logi)
table(df_Logi$Chance_of_Admit)

#------------------------------------------Plotting-----------------------------

#Plotting - Histogram - Preliminary data analysis (Chance_of_Admit column - 0 Vs 1)
hist(df_Logi$Chance_of_Admit,
     main="Distribution of Admission", 
     ylab="Total Admission", 
     xlab=("Admission"),
     #breaks = 5,
     #border="blue",
     col=c ("red","blue"),
     ylim=c(0,250)
     #xlim=c(0,0.5,1)
) 
text(0,209,"209",adj = c(.1, -.5), col = "blue3",cex=.8)
text(1,191,"191",adj = c(.9, -.5), col = "purple3",cex=.8)

#Convert the 'Chance_of_Admit' 'as (0,1) factor.
df_Logi$Chance_of_Admit <- as.factor(df_Logi$Chance_of_Admit)

#Package loading
library(cowplot)        #to display multiple graph
library(ggplot2)        #for plotting
dev.off()


#Plotting - Chance_Of_Admit Vs all other variables
a <- ggplot(df, aes(CGPA, Chance_of_Admit)) +
  labs(y = 'Admission')+
  geom_jitter(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_light()


b <- ggplot(df, aes(GRE, Chance_of_Admit)) +
  labs(y = 'Admission')+
  geom_jitter(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_light()

c <- ggplot(df, aes(Research, Chance_of_Admit)) +
  geom_jitter(color = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_light()+
  labs(y = 'Admission')

d <- ggplot(df, aes(SOP, Chance_of_Admit)) +
  labs(y = 'Admission')+
  geom_jitter(color = "brown", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)
theme_light()

x <- ggplot(df, aes(TOEFL, Chance_of_Admit)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_light()+
  labs(y = 'Admission')

y <- ggplot(df, aes(University_Rating, Chance_of_Admit)) +
  geom_jitter(color = "purple", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)+
  theme_light()+
  labs(y = 'Admission')

z <- ggplot(df, aes(LOR, Chance_of_Admit,xlab = 'Letter of Recomm')) +
  geom_jitter(color = "red", alpha = 0.5) +
  geom_smooth(color = "red",method = "lm", se = T)+
  theme_light()+
  labs(y = 'Admission')


plot_grid(a,b,c,d,x, y,z)

#Plotting - Box Plot of the  four variables.

m <-      ggplot(df,aes(x = "", y = CGPA))+
  geom_boxplot(fill = 'blue')+
  coord_cartesian(ylim = c(7, 10))+
  theme_classic()+
  stat_summary(fun = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun = mean, geom="text",colour="white", size=5,
               vjust = -0.7, aes(label = paste("Mean:", round(..y.., digits = 1))))


n <-      ggplot(df,aes(x = "", y = GRE))+
  geom_boxplot(fill = 'blue')+
  coord_cartesian(ylim = c(300, 340))+
  theme_classic()+
  stat_summary(fun = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun = mean, geom="text",colour="white", size=5,
               vjust = -0.7, aes(label = paste("Mean:", round(..y.., digits = 1)))) 

o <-    ggplot(df,aes(x = "", y = TOEFL))+
  geom_boxplot(fill = 'blue')+
  coord_cartesian(ylim = c(90, 120))+
  theme_classic()+
  stat_summary(fun = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun = mean, geom="text",colour="white", size=5,
               vjust = -0.7, aes(label = paste("Mean:", round(..y.., digits = 1))))

p <-    ggplot(df,aes(x = "", y = University_Rating))+
  geom_boxplot(fill = 'blue')+
  coord_cartesian(ylim = c(1, 5))+
  theme_classic()+
  stat_summary(fun = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun = mean, geom="text",colour="white", size=5,
               vjust = -0.7, aes(label = paste("Mean:", round(..y.., digits = 1))))


plot_grid(m,n,o,p)

#-------------------------------------------Correlation Analysis----------------------

#Correlation Analysis

#Correlation analysis
library(GGally) #to run ggpairs
ggpairs(df)

#Correlation matrix using corrplot
library(corrplot) #to run corrplot
cor_df <- cor(df)


corrplot(cor_df, method = 'number', diag = F, 
         order = 'hclust',
         addrect = 3, rect.col = 'black', 
         rect.lwd = 3, tl.pos = 'lt',t1.cex=1,t1.srt = 45)

#Now using corrgram
library(corrgram)
corrgram(df,
         upper.panel = panel.cor)

#--------------------------------------------------------Regression Model ----------------------
#------------------Linear Regression Vs RandomForest---------------------------
#####
library(caret) # for classification and Regression model

############ Linear Regression#####

#0. Fixing the initial randomization for linear regression
set.seed(100)

#1. Split the data into training and testing
train_positions <- createDataPartition(y = df$Chance_of_Admit, 
                                       p = .7,        
                                       list = F)     

training <- df[train_positions,]
testing <- df[-train_positions,]

#2. Cross-validation and model tuning options
fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random')

#3. Fit an algorithm to your data
train(Chance_of_Admit ~ .,
      data       = training,
      method     = 'lm',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      trControl  = fit_control) -> model_fit

#3a. Check that the linear model is appropriate
#Residual analysis 

#Prediction for training
training_pred <- predict(model_fit, training) 
training_pred <- as.data.frame(training_pred)


#Residuals for training
training_residuals <- training_pred - training$Chance_of_Admit
training_residuals <- as.data.frame(training_residuals)
colnames(training_residuals) <- 'training_residuals'

#Plot predicted values vs residuals
dev.off()
plot(y = scale(training_residuals$training_residuals),
     x = training_pred$training_pred,
     main = 'Residual analysis',
     ylab = 'Standardized Residuals (std dev)',
     xlab = 'Predicted values')

abline(h = 0, col = 'red')
abline(h = 2, col = 'blue')
abline(h = -2, col = 'blue')
abline(h = 1, col = 'green')
abline(h = -1, col = 'green')


#Density plot
plot(density(training_residuals$training_residuals),
     main = 'Distr of residuals')

#linear model
model_fit
model_fit$finalModel
summary(model_fit$finalModel)

#RMSE - Linear Model
# RMSE        Rsquared   MAE       
# 0.06462765  0.8020547  0.04644549

#Conclude that linear regression is one of a good fit for my data

#Check another model - Random Forest, if we can improve performance.

#-----------------------------Regression Model - RandomForest ---------

#1. Split the data into training and testing
#Check Random Forest
train_positions <- createDataPartition(y = df$Chance_of_Admit, 
                                       p = .7,        #Training %
                                       list = F)     

training <- df[train_positions,]
testing <- df[-train_positions,]

#2. Cross-validation and model tuning options
fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random')

#3. Fit an algorithm to your data
train(Chance_of_Admit ~ .,
      data       = training,
      method     = 'rf',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      trControl  = fit_control) -> model_fit_rf

#Final result
model_fit_rf
summary(model_fit_rf)

#model_fit_rf
# mtry  RMSE        Rsquared   MAE       
# 1     0.06357185  0.8122486  0.04733647
# 2     0.06127051  0.8178095  0.04459781
# 3     0.06144112  0.8150052  0.04454838
# 4     0.06219371  0.8096908  0.04522954
# 6     0.06360797  0.7995191  0.04622727
# 
# RMSE was used to select the optimal model using the
# smallest value.
# The final value used for the model was mtry = 2

#8. Variable Importance --- We check for rf,since it is the best model.
dev.off()
varImp(model_fit_rf, scale = T)
plot(varImp(model_fit_rf, scale = T))

#Apply the best fit model to the whole data - Randomforest
#Predictions for whole data 
final_pred <- predict(model_fit_rf, df) 
final_pred <- as.data.frame(final_pred)

#Checking performance
postResample(final_pred, df$Chance_of_Admit)

# RMSE   Rsquared        MAE 
# 0.04812498 0.88841279 0.03133679

#Our model performance is improved with the whole data.


#Lets Ans the question (.90)

#   GRE TOEFL University_Rating SOP LOR CGPA Research
# 6 330   115                 5 4.5   3 9.34        1


df_pred1 <- df[c(6),-c(8)]
final_pred <- predict(model_fit, df_pred1) 
final_pred
#.87

final_pred_rf <- predict(model_fit_rf, df_pred1) 
final_pred_rf
#.8989









