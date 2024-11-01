# loading the working directory
setwd("D:/upgrad/capstone project/")

#Loading nessesary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(Information)
library(MASS)
library(car)
library(caret)
library(e1071)
library(caTools)
library(ROCR)
library(DMwR)
library(randomForest)

#Loading Data
demographic <- read.csv("Demographic data.csv",header = TRUE, na.strings = c("NA",""))
credit_bureau <- read.csv("Credit Bureau data.csv",header = TRUE, na.strings = c("NA",""))

# having a peek at the structure of data inside both the files
str(demographic)
str(credit_bureau)

# taking a look at the summary of each data
summary(demographic)
summary(credit_bureau)

# we can merge the data about Application Id column
# but before that we need to make sure that it is same in both the datasets

# checking the dimension of both the datasets
dim(demographic)
# 71295    12
dim(credit_bureau)
# 71295    19
#no of records is same in both the datasets

setdiff(demographic$Application.ID,credit_bureau$Application.ID)
# 0 means both the columns are same, so we can merge both

# merging both the data sets
main_data<- merge(demographic,credit_bureau)

#keeping a copy of the original merged data
merged_complete_data <- main_data

dim(main_data)
# 71299    29
# there is an increase of 4 records

# we need to check for the duplicates records in the dataset
sum(duplicated(main_data))
# 0 means there are no two rows which are same

######################################################################################
########################## Checking the data for Quality Issues ######################
############# And Treating the Data for all the Quality issues Found #################

# checking the application ID for duplicate values
sum(duplicated(main_data$Application.ID))
# 7 records are found duplicate

duplicate_appId <- main_data$Application.ID[duplicated(main_data$Application.ID)]
# 653287861 671989187 671989187 671989187 765011468 765011468 765011468

# removing the duplicate rows from application id
main_data <- main_data[-which(duplicated(main_data$Application.ID)),]

# checking the dataset for the NA values
sapply(main_data, function(x) sum(is.na(x)))
# few  of the columns contain NA values
# performance tag has 1425 NA values - 2% of the whole data
# Gender contains 2 NAs
# Marital Status contains 6 NAs
# Education contains 119 NAs
# Profession contains 14 NAs
# Type of Residence contains 8 NAs
# Average CC utilization contains 1058 NAs
# No of trades opened in last 6 months contains 1 NA
# Presence of Open Home loan contains 272 NAs
# Outstanding Balance contains 272 NAs

# checking the dataset for blank values
sapply(main_data, function(x) sum( trimws(x) == "",na.rm = TRUE))
# there are no blank values present

# %age of NA values in the data set
sum(is.na(main_data))/nrow(main_data)
# 4.5% of data contains NA values

# %age of NA values present in the Performance Tag column
sum(is.na(main_data$Performance.Tag))/nrow(main_data)
# 2% of the values are just present in the Performance Tag, which we need to remove
# so we can safely remove all the rows containing NA values

# removing all the records having missing values
main_data<-main_data[complete.cases(main_data),]

# Segregating the data into categorical and continuous features
cat <- c("Performance.Tag","Gender","Marital.Status..at.the.time.of.application.",
         "Education","Profession","Type.of.residence","Presence.of.open.home.loan",
         "Presence.of.open.auto.loan")

cont <- c("Age","Income","No.of.dependents","No.of.months.in.current.residence",
          "No.of.months.in.current.company","No.of.times.90.DPD.or.worse.in.last.6.months",
          "No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months",
          "No.of.times.90.DPD.or.worse.in.last.12.months","No.of.times.60.DPD.or.worse.in.last.12.months",
          "No.of.times.30.DPD.or.worse.in.last.12.months","Avgas.CC.Utilization.in.last.12.months",
          "No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months",
          "No.of.PL.trades.opened.in.last.6.months","No.of.PL.trades.opened.in.last.12.months",
          "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
          "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
          "Outstanding.Balance","Total.No.of.Trades")

main_cat <- main_data[,cat]
main_cont <- main_data[,cont]

# checking for any outliers in the continuous data using quantiles and boxplots
quantiles_df <- sapply(main_cont, 
                       function(x) quantile(x,seq(0,1,.1)))
summary(quantiles_df)
# Age,Income contains negative values

# capping the age and income
main_cont$Age[main_cont$Age < 15 ] <- 15
main_cont$Income[main_cont$Income <= 0 ] <- 0

# converting categorical attributes to factor
main_cat<- data.frame(sapply(main_cat, function(x) factor(x)))
str(main_cat)

######################################################################################
############################## Univariate Analysis ##################################

# Histogram and Boxplots for continuous variables 
box_theme_x<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                    axis.ticks=element_blank(), axis.text=element_blank())

plot_fun_continuous <- function(cont_col_name,var_name){
  
  plot_grid(ggplot(main_cont, aes(cont_col_name))+ geom_histogram(binwidth = 10) +  labs(x = var_name),
            ggplot(main_cont, aes(x="",y=cont_col_name))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, 
            align = "v",ncol = 1)
  
}

plot_fun_continuous(main_cont$Age,"Age")
plot_fun_continuous(main_cont$No.of.dependents,"No.of.dependents")
plot_fun_continuous(main_cont$Income,"Income")
plot_fun_continuous(main_cont$No.of.months.in.current.residence,"No.of.months.in.current.residence")
plot_fun_continuous(main_cont$No.of.months.in.current.company,"No.of.months.in.current.company")
plot_fun_continuous(main_cont$No.of.times.90.DPD.or.worse.in.last.6.months,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_fun_continuous(main_cont$No.of.times.60.DPD.or.worse.in.last.6.months,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_fun_continuous(main_cont$No.of.times.30.DPD.or.worse.in.last.6.months,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_fun_continuous(main_cont$No.of.times.90.DPD.or.worse.in.last.12.months,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_fun_continuous(main_cont$No.of.times.60.DPD.or.worse.in.last.12.months,"No.of.times.60.DPD.or.worse.in.last.12.months")
plot_fun_continuous(main_cont$No.of.times.30.DPD.or.worse.in.last.12.months,"No.of.times.30.DPD.or.worse.in.last.12.months")
plot_fun_continuous(main_cont$Avgas.CC.Utilization.in.last.12.months,"Avgas.CC.Utilization.in.last.12.months")
plot_fun_continuous(main_cont$No.of.trades.opened.in.last.6.months,"No.of.trades.opened.in.last.6.months")
plot_fun_continuous(main_cont$No.of.trades.opened.in.last.12.months,"No.of.trades.opened.in.last.12.months")
plot_fun_continuous(main_cont$No.of.PL.trades.opened.in.last.6.months,"No.of.PL.trades.opened.in.last.6.months")
plot_fun_continuous(main_cont$No.of.PL.trades.opened.in.last.12.months,"No.of.PL.trades.opened.in.last.12.months")
plot_fun_continuous(main_cont$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans")
plot_fun_continuous(main_cont$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans")
plot_fun_continuous(main_cont$Outstanding.Balance,"Outstanding.Balance")
plot_fun_continuous(main_cont$Total.No.of.Trades,"Total.No.of.Trades")

# for categorical variables
univariate_cat <- function(col_name,var_name) {
  ggplot(main_cat,aes(x = col_name)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Percent", x = var_name)+theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1))
}

plot_grid(univariate_cat(main_cat$Gender,"Gender"),univariate_cat(main_cat$Marital.Status..at.the.time.of.application.,"Marital_status"),
          univariate_cat(main_cat$Education,"Education"),align = "h")
plot_grid(univariate_cat(main_cat$Profession,"Profesison"),
          univariate_cat(main_cat$Type.of.residence,"Type of Residence"),
          univariate_cat(main_cat$Presence.of.open.home.loan,"Presence of Home Loan"),
          univariate_cat(main_cat$Presence.of.open.auto.loan,"Presence of Auto load"),align = "h")


#####################################################################################
############################ WOE/IV analysis ############################

# following is the interpretation of the IV value
#< 0.02	useless for prediction
#0.02 to 0.1	Weak predictor
#0.1 to 0.3	Medium predictor
#0.3 to 0.5	Strong predictor

IV <- create_infotables(data=main_data, y="Performance.Tag", bins=10, parallel=T)
head(IV)

IV_table <- data.frame(IV$Summary)

IV_table_sorted <- IV_table[order(-IV_table$IV), ]

# plotting the IV values of variables in order of their significance of predictive power
ggplot(IV_table_sorted, aes(x = reorder(Variable,IV), y = IV))+
  geom_bar(width = 0.5, stat = "identity") +ggtitle("Information Value") +
  theme(plot.title = element_text(size = 10) ,axis.text.x = element_text(angle = 90))
  

# following variable seems to be good predictors
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# Avgas.CC.Utilization.in.last.12.months
# No.of.PL.trades.opened.in.last.12.months
# No.of.trades.opened.in.last.12.months
# Outstanding.Balance
# Total.No.of.Trades
# No.of.times.30.DPD.or.worse.in.last.6.months
# No.of.PL.trades.opened.in.last.6.months
# No.of.times.90.DPD.or.worse.in.last.12.months
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
# No.of.times.60.DPD.or.worse.in.last.6.months
# No.of.times.30.DPD.or.worse.in.last.12.months
# No.of.trades.opened.in.last.6.months
# No.of.times.60.DPD.or.worse.in.last.12.months
# No.of.times.90.DPD.or.worse.in.last.6.months

#####################################################################################
################# Preparing the DataSet for Model Designing #########################

# removing the Performance Tag from the main_cat data as we do not want a dummy for that
main_cat <- main_cat[,-1]

# creating the dummy variables
dummies<- data.frame(sapply(main_cat,function(x) data.frame(model.matrix(~x-1,data =main_cat))[,-1]))

#taking the performance tag variable from main_data
Performance.Tag <- as.factor(main_data$Performance.Tag)

# creating the final data set
main_final <- cbind(Performance.Tag,dummies,main_cont)

#####################################################################################

################# Creating the Normal Test and Train Dataset #########################
set.seed(100)
trainindices= sample(1:nrow(main_final), 0.7*nrow(main_final))
train = main_final[trainindices,]
test = main_final[-trainindices,]
#####################################################################################3

######################### Logistic Regressing ########################################
# the very first model containing all the variables against target variable
model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)

# using STEPAIC to find to remove insignificant features
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
sort(vif(model_2))

#removing No.of.times.30.DPD.or.worse.in.last.12.months based on high vif and less significant p-value 
model_3 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + No.of.months.in.current.residence + 
                 No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_3)
sort(vif(model_3))

#removing No.of.times.60.DPD.or.worse.in.last.6.months based on high vif and less significant p-value 
model_4 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + No.of.months.in.current.residence + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_4)
sort(vif(model_4))

#removing No.of.trades.opened.in.last.12.months based on vif value 
model_5 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + No.of.months.in.current.residence + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_5)
sort(vif(model_5))

#removing No.of.PL.trades.opened.in.last.6.months based on less significant p-value 
model_6 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + No.of.months.in.current.residence + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_6)
sort(vif(model_6))

#removing No.of.months.in.current.residence based on less significant p-value 
model_7 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_7)
sort(vif(model_7))

#removing No.of.times.90.DPD.or.worse.in.last.12.months based on less significant p-value 
model_8 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_8)
sort(vif(model_8))

#removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. based on less significant p-value 
model_9 <- glm(formula = Performance.Tag ~ Profession.xSE + Income + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_9)
sort(vif(model_9))

#removing Income based on less significant p-value 
model_10 <- glm(formula = Performance.Tag ~ Profession.xSE + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., 
               family = "binomial", data = train)
summary(model_10)
sort(vif(model_10))

final_model_logistic <- model_10
# following are the significant variable predicted by our model 
#"Profession.xSE","No.of.times.30.DPD.or.worse.in.last.6.months",
#"Avgas.CC.Utilization.in.last.12.months","No.of.PL.trades.opened.in.last.12.months",
#"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."


# predicted probabilities of default for test data
test_pred = predict(final_model_logistic, type = "response",newdata = test[,-1])

# Let's see the summary 
summary(test_pred)
test$prob <- test_pred
View(test)

test_actual_def <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

#######################################################################
# finding the optimal cutoff value
perform_fn <- function(cutoff) 
{
  predicted_def <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_def, test_actual_def, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
cutoff
# optimal cutoff obtained by tuning is 0.498

test_cutoff_def <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 66.9%
# Sensitivity = 57.6%
# Specificity = 67.4%

############################################################################################
################################# Model Evaluation##################################
########################## KS -statistic - Test Data ###############################

test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
test_actual_def <- ifelse(test_actual_def=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_def, test_actual_def)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.25
# KS- Stat value is 25%

# finding the area under the ROC curve
ROC <- performance(pred_object_test, measure = "auc")
area <- ROC@y.values[[1]]
area 
# 0.625

# plotting the ROC curve
tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) + geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression Model") +
  theme(axis.text.x=element_text(hjust=1))

 
####################################################################
# Lift & Gain Chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile = lift(test_actual_def, test_pred, groups = 10)
default_decile

# Plotting Gain Chart
ggplot(default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_line(data=default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Logistic Model's Gain Chart")


# Plotting Lift Chart
ggplot(default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+ geom_line(data=default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Logistic Model's Lift Chart")

##############################################################################################
# Performing the cross validation on the final Logistic Model
# Load data
# Select statistically significant variables
cross_data <- subset(main_final,select=c("Performance.Tag","Profession.xSE","No.of.times.30.DPD.or.worse.in.last.6.months",
                                         "Avgas.CC.Utilization.in.last.12.months","No.of.PL.trades.opened.in.last.12.months",
                                         "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."))

cross_data <- main_final

# False positive rate
fpr <- NULL

# False negative rate
fnr <- NULL

# Number of iterations
k <- 500

# Accuracy
acc <- NULL
sens <- NULL
spec <- NULL
set.seed(100)

for(i in 1:k)
{
  # Train-test splitting
  # 80% of samples -> fitting
  # 20% of samples -> testing
  smp_size <- floor(0.80 * nrow(cross_data))
  index <- sample(seq_len(nrow(cross_data)),size=smp_size)
  train_cross <- cross_data[index, ]
  test_cross <- cross_data[-index, ]
  
  # Predict results
  results_prob <- predict(final_model_logistic,newdata = test_cross[,-1],type='response')
  
  # using the best cutoff for the probabilities
  results <- factor(ifelse(results_prob > cutoff,1,0),levels = 0:1)
  
  # Actual answers
  answers <- factor(test_cross$Performance.Tag,levels = 0:1)
  
  # Confusion matrix
  cm <- confusionMatrix(data=results, reference=answers)
  acc[i] <- cm$overall[1]
  sens[i] <- cm$byClass[1]
  spec[i] <- cm$byClass[2]
  fpr[i] <- cm$table[2]/(nrow(cross_data)-smp_size)
  fnr[i] <- cm$table[3]/(nrow(cross_data)-smp_size)
}

# Average accuracy,sensitivity and specificity of the model
mean(acc) # 66.9%
mean(sens) # 67.5%
mean(spec) # 54.9
par(mfcol=c(1,2))

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Boxplot-Accuracy')

# plots of fpr and fnr
mean(fpr) #31.1%
mean(fnr) # 1.9%

hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)

# the mean accuracy, specificity and sensitivity after the cross validation suggests
# that our logistic regression model is very stable

#####################################################################################
########################################################################################
# Logistic regression using the SMOTE training data
################# Creating the Normal Test and Train Dataset #########################
set.seed(100)
trainindices= sample(1:nrow(main_final), 0.7*nrow(main_final))
smote_sample = main_final[trainindices,]
test_smote = main_final[-trainindices,]
train_smote <- SMOTE(Performance.Tag ~ ., smote_sample, perc.over = 100, perc.under=200)
#####################################################################################3
# the very first model containing all the variables against target variable
model_smote_1 <- glm(Performance.Tag~.,data=train_smote,family = "binomial")
summary(model_smote_1)

# using STEPAIC to find to remove insignificant features
model_smote_2 <- stepAIC(model_smote_1,direction = "both")
summary(model_smote_2)
sort(vif(model_smote_2))

# removing No.of.trades.opened.in.last.12.months based on high vif and less significant p-value
model_smote_3 <- glm(formula = Performance.Tag ~ Education.xPhd + Presence.of.open.auto.loan + 
                 Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.12.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance, family = "binomial", data = train_smote)
summary(model_smote_3)
sort(vif(model_smote_3))


# removing No.of.times.60.DPD.or.worse.in.last.12.months based on high vif and less significant p-value
model_smote_4 <- glm(formula = Performance.Tag ~ Education.xPhd + Presence.of.open.auto.loan + 
                 Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance, family = "binomial", data = train_smote)
summary(model_smote_4)
sort(vif(model_smote_4))


# removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. based on high vif and less significant p-value
model_smote_5 <- glm(formula = Performance.Tag ~ Education.xPhd + Presence.of.open.auto.loan + 
                       Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                       No.of.times.30.DPD.or.worse.in.last.6.months + 
                       Avgas.CC.Utilization.in.last.12.months + 
                       No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       Outstanding.Balance, family = "binomial", data = train_smote)
summary(model_smote_5)
sort(vif(model_smote_5))

# removing No.of.PL.trades.opened.in.last.6.months based on high vif and less significant p-value
model_smote_6 <- glm(formula = Performance.Tag ~ Education.xPhd + Presence.of.open.auto.loan + 
                       Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                       No.of.times.30.DPD.or.worse.in.last.6.months + 
                       Avgas.CC.Utilization.in.last.12.months + 
                       No.of.PL.trades.opened.in.last.12.months + 
                       No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                       Outstanding.Balance, family = "binomial", data = train_smote)
summary(model_smote_6)
sort(vif(model_smote_6))


# removing Education.xPhd based less significant p-value
model_smote_7 <- glm(formula = Performance.Tag ~ Presence.of.open.auto.loan + 
                 Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance, family = "binomial", data = train_smote)
summary(model_smote_7)
sort(vif(model_smote_7))

# removing No.of.months.in.current.company based on less significant p-value
model_smote_8 <- glm(formula = Performance.Tag ~ Presence.of.open.auto.loan + 
                 Income + No.of.months.in.current.residence + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance, family = "binomial", data = train_smote)
summary(model_smote_8)
sort(vif(model_smote_8))

# removing Outstanding.Balance based on less significant p-value
model_smote_9 <- glm(formula = Performance.Tag ~ Presence.of.open.auto.loan + 
                 Income + No.of.months.in.current.residence + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., family = "binomial", data = train_smote)
summary(model_smote_9)
sort(vif(model_smote_9))

final_logistic_model_smote <- model_smote_9
# following are the features significant as per the model
#"Presence.of.open.auto.loan"
#"Income + No.of.months.in.current.residence"
#"No.of.times.30.DPD.or.worse.in.last.6.months" 
#"Avgas.CC.Utilization.in.last.12.months"
#"No.of.PL.trades.opened.in.last.12.months" 
#"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."

#predicted probabilities of default for test data
test_pred_smote = predict(final_logistic_model_smote, type = "response",newdata = test_smote[,-1])

# Let's see the summary 

summary(test_pred_smote)
test_smote$prob <- test_smote
View(test_smote)

test_actual_def_smote <- factor(ifelse(test_smote$Performance.Tag==1,"Yes","No"))

#######################################################################

perform_fn <- function(cutoff) 
{
  predicted_def <- factor(ifelse(test_pred_smote >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_def, test_actual_def_smote, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.99,length=100)

OUT_smote = matrix(0,100,3)

for(i in 1:100)
{
  OUT_smote[i,] = perform_fn(s[i])
} 


plot(s, OUT_smote[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_smote[,2],col="darkgreen",lwd=2)
lines(s,OUT_smote[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff_smote <- s[which(abs(OUT_smote[,1]-OUT_smote[,2])==min(abs(OUT_smote[,1]-OUT_smote[,2])))]
cutoff_smote

# the optimal cutoff value is 0.534

test_cutoff_def_smote <- factor(ifelse(test_pred_smote >=cutoff_smote, "Yes", "No"))

conf_final_smote <- confusionMatrix(test_cutoff_def_smote, test_actual_def_smote, positive = "Yes")

acc <- conf_final_smote$overall[1]

sens <- conf_final_smote$byClass[1]

spec <- conf_final_smote$byClass[2]
conf_final_smote

# Accuracy = 62.42%
# Sensitivity = 61.1%
# Specificity = 62.4%

############################################################################################
############# Model Evaluation##################
### KS -statistic - Test Data ######

test_cutoff_def_smote <- ifelse(test_cutoff_def_smote=="Yes",1,0)
test_actual_def_smote <- ifelse(test_actual_def_smote=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_def_smote, test_actual_def_smote)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.23

# finding the area under the ROC curve
ROC <- performance(pred_object_test, measure = "auc")
area <- ROC@y.values[[1]]
area 
#0.61

# plotting the ROC curve
tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) + geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
  labs(x="False Positive Rate",y="True Positive Rate",title="ROC Curve for Logistic Regression Model using SMOTE") +
  theme(axis.text.x=element_text(hjust=1))


# Lift & Gain Chart 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

default_decile_smote = lift(test_actual_def_smote, test_cutoff_def_smote, groups = 10)
default_decile_smote

# Plotting Gain Chart
ggplot(default_decile_smote, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_line(data=default_decile_smote,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Logistic Model using SMOTE - Gain Chart")


# Plotting Lift Chart
ggplot(default_decile_smote, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+ geom_line(data=default_decile_smote,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Logistic Model using SMOTE - Lift Chart")

##############################################################################################
# Performing the cross validation on the final model

# Load data

cross_data_smote <- main_final
#-------------------------------------------------------------------------------
# False positive rate
fpr <- NULL

# False negative rate
fnr <- NULL

# Number of iterations
k <- 500


# Accuracy
acc <- NULL
sens <- NULL
spec <- NULL
set.seed(100)

for(i in 1:k)
{
  # Train-test splitting
  # 80% of samples -> fitting
  # 20% of samples -> testing
  smp_size <- floor(0.80 * nrow(cross_data_smote))
  index <- sample(seq_len(nrow(cross_data_smote)),size=smp_size)
  train_smote <- cross_data_smote[index, ]
  test_smote <- cross_data_smote[-index, ]
  
  # Predict results
  results_prob <- predict(final_logistic_model_smote,newdata = test_smote[,-1],type='response')
  
  # If prob > 0.5 then 1, else 0
  results <- factor(ifelse(results_prob > 0.5,1,0),levels = 0:1)
  
  # Actual answers
  answers <- factor(test_smote$Performance.Tag,levels = 0:1)
  
  # Confusion matrix
  cm <- confusionMatrix(data=results, reference=answers)
  acc[i] <- cm$overall[1]
  sens[i] <- cm$byClass[1]
  spec[i] <- cm$byClass[2]
  fpr[i] <- cm$table[2]/(nrow(cross_data_smote)-smp_size)
  fnr[i] <- cm$table[3]/(nrow(cross_data_smote)-smp_size)
  
}

# Average accuracy,sensitivity and specificity of the model
mean(acc) # 56.3%
mean(sens) # 55.8%
mean(spec) # 68.3%

par(mfcol=c(1,2))

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='boxplot-Accuracy')

# plots of fpr and fnr
mean(fpr)
mean(fnr)
hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)

####################################################################################
############################### Random Forrest ##################################### 
set.seed(100)
main_final_rf <- main_final
trainindices_rf= sample(1:nrow(main_final_rf), 0.7*nrow(main_final_rf))
train_rf = main_final_rf[trainindices_rf,]
test_rf = main_final_rf[-trainindices_rf,]

model_rf <- randomForest(Performance.Tag ~.,data = train_rf,proximity = F,do.trace = T,
                             mtry = 5,ntree=1000,importance = TRUE)

summary(model_rf)
varImpPlot(model_rf,type=2)

test_pred_rf <- predict(model_rf, test_rf, type = "prob")
test_actual_rf <- factor(ifelse(test_rf$Performance.Tag==1,"yes","no"))

#finding the optimal cutoff value for probalility
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(test_pred_rf[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_actual_rf, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs
plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
cutoff_rf
# 0.039

# optimal cutoff value is 0.039
test_pred_optimal<- factor(ifelse(test_pred_rf[, 2] >= cutoff_rf, "yes", "no"))
conf_rf <- confusionMatrix(test_pred_optimal, test_actual_rf, positive = "yes")
conf_rf

# Accuracy : 50.4%
# Sensitivity : 73.6%       
# Specificity : 49.4%

####################### KS - statistic -Random Forest - Test Data######################## #######################

test_actual_rf<-ifelse(test_actual_rf == "yes", 1,0)
test_pred_optimal<-ifelse(test_pred_optimal == "yes", 1,0)

pred_object_test<- prediction(test_pred_optimal, test_actual_rf)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #.23
#KS-statistic is 23% 

# find area under the roc curve
roc <- performance(pred_object_test, measure = "auc")
area <- roc@y.values[[1]]
area 

# 0.61

tpr_fpr_table <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(tpr_fpr_table ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1))) +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Random Forest") +
  theme(axis.text.x=element_text(hjust=1)) 

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Default_decile = lift(test_actual_rf, test_pred_optimal, groups = 10)
Default_decile

# Plotting Gain Chart
ggplot(Default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_line(data=Default_decile,aes(x=bucket,y=Gain),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")


# Plotting Lift Chart
ggplot(Default_decile, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_line(data=Default_decile,aes(x=bucket,y=Cumlift),color='red',size=1, group = 1)+
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")

###############################################################################################
################ Scorecard Using Logistic Regression Model ##########################
# Build an application scorecard with the good to bad odds of 10 to 1 
# at a score of 400 doubling every 20 points.  
final_dataset <- main_final
final_dataset$perdict_default  <- predict(final_model_logistic, type = "response", newdata = final_dataset[,-1])
final_dataset$predict_NonDefault <- 1 - final_dataset$perdict_default
final_dataset$odds <-  log(final_dataset$predict_NonDefault/final_dataset$perdict_default)

Offset = 400
PDO = 20
odds = 10
Factor = PDO/log(2)
Factor  #28.8539

final_dataset$Score <- floor(Offset + (Factor * final_dataset$odds))

str(final_dataset$Score)
summary(final_dataset$Score)
# min - 428 to max - 514

ggplot(final_dataset,aes(x = Score))+ geom_bar(aes(fill = factor(Performance.Tag)))
# graphically we can see that there is a dip in the scores areound 500,
# this can be choosen as the cutoff value

cutoff_score =500

defaulters_below_cutoff<-length(which(final_dataset$Performance.Tag==1 & final_dataset$Score<500))
total_defaulters<-length(which(final_dataset$Performance.Tag==1))

predicted_defaulters_below_cutoff_score<-ceiling((defaulters_below_cutoff/total_defaulters)*100)
predicted_defaulters_below_cutoff_score
# cutoff score of 500 covers 82% of the defaulters                                                                                                                                                                                              label=paste("Defaults covered by 412 cut off : " ,predicted_defaulters_below_cutoff_score,"%"))

###############################################################################################
################# Predicting score for rejected applicants###############################
#########################################################################################
# Assuming that the NA values in the Performance Correspond the rejected Applications
rej_apps <- merged_complete_data[which( is.na(merged_complete_data$Performance.Tag)),]
rej_apps <- rej_apps[,-1]
str(rej_apps)

rej_cat <- c("Gender","Marital.Status..at.the.time.of.application.",
              "Education","Profession","Type.of.residence","Presence.of.open.home.loan",
              "Presence.of.open.auto.loan")

rej_apps_cat <- rej_apps[,rej_cat]
sapply(rej_apps_cat, function(x) sum(is.na(x)))
#there are present NA values in Education and Profession Column
# Imputing the NA value in Education as Masters
# and in Profession's column making it as SAL
rej_apps_cat$Education[which(is.na(rej_apps_cat$Education))] <- "Masters" 
rej_apps_cat$Profession[which(is.na(rej_apps_cat$Profession))] <- "SAL"

rej_apps_cont <- rej_apps[,cont]

# creating dummy variables
rej_dummies <- data.frame(sapply(rej_apps_cat,function(x) data.frame(model.matrix(~x-1,data =rej_apps_cat))[,-1])) 

# combine all relevant columns to build final training data
rej_final<- cbind(rej_apps_cont,rej_dummies)
str(rej_final)

rej_final$perdict_default  <- predict(final_model_logistic, type = "response", newdata = rej_final)
rej_final$predict_NonDefault <- 1 - rej_final$perdict_default
rej_final$odds <-  log(rej_final$predict_NonDefault/rej_final$perdict_default)

rej_final$Score = floor(Offset + (Factor*rej_final$odds))

summary(rej_final$Score)

length(which(rej_final$Score<500))/nrow(rej_final) # 0.97
# which suggest that with the final Logistic model designed, we are able to predict
# the rejected defaultes with an accuracy of 97%

cutoff_score= 500
ggplot(rej_final, aes(x = Score)) +geom_bar()+geom_vline(aes(xintercept = cutoff_score,col="red"))+labs(x="Score",y="Count",title="Score Distribution of Actual Rejected applications")+annotate("text", x=380,y=1, colour = "white",hjust=0, vjust=0, size=7,
                                                                                                                                                                                                         label=paste("Corect rejections by score card% =", length(which(rej_final$Score<500))/nrow(rej_final)*100)) 
#################### Financial Benefit Of the Model ###########################

# Financial Benefit of the designed model will be in terms of either
# 1. decreasing the rejection the non-defaulters
# 2. increasing the rejection of defaulters
approval_rate <-(nrow(merged_complete_data) -nrow(rej_apps))/nrow(merged_complete_data) *100

approval_rate
# current approval rate : 98%

out_bal_def <- main_data$Outstanding.Balance[which(main_data$Performance.Tag==1)]

credit_loss_due_to_approved_def<- sum(out_bal_def)
# 3689945678

# default_users_ID <- master_data_backup$Application.ID [which(master_data_backup$Performance.Tag==1)]

out_bal_def_final_data<-final_dataset$Outstanding.Balance[which(final_dataset$Performance.Tag==1)]
scores_final_data<-final_dataset$Score[which(final_dataset$Performance.Tag==1)]    

# outstanding_ref <- cbind(default_users_ID,default_users_outstanding,scale(default_users_outstanding),t1,t2)
outstanding_df <- cbind(out_bal_def,out_bal_def_final_data,scores_final_data)

possible_defaults_with_more_than_500_score<-data.frame(subset(outstanding_df,scores_final_data>500))

sum(possible_defaults_with_more_than_500_score$out_bal_def)
# Credit loss using our Model's approval rate - 545947417
# this is roughtly 14% of the actual credit loss

nrow(data.frame(subset(final_dataset,Score>500)))/nrow(final_dataset)
# approval rate using our model : 43%

# the net credit loss  is much lesser, 314 crores lesser than the actual loss!
# but there has to be the tradeoff between the approvals as well, which in our case goes down to
# just 43%. In this case we can increase the cutoff score to be more than 500.

possible_defaults_with_more_than_480_score<-data.frame(subset(outstanding_df,scores_final_data>480))

sum(possible_defaults_with_more_than_480_score$out_bal_def)
# credit loss at cutoff score == 480 is 2075459342
# this is roughly 56% of the original credit loss

nrow(data.frame(subset(final_dataset,Score>480)))/nrow(final_dataset)
# New approval rate : 76%

# This cutoff score can be adjusted to the business needs,
# keeping in mind that decreasing the cutoff score will lead to increase in approval rate and
# credit loss and vice versa