

#Loading nessesary packages
library(MASS)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)
library(DMwR)
library(gridExtra)
library(Information)
library(scorecard)
library(cowplot)
library(caTools)

#Loading Data
demographic <- read.csv("Demographic data.csv",header = TRUE, na.strings = "NA",stringsAsFactors = FALSE)
credit_bureau <- read.csv("Credit Bureau data.csv",header = TRUE, na.strings = "NA", stringsAsFactors = FALSE)

# having a peek at the structure of data inside both the files
str(demographic)
str(credit_bureau)

# business understanding
# write something about the data present in both the files

# Application Id seems to be a common key between both the datasets, so we will be
# collating the data on Application Id
# making a pre-check whether the Application Id is same in both the Data Sets
length(unique(demographic$Application.ID)) # 71292 suggests that there are present 3 duplicates as well
length(unique(credit_bureau$Application.ID)) # 71292 suggests that there are present 3 duplicates as well

setdiff(demographic$Application.ID,credit_bureau$Application.ID)
setdiff(demographic$Performance.Tag,credit_bureau$Performance.Tag)
# 0 suggests that Application Ids and Performance Tag are identical in both the data sets

# merging both the data sets around the Application Id and Performance Tag column
demo_cb_merged<- merge(demographic,credit_bureau)

length(unique(demo_cb_merged$Application.ID))
# 71292 suggests there are present some duplicate values

# having a peek at the structure of the merged file
str(demo_cb_merged)
# write about the columns and features available in the file


# checking the performance tag column
sum(demo_cb_merged$Performance.Tag,na.rm = TRUE)/nrow(demo_cb_merged)
sum(demo_cb_merged$Performance.Tag,na.rm = TRUE)
# only 4% of the values have Performance Tag are one ie 2984/71295

# Following are the continuous variables
# "Age","No.of.dependents","Income","No.of.months.in.current.residence",
# "No.of.months.in.current.company","No.of.times.90.DPD.or.worse.in.last.6.months",                   
# "No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months",
# "No.of.times.90.DPD.or.worse.in.last.12.months","No.of.times.60.DPD.or.worse.in.last.12.months",
# "No.of.times.30.DPD.or.worse.in.last.12.months","Avgas.CC.Utilization.in.last.12.months",
# "No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months",
# "No.of.PL.trades.opened.in.last.6.months","No.of.PL.trades.opened.in.last.12.months"
# "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
# "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
# "Presence.of.open.home.loan","Outstanding.Balance","Total.No.of.Trades",
# "Presence.of.open.auto.loan"

continuous <- c("Age","No.of.dependents","Income","No.of.months.in.current.residence",
"No.of.months.in.current.company","No.of.times.90.DPD.or.worse.in.last.6.months",
"No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months",
"No.of.times.90.DPD.or.worse.in.last.12.months","No.of.times.60.DPD.or.worse.in.last.12.months",
"No.of.times.30.DPD.or.worse.in.last.12.months","Avgas.CC.Utilization.in.last.12.months",
"No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months",
"No.of.PL.trades.opened.in.last.6.months","No.of.PL.trades.opened.in.last.12.months",
"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
"Outstanding.Balance","Total.No.of.Trades")

# Following are the categorical variables
# "Gender","Marital.Status..at.the.time.of.application.","Education"                                                      
# "Profession","Type.of.residence","Presence.of.open.home.loan","Presence.of.open.auto.loan"

categorical <- c("Gender","Marital.Status..at.the.time.of.application.","Education",
                 "Profession","Type.of.residence","Presence.of.open.home.loan","Presence.of.open.auto.loan")

demo_cb_continuous <- cbind(demo_cb_merged[,continuous],demo_cb_merged$Performance.Tag)
demo_cb_categorical <- cbind(demo_cb_merged[,categorical],demo_cb_merged$Performance.Tag)

library(gdata)
demo_cb_continuous <- rename.vars(demo_cb_continuous, from = "demo_cb_merged$Performance.Tag", to = "Performance.Tag")
demo_cb_categorical <- rename.vars(demo_cb_categorical, from = "demo_cb_merged$Performance.Tag", to = "Performance.Tag")

str(demo_cb_categorical)
# "Presence.of.open.home.loan","Presence.of.open.auto.loan" needs to be changed to
#  factors
demo_cb_categorical$Presence.of.open.auto.loan<- ifelse(demo_cb_categorical$Presence.of.open.auto.loan==1,"Yes","No")
demo_cb_categorical$Presence.of.open.home.loan<- ifelse(demo_cb_categorical$Presence.of.open.home.loan==1,"Yes","No")

str(demo_cb_categorical)
###################################################################################
########################## Data Preparation and EDA ###############################

# there were found duplicate Application Ids, lets find those and remove those
# de-duplicating the data

sum(duplicated(demo_cb_merged$Application.ID))
# 7 in both the data combined

duplicates <- demo_cb_merged$Application.ID[duplicated(demo_cb_merged$Application.ID)]
duplicates
# following are the duplicates
# 653287861 671989187 671989187 671989187 765011468 765011468 765011468

demo_cb_merged <- demo_cb_merged[-which(duplicated(demo_cb_merged$Application.ID)),]
sum(duplicated(demo_cb_merged$Application.ID))
# 0 , means all the duplicates are removed

# univariate analysis
# Histogram and Boxplots for numeric variables 
box_theme_x<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_fun_continuous <- function(cont_col_name,var_name){
  
  plot_grid(ggplot(demo_cb_continuous, aes(cont_col_name))+ geom_histogram(binwidth = 10) +  labs(x = var_name),
            ggplot(demo_cb_continuous, aes(x="",y=cont_col_name))+ geom_boxplot(width=0.1)+coord_flip()+box_theme_x, 
            align = "v",ncol = 1)
  
}

plot_fun_continuous(demo_cb_continuous$Age,"Age")
plot_fun_continuous(demo_cb_continuous$No.of.dependents,"No.of.dependents")
plot_fun_continuous(demo_cb_continuous$Income,"Income")
plot_fun_continuous(demo_cb_continuous$No.of.months.in.current.residence,"No.of.months.in.current.residence")
plot_fun_continuous(demo_cb_continuous$No.of.months.in.current.company,"No.of.months.in.current.company")
plot_fun_continuous(demo_cb_continuous$No.of.times.90.DPD.or.worse.in.last.6.months,"No.of.times.90.DPD.or.worse.in.last.6.months")
plot_fun_continuous(demo_cb_continuous$No.of.times.60.DPD.or.worse.in.last.6.months,"No.of.times.60.DPD.or.worse.in.last.6.months")
plot_fun_continuous(demo_cb_continuous$No.of.times.30.DPD.or.worse.in.last.6.months,"No.of.times.30.DPD.or.worse.in.last.6.months")
plot_fun_continuous(demo_cb_continuous$No.of.times.90.DPD.or.worse.in.last.12.months,"No.of.times.90.DPD.or.worse.in.last.12.months")
plot_fun_continuous(demo_cb_continuous$No.of.times.60.DPD.or.worse.in.last.12.months,"No.of.times.60.DPD.or.worse.in.last.12.months")
plot_fun_continuous(demo_cb_continuous$No.of.times.30.DPD.or.worse.in.last.12.months,"No.of.times.30.DPD.or.worse.in.last.12.months")
plot_fun_continuous(demo_cb_continuous$Avgas.CC.Utilization.in.last.12.months,"Avgas.CC.Utilization.in.last.12.months")
plot_fun_continuous(demo_cb_continuous$No.of.trades.opened.in.last.6.months,"No.of.trades.opened.in.last.6.months")
plot_fun_continuous(demo_cb_continuous$No.of.trades.opened.in.last.12.months,"No.of.trades.opened.in.last.12.months")
plot_fun_continuous(demo_cb_continuous$No.of.PL.trades.opened.in.last.6.months,"No.of.PL.trades.opened.in.last.6.months")
plot_fun_continuous(demo_cb_continuous$No.of.PL.trades.opened.in.last.12.months,"No.of.PL.trades.opened.in.last.12.months")
plot_fun_continuous(demo_cb_continuous$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans")
plot_fun_continuous(demo_cb_continuous$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans")
plot_fun_continuous(demo_cb_continuous$Outstanding.Balance,"Outstanding.Balance")
plot_fun_continuous(demo_cb_continuous$Total.No.of.Trades,"Total.No.of.Trades")

# for categorical variables
bar_theme1<- theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5), 
                   legend.position="none")
plot_grid(ggplot(demo_cb_categorical, aes(x=Gender,fill=factor(Performance.Tag)))+ geom_bar(),
          ggplot(demo_cb_categorical, aes(x=Marital.Status..at.the.time.of.application.,fill=factor(Performance.Tag)))+ geom_bar(),
          ggplot(demo_cb_categorical, aes(x=Education,fill=factor(Performance.Tag)))+ geom_bar())
plot_grid(ggplot(demo_cb_categorical, aes(x=Profession,fill=factor(Performance.Tag)))+ geom_bar(),
          ggplot(demo_cb_categorical, aes(x=Type.of.residence,fill=factor(Performance.Tag)))+ geom_bar(),
          ggplot(demo_cb_categorical, aes(x=Presence.of.open.home.loan,fill=factor(Performance.Tag)))+ geom_bar(),
          ggplot(demo_cb_categorical, aes(x=Presence.of.open.auto.loan,fill=factor(Performance.Tag)))+ geom_bar())

# outliers and blank values are present in some of the columns

# checking for outliers using the quantiles
quantiles_df <- sapply(demo_cb_merged[,continuous], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T))

# only the age and income column have the outliers, containing negative values

nrow(demo_cb_merged[which(demo_cb_merged$Age <= 0),])/nrow(demo_cb_merged)
# 0.028% of the values are <=0, so we can remove them
# removing the negative and 0 values in age column
demo_cb_merged <- demo_cb_merged[-which(demo_cb_merged$Age <= 0),]

# for income column
nrow(demo_cb_merged[which(demo_cb_merged$Income <= 0),])/nrow(demo_cb_merged)
# 0.15% are <= 0, so we can safely remove these
demo_cb_merged <- demo_cb_merged[-which(demo_cb_merged$Income <= 0),]

#checking for the NAs  in the data
sapply(demo_cb_merged, function(x) sum(is.na(x)))
# performance tag has 1425 
# No. of Dependents have 3
# Avgas.CC.Utilization.in.last.12.months has 1058
# No.of.trades.opened.in.last.6.months has 1
# Presence.of.open.home.loan has 272
# Outstanding.Balance has 272

sum(is.na(demo_cb_merged))/nrow(demo_cb_merged)
# 4% of the total values are NAs hence we can safely remove all the rows containing NAs
# removing all the NA values
demo_cb_merged <- demo_cb_merged[!is.na(demo_cb_merged$Performance.Tag),]
demo_cb_merged <- demo_cb_merged[!is.na(demo_cb_merged$Presence.of.open.home.loan),]
demo_cb_merged <- demo_cb_merged[!is.na(demo_cb_merged$Outstanding.Balance),]
demo_cb_merged <- demo_cb_merged[!is.na(demo_cb_merged$No.of.trades.opened.in.last.6.months),]
demo_cb_merged <- demo_cb_merged[!is.na(demo_cb_merged$Avgas.CC.Utilization.in.last.12.months),]
demo_cb_merged <- demo_cb_merged[!is.na(demo_cb_merged$No.of.dependents),]

# all the NAs are removed now

#checking for the missing values
sapply(demo_cb_categorical, function(x) table(x))
# Gender has 1
# Marital status has 5
# Education has 117
# Profession has 11
# type of residence has 8

# taking an assumption that imputing 1-2% of the data won't interfere with the Models performance
demo_cb_merged$Gender[which(demo_cb_merged$Gender == "")] <- "M"
demo_cb_merged$Education[which(demo_cb_merged$Education == "")] <- "Others"
demo_cb_merged$Marital.Status..at.the.time.of.application.[which(demo_cb_merged$Marital.Status..at.the.time.of.application. == "")] <- "Married"
demo_cb_merged$Profession[which(demo_cb_merged$Profession == "")] <- "SAL"
demo_cb_merged$Type.of.residence[which(demo_cb_merged$Type.of.residence == "")] <- "Others"

# binning the data into appropriate groups rather than considering the values present
# in them as outliers. eg: average cc utilization has values exceeding 100 
# which as per the boxplots and the quantile will fall under the category of outliers
# but this could be a real life scenario where a user is excceding his card limit often

## Age binning
#demo_cb_merged$bin.age <- demo_cb_merged$Age
#demo_cb_merged$bin.age <- ifelse((demo_cb_merged$Age>=10 & demo_cb_merged$Age<=30) , 'Youngsters',demo_cb_merged$bin.age)
#demo_cb_merged$bin.age <- ifelse((demo_cb_merged$Age>30 & demo_cb_merged$Age<=55) , 'Adults',demo_cb_merged$bin.age)
#demo_cb_merged$bin.age <- ifelse((demo_cb_merged$Age>55) , 'Senior_Citizen',demo_cb_merged$bin.age)
#table(demo_cb_merged$bin.age)
#
#ggplot(demo_cb_merged, aes(x=bin.age,fill=factor(Performance.Tag)))+ geom_bar()
#
## Income binning
#demo_cb_merged$bin.income <- demo_cb_merged$Income
#demo_cb_merged$bin.income <- ifelse((demo_cb_merged$Income>=0 & demo_cb_merged$Income<=15) , 'Low_Income',demo_cb_merged$bin.income)
#demo_cb_merged$bin.income <- ifelse((demo_cb_merged$Income>15 & demo_cb_merged$Income<=30) , 'Medium_Income',demo_cb_merged$bin.income)
#demo_cb_merged$bin.income <- ifelse((demo_cb_merged$Income>30) , 'High_Income',demo_cb_merged$bin.income)
#table(demo_cb_merged$bin.income)
#
#ggplot(demo_cb_merged, aes(x=bin.income,fill=factor(Performance.Tag)))+ geom_bar()
#
## No of months in current company binning
#demo_cb_merged$bin.nomcc <- demo_cb_merged$No.of.months.in.current.company
#demo_cb_merged$bin.nomcc <- ifelse((demo_cb_merged$No.of.months.in.current.company>=0 & demo_cb_merged$No.of.months.in.current.company<=24) , 'Short_Tenure',demo_cb_merged$bin.nomcc)
#demo_cb_merged$bin.nomcc <- ifelse((demo_cb_merged$No.of.months.in.current.company>24 & demo_cb_merged$No.of.months.in.current.company<=72) , 'Medium_Tenure',demo_cb_merged$bin.nomcc)
#demo_cb_merged$bin.nomcc <- ifelse((demo_cb_merged$No.of.months.in.current.company>72) , 'High_Tenure',demo_cb_merged$bin.nomcc)
#table(demo_cb_merged$bin.nomcc)
#
#ggplot(demo_cb_merged, aes(x=bin.nomcc,fill=factor(Performance.Tag)))+ geom_bar()
#
##No of months in current residence
#demo_cb_merged$bin.nomcr <- demo_cb_merged$No.of.months.in.current.residence
#demo_cb_merged$bin.nomcr <- ifelse((demo_cb_merged$No.of.months.in.current.residence>=0 & demo_cb_merged$No.of.months.in.current.residence<=24) , 'Short_Tenure',demo_cb_merged$bin.nomcr)
#demo_cb_merged$bin.nomcr <- ifelse((demo_cb_merged$No.of.months.in.current.residence>24 & demo_cb_merged$No.of.months.in.current.residence<=72) , 'Medium_Tenure',demo_cb_merged$bin.nomcr)
#demo_cb_merged$bin.nomcr <- ifelse((demo_cb_merged$No.of.months.in.current.residence>72) , 'High_Tenure',demo_cb_merged$bin.nomcr)
#table(demo_cb_merged$bin.nomcr)
#
#ggplot(demo_cb_merged, aes(x=bin.nomcr,fill=factor(Performance.Tag)))+ geom_bar()
#
## Outstanding Balance binning
#demo_cb_merged$bin.outstanding <- demo_cb_merged$Outstanding.Balance
#demo_cb_merged$bin.outstanding <- ifelse((demo_cb_merged$Outstanding.Balance>=0 & demo_cb_merged$Outstanding.Balance<=500000) , 'Low',demo_cb_merged$bin.outstanding)
#demo_cb_merged$bin.outstanding <- ifelse((demo_cb_merged$Outstanding.Balance>500000 & demo_cb_merged$Outstanding.Balance<=1500000) , 'Medium',demo_cb_merged$bin.outstanding)
#demo_cb_merged$bin.outstanding <- ifelse((demo_cb_merged$Outstanding.Balance>1500000 & demo_cb_merged$Outstanding.Balance<=3500000) , 'High',demo_cb_merged$bin.outstanding)
#demo_cb_merged$bin.outstanding <- ifelse((demo_cb_merged$Outstanding.Balance>3500000 ) , 'Very_High',demo_cb_merged$bin.outstanding)
#table(demo_cb_merged$bin.outstanding)
#
#ggplot(demo_cb_merged, aes(x=bin.outstanding,fill=factor(Performance.Tag)))+ geom_bar()
#
## Total no. of trades
##demo_cb_merged$bin.total_trades <- demo_cb_merged$Total.No.of.Trades
##demo_cb_merged$bin.total_trades <- ifelse((demo_cb_merged$Total.No.of.Trades>=0 & demo_cb_merged$Total.No.of.Trades<=5) , 'Low',demo_cb_merged$bin.total_trades)
##demo_cb_merged$bin.total_trades <- ifelse((demo_cb_merged$Total.No.of.Trades>5 & demo_cb_merged$Total.No.of.Trades<=10) , 'Medium',demo_cb_merged$bin.total_trades)
##demo_cb_merged$bin.total_trades <- ifelse((demo_cb_merged$Total.No.of.Trades>10) , 'High',demo_cb_merged$bin.total_trades)
#
## Average CC Utilization
#demo_cb_merged$bin.avg_cc <- demo_cb_merged$Avgas.CC.Utilization.in.last.12.months
#demo_cb_merged$bin.avg_cc <- ifelse((demo_cb_merged$Avgas.CC.Utilization.in.last.12.months>=0 & demo_cb_merged$Avgas.CC.Utilization.in.last.12.months<=20) , 'Low',demo_cb_merged$bin.avg_cc)
#demo_cb_merged$bin.avg_cc <- ifelse((demo_cb_merged$Avgas.CC.Utilization.in.last.12.months>20 & demo_cb_merged$Avgas.CC.Utilization.in.last.12.months<=50) , 'Medium',demo_cb_merged$bin.avg_cc)
#demo_cb_merged$bin.avg_cc <- ifelse((demo_cb_merged$Avgas.CC.Utilization.in.last.12.months>50 & demo_cb_merged$Avgas.CC.Utilization.in.last.12.months<=80) , 'High',demo_cb_merged$bin.avg_cc)
#demo_cb_merged$bin.avg_cc <- ifelse((demo_cb_merged$Avgas.CC.Utilization.in.last.12.months>80 ) , 'Very_High',demo_cb_merged$bin.avg_cc)
#table(demo_cb_merged$bin.avg_cc)
#
#ggplot(demo_cb_merged, aes(x=bin.avg_cc,fill=factor(Performance.Tag)))+ geom_bar()

#to_remove <- c("Age","Income","No.of.months.in.current.company","No.of.months.in.current.residence",
#               "Outstanding.Balance","Avgas.CC.Utilization.in.last.12.months")

#demo_cb_merged <- demo_cb_merged[,!(names(demo_cb_merged) %in% to_remove)]

# creating final set of categorical and continuous variables
#cat <- c("Performance.Tag","Gender","Marital.Status..at.the.time.of.application.","Presence.of.open.home.loan","Presence.of.open.auto.loan","bin.age","bin.income","bin.nomcc","bin.nomcr","bin.outstanding","bin.avg_cc","Education","Profession","Type.of.residence","No.of.dependents")
#cont <- c("No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months","No.of.times.60.DPD.or.worse.in.last.12.months","No.of.times.30.DPD.or.worse.in.last.12.months","No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months","No.of.PL.trades.opened.in.last.6.months","No.of.PL.trades.opened.in.last.12.months","No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.","No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.","Total.No.of.Trades")

cat <- c("Performance.Tag","Gender","Marital.Status..at.the.time.of.application.","No.of.dependents","Education","Profession","Type.of.residence","Presence.of.open.home.loan","Presence.of.open.auto.loan")
cont <- c("Age","Income","No.of.months.in.current.residence","No.of.months.in.current.company","No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months","No.of.times.60.DPD.or.worse.in.last.12.months","No.of.times.30.DPD.or.worse.in.last.12.months","Avgas.CC.Utilization.in.last.12.months","No.of.trades.opened.in.last.6.months","No.of.trades.opened.in.last.12.months","No.of.PL.trades.opened.in.last.6.months","No.of.PL.trades.opened.in.last.12.months","No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.","No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.","Outstanding.Balance","Total.No.of.Trades")
  
final_cat <- demo_cb_merged[,cat]
final_cont <- demo_cb_merged[,cont]

# Normalising continuous features 
#final_cont <- data.frame(sapply(final_cont, function(x) scale(x)))
#str(final_cont)

# converting categorical attributes to factor
final_cat<- data.frame(sapply(final_cat, function(x) factor(x)))
str(final_cat)

summary(final_cat$Gender)
#    F     M 
# 16234 52483 

summary(final_cat$Marital.Status..at.the.time.of.application.)
#Married  Single 
# 58548   10169 

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(final_cat, 
                            function(x) data.frame(model.matrix(~x-1,data =final_cat))[,-1]))
sum(dummies$Gender)
# suggest Male == 1 and Female == 0
sum(dummies$Marital.Status..at.the.time.of.application.)
# suggest Single == 1 and Married == 0

final_data <- cbind(dummies,final_cont)

########################################################################
######################### splitting the data between train and test #############################
set.seed(100)

indices = sample.split(final_data$Performance.Tag, SplitRatio = 0.7)

train = final_data[indices,]

test = final_data[!(indices),]

#####################################################################################
#Model Building
#####################################################################################
############################# Logistic Regression####################################

#model containing all the variables
model_1 <- glm(Performance.Tag~.,data=train,family = "binomial")
summary(model_1)

# used STEPAIC to find the best model
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
sort(vif(model_2))

# removing 60 dpd in last 6 months
model_3 <- glm(formula = Performance.Tag ~ No.of.dependents.x2 + Profession.xSE + 
                 Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.times.90.DPD.or.worse.in.last.12.months + Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Total.No.of.Trades, family = "binomial", data = train)

summary(model_3)
sort(vif(model_3))

# removing no of times 90 dpd based on the p-value
model_4 <- glm(formula = Performance.Tag ~ No.of.dependents.x2 + Profession.xSE + 
                 Income + No.of.months.in.current.residence + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Total.No.of.Trades, family = "binomial", data = train)

summary(model_4)
sort(vif(model_4))

# removing income based on the p-value
model_5 <- glm(formula = Performance.Tag ~ No.of.dependents.x2 + Profession.xSE + 
                 No.of.months.in.current.residence + No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Total.No.of.Trades, family = "binomial", data = train)

summary(model_5)
sort(vif(model_5))


# removing no of months in current residence based on the p-value
model_6 <- glm(formula = Performance.Tag ~ No.of.dependents.x2 + Profession.xSE + 
                 No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Total.No.of.Trades, family = "binomial", data = train)

summary(model_6)
sort(vif(model_6))


# removing profession based on the p-value
model_7 <- glm(formula = Performance.Tag ~ No.of.dependents.x2 + 
                 No.of.months.in.current.company + 
                 No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Avgas.CC.Utilization.in.last.12.months + 
                 No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Total.No.of.Trades, family = "binomial", data = train)

summary(model_7)
sort(vif(model_7))

#predicted probabilities of default for test data
test_pred = predict(model_7, type = "response", 
                    newdata = test[,-1])


# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_def <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_def <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))


table(test_actual_def,test_pred_def)

test_conf <- confusionMatrix(test_pred_def, test_actual_def, positive = "Yes")
test_conf
#######################################################################

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

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

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


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.05 for final model

test_cutoff_def <- factor(ifelse(test_pred >=0.05, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_def, test_actual_def, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]
conf_final

# Accuracy = 68%
# Sensitivity = 53.2%
# Specificity = 68.9%

############################################################################################
############# Model Evaluation##################
### KS -statistic - Test Data ######

test_cutoff_def <- ifelse(test_cutoff_def=="Yes",1,0)
test_actual_def <- ifelse(test_actual_def=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_def, test_actual_def)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

# 0.22

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

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

Churn_decile = lift(test_actual_def, test_pred, groups = 10)

#####################################################################################################################
# Since there are just 2897/68717 records having performance tag value as 1, this will create an imbalance
# in the model, so we will use Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification


#####################################################################################
############################### Random Forrest ###################################### 
#####################################################################################
set.seed(100)
trainindices= sample(1:nrow(final_data), 0.7*nrow(final_data))
train = final_data[trainindices,]
test = final_data[-trainindices,]

rf_fit <- randomForest(Performance.Tag~.,train,ntree=500,importance=T)

plot(rf_fit)

varImpPlot(rf_fit,sort = T,main="Variable Importance",n.var=5)

var.imp <- data.frame(importance(rf_fit,type=2))

var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]

test$predicted.prob <- predict(rf_fit ,test[,-1], type = "prob")

s = seq(.5,.99,length=100)

OUT = matrix(0,100,3)

for(i in 1:100){  
  OUT[i,] = perform_fn1(s[i])
} 

#Plot to choose best cutoff
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.6,.80,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

best_cutoff <- s[which(abs(OUT[,1]-OUT[,2])==min(abs(OUT[,1]-OUT[,2])))]
best_cutoff

test$Performance.Tag<- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
test$predicted.response_1<- factor(ifelse(test$predicted.prob[,1] >= best_cutoff, "Yes", "No"))
confusionmartix_final_rf<-confusionMatrix(test$predicted.response_1, test$Performance.Tag, positive = "Yes")
