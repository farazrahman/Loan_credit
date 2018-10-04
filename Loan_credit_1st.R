#Set up the working directory

#1. Introduction- Determining the driving factors or driver variables behind loan default through EDA
# 1.1- Load libraries and data files

library('readr') # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('stringr') # string manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('corrplot') # visualisation
library('lubridate')
library("VIM")

#1.2- Reading in the data files

loan <- read_csv("loan.csv")
glimpse(loan) #There are 39,717 observations and 111 variables
summary(loan)

#1.3- Target variable will be Loan status because we want to find out the driving factors for loan default. 

#2. DATA CLEANING
#Since there are a lot of variables, we need to get rid of unwanted columns.
#checking % of missing or NA values in the data set
#Here the dataset is divided into two sets in order to visualise the missing data
#missing_values1st- for columns 1 to 55 
missing_values1st <- loan[,1:55] %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values1st <- gather(missing_values1st, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

#missing_values2nd for rest of 56 to 111 columns

missing_values2nd <- loan[,56:111] %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values2nd <- gather(missing_values2nd, key="feature", value="missing_pct")
missing_values2nd %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw()

Column_NAs <- data.frame(colSums(is.na(loan)))
View(Column_NAs)

aggr(loan, prop = T)



#2.1- Removing columns having only NA values 
loan_cleaned <- loan[, colSums(is.na(loan)) != nrow(loan)]

#2.2- We can also remove columns having significant number of NAs such as columns having more than 20000 of missing data
loan_cleaned <- loan[, colSums(!is.na(loan)) > 20000]

#2.3- The url, desc, title column can be removed as it is redundant with the purpose column
#Also the zipcode column can be removed as it has only first three digits and that information can be obtained by the state column
#Sub-grade can be removed too, because that information can be obtained from grade column
#Similarly, id column can be removed as it has unique values for loan identification only
#The columns "collections_12_mths_ex_med", "acc_now_delinq", "chargeoff_within_12_mths", "delinq_amnt", "tax_liens" have mostly 0's in them hence can be removed

loan_cleaned <- within(loan_cleaned, rm('id', 'url','desc', 'title', 'zip_code', 'sub_grade','collections_12_mths_ex_med', 'acc_now_delinq', 'chargeoff_within_12_mths', 'delinq_amnt', 'tax_liens' ))

View(loan_cleaned)
str(loan_cleaned)
summary(loan_cleaned)

#2.4- Converting incorrect data types- Date columns are in Chr format, so we will convert them into Date format
typeof(loan_cleaned$issue_d)
loan_cleaned$issue_d <- format(parse_date(loan_cleaned$issue_d,format =  "%b-%y"), "%Y-%m")
loan_cleaned$earliest_cr_line <- format(parse_date(loan_cleaned$earliest_cr_line,format =  "%b-%y"), "%Y-%m")
loan_cleaned$last_pymnt_d <- format(parse_date(loan_cleaned$last_pymnt_d,format =  "%b-%y"), "%Y-%m")
loan_cleaned$last_credit_pull_d <- format(parse_date(loan_cleaned$last_credit_pull_d,format =  "%b-%y"), "%Y-%m")


ggplot(loan_cleaned, aes(x= loan_cleaned$funded_amnt, y = loan_cleaned$funded_amnt_inv)) + geom_point()
all(loan_cleaned$funded_amnt == loan_cleaned$funded_amnt_inv)

#2.5- Handling Categirical variables-There are some Columns that are categorical variables but are represented as characters. We need to convert them into factors

loan_cleaned$term <- as.factor(loan_cleaned$term)
loan_cleaned$grade <- as.factor(loan_cleaned$grade)
loan_cleaned$emp_length <- as.factor(loan_cleaned$emp_length)
loan_cleaned$home_ownership <- as.factor(loan_cleaned$home_ownership)
loan_cleaned$verification_status <- as.factor(loan_cleaned$verification_status)
loan_cleaned$loan_status <- as.factor(loan_cleaned$loan_status)
loan_cleaned$application_type <- as.factor(loan_cleaned$application_type)
loan_cleaned$pymnt_plan <- as.factor(loan_cleaned$pymnt_plan)
loan_cleaned$initial_list_status <- as.factor(loan_cleaned$initial_list_status)
loan_cleaned$policy_code <- as.factor(loan_cleaned$policy_code)

#As we can see the variables "policy_code", "initial_list_status" and "pymnt_plan", "application_type" have only one level of data in all rows, we can get rid of them as well

loan_cleaned <- within(loan_cleaned, rm("policy_code", "initial_list_status", "pymnt_plan", "application_type"))
summary(loan_cleaned)



#2.6- Handling Quantitative variables
#The variables int_rate(interest rate on loan) and revol_util(revolving utilization) are in character format so they need to get converted to integer format

loan_cleaned$int_rate <- as.integer(gsub("%", "", loan_cleaned$int_rate))
loan_cleaned$revol_util <- as.integer(gsub("%", "", loan_cleaned$revol_util))


#3- Correlation between quantitative variables

png(height=1200, width=1200, pointsize=25)

corrplot(cor(loan_cleaned[,unlist(lapply(loan_cleaned, is.numeric))], use = "complete.obs"), method = "number")


corrplot(cor(loan_cleaned[,unlist(lapply(loan_cleaned, is.numeric))], use = "complete.obs"), type = "upper", method = "number")

corrplot(cor(loan_cleaned[,unlist(lapply(loan_cleaned, is.numeric))], use = "complete.obs"), type = "lower")

#4- Univariate analysis of target variable, "Loan Status"

ggplot(loan_cleaned, aes(x= loan_status, fill = loan_status)) + geom_bar()+theme_bw()

loan_status.pct = loan_cleaned %>% group_by(loan_status) %>%
  summarise(count=n()) %>% mutate(pct=count/sum(count))


#The 82.9% of loan were fully paid, 2.8% loan are current loan, 14.167% loan are charged off(defaulted)


#4.2- distribution of loan amount

ggplot(loan_cleaned, aes(x= loan_amnt)) + geom_histogram(bins = 40, color= "blue", fill= "#CCCCFF")

#4.3- distribution of term length

ggplot(loan_cleaned, aes(x= term)) + geom_bar(color= "#336600", fill= "#66FF33")

#Loans with 36 months tenure is more than 60 months

#Bivariate analysis

#since we have to find the driver variables for loan default, we must inspect all the variables w.r.t loan status in the bivariate analysis

ggplot(loan_cleaned, aes(x= loan_status, y = loan_amnt)) + geom_boxplot(aes(fill= loan_status))
ggplot(loan_cleaned, aes(x= term, y = loan_status)) + geom_jitter(aes(color = loan_status))
ggplot(loan_cleaned, aes(x= installment, fill = loan_status)) + geom_histogram(bins = 60, position = "fill")
ggplot(loan_cleaned, aes(x = int_rate, fill = loan_status)) + geom_bar(position = "fill")
#default increase when rate of interest increases.

ggplot(loan_cleaned, aes(x= grade, fill = loan_status )) + geom_bar(position = "fill")
#lower the grade higher the interest rate, higher the probability of default

ggplot(loan_cleaned, aes(x = grade, y = int_rate)) + geom_jitter(aes(color = loan_status))
#A higher grade increases the probability of loan repayment, lower grade increases the interest rate so the number of repayments are less
#It gradually decreases from higher to lower grade

ggplot(loan_cleaned, aes(x= emp_length, y = funded_amnt, fill= loan_status)) + geom_bar(stat = "identity", position = "fill")

ggplot(loan_cleaned, aes(x= emp_length, y = funded_amnt)) + geom_bar(stat = "identity", fill= "#993366")
#emp_length doesn't have any impact on loan funded amount and default

ggplot(loan_cleaned, aes(x = as.factor(emp_title), fill = funded_amnt)) + geom_bar()


ggplot(loan_cleaned, aes(home_ownership, fill = loan_status)) + geom_bar(position = "fill")
#No results

options(scipen = "999")
ggplot(loan_cleaned, aes(x = annual_inc )) + geom_histogram(bins = 10)


ggplot(loan_cleaned, aes(x = verification_status, y = loan_status)) + geom_jitter()


ggplot(loan_cleaned, aes(x = verification_status, fill= loan_status)) + geom_bar(position = "fill")
#No effect

ggplot(loan_cleaned, aes(x = purpose, fill = loan_status)) + geom_bar(position = "fill")
#small business loans get defaulted more as compared to other loans

ggplot(loan_cleaned, aes(x = addr_state, fill = loan_status)) + geom_bar(position = "fill")
#NE state had many defaulters

ggplot(loan_cleaned, aes(x = dti, fill = loan_status)) + geom_bar(position = "fill")

ggplot(loan_cleaned, aes(x = revol_util, fill = loan_status)) + geom_bar(position = "fill")
#higher % of revol_util leads to more default

ggplot(loan_cleaned, aes(x = out_prncp)) + geom_histogram()

write.csv()
write.csv(loan_cleaned, "loan_cleaned.csv", row.names = F)

str(loan_cleaned)

