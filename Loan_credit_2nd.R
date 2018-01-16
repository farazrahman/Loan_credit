#Set up the working directory

setwd("C:/PGDDS/Course 2/Gramener Case study- group project")

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
#checking the number of missing or NA values in the data set

Column_NAs <- data.frame(colSums(is.na(loan)))
View(Column_NAs)

#2.1- Removing columns having only NA values 
loan_cleaned <- loan[, colSums(is.na(loan)) != nrow(loan)]

#2.2- We can also remove columns having significant number of NAs such as columns having more than 20000 of missing data
loan_cleaned <- loan[, colSums(!is.na(loan)) > 20000]

#2.3- The url, desc, title column can be removed as it is redundant with the purpose column
#Also the zipcode column can be removed as it has only first three digits and that information can be obtained by the state column
#Sub-grade can be removed too, because that information can be obtained from grade column
#Similarly, id  & member_id column can be removed as it has unique values for loan identification only
#The columns "collections_12_mths_ex_med", "acc_now_delinq", "chargeoff_within_12_mths", "delinq_amnt", "tax_liens" have mostly 0's in them hence can be removed

loan_cleaned <- within(loan_cleaned, rm('member_id', 'id', 'url','desc', 'title', 'zip_code', 'sub_grade','collections_12_mths_ex_med', 'acc_now_delinq', 'chargeoff_within_12_mths', 'delinq_amnt', 'tax_liens' ))

#2.4- Converting incorrect data types- Date columns are in Chr format, so we will convert them into Date format
typeof(loan_cleaned$issue_d)
loan_cleaned$issue_d <- format(parse_date(loan_cleaned$issue_d,format =  "%b-%y"), "%Y-%m")
loan_cleaned$earliest_cr_line <- format(parse_date(loan_cleaned$earliest_cr_line,format =  "%b-%y"), "%Y-%m")
loan_cleaned$last_pymnt_d <- format(parse_date(loan_cleaned$last_pymnt_d,format =  "%b-%y"), "%Y-%m")
loan_cleaned$last_credit_pull_d <- format(parse_date(loan_cleaned$last_credit_pull_d,format =  "%b-%y"), "%Y-%m")

#2.5- Handling Categorical variables-There are some Columns that are categorical variables but are represented as characters. We need to convert them into factors

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



#3- Handling Quantitative variables
#The variables int_rate(interest rate on loan) and revol_util(revolving utilization) are in character format so they need to get converted to integer format

loan_cleaned$int_rate <- as.integer(gsub("%", "", loan_cleaned$int_rate))
loan_cleaned$revol_util <- as.integer(gsub("%", "", loan_cleaned$revol_util))


#3.1- Correlation between quantitative variables

corrplot(cor(loan_cleaned[,unlist(lapply(loan_cleaned, is.numeric))], use = "complete.obs"), type = "lower", method = "number", number.cex = 0.8)

#As we can see that high correlation exists between the numeric variables such as loan amount, funded amount, funded_amount_inv & installment means these variables are redundant and express the same information
#Using them together will lead to duplication of variables, hence we will get rid of funded amount, funded_amount_inv
#Similarly the variables  #out_prncp, out_prncp_inv, 
                          #total_pymnt, total_pymnt_inv, total_rec_prncp, total_rec_int
                          #total_rec_late_fee, recoveries, collection_recovery_fee 
#represent the data which will happen in future if a loan gets repaid or defaulted, hence, doesn't necessarily represent the cause of default or repayment
#It is obivious that if there is a default, the above variables will be less as compared to a loan repayment. hence, we can get rid of them as well

loan_cleaned <- within(loan_cleaned, rm("funded_amnt", "funded_amnt_inv", "out_prncp", "out_prncp_inv", "total_pymnt", 
                                        "total_pymnt_inv" , "total_rec_prncp", "total_rec_int", "total_rec_late_fee", 
                                        "recoveries", "collection_recovery_fee"))


View(loan_cleaned)
str(loan_cleaned)
summary(loan_cleaned)


#4- Univariate analysis of Categorical variables
#Term(unordered), Grade(ordered), emp_length(ordered), home_ownership(unordered), verification_status(unordered),Loan_status(ordered)

#4.1-Univariate analysis of target variable, "Loan Status"

loan_status.pct = loan_cleaned %>% group_by(loan_status) %>%
  summarise(count=n()) %>% mutate(pct=count/sum(count))

ggplot(loan_status.pct, aes(x=loan_status, y=pct, colour=loan_status, fill=loan_status)) +
  geom_bar(stat="identity") +
  geom_text(data=loan_status.pct, aes(label=paste0(round(pct*100,2),"%"),
                               y=pct+0.05), size=4) + theme_bw()

#The 82.9% of loan were fully paid, 2.8% loan are current loan, 14.17% loan are charged off(defaulted)

#4.2- distribution of term length

ggplot(loan_cleaned, aes(x= term)) + geom_bar(color= "#336600", fill= "#CC00CC")

#number of loans with term 36 months is higher

#4.3- distribution of Grade

ggplot(loan_cleaned, aes(x = grade, fill = grade)) + geom_bar()
#Loan with Grade B are more

#4.4- distribution of emp_length
ggplot(loan_cleaned, aes(x = emp_length, fill = emp_length)) + geom_bar()

#4.5- distribution of home-ownership

ggplot(loan_cleaned, aes(x = home_ownership, fill = home_ownership)) + geom_bar(position = "dodge")

#Number of rent and mortgage is more

#4.6- distribution of verification status

ggplot(loan_cleaned, aes(x = verification_status, fill = verification_status)) + geom_bar()

#5- Univariate analysis on continous variable

#5.1- distribution of loan amount

ggplot(loan_cleaned, aes(x= loan_amnt)) + geom_histogram(bins = 20, color= "blue", fill= "#CCCCFF")

#5.2- distribution of interest rate

ggplot(loan_cleaned, aes(x= int_rate)) + geom_histogram(bins =20 , color= "green", fill= "#CCCCFF")

#5.3- distribution of revol_util

ggplot(loan_cleaned, aes(x= revol_util)) + geom_histogram(bins = 10, color= "blue", fill= "#CCCCFF")


#6- Bivariate analysis

#6.1- does high loan amount has an impact on loan status- kind of 


ggplot(loan_cleaned, aes(x= loan_amnt, color = loan_status)) + geom_histogram( bins = 10, position = "fill")
#Yes default increases with increase in loan amount


ggplot(loan_cleaned, aes(x= home_ownership, y = loan_amnt, fill = loan_status)) + geom_boxplot()
#The median loan amount is higher in case of all homeownership categories

#6.2- does interst rate has an impact on loan status- Yes

ggplot(loan_cleaned, aes(x= int_rate, fill = loan_status)) + geom_bar(position = "fill")
ggplot(loan_cleaned, aes(x=loan_status , y = int_rate)) + geom_boxplot(aes(fill= loan_status))
#yes high interest rate affects loan default. The 1st quartile median and 3rd quartile is high in case of charged off as compared to fully paid category

#6.2.1- If high interest rate leads to loan default, does it mean it is the same case with all verification status?
ggplot(loan_cleaned, aes(x= verification_status, y = int_rate)) + geom_boxplot(aes(fill= loan_status))
ggplot(loan_cleaned, aes(x= int_rate, fill = verification_status)) + geom_bar(position = "fill") #strange-unverified loan get less interest

#6.2.2- Employment length has any impact on interest rate- No
ggplot(loan_cleaned, aes(x= emp_length, y = int_rate, fill = loan_status)) + geom_bar(stat = "identity", position = "fill")


#6.2.3-Does high loan amount leads to high interest rate?No

ggplot(loan_cleaned, aes(x= loan_amnt, y =int_rate)) + geom_jitter(aes(color = loan_status))


#6.2.4-how does grade vary with int_rate

ggplot(loan_cleaned, aes(x = grade, y = int_rate, fill = loan_status)) + geom_boxplot()
ggplot(loan_cleaned, aes(x = grade, fill = loan_status)) + geom_bar(position = "fill")
#The interest increases from Grade A to G. and default also increases with low grade. They are kind of same parameter

#6.2.5- does annual income has an impact on interest rate? No
options(scipen = "999")
ggplot(loan_cleaned, aes(x = int_rate, y = annual_inc )) + geom_jitter()



#6.3- Does purpose of the loan has an impact on loan default- yes-small business

ggplot(loan_cleaned, aes(x = purpose, fill = loan_status)) + geom_bar(position = "fill")

#6.4- does home ownership has an impact of laon default- No
ggplot(loan_cleaned, aes(home_ownership, fill = loan_status)) + geom_bar(position = "fill")
#No results

#6.5- does verification status has an impact on loan default? No

ggplot(loan_cleaned, aes(x = verification_status, fill= loan_status)) + geom_bar(position = "fill")

#6.6- Which state has more loan defaults?
ggplot(loan_cleaned, aes(x = addr_state, fill = loan_status)) + geom_bar(position = "fill")
#NE state has many defaulters

#6.6.1- Why high defaults in NE state? data driven metrics
ggplot(loan_cleaned, aes(x = addr_state)) + geom_bar() #negligible loan applied in NE state
loan_amnt_state <- loan_cleaned %>% group_by(addr_state) %>% summarise(total_loan_amnt = sum(loan_amnt))
int_rate_state <- loan_cleaned %>% group_by(addr_state) %>% summarise(total_int_rate = round(mean(int_rate)))


ggplot(loan_amnt_state, aes(x = addr_state, y = total_loan_amnt)) + geom_point()
ggplot(int_rate_state, aes(x = addr_state, y = total_int_rate)) + geom_point()



#7-impact of dti on loan status-Not much
ggplot(loan_cleaned, aes(x = loan_status, y = dti)) + geom_boxplot()


#8- Impact of revol_util on loan status-yes
ggplot(loan_cleaned, aes(x =loan_status , y = revol_util, fill = loan_status)) + geom_boxplot()
#higher % of revol_util leads to more default

#9- impact of total_acc- No

ggplot(loan_cleaned, aes(x = loan_status, y = total_acc)) + geom_boxplot()

#10- impact of revol_bal- NO

ggplot(loan_cleaned, aes(x = loan_status, y = revol_bal)) + geom_boxplot()

#11- impact of public_rec_bankruptcies- to be reworked
ggplot(loan_cleaned, aes(x = loan_status, y = pub_rec_bankruptcies)) + geom_boxplot()

#12- impact of inq_last 6months- yes

ggplot(loan_cleaned, aes(x = loan_status, y = inq_last_6mths)) + geom_boxplot()

#13- impact delinq_2yrs

ggplot(loan_cleaned, aes(x = loan_status, y = delinq_2yrs)) + geom_boxplot()


write.csv()
write.csv(loan_cleaned, "loan_cleaned.csv", row.names = F)



