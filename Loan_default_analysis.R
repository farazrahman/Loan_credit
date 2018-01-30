# 1.1- Load libraries and data files

library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('stringr') # string manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('lubridate') # date and time
library('purrr') # data manipulation

# Setting options for Time Zone
Sys.setenv(TZ="GMT")

#1.2- Reading in the data files

loan <- read.csv("loan.csv")
glimpse(loan) 
summary(loan)

#Observation:
#i. There are 39,717 observations
#ii. 111 variables with many columns having lots of NA values
#iii- Target variable will be Loan status as it has information about loan paid, current and charged off(default).

#2. DATA CLEANING
#2.1- Since there are a lot of variables, we need to get rid of unwanted columns.
#First we will check the number of missing or NA values in the data set

colSums(is.na(loan))

#Visualizing the missing data from column 1 to 55
missing_data1 <- loan[,1:55] %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data1 <- gather(missing_data1, key = "variables1", value = "percent_missing1") 
ggplot(missing_data1, aes(x = reorder(variables1, percent_missing1), y = percent_missing1)) +
  geom_bar(stat = "identity", fill = "red")+coord_flip()+ theme_few()

#Visualizing the missing data from column 56 to 111
missing_data2 <- loan[,56:111] %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data2 <- gather(missing_data2, key = "variables2", value = "percent_missing2") 
ggplot(missing_data2, aes(x = reorder(variables2, percent_missing2), y = percent_missing2)) +geom_bar(stat = "identity", fill = "red")+coord_flip()+ theme_few()

#Observation: 
#i. total 54 columns have complete NAs
#ii. 4 columns have more than 25% missing data
#iii. 7 columns have less than 10% missing data

#2.2- Removing columns having all NA values 
loan_cleaned <- loan[, colSums(is.na(loan)) != nrow(loan)]

#2.2- We can also remove columns having significant number of NAs such as columns having more than 25% of missing data
#loan_test <- loan_cleaned[, -which((colSums(is.na(loan_cleaned))/nrow(loan_cleaned)) > 0.25)]
loan_cleaned <- loan[, -which(colMeans(is.na(loan)) > 0.25)]

#2.3- Removing the redundant, duplicate and non relevant variables 

#i. id, member_id, url(X), desc columns can be removed as they have unique values for the purpose of loan identification only.
#ii. The title column can be removed as it is redundant with the purpose column.
#iii. zipcode column can be removed as it has only first three digits and that information can be obtained by the state column
#iv. We can remove emp_title as well because it has more than 28000 unique emp_title and around 6% NA values and 
#imputing them or deleting 2453 rows from data will lead to loss of data, hence, we will take out that column itself.
#vi. The columns "collections_12_mths_ex_med", "acc_now_delinq", "chargeoff_within_12_mths", "delinq_amnt", "tax_liens" have mostly 0's in them hence can be removed
# The columns sub_grade can be removed as it has redundant information as grade

loan_cleaned <- within(loan_cleaned, rm('member_id', 'id', 'X', 'title', 'emp_title', "zip_code" ,'collections_12_mths_ex_med', 'acc_now_delinq', 'chargeoff_within_12_mths', 'delinq_amnt', 'tax_liens', 'sub_grade', 'desc'))

#2.4- Converting incorrect data types

#i. Handling Categorical variables-There are some Columns that are factor variables but are represented as characters. We need to convert them into factors

nms <- c("term", "grade", "emp_length", "home_ownership", "verification_status", "loan_status", "application_type", "pymnt_plan", "initial_list_status", "policy_code", "addr_state", "purpose") 
loan_cleaned[nms] <- lapply(loan_cleaned[nms], as.factor) 

#As we can see the variables "policy_code", "initial_list_status" and "pymnt_plan", "application_type" have only one level of data in all rows, we can get rid of them as well

loan_cleaned <- within(loan_cleaned, rm("policy_code", "initial_list_status", "pymnt_plan", "application_type"))

#ii. Handling Quantitative variables
#The variables int_rate(interest rate on loan) and revol_util(revolving utilization) are in character format so they need to get converted to integer format

convert_to_int <- function(varname) {
  int_var <- as.integer(gsub("%", "", varname))
}

int_vars <- c("int_rate","revol_util")

loan_cleaned[int_vars] <- sapply(loan_cleaned[int_vars], convert_to_int)

#iii. Converting incorrect data types- Date columns are not standardized, so we will convert them into standard format

convert_to_date <- function(datestr) {
  datestr_std <- ifelse(str_detect(str_sub(datestr, 1, 1), "[0-9]"), format(as.Date(paste(datestr, "-01", sep=""), "%y-%b-%d"), "%b-%Y"), format(as.Date(paste("01-", datestr, sep=""),"%d-%b-%y"), "%b-%Y"))
  }

date_vars <- c("issue_d", "earliest_cr_line", "last_pymnt_d", "last_credit_pull_d")
loan_cleaned[date_vars] <- sapply(loan_cleaned[date_vars], convert_to_date)

# Since the year provided in the data was two digit and the R beginning date is 1970, the years before 1970 are interpreted incorrectly.

# First remove the observations that have incorrect years for earlies_cr_line

loan_cleaned <- subset(loan_cleaned, as.integer(str_split_fixed(earliest_cr_line, "-", 2)[,2]) < as.integer(str_split_fixed(issue_d, "-", 2)[,2]))

# Correct the years for the earliest_cr_line and add them back to the data
incorrect_dates <- subset(loan_cleaned, as.integer(str_split_fixed(earliest_cr_line, "-", 2)[,2]) > as.integer(str_split_fixed(issue_d, "-", 2)[,2]))
correct_dates <- mutate(incorrect_dates, earliest_cr_line=str_replace(incorrect_dates$earliest_cr_line, "20", "19"))

loan_cleaned <- rbind(loan_cleaned, correct_dates)

#2.5- Dealing with redundant or duplicate variables

#i.  Correlation between quantitative variables

corrplot(cor(loan_cleaned[,unlist(lapply(loan_cleaned, is.numeric))], use = "complete.obs"), type = "lower")

#Observation:
#i. High correlation exists between loan amount, funded amount, funded_amount_inv.
#ii.total_pymnt, total_pymnt_inv, total_rec_prncp, total_rec_int have high correlation with loan_amnt & with each other as well.
#iii. Also, these variables represent the data for the activities which will happen in the future once the loan request is approved for the customer
#iv. Meaning these data will not be available while deciding whether to loan or not.
#v. Similarly the variables out_prncp, out_prncp_inv, total_rec_late_fee, recoveries, collection_recovery_fee also represent the data for the activities that will happen in the future
#vi. public_rec & pub_rec_bankruptcies are also correlated, however, we will get rid of pub_rec_bankruptcies as it has 697 missing values

loan_cleaned <- within(loan_cleaned, rm("funded_amnt", "funded_amnt_inv", "out_prncp", "out_prncp_inv", "total_pymnt", 
                                        "total_pymnt_inv" , "total_rec_prncp", "total_rec_int", "total_rec_late_fee", 
                                        "recoveries", "collection_recovery_fee", "pub_rec_bankruptcies"))

colSums(is.na(loan_cleaned))

#2.6-Handling NA values
#There are 50 NAs in variable revol_util, 71 NAs in last_pymnt_d and 2 NAs in last_credit_pull_d, which represent only 0.3% of total data
#Hence, we can get rid of the rows corresponding to the NA values

loan_cleaned <- na.omit(loan_cleaned)

#3- DATA PREPARATION

loan_status.pct <- loan_cleaned %>% group_by(loan_status) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))

ggplot(loan_status.pct, aes(x=loan_status, y=pct, colour=loan_status, fill=loan_status)) +
  geom_bar(stat="identity") +
  geom_text(data=loan_status.pct, aes(label=paste0(round(pct*100,2),"%"),
                                      y=pct+0.05), size=4) + theme_bw()

#3.1- The target variable column Loan_status has three categories Fully Paid, Current & Charged-off(default)
#3.2- We are also told that "Current" category cannot be considered as default.
#3.3- Hence, we don't know whether the current status will become "Fully Paid" or "Charged off".
#3.4- Our goal is to find out the driving factors for default, so we will only consider the data from Fully paid and charged off category.
#3.5- We will remove the rows having Current status which is about 2.88% of the total data
#3.6- And insert a new column with binary values for the fully paid column as 1 and charged off as 0.

loan_cleaned <- filter(loan_cleaned, loan_status != "Current")
loan_cleaned <- mutate(loan_cleaned, binary_status=as.numeric(ifelse(loan_cleaned$loan_status == "Fully Paid", 0, 1))) 
str(loan_cleaned)
summary(loan_cleaned)
summary(loan_cleaned$annual_inc)

#Observations:
# The annual income has some outliers as there are extreme values for some observations that are more than three standard
# deviations apart from mean so we will remove them from the analysis

annual_inc_upper_limit <- mean(loan_cleaned$annual_inc) + 3 * sd(loan_cleaned$annual_inc)
loan_cleaned <- subset(loan_cleaned, annual_inc <= annual_inc_upper_limit)

#4- UNIVARIATE ANALYSIS

#4.1- Univariate analysis on Categorical variables

P_term <- ggplot(loan_cleaned, aes(x = term, fill = term ))+ geom_bar() + theme_few()
#Number of loans issued for 36 months are more

P_grade <- ggplot(loan_cleaned, aes(x = grade, fill = grade ))+ geom_bar() + theme_few()
#number of loans issued is highest for Grade B and has a decreasing trend from loan A_G

P_elength <- ggplot(loan_cleaned, aes(x = emp_length, fill = emp_length ))+ geom_bar() + theme_few()
#total number of loans shows a decreasing trend from employee_length 2-9 years. More people with less experience are applying for loan.

P_home <- ggplot(loan_cleaned, aes(x = home_ownership, fill = home_ownership ))+ geom_bar() + theme_few()
#Rent and mortgage home owners account for maximum number of loans

P_verificatn <- ggplot(loan_cleaned, aes(x = verification_status, fill = verification_status ))+ geom_bar() + theme_few()
#Non-verified loans are more

P_purpose <- ggplot(loan_cleaned, aes(x = purpose, fill = purpose ))+ geom_bar() + theme_few()
#debt consolidation seems to be the main purpose for which people are applying for loan as compared to other loans

P_state <- ggplot(loan_cleaned, aes(x = addr_state, fill = addr_state ))+ geom_bar() + theme_few() + coord_flip()
#loans applied in CA are more

#4.2- Univarate analysis on Continous variables

P_distribution <- loan_cleaned %>% keep(is.numeric) %>% gather() %>%  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=20, color= "black", fill= "#3399FF")

#5- Segmented Univariate analysis
#since our target is to know about loan status, we will analyse the impact of other variables on loan_status.

#5.1- Impact of Categorical variables on loan status

termVsStatus <- ggplot(loan_cleaned, aes(x =term, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'Term') +
  scale_fill_discrete(name="Loan_Status") +theme_few()
#i- Loans with 60 months term get defaulted more as compared to 36 months term

gradeVsStatus <- ggplot(loan_cleaned, aes(x = grade, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'Grade') +
  scale_fill_discrete(name="Loan_Status") +theme_few()
#ii- Default increases with increase in Grade from A-G, A means lowest risk of loan default and G means higher risk of loan default

emp_lengthVsStatus <- ggplot(loan_cleaned, aes(x =emp_length, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'emp_length') +
  scale_fill_discrete(name="Loan_Status") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#iii- length of employment doesn't seems to have much impact on loan status

homeVsStatus <- ggplot(loan_cleaned, aes(x =home_ownership, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'home_ownership') +
  scale_fill_discrete(name="Loan_Status") +theme_few()
#iv- The default rate in Own, rent and mortgage home status is almost same

verificationVsStatus <- ggplot(loan_cleaned, aes(x =verification_status, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'Verification_status') +
  scale_fill_discrete(name="Loan_Status") +theme_few()
#v- The default rate in verified category is slightly more than non verified categories

PurposeVsStatus <- ggplot(loan_cleaned, aes(x =purpose, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'Purpose') +
  scale_fill_discrete(name="Loan_Status") +theme_few()
#vi- The default rate in small business category is highest as compared to other categories followed by renewable energy

StateVsStatus <- ggplot(loan_cleaned, aes(x =addr_state, fill = loan_status)) + geom_bar(stat='count', position='fill') +labs(x = 'State') +
  scale_fill_discrete(name="Loan_Status") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
#vii- The default rate in in NEBRASKA state is high as compared to other states

#5.2- segmented univariate analysis- with segment as Loan status on continous variables
#Correlation between loan status and various continous variables

cor2 <- corrplot(cor(loan_cleaned[,unlist(lapply(loan_cleaned, is.numeric))], use = "complete.obs"), type = "lower", method = "number")

#observation:
#i. binary_status has a negative correlation with loan amount, int_rate, dti, inq_last_6months,pub_rec, revol_util
#ii. binary_status has a positive correlation with last_pymnt_amnt
#iii. we will further investigate the impact of these parameters on loan status

amountVsStatus <- ggplot(loan_cleaned, aes(x= loan_amnt)) + geom_density(aes(fill = as.factor(loan_status)))+  scale_fill_discrete(name="Loan_Status") +xlab("Loan_amount")+theme_few()
#i. Incidences of loan default is frequent when loan amount is above 12000

int_rateVsStatus <- ggplot(loan_cleaned, aes(x= int_rate)) + geom_bar(position = "fill", aes(fill = factor(loan_status)))+  scale_fill_discrete(name="Loan_Status") +xlab("Interest Rate")+theme_few()
#ii. default increases with increase in int_rate in most of the cases

dtiVsStatus <- ggplot(loan_cleaned, aes(x= dti, fill = loan_status)) + geom_density()+  scale_fill_discrete(name="Loan_Status") + theme_few() + scale_x_continuous(breaks = seq(0,30,5))
#iii. default increases with increase in dti upto dti 25 but has an decreasing trend from 25 plus onwards

inquiryVsStatus <- ggplot(loan_cleaned, aes(x= factor(inq_last_6mths), fill = factor(loan_status))) + geom_bar(position = "fill")+  scale_fill_discrete(name="Loan_Status") + xlab("Inquiry in Last 6 months")+theme_few()
#iv. default rate is more for accounts that have 6 or 7 inquires

pub_recVsStatus <- ggplot(loan_cleaned, aes(x= pub_rec, fill = factor(loan_status))) + geom_bar(position = "fill")+  scale_fill_discrete(name="Loan_Status") +theme_few()
#v. default rate is more for accounts that have less than 3 public records

revol_utilVsStatus <- ggplot(loan_cleaned, aes(x= revol_util, fill = loan_status)) + geom_histogram(bins = 20, position = "fill")+  scale_fill_discrete(name="Loan_Status") +theme_few()
#vi. default increases with increase in revol_util percentage

pymntVsStatus <- ggplot(loan_cleaned, aes(x= last_pymnt_amnt, fill = factor(loan_status))) + geom_histogram(bins = 20)+  scale_fill_discrete(name="Loan_Status") +theme_few()
#vii. default decreases with increase in last payment amount

#5.3- Bivariate Analysis

#We will see the relationship of highly correlated variable on each other
#From the cor2 plot we can see that we have positive correlation among the variables,
# i. Installment & Loan_amnt
#ii. total_acc & open_acc
#iii. revol_util & int_rate
#iv. open_acc & dti
#v. last_pymnt_amnt with loan_amnt, installment
#vi. revol_bal with total_acc
#vii. revol_bal with revol_util

#Negative correlation between
#i. dti and annual_income
#ii. open_acc and revol_util

# i. Installment & Loan_amnt

InstVsloan <- ggplot(loan_cleaned, aes(x = loan_amnt, y = installment, color = loan_status)) + geom_jitter()+theme_few()

# Installment increases with increase in loan amount

#ii. total_acc & open_acc

totVsopen_acc <- ggplot(loan_cleaned, aes(x = total_acc, y = open_acc, color = loan_status)) + geom_point() + theme_few()

#iii. revol_util & int_rate

utilVsint <- ggplot(loan_cleaned, aes(x = int_rate, y = revol_util, color = loan_status)) + geom_point(position = "jitter")

#iv. open_acc & dti

openaccVsdti <- ggplot(loan_cleaned, aes(x = open_acc, y = dti)) + geom_point() + facet_wrap(~loan_status)

#v.last_pymnt_amnt with loan_amnt, installment

pymntVsloan <- ggplot(loan_cleaned, aes(x = loan_amnt, y = last_pymnt_amnt)) + geom_point() + facet_wrap(~factor(loan_status))

#vi.last_pymnt_amnt with installment

pymntVsinstallment <- ggplot(loan_cleaned, aes(x = installment, y =last_pymnt_amnt, color = loan_status ))+ geom_point() 

#vii. revol_bal with total_acc

revolVstotal_acc <- ggplot(loan_cleaned, aes(x = total_acc, y = revol_util, color = loan_status)) + geom_point()

#viii. revol_bal and revol_util

balVsutil <- ggplot(loan_cleaned, aes(x = revol_util, y = revol_bal, color = loan_status)) + geom_point()

#ix. dti and annual_income

dtiVsincome <- ggplot(loan_cleaned, aes(x = annual_inc, y = dti, color = loan_status)) + geom_point()

#x. open_acc and revol_util

open_accVsUtil <- ggplot(loan_cleaned, aes(x = open_acc, y = revol_util, color = loan_status)) + geom_point()


#DERIVED METRICS

#i. Ratio of open_acc and total_acc

loan_cleaned <- mutate(loan_cleaned, ratio = open_acc/total_acc)
options(scipen = "999")
acc_ratio <- ggplot(loan_cleaned, aes(x = ratio, fill = loan_status)) + geom_density() + xlab("Ratio of open_acc & total_acc") + theme_few()

#Instances of default increases when the account ratio is above 0.5

#ii. last_credit_pull_d

loan_cleaned <- mutate(loan_cleaned, pull.d = str_split_fixed(last_credit_pull_d, "-", 2)[,2])
credit_pull_dt <- ggplot(loan_cleaned, aes(x= pull.d, fill = loan_status)) + geom_bar(position = "fill") + xlab("Recent Year Credit was pulled") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#default rate increases in most of the cases when when pull date is recent.

#iv.Interest paid

loan_cleaned <- mutate(loan_cleaned, int_paid =  (loan_amnt*int_rate)/100)

int_rate_plot <- ggplot(loan_cleaned, aes(x= int_paid, fill = loan_status)) + geom_histogram(bins = 30, position = "fill") + theme_few()
#Default rate increases when the amount of interest paid is high.

#iii.number of years between issue_d and earliest_cr_line to get length of credit history

loan_cleaned <- mutate(loan_cleaned, cr_history_length =  as.integer(str_split_fixed(issue_d, "-", 2)[,2]) - as.integer(str_split_fixed(earliest_cr_line, "-", 2)[,2]))

# Credit History Grouping

loan_cleaned <- mutate(loan_cleaned, cr_hist_grp = cr_history_length)

loan_cleaned$cr_hist_grp[loan_cleaned$cr_history_length <= 5] <- 'Less_than_5_yrs'
loan_cleaned$cr_hist_grp[loan_cleaned$cr_history_length > 5 & loan_cleaned$cr_history_length <= 10] <- '5_to_10_yrs'
loan_cleaned$cr_hist_grp[loan_cleaned$cr_history_length > 10 & loan_cleaned$cr_history_length <= 15] <- '10_to_15_yrs'
loan_cleaned$cr_hist_grp[loan_cleaned$cr_history_length > 15 & loan_cleaned$cr_history_length <= 20] <- '15_to_20_yrs'
loan_cleaned$cr_hist_grp[loan_cleaned$cr_history_length > 20] <- 'Greater_than_20_yrs'

cr_hist_plot <- ggplot(loan_cleaned, aes(x= cr_hist_grp, fill = loan_status)) + geom_bar(position = "fill") + xlab("Credit_History_Length")+theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

# Loan Default rate is more for those who have less than 5 years of credit history

#v. Income group

loan_cleaned <- mutate(loan_cleaned, inc_grp = annual_inc)

loan_cleaned$inc_grp[loan_cleaned$annual_inc <= 60000] <- "verylow_inc"
loan_cleaned$inc_grp[loan_cleaned$annual_inc > 60000 & loan_cleaned$annual_inc <= 120000] <- 'low_inc'
loan_cleaned$inc_grp[loan_cleaned$annual_inc > 120000 & loan_cleaned$annual_inc <= 180000] <- 'middle_inc'
loan_cleaned$inc_grp[loan_cleaned$annual_inc > 180000 & loan_cleaned$annual_inc <= 240000] <- 'high_inc'
loan_cleaned$inc_grp[loan_cleaned$annual_inc > 240000] <- 'veryhigh_inc'

inc_plot <- ggplot(loan_cleaned, aes(x= inc_grp, fill = loan_status)) + geom_bar(position = "fill") + xlab("Income_groups")+theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))

# Default rate is slightly higher in very low income group

#Conclusions:

#Variables that indicate default

#1. High interest rate with high loan amount
#The interest rate increases with increase in loan amount leading to higher chances of default

#2. Grade
#When a person is assigned Grade A, the risk of default is lowest and G grade shows the risk of default is highest.
#This is because interest rate increase from A-G

#3. revol_util - revolving utilization
#revol_util or revolving utilization is the debt to limit ratio. Hence, higher revol_util is an indicator of loan default.

#4. inq_last_6mths- inquiries in last 6 months
#There is a increase in default when number of inquiries increases in last 6 months.
#Too many inquiries in 6 months is also an indicator that the borrower is not getting loan from anywhere and is desperate to find one, hence, the number of inquiries are high.

#5. dti- debt to income ratio
#incidences of default is evident when the dti is more than 10. So, higher dti is also a good indicator of loan default

#6. Term
#default rate is high on 60 months term

#Additional insights- From various univariate and bivariate analysis
#i. While debt consolidation is the main purpose for borrowing money, the default rate is high in case of small business category
#ii. The state of Nebraska(NE) has more than 50% default rate. On the other hand investing in Indiana(IN), Idaho(IA) & Maine(ME) is a safe bet
#iii. We also found that the last payment amount increases with installment and default is more when the last payment amount is not in line with installment 

#Additional insights- From various derived metrics we also came to know the following,

#i. Incidences of default is evident when the ratio of open_acc to total_acc is more than 0.5
#ii.Investing in Middle income to Very High income group (annual_inc > 120000) is a safe bet
#iv. Default rate increases when the credit history is less than 5 years
