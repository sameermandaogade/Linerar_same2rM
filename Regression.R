# Importing the file
library(readxl)
mydata<-read_xlsx("E:\\Career\\R Analytics\\Data science Case studies\\Linear Regression\\Linear Regression Case.xlsx"
                  ,sheet="customer_dbase")
str(mydata)

mystats <- function(x) {    # User defined function for getting the statistics of numerical variable
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a,is.na=TRUE)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- quantile(a,0.95)
  LC <- quantile(a,0.05)
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

mystats_categorical<-function(x) # Statistics for categorical variable
{
  Var_Type=class(x)
  n<-length(x)
  nmiss<-sum(is.na(x))
  return(c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  
}

# Getting the array of all variables
var <-c("region",	"townsize",	"gender",	"age", "agecat",	"ed",
            "edcat","jobcat",	 "union",	 "employ", "empcat",	 "retire",	
            "income",	 "lninc",	"inccat",	 "debtinc",	 "creddebt",	 "lncreddebt",	
            "othdebt",	"lnothdebt",	"default", "jobsat",	"marital", "spoused",	
            "spousedcat",	"reside", "pets",	"pets_cats", "pets_dogs",	 "pets_birds",	
            "pets_reptiles",	"pets_small",	 "pets_saltfish",	"pets_freshfish",	 "homeown",	"hometype",	
            "address", "addresscat", "cars","carown","cartype",	"carvalue",	
            "carcatvalue","carbought", "carbuy","commute", "commutecat",	 "commutetime",	
            "commutecar",	 "commutemotorcycle",	 "commutecarpool", "commutebus", "commuterail","commutepublic",	
            "commutebike","commutewalk","commutenonmotor", "telecommute",	 "reason", "polview",	
            "polparty",	 "polcontrib", "vote",	"card",	 "cardtype","cardbenefit",	
            "cardfee","cardtenure","cardtenurecat", "card2", "card2type",	"card2benefit",	
            "card2fee","card2tenure",	 "card2tenurecat","carditems","cardspent", "card2items", "card2spent",
            "active", "bfast","tenure",	"churn", "longmon", "lnlongmon",	
            "longten", "lnlongten",	"tollfree",	 "tollmon",	 "lntollmon", "tollten",	
            "lntollten", "equip",	"equipmon","lnequipmon", "equipten", "lnequipten","callcard",
            "cardmon","lncardmon", "cardten",	"lncardten", "wireless", "wiremon",	
            "lnwiremon", "wireten", "lnwireten", "multline", "voice",	 "pager",	
            "internet",	 "callid", "callwait", "forward",	"confer",	"ebill",	
            "owntv","hourstv", "ownvcr", "owndvd","owncd","ownpda",	
            "ownpc" ,"ownipod", "owngame","ownfax", "news",	"response_01","response_02","response_03"
)

# Only categorical variables
cat_var<-c("custid","region",	"townsize",	"gender",	"agecat",	"birthmonth",	"edcat",	"jobcat",	"union",
           "employ",	"empcat",	"retire",	"inccat",	"default",	"jobsat",	"marital",	"spousedcat",
           "homeown",	"hometype",	"address",	"addresscat",	"cars",	"carown",	"cartype",	"carcatvalue",
           "carbought",	"carbuy",	"commute",	"commutecat",	"commutecar",	"commutemotorcycle",	"commutecarpool",	"commutebus",
           "commuterail",	"commutepublic",	"commutebike",	"commutewalk",	"commutenonmotor",	"telecommute",	"reason",	"polview",
           "polcontrib",	"vote",	"card",	"cardtype",	"cardbenefit",	"cardfee",	"cardtenure",	"cardtenurecat",
           "card2type",	"card2benefit",	"card2fee",	"card2tenure",	"card2tenurecat",	"active",	"bfast",	"churn",
           "equip",	"callcard",	"wireless",	"multline",	"voice",	"pager",	"internet",	"callid",
           "forward",	"confer",	"ebill",	"owntv",	"ownvcr",	"owndvd",	"owncd",	"ownpda",
           "ownipod",	"owngame",	"ownfax",	"news",	"response_01",	"response_02",	"response_03"	
)

# removing custid and birthmonth variables intutively as both these variables do not add any significance to outcome
cat_var_1<-c("region",	"gender",	"agecat",	"edcat",	"jobcat",	"union",
           "employ",	"empcat",	"retire",	"inccat",	"default",	"jobsat",	"marital",	"spousedcat",
           "homeown",	"hometype",	"address",	"addresscat",	"cars",	"carown",	"cartype",	"carcatvalue",
           "carbought",	"carbuy",	"commute",	"commutecat",	"commutecar",	"commutemotorcycle",	"commutecarpool",	"commutebus",
           "commuterail",	"commutepublic",	"commutebike",	"commutewalk",	"commutenonmotor",	"telecommute",	"reason",	"polview",
           "polcontrib",	"vote",	"card",	"cardtype",	"cardbenefit",	"cardfee",	"cardtenure",	"cardtenurecat",
           "card2type",	"card2benefit",	"card2fee",	"card2tenure",	"card2tenurecat",	"active",	"bfast",	"churn",
           "equip",	"callcard",	"wireless",	"multline",	"voice",	"pager",	"internet",	"callid",
           "forward",	"confer",	"ebill",	"owntv",	"ownvcr",	"owndvd",	"owncd",	"ownpda",
           "ownipod",	"owngame",	"ownfax",	"news",	"response_01",	"response_02",	"response_03"	
)

# Getting all the continous variables
num_var<-c("age",	"ed",	"income",	"lninc",	"debtinc",	"creddebt",	"lncreddebt",	"othdebt",
           "lnothdebt",	"spoused",	"reside",	"pets",	"pets_cats",	"pets_dogs",	"pets_birds",	"pets_reptiles",
           "pets_small",	"pets_saltfish",	"pets_freshfish",	"carditems",	"cardspent",	"card2items",	"card2spent",	"tenure",
           "longmon",	"lnlongmon",	"longten",	"lnlongten",	"tollmon",	"lntollmon",	"tollten",	"lntollten",
           "equipmon",	"lnequipmon",	"equipten",	"lnequipten",	"cardmon",	"lncardmon",	"cardten",	"lncardten",
           "wiremon",	"lnwiremon",	"wireten",	"lnwireten"				
)


diag_stats_categorical<-t(data.frame(apply(mydata[,cat_var], 2, mystats_categorical))) # for categorical variable
View(diag_stats_categorical)
write.csv(diag_stats_categorical,"E:\\Career\\R Analytics\\Data science Case studies\\Linear Regression 2\\mystats_categorical.csv") 


diag_stats<-t(data.frame(apply(mydata[,num_var], 2, mystats)))     # for continous variable     
View(diag_stats)           
write.csv(diag_stats,"E:\\Career\\R Analytics\\Data science Case studies\\Linear Regression 2\\mystats_num.csv")            

# Treatment for missing values
# We cap the upper outlier with 95 percentile and lower outliers for 5 percentile 
M1_fun <- function(x){
  quantiles <- quantile(x, c(.05, .95 ),na.rm=TRUE )
  # Above line will calc the P5 and P95
  
  x[ x < quantiles[1] ] <- quantiles[1]  # if value < P5, then P1
  x[ x > quantiles[2] ] <- quantiles[2]  # if value > P95, then P95
  x
}

# Applying the func for Outlier Treatment
mydata[,num_var] <- apply(data.frame(mydata[,num_var]), 2, M1_fun) 

#Missing Value Treatment for continuos variables-replacing the missing values with mean in case of continous variable
mydata[,num_var] <- apply(data.frame(mydata[,num_var]), 2, function(x){x <- replace(x, is.na(x), mean(x, na.rm=TRUE))})
#Missing Value Treatment for categorical variables- replacing the missing value with mode in case of categorical variable
mydata[cat_var] <- apply(data.frame(mydata[,cat_var]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})

# The dependent variabe in this case is the totalspend which is the sum of primary card spend and secondary card spend
# Creating a new variable Total spend
mydata$TotalSpend<-mydata$cardspent+mydata$card2spent

# Selecting significant numerical varialbles using factor analysis
# The below array contains the continuos variable after that not contain variable which have missing values in high numbers
f_var<-c("age",	"ed",	"income",	"lninc",	"debtinc",	"creddebt",	"lncreddebt",	"othdebt",
           "lnothdebt",	"spoused",	"reside",	"pets",	"pets_cats",	"pets_dogs",	"pets_birds",	"pets_reptiles",
           "pets_small",	"pets_saltfish",	"pets_freshfish",	"tenure", "longmon",	"lnlongmon",	"longten",	"lnlongten",	"tollmon",	"tollten",	
           "equipmon",	"equipten",		"cardmon",	"cardten", "wiremon",		"wireten"		
)

fact_mydata<-mydata[f_var]
zv <- apply(fact_mydata, 2, function(x) length(unique(x)) == 1)   # This was done for  the warning which shows
sum(zv)                                                           # some variables have std dev 0
fact_mydata <- fact_mydata[, !zv]                                 # so here I remove the variables that showing std dev 0

corrm<-cor(fact_mydata)

require(psych)
require(GPArotation)


### DECIDING NUMBER OF FACTORS USING SCREE PLOT & KAISER TEST(NUMBER OF EIGEN VALUES OVER 1)

plot.new()
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=T) ### SCREE PLOT
VSS.scree(corrm, main = "scree plot")


require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values)) 



plot(eigen_values$pct_var,type='b')

# 8 factors constitute for 73%  and 7 factors for 70%  variance so I decided 8 or 7 as a optimal number of factors

FA<-fa(r=corrm, 7, rotate="varimax", fm="ml")             

print(FA)                                               

FA_SORT<-fa.sort(FA)                                

ls(FA_SORT)                                       
FA_SORT$loadings
#FA_SORT$e.values                                         
Loadings<-data.frame(FA_SORT$loadings[1:ncol(fact_mydata),]) 
View(Loadings)
write.csv(Loadings, "E://Career//R Analytics//Data science Case studies//Linear Regression 2//loading.csv") ### SAVING THE FILE

# Significant selected numerical variables
s_num_var<-c("lnlongten",	"lnlongmon",	"tenure",	"lnothdebt",	"lninc",	"lncreddebt",	
             "equipmon",	"equipten",	"tollmon",	"pets",	"pets_freshfish",	"debtinc",	"cardmon"
)


#Apply transformation on numeric Variables (log, sqrt, squre, exp) 
# and identify which transformation gives  best correlation between dependent and independent variables.


T1<-cor(mydata[,"TotalSpend"],mydata[,s_num_var])
T1<-as.data.frame(T1)

# Taking square of the numeric variable
mydata_S<- apply(mydata[,s_num_var], 2, function(x) x^2)
T2<-cor(mydata[,"TotalSpend"],mydata_S)
T2<-as.data.frame(T2)

# Taking square root of the numeric variables
mydata_SR<- apply(mydata[,s_num_var], 2, function(x) x^0.5)
T3<-cor(mydata[,"TotalSpend"],mydata_SR)
T3<-as.data.frame(T3)

# Taking exponent of numeric variable
mydata_E<- apply(mydata[,s_num_var], 2, function(x) exp(x) )
T4<-cor(mydata[,"TotalSpend"],mydata_E)
T4<-as.data.frame(T4)

T<-rbind(T1,T2,T3,T4)
write.csv(T,"E://Career//R Analytics//Data science Case studies//Linear Regression 2//transform12.csv")

# Note : Further analysis on which transformation apply to which variable has been done in excel file 

# transforming the variable to get the best correlationn between dependent and independent variable

mydata_T<-mydata # Creating a duplicate dataset

c1<-c("equipten",	"tollmon")
mydata_T[,c1]<- apply(mydata[,c1], 2, function(x) x^2)  # square transformation

c2<-c("lnlongmon","tenure","pets_freshfish","cardmon")
mydata_T[,c2]<- apply(mydata[,c2], 2, function(x) x^0.5)  # square root transformation

c3<-c( "pets",	"lnothdebt",	"debtinc","lncreddebt","equipmon")
mydata_T[,c3]<- apply(mydata[,c3], 2, function(x) exp(x))  # Exponential transformation transformation
sum(is.na(mydata_T[,c1]))
#============================================================================================================#
mydata1<-mydata # Creating duplicate data 
# To choose the significant categorical variables we perform anova test ,the variables which are significant
# are selected are decided on the basis of F values , statistically significant categorical variables are 
# are commented with "# s"

# First we select significant categorical variable and after that we perform step wise linear regression we convert
# them in to factor but for the anova test we have the need to convert them in to factor therefor we make duplicate
# data set mydata1 and convert the categorical variables in to factor


mydata1[,cat_var]<-lapply(mydata[,cat_var],as.factor)


aov_region=aov(mydata1$TotalSpend~mydata1$region) 
summary(aov_region)
aov_townsize=aov(mydata1$TotalSpend~mydata1$townsize)	
summary(aov_townsize)
aov_gender=aov(mydata1$TotalSpend~mydata1$gender)	# s
summary(aov_gender)
aov_agecat=aov(mydata1$TotalSpend~mydata1$agecat) # s
summary(aov_agecat)
aov_birthmonth=aov(mydata1$TotalSpend~mydata1$birthmonth)	
summary(aov_birthmonth)
aov_edcat=aov(mydata1$TotalSpend~mydata1$edcat)	# s
summary(aov_edcat)
aov_jobcat=aov(mydata1$TotalSpend~mydata1$jobcat) # s
summary(aov_jobcat)
aov_union=aov(mydata1$TotalSpend~mydata1$union)	
summary(aov_union)
aov_employ=aov(mydata1$TotalSpend~mydata1$employ) # s
summary(aov_employ)
aov_empcat=aov(mydata1$TotalSpend~mydata1$empcat) # s
summary(aov_empcat)
aov_retire=aov(mydata1$TotalSpend~mydata1$retire) # s
summary(aov_retire)
aov_inccat=aov(mydata1$TotalSpend~mydata1$inccat) # s 
summary(aov_inccat)
aov_default=aov(mydata1$TotalSpend~mydata1$default)	
summary(aov_default)
aov_jobsat=aov(mydata1$TotalSpend~mydata1$jobsat)	# s
summary(aov_jobsat)
aov_marital=aov(mydata1$TotalSpend~mydata1$marital)	
summary(aov_marital)
aov_spousedcat=aov(mydata1$TotalSpend~mydata1$spousedcat) # s
summary(aov_spousedcat)
aov_homeown=aov(mydata1$TotalSpend~mydata1$homeown)	# s
summary(aov_homeown)
aov_hometype=aov(mydata1$TotalSpend~mydata1$hometype) # s
summary(aov_hometype)
aov_address=aov(mydata1$TotalSpend~mydata1$address)	
summary(aov_address)
aov_addresscat=aov(mydata1$TotalSpend~mydata1$addresscat) # s
summary(aov_addresscat)
aov_cars=aov(mydata1$TotalSpend~mydata1$cars)	
summary(aov_cars)
aov_carown=aov(mydata1$TotalSpend~mydata1$carown) # s
summary(aov_carown)
aov_cartype=aov(mydata1$TotalSpend~mydata1$cartype)
summary(aov_cartype)
aov_carcatvalue=aov(mydata1$TotalSpend~mydata1$carcatvalue) # s
summary(aov_carcatvalue)
aov_carbought=aov(mydata1$TotalSpend~mydata1$carbought)
summary(aov_carbought)
aov_carbuy=aov(mydata1$LTotalSpend~mydata1$carbuy)	
summary(aov_carbuy)
aov_commute=aov(mydata1$TotalSpend~mydata1$commute)	
summary(aov_commute)
aov_commutecat=aov(mydata1$TotalSpend~mydata1$commutecat)
summary(aov_commutecat)
aov_commutecar=aov(mydata1$TotalSpend~mydata1$commutecar)	
summary(aov_commutecar)
aov_commutemotorcycle=aov(mydata1$TotalSpend~mydata1$commutemotorcycle)
summary(aov_commutemotorcycle)
aov_commutecarpool=aov(mydata1$TotalSpend~mydata1$commutecarpool)	
summary(aov_commutecarpool)
aov_commutebus=aov(mydata1$TotalSpend~mydata1$commutebus)
summary(aov_commutebus)
aov_commuterail=aov(mydata1$TotalSpend~mydata1$commuterail)
summary(aov_commuterail)
aov_commutepublic=aov(mydata1$TotalSpend~mydata1$commutepublic)
summary(aov_commutepublic)
aov_commutebike=aov(mydata1$TotalSpend~mydata1$commutebike)	
summary(aov_commutebike)
aov_commutewalk=aov(mydata1$TotalSpend~mydata1$commutewalk)	
summary(aov_commutewalk)
aov_commutenonmotor=aov(mydata1$TotalSpend~mydata1$commutenonmotor)
summary(aov_commutenonmotor)
aov_telecommute=aov(mydata1$TotalSpend~mydata1$telecommute)
summary(aov_telecommute)
aov_reason=aov(mydata1$TotalSpend~mydata1$reason) # s
summary(aov_reason)
aov_polview=aov(mydata1$TotalSpend~mydata1$polview)
summary(aov_polview)
aov_polparty=aov(mydata1$TotalSpend~mydata1$polparty)	
summary(aov_polparty)
aov_polcontrib=aov(mydata1$TotalSpend~mydata1$polcontrib)	
summary(aov_polcontrib) 
aov_vote=aov(mydata1$TotalSpend~mydata1$vote) # s
summary(aov_vote)
aov_card=aov(mydata1$TotalSpend~mydata1$card)  # s
summary(aov_card)
aov_cardtype=aov(mydata1$TotalSpend~mydata1$cardtype)
summary(aov_cardtype)
aov_cardbenefit=aov(mydata1$TotalSpend~mydata1$cardbenefit)	
summary(aov_cardbenefit)
aov_cardfee=aov(mydata1$TotalSpend~mydata1$cardfee)	
summary(aov_cardfee)
aov_cardtenure=aov(mydata1$TotalSpend~mydata1$cardtenure)  # s
summary(aov_cardtenure)
aov_cardtenurecat=aov(mydata1$TotalSpend~mydata1$cardtenurecat)  # s
summary(aov_cardtenurecat)
aov_card2=aov(mydata1$TotalSpend~mydata1$card2)  # s
summary(aov_card2)
aov_card2type=aov(mydata1$TotalSpend~mydata1$card2type)
summary(aov_card2type)
aov_card2benefit=aov(mydata1$TotalSpend~mydata1$card2benefit)	
summary(aov_card2benefit)
aov_card2fee=aov(mydata1$TotalSpend~mydata1$card2fee)
summary(aov_card2fee)
aov_card2tenure=aov(mydata1$TotalSpend~mydata1$card2tenure) # s
summary(aov_card2tenure)
aov_card2tenurecat=aov(mydata1$LTotalSpend~mydata1$card2tenurecat)	# s R
summary(aov_card2tenurecat)
aov_active=aov(mydata1$TotalSpend~mydata1$active)
summary(aov_active)
aov_bfast=aov(mydata1$TotalSpend~mydata1$bfast) # s R
summary(aov_bfast)
aov_churn=aov(mydata1$TotalSpend~mydata1$churn)
summary(aov_churn)
aov_tollfree=aov(mydata1$TotalSpend~mydata1$tollfree) # s
summary(aov_tollfree)
aov_equip=aov(mydata1$TotalSpend~mydata1$equip) # s	
summary(aov_equip)
aov_callcard=aov(mydata1$TotalSpend~mydata1$callcard)
summary(aov_callcard)
aov_wireless=aov(mydata1$TotalSpend~mydata1$wireless) # s
summary(aov_wireless)
aov_multline=aov(mydata1$TotalSpend~mydata1$multline) # s
summary(aov_multline)
aov_voice=aov(mydata1$TotalSpend~mydata1$voice) # s
summary(aov_voice)
aov_pager=aov(mydata1$TotalSpend~mydata1$pager) # s
summary(aov_pager)
aov_internet=aov(mydata1$TotalSpend~mydata1$internet) # s
summary(aov_internet)
aov_callid=aov(mydata1$TotalSpend~mydata1$callid) # s
summary(aov_callid)
aov_callwait=aov(mydata1$TotalSpend~mydata1$callwait) # s
summary(aov_callwait)
aov_forward=aov(mydata1$TotalSpend~mydata1$forward)	# s
summary(aov_forward)
aov_confer=aov(mydata1$TotalSpend~mydata1$confer) # s
summary(aov_confer)
aov_ebill=aov(mydata1$TotalSpend~mydata1$ebill)	
summary(aov_ebill)
aov_owntv=aov(mydata1$TotalSpend~mydata1$owntv) # s
summary(aov_owntv)
aov_ownvcr=aov(mydata1$TotalSpend~mydata1$ownvcr) # s
summary(aov_ownvcr)
aov_owndvd=aov(mydata1$TotalSpend~mydata1$owndvd) # s
summary(aov_owndvd)
aov_owncd=aov(mydata1$TotalSpend~mydata1$owncd) # s
summary(aov_owncd)
aov_ownpda=aov(mydata1$TotalSpend~mydata1$ownpda) # s
summary(aov_ownpda)
aov_ownpc=aov(mydata1$TotalSpend~mydata1$ownpc) # s
summary(aov_ownpc)
aov_ownipod=aov(mydata1$TotalSpend~mydata1$ownipod)	
summary(aov_ownipod)
aov_owngame=aov(mydata1$TotalSpend~mydata1$owngame) # s R
summary(aov_owngame)
aov_ownfax=aov(mydata1$TotalSpend~mydata1$ownfax) # s
summary(aov_ownfax)
aov_news=aov(mydata1$TotalSpend~mydata1$news) # s N
summary(aov_news)
aov_response_01=aov(mydata1$TotalSpend~mydata1$response_01)	
summary(aov_response_01)
aov_response_02=aov(mydata1$TotalSpend~mydata1$response_02)	
summary(aov_response_02)
aov_response_03=aov(mydata1$TotalSpend~mydata1$response_03) # s
summary(aov_response_03)


#Selected categorical variables after ANOVA test
cat_var_sel<-c("gender",	"agecat",	"edcat",	"jobcat",	"empcat",	"retire",	"inccat",
               "jobsat",	"homeown",	"addresscat",	"carown",	"carcatvalue",	"reason",
               "vote",	"card",	"cardtenurecat",	"card2",	"card2tenurecat",	
               "tollfree",	"equip",	"wireless",	"multline",	"voice",	"pager",	"internet",	"callid",
               "callwait",	"forward",	"confer",	"owntv",	"ownvcr",	"owndvd",	"owncd",	"ownpda",
               "ownpc","news",	"ownfax",	"response_03"			
)
str(mydata[,cat_var_sel])

mydata_T[,cat_var_sel] <- apply(data.frame(mydata_T[,cat_var_sel]), 2, function(x){x <- trimws(x)})

# Initially I had converted categorical variable into  factor but before step wise regression I need my categorical variable
# as numeric .
mydata_T[,cat_var_sel] <- apply(data.frame(mydata_T[,cat_var_sel]), 2, function(x){x <- as.numeric(x)})

mydata_T[,cat_var_sel] <- apply(data.frame(mydata_T[,cat_var_sel]), 2, function(x){x <- replace(x, is.na(x), which.max(prop.table(table(x))))})
View(mydata[,cat_var_sel])
sum(is.na(mydata_T[,cat_var]))

# Making a linear model
fit0 <- lm(TotalSpend~gender+	agecat+	edcat+	jobcat+	empcat+	retire+	inccat+					
            jobsat+	homeown+	addresscat+	carown+	carcatvalue+	reason+					
            vote+	card+	cardtenurecat+	card2+card2tenurecat+						
            tollfree+	equip+	wireless+	multline+	voice+	pager+	internet+	callid+					
            callwait+	forward+	confer+	owntv+	ownvcr+	owndvd+	owncd+	ownpda+					
            ownpc+	ownfax+	response_03+news	+								
            lnlongten+	lnlongmon+	tenure+	lnothdebt+	lninc+	lncreddebt+	equipmon+	equipten+	tollmon+	pets+	pets_freshfish+	debtinc+	cardmon+
            tollmon+	pets+	pets_freshfish+	debtinc+	cardmon	,data = mydata_T						
          )			

summary(fit0)

# To check whether dependent variable is normal or not
hist(log(mydata$TotalSpend))

mydata$LTotalSpend<-log(mydata$TotalSpend) # Applying log transformation


require(MASS)
require(faraway)

# Now applying box cos transformation
boxcox(fit0, plotit = TRUE)

boxcox(fit0, plotit = TRUE, lambda = seq(0.1, 1.5, by = 0.02))
# Lambda comes out to be 0.2

hist((mydata$TotalSpend^0.20-1)/0.20)


# Normalising the dependent variable
mydata_T$TotalSpendNormal<-(mydata_T$TotalSpend^0.20-1)/0.20
sum(is.na(mydata_T$TotalSpendNormal))
hist(mydata_T$TotalSpendNormal)

fit <- lm(TotalSpendNormal~gender+	agecat+	edcat+	jobcat+	empcat+	retire+	inccat+					
            jobsat+	homeown+	addresscat+	carown+	carcatvalue+	reason+					
            vote+	card+	cardtenurecat+	card2+card2tenurecat+						
            tollfree+	equip+	wireless+	multline+	voice+	pager+	internet+	callid+					
            callwait+	forward+	confer+	owntv+	ownvcr+	owndvd+	owncd+	ownpda+					
            ownpc+	ownfax+	response_03+news	+								
            lnlongten+	lnlongmon+	tenure+	lnothdebt+	lninc+	lncreddebt+	equipmon+	equipten+	tollmon+	pets+	pets_freshfish+	debtinc+	cardmon+
            tollmon+	pets+	pets_freshfish+	debtinc+	cardmon	,data = mydata_T						
          					 )			

summary(fit)


require(MASS)
# using step wise regression to reduce the variable and to get significant one.
step0<-stepAIC(fit,direction = "both")


fit2<-lm(TotalSpendNormal ~ gender + agecat + edcat + jobcat + card + 
           cardtenurecat + card2 + voice + internet + response_03 + 
           tenure + lninc + lncreddebt + cardmon,data=mydata_T)
summary(fit2)

require(car)
vif(fit2) # Checking for multicollinearity and removing variables if any in further

fit3<-lm(TotalSpendNormal ~ gender + agecat + edcat + jobcat + card + 
           card2 + voice + internet + response_03 + 
           tenure + lninc + lncreddebt + cardmon,data=mydata_T)
vif(fit3)
summary(fit3)

# Final selected categorical variables
f_cat_var<-c("gender" ,"edcat" ,"agecat","jobcat", "card" 
               ,"card2" , "voice", 
               "internet", "response_03" )

# Now I am converting the categorical variable into factor
mydata_T$gender=factor(mydata_T$gender)
mydata_T$edcat=factor(mydata_T$edcat)
mydata_T$agecat=factor(mydata_T$agecat)
mydata_T$jobcat=factor(mydata_T$jobcat)
mydata_T$card=factor(mydata_T$card)
mydata_T$card2=factor(mydata_T$card2)
mydata_T$voice=factor(mydata_T$voice)
mydata_T$internet=factor(mydata_T$internet)
mydata_T$response_03=factor(mydata_T$response_03)

fit4<-lm(TotalSpendNormal ~ gender + agecat + edcat + jobcat + card + 
           card2 + voice + internet + response_03 + 
           tenure + lninc + lncreddebt + cardmon,data=mydata_T)

summary(fit4)

vif(fit4) # Again checking for multicollinearity after converting into factor

# Dividing the data into training and partition one.
require('caret')
set.seed(123)
train_ind<-createDataPartition(y=mydata_T$TotalSpendNormal,p=0.7,list = FALSE)

training<-mydata_T[train_ind,] # Dev data set
testing<-mydata_T[-train_ind,] # val data set

fit4<-lm(TotalSpendNormal ~ gender + agecat + edcat + jobcat + card + 
           card2 + voice + internet + response_03 + 
           tenure + lninc + lncreddebt + cardmon,data=training)

summary(fit4)

#Here I calculate cook's D value 
training$Cd<- cooks.distance(fit4)
#thereafter remove influential observation 
#This would give better R-sqr value.
training1<-subset(training, Cd< (4/3500))
                  
#run the regression model with new dev dataset and same set of variables 
fit5<-lm(TotalSpendNormal ~ gender + agecat + edcat + jobcat + card + 
           card2 + voice + internet + response_03 + 
           tenure + lninc + lncreddebt + cardmon,data=training1)
summary(fit5)

# Again applying step  wise regression to reduce and discard the insignificant variable
step1<-stepAIC(fit5,direction = "both")

fit6<-lm(TotalSpendNormal ~ gender + jobcat + card + card2 + voice + response_03 + 
           lninc + lncreddebt + cardmon,data=training1)

mean(fit6$residuals)
summary(fit6)
par(mfrow=c(2,2))
plot(fit6)

#-----------------------------------------------------------------------------------------------
## Checking the scoring for fit3

###############################################################
x<-predict(fit6)
y=(0.2*x+1)^5    # Denormalising the dependent variable

t1_fit<-cbind(training1, pred_spend = y)

t1_fit<- transform(t1_fit, APE = abs(pred_spend - TotalSpend)/TotalSpend)
mean(t1_fit$APE)


x_t<-predict(fit6,testing)
y_t=(0.2*x_t+1)^5  # Denormalising the dependent variable    
t2_fit<-cbind(testing, pred_spend=y_t)
t2_fit<- transform(t2_fit, APE = abs(pred_spend - TotalSpend)/TotalSpend)
mean(t2_fit$APE)

##################################Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1_fit$pred_spend, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1_fit$decile <- findInterval(t1_fit$pred_spend,c(-Inf,decLocations, Inf))

require(sqldf)
t1_fitDA <- sqldf("select decile, count(decile) as count, avg(pred_spend) as avg_spent,   
                   avg(TotalSpend) as avg_Actual_spend
                   from t1_fit
                   group by decile
                   order by decile desc")

View(t1_fitDA)
write.csv(t1_fitDA,"E:\\Career\\R Analytics\\Data science Case studies\\Linear Regression 2\\Decile_pred_and_actual.csv")


##################################Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2_fit$pred_spend, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2_fit$decile <- findInterval(t2_fit$pred_spend,c(-Inf,decLocations, Inf))

require(sqldf)
t2_fitDA <- sqldf("select decile, count(decile) as count, avg(pred_spend) as avg_pre_spend,   
                   avg(TotalSpend) as avg_Actual_spent
                   from t2_fit
                   group by decile
                   order by decile desc")

View(t2_fitDA)
write.csv(t2_fitDA,"E:\\Career\\R Analytics\\Data science Case studies\\Linear Regression 2\\Decile_pred_and_actual_test.csv")



# Prioritize the drivers based on the importance
# So we have to decide which are the factor that are important and accordingly prioritize them.
# This is done using by standardised betas.
# Standardised (or beta ) coefficents from a linear regression model are the parameter estimates obtained when
# the predictors and outcomes have been standardised to variance 1

# To find the standardised coefficient I am using the package QuantPsyc and use the function lm.beta which simply takes the
# model as an argument


require(QuantPsyc)
std_coeff<-lm.beta(fit6)
View(std_coeff)
write.csv(std_coeff,"E:\\Career\\R Analytics\\Data science Case studies\\Linear Regression 2\\Std_coeff.csv")



# Correlation between actual and predicted spend
cor(t1_fit$TotalSpend,t1_fit$pred_spend)
cor(t2_fit$TotalSpend,t2_fit$pred_spend)
