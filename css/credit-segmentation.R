# Set the directory
setwd("C:\\Users\\Shubham\\Documents\\BA Case Study\\Final Case Study 1 - Credit Card Segmentation")
getwd()

# Importing the file
cc <- read.csv("CC GENERAL.csv")

# Exploring the data
View(cc)
str(cc)
names(cc)

cc$CUST_ID <- NULL


############ creating new variables and validating the values.###############

cc$m_avg_purchase <- cc$PURCHASES/12
cc$m_avg_cash_adamount <- cc$CASH_ADVANCE/12
cc$purchase_type <- as.numeric(ifelse(cc$INSTALLMENTS_PURCHASES > cc$ONEOFF_PURCHASES,0,
                                             ifelse(cc$INSTALLMENTS_PURCHASES == cc$ONEOFF_PURCHASES,1,
                                                    ifelse(cc$INSTALLMENTS_PURCHASES < cc$ONEOFF_PURCHASES,1,""))))

cc$avg_amt_per_purchase <- cc$PURCHASES/cc$PURCHASES_TRX
cc$avg_amt_per_purchase[is.na(cc$avg_amt_per_purchase)] <- 0


cc$avg_amt_per_purchase[!is.finite(cc$avg_amt_per_purchase)] <- 0

cc$avg_cash_advance_amt <- cc$CASH_ADVANCE/cc$CASH_ADVANCE_TRX
cc$avg_cash_advance_amt[!is.finite(cc$avg_cash_advance_amt)] <- 0


cc$limit_usage <-cc$BALANCE/cc$CREDIT_LIMIT
cc$pay_ratio <- cc$PAYMENTS/cc$MINIMUM_PAYMENTS

# Creating user defined function for descriptive statistics
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

vars<-c("BALANCE","BALANCE_FREQUENCY",
        "PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES",
        "CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY",
        "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX",
        "PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS",
        "MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE",
        "m_avg_purchase","m_avg_cash_adamount","avg_amt_per_purchase","avg_cash_advance_amt","limit_usage",
        "pay_ratio")

diag_stats<-t(data.frame(apply(cc[vars], 2, mystats)))

write.csv(diag_stats, "diag_stats.csv")

# Outliers Capping
cc$BALANCE[cc$BALANCE>5909.112]<- 5909.112
cc$PURCHASES[cc$PURCHASES>3998.62]<- 3998.62
cc$ONEOFF_PURCHASES[cc$ONEOFF_PURCHASES>2761.094]<- 2761.094
cc$INSTALLMENTS_PURCHASES[cc$INSTALLMENTS_PURCHASES>1750.088]<- 1750.088
cc$CASH_ADVANCE[cc$CASH_ADVANCE>4647.169]<- 4647.169
cc$CREDIT_LIMIT[cc$CREDIT_LIMIT>12000]<- 12000
cc$PAYMENTS[cc$PAYMENTS>6082.091]<- 6-82.091
cc$MINIMUM_PAYMENTS[cc$MINIMUM_PAYMENTS>2766.563]<- 2766.563
cc$m_avg_purchase[cc$m_avg_purchase>333.2183]<- 333.2183
cc$m_avg_cash_adamount[cc$m_avg_cash_adamount>387.2641]<- 387.2641
cc$avg_amt_per_purchase[cc$avg_amt_per_purchase>228.2108]<- 228.2108
cc$avg_cash_advance_amt[cc$avg_cash_advance_amt>926.7271]<- 926.7271
cc$limit_usage[cc$limit_usage > 0.966694]<- 0.966694
cc$pay_ratio[cc$pay_ratio == 21.43824]<- 21.43824

View(cc)

# Missing value imputation
miss_impuation<- function(x){
  x[is.na(x)]<-mean(x,na.rm=T)
  x
}


cc[vars]<-apply(cc[vars],2, FUN=miss_impuation)

View(cc)

## FACTOR ANALYSIS 
corrm<- cor(cc)                                 ### CORRELATION MATRIX

require(psych)
require(GPArotation)

### DECIDING NUMBER OF FACTORS USING SCREE PLOT & EIGEN VALUES OVER 1

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT

eigen(corrm)$values                                                     ### EIGEN VALUES

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))  # CALCULATING VARIANCE 

write.csv(eigen_values, "eigenvalue.csv", row.names = F)

FA <- fa(r=corrm, 9, rotate = "varimax", fm="ml")      #CONDUCTING FACTOR ANALYSIS
print(FA)
FA_SORT <- fa.sort(FA)
ls(FA_SORT)
FA_SORT$loadings
loadings <- data.frame(FA_SORT$loadings[1:ncol(cc),])
write.csv(loadings, "loadings.csv", row.names = T)

#Prepare final Data
final_data <- cc

# Scaling
final_data <- scale(cc[,c("BALANCE",
                                 "PURCHASES","ONEOFF_PURCHASES",
                                 "CASH_ADVANCE","INSTALLMENTS_PURCHASES","PURCHASES_TRX",
                                 "PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_TRX",
                                 "CREDIT_LIMIT","PAYMENTS",
                                 "BALANCE_FREQUENCY")])

View(final_data)

#standardizing the data

final_data = scale(final_data)
View(final_data)

#building clusters using k-means clustering 

cluster_three <- kmeans(final_data,3)
cluster_four <- kmeans(final_data,4)
cluster_five <- kmeans(final_data,5)
cluster_six <- kmeans(final_data,6)

cluster_three$cluster


cc_new<-cbind(cc,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(cc_new)

#Graph based on k-means - Optional
require(cluster)

clusplot(final_data, #dataframe
         cluster_five$cluster, #clusterdata
         color = TRUE, #color
         shade = TRUE, # Lines in clusters
         lines =6, # lines connecting centroids
         labels = 2 # Labels clusters and cases
)

###Profiling

#Converting into factors
cc_new$km_clust_3=factor(cc_new$km_clust_3)
cc_new$km_clust_4=factor(cc_new$km_clust_4)
cc_new$km_clust_5=factor(cc_new$km_clust_5)
cc_new$km_clust_6=factor(cc_new$km_clust_6)

names(cc)
require(tables)
profile<-tabular(1+BALANCE+PURCHASES+ONEOFF_PURCHASES+CASH_ADVANCE+INSTALLMENTS_PURCHASES
                 +PURCHASES_TRX+PURCHASES_INSTALLMENTS_FREQUENCY+CASH_ADVANCE_TRX+CREDIT_LIMIT
                 +PAYMENTS+BALANCE_FREQUENCY ~ 
                   mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                 data=cc_new)

profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)

profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=cc_new)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)

write.csv(profile1,"profile1.csv",row.names = F)
write.csv(profile2,"profile2.csv",row.names = F)

