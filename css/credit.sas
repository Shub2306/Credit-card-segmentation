libname b '/folders/myfolders/BA Case Study/Final Case Study 1 - Credit Card Segmentation';

proc import datafile='/folders/myfolders/BA Case Study/Final Case Study 1 - Credit Card Segmentation/CC General.csv'
out= b.cc  dbms=csv replace;
getnames= yes;
run;

/* Data Exploration */

Proc contents data=b.cc varnum;
run;

/* Outlier Treatment */ 
proc means data=b.cc nmiss min max mean std var;
run;

/* Creating validobs to identify outliers */
data cluster;
set b.cc;

valid_obs_2=1;
if BALANCE > 5727.53 or
PURCHASES > 5276.46 or
ONEOFF_PURCHASES > 3912.2173709 or
INSTALLMENTS_PURCHASES > 2219.7438751 or
CASH_ADVANCE > 5173.1911125 or
CASH_ADVANCE_TRX > 16.8981202 or
PURCHASES_TRX > 64.4251306 or
CREDIT_LIMIT > 11772.09 or
PAYMENTS > 7523.26 or
MINIMUM_PAYMENTS > 5609.1065423 or
TENURE > 14.19398 
then valid_obs_2=0;

valid_obs_3=1;
if BALANCE > 7809.06 or
PURCHASES > 7413.09 or
ONEOFF_PURCHASES > 5572.1073709 or
INSTALLMENTS_PURCHASES > 3124.0819903 or
CASH_ADVANCE > 7270.3511125 or
CASH_ADVANCE_TRX > 23.7227669 or
PURCHASES_TRX > 89.2827797 or
CREDIT_LIMIT > 15410.91 or
PAYMENTS > 10418.32 or
MINIMUM_PAYMENTS > 7981.5565423 or
TENURE > 15.5323108
then valid_obs_3=0;
run;

proc freq data=cluster;
table valid_obs_2 valid_obs_3;
run;

/*************** MISSING VALUE TREATMENT *********************/

data b.cc;
set b.cc;
if Mnth_avg_pur=. then Mnth_avg_pur = 0;
if Avg_cash_adv=. then Avg_cash_adv=0;
if Bal_To_limit=. then Bal_To_limit=0;
if Pay_by_min=. then Pay_by_min=0;
if minimum_payments=. then minimum_payments=864.2065423;
if credit_limit=. then credit_limit=4494.45;
run;

/* Advanced data prepration */

data b.cc;
set b.cc;
Mnth_avg_pur = PURCHASES/(PURCHASES_FREQUENCY*TENURE);
Avg_cash_adv = CASH_ADVANCE / CASH_ADVANCE_TRX;
Mnthly_cash_adv = CASH_ADVANCE/TENURE;
Bal_To_limit = BALANCE/CREDIT_LIMIT;
Pay_by_min   = PAYMENTS/MINIMUM_PAYMENTS;
if ONEOFF_PURCHASES=0 & INSTALLMENTS_PURCHASES=0 then purchase_type = 'None';
if ONEOFF_PURCHASES>0 & INSTALLMENTS_PURCHASES=0 then purchase_type = 'One_Of';
if ONEOFF_PURCHASES=0 & INSTALLMENTS_PURCHASES>0 then purchase_type = 'Installment_Purchases';
if ONEOFF_PURCHASES>0 & INSTALLMENTS_PURCHASES>0 then purchase_type = 'Both';
run;


/* Standardizing segmentation variable and selection valid_obs*/
data cluster;
set cluster;

Z_BALANCE =BALANCE;
Z_PURCHASES = PURCHASES;
Z_ONEOFF_PURCHASES=ONEOFF_PURCHASES;
Z_INSTALLMENTS_PURCHASES=INSTALLMENTS_PURCHASES;
Z_CASH_ADVANCE=CASH_ADVANCE;
Z_CASH_ADVANCE_TRX=CASH_ADVANCE_TRX;
Z_PURCHASES_TRX=PURCHASES_TRX;
Z_CREDIT_LIMIT=CREDIT_LIMIT;
Z_PAYMENTS=PAYMENTS;
Z_MINIMUM_PAYMENTS=MINIMUM_PAYMENTS;
Z_TENURE=TENURE;
Z_PURCHASES_FREQUENCY=PURCHASES_FREQUENCY;
Z_ONEOFF_PURCHASES_FREQUENCY=ONEOFF_PURCHASES_FREQUENCY;
Z_PURCHASES_INSTALL_FREQ=PURCHASES_INSTALLMENTS_FREQUENCY;
Z_PRC_FULL_PAYMENT=PRC_FULL_PAYMENT;
Z_BALANCE_FREQUENCY=BALANCE_FREQUENCY;
Z_CASH_ADVANCE_FREQUENCY=CASH_ADVANCE_FREQUENCY;

where valid_obs_3=1;
run;

/* Factor Analysis */

proc factor data=cluster method=principal 
mineigen=0 nfactors=6 scree rotate= varimax reorder;
var 
BALANCE
BALANCE_FREQUENCY
PURCHASES
ONEOFF_PURCHASES
INSTALLMENTS_PURCHASES
CASH_ADVANCE
PURCHASES_FREQUENCY
ONEOFF_PURCHASES_FREQUENCY
PURCHASES_INSTALLMENTS_FREQUENCY
CASH_ADVANCE_FREQUENCY
CASH_ADVANCE_TRX
PURCHASES_TRX
CREDIT_LIMIT
PAYMENTS
MINIMUM_PAYMENTS
PRC_FULL_PAYMENT
TENURE;
run;

/*  VARIABLE STANDARDIZATION */

proc standard data=cluster out=cluster mean=0 std=1;
var 
Z_ONEOFF_PURCHASES
Z_PURCHASES
Z_PAYMENTS
Z_CASH_ADVANCE_TRX
Z_CASH_ADVANCE
Z_CASH_ADVANCE_FREQUENCY
Z_BALANCE
Z_INSTALLMENTS_PURCHASES
Z_PURCHASES_TRX
Z_PRC_FULL_PAYMENT
Z_TENURE
Z_CREDIT_LIMIT
Z_BALANCE_FREQUENCY;
run;

/*  Clustering  */

proc fastclus data=cluster out=cluster cluster=cluster3 maxclusters=3 maxiter=100;
var
Z_ONEOFF_PURCHASES
Z_PURCHASES
Z_PAYMENTS
Z_CASH_ADVANCE_TRX
Z_CASH_ADVANCE
Z_CASH_ADVANCE_FREQUENCY
Z_BALANCE
Z_INSTALLMENTS_PURCHASES
Z_PURCHASES_TRX
Z_PRC_FULL_PAYMENT
Z_TENURE
Z_CREDIT_LIMIT
Z_BALANCE_FREQUENCY;
run;


proc fastclus data=cluster out=cluster cluster=cluster4 maxclusters=4 maxiter=100;
var 
Z_ONEOFF_PURCHASES
Z_PURCHASES
Z_PAYMENTS
Z_CASH_ADVANCE_TRX
Z_CASH_ADVANCE
Z_CASH_ADVANCE_FREQUENCY
Z_BALANCE
Z_INSTALLMENTS_PURCHASES
Z_PURCHASES_TRX
Z_PRC_FULL_PAYMENT
Z_TENURE
Z_CREDIT_LIMIT
Z_BALANCE_FREQUENCY;
run;


proc fastclus data=cluster out=cluster cluster=cluster5 maxclusters=5 maxiter=100;
var
Z_ONEOFF_PURCHASES
Z_PURCHASES
Z_PAYMENTS
Z_CASH_ADVANCE_TRX
Z_CASH_ADVANCE
Z_CASH_ADVANCE_FREQUENCY
Z_BALANCE
Z_INSTALLMENTS_PURCHASES
Z_PURCHASES_TRX
Z_PRC_FULL_PAYMENT
Z_TENURE
Z_CREDIT_LIMIT
Z_BALANCE_FREQUENCY;
run;



proc fastclus data=cluster out=cluster cluster=cluster6 maxclusters=6 maxiter=100;
var
Z_ONEOFF_PURCHASES
Z_PURCHASES
Z_PAYMENTS
Z_CASH_ADVANCE_TRX
Z_CASH_ADVANCE
Z_CASH_ADVANCE_FREQUENCY
Z_BALANCE
Z_INSTALLMENTS_PURCHASES
Z_PURCHASES_TRX
Z_PRC_FULL_PAYMENT
Z_TENURE
Z_CREDIT_LIMIT
Z_BALANCE_FREQUENCY;
run;

proc fastclus data=cluster out=cluster cluster=cluster7 maxclusters=7 maxiter=100;
var
Z_ONEOFF_PURCHASES
Z_PURCHASES
Z_PAYMENTS
Z_CASH_ADVANCE_TRX
Z_CASH_ADVANCE
Z_CASH_ADVANCE_FREQUENCY
Z_BALANCE
Z_INSTALLMENTS_PURCHASES
Z_PURCHASES_TRX
Z_PRC_FULL_PAYMENT
Z_TENURE
Z_CREDIT_LIMIT
Z_BALANCE_FREQUENCY;
run;


/* cluster size check*/

proc freq data=cluster;
tables cluster3 cluster4 cluster5 cluster6 cluster7 ;
run;

/** Profiling **/

proc tabulate data=cluster;
var ONEOFF_PURCHASES
PURCHASES
PAYMENTS
CASH_ADVANCE_TRX
CASH_ADVANCE
CASH_ADVANCE_FREQUENCY
BALANCE
INSTALLMENTS_PURCHASES
PURCHASES_TRX
PRC_FULL_PAYMENT
TENURE
CREDIT_LIMIT
BALANCE_FREQUENCY;

class cluster3 cluster4 cluster5 cluster6 cluster7;

table (ONEOFF_PURCHASES
PURCHASES
PAYMENTS
CASH_ADVANCE_TRX
CASH_ADVANCE
CASH_ADVANCE_FREQUENCY
BALANCE
INSTALLMENTS_PURCHASES
PURCHASES_TRX
PRC_FULL_PAYMENT
TENURE
CREDIT_LIMIT
BALANCE_FREQUENCY)*mean, cluster3 cluster4 cluster5 cluster6 cluster7 ALL;
run;


/**************************END*********************************/