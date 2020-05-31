*Final project for EPI 740;
*Yijin Xiang;

**************
*read in dataset and check the variable name;
**************;
libname yijin "H:\3rd semester\EPI740\FINAL";
proc contents data=yijin.frmgham2;
run;
**************

**************
*Part A;
**************;
**1);
Data final(keep = prevhyp death sex bmi age educ cursmoke timedth period diabetes);
set yijin.frmgham2;
if PERIOD=1;
run;
*check the log, the new dataset has 4434 obs;
**2);
proc means data=final n mean std  max min;
var bmi age;
class prevhyp;
run;
proc freq data=final;
table (death sex educ cursmoke)*prevhyp/nocol norow;
run;
**************

**************
*Part B;
**************;
**1);
****prevhvp***;
*graphical;
Proc lifetest data=final method=km plots=lls;
time timedth * death(0);
strata prevhyp;
run;
*lines are parallel, no violation of PH assumption;
*GOF;
proc phreg data=final;
model timedth * death(0)= prevhyp;
output out=residual ressch=sh_prevhyp;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_prevhyp;
run;
*P-value=0.0009. We can reject the null hypothesis and the PH assumption didn't meet; 

*time-dependent;
proc phreg data=final;
model timedth * death(0)= prevhyp prevhyp_t;
prevhyp_t=prevhyp * timedth;
run;
proc phreg data=final;
model timedth * death(0)= prevhyp prevhyp_logt;
prevhyp_logt=prevhyp * log(timedth);
run;
*P-value for both test smaller than 0.05. Ph assumption is violated;
*decision;
*prevalent hypetersion--PH assumption violated;

****cursmoke***;
*graphical;
Proc lifetest data=final method=km plots=lls;
time timedth * death(0);
strata cursmoke;
run;
*lines are quite parallel, no violation of PH assumption;
*GOF;
proc phreg data=final;
model timedth * death(0)= cursmoke;
output out=residual ressch=sh_cursmoke;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_cursmoke;
run;
*P-value=0.4754. We fail to reject the null hypothesis and the PH assumption meet; 

*time-dependent;
proc phreg data=final;
model timedth * death(0)= cursmoke cursmoke_t;
cursmoke_t=cursmoke * timedth;
run;
proc phreg data=final;
model timedth * death(0)= cursmoke cursmoke_logt;
cursmoke_logt=cursmoke * log(timedth);
run;
*P-value for both test are greater than 0.05. Ph assumption is not violated;
*decision;
*current smoking status--PH assumption not violated;

*******************bmi&age;
*categorize the bmi and age variable;
proc univariate data=final ;
   var bmi age;
   output out=percentiles pctlpts=33.3 50 66.6 pctlpre=bmi_ age_ 
   pctlname=P33 P50 P66;
run;
proc print data=percentiles;
run;
*bmi: 23.95 25.45 27.06 
*age: 45  49  54;

data final_1;
set final;
bmi2=0;
bmi3=0;
age2=0;
age3=0;
*categorizing bmi;
if  bmi<=23.95 then bmi3=1;
else if  bmi<=27.06 then bmi3=2;
else bmi3=3;
if  bmi<=25.45 then bmi2=1;
else bmi2=2;
if bmi=. then do bmi2=.;bmi3=.;end;
*categorizing age;
if  age<=45 then age3=1;
else if  age<=54 then age3=2;
else age3=3;
if  age<=49 then age2=1;
else age2=2;
if age=. then do age2=.;age3=.;end;
run;

proc freq data=final_1;
table bmi3 bmi2 age3 age2;
run;


****bmi2***;
*graphical;
Proc lifetest data=final_1 method=km plots=lls;
time timedth * death(0);
strata bmi2;
run;
*lines are not parallel, indicating violation of PH assumption;
*GOF;
proc phreg data=final;
model timedth * death(0)= bmi;
output out=residual ressch=sh_bmi;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_bmi;
run;
*P-value=0.1035. We fail to reject the null hypothesis and the PH assumption meet; 

*time-dependent;
proc phreg data=final_1;
model timedth * death(0)= bmi bmi_t;
bmi_t=bmi * timedth;
run;
proc phreg data=final_1;
model timedth * death(0)= bmi bmi_logt;
bmi_logt=bmi * log(timedth);
run;
*P-value for t* bmi term is 0.0752 are greater than 0.05. Ph assumption is not violated;
*P-value for log(t)* bmi term is 0.0780 are greater  than 0.05. Ph assumption is not  violated;

****bmi3***;
*graphical;
Proc lifetest data=final_1 method=km plots=lls;
time timedth * death(0);
strata bmi3;
run;
*lines are not parallel, indicating violation of PH assumption;
*decision;
*BMI --PH assumption not violated;

****age2***;
*graphical;
Proc lifetest data=final_1 method=km plots=lls;
time timedth * death(0);
strata age2;
run;
*lines are  parallel, indicating no violation of PH assumption;
*GOF;
proc phreg data=final_1;
model timedth * death(0)= age;
output out=residual ressch=sh_age;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_age;
run;
*P-value=0.2149. We fail to reject the null hypothesis and the PH assumption meet; 

*time-dependent;
proc phreg data=final;
model timedth * death(0)= age age_t;
age_t=age * timedth;
run;
proc phreg data=final;
model timedth * death(0)= age age_logt;
age_logt=age * log(timedth);
run;
*P-value for t* age term is 0.2586 are greater than 0.05. Ph assumption is not violated;
*P-value for log(t)* age term is 0.9523 are smaller than 0.05. Ph assumption is violated;

****age3***;
*graphical;
Proc lifetest data=final_1 method=km plots=lls;
time timedth * death(0);
strata age3;
run;
*lines are parallel, indicating no violation of PH assumption;
*decision;
*AGE --PH assumption not violated;

****sex***;
*graphical;
Proc lifetest data=final method=km plots=lls;
time timedth * death(0);
strata sex;
run;
*lines are not parallel, indicating violation of PH assumption;
*GOF;
proc phreg data=final;
model timedth * death(0)= sex;
output out=residual ressch=sh_sex;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_sex;
run;
*P-value=0.3117. We fail to reject the null hypothesis and the PH assumption meet; 

*time-dependent;
proc phreg data=final;
model timedth * death(0)= sex sex_t;
sex_t=sex * timedth;
run;
proc phreg data=final;
model timedth * death(0)= sex sex_logt;
sex_logt=sex * log(timedth);
run;
*P-value for t* educ term is 0.3883 are greater than 0.05. Ph assumption is not violated;
*P-value for log(t)* educ term is 0.4493 are greater than 0.05. Ph assumption is violated;
*decision;
*current smoking status--PH assumption not violated;



****education***;
*graphical;
Proc lifetest data=final method=km plots=lls;
time timedth * death(0);
strata educ;
run;
*lines are not parallel, indicating violation of PH assumption;
*GOF;
proc phreg data=final;
model timedth * death(0)= educ;
output out=residual ressch=sh_educ;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_educ;
run;
*P-value=0.6895. We fail to reject the null hypothesis and the PH assumption meet; 

*time-dependent;
proc phreg data=final;
model timedth * death(0)= educ educ_t;
educ_t=educ * timedth;
run;
proc phreg data=final;
model timedth * death(0)= educ educ_logt;
educ_logt=educ * log(timedth);
run;
*P-value for t* educ term is 0.8221 are greater than 0.05. Ph assumption is not violated;
*P-value for log(t)* educ term is 0.8995 are greater than 0.05. Ph assumption is not violated;
*decision;
*Education--PH assumption not violated;





******2);
proc phreg data=final_1;
model timedth * death(0)=prevhyp age bmi sex cursmoke educ;
output out=residual ressch=sh_prevhyp sh_age sh_bmi sh_sex sh_cursmoke sh_educ;
run;

data failure;
     set residual;
	 where death=1;
run;

proc rank data=failure out=rank ties=mean;
     var timedth;
	 ranks timerank;
run;
proc corr data=rank nosimple;
     with timerank;
	 var sh_prevhyp;
run;
**************

**************
*Part C;
**************;
**1);
proc lifetest data=final;
  time timedth * death(0);
  strata prevhyp;
run;
**************

**************
*Part D;
**************;
**2);
*categorize the bmi and age variable;
proc means data=final p25 mean max;
var timedth;
run;

proc phreg data=final;
    model timedth * death(0)=prevhyp bmi age sex educ cursmoke prevhyp_t;
    prevhyp_t=prevhyp*timedth;
	contrast 'HR-first quartile of follow-up time' prevhyp 1 prevhyp_t 6974/estimate=exp;
	contrast 'HR-mean follow-up time' prevhyp 1 prevhyp_t 7506/estimate=exp;
	contrast 'HR-max follow-up time' prevhyp 1 prevhyp_t 8766/estimate=exp;
run;

**4);
*unconditional logistic regression;
proc logistic data=final;
model death(event='1')=prevhyp bmi age sex educ cursmoke;
contrast 'OR'  prevhyp 1 /estimate=exp;
run;
*log binominal regression;
proc genmod data=final;
model death(event='1')=prevhyp bmi age sex educ cursmoke/dist=bin link=log;
estimate 'PR'  prevhyp 1 /exp;
run;
proc freq data=final;
table death;
run;

**************
*Part E;
**************;
*2);
*Full model;
proc phreg data=final;
    model timedth * death(0)=diabetes bmi age sex educ cursmoke  diabetes*age;
run;
*Reduced model;
proc phreg data=final;
    model timedth * death(0)=diabetes bmi age sex educ cursmoke;
run;

*p-value;
data pvalue;
p=1-probchi(6.36,1);
proc print;
run;

*4);
*Full model;
proc phreg data=final;
    model timedth * death(0)=diabetes bmi age sex educ cursmoke  diabetes*age;
    contrast 'HR-Age for 50' diabetes 1 diabetes*age 50/estimate=exp;
	contrast 'HR-Age for 60' diabetes 1 diabetes*age 60/estimate=exp;
	contrast 'HR-Age for 70' diabetes 1 diabetes*age 70/estimate=exp;
run;
*Reduced model;
proc phreg data=final;
    model timedth * death(0)=diabetes bmi age sex educ diabetes*age;
    contrast 'HR-Age for 50' diabetes 1 diabetes*age 50/estimate=exp;
	contrast 'HR-Age for 60' diabetes 1 diabetes*age 60/estimate=exp;
	contrast 'HR-Age for 70' diabetes 1 diabetes*age 70/estimate=exp;
run;
**************

**************
*Extra Credit;
**************;
*1);
proc means data=final median;
var bmi age;
run;
*median for bmi: 25.45:
*median for age: 49;
data final_2;
set final;
bmi2=0;
age2=0;
if  bmi<25.45 then bmi2=0;
else bmi2=1;
if bmi=. then bmi2=.;
*categorizing age;
if  age<49 then age2=0;
else age2=1;
if age=. then age2=.;
run;
proc freq data=final_2;
table age2 bmi2;
run;
proc contents data=final_2;
run;
*aaggregate the dataset;
proc means data=final_2 sum maxdec=0;
var death timedth;
class diabetes bmi2 age2 sex educ cursmoke;
output out=final_aggregate sum(death timedth)= death_agg timedth_agg;
run;
proc print data=final_aggregate;
run;

data final_nomissing;
set final_aggregate;
where _type_=63;
ln_timedth_agg=log(timedth_agg);
proc print;
run;

*3);
proc genmod data=final_nomissing;
 model death_agg = diabetes bmi2 age2 sex educ cursmoke diabetes*age2/link=log dist=poisson offset=ln_timedth_agg;
 estimate "RR aged at or above the median" diabetes 1 diabetes*age2 1;
 estimate "RR aged below the median" diabetes 1;
run;

*4);
proc genmod data=final_nomissing;
 model death_agg = diabetes bmi2 age2 sex educ cursmoke diabetes*age2/link=log dist=poisson offset=ln_timedth_agg dscale;
 estimate "RR aged at or above the median" diabetes 1 diabetes*age2 1;
 estimate "RR aged below the median" diabetes 1;
run;

*5);
proc phreg data=final_2;
    model timedth * death(0)=diabetes bmi age2 sex educ cursmoke diabetes*age2;
    contrast 'HR-age above median' diabetes 1 diabetes*age2 1/estimate=exp;
    contrast 'HR-age below median' diabetes 1 /estimate=exp;
run;
