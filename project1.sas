************************************
*Yijin Xiang 2350801               * 
*project 1                         *
************************************
;

libname yijin"H:\2nd semester\Bios532\project1";

************************************************************************************************************************************
*question a;
*import xls data with insgluc spreadsheet;
PROC IMPORT OUT= WORK.insgluc 
            DATAFILE= "S:\course\Bios532\2019\insgluc.xls" 
            DBMS=EXCEL REPLACE;
     RANGE="insgluc"; 
     GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;
proc print;
run;
*sort data by id and time;
proc sort data=insgluc;
     by id time;
run;
proc print;
run;
***************************************************************************************************************************** 
*if there is missing value, ignore the point and connect last point with value and next point with value to calculate the auc;
data insauc;
     set insgluc;
	 if ins=. then delete;
	 by id;
     preins=lag(ins);
	 pretime=lag(time);
	 if first.id then do 
     preins=.;
	 pretime=.;
	 end;
	 *calculate auc for ins;
	 aucins=0.5*(time-pretime)*(preins+ins);
run;
data glucauc;
     set insgluc;
	 if gluc=. then delete;
	 by id;
     pregluc=lag(gluc);
	 pretime=lag(time);
	 if first.id then do 
     pregluc=.;
	 pretime=.;
	 end;
	 *calculate auc for gluc;
	 aucgluc=0.5*(gluc+pregluc)*(time-pretime);
run;
proc sort data=glucauc;
by id time;
run;
data insglucauc;
     merge insauc glucauc;
	 by id time;
run;

*use the proc means to get the sum of aucins and aucgluc for each var;
proc means data=insglucauc sum noprint;
var aucins aucgluc;
class id;
output out=finala(drop=_TYPE_ _FREQ_ ) sum=sumaucins sumaucgluc ;
run;
data finalauc;
     set finala;
     if id=. then delete;
run;
proc print;
run;
*******************************************************************************************************************************;
*if there is missing value, set the auc of variable among this subjects as missing;

data insglucauc;
     set insgluc;
	 by id;
	 preins=lag(ins);
	 pretime=lag(time);
	 pregluc=lag(gluc);
	 if first.id then do 
     preins=.;
     pregluc=.;
	 pretime=.;
	 cumaucins=0;
	 cumaucgluc=0;
	 end;
	 if time=0 then do
     preins=ins;
	 pregluc=gluc;
	 pretime=time;
	 end;
	 *calculate auc for ins;
	 aucins=0.5*(time-pretime)*(preins+ins);
     cumaucins=cumaucins+aucins;
	 *calculate auc for gluc;
	 aucgluc=0.5*(gluc+pregluc)*(time-pretime);
     cumaucgluc=cumaucgluc+aucgluc;
	 retain cumaucins cumaucgluc;
     if last.id;
	 keep id cumaucins cumaucgluc;
proc print;
run;
*************************************************************************************************************************;

********************************************************************************************************************************;
*question b;
*generalize the auc calculation based on this two function;
%macro auc_simpson(c,d,n,function);*c-lower limit, d-upper limit, n-# of interval,f-function;
%if &function=exponential %then
    %do;
         data work.auc;
           interval=(&d-&c)/(&n);
           upper=&d-interval;
                do i= &c to upper by interval;
                  auc=interval/6*(exp(-i)+4*exp(-0.5*(2*i+interval))+exp(-(i+interval)));
		          output;
                end;
         run;

	%end;
%if &function=quadratic %then
    %do;
        data work.auc;
             interval=(&d-&c)/(&n);
             upper=&d-interval;
                do i= &c to upper by interval;
                  auc=interval/6*(i**2+4*(0.5*(2*i+interval))**2+(i+interval)**2);
		          output;
                end;
        run;
	%end;
	proc print;
	run;
proc means data=auc noprint sum;
     var auc;
     output out=aucsum sum=totalauc;
run;
proc print data=aucsum;
run;
%mend auc_simpson;
%auc_simpson(c=0,d=1,n=100,function=exponential);
%auc_simpson(c=1,d=3,n=100,function=quadratic);
options mprint symbolgen;
*************************************************************************************************************************************;
******generalize for any function;
%macro auc_simpson1(c,d,n,function);*c-lower limit, d-upper limit, n-# of interval,f-function;

data work.auc;
      interval=(&d-&c)/(&n);
      upper=&d-interval;
      do i= &c to upper by interval;
	   *one represents first term in the auc calcualtion;
	  a=i+ 0.5*interval;
	  b=i+interval;
	   x=i;one=&function;
	   x=a; two=&function;
	   x=b; three=&function;
       auc=interval/6*(one+4*two+three);
	   output;
       end;
         run;
proc print;
run;
proc means data=auc noprint sum;
     var auc;
     output out=aucsum sum=totalauc;
run;
proc print data=aucsum;
run;
%mend auc_simpson1;
%auc_simpson1(c=1,d=3,n=100,function=x*x);
%auc_simpson1(c=0,d=1,n=100,function=EXP(-x));
options mprint symbolgen;

**************************************************************************************************************************;


*************************************************************************************************************************;
*question c;
*prepare the data for calculation;
data work.data;
     input group x n;
	 datalines;
	 1 32 87 
	 2 43 108
	 3 16 80
	 4 9 25
	 ;
run;
*get the number of group;
proc means data = data noprint;
output out = total
         n = ngroup;
proc print;
var ngroup;
run;


%macro turky(dsn,ngroup,a);*dsn for dataset name, ngroup is the number of groups, a is the alpha level we choose;

data calculate;
     set &dsn;
     proportion=x/n;
	 pi=constant("pi");
	 acsin1=ARSIN(sqrt(x/(n+1)));
	 acsin2=ARSIN(sqrt((x+1)/(n+1)));
	 transp=0.5*(acsin1+acsin2)*180/pi;
	 partialse=410.35/(n+0.5);*create a var will be used in calculating the standard deviation later;
	 drop acsin1 acsin2 pi proportion x n;
run;

data oneline;*set all the data into one line;
     array transparray(%eval(&ngroup)) transp1-transp%eval(&ngroup);
	 array partialsearray(%eval(&ngroup)) partialse1-partialse%eval(&ngroup);
	 do i=1 to %eval(&ngroup);
	 set calculate;
	    if group=i then do; 
		    transparray(i)=transp;
			partialsearray(i)=partialse;
		end;
	end;
		drop transp partialse group i;
run;

data final;
     set oneline;
     array transparray(%eval(&ngroup)) transp1-transp%eval(&ngroup);
	 array partialsearray(%eval(&ngroup)) partialse1-partialse%eval(&ngroup);
     level=1-&a;
     criticalq = probmc('range',.,level,.,&ngroup);
     do i= 1 to %eval(&ngroup-1);
	    do j= i+1 to %eval(&ngroup);
	      diff=transparray(i)-transparray(j);
	      SE=sqrt(partialsearray(i)+partialsearray(j));
	      q=abs(diff/se);
	      Comparison='group'||i||'- group'||j;
	      if q < criticalq then
			conclusion = 'Do not reject H0: ' || 'p' || i || '=' || 'p' || j;
		  else conclusion = 'Reject H0: '|| 'p' || i || '=' || 'p' || j;
       output;
	   end;
	end;
	label comparison="Comparison"
          criticalq = "q( &a,infinite,&ngroup) "
	      diff="Difference"
          q="q"
		  SE="Se"
		  conclusion="Conclusion";
	keep Comparison diff SE q criticalq conclusion;
 run;
proc print label;
var Comparison diff SE q criticalq conclusion;
run;

%mend turky;
%turky(data,4,0.05);

*************************************************************************************************************************;
