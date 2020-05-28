*************************************************Part A***************************************************;
*libname project1 'F:\BIOS532';
PROC IMPORT OUT= insgluc
            DATAFILE= "F:\BIOS532\insgluc" 
            DBMS=EXCEL REPLACE;
     range="insgluc$A1:D"; 
	GETNAMES=YES;
     MIXED=NO;
     SCANTEXT=YES;
     USEDATE=YES;
     SCANTIME=YES;
RUN;

proc sort data = insgluc;
	by id time;
run;

*Not used: if there is missing value among ins or gluc, connect the non-missing value point and calculate that AUC
as estimated for the missing value(s);
/*data auc_insgluc1;*/
/*	set insgluc;*/
/*	by id;*/
/*	if ins ~= . then do;*/
/*		preins = lag(ins);*/
/*		pretime = lag(time);*/
/*		end;*/
/*	if first.id = 1 then do;*/
/*		preins = .;*/
/*		pregluc = .;*/
/*		pretime = .;*/
/*		end;*/
/*	aucins = (time - pretime) * (ins + preins)/2;*/
/*	putlog id first.id preins pregluc pretime aucins aucgluc;*/
/*run;*/
/**/
/*data auc_insgluc2;*/
/*	set auc_insgluc1;*/
/*	by id;*/
/*	if gluc ~= . then do;*/
/*		pregluc = lag(gluc);*/
/*		pretime = lag(time);*/
/*		end;*/
/*	if first.id = 1 then do;*/
/*		preins = .;*/
/*		pregluc = .;*/
/*		pretime = .;*/
/*		end;*/
/*	aucgluc = (time - pretime) * (gluc + pregluc)/2;*/
/*	putlog id first.id preins pregluc pretime aucins aucgluc;*/
/*	keep id aucins aucgluc;*/
/*run;*/


* if there is missing value among ins(or gluc), then set that patients ins(or gluc) auc missing value;
data auc_insgluc1;
	set insgluc;
	by id;
	preins = lag(ins);
	pregluc = lag(gluc);
	pretime = lag(time);
	if first.id = 1 then do;
		preins = .;
		pregluc = .;
		pretime = .;
		end;
	aucins = (time - pretime) * (ins + preins)/2;
	aucgluc = (time - pretime) * (gluc + pregluc)/2;
	putlog id first.id preins pregluc pretime aucins aucgluc;
run;

proc sql;
	create table auc_insgluc_final
	as 
		select id,
				sum(aucins) as aucins_sum,
			   sum(aucgluc) as aucgluc_sum,
			   nmiss(aucins) as nmiss_aucins,
			   nmiss(aucgluc) as nmiss_aucgluc
		from auc_insgluc2
		group by id
		;
quit;

data auc_insgluc_final1;
	set auc_insgluc_final;
	if nmiss_aucins > 1 then aucins_sum = .;
	if nmiss_aucgluc > 1 then aucgluc_sum = .;
	keep id aucins_sum aucgluc_sum;
run;

proc print data = auc_insgluc_final1;
title AUC table for ins and gluc;
run;






************************************************Part B******************************************************;
options mprint symbolgen;
/*%let function = constant('e')**(-x);*/
/*%let c = 0;*/
/*%let d = 1;*/
/*%let n = 2;*/

*****************************************************************
**** this macro accept four variable: c, d, n, f
**** c --- the start point of the interval
**** d --- the end point of the interval
**** n --- # number of small intervals that [c, d] will be splitted into
**** f --- the function need to be integrated, f must be in the form of x and special constant(e.g. pi\e)
****    must be represented in the form of SAS constant function
****    eg: constant('e') ** (-x)
**** all variable don't have default value, use correct order if implict assignment is used 
;
%macro simpson_auc(c, d, n, f);
data work.simpson_auc;
	*interval is the length of interval after break up [c, d];
	interval = (&d - &c)/&n;
	putlog interval;
	*use %eval to use shortcut to assign array variable name;
	*eval(&n+1) is the number of point when interval[c,d] was splitted into n small intervals;
	array points[%eval(&n+1)] point1-point%eval(&n+1);
	*initialize all the point;
	do i = 1 to %eval(&n+1);
		points[i] = &c + (i - 1) * interval;
		putlog  'points[i]: ' points[i]; 
	end;
	*use simplon formula to calculate the value of each interval;
	*there are totally &n interval;
	*function inputted as a variable is used directly, and must in the form of f(x);
	putlog '-------------------------';
	do i=1 to &n;
		putlog '***************************';
		b = points[i+1];
		a = points[i];
		ab_mean = (a+b)/2;
		putlog b a ab_mean;
		x = a;
		first_term = &f;
		putlog 'x: ' x 'first_term: ' first_term;
		x = ab_mean;
		second_term = &f;
		putlog 'x: ' x 'second_term: ' second_term;
		x = b;
		third_term = &f;
		putlog 'x: ' x 'third_term: ' third_term;
		auc + (interval/6)*(
						 first_term +
						 4*second_term +
						 third_term
						 );
		*output auc of each interval;
		output;
	end;
	keep auc;
run;
*only print the summation of auc;
proc print data = simpson_auc (firstobs = &n );
	title "AUC for function &f";
run;
%mend simpson_auc;

%simpson_auc(0, 1, 100, constant('e')**(-x));
%simpson_auc(1, 3, 100, x**2);
%simpson_auc(1, 3, 100, x**4 + x**3 + x**2);






*************************************************Part C**************************************************;
data partc;
	input group x n proportion;
	cards;
	1 32 87 0.368
	2 43 108 0.398
	3 16 80 0.200
	4 9 25 0.360
	;
run;

* p_prime transformation + radians to degrees transformation;
data work.partc_1;
	set partc;
	p_prime = 57.3*1/2*(arsin(sqrt(x/(n + 1))) + 
				   arsin(sqrt((x + 1)/(n + 1)))
				   );
run;

* make all needed data into one line;
data work.oneline;
	array denominator[4] n1-n4;
	array numerator[4] x1-x4;
	array p_prime_array[4] pprime1-pprime4;
	ngroups = 4;
	do i=1 to ngroups;
		set partc_1;
		denominator[i] = n;
		numerator[i] = x;
		p_prime_array[i] = p_prime;
	end;
	keep ngroups x1-x4 n1-n4 pprime1-pprime4;
run;

* calculate difference, test statistic q, based on critical value make conclusion;
data tukey_proportion;
	set oneline;
	array denominator[4] n1-n4;
	array numerator[4] x1-x4;
	array p_prime_array[4] pprime1-pprime4;
	do i=1 to ngroups-1;
      do j=i+1 to ngroups;
	  	comparison = put(i, 2.) || 'vs.' || put(j, 2.);
		diff = p_prime_array[i] - p_prime_array[j];
		se = sqrt(410.35/(denominator[i] + 0.5) +
				  410.35/(denominator[j] + 0.5));
		q = diff/se;
		critical_value_q = probmc('range',.,0.95,.,ngroups);
		if q < critical_value_q then
			conclusion = 'Do not reject H0: ' || 'p' || i || '=' || 'p' || j;
		else conclusion = 'Reject H0: '|| 'p' || i || '=' || 'p' || j;
		output;
	  end; * end j loop;
	end; *end i loop;
	keep comparison diff se q critical_value_q conclusion;
	label comparison = 'Comparison'
		  diff = "Difference
				  P'B - P'A"
		  se = 'SE'
		  critical_value_q = "q0.05,infinite,4"
		  conclusion = 'Conclusion'
	;
run;

proc print data = tukey_proportion label;
run;
	
