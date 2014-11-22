log close _all
graph drop _all

#delimit ;
clear;
set more off;

log using 02_cces08_analysis.log, replace;

************************************************************************;
*  File-Name:	02_cces08_analysis.do  							       *;
*  Date:       	November 22, 2014								       *;
*  Author:     	Andrew Therriault			      	    			   *;
*  Purpose:    	Final analyses for BJPS article					       *;
*  Data In:     cces_2008_prepped.dta			     	  		       *;
*  Data Out:    None											       *;
*  Log File:    02_cces08_analysis.log							       *;
*  Status:		Submission			                        	       *;
*  Machine:     AMT-ThinkPad2									       *;
************************************************************************;

use cces_2008_prepped.dta;

************************************************************************;
*								  								       *;
*	Tabulating general results by wording and doing t-tests			   *;
*																       *;
************************************************************************;

table splitabc, contents(mean beval_health mean 
	beval_socsec mean beval_iraq mean beval_terror 
	mean beval_econ) format(%12.3g);

table splitabc, contents(mean beval_taxes mean 
	beval_educ mean beval_energy mean beval_immig) 
	format(%12.3g);

#delimit cr

local evars health socsec iraq terror econ taxes educ energy immig

	while "`evars'" ~="" {
		
		gettoken e evars : evars

		di "************************************************"
		di "************************************************"
		di "************** Issue `e' *************************"
		di "************************************************"
		di "************************************************"

		ttest beval_`e' if splitabc ~= 3, by(splitabc)
		ttest beval_`e' if splitabc ~= 2, by(splitabc)

		}


#delimit ;

gen i = _n;

reshape long beval, i(i) j(topic) string;

ttest beval if splitabc ~= 3, by(splitabc);
ttest beval if splitabc ~= 2, by(splitabc);




************************************************************************;
*								 								       *;
*	Tabulating defectors										       *;
*																       *;
************************************************************************;

clear;

use cces_2008_prepped.dta;

table splitabc, contents(mean defect_health mean 
	defect_socsec mean defect_iraq mean defect_terror 
	mean defect_econ) format(%12.3g);

table splitabc, contents(mean defect_taxes mean 
	defect_educ mean defect_energy mean defect_immig) 
	format(%12.3g);


#delimit cr

local evars health socsec iraq terror econ taxes educ energy immig

	while "`evars'" ~="" {
		
		gettoken e evars : evars

		di "************************************************"
		di "************************************************"
		di "************** Issue `e' *************************"
		di "************************************************"
		di "************************************************"

		ttest defect_`e' if splitabc ~= 3, by(splitabc)
		ttest defect_`e' if splitabc ~= 2, by(splitabc)


		}


#delimit ;

gen i = _n;

reshape long defect, i(i) j(topic) string;

ttest defect if splitabc ~= 3, by(splitabc);
ttest defect if splitabc ~= 2, by(splitabc);

************************************************************************;
*																       *;
*	Creating overall binary eval variables						       *;
*																       *;
************************************************************************;

clear;

use cces_2008_prepped.dta;

fvset base 3 beh_pid_7pt;
fvset base 1 beh_pid_3pt_3way;
fvset base 3 beh_ideology;
fvset base 1 splitabc;

keep beh* dem* con* beval* weight splitabc;

gen i = _n;

reshape long beval, i(i) j(topic) string;
encode topic, generate(issue);

fvset base 9 issue;

************************************************************************;
*																       *;
*	Rerunning overall binary tabulations to see sample sizes	       *;
*																       *;
************************************************************************;

table splitabc, contents(mean beval) format(%12.3g);

gen temp_b = (beval ~= .);
table splitabc, contents(sum temp_b) by(issue);
replace temp_b = (temp_b / 9);
table splitabc, contents(sum temp_b);
drop temp_b;

table beh_pid_3pt_3way, contents(mean beval) by(splitabc);

ttest beval if splitabc ~= 3, by(splitabc);
ttest beval if splitabc ~= 2, by(splitabc);

************************************************************************;
*																       *;
*	Running tests (all issues, binary, pooled treatments)	 	       *;
*																       *;
************************************************************************;	


***BASIC SEs***;

logit beval  i.splitabc c.beh_ideology c.beh_ideology#i.splitabc 
	i.beh_pid_3pt_3way i.beh_pid_3pt_3way#i.splitabc 
	i.issue i.issue#2.splitabc i.issue#3.splitabc;
	 
testparm i(1/8).issue;
testparm i(1/8).issue i(1/8).issue#2.splitabc;
testparm i(1/8).issue i(1/8).issue#3.splitabc;
	
epcp;	

***CLUSTERED SEs***;

logit beval  i.splitabc c.beh_ideology c.beh_ideology#i.splitabc 
	i.beh_pid_3pt_3way i.beh_pid_3pt_3way#i.splitabc 
	i.issue i.issue#2.splitabc i.issue#3.splitabc
	, vce(cluster i);
	 
testparm i(1/8).issue;
testparm i(1/8).issue i(1/8).issue#2.splitabc;
testparm i(1/8).issue i(1/8).issue#3.splitabc;
	
epcp;	

************************************************************************;
*																       *;
*	Running tests (all issues, binary, by treatment)			       *;
*																       *;
************************************************************************;

#delimit cr

local a 1

	while `a' <= 3 {

		di ""
		di ""
		di ""
		di "************************************************"
		di "************************************************"
		di "************** TREATMENT `a' *********************"
		di "************************************************"
		di "************************************************"
		di ""
		di "************************************************"
		di "************** BASIC SEs ***********************"
		di "************************************************"
		di ""
		di "************************************************"
		di "************** No FEs **************************"
		di "************************************************"

		logit beval beh_ideology i.beh_pid_3pt_3way ///
		if splitabc == `a'
		qui estimates store treatment`a'_nofixed
		
		di ""
		di "************************************************"
		di "************** With FEs **************************"
		di "************************************************"

		logit beval beh_ideology i.beh_pid_3pt_3way ///
		i.issue ///
		if splitabc == `a'
		qui estimates store treatment`a'_withfixed
		epcp

		lrtest treatment`a'_nofixed treatment`a'_withfixed, force 	
		
		testparm i(1/8).issue
	
		margins, at(beh_ideology = (1(1)5) beh_pid_3pt_3way = 1)
		margins, at(issue = (1(1)9) beh_ideology = 3 beh_pid_3pt_3way = 1)
		margins, at(beh_pid_3pt_3way = (0 1 2) beh_ideology = 3)
		
		di ""
		di "************************************************"
		di "************** CLUSTERED SEs *******************"
		di "************************************************"
		di ""
		di "************************************************"
		di "************** No FEs **************************"
		di "************************************************"

		logit beval beh_ideology i.beh_pid_3pt_3way ///
		if splitabc == `a' ///
		, vce(cluster i)
		qui estimates store treatment`a'_nofixed
		
		di ""
		di "************************************************"
		di "************** With FEs **************************"
		di "************************************************"

		logit beval beh_ideology i.beh_pid_3pt_3way ///
		i.issue ///
		if splitabc == `a' ///
		, vce(cluster i)
		qui estimates store treatment`a'_withfixed
		epcp

		lrtest treatment`a'_nofixed treatment`a'_withfixed, force 	
		
		testparm i(1/8).issue
	
		margins, at(beh_ideology = (1(1)5) beh_pid_3pt_3way = 1)
		margins, at(issue = (1(1)9) beh_ideology = 3 beh_pid_3pt_3way = 1)
		margins, at(beh_pid_3pt_3way = (0 1 2) beh_ideology = 3)

		local ++a

	}

#delimit ;

************************************************************************;
*																       *;
*	closing														       *;
*																       *;
************************************************************************;


log close;






