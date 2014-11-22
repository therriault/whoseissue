log close _all
graph drop _all
set seed 092981

#delimit ;
clear;
set more off;

log using 01_cces08_prep.log, replace;

************************************************************************;
*  File-Name:	01_cces08_prep.do    							       *;
*  Date:       	November 22, 2014								       *;
*  Author:     	Andrew Therriault			      	    			   *;
*  Purpose:    	Final data prep for BJPS article				       *;
*  Data In:     2008_cces_nyu.dta	     	  					       *;
*				cces_2008_recodes.csv							       *;
*  Data Out:    cces_2008_prepped.dta							       *;
*  Log File:    01_cces08_prep.log								       *;
*  Status:		Submission			                        	       *;
*  Machine:     AMT-ThinkPad2									       *;
************************************************************************;


************************************************************************;
*								     								   *;
*	Merging in recoded variables				       				   *;
*																       *;
************************************************************************;

insheet using cces_2008_recodes.csv;
save cces_2008_recodes.dta, replace;

#delimit cr;

local oldvars v206 v207 v208 v209 v211 v213 v214 ///
	cc333 v215 v219 v246 v243 cc307a v628 ///
	v622 v637 v640 v669 v660 v675 ucb444 ucb445 ucb446 ucb447 ///
	ucb448 ucb449 ucb450 ucb451 ucb452 ///
	cc410 cc411 cc412

local newvars dem_region_* dem_age_* dem_female dem_retired ///
	dem_race* dem_educ* dem_married dem_homeowner ///
	dem_bornagain dem_religion* dem_income* ///
	beh_ideology beh_pid* ///
	con_openseat_house con_incumbentdem_house ///
	con_incumbentrep_house con_unopposed_house ///
	con_openseat_senate con_incumbentdem_senate ///
	con_incumbentrep_senate ///
	eval_health eval_socsec eval_iraq eval_terror eval_econ ///
	eval_taxes eval_educ eval_energy eval_immig ///
	beh_vote2p_mccain beh_vote2p_senaterep beh_vote2p_houserep
	

	while "`oldvars'" ~="" {
		
		gettoken o oldvars : oldvars

		gettoken n newvars : newvars

		use cces_2008_recodes.dta

		keep `o' `n'

		duplicates drop `o', force

		save temp\__`o'.dta, replace

		clear

		}

use 2008_cces_nyu.dta

local oldvars v206 v207 v208 v209 v211 v213 v214 ///
	cc333 v215 v219 v246 v243 cc307a v628 ///
	v622 v637 v640 v669 v660 v675 ucb444 ucb445 ucb446 ucb447 ///
	ucb448 ucb449 ucb450 ucb451 ucb452  ///
	cc410 cc411 cc412

local newvars dem_region_* dem_age_* dem_female dem_retired ///
	dem_race* dem_educ* dem_married dem_homeowner ///
	dem_bornagain dem_religion* dem_income* ///
	beh_ideology beh_pid* ///
	con_openseat_house con_incumbentdem_house ///
	con_incumbentrep_house con_unopposed_house ///
	con_openseat_senate con_incumbentdem_senate ///
	con_incumbentrep_senate ///
	eval_health eval_socsec eval_iraq eval_terror eval_econ ///
	eval_taxes eval_educ eval_energy eval_immig  ///
	beh_vote2p_mccain beh_vote2p_senaterep beh_vote2p_houserep

	while "`oldvars'" ~="" {
		
		gettoken o oldvars : oldvars

		gettoken n newvars : newvars

		di "Matching `o' and `n'"

		merge m:1 `o' using temp\__`o'.dta, ///
			keep(master match) keepusing(`n')

		drop _merge

		}



#delimit ;

keep splitabc dem* beh*	con* eval*;


************************************************************************;
*																       *;
*	Dealing with missing data									       *;
*																       *;
************************************************************************;

#delimit cr

local demvars homeowner bornagain religion_catholic religion_none ///
	income_under40k income_over70k

foreach v of local demvars {

	bysort dem_region* dem_age* dem_female* beh_pid_3pt_3way: hotdeckvar dem_`v'
	
	drop dem_`v'
	
	rename dem_`v'_i dem_`v'

	}


#delimit ;

************************************************************************;
*																       *;
*	Generating sample weights for question wordings				       *;
*																       *;
************************************************************************;

mprobit splitabc dem* i.beh_pid_7pt;

predict word1-word3;

replace word1 = (1/word1)/3;
replace word2 = (1/word2)/3;
replace word3 = (1/word3)/3;

gen weight = .;
replace weight = word1 if splitabc == 1;
replace weight = word2 if splitabc == 2;
replace weight = word3 if splitabc == 3;


************************************************************************;
*																       *;
*	Generating binary eval vars and "unsures"					       *;
*																       *;
************************************************************************;

#delimit cr

local evars health socsec iraq terror econ taxes educ energy immig

	while "`evars'" ~="" {
		
		gettoken e evars : evars

		gen beval_`e' = eval_`e'

		replace beval_`e' = . if beval_`e' == 0

		replace beval_`e' = 0 if beval_`e' == -1
		
		gen unsure_`e' = (beval_`e' == .)

		}


#delimit ;


************************************************************************;
*																       *;
*	Fixing controls												       *;
*																       *;
************************************************************************;

replace con_incumbentdem_house = 0 if con_incumbentdem_house == .;
replace con_incumbentrep_house = 0 if con_incumbentrep_house == .;

************************************************************************;
*																       *;
*	Generating 3way votes + extremism							       *;
*																       *;
************************************************************************;

gen beh_vote_3way_pres = 1;
replace beh_vote_3way_pres = 0 if beh_vote2p_mccain == 0;
replace beh_vote_3way_pres = 2 if beh_vote2p_mccain == 1;

gen beh_vote_3way_senate = 1;
replace beh_vote_3way_senate = 0 if beh_vote2p_senaterep == 0;
replace beh_vote_3way_senate = 2 if beh_vote2p_senaterep == 1;
replace beh_vote_3way_senate = . if con_openseat_senate == .;

gen beh_vote_3way_house = 1;
replace beh_vote_3way_house = 0 if beh_vote2p_houserep == 0;
replace beh_vote_3way_house = 2 if beh_vote2p_houserep == 1;

gen beh_extreme = abs(3 - beh_ideology);


************************************************************************;
*																       *;
*	Generating defections										       *;
*																       *;
************************************************************************;

#delimit cr

local evars health socsec iraq terror econ taxes educ energy immig

	while "`evars'" ~="" {
		
		gettoken e evars : evars

		gen defect_`e' = .

		replace defect_`e' = 0 if beh_pid_3pt_dem == 1 & eval_`e' == -1
		replace defect_`e' = 1 if beh_pid_3pt_dem == 1 & eval_`e' == 0
		replace defect_`e' = 1 if beh_pid_3pt_dem == 1 & eval_`e' == 1

		replace defect_`e' = 0 if beh_pid_3pt_rep == 1 & eval_`e' == 1
		replace defect_`e' = 1 if beh_pid_3pt_rep == 1 & eval_`e' == 0
		replace defect_`e' = 1 if beh_pid_3pt_rep == 1 & eval_`e' == -1

		}


#delimit ;

************************************************************************;
*																       *;
*	Saving and closing											       *;
*																       *;
************************************************************************;

sum;

save cces_2008_prepped.dta, replace;
log close;






