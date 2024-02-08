*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/9/2020
*	-------
*	1st Online Survey Tables
* 	Appendix Table B9: Sample Summary Statistics
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off
	
********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	
	
* ------------------------------------------------------------------------------
* 	1st National Survey - Table B9: Sample Summary Statistics 
* ------------------------------------------------------------------------------
	
	use "${datadir}/clean_data/03_1st_online_svy_clean.dta", clear 
	
	local sumvars age college_deg children employed_now employed_wife employed_out_wife
			
	local age_label "Age"
	local children_label "Number of Children"
	local college_deg_label "College Degree (\%)"
	local employed_now_label "Employed (\%)"
	local employed_wife_label "Wife Employed (\%)"
	local employed_out_wife_label "Wife Employed Outside of Home (\%)"
	
	
	foreach var of varlist `sumvars'{
		//stats by treatment and control
		forval i=0/1{
			su `var' if condition==`i'
			if inlist(`var', age, children){
				local `var'_`i': di %3.2f `r(mean)'
				local `var'_`i'_sd : di %3.2f `r(sd)'
			}
			else{
				local `var'_`i': di %3.2f `r(mean)'*100
				local `var'_`i'_sd : di %3.2f `r(sd)'*100
			}
		}

		//full sample
		su `var'
		if inlist(`var', age, children){
			local `var'_all: di %3.2f `r(mean)'
			local `var'_all_sd : di %3.2f `r(sd)'
		}
		else{
			local `var'_all: di %3.2f `r(mean)'*100
			local `var'_all_sd: di %3.2f `r(sd)'*100
		}
	}	
	
	//number of obs.
	forval i=0/1{
		count if condition==`i'
		local count_`i' `r(N)'
	}

	local count_all = _N
	
	file open table using "${outdir}/appendix_table9.tex", write replace

	local fwt "file write table"
	
	local cols 3
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"
	
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& All & Control & Treatment \\" _n
	`fwt' "\midrule" _n

	`fwt' "N  & `count_all' & `count_0' & `count_1' \\" _n
	
	`fwt' "\midrule" _n

	foreach var of varlist age children{
		`fwt' "``var'_label' & ``var'_all' & ``var'_0' & ``var'_1' \\" _n
		`fwt' " & (``var'_all_sd') & (``var'_0_sd') & (``var'_1_sd') \\" _n
	}
	
	foreach var of varlist college_deg employed_now employed_wife employed_out_wife{
		`fwt' "``var'_label' & ``var'_all' & ``var'_0' & ``var'_1' \\" _n
	}

	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n

	file close table
