*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/9/2020
*	-------
*	2nd Online Survey Tables
* 	Appendix Table B10: Sample Summary Statistics
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	
	
* ------------------------------------------------------------------------------
* 	2nd National Survey - Table B10: Sample Summary Statistics 
* ------------------------------------------------------------------------------

	use "${datadir}/clean_data/04_2nd_online_svy_clean.dta", clear
	
	* recode dummies as percentage 
	foreach v of varlist higher_edu employed_now wife_work wife_work_outside {
		replace `v' = `v' * 100
	}

	gen random1 = belief1_fob != .

	local sumvars age num_child higher_edu employed_now wife_work wife_work_outside

	local age_label "Age"
	local higher_edu_label "College Degree (\%)"
	local employed_now_label "Employed (\%)"
	local wife_work_label "Wife Employed (\%)"
	local wife_work_outside_label "Wife Employed (\%) Outside the Home"
	local num_child_label "Number of Children"

	foreach var of varlist `sumvars' {
		//stats by randomizations 1 & 2 
			forval i = 0/1 {	
				sum `var' if random1 == `i' 
				local `var'_`i': di %3.2f `r(mean)'
				local `var'_`i'_sd : di %3.2f `r(sd)'
			}
		
		//full sample
		su `var'

		local `var'_all: di %3.2f `r(mean)'
		local `var'_all_sd : di %3.2f `r(sd)'
		
		//number of obs.
			forval i=0/1{
				count if random1==`i'
				local count_`i' `r(N)'
			}
		}
		
		local count_all = _N

		file open table using "${outdir}/appendix_table10.tex", write replace

		local fwt "file write table"
		
		local cols 3
		local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
		local footer "\end{tabular}"

		`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
		`fwt' "`header'" _n
		`fwt' "& All & Randomization & Randomization \\" _n
		`fwt' "&  & Arm 1 & Arm 2 \\" _n
		`fwt' "\midrule" _n

		`fwt' "N  & `count_all' & `count_0' & `count_1' \\" _n
		
		`fwt' "\midrule" _n

			foreach var of varlist age num_child {
				`fwt' "``var'_label' & ``var'_all' & ``var'_0' & ``var'_1' \\" _n
				`fwt' " & (``var'_all_sd') & (``var'_0_sd') & (``var'_1_sd') \\" _n
			}
			
			foreach var of varlist higher_edu employed_now wife_work wife_work_outside {
				`fwt' "``var'_label' & ``var'_all' & ``var'_0' & ``var'_1' \\" _n
			}
		
		
		`fwt' "\bottomrule" _n
		`fwt' "`footer'" _n
	
		
		file close table
