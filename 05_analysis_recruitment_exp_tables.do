*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/9/2020
*	-------
*	Recruitment Experiment Tables
* 	Table 6: Share of Women Choosing and Showing Up for Job Outside the Home 
* 	Appendix Table B11: Sample Summary Statistics 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
	set seed 960713
	local permutations 1000
	local bootstraps 1000
	
* ------------------------------------------------------------------------------
* 	Recruitment Experiment - Table 6: Share of Women Choosing and Showing Up for 
*									  Job Outside the Home 
* ------------------------------------------------------------------------------	
	
	use "${datadir}/clean_data/05_recruitment_exp_clean.dta", clear 
	
	file open table using "${outdir}/table6.tex", write replace
	
	local fwt "file write table"
	
	local cols 4
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}}\toprule"
	local footer "\end{tabular}"
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& Outside-Home & Outside-Home & Outside-Home & Outside-Home  \\" _n 
	`fwt' "& Sign-up & Sign-up & Show-up & Show-up \\" _n

	file close table
	
	local contlist age married num_child i.education employed_now 

	//Column 1
		eststo col1: reg outside_job_takeup treated, robust
		estadd local inference ""
		local t = _b[treated]/_se[treated]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		
		qui permute treated conditionp=(abs(_b[treated]/_se[treated])), reps(`permutations'): reg outside_job_takeup treated, robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col1
		estadd local p_permute `p_permute'
		
		estadd local controls ""

	//Column 2
		eststo col2: reg outside_job_takeup treated `contlist', robust
		estadd local inference ""
		local t = _b[treated]/_se[treated]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'
		
		
		qui permute treated conditionp=(abs(_b[treated]/_se[treated])), reps(`permutations'): reg outside_job_takeup treated `contlist', robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col2
		estadd local p_permute `p_permute'
		
		estadd local controls "\checkmark"

	//Column 3
		eststo col3: reg outside_job_takeupXshow_up treated, robust
		estadd local inference ""
		local t = _b[treated]/_se[treated]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui permute treated conditionp=(abs(_b[treated]/_se[treated])), reps(`permutations'): reg outside_job_takeupXshow_up treated, robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col3
		estadd local p_permute `p_permute'
		
		estadd local controls ""

	//Column 4
		eststo col4: reg outside_job_takeupXshow_up treated `contlist', robust
		estadd local inference ""
		local t = _b[treated]/_se[treated]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'
		
		qui permute treated conditionp=(abs(_b[treated]/_se[treated])), reps(`permutations'): reg outside_job_takeupXshow_up treated `contlist', robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col4
		estadd local p_permute `p_permute'
		
		estadd local controls "\checkmark"
		
		
		esttab using "${outdir}/table6.tex", append se noobs booktabs nostar ///
		fragment drop(age married num_child *education employed_now) ///
		nobaselevels coeflabels(treated "Treatment (\$\beta\$)" _cons "Constant") ///
		scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
		"p_permute \qquad \emph{p}-value: Permutation Test" ///
		"controls Controls" "N \$N\$" "r2 \$R^2\$") ///
		order(treated _cons) nomtitles 
		
		eststo clear
		
		file open table using "${outdir}/table6.tex" , write append

		`fwt' "\bottomrule" _n
		`fwt' "`footer'" _n
		
		file close table
		
********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	

* ------------------------------------------------------------------------------
* 	Recruitment Experiment - Table B11: Sample Summary Statistics 
* ------------------------------------------------------------------------------

	use "${datadir}/clean_data/05_recruitment_exp_clean.dta", clear 
	
	local sumvars age num_child married higher_edu employed_now 

	local age_label "Age"
	local married_label "Married (\%)"
	local num_child_label "Number of Children"
	local higher_edu_label "College Degree (\%)"
	local employed_now_label "Employed (\%)"

	local n_size = _N
	local contlist age married num_child i.education employed_now 
	
	* turn dummy to percentage 
	foreach v of varlist married higher_edu employed_now {
		replace `v' = `v' * 100
	}

	foreach var of varlist `sumvars' {
		//stats by treatment and control
		forval i = 0/1 {	
			sum `var' if treated == `i' 
			local `var'_`i': di %3.2f `r(mean)'
			local `var'_`i'_sd : di %3.2f `r(sd)'
		}
		
		//full sample
		su `var'

		local `var'_all: di %3.2f `r(mean)'
		local `var'_all_sd : di %3.2f `r(sd)'
		
		//number of obs.
		forval i=0/1{
			count if treated==`i'
			local count_`i' `r(N)'
		}
		
		local count_all = _N
		cap file close table 
		file open table using "${outdir}/appendix_table11.tex", write replace

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

		foreach var of varlist age num_child{
			`fwt' "``var'_label' & ``var'_all' & ``var'_0' & ``var'_1' \\" _n
			`fwt' " & (``var'_all_sd') & (``var'_0_sd') & (``var'_1_sd') \\" _n
		}
		
		foreach var of varlist married higher_edu employed_now {
			`fwt' "``var'_label' & ``var'_all' & ``var'_0' & ``var'_1' \\" _n
		}
		
		
		`fwt' "\bottomrule" _n
		`fwt' "`footer'" _n

	}
	
	file close table
	
