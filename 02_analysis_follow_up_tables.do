*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/9/2020
*	-------
*	Follow-up Experiment Tables
*	Table 3: Effect of Treatment on Labor Supply Outcomes
* 	Appendix Table B1: Attrition
* 	Appendix Table B2: Attrition by Treatment 
* 	Appendix Table B3: Persistence of Beliefs Update
* 	Appendix Table B4: Effect of Treatment on Labor Supply Outcomes: Heterogeneity 
*					   by Wedge (No Controls) 
* 	Appendix Table B5: Effect of Treatment on Labor Supply Outcomes: Heterogeneity 
*					   by Wedge (Full Specification)
* 	Appendix Table B6: Effect of Belief Update on Labor Supply Outcomes 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
	set seed 5052018
	
	//permutations and bootstraps
	local permutations 1000
	local bootstraps 1000
	
* ------------------------------------------------------------------------------
*   Follow-up - Table 3: Effect of Treatment on Labor Supply Outcomes
* ------------------------------------------------------------------------------
	
	use "${datadir}/clean_data/02_follow_up_clean.dta", clear 
	
	local index employed_now_out_fl ///
				applied_out_fl ///
				interviewed_out_fl interview_sched_out_fl ///
				driving_fl outside_others_fl2
	
	//Kling, Liebman, Katz (2007) Index
	foreach var of varlist `index'{
		egen std_`var' = std(`var')
	}
	gen klk_index = std_employed_now_out_fl+std_applied_out_fl+std_interviewed_out_fl+ ///
		std_interview_sched_out_fl + std_driving_fl + std_outside_others_fl2
	replace klk_index = klk_index / 6
		
	local employed_now_out_fl_label "Currently Employed (\%)"
	local applied_out_fl_label "Applied to a Job (\%)"
	local interviewed_out_fl_label "Interviewed for a Job (\%)"
	local interview_sched_out_fl_label "Interview Scheduled (\%)"
	local driving_fl_label "Driving lessons sign-up (\%)"
	local outside_others_2_label "Baseline (\%)"
	local outside_others_fl2_label "Follow-up (\%)"
	local klk_index_label "Kling-Liebman-Katz Index"
	
	
*	outcomes : regression

	local indvars klk_index employed_now_out_fl applied_out_fl interviewed_out_fl driving_fl outside_others_fl2
	
	file open table using "${outdir}/table3.tex", write replace

	local fwt "file write table"
	local cols 6
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& K-L-K Index & Employed & Applied & Interviewed & Driving & Beliefs about \\" _n
	`fwt' "& & & & & Lessons & Neighbors \\ \midrule" _n
	`fwt' "\multicolumn{7}{@{}l}{\textbf{Panel A}: No controls} \\" _n
	
	file close table

	//Panel A
	local i = 0
	
	//corrected p-values from mthexp (enter results manually since the provided command does not seem to store the results)
	preserve
	keep if matched == 1
	mhtexp `index' if matched==1, treatment(condition2)
	restore
	
	matrix mht = (0, 0.484, 0.003, 0.05, 0.039, 0.000)
	
	foreach var in `indvars'{
		eststo col`++i': reg `var' condition2 if matched==1, robust
		local t = _b[condition2]/_se[condition2]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): reg `var' condition2 if matched==1, robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
		
		local p_mhtexp: di %4.3f mht[1,`i']
		if `i' > 1 estadd local p_mhtexp `p_mhtexp'
		if `i' == 1 estadd local p_mhtexp "--"
		
		leebounds `var' condition2
		mat b = e(b)
		mat V = e(V)
		estimates restore col`i'
		local lower: di %4.3f b[1,1]
		local upper: di %4.3f b[1,2]
		local lower_se: di %5.4f sqrt(V[1,1])
		local upper_se: di %5.4f sqrt(V[2,2])

		estadd local lower `lower'
		estadd local upper `upper'
		estadd local lower_se (`lower_se')
		estadd local upper_se (`upper_se')
	}
	
	esttab using "${outdir}/table3.tex", append se noobs booktabs nostar /// 
	fragment sfmt(a3) ///
	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"p_mhtexp \qquad \emph{p}-value: L-S-X MHT Correction" ///
	"lee \midrule Lee Attrition Bounds" "lower \qquad Lower Bound:" "lower_se \qquad" "upper \addlinespace \qquad Upper Bound:" "upper_se \qquad" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(condition2 _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/table3.tex" , write append
	`fwt' "\bottomrule" _n
	`fwt' "\noalign{\vskip 2mm} " _n
	`fwt' "\multicolumn{7}{@{}l}{\textbf{Panel B}: Session fixed effects, baseline beliefs and socioeconomic controls} \\" _n
	file close table
	
	//Panel B
	local i = 0
	foreach var in `indvars'{
		eststo col`++i': reg `var' condition2 i.session *_self *_others employed_wife employed_now i.education ///
			children num_know_per num_mfs_per age if matched==1, robust
		local t = _b[condition2]/_se[condition2]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): reg `var' condition2 i.session *_self *_others employed_wife employed_now i.education children num_know_per num_mfs_per age if matched==1, robust
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
	}
	
	esttab using "${outdir}/table3.tex", append se noobs booktabs nostar ///
	fragment drop(*session employed_* *education children age num_* *_self *_others) ///
	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(condition2 _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/table3.tex" , write append
	
	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
	file close table

********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	
	
* ------------------------------------------------------------------------------
* 	Follow-up - Appendix Table B1: Attrition
* ------------------------------------------------------------------------------
	
	local sumvars condition2 age college_deg children employed_now employed_wife ///
		num_know_per num_mfs_per signed_up_number
		
	local condition2_label "Treatment (\%)"	
	local age_label "Age"
	local children_label "Number of Children"
	local college_deg_label "College Degree (\%)"
	local employed_now_label "Employed (\%)"
	local employed_wife_label "Wife Employed (\%)"
	local num_know_per_label "Other Participants Known (\%)"
	local num_mfs_per_label "Other Participants with Mutual Friends (\%)"
	local signed_up_number_label "Job-Matching Service Sign-up (\%)"
	
	
	foreach var of varlist `sumvars'{
		//stats by matched and unmatched
		forval i=0/1{
			su `var' if matched == `i'
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
		local `var'_mean `r(mean)'
		if inlist(`var', age, children){
			local `var'_all: di %3.2f `r(mean)'
			local `var'_all_sd : di %3.2f `r(sd)'
		}
		else{
			local `var'_all: di %3.2f `r(mean)'*100
			local `var'_all_sd : di %3.2f `r(sd)'*100
		}

	}	
	
	//number of obs.
	forval i=0/1{
		count if matched==`i'
		local count_`i' `r(N)'
	}

	local count_all = _N
	
	file open table using "${outdir}/appendix_table1.tex", write replace

	local fwt "file write table"
	local cols 3
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"
	
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& All & Successful follow-up & No follow-up \\" _n
	`fwt' "\midrule" _n
	`fwt' "N  & `count_all' & `count_1' & `count_0' \\" _n
	`fwt' "\midrule" _n
	
	foreach var of varlist condition2{
		`fwt' "``var'_label' & ``var'_all' & ``var'_1' & ``var'_0' \\" _n
	}
	foreach var of varlist age children{
		`fwt' "``var'_label' & ``var'_all' & ``var'_1' & ``var'_0' \\" _n
		`fwt' " & (``var'_all_sd') & (``var'_1_sd') & (``var'_0_sd') \\" _n
	}
	foreach var of varlist college_deg employed_now employed_wife signed_up_number{
		`fwt' "``var'_label' & ``var'_all' & ``var'_1' & ``var'_0' \\" _n
	}

	foreach var of varlist num_know_per num_mfs_per{
		`fwt' "``var'_label' & ``var'_all' & ``var'_1' & ``var'_0' \\" _n
		`fwt' " & (``var'_all_sd') & (``var'_1_sd') & (``var'_0_sd') \\" _n
	}

	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
	file close table
	
* ------------------------------------------------------------------------------
* 	Follow-up - Appendix Table B2: Attrition by Treatment 
* ------------------------------------------------------------------------------
		
	gen matched2 = 2 * condition2 + matched
	
	
	foreach var of varlist `sumvars'{
		//stats by matched and unmatched
		forval i=0/3{
			su `var' if matched2 == `i'
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
		forval i = 0/1{
		su `var' if condition2 == `i'
		local `var'_`i'_mean `r(mean)'
		if inlist(`var', age, children){
			local `var'_`i'_all: di %3.2f `r(mean)'
			local `var'_`i'_all_sd : di %3.2f `r(sd)'
		}
		else{
			local `var'_`i'_all: di %3.2f `r(mean)'*100
			local `var'_`i'_all_sd : di %3.2f `r(sd)'*100
		}
		}

	}	
	
	//number of obs.
	forval i=0/3{
		count if matched2 == `i'
		local count_`i' `r(N)'
	}
	
	forval i=0/1{
		count if condition2 == `i'
		local count_`i'_all `r(N)'
	}
	
	file open table using "${outdir}/appendix_table2.tex", write replace

	local fwt "file write table"
	
	local cols 6
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"

	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& \multicolumn{3}{c}{Control} & \multicolumn{3}{c}{Treatment} \\ \cmidrule{2-4} \cmidrule{5-7}" _n
	`fwt' "& All & Follow-up & Dropped & All & Follow-up & Dropped \\" _n
	`fwt' "\midrule" _n
	`fwt' "N  & `count_0_all' & `count_1' & `count_0' & `count_1_all' & `count_3' & `count_2' \\" _n
	`fwt' "\midrule" _n
	
	foreach var of varlist age children{
		`fwt' "``var'_label' & ``var'_0_all' & ``var'_1' & ``var'_0' & ``var'_1_all' & ``var'_3' & ``var'_2' \\" _n
		`fwt' " & (``var'_0_all_sd') & (``var'_1_sd') & (``var'_0_sd') & (``var'_1_all_sd') & (``var'_3_sd') & (``var'_2_sd') \\" _n
	}
	foreach var of varlist college_deg employed_now employed_wife signed_up_number{
		`fwt' "``var'_label' & ``var'_0_all' & ``var'_1' & ``var'_0' & ``var'_1_all' & ``var'_3' & ``var'_2' \\" _n
	}

	foreach var of varlist num_know_per num_mfs_per{
		`fwt' "``var'_label' & ``var'_0_all' & ``var'_1' & ``var'_0' & ``var'_1_all' & ``var'_3' & ``var'_2' \\" _n
		`fwt' " & (``var'_0_all_sd') & (``var'_1_sd') & (``var'_0_sd') & (``var'_1_all_sd') & (``var'_3_sd') & (``var'_2_sd') \\" _n
	}

	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
	file close table
	
* ------------------------------------------------------------------------------
* 	Follow-up - Appendix Table B6: Effect of Belief Update on Labor Supply Outcomes 
* ------------------------------------------------------------------------------

	gen update = -outside_wedge if condition2 == 1
	replace update = 0 if condition2 == 0
	
	file open table using "${outdir}/appendix_table6.tex", write replace

	local fwt "file write table"
	
	
	local cols 6
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& K-L-K Index & Employed & Applied & Interviewed & Driving & Beliefs about \\" _n
	`fwt' "& & & & & Lessons & Neighbors \\ \midrule" _n
	`fwt' "\multicolumn{7}{@{}l}{\textbf{Panel A}: Baseline beliefs and confidence} \\" _n
	
	file close table

	//Panel A
	local i = 0
	
	foreach var in `indvars'{
		eststo col`++i': reg `var' update outside_self outside_others outside_confidence if matched==1, robust
		local t = _b[update]/_se[update]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (update), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute update conditionp=(abs(_b[update]/_se[update])), reps(`permutations'): reg `var' update outside_self outside_others outside_confidence if matched==1, robust
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
	}
	
	esttab using "${outdir}/appendix_table6.tex", append se noobs booktabs nostar ///
	drop(outside_self outside_others outside_confidence) ///
	fragment sfmt(a3) substitute (\_ _) ///
	nobaselevels coeflabels(update "Update (\$-\$Wedge\$*\mathds{1}_{Treatment}\$;\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(update _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/appendix_table6.tex", write append
	`fwt' "\bottomrule" _n
	`fwt' "\noalign{\vskip 2mm} " _n
	`fwt' "\multicolumn{7}{@{}l}{\textbf{Panel B}: Baseline beliefs, confidence, session fixed effects and demographic controls} \\" _n
	file close table
	
	//Panel B
	local i = 0
	foreach var in `indvars'{
		eststo col`++i': reg `var' update outside_self outside_others outside_confidence i.session employed_wife employed_now i.education children num_know_per num_mfs_per age if matched==1, robust
		local t = _b[update]/_se[update]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (update), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute update conditionp=(abs(_b[update]/_se[update])), reps(`permutations'): reg `var' update outside_self outside_others outside_confidence i.session employed_wife employed_now i.education children num_know_per num_mfs_per age if matched==1, robust
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
	}
	
	esttab using "${outdir}/appendix_table6.tex", append se noobs booktabs nostar ///
	fragment drop(*session employed_* *education children age num_* *_self *_others outside_confidence) ///
	nobaselevels coeflabels(update "Update (\$-\$Wedge\$*\mathds{1}_{Treatment}\$;\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"N \midrule \$N\$" "r2 \$R^2\$") substitute (\_ _) ///
	order(update _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/appendix_table6.tex", write append
	
	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
	file close table
	
* ------------------------------------------------------------------------------
* 	Follow-up - Appendix Table B4: Effect of Treatment on Labor Supply Outcomes: 
*								   Heterogeneity by Wedge (No Controls)
* ------------------------------------------------------------------------------	
	
	file open table using "${outdir}/appendix_table4.tex", write replace

	local fwt "file write table"
	
	local cols 6
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"
	
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& K-L-K Index & Employed & Applied & Interviewed & Driving & Beliefs about \\" _n
	`fwt' "& & & & & Lessons & Neighbors \\ \midrule" _n
	
	//Panel A
	`fwt' "\multicolumn{6}{@{}l}{\textbf{Panel A}: Wedge $\le 0$} \\" _n
	file close table
	
	//corrected p-values from mthexp (enter results manually since the provided command does not seem to store the results)
	preserve
	keep if matched == 1 & outside_wedge <= 0
	mhtexp `index', treatment(condition2)
	restore
	
	matrix mht = (0, 0.904, 0.006, 0.111, 0.117, 0.000)
	
	local i = 0
	foreach var in `indvars'{
		eststo col`++i': reg `var' condition2 if matched==1 & outside_wedge <= 0, robust
		local t = _b[condition2]/_se[condition2]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): reg `var' condition2 if matched==1 & outside_wedge<=0, robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
		
		local p_mhtexp: di %4.3f mht[1,`i']
		if `i' > 1 estadd local p_mhtexp `p_mhtexp'
		if `i' == 1 estadd local p_mhtexp "--"
		
		leebounds `var' condition2 if outside_wedge <= 0
		mat b = e(b)
		mat V = e(V)
		estimates restore col`i'
		local lower: di %4.3f b[1,1]
		local upper: di %4.3f b[1,2]
		local lower_se: di %5.4f sqrt(V[1,1])
		local upper_se: di %5.4f sqrt(V[2,2])

		estadd local lower `lower'
		estadd local upper `upper'
		estadd local lower_se (`lower_se')
		estadd local upper_se (`upper_se')
	}
	
	esttab using "${outdir}/appendix_table4.tex", append se noobs booktabs nostar ///
	fragment sfmt(a3) ///
	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"p_mhtexp \qquad \emph{p}-value: L-S-X MHT Correction" ///
	"lee \midrule Lee Attrition Bounds" "lower \qquad Lower Bound:" "lower_se \qquad" "upper \addlinespace \qquad Upper Bound:" "upper_se \qquad" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(condition2 _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/appendix_table4.tex" , write append
	`fwt' "\bottomrule" _n
	
	//Panel B
	`fwt' "\noalign{\vskip 2mm} " _n
	`fwt' "\multicolumn{6}{@{}l}{\textbf{Panel B}: Wedge $>0$} \\" _n
	file close table
	
	//corrected p-values from mthexp (enter results manually since the provided command does not seem to store the results)
	preserve
	keep if matched == 1 & outside_wedge > 0
	mhtexp `index', treatment(condition2)
	restore
	
	matrix mht = (0, 0.346, 0.323, 0.486, 0.436, 0.498)
	
	local i = 0
	foreach var in `indvars'{
		eststo col`++i': reg `var' condition2 if matched==1 & outside_wedge > 0, robust
		local t = _b[condition2]/_se[condition2]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): reg `var' condition2 if matched==1 & outside_wedge>0, robust 
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
		
		local p_mhtexp: di %4.3f mht[1,`i']
		if `i' > 1 estadd local p_mhtexp `p_mhtexp'
		if `i' == 1 estadd local p_mhtexp "--"
		
		leebounds `var' condition2 if outside_wedge > 0
		mat b = e(b)
		mat V = e(V)
		estimates restore col`i'
		local lower: di %4.3f b[1,1]
		local upper: di %4.3f b[1,2]
		local lower_se: di %5.4f sqrt(V[1,1])
		local upper_se: di %5.4f sqrt(V[2,2])

		estadd local lower `lower'
		estadd local upper `upper'
		estadd local lower_se (`lower_se')
		estadd local upper_se (`upper_se')
	}
	
	esttab using "${outdir}/appendix_table4.tex", append se noobs booktabs nostar ///
	fragment sfmt(a3) ///
	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"p_mhtexp \qquad \emph{p}-value: L-S-X MHT Correction" ///
	"lee \midrule Lee Attrition Bounds" "lower \qquad Lower Bound:" "lower_se \qquad" "upper \addlinespace \qquad Upper Bound:" "upper_se \qquad" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(condition2 _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/appendix_table4.tex" , write append
	
	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
	file close table
	
	
* ------------------------------------------------------------------------------
* 	Follow-up - Appendix Table B5: Effect of Treatment on Labor Supply Outcomes: 
*								   Heterogeneity by Wedge (Full Specification)
* ------------------------------------------------------------------------------	
	
	file open table using "${outdir}/appendix_table5.tex", write replace

	local fwt "file write table"
	
	local cols 6
	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
	local footer "\end{tabular}"
	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
	`fwt' "`header'" _n
	`fwt' "& K-L-K Index & Employed & Applied & Interviewed & Driving & Beliefs about \\" _n
	`fwt' "& & & & & Lessons & Neighbors \\ \midrule" _n
	
	//Panel A
	`fwt' "\multicolumn{6}{@{}l}{\textbf{Panel A}: Wedge $\le 0$ (Session fixed effects, baseline beliefs and socioeconomic controls)} \\" _n
	file close table
	
	local i = 0
	foreach var in `indvars'{
		eststo col`++i': reg `var' condition2 i.session *_self *_others employed_wife employed_now i.education ///
			children num_know_per num_mfs_per age if matched==1 & outside_wedge <= 0, robust
		local t = _b[condition2]/_se[condition2]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): reg `var' condition2 i.session *_self *_others employed_wife employed_now i.education children num_know_per num_mfs_per age if matched==1 & outside_wedge <= 0, robust
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
	}
	
	esttab using "${outdir}/appendix_table5.tex", append se noobs booktabs nostar ///
	fragment drop(*session employed_* *education children age num_* *_self *_others) ///
	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(condition2 _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/appendix_table5.tex", write append
	`fwt' "\bottomrule" _n
	
	//Panel B
	`fwt' "\noalign{\vskip 2mm} " _n
	`fwt' "\multicolumn{6}{@{}l}{\textbf{Panel B}: Wedge $>0$ (Session fixed effects, baseline beliefs and socioeconomic controls)} \\" _n
	file close table
	
	local i = 0
	foreach var in `indvars'{
		eststo col`++i': reg `var' condition2 i.session *_self *_others employed_wife employed_now i.education ///
			children num_know_per num_mfs_per age if matched==1 & outside_wedge > 0, robust
		local t = _b[condition2]/_se[condition2]
		local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
		estadd local p_robust `p_robust'

		qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
		local p_wboot: di %4.3f `r(p)'
		estimates restore col`i'
		estadd local p_wboot `p_wboot'
	
		qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): reg `var' condition2 i.session *_self *_others employed_wife employed_now i.education children num_know_per num_mfs_per age if matched==1 & outside_wedge > 0, robust
		mat p = r(p)
		local p_permute: di %4.3f p[1,1]
		estimates restore col`i'
		estadd local p_permute `p_permute'
	}
	
	esttab using "${outdir}/appendix_table5.tex", append se noobs booktabs nostar ///
	fragment drop(*session employed_* *education children age num_* *_self *_others) ///
	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" _cons "Constant") ///
	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
	"N \midrule \$N\$" "r2 \$R^2\$") ///
	order(condition2 _cons) nonumbers nomtitles
	
	eststo clear
	file open table using "${outdir}/appendix_table5.tex" , write append
	
	`fwt' "\bottomrule" _n
	`fwt' "`footer'" _n
	file close table
	
 * ------------------------------------------------------------------------------
 * 	Follow-up - Appendix Table B3: Persistence of Beliefs Update 
 * ------------------------------------------------------------------------------

 	file open table using "${outdir}/appendix_table3.tex", write replace

 	local fwt "file write table"

 	//drop all unmatched responses (!!)
 	keep if matched == 1

 	local cols 4
 	local header "\begin{tabular}{@{\extracolsep{0.1cm}}l*{`cols'}{c}} \toprule"
 	local footer "\end{tabular}"
 	`fwt' "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" _n
 	`fwt' "`header'" _n
	
 	gen condition2xbaseline = condition2 * outside_others
	
 	file close table
	
 	local spec1 "reg outside_others_fl condition2 i.session, robust"
 	local spec2 "reg outside_others_fl condition2 i.session *_self *_others, robust"
 	local spec3 "reg outside_others_fl condition2 i.session condition2xbaseline *_self *_others, robust"
 	local spec4 "reg outside_others_fl condition2 i.session condition2xbaseline *_self *_others employed_wife employed_now i.education children num_know_per num_mfs_per age, robust"

 	forval i=1/4{
 	eststo col`i': `spec`i''
 	local t = _b[condition2]/_se[condition2]
 	local p_robust: di %4.3f 2*ttail(e(df_r),abs(`t'))
 	estadd local p_robust `p_robust'
	
 	if `i' >= 3{
 	local t2 = _b[condition2xbaseline]/_se[condition2xbaseline]
 	local p2_robust: di %4.3f 2*ttail(e(df_r),abs(`t2'))
 	estadd local p2_robust `p2_robust'
 	}
	
 	qui boottest (condition2), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
 	local p_wboot: di %4.3f `r(p)'
 	estimates restore col`i'
 	estadd local p_wboot `p_wboot'
	
 	if `i' >= 3{
 	qui boottest (condition2xbaseline), boottype(wild) reps(`bootstraps') weighttype(webb) cluster(session) nograph
 	local p2_wboot: di %4.3f `r(p)'
 	estimates restore col`i'
 	estadd local p2_wboot `p2_wboot'
 	}
	
 	qui permute condition2 conditionp=(abs(_b[condition2]/_se[condition2])), reps(`permutations'): `spec`i''
 	mat p = r(p)
 	local p_permute: di %4.3f p[1,1]
 	estimates restore col`i'
 	estadd local p_permute `p_permute'
	
 	if `i' >= 3{
 	qui permute condition2xbaseline conditionp=(abs(_b[condition2xbaseline]/_se[condition2xbaseline])), reps(`permutations'): `spec`i''
 	mat p = r(p)
 	local p2_permute: di %4.3f p[1,1]
 	estimates restore col`i'
 	estadd local p2_permute `p2_permute'
 	}
	
 	if `i'==1{
 		estadd local fes "\checkmark"
 		estadd local baseline ""
 		estadd local controls ""
 	}
 	if `i'==2{
 		estadd local fes "\checkmark"
 		estadd local baseline "\checkmark"
 		estadd local controls ""
 	}
 	if `i'==3{
 		estadd local fes "\checkmark"
 		estadd local baseline "\checkmark"
 		estadd local controls ""
 	}
 	if `i'==4{
 		estadd local fes "\checkmark"
 		estadd local baseline "\checkmark"
 		estadd local controls "\checkmark"
 	}
 	}
	
 	esttab using "${outdir}/appendix_table3.tex", append se noobs booktabs nostar ///
 	fragment keep(condition2 condition2xbaseline _cons) ///
 	nobaselevels coeflabels(condition2 "Treatment (\$\beta\$)" condition2xbaseline "Treatment*Baseline Belief about Others (\$\gamma\$)" _cons "Constant") ///
 	scalars("inference Inference Robustness (\$\beta\$)" "p_robust \qquad \emph{p}-value: Robust S.E." ///
 	"p_wboot \qquad \emph{p}-value: Wild Bootstrap" "p_permute \qquad \emph{p}-value: Permutation Test" ///
 	"inference2 \midrule Inference Robustness (\$\gamma\$)" "p2_robust \qquad \emph{p}-value: Robust S.E." ///
 	"p2_wboot \qquad \emph{p}-value: Wild Bootstrap" "p2_permute \qquad \emph{p}-value: Permutation Test" ///
 	"fes \midrule Session F.E" "baseline Baseline beliefs" "controls Controls" "N \$N\$" "r2 \$R^2\$") ///
 	order(condition2 condition2xbaseline _cons) nomtitles
	
 	eststo clear
	
 	file open table using "${outdir}/appendix_table3.tex", write append
	
 	`fwt' "\bottomrule" _n
 	`fwt' "`footer'" _n

 	file close table
		
	
	
	
