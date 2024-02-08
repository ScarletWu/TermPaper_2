*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/6/2020
*	-------
*	Follow-up Experiment Figures 
* 	Figure 4: Long-Term Labor Supply Outcomes
* 	Figure B3: Perceptions of Others’ Opinions Regarding the Filler Question 
*	Figure B4: Persistence in Beliefs Update 
* 	Figure B6: Long-Term Labor Supply Outcomes: Heterogeneity by Children 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
	use "${datadir}/clean_data/02_follow_up_clean.dta", clear 
	
* ------------------------------------------------------------------------------
* 	Follow-up - Figure 4: Long-Term Labor Supply Outcomes 
* ------------------------------------------------------------------------------

	//drop all unmatched responses (!!)
	keep if matched == 1
	
	//(for looping purposes)
	rename driving_fl driving_out_fl

	bys condition2: egen applied_rate = mean(applied_out_fl)
	bys condition2: egen interviewed_rate = mean(interviewed_out_fl)
	bys condition2: egen driving_rate = mean(driving_out_fl)
	bys condition2: egen employed_now_rate = mean(employed_now_out_fl)
	replace applied_rate = applied_rate*100
	replace interviewed_rate = interviewed_rate*100
	replace driving_rate = driving_rate*100
	replace employed_now_rate = employed_now_rate*100
	
	//y coord. for p-values
	local applied_p 22
	local interviewed_p 9
	local driving_p 82
	local employed_now_p 13.5

	foreach var in applied interviewed employed_now driving {
	gen `var'_rate_lo = .
	gen `var'_rate_hi = .
	
	forval i=0/1{
		su `var'_rate if condition2==`i'
		local `var'_rate_`i': di %4.2f `r(mean)'
		ci proportions `var'_out_fl if condition2==`i'
		replace `var'_rate_hi =`r(ub)'*100 if condition2 == `i'
		replace `var'_rate_lo =`r(lb)'*100 if condition2 == `i'
	}

	prtest `var'_out_fl, by(condition2)
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_`var': di %4.3f `p1'
	}
	
	* 4(a)
	twoway (bar applied_rate condition2, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7) base(0)) ///
		(rcap applied_rate_hi applied_rate_lo condition2, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%", margin(small)) ///
		xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) ///
		text(1 0.2 "`applied_rate_0'%", size(median)) text(1 1.2 "`applied_rate_1'%", size(median)) ///
		text(`applied_p' 0.5 "p-value = `p1_applied'", size(median)) ///
		ylabel(0(5)25,glcolor(gs15) glwidth(vthin)) ///
		name("applied", replace)
	
	* 4(b)
	twoway (bar interviewed_rate condition2, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7) base(0)) ///
		(rcap interviewed_rate_hi interviewed_rate_lo condition2, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%", margin(small)) ///
		xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) ///
		text(0.5 0.2 "`interviewed_rate_0'%", size(median)) text(0.5 1.2 "`interviewed_rate_1'%", size(median)) ///
		text(`interviewed_p' 0.5 "p-value = `p1_interviewed'", size(med)) ///
		ylabel(0(2)10,glcolor(gs15) glwidth(vthin)) ///
		name("interviewed", replace)
	
	* 4(c)
	twoway (bar employed_now_rate condition2, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7) base(0)) ///
		(rcap employed_now_rate_hi employed_now_rate_lo condition2, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%", margin(small)) ///
		xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
		xtitle(" ", margin(small))  ///
		text(1 0.2 "`employed_now_rate_0'%", size(med)) text(1 1.2 "`employed_now_rate_1'%", size(med)) ///
		text(`employed_now_p' 0.5 "p-value = `p1_employed_now'", size(med)) ///
		ylabel(0(2)14,glcolor(gs15) glwidth(vthin)) ///
		name("employed_now", replace)
	
	* 4(d)
	twoway (bar driving_rate condition2, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7) base(0)) ///
		(rcap driving_rate_hi driving_rate_lo condition2, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%", margin(small)) ///
		xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) ///
		text(5 0.2 "`driving_rate_0'%", size(med)) text(5 1.2 "`driving_rate_1'%", size(med)) ///
		text(`driving_p' 0.5 "p-value = `p1_driving'", size(med)) ///
		ylabel(0(10)90,glcolor(gs15) glwidth(vthin)) ///
		name("driving", replace)
		
********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	

* ------------------------------------------------------------------------------
* 	Follow-up - Figure B3: Perceptions of Others’ Opinions Regarding the Filler Question 
* ------------------------------------------------------------------------------

	
	// filler question: minimum wage
	gen min_wage_fl_per_hi = .
	gen min_wage_fl_per_lo = .
	
	bysort condition2: egen min_wage_fl_per_mean = mean(min_wage_fl_per)
	replace min_wage_fl_per_mean = min_wage_fl_per_mean * 100
	
	forval i = 0/1 {
		sum min_wage_fl_per if condition2 == `i'
		local mean`i': di %4.2f `r(mean)'*100
		ci means min_wage_fl_per if condition2 == `i'
		replace min_wage_fl_per_hi =`r(ub)'*100 if condition2 == `i'
		replace min_wage_fl_per_lo =`r(lb)'*100 if condition2 == `i'
	}
	
	ttest min_wage_fl_per, by(condition2)
	local pvalue = r(p)
	local pvalue: di %4.3f `pvalue'
	
	
	twoway (bar min_wage_fl_per_mean condition2, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7)) ///
			(rcap min_wage_fl_per_hi min_wage_fl_per_lo condition2, lcolor(black)), legend(off) ///
			graphregion(color(white)) ///
			ytitle("%", margin(small)) yscale(range(0 50)) ylabel(0(10)50) ///
			xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
			xtitle(" ", margin(small)) aspectratio(0.7) ///
			text(5 0 "`mean0'%", size(small)) text(5 1 "`mean1'%", size(small)) ///
			text(50 0.5 "p-value = `pvalue'", size(small)) ///
			ylabel(,glcolor(gs15) glwidth(vthin)) ///
			name("min_wage", replace)
			
* ------------------------------------------------------------------------------
* 	Follow-up - Figure B4: Persistence in Beliefs Update 
* ------------------------------------------------------------------------------
	
	sum outside_objective, d
	binscatter outside_others_fl outside_guess, by(condition2) n(10) ///
		xline(`r(mean)', lcolor(maroon) lpattern(-)) ///
		ytitle("Follow-up", height(5)) xtitle(Main Experiment) ///
		ylabel(,glcolor(gs15) glwidth(vthin)) xlabel(,grid glcolor(gs15) glwidth(vthin)) ///
		xsc(range(5(5)30)) ysc(range(5(5)30)) ///
		xlabel(5(5)30, labsize(small)) ylabel(5(5)30, labsize(small)) ///
		legend(label(1 "Control") label(2 "Treatment") size(small)) ///
		colors(gs10 gs0) msymbols(dh o) ///
		aspect(1) name("persistence", replace)

* ------------------------------------------------------------------------------
* 	Follow-up - Figure B6: Long-Term Labor Supply Outcomes: Heterogeneity by Children 
* ------------------------------------------------------------------------------

	rename children child

	count if child == 0 
	local no_child = r(N)
	
	count if child != 0 
	local have_child = r(N)
	
	gen condition_grade_child = condition2 if child == 0
	replace condition_grade_child = 3+condition2 if child > 0
	
	bys condition_grade_child: egen applied_rate_child = mean(applied_out_fl)
	bys condition_grade_child: egen interviewed_rate_child = mean(interviewed_out_fl)
	bys condition_grade_child: egen driving_rate_child = mean(driving_out_fl)
	bys condition_grade_child: egen employed_now_rate_child = mean(employed_now_out_fl)
	replace applied_rate_child = applied_rate_child*100
	replace interviewed_rate_child = interviewed_rate_child*100
	replace driving_rate_child = driving_rate_child*100
	replace employed_now_rate_child = employed_now_rate_child*100

 	foreach var in applied interviewed employed_now driving {

	gen `var'_rate_lo_child = .
	gen `var'_rate_hi_child = .
	
	forval i=0/1{
		su `var'_rate_child if condition2==`i' & child == 0 
		local `var'_rate_nochild_`i': di %4.2f `r(mean)'
		su `var'_rate_child if condition2==`i' & child > 0 
		local `var'_rate_child_`i': di %4.2f `r(mean)' 

		//confidence intervals
		ci proportions `var'_out_fl if condition2==`i' & child == 0
		replace `var'_rate_hi_child =`r(ub)'*100 if condition2 == `i' & child == 0
		replace `var'_rate_lo_child =`r(lb)'*100 if condition2 == `i' & child == 0
		ci proportions `var'_out_fl if condition2==`i' & child > 0
		replace `var'_rate_hi_child =`r(ub)'*100 if condition2 == `i' & child > 0
		replace `var'_rate_lo_child =`r(lb)'*100 if condition2 == `i' & child > 0
	}
	
	prtest `var'_out_fl if child == 0, by(condition2) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local `var'_p1_nochild: di %4.3f `p1'

	prtest `var'_out_fl if child > 0, by(condition2) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local `var'_p1_child: di %4.3f `p1'

	}

	* B6(a). Applied for Job
	
	twoway (bar applied_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9) base(0)) ///
		(rcap applied_rate_hi_child applied_rate_lo_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%") ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(-2 0 "`applied_rate_nochild_0'%", size(med)) text(-2 1 "`applied_rate_nochild_1'%", size(med)) ///
		text(-2 3 "`applied_rate_child_0'%", size(med)) 	 text(-2 4 "`applied_rate_child_1'%", size(med)) ///
		text(50 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(50 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(47 0.5 "p-value = `applied_p1_nochild'", size(med)) text(47 3.5 "p-value = `applied_p1_child'", size(med)) ///
		ylabel(0(10)50,glcolor(gs15) glwidth(vthin)) yscale(range(-3 50)) ///
		name("applied_child", replace)
	
	* B6(b). Interviewed for Job
	
	twoway (bar interviewed_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9) base(0)) ///
		(rcap interviewed_rate_hi_child interviewed_rate_lo_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%") ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(-2 0 "`interviewed_rate_nochild_0'%", size(med)) text(-2 1 "`interviewed_rate_nochild_1'%", size(med)) ///
		text(-2 3 "`interviewed_rate_child_0'%", size(med)) 	 text(-2 4 "`interviewed_rate_child_1'%", size(med)) ///
		text(32 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(32 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(30 0.5 "p-value = `interviewed_p1_nochild'", size(med)) text(30 3.5 "p-value = `interviewed_p1_child'", size(med)) ///
		ylabel(0(5)32,glcolor(gs15) glwidth(vthin)) yscale(range(-3 32)) ///
		name("interviewed_child", replace)
		
	* B6(c). Employed	
	
	twoway (bar employed_now_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9) base(0)) ///
		(rcap employed_now_rate_hi_child employed_now_rate_lo_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%") ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(-2 0 "`employed_now_rate_nochild_0'%", size(med)) 	 text(-2 1 "`employed_now_rate_nochild_1'%", size(med)) ///
		text(-2 3 "`employed_now_rate_child_0'%",   size(med)) 	 text(-2 4 "`employed_now_rate_child_1'%", 	 size(med)) ///
		text(30 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(30 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(28 0.5 "p-value = `employed_now_p1_nochild'", size(med)) text(28 3.5 "p-value = `employed_now_p1_child'", size(med)) ///
		ylabel(0(5)30,glcolor(gs15) glwidth(vthin)) yscale(range(-3 30)) ///
		name("employed_now_child", replace)

	
	* B6(d) Driving Lessons
		
	twoway (bar driving_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9) base(0)) ///
		(rcap driving_rate_hi_child driving_rate_lo_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ytitle("%") ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(-4 0 "`driving_rate_nochild_0'%", size(med)) 	 text(-4 1 "`driving_rate_nochild_1'%", size(med)) ///
		text(-4 3 "`driving_rate_child_0'%",   size(med)) 	 text(-4 4 "`driving_rate_child_1'%", 	size(med)) ///
		text(100 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(100 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(92 0.5 "p-value = `driving_p1_nochild'", size(med)) text(92 3.5 "p-value = `driving_p1_child'", size(med)) ///
		ylabel(0(10)100,glcolor(gs15) glwidth(vthin)) yscale(range(-8 100)) ///
		name("driving_child", replace)
		
* ------------------------------------------------------------------------------
* Export Figures
* ------------------------------------------------------------------------------

	graph export "${outdir}/figure4a.pdf",   name(applied) replace
	graph export "${outdir}/figure4b.pdf",   name(interviewed) replace
	graph export "${outdir}/figure4c.pdf",   name(employed_now) replace
	graph export "${outdir}/figure4d.pdf",   name(driving) replace
	graph export "${outdir}/appendix_figure3.pdf",    name(min_wage) replace
	graph export "${outdir}/appendix_figure4.pdf",    name(persistence) replace
	graph export "${outdir}/appendix_figure6a.pdf",   name(applied_child) replace
	graph export "${outdir}/appendix_figure6b.pdf",   name(interviewed_child) replace
	graph export "${outdir}/appendix_figure6c.pdf",   name(employed_now_child) replace
	graph export "${outdir}/appendix_figure6d.pdf",   name(driving_child) replace

	
	
