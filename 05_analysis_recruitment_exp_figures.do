*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/6/2020
*	-------
*	Recruitment Experiment Figures 
* 	Figure 9: Share of Women Choosing and Showing Up for Job Outside the Home 
* 	Appendix Figure B7: Outside-Home Job: Heterogeneity by Children
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
	use "${datadir}/clean_data/05_recruitment_exp_clean.dta", clear 
	
* ------------------------------------------------------------------------------
* 	Recruitment Experiment - Figure 9: Share of Women Choosing and Showing Up for 
* 										Job Outside the Home 
* ------------------------------------------------------------------------------
	
	* 9(a): Job Sign-up
	
	bysort treated: egen outside_job_takeup_mean = mean(outside_job_takeup)
	
	* change to percentage
	replace outside_job_takeup_mean = outside_job_takeup_mean * 100
	
	gen outside_job_takeup_hi = .
	gen outside_job_takeup_lo = .
	
	forval i = 0/1 {
	sum outside_job_takeup if treated == `i'
	local mean`i': di %4.2f `r(mean)'*100
	ci proportions outside_job_takeup if treated == `i'
	replace outside_job_takeup_hi =`r(ub)'*100 if treated == `i'
	replace outside_job_takeup_lo =`r(lb)'*100 if treated == `i'
}
	
	prtest outside_job_takeup, by(treated) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local pvalue = `p2'/2
	local pvalue: di %4.3f `pvalue'
	display `pvalue'

	twoway (bar outside_job_takeup_mean treated, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7)) ///
			(rcap outside_job_takeup_hi outside_job_takeup_lo treated, lcolor(black)), legend(off) ///
			graphregion(color(white)) ///
			ytitle("% Sign-up for Outside-Home Job", margin(small)) yscale(range(0 40)) ylabel(0(10)40) ///
			xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
			xtitle(" ", margin(small)) aspectratio(0.7) ///
			text(5 0 "`mean0'%", size(med)) text(5 1 "`mean1'%", size(med)) ///
			text(40 0.5 "p-value = `pvalue'", size(med)) ///
			ylabel(,glcolor(gs15) glwidth(vthin)) ///
			name(job_takeup, replace)
	

	* 9(b) Job Take-up

	bysort treated: egen outside_job_takeupXshow_up_mean = mean(outside_job_takeupXshow_up)
	
	* change to percentage
	replace outside_job_takeupXshow_up_mean =outside_job_takeupXshow_up_mean * 100
	
	gen outside_job_takeupXshow_up_hi = .
	gen outside_job_takeupXshow_up_lo = .
	
	forval i = 0/1 {
	sum outside_job_takeupXshow_up if treated == `i'
	local mean`i': di %4.2f `r(mean)'*100
	ci proportions outside_job_takeupXshow_up if treated == `i'
	replace outside_job_takeupXshow_up_hi =`r(ub)'*100 if treated == `i'
	replace outside_job_takeupXshow_up_lo =`r(lb)'*100 if treated == `i'
}

	prtest outside_job_takeupXshow_up, by(treated) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local pvalue = `p2'/2
	local pvalue: di %4.3f `pvalue'

	twoway (bar outside_job_takeupXshow_up_mean treated, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7)) ///
			(rcap outside_job_takeupXshow_up_hi outside_job_takeupXshow_up_lo treated, lcolor(black)), legend(off) ///
			graphregion(color(white)) ///
			ytitle("% Show-up for Outside-Home Job", margin(small)) yscale(range(0 35)) ylabel(0(10)35) ///
			xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
			xtitle(" ", margin(small)) aspectratio(0.7) ///
			text(5 0 "`mean0'%", size(med)) text(5 1 "`mean1'%", size(med)) ///
			text(32 0.5 "p-value = `pvalue'", size(med)) ///
			ylabel(,glcolor(gs15) glwidth(vthin)) ///
			name(job_showup, replace)
			
********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	

* ------------------------------------------------------------------------------
* 	Recruitment Experiment - Figure B7: Outside-Home Job: Heterogeneity by Children
* ------------------------------------------------------------------------------	
	
	count if num_child == 0 
	local no_child = r(N)
	
	count if num_child != 0 
	local have_child = r(N)

	* B7(a). Outside-Home Job Take-up
	
	preserve 
	
	gen rate_lo2_child = .
	gen rate_hi2_child = .

	gen condition_grade_child = treated if num_child == 0
	replace condition_grade_child = 3+treated if num_child > 0

	bys condition_grade_child: egen sign_up_rate_child = mean(outside_job_takeup)
	replace sign_up_rate_child = sign_up_rate_child * 100

	forval i=0/1{
		//sign up by treatment, by childern
		su outside_job_takeup if treated==`i' & num_child == 0
		local rate_nochild_`i': di %4.2f `r(mean)'*100
		su outside_job_takeup if treated==`i' & num_child > 0
		local rate_child_`i': di %4.2f `r(mean)'*100
		//confidence intervals
		ci proportions outside_job_takeup if treated==`i' & num_child == 0
		replace rate_hi2_child =`r(ub)'*100 if treated == `i' & num_child == 0
		replace rate_lo2_child =`r(lb)'*100 if treated == `i' & num_child == 0
		ci proportions outside_job_takeup if treated==`i' & num_child > 0
		replace rate_hi2_child =`r(ub)'*100 if treated == `i' & num_child > 0
		replace rate_lo2_child =`r(lb)'*100 if treated == `i' & num_child > 0
	}

	prtest outside_job_takeup if num_child == 0, by(treated) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_nochild: di %4.3f `p1'

	prtest outside_job_takeup if num_child > 0, by(treated) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_child: di %4.3f `p1'

	twoway (bar sign_up_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9)) ///
		(rcap rate_hi2_child rate_lo2_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ///
		ytitle("% Sign-up for Outside-Home Job") yscale(range(0 60)) ylabel(0(10)60) ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(5 0 "`rate_nochild_0'%", size(med)) text(5 1 "`rate_nochild_1'%", size(med)) ///
		text(5 3 "`rate_child_0'%",   size(med)) text(5 4 "`rate_child_1'%",   size(med)) ///
		text(55 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(55 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(50 0.5 "p-value = `p1_nochild'", size(med)) text(50 3.5 "p-value = `p1_child'", size(med)) ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		name("takeup_child_het", replace)
	
	restore
	

	* B7(b). Outside-Home Job Show-up
	
	preserve
	
	gen rate_lo2_child = .
	gen rate_hi2_child = .

	gen condition_grade_child = treated if num_child == 0
	replace condition_grade_child = 3+treated if num_child > 0

	bys condition_grade_child: egen show_up_rate_child = mean(outside_job_takeupXshow_up)
	replace show_up_rate_child = show_up_rate_child * 100

	forval i=0/1{
		//show up by treatment, by childern
		su outside_job_takeupXshow_up if treated==`i' & num_child == 0
		local rate_nochild_`i': di %4.2f `r(mean)'*100
		su outside_job_takeupXshow_up if treated==`i' & num_child > 0
		local rate_child_`i': di %4.2f `r(mean)'*100
		//confidence intervals
		ci proportions outside_job_takeupXshow_up if treated==`i' & num_child == 0
		replace rate_hi2_child =`r(ub)'*100 if treated == `i' & num_child == 0
		replace rate_lo2_child =`r(lb)'*100 if treated == `i' & num_child == 0
		ci proportions outside_job_takeupXshow_up if treated==`i' & num_child > 0
		replace rate_hi2_child =`r(ub)'*100 if treated == `i' & num_child > 0
		replace rate_lo2_child =`r(lb)'*100 if treated == `i' & num_child > 0
	}

	prtest outside_job_takeupXshow_up if num_child == 0, by(treated) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_nochild: di %4.3f `p1'

	prtest outside_job_takeupXshow_up if num_child > 0, by(treated) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_child: di %4.3f `p1'

	twoway (bar show_up_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9)) ///
		(rcap rate_hi2_child rate_lo2_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ///
		ytitle("% Show-up for Outside-Home Job") yscale(range(0 60)) ylabel(0(10)60) ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(5 0 "`rate_nochild_0'%", size(med)) text(5 1 "`rate_nochild_1'%", size(med)) ///
		text(5 3 "`rate_child_0'%",   size(med)) text(5 4 "`rate_child_1'%",   size(med)) ///
		text(55 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(55 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(50 0.5 "p-value = `p1_nochild'", size(med)) text(50 3.5 "p-value = `p1_child'", size(med)) ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		name("showup_child_het", replace)
	
	restore

* ------------------------------------------------------------------------------
* Export Figures
* ------------------------------------------------------------------------------

	graph export "${outdir}/figure9a.pdf",  name(job_takeup) replace
	graph export "${outdir}/figure9b.pdf",  name(job_showup) replace
	graph export "${outdir}/appendix_figure7a.pdf",   name(takeup_child_het) replace
	graph export "${outdir}/appendix_figure7b.pdf",   name(showup_child_het) replace
	
	
