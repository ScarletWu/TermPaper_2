*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/6/2020
*	-------
*	Main Experiment Figures 
*	Figure 2/Appendix Figure A9(a): Wedges in Perceptions of Others’ Beliefs (WWOH)
* 	Figure 3: Job-Matching Service Sign-Up 
* 	Figure 5: Job-Matching Service Sign-Up: Heterogeneity by Wedge 
* 	Appendix Figure B1: Wedges in Perceptions of Others’ Beliefs (Semi-segregated 
*						Environment)
* 	Appendix Figure B2: Confidence and Social Connections 
* 	Appendix Figure B5: Job-Matching Service Sign-Up: Heterogeneity by Children
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
	use "${datadir}/clean_data/01_main_exp_clean.dta", clear 
	
* ------------------------------------------------------------------------------
* 	Main Experiment - Figure 2/Appendix Figure B9(a): Wedges in Perceptions of Others’ Beliefs 
* ------------------------------------------------------------------------------

	local q "outside"
	
	gen `q'_guess_share = `q'_guess / 30
	gen `q'_others_truth = `q'_objective / 30 if stag == 1
	su `q'_self
	local `q'_mean `r(mean)'

	gen `q'_wedge_share = (`q'_wedge / 30) * 100

	twoway (hist `q'_wedge_share, lwidth(med) lcolor(gs0) fcolor(gs13) width(10) barwidth(10.5) start(-100)), ///
		xtitle("Wedge (guess % - objective %)") ///
		ytitle("Density", height(5)) ///
		xlabel(-100(20)100) ///
		graphregion(color(white)) ylabel(,glcolor(gs15) glwidth(vthin)) ///
		xline(0, lpattern(_) lcolor(cranberry) lwidth(medthin)) ///
		aspect(0.6) ///
		name("`q'_wedges", replace)
		
* ------------------------------------------------------------------------------
* 	Main Experiment - Figure 3: Job-Matching Service Sign-Up 
* ------------------------------------------------------------------------------
	
	bys condition2: egen sign_up_rate = mean(signed_up_number)
	replace sign_up_rate = sign_up_rate*100

	gen rate_lo = .
	gen rate_hi = .

	forval i=0/1{
		su sign_up_rate if condition2==`i'
		local rate_`i': di %4.2f `r(mean)'
		ci proportions signed_up_number if condition2==`i'
		replace rate_hi =`r(ub)'*100 if condition2 == `i'
		replace rate_lo =`r(lb)'*100 if condition2 == `i'
	}

	prtest signed_up_number, by(condition2)
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1: di %4.3f `p1'

	twoway (bar sign_up_rate condition2, color(gs13) lcolor(black) lwidth(medium) barwidth(0.7)) ///
		(rcap rate_hi rate_lo condition2, lcolor(black)), legend(off) ///
		graphregion(color(white)) ///
		ytitle("% Sign-up", margin(small)) yscale(range(0 40)) ylabel(0(10)40) ///
		xlabel(0 "Control" 1 "Treatment", labgap(small)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) ///
		text(5 0 "`rate_0'%", size(small)) text(5 1 "`rate_1'%", size(small)) ///
		text(38 0.5 "p-value = `p1'", size(small)) ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		name("signup", replace)	
		
* ------------------------------------------------------------------------------
* 	Main Experiment - Figure 5: Job-Matching Service Sign-Up: Heterogeneity by Wedge 
* ------------------------------------------------------------------------------
	
	local q "outside"

	gen rate_lo2_`q' = .
	gen rate_hi2_`q' = .

	gen condition_grade_`q' = condition2 if `q'_wedge <= 0
	replace condition_grade_`q' = 3+condition2 if `q'_wedge > 0

	bys condition_grade_`q': egen sign_up_rate_`q' = mean(signed_up_number)
	replace sign_up_rate_`q' = sign_up_rate_`q' * 100

	forval i=0/1{
		//sign up by treatment, by wedge
		su signed_up_number if condition2==`i' & `q'_wedge <=0
		local rate_pes_`i': di %4.2f `r(mean)'*100
		su signed_up_number if condition2==`i' & `q'_wedge > 0
		local rate_opt_`i': di %4.2f `r(mean)'*100
		//confidence intervals
		ci proportions signed_up_number if condition2==`i' & `q'_wedge <=0
		replace rate_hi2_`q' =`r(ub)'*100 if condition2 == `i' & `q'_wedge <=0
		replace rate_lo2_`q' =`r(lb)'*100 if condition2 == `i' & `q'_wedge <=0
		ci proportions signed_up_number if condition2==`i' & `q'_wedge >0
		replace rate_hi2_`q' =`r(ub)'*100 if condition2 == `i' & `q'_wedge >0
		replace rate_lo2_`q' =`r(lb)'*100 if condition2 == `i' & `q'_wedge >0
	}

	prtest signed_up_number if `q'_wedge<=0, by(condition2) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_pes: di %4.3f `p1'

	prtest signed_up_number if `q'_wedge>0, by(condition2) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_opt: di %4.3f `p1'

	twoway (bar sign_up_rate_`q' condition_grade_`q', color(gs13) lcolor(black) lwidth(medium) barwidth(0.9)) ///
		(rcap rate_hi2_`q' rate_lo2_`q' condition_grade_`q', lcolor(black)), legend(off) ///
		graphregion(color(white)) ///
		ytitle("% Sign-up", height(5)) yscale(range(0 60)) ylabel(0(10)50) ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(10 0 "`rate_pes_0'%", size(small)) text(10 1 "`rate_pes_1'%", size(small)) ///
		text(10 3 "`rate_opt_0'%", size(small)) text(10 4 "`rate_opt_1'%", size(small)) ///
		text(55 0.5 "Wedge {&le} 0", box margin(small) bcolor(gs14%60)) text(55 3.5 "Wedge > 0", box margin(small) bcolor(gs14%60)) ///
		text(50 0.5 "p-value = `p1_pes'", size(small)) text(50 3.5 "p-value = `p1_opt'", size(small)) ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		name("signup_`q'_het", replace)
	
	
********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************	

* ------------------------------------------------------------------------------
* 	Main Experiment - Figure B1: Wedges in Perceptions of Others’ Beliefs 
* ------------------------------------------------------------------------------
	
	local q "semiseg"
	
	gen `q'_guess_share = `q'_guess / 30
	gen `q'_others_truth = `q'_objective / 30 if stag == 1
	su `q'_self
	local `q'_mean `r(mean)'

	gen `q'_wedge_share = (`q'_wedge / 30) * 100

	twoway (hist `q'_wedge_share, lwidth(med) lcolor(gs0) fcolor(gs13) width(10) barwidth(10.5) start(-100)), ///
		xtitle("Wedge (guess % - objective %)") ///
		ytitle("Density", height(5)) ///
		xlabel(-100(20)100) ///
		graphregion(color(white)) ylabel(,glcolor(gs15) glwidth(vthin)) ///
		xline(0, lpattern(_) lcolor(cranberry) lwidth(medthin)) ///
		aspect(0.6) ///
		name("`q'_wedges", replace)	
		
* ------------------------------------------------------------------------------
* 	Main Experiment - Figure B2: Confidence and Social Connections 
* ------------------------------------------------------------------------------
	
	*	B2(a). Confidence and Accuracy

	gen outside_wedge_abs = abs(outside_wedge)
	bys outside_confidence: egen conf_outside_wedge = mean(outside_wedge_abs)
	graph twoway bar conf_outside_wedge outside_confidence, ///
		horizontal ///
		ylabel(1 "Not at all Confident"  3 "Neutral" 5 "Very Confident", labsize(small) ///
		labgap(vsmall)) ytitle("") lwidth(medium) lcolor(black) barwidth(0.75) graphregion(color(white)) ///
		color(gs13) xscale(range(0(5)20)) xlabel(0(5)20, grid glcolor(gs15) glwidth(vthin)) ///
		xtitle("Absolute Wedge (|guess-objective|)") ///
		ylabel(,nogrid) ///
		name("confidence")
		
	
	*	B2(b). Connections and Confidence
	
	binscatter outside_confidence num_know_per, ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		xlabel(,grid glcolor(gs15) glwidth(vthin)) ///
		ytitle("Confidence", height(5)) xtitle("Share of participants known") ///
		mcolor(gs0) lcolor(maroon) ///
		name("connections_confidence")
		
	*	B2(c). Connections and Accuracy

	binscatter outside_wedge_abs num_know_per, ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		xlabel(,grid glcolor(gs15) glwidth(vthin)) ///
		ytitle("Absolute Wedge", height(5)) xtitle("Share of participants known") ///
		mcolor(gs0) lcolor(maroon) ///
		name("connections_accuracy")
		
* ------------------------------------------------------------------------------
* 	Main Experiment - Figure B5: Job-Matching Service Sign-Up: Heterogeneity by Children 
* ------------------------------------------------------------------------------
	rename children child
	
	count if child == 0 
	local no_child = r(N)
	
	count if child != 0 
	local have_child = r(N)
	
	gen rate_lo2_child = .
	gen rate_hi2_child = .

	gen condition_grade_child = condition2 if child == 0
	replace condition_grade_child = 3+condition2 if child > 0

	bys condition_grade_child: egen sign_up_rate_child = mean(signed_up_number)
	replace sign_up_rate_child = sign_up_rate_child * 100

	forval i=0/1{
		//sign up by treatment, by childern
		su signed_up_number if condition2==`i' & child == 0
		local rate_nochild_`i': di %4.2f `r(mean)'*100
		su signed_up_number if condition2==`i' & child > 0
		local rate_child_`i': di %4.2f `r(mean)'*100
		//confidence intervals
		ci proportions signed_up_number if condition2==`i' & child == 0
		replace rate_hi2_child =`r(ub)'*100 if condition2 == `i' & child == 0
		replace rate_lo2_child =`r(lb)'*100 if condition2 == `i' & child == 0
		ci proportions signed_up_number if condition2==`i' & child > 0
		replace rate_hi2_child =`r(ub)'*100 if condition2 == `i' & child > 0
		replace rate_lo2_child =`r(lb)'*100 if condition2 == `i' & child > 0
	}

	prtest signed_up_number if child == 0, by(condition2) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_nochild: di %4.3f `p1'

	prtest signed_up_number if child > 0, by(condition2) 
	local p2 = 2*(normprob(-abs(`r(z)')))
	local p1 = `p2'/2
	local p1_child: di %4.3f `p1'

	twoway (bar sign_up_rate_child condition_grade_child, color(gs13) lcolor(black) lwidth(medium) barwidth(0.9)) ///
		(rcap rate_hi2_child rate_lo2_child condition_grade_child, lcolor(black)), legend(off) ///
		graphregion(color(white)) ///
		ytitle("% Sign-up", height(5)) yscale(range(0 60)) ylabel(0(10)50) ///
		xlabel(0 "Control" 1 "Treatment" 3 "Control" 4 "Treatment", labsize(small) labgap(vsmall)) ///
		xtitle(" ", margin(small)) aspectratio(0.7) xline( 2, lpattern(-) lcolor(gs8)) xscale(range(-1 5)) ///
		text(5 0 "`rate_nochild_0'%", size(small)) text(5 1 "`rate_nochild_1'%", size(small)) ///
		text(5 3 "`rate_child_0'%", size(small)) text(5 4 "`rate_child_1'%", size(small)) ///
		text(55 0.5 "No Children (N=`no_child')", box margin(small) bcolor(gs14%60)) text(55 3.5 "Have Children (N=`have_child')", box margin(small) bcolor(gs14%60)) ///
		text(50 0.5 "p-value = `p1_nochild'", size(small)) text(50 3.5 "p-value = `p1_child'", size(small)) ///
		ylabel(,glcolor(gs15) glwidth(vthin)) ///
		name("signup_child_het", replace)

* ------------------------------------------------------------------------------
* Export Figures
* ------------------------------------------------------------------------------

	graph export "${outdir}/figure2.pdf",    name(outside_wedges) replace
	graph export "${outdir}/figure3.pdf",    name(signup) replace
	graph export "${outdir}/figure5.pdf",    name(signup_outside_het) replace
	graph export "${outdir}/appendix_figure1.pdf",    name(semiseg_wedges) replace
	graph export "${outdir}/appendix_figure2a.pdf",   name(confidence) replace
	graph export "${outdir}/appendix_figure2b.pdf",   name(connections_confidence) replace
	graph export "${outdir}/appendix_figure2c.pdf",   name(connections_accuracy) replace
	graph export "${outdir}/appendix_figure5.pdf",    name(signup_child_het) replace
	graph export "${outdir}/appendix_figure9a.pdf",   name(outside_wedges) replace

