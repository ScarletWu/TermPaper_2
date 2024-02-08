*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Han & R. Wu
*	5/6/2020
*	-------
*	1st Online Survey Figures 
* 	Figure 6: Misperceptions about Others’ Beliefs
* 	Figure B8: Confidence and Accuracy
* 	Figure B9(b)&(c): Wedges in Perceptions of Others’ Beliefs
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	clear all
	set more off 
	
	use "${datadir}/clean_data/03_1st_online_svy_clean.dta", clear 
		
* ------------------------------------------------------------------------------
* 	1st National Survey - Figure 6: Misperceptions about Others’ Beliefs 
* ------------------------------------------------------------------------------
	
	*	wedges
	su c_outside_self
	local c_outside_mean `r(mean)'
	
	su t_list
	local t_list_mean `r(mean)'
	su c_list
	local c_list_mean `r(mean)'
	local t_outside_mean = `t_list_mean' - `c_list_mean'
	
	preserve
	
	cumul c_outside_guess_frac, gen(c_outside_cdf)
	cumul t_outside_guess_frac, gen(t_outside_cdf)
	stack c_outside_cdf c_outside_guess_frac  t_outside_cdf t_outside_guess_frac , ///
		into(cdf resp_share) wide clear
	
	sort resp_share c_outside_cdf t_outside_cdf 
	
	twoway (line c_outside_cdf t_outside_cdf resp_share, lpattern("l" "-") lwidth(medthin medthin) lcolor(gs0 maroon) c(L) sort), ///
		xtitle("Share of others agreeing") ///
		ytitle("Cumulative Probability") ///
		legend(label(1 "Control (perceptions about others' {it:answers})") label(2 "Treatment (perceptions about others' {it:beliefs})") region(lcolor(white)) span cols(1)) ///
		graphregion(color(white)) ylabel(,glcolor(gs15) glwidth(vthin)) xlabel(,grid glcolor(gs15) glwidth(vthin)) ///
		xline(`c_outside_mean', lpattern(l) lcolor(gs0) lwidth(thin)) ///
		xline(`t_outside_mean', lpattern(-) lcolor(maroon) lwidth(thin)) ///
		text(0.1 0.83 "True proportion (control)",size(vsmall) placement(right) color(gs0)) ///
		text(0.1 0.79 "True proportion (treatment)",size(vsmall) placement(left) color(maroon)) ///
		aspect(0.6) ///
		name("misp_beliefs", replace)
		
	restore

********************************************************************************		
***************************        APPENDIX        *****************************		
********************************************************************************		
	
* ------------------------------------------------------------------------------
* 	1st National Survey - Figure B8: Confidence and Accuracy 
* ------------------------------------------------------------------------------	

	foreach cond in c t {
	
	gen `cond'_outside_wedge_abs = abs(`cond'_outside_wedge)
	bys `cond'_outside_confidence: egen `cond'_conf_outside_wedge = mean(`cond'_outside_wedge_abs)
	
	graph twoway bar `cond'_conf_outside_wedge `cond'_outside_confidence, ///
		horizontal ///
		ylabel(1 "Not at all Confident"  3 "Neutral" 5 "Very Confident", labsize(small) ///
		labgap(vsmall)) ytitle("") lwidth(medium) lcolor(black) barwidth(0.75) graphregion(color(white)) ///
		color(gs13) xscale(range(0(20)100)) ///
		xtitle("Absolute Wedge (|guess-objective|)") ///
		xlabel(0(20)100, grid glcolor(gs15) glwidth(vthin)) ylab(,nogrid) ///
		name("`cond'_confidence", replace)
	}
	
* ------------------------------------------------------------------------------
* 	1st National Survey - Figure B9(b)&(c): Wedges in Perceptions of Others’ Beliefs 
* ------------------------------------------------------------------------------	 
	
	* B9(b)&(c): National Survey 1: Control & Treatment
	
	local list "c t"
	foreach i in `list' {
	twoway (hist `i'_outside_wedge, lwidth(medium) lcolor(gs0) fcolor(gs13) width(10) barwidth(10.5) start(-100)), ///
		xtitle("Wedge (guess % - objective %)") ///
		ytitle("Density") ///
		xlabel(-100(20)100) ///
		graphregion(color(white)) ylabel(,glcolor(gs15) glwidth(vthin)) ///
		xline(0, lpattern(_) lcolor(cranberry) lwidth(thin)) ///
		aspect(0.6) ///
		name("wedges_`i'", replace)
	
	}
	
* ------------------------------------------------------------------------------
* Export Figures
* ------------------------------------------------------------------------------

	graph export "${outdir}/figure6.pdf",    name(misp_beliefs) replace	
	graph export "${outdir}/appendix_figure8a.pdf",   name(c_confidence) replace
	graph export "${outdir}/appendix_figure8b.pdf",   name(t_confidence) replace
	graph export "${outdir}/appendix_figure9b.pdf",   name(wedges_c) replace
	graph export "${outdir}/appendix_figure9c.pdf",   name(wedges_t) replace
