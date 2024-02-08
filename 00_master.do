*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*
*	Misperceived Social Norms
*	L. Bursztyn, A. Gonzalez, D. Yanagizawa-Drott
*	Prepared by: R. Wu
*	5/6/2020
*	-------
*	Master Do File 
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*

	version 15.1
	clear all
	set more off
	
* ------------------------------------------------------------------------------
* Change root directory by username and operating system
* ------------------------------------------------------------------------------
	
	global maindirectory = "~/Desktop/replication_package" 

	cd "${maindirectory}"

	* Define directory to be checked or created
	global codedir  = "${maindirectory}/code"
	global datadir  = "${maindirectory}/data"
	global outdir   = "${maindirectory}/output"
	global adobase  = "${maindirectory}"
	
* ------------------------------------------------------------------------------
* Run codes
* ------------------------------------------------------------------------------

* 	Each of the following do files cleans or analyzes one particular dataset. 
* 	The analysis do files are further split by figure and table outputs.  
* 	Please refer to the README file which states the specific code for each figure/table. 

	***** Data cleaning (The code takes less than a minute to finish)
	do "${codedir}/01_cleaning/01_clean_main_exp.do"
	do "${codedir}/01_cleaning/02_clean_follow_up.do"
	do "${codedir}/01_cleaning/03_clean_1st_online_svy.do"
	do "${codedir}/01_cleaning/04_clean_2nd_online_svy.do"
	do "${codedir}/01_cleaning/05_clean_recruitment_exp.do" 
	do "${codedir}/01_cleaning/06_clean_arab_barometer.do"
	
	***** Data analysis
	
	** Tables (The code takes about 20 minutes to finish.)
	do "${codedir}/02_analysis/01_analysis_main_exp_tables.do"
	do "${codedir}/02_analysis/02_analysis_follow_up_tables.do"
	do "${codedir}/02_analysis/03_analysis_1st_online_svy_tables.do"
	do "${codedir}/02_analysis/04_analysis_2nd_online_svy_tables.do"
	do "${codedir}/02_analysis/05_analysis_recruitment_exp_tables.do"
	
	** Figures (The code takes about a minute to finish.)
	do "${codedir}/02_analysis/01_analysis_main_exp_figures.do"
	do "${codedir}/02_analysis/02_analysis_follow_up_figures.do"
	do "${codedir}/02_analysis/03_analysis_1st_online_svy_figures.do"
	do "${codedir}/02_analysis/04_analysis_2nd_online_svy_figures.do"
	do "${codedir}/02_analysis/05_analysis_recruitment_exp_figures.do"
	do "${codedir}/02_analysis/06_analysis_arab_barometer_figure.do"
