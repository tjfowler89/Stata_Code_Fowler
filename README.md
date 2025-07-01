This docuement outlines the Stata code used in the analysis for the following paper, which is published in PLoS Medicine (2025):
Association between surgeon training grade and the risk of revision following total knee replacement: an analysis of National Joint Registry data

Corresponding author: t.j.fowler@bristol.ac.uk

We request that users cite their use of this code accordingly.

Data availaibility statement: The data used in the study are available from The National Joint Registry (NJR) (https://www.njrcentre.org.uk). 
Restrictions apply to the availability of these data, which were used under license for the current study, and are therefore not publicly available. 
Data access applications can be made to the NJR Research Committee. With NJR permission in place, the data underlying the presented results will be available to access via the NJR data access network. 
The authors of this manuscript are not the data owner and do not have permission to share the data.

The base dataset used in this analysis was the same as that used in the NJR 17th Annual Report.

NB: Please note that the term training_status is used in this code. In the paper, we use the term surgeon_grade.

NB: Please see S2 Appendinx in the supplementary material for the paper, which summarises the steps taken in defining the number of knots/degrees of freedom used in the stpm2 model. 
S2 Appendix also outlines the justification for the use of stpm2 (FPM) over conventional Cox PH models.

*STEP 1: Defining and labelling key categorial variables

{
//Training & Supervision categories
//Generating training_status: 1=trainee 0=consultant
encode 		LeadSurgeonGrade, generate(surgeon_grade)
generate 	training_status = surgeon_grade
replace  	training_status =1 if surgeon_grade== 3  	//F1-ST2
replace  	training_status =1 if surgeon_grade== 4  	//Fellow
replace  	training_status =1 if surgeon_grade== 5 	//HO
replace		training_status =1 if surgeon_grade== 10  //SPR/ST3-8
replace  	training_status =1 if surgeon_grade== 11  //SHO
replace  	training_status =1 if surgeon_grade== 12 	//SPR
replace  	training_status =0 if surgeon_grade== 2  	//Consultant

//Label variables
label 		define training_status_lb 1 "Trainee" 0 "Consultant"
label 		values training_status training_status_lb
label 		variable training_status "Training Status"

//Drop sugeon grades if not relevant to the analysis
//Keeps Consultant; SpR; SHO; F1-ST2; Fellow
drop if 	surgeon_grade== 1  //assoc spec	
drop if 	surgeon_grade== 6  //other 		
drop if 	surgeon_grade== 7  //other 		
drop if 	surgeon_grade== 8  //other		
drop if 	surgeon_grade== 9  //overseas	
drop if 	surgeon_grade== 13 //SAS		
drop if 	surgeon_grade== 14 //Staff grade

//Generate assistant_status: 1=consultant 0=other
encode 		FirstAssistantGrade, generate (assistant_grade)
generate 	assistant_status = assistant_grade
replace 	assistant_status =1 if assistant_grade==2  //cons
replace 	assistant_status =0 if assistant_grade==1  //assoc spec
replace 	assistant_status =0 if assistant_grade==3  //fellow
replace 	assistant_status =0 if assistant_grade==4  //ho
replace 	assistant_status =0 if assistant_grade==5  //non-medical prac
replace 	assistant_status =0 if assistant_grade==6  //non-medic quali prac
replace 	assistant_status =0 if assistant_grade==7  //other
replace 	assistant_status =0 if assistant_grade==8  //other
replace 	assistant_status =0 if assistant_grade==9  //other
replace 	assistant_status =0 if assistant_grade==10 //overseas
replace 	assistant_status =0 if assistant_grade==11 //spr
replace 	assistant_status =0 if assistant_grade==12 //sho
replace 	assistant_status =0 if assistant_grade==13 //spr
replace 	assistant_status =0 if assistant_grade==14 //spec nurse prac
replace 	assistant_status =0 if assistant_grade==15 //staff grade/clinical ass
replace 	assistant_status =0 if assistant_grade==16 //surg ass

//Label variables
label		define assistant_status_lb 1 "Consultant" 0 "Other"
label		values assistant_status assistant_status_lb
label		variable assistant_status "Assistant"

//Generate surgeon_assistant_combo
generate 	surgeon_assistant_combo = training_status
replace  	surgeon_assistant_combo =1 if training_status==0
replace  	surgeon_assistant_combo =2 if training_status==1 & assistant_status==1
replace  	surgeon_assistant_combo =3 if training_status==1 & assistant_status==0

//Label variables
label 		define surgeon_assistant_combo_lb 1 "Consultant" 2 "Trainee:Cons" 3 "Trainee:Other"
label 		values surgeon_assistant_combo surgeon_assistant_combo_lb
label 		variable surgeon_assistant_combo "Surgeon:Assistant"

//Age 
//Drop the existing age category cariable & gen new age_grp that mirrors NJR categories
drop 	 age_grp
generate age_grp = 1 if age <55
replace  age_grp = 2 if age >=55 & age <65
replace  age_grp = 3 if age >=65 & age <75
replace  age_grp = 4 if age >=75 & age <85
replace  age_grp = 5 if age >=85 

//label age categories
label define 	age_grp_lb 1"<55" 2"55-64" 3"65-74" 4"75-84" 5"85+"
label value 	age_grp age_grp_lb

//Sex
rename		Sex sex

//ASA - new ASA categories 1-3
generate 	ASA = asa
replace 	ASA = 1 if asa==1
replace 	ASA = 2 if asa==2
replace 	ASA = 3 if asa==3
replace 	ASA = 3 if asa==4
replace 	ASA = 3 if asa==5

//label ASA categories
label 		define ASA_lb 1"1" 2"2" 3">=3" 
label 		value ASA ASA_lb
drop 		asa
rename 		ASA asa

//BMI categories
generate 	BMI_cat = BMI
replace		BMI_cat = 5 if BMI==.
replace 	BMI_cat = 5 if BMI <10
replace 	BMI_cat = 5 if BMI >60
replace 	BMI_cat = 1 if BMI >=19 	& BMI <25
replace 	BMI_cat = 2 if BMI >=10 	& BMI <19
replace 	BMI_cat = 3 if BMI >=25 	& BMI <30
replace 	BMI_cat = 4 if BMI >=30

//label BMI categories
label define 	BMI_cat_lb 1"Normal (19-24.9)" 2"Underweight (<19)" 3"Overweight (25-29.9)" 4"Obese (>30)" 5"Missing"
label value 	BMI_cat BMI_cat_lb

//Anaesthetic
rename 		general_anaesthesia_used 	anaesthetic_general
rename 		epidural_anaesthesia_used 	anaesthetic_epidural
rename 		spinal_anaesthesia_used 		anaesthetic_spinal
rename 		nerve_block_anaesthesia_used anaesthetic_block

//Approach
encode 		surgical_approach, generate(approach)
drop 		surgical_approach
replace 	approach = 4 if approach==5
replace 	approach = 5 if approach==6
label 		define approach_lb 1"Lateral parapatellar" 2"Medial parapatellar" 3"Mid-vastus" 4"Other" 5"Sub-vastus"
label 		value approach approach_lb

//Contraint
encode 		constraint_combine, generate(constraint)	
drop 		constraint_tot_men
drop 		constraint_uni_men
drop 		constraint_combine

//Fixation
rename fixation_combine fixation

//Patella resurfacing
generate patella_resurfaced = patella_total
replace patella_resurfaced 	= 1 if patella_total==1
replace patella_resurfaced 	= 0 if patella_total==.

//label categories
label define patella_resurfaced_lb 1"Yes" 0"No"
label value patella_resurfaced patella_resurfaced_lb
drop patella_total

//Funding
rename 		Funding funder
replace		funder =. if funder==1
replace		funder =1 if funder==2
replace		funder =0 if funder==3
label 		define funder_lb 1"NHS" 0"Private"
label 		value funder funder_lb

//Year of operation
generate 	year_op = year
replace 	year_op =1 if year == 2003
replace 	year_op =1 if year == 2004
replace 	year_op =1 if year == 2005
replace 	year_op =1 if year == 2006
replace 	year_op =1 if year == 2007
replace 	year_op =1 if year == 2008
replace 	year_op =1 if year == 2009
replace 	year_op =1 if year == 2010
replace 	year_op =1 if year == 2011
replace 	year_op =2 if year == 2012
replace 	year_op =2 if year == 2013
replace 	year_op =2 if year == 2014
replace 	year_op =2 if year == 2015
replace 	year_op =2 if year == 2016
replace 	year_op =2 if year == 2017
replace 	year_op =2 if year == 2017
replace 	year_op =2 if year == 2018
replace 	year_op =2 if year == 2019

//Label variables
label 		define year_op_lb 1 "2003-2011" 2 "2012-2019"
label 		values year_op year_op_lb
label 		variable year_op "Year Op Categories"
}


*STEP 2: Accounting for missing data - excludes missing values in any of the categories that might be adjusted for.

{
// Patient factors
* Age Sex ASA
count if age ==. 
count if sex ==.
count if asa ==.			

count if (inlist(.,age, sex, asa)==1)

notes _dta : "Missing Patient factors = `r(N)'"

gen patient_ok = 1 if inlist(.,age, sex, asa)!=1

count if patient_ok == 1
notes _dta : "Patient factors = `r(N)'"
			 
// Operation data
*fixation, approach, anaesthetic, contraint, patella resufacing
count if fixation				==. 	& patient_ok == 1	
count if approach				==. 	& patient_ok == 1
count if anaesthetic_general	==. 	& patient_ok == 1	
count if anaesthetic_spinal		==. 	& patient_ok == 1	
count if anaesthetic_epidural	==. 	& patient_ok == 1	
count if anaesthetic_block		==. 	& patient_ok == 1		
count if constraint				==.		& patient_ok == 1		
count if patella_resurfaced		==. 	& patient_ok == 1	

count if inlist(., fixation, approach, anaesthetic_general,	///
					anaesthetic_spinal, anaesthetic_epidural,	///
					anaesthetic_block, constraint, patella_resurfaced)==1			///
					& patient_ok == 1	

notes _dta : "Missing Operation factors = `r(N)'"


gen operation_ok = 1 if inlist(., fixation, approach, anaesthetic_general,	///
					anaesthetic_spinal, anaesthetic_epidural,	///
					anaesthetic_block, constraint, patella_resurfaced)!=1

count if patient_ok==1  & operation_ok==1
notes _dta : "Operation factors = `r(N)'"

// Unit factors
*funder, year, IMDdecile
count if funder 			==.			& patient_ok == 1 & operation_ok==1
count if year_op			==.			& patient_ok == 1 & operation_ok==1
count if lsoa11_imd_decile 	==.			& patient_ok == 1 & operation_ok==1

count if inlist(., funder, year_op, lsoa11_imd_decile)==1 			///
				& patient_ok == 1  & operation_ok==1	

notes _dta : "Missing Unit factors = `r(N)'"


gen unit_ok = 1 if inlist(., funder, year_op, lsoa11_imd_decile)!=1

count if patient_ok==1  & operation_ok==1 & unit_ok==1
notes _dta : "Unit factors = `r(N)'"

// Surgeon Factors
count if surgeon_grade==.  	& patient_ok == 1  & operation_ok==1 & unit_ok==1
count if assistant_status==.	& patient_ok == 1  & operation_ok==1 & unit_ok==1

count if inlist(., training_status, assistant_status)==1 ///
	& patient_ok == 1  & operation_ok==1	 & unit_ok==1

	notes _dta : "Missing Surgeon factors = `r(N)'"
	
gen surgeon_ok =1 if inlist(., training_status, assistant_status)!=1
	count if patient_ok == 1  & operation_ok==1 & unit_ok==1 & surgeon_ok==1
		notes _dta : "Surgeon factors = `r(N)'"
}	


*STEP 3: Desginate survival data

{
// St set the data IMPLANT FAILURE
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///
,  id(id) failure(fail==1) exit(fail== 1 2) 
} 

*STEP 4: Code for each table + figure

*Table 2 - The unadjusted cumulative probability of all-cause revision of TKRs according to surgeon grade (exposure A) and supervision (exposure B). 

{
* Life table 
generate revised = fail
replace revised = 0 if fail ==2
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///
                                                            ,  id(id) failure(revised== 1) exit(revised== 1) 
keep if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)
ltable fu_time revised, survival failure by(surgeon_grade) intervals(1)
}


*Figure 2 - Kaplan-Meier plots (one minus survival) demonstrating the cumulative probability of TKR failure (i.e. all-cause revision) according to surgeon grade (A) and supervision (B).

{
*i. Surgeon grade (Figure 2A)
stsplit sp_time, at(17)
keep if sp_time==0

#delimit ;
sts graph, fail by (training_status) ci 
	  title("" , ring(0) size(small) c(black) )
	  xtitle("Years since primary procedure",size(small))
	  xlabel(0(2)17, labsize(small)) xmtick(0 (1) 17, labsize(small))
	  ylabel(0(0.01)0.07)
	  ylabel( `ylabstr'  , nogrid labsize(small) ) 
	  ytitle("Cumulative probability of failure (%)",size(small))
	  graphregion(color(white)) 
	  risktable(, size(small) rowtitle("Consultant"	,	size(small)) group(#1)) ///
	  risktable(, size(small) rowtitle("Trainee"	,	size(small)) group(#2)) ///
	  risktable(, title("Number at risk", size(small)))
	  legend( `newlab' size(small) on   cols(2) lwidth(none) region(lwidth(none) c(none) lc(none)))
;
#delimit cr

*ii. Supervised vs unsupervised trainees (Figure 2B)
stsplit sp_time, at(17)
keep if sp_time==0

drop if surgeon_assistant_combo==1
#delimit ;
sts graph, fail by (surgeon_assistant_combo) ci 
	  title("" , ring(0) size(small) c(black) )
	  xtitle("Years since primary procedure",size(small))
	  xlabel(0(2)17, labsize(small)) xmtick(0 (1) 17, labsize(small))
	  ylabel(0(0.01)0.07)
	  ylabel( `ylabstr'  , nogrid labsize(small) ) 
	  ytitle("Cumulative probability of failure (%)",size(small))
	  graphregion(color(white)) 
	  risktable(, size(small) rowtitle("Consultant"	,	size(small)) group(#1)) ///
	  risktable(, size(small) rowtitle("Trainee"	,	size(small)) group(#2)) ///
	  risktable(, title("Number at risk", size(small)))
	  legend( `newlab' size(small) on   cols(2) lwidth(none) region(lwidth(none) c(none) lc(none)))
;
#delimit cr
}

*Figure 3 - Risk of all-cause revision of TKRs according to surgeon grade (exposure A).

{		
*MODEL 1

xi: stpm2 i.training_status, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)	
}

{		
*MODEL 2

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)	
}

{		
*MODEL 3

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)	
}

{		
*MODEL 4

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)		
}


{
*BMI: Sensitivity anlaysis
xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.BMI_cat ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.surgeon_grade) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}

{
*NHS only cases: Sensitivity anlaysis

keep if unit_type==1
keep if funder ==1

//restricts anlaysis to only NHS funded cases in NHS units.

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}

*Figure 4 - Risk of all-cause revision of TKRs according to the level of supervision of trainees (exposure B).

{
//anlaysis resticted to supervised vs unsupervised trainees.

xi: stpm2 i.surgeon_assistant_combo ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.surgeon_assistant_combo) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Isurgeon_assistant_combo__1 1) hrdenom(_Isurgeon_assistant_combo__1 0 timevar(temptime) ci

twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)	
}

*Figure 5 - Risk of all-cause revision of TKRs according to specific training grade (exposure C).

{
//analysis restricted to trainee cases (F1-ST2; ST3-8; fellow)

xi: stpm2 i.surgeon_grade ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.surgeon_grade) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Isurgeon_grade__1 1) hrdenom(_Isurgeon_grade__1 0) timevar(temptime)ci

twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}


*Figure 6 - Risk of all-cause revision of TKRs according to the level of supervision of ST3-ST8 trainees.

//analysis restricted to ST3-ST8 cases, subgrouped accoring to supervision.

{
xi: stpm2 i.surgeon_assistant_combo ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.surgeon_assistant_combo) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Isurgeon_assistant_combo__1 1) hrdenom(_Isurgeon_assistant_combo__1 0 timevar(temptime) ci

twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)	
}

*Figure 7 - The indication for TKR revision according to surgeon grade (exposure A).
		
//ASEPTIC LOOSENING/LYSIS

{
//generating 'failure due to loosening' 
generate loosening = fail
replace loosening = 0 if rfr_aseptic_lysis !=1 & rfr_aseptic_lysis_2 !=1 
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///    
                                                        ,  id(id) failure(loosening==1) exit(fail == 1 2)

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}		

//INFECTION 

{
//generating 'failure due to infection' 
generate infection = fail
replace infection = 0 if rfr_Infection !=1 & rfr_Infection_2 !=1
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///    
                                                        ,  id(id) failure(infection==1) exit(fail == 1 2)

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci	
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}


//INSTABILITY

{
//generating 'failure due to instability' 
generate instability = fail
replace instability = 0 if rfr_dislocationsubluxation !=1 & rfr_Instability !=1 & rfr_dislocationsubluxation_2 !=1 & rfr_Instability_2 !=1 
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///    
                                                        ,  id(id) failure(instability==1) exit(fail == 1 2)

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci	
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}

		
//PAIN

{
//generating 'failure due to pain' 
generate pain = fail
replace pain = 0 if rfr_pain !=1 & rfr_pain_2 !=1
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///    
                                                        ,  id(id) failure(pain==1) exit(fail == 1 2)


xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci	
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}


//PROGRESSIVE OA 

{
//generating 'failure due to prog oa' 
generate progress_oa = fail
replace progress_oa = 0 if rfr_ProgressiveArthRemains !=1 & rfr_ProgressiveArthRemains_2 !=1
stset fu_time if !inlist(., patient_ok, operation_ok, unit_ok, surgeon_ok)  ///    
                                                        ,  id(id) failure(progress_oa==1) exit(fail == 1 2)
}   

xi: stpm2 i.training_status ///
i.age_grp ///
i.asa ///
i.sex ///
i.lsoa11_imd_decile ///
i.approach ///
i.fixation ///
i.constraint ///
i.patella_resurfaced ///
i.anaesthetic_block ///
i.anaesthetic_general ///
i.anaesthetic_spinal ///
i.anaesthetic_epidural ///
i.year_op ///
i.funder, ///
scale(hazard) df(8) tvc(i.training_status) dftvc(2) eform

range temptime 0 18 7000 

predict hr, hrnum(_Itraining__1 1) hrdenom(_Itraining__1 0) timevar(temptime) ci		
twoway	(rarea hr_lci hr_uci temptime if temptime<=17, sort pstyle(ci)) ///
		(line hr temptime if temptime<=17, sort lpattern(solid) lwidth(thick)) ///
		, scheme(sj) ///
		legend(off) ///
		ylabel(,angle(h)) ytitle(Hazard ratio) ///
		xlabel (0(1)17) ///
		xtitle("Years since primary procedure") ///
		yline(1, lpattern(dash) lwidth(thin)) yscale(log)
}
