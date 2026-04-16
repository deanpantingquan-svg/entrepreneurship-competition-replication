use "data_final_cleaned.dta", clear

drop if entre_consistent==.

///Table 1
bysort treatment entre_consistent: outreg2 using table1.doc, replace sum(log) dec(2) eqkeep( mean sd) keep(female married1 ball_success investment_point age education vocational contacts profit_total1 guess_value ball_success_2)

tab female entre_c, chi2
tab female treatment, chi2
tab married1 entre_c, chi2
tab married1 treatment, chi2
ranksum  ball_success, by(entre_c)
ranksum  ball_success, by(treatment)
ranksum  ball_success_2, by(entre_c)
ranksum  ball_success_2, by(treatment)
ranksum investment_point, by(entre_c)
ranksum investment_point, by(treatment)
ranksum age, by(entre_c)
ranksum age, by(treatment)
ranksum  education, by(entre_c)
ranksum  education, by(treatment)
ranksum  vocational, by(entre_c)
ranksum  vocational, by(treatment)
ranksum  contacts, by(entre_c)
ranksum  contacts, by(treatment)
ranksum  profit_total1, by(treatment)


///Figure 1
cibar competition, over(entre_c treat) barcol(gs0 gs10)  

///Table 2

quietly reg competition  i.treatment i.entre_c i.treatment##i.entre_c, robust cluster(ball_room)

test _b[1.entre_c#1.treatment] +  _b[1.treatment] =0
local p1 = r(p)
test _b[1.entre_c#1.treatment] +  _b[1.entre_c] =0
local p2 = r(p)

outreg2 using table2.doc, replace label dec(3)  title(Competitive choice) ctitle(No Control) addstat(Public + Public*Business p-value, `p1', Business + Public*Business p-value, `p2')

quietly reg competition  i.treatment i.entre_c i.treatment##i.entre_c  female investment_point married1  education  vocational age contacts area, robust cluster(ball_room)


test _b[1.entre_c#1.treatment] +  _b[1.treatment] =0
local p1 = r(p)

test _b[1.entre_c#1.treatment] +  _b[1.entre_c] =0
local p2 = r(p)

outreg2 using table2.doc, append label dec(3)  ctitle(Controls) addstat(Public + Public*Business p-value, `p1', Business + Public*Business p-value, `p2')


quietly reg competition  i.treatment i.entre_c i.treatment##i.entre_c female investment_point married1  education  vocational age contacts guess_value if area==1, robust cluster(ball_room)

test _b[1.entre_c#1.treatment] +  _b[1.treatment] =0
local p1 = r(p)
test _b[1.entre_c#1.treatment] +  _b[1.entre_c] =0
local p2 = r(p)

outreg2 using table2.doc, append label dec(3)  title(Competitive choice) ctitle(Confidence) addstat(Public + Public*Business p-value, `p1', Business + Public*Business p-value, `p2')

///Table 3 
bysort treatment entre_consistent: outreg2 using table3.xls, replace sum(log) dec(2) eqkeep( mean sd) keep(comp_approp_business_own comp_success_business_own comp_approp_salaried_own comp_success_salaried_own surv_like_comp_pub surv_like_comp surv_others_opinion surv_social_image surv_do_best )

ranksum comp_approp_business_own , by(entre_c)
ranksum comp_approp_business_own , by(treatment)
ranksum comp_approp_salaried_own , by(entre_c)
ranksum comp_approp_salaried_own , by(treatment)
ranksum comp_success_business_own , by(entre_c)
ranksum comp_success_business_own , by(treatment)
ranksum comp_success_salaried_own , by(entre_c)
ranksum comp_success_salaried_own , by(treatment)

ranksum surv_like_comp_pub , by(entre_c)
ranksum surv_like_comp_pub , by(treatment)
ranksum surv_like_comp , by(entre_c)
ranksum surv_like_comp , by(treatment)
ranksum surv_others_opinion , by(entre_c)
ranksum surv_others_opinion , by(treatment)
ranksum surv_social_image , by(entre_c)
ranksum surv_social_image , by(treatment)
ranksum surv_do_best , by(entre_c)
ranksum surv_do_best , by(treatment)

clear all

***Appendix 
use "data_final_cleaned.dta"
replace entre_base=entre_co if area==1

***Figure A2
cibar competition, over(entre_base treatment) barcol(gs0 gs10)  

***Table A1
probit competition i.treatment i.entre_c treatment##i.entre_c , robust cluster(ball_room)
outreg2 using table_A1.doc, replace label dec(3)  title(Probit Regressions on Competitive Choice)  ctitle(No Controls) 

probit competition i.treatment i.entre_c treatment##i.entre_c female investment_point married1  education  vocational age contacts  , robust cluster(ball_room)
outreg2 using table_A1.doc, append label dec(3)   ctitle(Controls) 

probit competition i.treatment i.entre_c treatment##i.entre_c female investment_point married1  education  vocational age contacts guess_value if area==1 , robust cluster(ball_room)
outreg2 using table_A1.doc, append label dec(3) ctitle(Confidence)  

***Table A2
quietly reg competition  i.treatment i.entre_base i.treatment##i.entre_base, robust cluster(ball_room)

test _b[1.entre_base#1.treatment] +  _b[1.treatment] =0
local p1 = r(p)
test _b[1.entre_base#1.treatment] +  _b[1.entre_base] =0
local p2 = r(p)

outreg2 using tableA2.doc, replace label dec(3)  title(Competitive choice) ctitle(No Control) addstat(Public + Public*Business p-value, `p1', Business + Public*Business p-value, `p2')

quietly reg competition  i.treatment ii.entre_base i.treatment##i.entre_base  female investment_point married1  education  vocational age contacts area, robust cluster(ball_room)

test _b[1.entre_base#1.treatment] +  _b[1.treatment] =0
local p1 = r(p)

test _b[1.entre_base#1.treatment] +  _b[1.entre_base] =0
local p2 = r(p)

outreg2 using tableA2.doc, append label dec(3)  ctitle(Controls) addstat(Public + Public*Business p-value, `p1', Business + Public*Business p-value, `p2')


*===============================================================================
* EXTENSIONS
*===============================================================================
clear all
use "data_final_cleaned.dta", clear
drop if entre_consistent==.


*--- Summary Statistics (LaTeX version of Table 1) -----------------------------

// Label variables for clean table output
label variable female "Female"
label variable married1 "Married"
label variable ball_success "Performance"
label variable ball_success_2 "Performance (Forced)"
label variable investment_point "Investment in Risk"
label variable guess_value "Confidence in Task"
label variable age "Age"
label variable education "Formal Education"
label variable vocational "Vocational Training"
label variable contacts "N of Contacts"
label variable competition "Competition Entry"

local sumvars female married1 ball_success ball_success_2 ///
	investment_point guess_value age education vocational contacts competition

// Four subgroups: Salaried-Private, Entrepreneur-Private, Salaried-Public, Entrepreneur-Public
eststo sp: estpost summarize `sumvars' if treatment==0 & entre_c==0
eststo ep: estpost summarize `sumvars' if treatment==0 & entre_c==1
eststo su: estpost summarize `sumvars' if treatment==1 & entre_c==0
eststo eu: estpost summarize `sumvars' if treatment==1 & entre_c==1

esttab sp ep su eu using "tables_figures/summary_stats.csv", replace ///
	cells("mean(fmt(2)) sd(par fmt(2))") ///
	mtitles("Salaried/Private" "Entrepr/Private" "Salaried/Public" "Entrepr/Public") ///
	label nonum noobs

esttab sp ep su eu using "tables_figures/summary_stats.tex", replace ///
	cells("mean(fmt(2)) sd(par fmt(2))") booktabs ///
	mtitles("Salaried/Private" "Entrepr/Private" "Salaried/Public" "Entrepr/Public") ///
	label nonum noobs

eststo clear


*--- Extension 1: Triple Interaction — Treatment × Entrepreneur × Female -------

eststo clear

// Spec 1: No controls
eststo m1: reg competition i.treatment##i.entre_c##i.female, ///
	robust cluster(ball_room)

// Spec 2: With controls (female excluded — already in the interaction)
eststo m2: reg competition i.treatment##i.entre_c##i.female ///
	investment_point married1 education vocational age contacts area, ///
	robust cluster(ball_room)

// Export table
esttab m1 m2 using "tables_figures/ext1_triple_gender.csv", replace ///
	se star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	mtitles("No Controls" "Controls") ///
	label stats(N r2, labels("Observations" "R-squared") fmt(0 3)) noomitted nobaselevels

esttab m1 m2 using "tables_figures/ext1_triple_gender.tex", replace ///
	se star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) booktabs ///
	mtitles("No Controls" "Controls") ///
	label stats(N r2, labels("Observations" "R-squared") fmt(0 3)) noomitted nobaselevels

// Figure: competition rates by treatment × occupation × gender
preserve
collapse (mean) comp_mean=competition (sd) comp_sd=competition ///
	(count) n=competition, by(treatment entre_c female)
gen comp_se = comp_sd / sqrt(n)
gen hi = comp_mean + 1.96 * comp_se
gen lo = comp_mean - 1.96 * comp_se

// x-axis group: Private-Salaried, Private-Entrepreneur, Public-Salaried, Public-Entrepreneur
gen group = 1 if treatment==0 & entre_c==0
replace group = 2 if treatment==0 & entre_c==1
replace group = 3 if treatment==1 & entre_c==0
replace group = 4 if treatment==1 & entre_c==1

// Offset bars for male vs female
gen group_m = group - 0.15
gen group_f = group + 0.15

twoway (bar comp_mean group_m if female==0, barw(0.28) color(navy)) ///
	(bar comp_mean group_f if female==1, barw(0.28) color(cranberry)) ///
	(rcap hi lo group_m if female==0, lcolor(black)) ///
	(rcap hi lo group_f if female==1, lcolor(black)), ///
	legend(order(1 "Male" 2 "Female") rows(1) position(6)) ///
	xlabel(1 "Priv/Salaried" 2 "Priv/Entrepr" 3 "Pub/Salaried" 4 "Pub/Entrepr", angle(15)) ///
	ytitle("Share Choosing Competition") ///
	title("Competition Rates by Treatment, Occupation, and Gender") ///
	scheme(s2color) name(ext1_fig, replace)
graph export "tables_figures/ext1_gender_bars.pdf", replace
restore


*--- Extension 2: Risk Preference Heterogeneity (Median Split) -----------------

// Create median-split indicator
summ investment_point, detail
local med = r(p50)
gen high_risk = (investment_point > `med') if investment_point != .

eststo clear

// Spec 1: Low risk, no controls
eststo r1: reg competition i.treatment##i.entre_c if high_risk==0, ///
	robust cluster(ball_room)

// Spec 2: High risk, no controls
eststo r2: reg competition i.treatment##i.entre_c if high_risk==1, ///
	robust cluster(ball_room)

// Spec 3: Low risk, with controls (investment_point excluded — defines the split)
eststo r3: reg competition i.treatment##i.entre_c ///
	female married1 education vocational age contacts area ///
	if high_risk==0, robust cluster(ball_room)

// Spec 4: High risk, with controls
eststo r4: reg competition i.treatment##i.entre_c ///
	female married1 education vocational age contacts area ///
	if high_risk==1, robust cluster(ball_room)

// Export table
esttab r1 r2 r3 r4 using "tables_figures/ext2_risk_split.csv", replace ///
	se star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
	mtitles("Low Risk" "High Risk" "Low Risk + Ctrl" "High Risk + Ctrl") ///
	label stats(N r2, labels("Observations" "R-squared") fmt(0 3)) noomitted nobaselevels

esttab r1 r2 r3 r4 using "tables_figures/ext2_risk_split.tex", replace ///
	se star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) booktabs ///
	mtitles("Low Risk" "High Risk" "Low Risk + Ctrl" "High Risk + Ctrl") ///
	label stats(N r2, labels("Observations" "R-squared") fmt(0 3)) noomitted nobaselevels

// Figure: grouped bar chart of the Public × Entrepreneur interaction across subsamples
// Extract coefficients manually for a twoway plot
preserve
clear
set obs 4
gen spec = _n
gen label_str = ""
replace label_str = "Low Risk, No Ctrl" if spec==1
replace label_str = "High Risk, No Ctrl" if spec==2
replace label_str = "Low Risk, Ctrl" if spec==3
replace label_str = "High Risk, Ctrl" if spec==4

gen coef = .
gen se_coef = .

// Pull stored estimates
estimates restore r1
replace coef = _b[1.treatment#1.entre_c] if spec==1
replace se_coef = _se[1.treatment#1.entre_c] if spec==1

estimates restore r2
replace coef = _b[1.treatment#1.entre_c] if spec==2
replace se_coef = _se[1.treatment#1.entre_c] if spec==2

estimates restore r3
replace coef = _b[1.treatment#1.entre_c] if spec==3
replace se_coef = _se[1.treatment#1.entre_c] if spec==3

estimates restore r4
replace coef = _b[1.treatment#1.entre_c] if spec==4
replace se_coef = _se[1.treatment#1.entre_c] if spec==4

gen hi = coef + 1.96 * se_coef
gen lo = coef - 1.96 * se_coef

twoway (bar coef spec, barw(0.6) color(navy)) ///
	(rcap hi lo spec, lcolor(black)), ///
	xlabel(1 `""Low Risk" "No Ctrl""' 2 `""High Risk" "No Ctrl""' ///
		3 `""Low Risk" "Ctrl""' 4 `""High Risk" "Ctrl""') ///
	ytitle("Coefficient: Public × Entrepreneur") ///
	yline(0, lcolor(red) lpattern(dash)) ///
	title("Public × Entrepreneur Interaction by Risk Group") ///
	legend(off) scheme(s2color) name(ext2_fig, replace)
graph export "tables_figures/ext2_risk_coefplot.pdf", replace
restore

