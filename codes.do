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
