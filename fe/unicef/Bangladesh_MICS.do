* Author: 	Md Zabir Hasan
* Date: 	08/01/2019
* Project: 	PHCPI
* Dataset: 	Multiple dDataset from MICS5 
* Purpose: 	Explore the Coverage and 

********************************************************************************
********************************************************************************
********************************************************************************
capture clear
capture log close 
cd "C:\Users\zabir\Documents\Dropbox\PHD\Hopkins\Work\World Bank\Bangladesh_MICS and UHS\Bangladesh_MICS Analysis"
log using "Bangladesh_MICS.log", replace
********************************************************************************
********************************************************************************
********************************************************************************

********************************************************************************
* Load the dataset in STATA:
********************************************************************************
use "C:\Users\zabir\Documents\Dropbox\PHD\Hopkins\Work\World Bank\Data\MICS\Bangladesh MICS5 2012-13\Bangladesh MICS 2012-13 SPSS Datasets\wm.dta",clear
********************************************************************************
********************************************************************************
* I will calculate the Under 5 mortality in Bangladesh from the MICS 2013
* data .

* Step- 1:
* Date of the interview: WDOI
sort WM6D-WM6Y  
* br WM6D-WM6Y 
********************************************************************************
* Time since first birth 
tab CM3, missing nolab
replace CM3 = . if CM3 == 97 | CM3 == 99
count 

* Categorize the time since last birth
tab CM3, missing
egen firstbirth = cut(CM3), at(0,5,10,15,20,25,30,40)
table firstbirth, contents(min CM3 max CM3)

tab firstbirth, missing
drop if CM3 == .

* Step- 1:
* Total number of children ever born = CEB
* Total number of children ever surviving = CSURV
* Total number of children ever dad = CDEAD
* Mother's age category = firstbirth

* Step 2: 
* Calculate mean child ever born 
egen meanborn =mean(CEB), by(firstbirth)
egen meansurvive =mean(CSURV), by(firstbirth)


sort windex5 meanborn meansurvive
br windex5 meanborn meansurvive

* Step 3: 
* Collapse the dataset by age 
* collapse (mean) meanborn meansurvive (sum) CEB CSURV [iw = wmweight], by(firstbirth) 
* gen propdead = 1- (meansurvive/meanborn)

collapse (mean) meanborn meansurvive (sum) CEB CSURV [iw = wmweight], by(firstbirth) 
gen propdead = 1- (meansurvive/meanborn)

list
 
* Step 1: 
* Input these numbers in the indirect mortality estimate excel file
* http://demographicestimation.iussp.org/sites/demographicestimation.iussp.org/files/CM_Indirect_6.xlsx

* Unfortunately, this is not also working.
* The estimation process requires model life table and Sata is not equip with it  
********************************************************************************
********************************************************************************
********************************************************************************
clear
********************************************************************************
********************************************************************************
********************************************************************************
use "C:\Users\zabir\Documents\Dropbox\PHD\Hopkins\Work\World Bank\Data\MICS\Bangladesh MICS5 2012-13\Bangladesh MICS 2012-13 SPSS Datasets\ch.dta",clear
********************************************************************************
* Children aged < 5 years with diarrhoea receiving oral rehydration salts
tab CA1, missing nolab
gen diarrhoea = 1 if CA1 == 1

* Received ORS
codebook CA4A CA4B
gen ors = 1 if CA4A == 1 | CA4B == 1

* Diarrhoea with ORS
gen diarrhoeaors = .
replace diarrhoeaors = 0 if diarrhoea == 1
replace diarrhoeaors = 1 if diarrhoea == 1 & ors == 1

tab diarrhoeaors
tab diarrhoeaors [iw = chweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab diarrhoeaors [iw = chweight] if HH7 == `x'
}
foreach x in 1 2 {
tab diarrhoeaors [iw = chweight] if HH6 == `x'
}
foreach x in 1 2 3 4 5 {
tab diarrhoeaors [iw = chweight] if windex5 == `x'
}
********************************************************************************
********************************************************************************
********************************************************************************
* Children aged < 5 years with pneumonia symptoms taken to a healthcare provider

codebook CA7 CA8 CA9 
gen cough = 1 if CA7 == 1
gen pneum = 1 if cough == 1 & CA8 == 1 & (CA9 ==1 | CA9 == 3)

gen careseeking = 1 if CA11A == "A" | CA11B == "B" | CA11C == "C" | CA11D == "D" | ///
                       CA11E == "E" | CA11H == "H" | CA11I == "I" | CA11J == "J" | ///
                       CA11L == "L" | CA11O == "O"
				
gen pneumcare = 0 if pneum == 1
replace  pneumcare = 1 if careseeking == 1	

tab pneumcare
tab pneumcare [iw = chweight]
				   
********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab pneumcare [iw = chweight] if HH7 == `x'
}
foreach x in 1 2 {
tab pneumcare [iw = chweight] if HH6 == `x'
}
foreach x in 1 2 3 4 5 {
tab pneumcare [iw = chweight] if windex5 == `x'
}
********************************************************************************
clear
********************************************************************************
use "C:\Users\zabir\Documents\Dropbox\PHD\Hopkins\Work\World Bank\Data\MICS\Bangladesh MICS5 2012-13\Bangladesh MICS 2012-13 SPSS Datasets\wm.dta",clear
********************************************************************************
* Antental care coverage: 

* Codebook 
codebook MN1-MN3
foreach var in MN1 MN2A MN2B MN2C MN2F MN2G MN2X MN3 {
tab `var', missing
}

gen preglast2y = 1 if MN1 == 1 | MN1 == 2
tab preglast2y, missing
********************************************************************************
gen anc4byany = . 
replace anc4byany = 1 if MN1 == 1 &  (MN2A == "A" | MN2B == "B" | MN2C == "C" | ///
                                      MN2F == "F" | MN2G == "G" | MN2X == "X") & ///
                    (WB2 >= 15 & WB2 <= 49) & (MN3 > 3 & MN3 < 50)

tab anc4byany, missing
tab anc4byany preglast2y  [iw = wmweight], missing row col
********************************************************************************
gen anc4prop = 0 if preglast2y == 1
replace anc4prop = 1 if preglast2y == 1 & anc4byany == 1
tab anc4prop
tab anc4prop  [iw = wmweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab anc4prop [iw = wmweight] if HH7 == `x'
}
foreach x in 1 2 {
tab anc4prop [iw = wmweight] if HH6 == `x'
}
foreach x in 1 2 3 4 5 {
tab anc4prop [iw = wmweight] if windex5 == `x'
}
********************************************************************************
********************************************************************************
********************************************************************************
foreach x in 1 2 3 4 5 6 7 {
tab anc4prop [iw = wmweight] if WAGE == `x'
}

********************************************************************************
* Inequity in ANC visit contents 

codebook MN4A MN4B MN4C MN1

gen anccontent = 0 if preglast2y == 1
replace anccontent = 1 if preglast2y == 1 & (MN4A == 1 & MN4B == 1 & MN4C == 1)

tab anccontent, missing
tab anccontent
tab anccontent [iw = wmweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab anccontent [iw = wmweight] if HH7 == `x'
}
foreach x in 1 2 {
tab anccontent [iw = wmweight] if HH6 == `x'
}

foreach x in 1 2 3 4 5 {
tab anccontent [iw = wmweight] if windex5 == `x'
}

di 74.64 - 15.32 

foreach x in 1 2 3 4 5 6 7 {
tab anccontent [iw = wmweight] if WAGE == `x'
}
********************************************************************************
clear
********************************************************************************
use "C:\Users\zabir\Documents\Dropbox\PHD\Hopkins\Work\World Bank\Data\MICS\Bangladesh MICS5 2012-13\Bangladesh MICS 2012-13 SPSS Datasets\hh.dta",clear
********************************************************************************
* Use of Mosquito Net: 


codebook HC16 HC17A

gen mosquitonetprop = . 
replace mosquitonetprop = 0 if HC16 == 1
replace mosquitonetprop = 1 if HC16 == 1 & HC17A == "A"
tab mosquitonetprop
tab mosquitonetprop  [iw = hhweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab mosquitonetprop [iw = hhweight] if HH7 == `x'
}
foreach x in 1 2 {
tab mosquitonetprop [iw = hhweight] if HH6 == `x'
}
foreach x in 1 2 3 4 5 {
tab mosquitonetprop [iw = hhweight] if windex5 == `x'
}
********************************************************************************
* This measure is not exactly as the PHCPI indicator
********************************************************************************
********************************************************************************
clear
********************************************************************************
use "C:\Users\zabir\Documents\Dropbox\PHD\Hopkins\Work\World Bank\Data\MICS\Bangladesh MICS5 2012-13\Bangladesh MICS 2012-13 SPSS Datasets\wm.dta",clear
********************************************************************************
/*
********************************************************************************
g unmet=.
**Set unmet need to NA for unmarried women if survey only included ever-married women 
tab MA1 MA5, missing
replace unmet=98 if MA1!=1 & (MA5==1)

tab unmet
********************************************************************************

** CONTRACEPTIVE USERS - GROUP 1
* using to limit if wants no more, sterilized, or declared infecund
codebook UN4 UN6
codebook CP3A-CP3X

gen currentlyusing = 1 if CP3C == "C" | CP3D == "D" | CP3E == "E" | CP3F == "F" ///
                        | CP3G == "G" | CP3H == "H" | CP3I == "I" | CP3J == "J" ///
						| CP3K == "K" | CP3L == "L" | CP3M == "M" | CP3X == "X"

recode unmet .= 4 if (UN4 == 2 | UN6 == 2) & currentlyusing == 1 & ///
                    (CP3A == "A" | CP3B == "B")

* using to space - all other contraceptive users
recode unmet .=3 if (UN4 != 2 | UN6 != 2) &  currentlyusing == 1 

********************************************************************************
********************************************************************************
********************************************************************************
* This will require more time to adpat the code from DHS to MICS the following
* of the code is from DHS. 
********************************************************************************
********************************************************************************
********************************************************************************
		
* DHS code 
/* 	Stata program to create Revised unmet need variable as described in 
	Analytical Study 25: Revising Unmet Need for Family Planning
	by Bradley, Croft, Fishel, and Westoff, 2012, published by ICF International
	measuredhs.com/pubs/pdf/AS25/AS25.pdf
** 	Program written by Sarah Bradley and edited by Trevor Croft, last updated 23 January 2011
	SBradley@icfi.com
** 	This program will work for most surveys. If your results do not match 
	Revising Unmet Need for Family Planning, the survey you are analyzing may require 
	survey-specific programming. See survey-specific link at measuredhs.com/topics/Unmet-Need.cfm */
**  Correction 10 May 2017 to work with DHS7 datasets.  
**  Two changes: checks for v000 now look for "7", and tsinceb now calculated using v222 which is based on century day codes in DHS7

g unmet=.
**Set unmet need to NA for unmarried women if survey only included ever-married women 
replace unmet=98 if v502!=1 & (v020==1)

** CONTRACEPTIVE USERS - GROUP 1
* using to limit if wants no more, sterilized, or declared infecund
recode unmet .=4 if v312!=0 & (v605>=5 & v605<=7)
* using to space - all other contraceptive users
recode unmet .=3 if v312!=0

**PREGNANT or POSTPARTUM AMENORRHEIC (PPA) WOMEN - GROUP 2
* Determine who should be in Group 2
* generate time since last birth
g tsinceb=v222
* generate time since last period in months from v215
g tsincep	    =	int((v215-100)/30) 	if v215>=100 & v215<=190
replace tsincep =   int((v215-200)/4.3) if v215>=200 & v215<=290
replace tsincep =   (v215-300) 			if v215>=300 & v215<=390
replace tsincep =	(v215-400)*12 		if v215>=400 & v215<=490
* initialize pregnant or postpartum amenorrheic (PPA) women
g pregPPA=1 if v213==1 | m6_1==96
* For women with missing data or "period not returned" on date of last menstrual period, use information from time since last period
* 	if last period is before last birth in last 5 years
replace pregPPA=1 if (m6_1==. | m6_1==99 | m6_1==97) & tsincep> tsinceb & tsinceb<60 & tsincep!=. & tsinceb!=.
* 	or if said "before last birth" to time since last period in the last 5 years
replace pregPPA=1 if (m6_1==. | m6_1==99 | m6_1==97) & v215==995 & tsinceb<60 & tsinceb!=.
* select only women who are pregnant or PPA for <24 months
g pregPPA24=1 if v213==1 | (pregPPA==1 & tsinceb<24)

* Classify based on wantedness of current pregnancy/last birth
* current pregnancy
g wantedlast=v225
* last birth
replace wantedlast = m10_1 if (wantedlast==. | wantedlast==9) & v213!=1
* no unmet need if wanted current pregnancy/last birth then/at that time
recode unmet .=7  if pregPPA24==1 & wantedlast==1
* unmet need for spacing if wanted current pregnancy/last birth later
recode unmet .=1  if pregPPA24==1 & wantedlast==2
* unmet need for limiting if wanted current pregnancy/last birth not at all
recode unmet .=2  if pregPPA24==1 & wantedlast==3
* missing=missing
recode unmet .=99 if pregPPA24==1 & (wantedlast==. | wantedlast==9)

**NO NEED FOR UNMARRIED WOMEN WHO ARE NOT SEXUALLY ACTIVE
* determine if sexually active in last 30 days
g sexact=1 if v528>=0 & v528<=30
* if unmarried and not sexually active in last 30 days, assume no need
recode unmet .=97 if v502!=1 & sexact!=1

**DETERMINE FECUNDITY - GROUP 3 (Boxes refer to Figure 2 flowchart in report)
**Box 1 - applicable only to currently married
* married 5+ years ago, no children in past 5 years, never used contraception, excluding pregnant and PPA <24 months
g infec=1 			if v502==1 & v512>=5 & v512!=. & (tsinceb>59 | tsinceb==.) & v302==0  & pregPPA24!=1
* in DHS VI, v302 replaced by v302a
cap replace infec=1 if v502==1 & v512>=5 & v512!=. & (tsinceb>59 | tsinceb==.) & v302a==0 & pregPPA24!=1 & (substr(v000,3,1)=="6" | substr(v000,3,1)=="7")
**Box 2
* declared infecund on future desires for children
replace infec=1 if v605==7
**Box 3
* menopausal/hysterectomy on reason not using contraception - slightly different recoding in DHS III and IV+
* DHS IV+ surveys
cap replace infec=1 if 	v3a08d==1 & (substr(v000,3,1)=="4" | substr(v000,3,1)=="5" | substr(v000,3,1)=="6" | substr(v000,3,1)=="7")
* DHSIII surveys
cap replace infec=1 if  v375a==23 & (substr(v000,3,1)=="3" | substr(v000,3,1)=="T")
* reason not using did not exist in DHSII, use reason not intending to use in future
cap replace infec=1 if  v376==14 & substr(v000,3,1)=="2"
**Box 4
* Time since last period is >=6 months and not PPA
replace infec=1 if tsincep>=6 & tsincep!=. & pregPPA!=1
**Box 5
* menopausal/hysterectomy on time since last period
replace infec=1 if v215==994
* never menstruated on time since last period, unless had a birth in the last 5 years
replace infec=1 if v215==996 & (tsinceb>59 | tsinceb==.)
**Box 6
* time since last birth>= 60 months and last period was before last birth
replace infec=1 if v215==995 & tsinceb>=60 & tsinceb!=.
* never had a birth, but last period reported as before last birth - assume code should have been 994 or 996
replace infec=1 if v215==995 & tsinceb==.
* exclude pregnant and PP amenorrheic < 24 months
replace infec=. if pregPPA24==1
recode unmet .=9 if infec==1

**FECUND WOMEN - GROUP 4
* wants within 2 years
recode unmet .=7 if v605==1
* wants in 2+ years, wants undecided timing, or unsure if wants
recode unmet .=1 if v605>=2 & v605<=4
* wants no more
recode unmet .=2 if v605==5
recode unmet .=99
  la def unmet ///
    1 "unmet need for spacing" ///
	2 "unmet need for limiting" ///
	3 "using for spacing" ///
	4 "using for limiting" ///
	7 "no unmet need" ///
	9 "infecund or menopausal" ///
	97 "not sexually active" ///
	98 "unmarried - EM sample or no data" ///
	99 "missing"
  la val unmet unmet
recode unmet (1/2=1 "unmet need") (else=0 "no unmet need"), g(unmettot)

**TABULATE RESULTS
* generate sampling weight
g wgt=v005/1000000
* tabulate for currently married women
ta unmet if v502==1 [iw=wgt], m
ta unmettot if v502==1 [iw=wgt]
* all women
ta unmet [iw=wgt]
ta unmettot [iw=wgt]
* sexually active unmarried
ta unmet if v502!=1 & sexact==1 [iw=wgt]
ta unmettot if v502!=1 & sexact==1 [iw=wgt]
********************************************************************************
*/
********************************************************************************
* Assistance at Delivery: 
tab CM13, missing

gen preglast2y = 1 if CM13 == "Y"
tab preglast2y, missing

gen sba = . 
replace sba = 0 if preglast2y == 1
replace sba = 1 if MN17A == "A" | MN17B == "B" | MN17C == "C"
		
		

tab sba, missing
tab sba
tab sba [iw = wmweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab sba [iw = wmweight] if HH7 == `x'
}

foreach x in 1 2 {
tab sba [iw = wmweight] if HH6 == `x'
}

foreach x in 1 2 3 4 5 {
tab sba [iw = wmweight] if windex5 == `x'
}

foreach x in 1 2 3 4 5 6 7 {
tab sba [iw = wmweight] if WAGE == `x'
}

********************************************************************************
* Delivered by C-section: 
tab MN19, missing nolab

gen csec = . 
replace csec = 0 if preglast2y == 1
replace csec = 1 if MN19 == 1

tab csec, missing
tab csec
tab csec [iw = wmweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab csec [iw = wmweight] if HH7 == `x'
}

foreach x in 1 2 {
tab csec [iw = wmweight] if HH6 == `x'
}

foreach x in 1 2 3 4 5 {
tab csec [iw = wmweight] if windex5 == `x'
}

foreach x in 1 2 3 4 5 6 7 {
tab csec [iw = wmweight] if WAGE == `x'
}
********************************************************************************
********************************************************************************
* Post natal check-up: 

tab PN21U
tab PN21U, nolab

tab PN21N if PN21U == 1
tab PN21N if PN21U == 2

gen pnc = 0 if preglast2y == 1
replace pnc = 1 if ((PN21N < 48 & PN21U == 1) | (PN21N <= 2 & PN21U == 2))
tab pnc
tab pnc [iw = wmweight]

********************************************************************************
* Stratification: 
codebook HH7 HH6 windex5

foreach x in 10 20 30 40 50 55 60 {
tab pnc [iw = wmweight] if HH7 == `x'
}

foreach x in 1 2 {
tab pnc [iw = wmweight] if HH6 == `x'
}

foreach x in 1 2 3 4 5 {
tab pnc [iw = wmweight] if windex5 == `x'
}

foreach x in 1 2 3 4 5 6 7 {
tab pnc [iw = wmweight] if WAGE == `x'
}
********************************************************************************
* Currently Married: 
tab MA1 [iw = wmweight]


foreach x in 1 2 3 4 5 6 7 {
tab MA1 [iw = wmweight] if WAGE == `x'
}
********************************************************************************
* Children ever bourn
tab CEB, missing
gen child = 0
replace child = 1 if CEB > 0 & CEB != . 
tab child

tab child [iw = wmweight]

foreach x in 1 2 3 4 5 6 7 {
tab child [iw = wmweight] if WAGE == `x'
}
********************************************************************************
* Gave birth within last two years 

gen lastbirth2y = 0
replace lastbirth2y = 1 if preglast2y == 1
tab lastbirth2y [iw = wmweight]

foreach x in 1 2 3 4 5 6 7 {
tab lastbirth2y [iw = wmweight] if WAGE == `x'
}
********************************************************************************
********************************************************************************
* I have calculated to stratifies numbers. However, collapsing the indicators 
* across divisions or district will produce the same result with the dataset. 
* Please use the weight when collapsing the data across different strata
********************************************************************************
clear
