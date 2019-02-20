infix using BDSQ35FL.dct

label variable gsqdiv   "Division number"
label variable gsqdistr "District number"
label variable gsqclust "Cluster number"
label variable gsqtype  "Type of area for thana"
label variable gsqint   "Interviewer number"
label variable gsqintd  "Day of interview"
label variable gsqintm  "Month of interview"
label variable gsqinty  "Year of interview (English)"
label variable gsqkeyer "Keyer number"
label variable gsqdai   "Total of Dai questionnaires"
label variable gsqdpl   "Total of Deliv.pers. q'aires"
label variable gsqfpp   "Total of FP pract'r. q'aires"
label variable gsqfwv   "Total of FWV questionnaires"
label variable gsqfwa   "Total of FWA questionnaires"
label variable totresp  "Sum of individual questionnair"
label variable gline_01 "Health personnel quest.ID"
label variable gptyp_01 "Type of questionnaire"
label variable gline_02 "Health personnel quest.ID"
label variable gptyp_02 "Type of questionnaire"
label variable gline_03 "Health personnel quest.ID"
label variable gptyp_03 "Type of questionnaire"
label variable gline_04 "Health personnel quest.ID"
label variable gptyp_04 "Type of questionnaire"
label variable gline_05 "Health personnel quest.ID"
label variable gptyp_05 "Type of questionnaire"
label variable gline_06 "Health personnel quest.ID"
label variable gptyp_06 "Type of questionnaire"
label variable gline_07 "Health personnel quest.ID"
label variable gptyp_07 "Type of questionnaire"
label variable gline_08 "Health personnel quest.ID"
label variable gptyp_08 "Type of questionnaire"
label variable gline_09 "Health personnel quest.ID"
label variable gptyp_09 "Type of questionnaire"
label variable gline_10 "Health personnel quest.ID"
label variable gptyp_10 "Type of questionnaire"
label variable gline_11 "Health personnel quest.ID"
label variable gptyp_11 "Type of questionnaire"
label variable gline_12 "Health personnel quest.ID"
label variable gptyp_12 "Type of questionnaire"
label variable gline_13 "Health personnel quest.ID"
label variable gptyp_13 "Type of questionnaire"
label variable gline_14 "Health personnel quest.ID"
label variable gptyp_14 "Type of questionnaire"
label variable gsq01    "Dist. to thana hdqtrs (miles)"
label variable gsq02    "Dist.to district hdqtrs(miles)"
label variable gsq03    "Mothers club/LA in village"
label variable gsq04    "Grameen bank in village"
label variable gsq05    "BSIC Industries in village"
label variable gsq06    "Co-operative soc. in village"
label variable gsq07    "Inc.gen NGO's in village"
label variable gsq08    "Comm. television in village"
label variable gsq09    "Prop.of HH's living in 1 room"
label variable gsq10    "Prop.of HH's in non pukka hses"
label variable gsq11_1  "Facility in village/miles"
label variable gsq11_2  "Facility in village/miles"
label variable gsq11_3  "Facility in village/miles"
label variable gsq11_4  "Facility in village/miles"
label variable gsq11_5  "Facility in village/miles"
label variable gsq11_6  "Facility in village/miles"
label variable gsq11_7  "Facility in village/miles"
label variable gsq12    "FPW visits village"
label variable gsq13a   "FPW prov. info. on Fmly.plng"
label variable gsq13b   "FPW prov. meths of Fmly.plng"
label variable gsq13c   "FPW helps at satellite clinic"
label variable gsq13d   "FPW takes wmn.to clinic/hosp."
label variable gsq13e   "FPW takes child.for immuniztn."
label variable gsq13f   "FPW prov. vit.A caps.for child"
label variable gsq15    "How long FPW worked in village"
label variable gsq16    "Another FPW before current one"
label variable gsq17    "How long ago did 1st FPW start"
label variable gsq18    "Is there health wkr in village"
label variable gsq19a   "HW provides health information"
label variable gsq19b   "HW provides immunizations"
label variable gsq19c   "HW provides ORS packets"
label variable gsq19d   "HW provides vit.A capsules"
label variable gsq20a   "Other FPW/hlth.wkr in village"
label variable gsq21    "Services prov.by other HWkr"
label variable gsq22    "Anyone who sells FP meths./hse"
label variable gsq23    "Methods sold from house"
label variable gsq24    "Satellite clinic in village"
label variable gsq25a   "Sat.clinic held nearby"
label variable gsq25b   "Distance to nearby sat.clinic"
label variable gsq26b   "Services avail.frm.satellite"
label variable gsq27u   "Freq. of sat.clinics : units"
label variable gsq27n   "Freq. of sat.clinics : number"
label variable gsq28    "Dist.to nearest family w.centr"
label variable gsq29    "Dist.to nrst hosp/thana HC"

#delimit ;
label define GSQTYPE 
     1 "Dhaka/Chittagong"
     2 "Small city"
     3 "Town"
     4 "Village"
;
label define GPTYPE  
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
;
label define GSQ01   
    97 "97 miles or more"
    98 "Don't know"
    99 "Missing"
;
label define GSQ02   
    97 "97 miles or more"
    98 "Don't know"
;
label define GSQ03   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ04   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ05   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ06   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ07   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ08   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ09   
     1 "All/almost all"
     2 "Most/more than half"
     3 "Less than half"
     4 "Very few/none"
     9 "Missing"
;
label define GSQ10   
     1 "All/almost all"
     2 "Most/more than half"
     3 "Less than half"
     4 "Very few/none"
     9 "Missing"
;
label define GSQ11   
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
    99 "Missing"
;
label define GSQ12   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ13A  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ13B  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ13C  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ13D  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ13E  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ13F  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ15   
     0 "Less than 1 year"
    98 "DK"
    99 "Missing"
;
label define GSQ16   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ17   
     0 "Less than 1 year"
    98 "DK"
    99 "Missing"
;
label define GSQ18   
     1 "Yes"
     2 "No"
     9 "Missing"
;
label define GSQ19A  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ19B  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ19C  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ19D  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ20A  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ22   
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ24   
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ25A  
     1 "Yes"
     2 "No"
     8 "DK"
     9 "Missing"
;
label define GSQ25B  
    97 "97 miles or more"
    98 "Don't know"
    99 "Missing"
;
label define GSQ27U  
     1 "Per month"
     2 "Per year"
     9 "Special answers"
;
label define GSQ27N  
    99 "Missing value"
;
label define GSQ28   
    97 "97 miles or more"
    98 "Don't know"
    99 "Missing"
;
label define GSQ29   
    97 "97 miles or more"
    98 "Don't know"
    99 "Missing"
;

#delimit cr
label values gsqtype  GSQTYPE 
label values gptyp_01 GPTYPE  
label values gptyp_02 GPTYPE  
label values gptyp_03 GPTYPE  
label values gptyp_04 GPTYPE  
label values gptyp_05 GPTYPE  
label values gptyp_06 GPTYPE  
label values gptyp_07 GPTYPE  
label values gptyp_08 GPTYPE  
label values gptyp_09 GPTYPE  
label values gptyp_10 GPTYPE  
label values gptyp_11 GPTYPE  
label values gptyp_12 GPTYPE  
label values gptyp_13 GPTYPE  
label values gptyp_14 GPTYPE  
label values gsq01    GSQ01   
label values gsq02    GSQ02   
label values gsq03    GSQ03   
label values gsq04    GSQ04   
label values gsq05    GSQ05   
label values gsq06    GSQ06   
label values gsq07    GSQ07   
label values gsq08    GSQ08   
label values gsq09    GSQ09   
label values gsq10    GSQ10   
label values gsq11_1  GSQ11   
label values gsq11_2  GSQ11   
label values gsq11_3  GSQ11   
label values gsq11_4  GSQ11   
label values gsq11_5  GSQ11   
label values gsq11_6  GSQ11   
label values gsq11_7  GSQ11   
label values gsq12    GSQ12   
label values gsq13a   GSQ13A  
label values gsq13b   GSQ13B  
label values gsq13c   GSQ13C  
label values gsq13d   GSQ13D  
label values gsq13e   GSQ13E  
label values gsq13f   GSQ13F  
label values gsq15    GSQ15   
label values gsq16    GSQ16   
label values gsq17    GSQ17   
label values gsq18    GSQ18   
label values gsq19a   GSQ19A  
label values gsq19b   GSQ19B  
label values gsq19c   GSQ19C  
label values gsq19d   GSQ19D  
label values gsq20a   GSQ20A  
label values gsq22    GSQ22   
label values gsq24    GSQ24   
label values gsq25a   GSQ25A  
label values gsq25b   GSQ25B  
label values gsq27u   GSQ27U  
label values gsq27n   GSQ27N  
label values gsq28    GSQ28   
label values gsq29    GSQ29   
