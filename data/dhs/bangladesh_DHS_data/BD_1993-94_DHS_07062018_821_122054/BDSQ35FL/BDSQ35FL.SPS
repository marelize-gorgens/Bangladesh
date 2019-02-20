DATA LIST FILE='BDSQ35FL.DAT' RECORDS=1
/
 GSQDIV      1-1   
 GSQDISTR    2-3   
 GSQCLUST    4-6   
 GSQTYPE     7-7   
 GSQINT      8-10  
 GSQINTD    11-12  
 GSQINTM    13-14  
 GSQINTY    15-18  
 GSQKEYER   19-20  
 GSQDAI     21-22  
 GSQDPL     23-24  
 GSQFPP     25-26  
 GSQFWV     27-28  
 GSQFWA     29-30  
 TOTRESP    31-32  
 GLINE$01   33-34  
 GPTYP$01   35-37  
 GLINE$02   38-39  
 GPTYP$02   40-42  
 GLINE$03   43-44  
 GPTYP$03   45-47  
 GLINE$04   48-49  
 GPTYP$04   50-52  
 GLINE$05   53-54  
 GPTYP$05   55-57  
 GLINE$06   58-59  
 GPTYP$06   60-62  
 GLINE$07   63-64  
 GPTYP$07   65-67  
 GLINE$08   68-69  
 GPTYP$08   70-72  
 GLINE$09   73-74  
 GPTYP$09   75-77  
 GLINE$10   78-79  
 GPTYP$10   80-82  
 GLINE$11   83-84  
 GPTYP$11   85-87  
 GLINE$12   88-89  
 GPTYP$12   90-92  
 GLINE$13   93-94  
 GPTYP$13   95-97  
 GLINE$14   98-99  
 GPTYP$14  100-102 
 GSQ01     103-104 
 GSQ02     105-106 
 GSQ03     107-107 
 GSQ04     108-108 
 GSQ05     109-109 
 GSQ06     110-110 
 GSQ07     111-111 
 GSQ08     112-112 
 GSQ09     113-113 
 GSQ10     114-114 
 GSQ11$1   115-116 
 GSQ11$2   117-118 
 GSQ11$3   119-120 
 GSQ11$4   121-122 
 GSQ11$5   123-124 
 GSQ11$6   125-126 
 GSQ11$7   127-128 
 GSQ12     129-129 
 GSQ13A    130-130 
 GSQ13B    131-131 
 GSQ13C    132-132 
 GSQ13D    133-133 
 GSQ13E    134-134 
 GSQ13F    135-135 
 GSQ15     136-137 
 GSQ16     138-138 
 GSQ17     139-140 
 GSQ18     141-141 
 GSQ19A    142-142 
 GSQ19B    143-143 
 GSQ19C    144-144 
 GSQ19D    145-145 
 GSQ20A    146-146 
 GSQ21     147-154  (A)
 GSQ22     155-155 
 GSQ23     156-159  (A)
 GSQ24     160-160 
 GSQ25A    161-161 
 GSQ25B    162-163 
 GSQ26B    164-173  (A)
 GSQ27U    174-174 
 GSQ27N    175-176 
 GSQ28     177-178 
 GSQ29     179-180 
.
VARIABLE LABELS
 GSQDIV   "Division number"
/GSQDISTR "District number"
/GSQCLUST "Cluster number"
/GSQTYPE  "Type of area for thana"
/GSQINT   "Interviewer number"
/GSQINTD  "Day of interview"
/GSQINTM  "Month of interview"
/GSQINTY  "Year of interview (English)"
/GSQKEYER "Keyer number"
/GSQDAI   "Total of Dai questionnaires"
/GSQDPL   "Total of Deliv.pers. q'aires"
/GSQFPP   "Total of FP pract'r. q'aires"
/GSQFWV   "Total of FWV questionnaires"
/GSQFWA   "Total of FWA questionnaires"
/TOTRESP  "Sum of individual questionnair"
/GLINE$01 "Health personnel quest.ID"
/GPTYP$01 "Type of questionnaire"
/GLINE$02 "Health personnel quest.ID"
/GPTYP$02 "Type of questionnaire"
/GLINE$03 "Health personnel quest.ID"
/GPTYP$03 "Type of questionnaire"
/GLINE$04 "Health personnel quest.ID"
/GPTYP$04 "Type of questionnaire"
/GLINE$05 "Health personnel quest.ID"
/GPTYP$05 "Type of questionnaire"
/GLINE$06 "Health personnel quest.ID"
/GPTYP$06 "Type of questionnaire"
/GLINE$07 "Health personnel quest.ID"
/GPTYP$07 "Type of questionnaire"
/GLINE$08 "Health personnel quest.ID"
/GPTYP$08 "Type of questionnaire"
/GLINE$09 "Health personnel quest.ID"
/GPTYP$09 "Type of questionnaire"
/GLINE$10 "Health personnel quest.ID"
/GPTYP$10 "Type of questionnaire"
/GLINE$11 "Health personnel quest.ID"
/GPTYP$11 "Type of questionnaire"
/GLINE$12 "Health personnel quest.ID"
/GPTYP$12 "Type of questionnaire"
/GLINE$13 "Health personnel quest.ID"
/GPTYP$13 "Type of questionnaire"
/GLINE$14 "Health personnel quest.ID"
/GPTYP$14 "Type of questionnaire"
/GSQ01    "Dist. to thana hdqtrs (miles)"
/GSQ02    "Dist.to district hdqtrs(miles)"
/GSQ03    "Mothers club/LA in village"
/GSQ04    "Grameen bank in village"
/GSQ05    "BSIC Industries in village"
/GSQ06    "Co-operative soc. in village"
/GSQ07    "Inc.gen NGO's in village"
/GSQ08    "Comm. television in village"
/GSQ09    "Prop.of HH's living in 1 room"
/GSQ10    "Prop.of HH's in non pukka hses"
/GSQ11$1  "Facility in village/miles"
/GSQ11$2  "Facility in village/miles"
/GSQ11$3  "Facility in village/miles"
/GSQ11$4  "Facility in village/miles"
/GSQ11$5  "Facility in village/miles"
/GSQ11$6  "Facility in village/miles"
/GSQ11$7  "Facility in village/miles"
/GSQ12    "FPW visits village"
/GSQ13A   "FPW prov. info. on Fmly.plng"
/GSQ13B   "FPW prov. meths of Fmly.plng"
/GSQ13C   "FPW helps at satellite clinic"
/GSQ13D   "FPW takes wmn.to clinic/hosp."
/GSQ13E   "FPW takes child.for immuniztn."
/GSQ13F   "FPW prov. vit.A caps.for child"
/GSQ15    "How long FPW worked in village"
/GSQ16    "Another FPW before current one"
/GSQ17    "How long ago did 1st FPW start"
/GSQ18    "Is there health wkr in village"
/GSQ19A   "HW provides health information"
/GSQ19B   "HW provides immunizations"
/GSQ19C   "HW provides ORS packets"
/GSQ19D   "HW provides vit.A capsules"
/GSQ20A   "Other FPW/hlth.wkr in village"
/GSQ21    "Services prov.by other HWkr"
/GSQ22    "Anyone who sells FP meths./hse"
/GSQ23    "Methods sold from house"
/GSQ24    "Satellite clinic in village"
/GSQ25A   "Sat.clinic held nearby"
/GSQ25B   "Distance to nearby sat.clinic"
/GSQ26B   "Services avail.frm.satellite"
/GSQ27U   "Freq. of sat.clinics : units"
/GSQ27N   "Freq. of sat.clinics : number"
/GSQ28    "Dist.to nearest family w.centr"
/GSQ29    "Dist.to nrst hosp/thana HC"
.
MISSING VALUE
 GSQ01    (99)
/GSQ03    (9)
/GSQ04    (9)
/GSQ05    (9)
/GSQ06    (9)
/GSQ07    (9)
/GSQ08    (9)
/GSQ09    (9)
/GSQ10    (9)
/GSQ11$1  (99)
/GSQ11$2  (99)
/GSQ11$3  (99)
/GSQ11$4  (99)
/GSQ11$5  (99)
/GSQ11$6  (99)
/GSQ11$7  (99)
/GSQ12    (9)
/GSQ13A   (9)
/GSQ13B   (9)
/GSQ13C   (9)
/GSQ13D   (9)
/GSQ13E   (9)
/GSQ13F   (9)
/GSQ15    (99)
/GSQ16    (9)
/GSQ17    (99)
/GSQ18    (9)
/GSQ19A   (9)
/GSQ19B   (9)
/GSQ19C   (9)
/GSQ19D   (9)
/GSQ20A   (9)
/GSQ22    (9)
/GSQ24    (9)
/GSQ25A   (9)
/GSQ25B   (99)
/GSQ28    (99)
/GSQ29    (99)
.
VALUE LABELS
 GSQTYPE 
     1 "Dhaka/Chittagong"
     2 "Small city"
     3 "Town"
     4 "Village"
/GPTYP$01
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$02
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$03
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$04
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$05
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$06
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$07
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$08
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$09
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$10
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$11
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$12
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$13
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GPTYP$14
     1 "Dai"
     2 "Delivery personnel"
     3 "Family pl.physician"
     4 "Family Welfare Vstor"
     5 "Family Welfare Asst."
/GSQ01   
    97 "97 miles or more"
    98 "Don't know"
/GSQ02   
    97 "97 miles or more"
    98 "Don't know"
/GSQ03   
     1 "Yes"
     2 "No"
/GSQ04   
     1 "Yes"
     2 "No"
/GSQ05   
     1 "Yes"
     2 "No"
/GSQ06   
     1 "Yes"
     2 "No"
/GSQ07   
     1 "Yes"
     2 "No"
/GSQ08   
     1 "Yes"
     2 "No"
/GSQ09   
     1 "All/almost all"
     2 "Most/more than half"
     3 "Less than half"
     4 "Very few/none"
/GSQ10   
     1 "All/almost all"
     2 "Most/more than half"
     3 "Less than half"
     4 "Very few/none"
/GSQ11$1 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ11$2 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ11$3 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ11$4 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ11$5 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ11$6 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ11$7 
     0 "In village"
    97 "97 miles or more"
    98 "Don't know distance"
/GSQ12   
     1 "Yes"
     2 "No"
/GSQ13A  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ13B  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ13C  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ13D  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ13E  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ13F  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ15   
     0 "Less than 1 year"
    98 "DK"
/GSQ16   
     1 "Yes"
     2 "No"
/GSQ17   
     0 "Less than 1 year"
    98 "DK"
/GSQ18   
     1 "Yes"
     2 "No"
/GSQ19A  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ19B  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ19C  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ19D  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ20A  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ22   
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ24   
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ25A  
     1 "Yes"
     2 "No"
     8 "DK"
/GSQ25B  
    97 "97 miles or more"
    98 "Don't know"
/GSQ27U  
     1 "Per month"
     2 "Per year"
     9 "Special answers"
/GSQ27N  
    99 "Missing value"
/GSQ28   
    97 "97 miles or more"
    98 "Don't know"
/GSQ29   
    97 "97 miles or more"
    98 "Don't know"
.
