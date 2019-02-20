libname user ' ';
Proc format;
  value GSQTYPE_
     1 = "Dhaka/Chittagong"
     2 = "Small city"
     3 = "Town"
     4 = "Village"
     ;
  value GPTYPE_
     1 = "Dai"
     2 = "Delivery personnel"
     3 = "Family pl.physician"
     4 = "Family Welfare Vstor"
     5 = "Family Welfare Asst."
     ;
  value GSQ01_
    97 = "97 miles or more"
    98 = "Don't know"
     ;
  value GSQ02_
    97 = "97 miles or more"
    98 = "Don't know"
     ;
  value GSQ03_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ04_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ05_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ06_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ07_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ08_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ09_
     1 = "All/almost all"
     2 = "Most/more than half"
     3 = "Less than half"
     4 = "Very few/none"
     ;
  value GSQ10_
     1 = "All/almost all"
     2 = "Most/more than half"
     3 = "Less than half"
     4 = "Very few/none"
     ;
  value GSQ11_
     0 = "In village"
    97 = "97 miles or more"
    98 = "Don't know distance"
     ;
  value GSQ12_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ13A_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ13B_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ13C_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ13D_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ13E_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ13F_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ15_
     0 = "Less than 1 year"
    98 = "DK"
     ;
  value GSQ16_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ17_
     0 = "Less than 1 year"
    98 = "DK"
     ;
  value GSQ18_
     1 = "Yes"
     2 = "No"
     ;
  value GSQ19A_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ19B_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ19C_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ19D_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ20A_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ22_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ24_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ25A_
     1 = "Yes"
     2 = "No"
     8 = "DK"
     ;
  value GSQ25B_
    97 = "97 miles or more"
    98 = "Don't know"
     ;
  value GSQ27U_
     1 = "Per month"
     2 = "Per year"
     9 = "Special answers"
     ;
  value GSQ27N_
    99 = "Missing value"
     ;
  value GSQ28_
    97 = "97 miles or more"
    98 = "Don't know"
     ;
  value GSQ29_
    97 = "97 miles or more"
    98 = "Don't know"
     ;
  attrib GSQDIV   length=4 label="Division number";
  attrib GSQDISTR length=4 label="District number";
  attrib GSQCLUST length=4 label="Cluster number";
  attrib GSQTYPE  length=4 format=GSQTYPE_. label="Type of area for thana";
  attrib GSQINT   length=4 label="Interviewer number";
  attrib GSQINTD  length=4 label="Day of interview";
  attrib GSQINTM  length=4 label="Month of interview";
  attrib GSQINTY  length=4 label="Year of interview (English)";
  attrib GSQKEYER length=4 label="Keyer number";
  attrib GSQDAI   length=4 label="Total of Dai questionnaires";
  attrib GSQDPL   length=4 label="Total of Deliv.pers. q'aires";
  attrib GSQFPP   length=4 label="Total of FP pract'r. q'aires";
  attrib GSQFWV   length=4 label="Total of FWV questionnaires";
  attrib GSQFWA   length=4 label="Total of FWA questionnaires";
  attrib TOTRESP  length=4 label="Sum of individual questionnair";
  attrib GLINE_01 length=4 label="Health personnel quest.ID";
  attrib GPTYP_01 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_02 length=4 label="Health personnel quest.ID";
  attrib GPTYP_02 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_03 length=4 label="Health personnel quest.ID";
  attrib GPTYP_03 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_04 length=4 label="Health personnel quest.ID";
  attrib GPTYP_04 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_05 length=4 label="Health personnel quest.ID";
  attrib GPTYP_05 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_06 length=4 label="Health personnel quest.ID";
  attrib GPTYP_06 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_07 length=4 label="Health personnel quest.ID";
  attrib GPTYP_07 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_08 length=4 label="Health personnel quest.ID";
  attrib GPTYP_08 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_09 length=4 label="Health personnel quest.ID";
  attrib GPTYP_09 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_10 length=4 label="Health personnel quest.ID";
  attrib GPTYP_10 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_11 length=4 label="Health personnel quest.ID";
  attrib GPTYP_11 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_12 length=4 label="Health personnel quest.ID";
  attrib GPTYP_12 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_13 length=4 label="Health personnel quest.ID";
  attrib GPTYP_13 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GLINE_14 length=4 label="Health personnel quest.ID";
  attrib GPTYP_14 length=4 format=GPTYPE_. label="Type of questionnaire";
  attrib GSQ01    length=4 format=GSQ01_. label="Dist. to thana hdqtrs (miles)";
  attrib GSQ02    length=4 format=GSQ02_. label="Dist.to district hdqtrs(miles)";
  attrib GSQ03    length=4 format=GSQ03_. label="Mothers club/LA in village";
  attrib GSQ04    length=4 format=GSQ04_. label="Grameen bank in village";
  attrib GSQ05    length=4 format=GSQ05_. label="BSIC Industries in village";
  attrib GSQ06    length=4 format=GSQ06_. label="Co-operative soc. in village";
  attrib GSQ07    length=4 format=GSQ07_. label="Inc.gen NGO's in village";
  attrib GSQ08    length=4 format=GSQ08_. label="Comm. television in village";
  attrib GSQ09    length=4 format=GSQ09_. label="Prop.of HH's living in 1 room";
  attrib GSQ10    length=4 format=GSQ10_. label="Prop.of HH's in non pukka hses";
  attrib GSQ11_1  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ11_2  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ11_3  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ11_4  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ11_5  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ11_6  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ11_7  length=4 format=GSQ11_. label="Facility in village/miles";
  attrib GSQ12    length=4 format=GSQ12_. label="FPW visits village";
  attrib GSQ13A   length=4 format=GSQ13A_. label="FPW prov. info. on Fmly.plng";
  attrib GSQ13B   length=4 format=GSQ13B_. label="FPW prov. meths of Fmly.plng";
  attrib GSQ13C   length=4 format=GSQ13C_. label="FPW helps at satellite clinic";
  attrib GSQ13D   length=4 format=GSQ13D_. label="FPW takes wmn.to clinic/hosp.";
  attrib GSQ13E   length=4 format=GSQ13E_. label="FPW takes child.for immuniztn.";
  attrib GSQ13F   length=4 format=GSQ13F_. label="FPW prov. vit.A caps.for child";
  attrib GSQ15    length=4 format=GSQ15_. label="How long FPW worked in village";
  attrib GSQ16    length=4 format=GSQ16_. label="Another FPW before current one";
  attrib GSQ17    length=4 format=GSQ17_. label="How long ago did 1st FPW start";
  attrib GSQ18    length=4 format=GSQ18_. label="Is there health wkr in village";
  attrib GSQ19A   length=4 format=GSQ19A_. label="HW provides health information";
  attrib GSQ19B   length=4 format=GSQ19B_. label="HW provides immunizations";
  attrib GSQ19C   length=4 format=GSQ19C_. label="HW provides ORS packets";
  attrib GSQ19D   length=4 format=GSQ19D_. label="HW provides vit.A capsules";
  attrib GSQ20A   length=4 format=GSQ20A_. label="Other FPW/hlth.wkr in village";
  attrib GSQ21    length=$8 label="Services prov.by other HWkr";
  attrib GSQ22    length=4 format=GSQ22_. label="Anyone who sells FP meths./hse";
  attrib GSQ23    length=$4 label="Methods sold from house";
  attrib GSQ24    length=4 format=GSQ24_. label="Satellite clinic in village";
  attrib GSQ25A   length=4 format=GSQ25A_. label="Sat.clinic held nearby";
  attrib GSQ25B   length=4 format=GSQ25B_. label="Distance to nearby sat.clinic";
  attrib GSQ26B   length=$10 label="Services avail.frm.satellite";
  attrib GSQ27U   length=4 format=GSQ27U_. label="Freq. of sat.clinics : units";
  attrib GSQ27N   length=4 format=GSQ27N_. label="Freq. of sat.clinics : number";
  attrib GSQ28    length=4 format=GSQ28_. label="Dist.to nearest family w.centr";
  attrib GSQ29    length=4 format=GSQ29_. label="Dist.to nrst hosp/thana HC";
Data BDSQ35FL;
infile 'BDSQ35FL.DAT' LRECL=181 MISSOVER ;
    input
    @1    GSQDIV   1.0
    @2    GSQDISTR 2.0
    @4    GSQCLUST 3.0
    @7    GSQTYPE  1.0
    @8    GSQINT   3.0
    @11   GSQINTD  2.0
    @13   GSQINTM  2.0
    @15   GSQINTY  4.0
    @19   GSQKEYER 2.0
    @21   GSQDAI   2.0
    @23   GSQDPL   2.0
    @25   GSQFPP   2.0
    @27   GSQFWV   2.0
    @29   GSQFWA   2.0
    @31   TOTRESP  2.0
    @33   GLINE_01 2.0
    @35   GPTYP_01 3.0
    @38   GLINE_02 2.0
    @40   GPTYP_02 3.0
    @43   GLINE_03 2.0
    @45   GPTYP_03 3.0
    @48   GLINE_04 2.0
    @50   GPTYP_04 3.0
    @53   GLINE_05 2.0
    @55   GPTYP_05 3.0
    @58   GLINE_06 2.0
    @60   GPTYP_06 3.0
    @63   GLINE_07 2.0
    @65   GPTYP_07 3.0
    @68   GLINE_08 2.0
    @70   GPTYP_08 3.0
    @73   GLINE_09 2.0
    @75   GPTYP_09 3.0
    @78   GLINE_10 2.0
    @80   GPTYP_10 3.0
    @83   GLINE_11 2.0
    @85   GPTYP_11 3.0
    @88   GLINE_12 2.0
    @90   GPTYP_12 3.0
    @93   GLINE_13 2.0
    @95   GPTYP_13 3.0
    @98   GLINE_14 2.0
    @100  GPTYP_14 3.0
    @103  GSQ01    2.0
    @105  GSQ02    2.0
    @107  GSQ03    1.0
    @108  GSQ04    1.0
    @109  GSQ05    1.0
    @110  GSQ06    1.0
    @111  GSQ07    1.0
    @112  GSQ08    1.0
    @113  GSQ09    1.0
    @114  GSQ10    1.0
    @115  GSQ11_1  2.0
    @117  GSQ11_2  2.0
    @119  GSQ11_3  2.0
    @121  GSQ11_4  2.0
    @123  GSQ11_5  2.0
    @125  GSQ11_6  2.0
    @127  GSQ11_7  2.0
    @129  GSQ12    1.0
    @130  GSQ13A   1.0
    @131  GSQ13B   1.0
    @132  GSQ13C   1.0
    @133  GSQ13D   1.0
    @134  GSQ13E   1.0
    @135  GSQ13F   1.0
    @136  GSQ15    2.0
    @138  GSQ16    1.0
    @139  GSQ17    2.0
    @141  GSQ18    1.0
    @142  GSQ19A   1.0
    @143  GSQ19B   1.0
    @144  GSQ19C   1.0
    @145  GSQ19D   1.0
    @146  GSQ20A   1.0
    @147  GSQ21    $8.
    @155  GSQ22    1.0
    @156  GSQ23    $4.
    @160  GSQ24    1.0
    @161  GSQ25A   1.0
    @162  GSQ25B   2.0
    @164  GSQ26B   $10.
    @174  GSQ27U   1.0
    @175  GSQ27N   2.0
    @177  GSQ28    2.0
    @179  GSQ29    2.0
    ;
    if GSQ01    =     99 then GSQ01 = .;
    if GSQ03    =      9 then GSQ03 = .;
    if GSQ04    =      9 then GSQ04 = .;
    if GSQ05    =      9 then GSQ05 = .;
    if GSQ06    =      9 then GSQ06 = .;
    if GSQ07    =      9 then GSQ07 = .;
    if GSQ08    =      9 then GSQ08 = .;
    if GSQ09    =      9 then GSQ09 = .;
    if GSQ10    =      9 then GSQ10 = .;
    if GSQ11_1  =     99 then GSQ11_1 = .;
    if GSQ11_2  =     99 then GSQ11_2 = .;
    if GSQ11_3  =     99 then GSQ11_3 = .;
    if GSQ11_4  =     99 then GSQ11_4 = .;
    if GSQ11_5  =     99 then GSQ11_5 = .;
    if GSQ11_6  =     99 then GSQ11_6 = .;
    if GSQ11_7  =     99 then GSQ11_7 = .;
    if GSQ12    =      9 then GSQ12 = .;
    if GSQ13A   =      9 then GSQ13A = .;
    if GSQ13B   =      9 then GSQ13B = .;
    if GSQ13C   =      9 then GSQ13C = .;
    if GSQ13D   =      9 then GSQ13D = .;
    if GSQ13E   =      9 then GSQ13E = .;
    if GSQ13F   =      9 then GSQ13F = .;
    if GSQ15    =     99 then GSQ15 = .;
    if GSQ16    =      9 then GSQ16 = .;
    if GSQ17    =     99 then GSQ17 = .;
    if GSQ18    =      9 then GSQ18 = .;
    if GSQ19A   =      9 then GSQ19A = .;
    if GSQ19B   =      9 then GSQ19B = .;
    if GSQ19C   =      9 then GSQ19C = .;
    if GSQ19D   =      9 then GSQ19D = .;
    if GSQ20A   =      9 then GSQ20A = .;
    if GSQ22    =      9 then GSQ22 = .;
    if GSQ24    =      9 then GSQ24 = .;
    if GSQ25A   =      9 then GSQ25A = .;
    if GSQ25B   =     99 then GSQ25B = .;
    if GSQ28    =     99 then GSQ28 = .;
    if GSQ29    =     99 then GSQ29 = .;
