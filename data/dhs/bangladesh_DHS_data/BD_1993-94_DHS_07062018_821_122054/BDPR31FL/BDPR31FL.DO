infix using BDPR31FL.dct

label variable hhid     "Case Identification"
label variable hv000    "Country code and phase"
label variable hv001    "Cluster number"
label variable hv002    "Household number"
label variable hv003    "Respondent's line number"
label variable hv004    "Ultimate area unit"
label variable hv005    "Sample weight"
label variable hv006    "Month of interview"
label variable hv007    "Year of interview"
label variable hv008    "Date of interview (CMC)"
label variable hv009    "Number of household members"
label variable hv010    "Number of eligible women in HH"
label variable hv011    "Number of eligible men in HH"
label variable hv012    "Number of de jure members"
label variable hv013    "Number of de facto members"
label variable hv014    "Number of children 5 and under"
label variable hv015    "Result of household interview"
label variable hv016    "Day of interview"
label variable hv017    "Number of visits"
label variable hv018    "Interviewer identification"
label variable hv019    "Keyer identification"
label variable hv020    "Ever-married sample"
label variable hv021    "Primary sampling unit"
label variable hv022    "Sample stratum number"
label variable hv023    "Sample domain"
label variable hv024    "Region"
label variable hv025    "Type of place of residence"
label variable hv026    "Place of residence"
label variable hv027    "Selection for male/husb. int."
label variable hv028    "Sample weight for male subsamp"
label variable hv030    "Field supervisor -NA"
label variable hv031    "Field editor -NA"
label variable hv032    "Office editor -NA"
label variable hv033    "Ultimate area selection prob."
label variable hv201    "Source of drinking water"
label variable hv202    "Source of non-drinking water"
label variable hv203    "Same source of water"
label variable hv204    "Time to get to water source"
label variable hv205    "Type of toilet facility"
label variable hv206    "Has electricity"
label variable hv207    "Has radio"
label variable hv208    "Has television"
label variable hv209    "Has refrigerator -NA"
label variable hv210    "Has bicycle"
label variable hv211    "Has motorcycle -NA"
label variable hv212    "Has car -NA"
label variable hv213    "Main floor material"
label variable hv214    "Main wall material"
label variable hv215    "Main roof material"
label variable hv216    "Rooms for sleeping"
label variable hv217    "Relationship structure"
label variable hv218    "Line number of head of househ."
label variable hv219    "Sex of head of household"
label variable hv220    "Age of head of household"
label variable hv221    "Has telephone -NA"
label variable hv222    "Type of salt used for cook -NA"
label variable shdistr  "District of residence"
label variable sh20     "Time to get to water source"
label variable sh20a    "Time to wait to get water:mins"
label variable sh25     "Toilet facility : children"
label variable sh26a    "Almirah"
label variable sh26b    "Table/chair/bench"
label variable sh26c    "Watch/clock"
label variable sh26d    "Cot/bed"
label variable sh26i    "Agricultural land"
label variable sh31     "Urban slum"
label variable hvidx    "Line number"
label variable hv101    "Relationship to head"
label variable hv102    "Usual resident"
label variable hv103    "Slept last night"
label variable hv104    "Sex of household member"
label variable hv105    "Age of household members"
label variable hv106    "Highest educational level"
label variable hv107    "Highest year of education"
label variable hv108    "Education in single years"
label variable hv109    "Educational attainment"
label variable hv110    "Member still in school"
label variable hv111    "Mother alive -NA"
label variable hv112    "Mother's line number -NA"
label variable hv113    "Father alive -NA"
label variable hv114    "Father's line number -NA"
label variable hv115    "Current marital status -NA"
label variable hv116    "Currently, formerly, never m."
label variable hv117    "Eligibility for female intview"
label variable hv118    "Eligibility for male interview"
label variable idxh4    "Index to household schedule"
label variable sh14     "Works for money"
label variable sh17     "Husbands line number"

#delimit ;
label define HV015   
     1 "Completed"
     2 "HH present, no resp"
     3 "HH absent"
     4 "Postponed"
     5 "Refused"
     6 "Dwelling vacant"
     7 "Dwelling destroyed"
     8 "Dwelling not found"
     9 "Other"
;
label define HV020   
     0 "All woman sample"
     1 "Ever married sample"
;
label define HV023   
     0 "National"
     1 "Country specific"
;
label define HV024   
     1 "Barishal"
     2 "Chittagong"
     3 "Dhaka"
     4 "Khulna"
     5 "Rajshani"
;
label define HV025   
     1 "Urban"
     2 "Rural"
;
label define HV026   
     0 "Dhaka/Chittagong"
     1 "Small city"
     2 "Town"
     3 "Village"
     9 "Missing"
;
label define HV027   
     0 "Not selected"
     1 "Men's survey"
     2 "Husband's survey"
;
label define HV201   
    10 "PIPED WATER"
    11 "Piped inside dwelln'"
    12 "Piped outside dwellg"
    20 "WELL WATER"
    21 "Tubewell"
    22 "Surface/other well"
    30 "SURFACE WATER"
    31 "Pond/tank/lake"
    32 "River/stream"
    41 "Rainwater"
    96 "Other"
    99 "Missing"
;
label define HV202   
    10 "PIPED WATER"
    11 "Piped inside dwelln'"
    12 "Piped outside dwellg"
    20 "WELL WATER"
    21 "Tubewell"
    22 "Surface/other well"
    30 "SURFACE WATER"
    31 "Pond/tank/lake"
    32 "River/stream"
    41 "Rainwater"
    96 "Other"
    99 "Missing"
;
label define HV203   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV204   
   996 "On premises"
   999 "Missing"
;
label define HV205   
    10 "FLUSH TOILET"
    11 "Septic tank/modern"
    20 "PIT TOILET LATRINE"
    21 "Water sealed/slab"
    22 "Pit latrine"
    23 "Open latrine"
    24 "Hanging latrine"
    30 "NO FACILITY"
    31 "No facility/bush"
    96 "OTHER"
    99 "Missing"
;
label define HV206   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV207   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV208   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV210   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV213   
    10 "NATURAL"
    11 "Earth"
    20 "RUDIMENTARY"
    21 "Wood"
    30 "FINISHED"
    31 "Cement/concrete"
    96 "OTHER"
    99 "Missing"
;
label define HV214   
    10 "NATURAL"
    11 "Jute/bamboo/mud"
    20 "RUDIMENTARY"
    21 "Wood"
    30 "FINISHED"
    31 "Brick/cement"
    32 "Tin"
    96 "OTHER"
    99 "Missing"
;
label define HV215   
    10 "NATURAL"
    11 "Jute/bamboo/mud"
    20 "RUDIMENTARY"
    21 "Tin"
    30 "FINISHED"
    31 "Cement/concrete"
    96 "OTHER"
    99 "Missing"
;
label define HV217   
     0 "No adults"
     1 "One adult"
     2 "Two adults, opp. sex"
     3 "Two adults, same sex"
     4 "Three+ related adult"
     5 "Unrelated adults"
;
label define HV219   
     1 "Male"
     2 "Female"
     9 "Missing"
;
label define HV220   
    97 "97+"
    98 "DK"
    99 "Missing"
;
label define SH20    
   996 "On premises"
   999 "Missing"
;
label define SH25    
     0 "No children"
    10 "FLUSH TOILET"
    11 "Septic tank/modern"
    20 "PIT TOILET LATRINE"
    21 "Water sealed/slab"
    22 "Pit latrine"
    23 "Open latrine"
    24 "Hanging latrine"
    30 "NO FACILITY"
    31 "No facility/bush"
    96 "OTHER"
    99 "Missing"
;
label define SH26A   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define SH26B   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define SH26C   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define SH26D   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define SH26I   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define SH31    
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV101   
     1 "Head"
     2 "Wife or husband"
     3 "Son/daughter"
     4 "Son/daughter-in-law"
     5 "Grandchild"
     6 "Parent"
     7 "Parent-in-law"
     8 "Brother/sister"
     9 "Co-spouse"
    10 "Other relative"
    11 "Adopted/foster child"
    12 "Not related"
    98 "DK"
    99 "Missing"
;
label define HV102   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV103   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV104   
     1 "Male"
     2 "Female"
     9 "Missing"
;
label define HV105   
    96 "96 or older"
    98 "DK"
    99 "Missing"
;
label define HV106   
     0 "No education"
     1 "Primary"
     2 "Secondary"
     3 "Higher"
     8 "DK"
     9 "Missing"
;
label define HV107   
    98 "DK"
    99 "Missing"
;
label define HV108   
    97 "Inconsistent"
    98 "DK"
    99 "Missing"
;
label define HV109   
     0 "No education"
     1 "Incomplete primary"
     2 "Complete primary"
     3 "Incomplete secondary"
     4 "Complete secondary"
     5 "Higher"
     8 "DK"
     9 "Missing"
;
label define HV110   
     0 "No"
     1 "Yes"
     9 "Missing"
;
label define HV116   
     0 "Never married"
     1 "Currently married"
     2 "Formerly/ever marr."
     9 "Missing"
;
label define HV117   
     0 "Not eligible"
     1 "Eligible"
;
label define HV118   
     0 "Not eligible"
     1 "Eligible"
;
label define SH14    
     0 "No"
     1 "Yes"
     8 "Don't know"
     9 "Missing"
;
label define SH17    
     0 "Not marr./husb away"
;

#delimit cr
label values hv015    HV015   
label values hv020    HV020   
label values hv023    HV023   
label values hv024    HV024   
label values hv025    HV025   
label values hv026    HV026   
label values hv027    HV027   
label values hv201    HV201   
label values hv202    HV202   
label values hv203    HV203   
label values hv204    HV204   
label values hv205    HV205   
label values hv206    HV206   
label values hv207    HV207   
label values hv208    HV208   
label values hv210    HV210   
label values hv213    HV213   
label values hv214    HV214   
label values hv215    HV215   
label values hv217    HV217   
label values hv219    HV219   
label values hv220    HV220   
label values sh20     SH20    
label values sh25     SH25    
label values sh26a    SH26A   
label values sh26b    SH26B   
label values sh26c    SH26C   
label values sh26d    SH26D   
label values sh26i    SH26I   
label values sh31     SH31    
label values hv101    HV101   
label values hv102    HV102   
label values hv103    HV103   
label values hv104    HV104   
label values hv105    HV105   
label values hv106    HV106   
label values hv107    HV107   
label values hv108    HV108   
label values hv109    HV109   
label values hv110    HV110   
label values hv116    HV116   
label values hv117    HV117   
label values hv118    HV118   
label values sh14     SH14    
label values sh17     SH17    
