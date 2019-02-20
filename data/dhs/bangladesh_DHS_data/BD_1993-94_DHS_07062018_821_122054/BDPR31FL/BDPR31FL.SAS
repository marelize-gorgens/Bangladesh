libname user ' ';
Proc format;
  value HV015_
     1 = "Completed"
     2 = "HH present, no resp"
     3 = "HH absent"
     4 = "Postponed"
     5 = "Refused"
     6 = "Dwelling vacant"
     7 = "Dwelling destroyed"
     8 = "Dwelling not found"
     9 = "Other"
     ;
  value HV020_
     0 = "All woman sample"
     1 = "Ever married sample"
     ;
  value HV023_
     0 = "National"
     1 = "Country specific"
     ;
  value HV024_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value HV025_
     1 = "Urban"
     2 = "Rural"
     ;
  value HV026_
     0 = "Dhaka/Chittagong"
     1 = "Small city"
     2 = "Town"
     3 = "Village"
     ;
  value HV027_
     0 = "Not selected"
     1 = "Men's survey"
     2 = "Husband's survey"
     ;
  value HV201_
    10 = "PIPED WATER"
    11 = "Piped inside dwelln'"
    12 = "Piped outside dwellg"
    20 = "WELL WATER"
    21 = "Tubewell"
    22 = "Surface/other well"
    30 = "SURFACE WATER"
    31 = "Pond/tank/lake"
    32 = "River/stream"
    41 = "Rainwater"
    96 = "Other"
     ;
  value HV202_
    10 = "PIPED WATER"
    11 = "Piped inside dwelln'"
    12 = "Piped outside dwellg"
    20 = "WELL WATER"
    21 = "Tubewell"
    22 = "Surface/other well"
    30 = "SURFACE WATER"
    31 = "Pond/tank/lake"
    32 = "River/stream"
    41 = "Rainwater"
    96 = "Other"
     ;
  value HV203_
     0 = "No"
     1 = "Yes"
     ;
  value HV204_
   996 = "On premises"
     ;
  value HV205_
    10 = "FLUSH TOILET"
    11 = "Septic tank/modern"
    20 = "PIT TOILET LATRINE"
    21 = "Water sealed/slab"
    22 = "Pit latrine"
    23 = "Open latrine"
    24 = "Hanging latrine"
    30 = "NO FACILITY"
    31 = "No facility/bush"
    96 = "OTHER"
     ;
  value HV206_
     0 = "No"
     1 = "Yes"
     ;
  value HV207_
     0 = "No"
     1 = "Yes"
     ;
  value HV208_
     0 = "No"
     1 = "Yes"
     ;
  value HV210_
     0 = "No"
     1 = "Yes"
     ;
  value HV213_
    10 = "NATURAL"
    11 = "Earth"
    20 = "RUDIMENTARY"
    21 = "Wood"
    30 = "FINISHED"
    31 = "Cement/concrete"
    96 = "OTHER"
     ;
  value HV214_
    10 = "NATURAL"
    11 = "Jute/bamboo/mud"
    20 = "RUDIMENTARY"
    21 = "Wood"
    30 = "FINISHED"
    31 = "Brick/cement"
    32 = "Tin"
    96 = "OTHER"
     ;
  value HV215_
    10 = "NATURAL"
    11 = "Jute/bamboo/mud"
    20 = "RUDIMENTARY"
    21 = "Tin"
    30 = "FINISHED"
    31 = "Cement/concrete"
    96 = "OTHER"
     ;
  value HV217_
     0 = "No adults"
     1 = "One adult"
     2 = "Two adults, opp. sex"
     3 = "Two adults, same sex"
     4 = "Three+ related adult"
     5 = "Unrelated adults"
     ;
  value HV219_
     1 = "Male"
     2 = "Female"
     ;
  value HV220_
    97 = "97+"
    98 = "DK"
     ;
  value SH20_
   996 = "On premises"
     ;
  value SH25_
     0 = "No children"
    10 = "FLUSH TOILET"
    11 = "Septic tank/modern"
    20 = "PIT TOILET LATRINE"
    21 = "Water sealed/slab"
    22 = "Pit latrine"
    23 = "Open latrine"
    24 = "Hanging latrine"
    30 = "NO FACILITY"
    31 = "No facility/bush"
    96 = "OTHER"
     ;
  value SH26A_
     0 = "No"
     1 = "Yes"
     ;
  value SH26B_
     0 = "No"
     1 = "Yes"
     ;
  value SH26C_
     0 = "No"
     1 = "Yes"
     ;
  value SH26D_
     0 = "No"
     1 = "Yes"
     ;
  value SH26I_
     0 = "No"
     1 = "Yes"
     ;
  value SH31_
     0 = "No"
     1 = "Yes"
     ;
  value HV101_
     1 = "Head"
     2 = "Wife or husband"
     3 = "Son/daughter"
     4 = "Son/daughter-in-law"
     5 = "Grandchild"
     6 = "Parent"
     7 = "Parent-in-law"
     8 = "Brother/sister"
     9 = "Co-spouse"
    10 = "Other relative"
    11 = "Adopted/foster child"
    12 = "Not related"
    98 = "DK"
     ;
  value HV102_
     0 = "No"
     1 = "Yes"
     ;
  value HV103_
     0 = "No"
     1 = "Yes"
     ;
  value HV104_
     1 = "Male"
     2 = "Female"
     ;
  value HV105_
    96 = "96 or older"
    98 = "DK"
     ;
  value HV106_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "DK"
     ;
  value HV107_
    98 = "DK"
     ;
  value HV108_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value HV109_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     8 = "DK"
     ;
  value HV110_
     0 = "No"
     1 = "Yes"
     ;
  value HV116_
     0 = "Never married"
     1 = "Currently married"
     2 = "Formerly/ever marr."
     ;
  value HV117_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value HV118_
     0 = "Not eligible"
     1 = "Eligible"
     ;
  value SH14_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value SH17_
     0 = "Not marr./husb away"
     ;
Data BDPR31FL;
  attrib HHID     length=$12 label="Case Identification";
  attrib HV000    length=$3 label="Country code and phase";
  attrib HV001    length=8 label="Cluster number";
  attrib HV002    length=4 label="Household number";
  attrib HV003    length=4 label="Respondent's line number";
  attrib HV004    length=4 label="Ultimate area unit";
  attrib HV005    length=8 label="Sample weight";
  attrib HV006    length=4 label="Month of interview";
  attrib HV007    length=4 label="Year of interview";
  attrib HV008    length=4 label="Date of interview (CMC)";
  attrib HV009    length=4 label="Number of household members";
  attrib HV010    length=4 label="Number of eligible women in HH";
  attrib HV011    length=4 label="Number of eligible men in HH";
  attrib HV012    length=4 label="Number of de jure members";
  attrib HV013    length=4 label="Number of de facto members";
  attrib HV014    length=4 label="Number of children 5 and under";
  attrib HV015    length=4 format=HV015_. label="Result of household interview";
  attrib HV016    length=4 label="Day of interview";
  attrib HV017    length=4 label="Number of visits";
  attrib HV018    length=4 label="Interviewer identification";
  attrib HV019    length=4 label="Keyer identification";
  attrib HV020    length=4 format=HV020_. label="Ever-married sample";
  attrib HV021    length=4 label="Primary sampling unit";
  attrib HV022    length=4 label="Sample stratum number";
  attrib HV023    length=4 format=HV023_. label="Sample domain";
  attrib HV024    length=4 format=HV024_. label="Region";
  attrib HV025    length=4 format=HV025_. label="Type of place of residence";
  attrib HV026    length=4 format=HV026_. label="Place of residence";
  attrib HV027    length=4 format=HV027_. label="Selection for male/husb. int.";
  attrib HV028    length=8 label="Sample weight for male subsamp";
  attrib HV030    length=4 label="Field supervisor -NA";
  attrib HV031    length=4 label="Field editor -NA";
  attrib HV032    length=4 label="Office editor -NA";
  attrib HV033    length=8 label="Ultimate area selection prob.";
  attrib HV201    length=4 format=HV201_. label="Source of drinking water";
  attrib HV202    length=4 format=HV202_. label="Source of non-drinking water";
  attrib HV203    length=4 format=HV203_. label="Same source of water";
  attrib HV204    length=4 format=HV204_. label="Time to get to water source";
  attrib HV205    length=4 format=HV205_. label="Type of toilet facility";
  attrib HV206    length=4 format=HV206_. label="Has electricity";
  attrib HV207    length=4 format=HV207_. label="Has radio";
  attrib HV208    length=4 format=HV208_. label="Has television";
  attrib HV209    length=4 label="Has refrigerator -NA";
  attrib HV210    length=4 format=HV210_. label="Has bicycle";
  attrib HV211    length=4 label="Has motorcycle -NA";
  attrib HV212    length=4 label="Has car -NA";
  attrib HV213    length=4 format=HV213_. label="Main floor material";
  attrib HV214    length=4 format=HV214_. label="Main wall material";
  attrib HV215    length=4 format=HV215_. label="Main roof material";
  attrib HV216    length=4 label="Rooms for sleeping";
  attrib HV217    length=4 format=HV217_. label="Relationship structure";
  attrib HV218    length=4 label="Line number of head of househ.";
  attrib HV219    length=4 format=HV219_. label="Sex of head of household";
  attrib HV220    length=4 format=HV220_. label="Age of head of household";
  attrib HV221    length=4 label="Has telephone -NA";
  attrib HV222    length=4 label="Type of salt used for cook -NA";
  attrib SHDISTR  length=4 label="District of residence";
  attrib SH20     length=4 format=SH20_. label="Time to get to water source";
  attrib SH20A    length=4 label="Time to wait to get water:mins";
  attrib SH25     length=4 format=SH25_. label="Toilet facility : children";
  attrib SH26A    length=4 format=SH26A_. label="Almirah";
  attrib SH26B    length=4 format=SH26B_. label="Table/chair/bench";
  attrib SH26C    length=4 format=SH26C_. label="Watch/clock";
  attrib SH26D    length=4 format=SH26D_. label="Cot/bed";
  attrib SH26I    length=4 format=SH26I_. label="Agricultural land";
  attrib SH31     length=4 format=SH31_. label="Urban slum";
  attrib HVIDX    length=4 label="Line number";
  attrib HV101    length=4 format=HV101_. label="Relationship to head";
  attrib HV102    length=4 format=HV102_. label="Usual resident";
  attrib HV103    length=4 format=HV103_. label="Slept last night";
  attrib HV104    length=4 format=HV104_. label="Sex of household member";
  attrib HV105    length=4 format=HV105_. label="Age of household members";
  attrib HV106    length=4 format=HV106_. label="Highest educational level";
  attrib HV107    length=4 format=HV107_. label="Highest year of education";
  attrib HV108    length=4 format=HV108_. label="Education in single years";
  attrib HV109    length=4 format=HV109_. label="Educational attainment";
  attrib HV110    length=4 format=HV110_. label="Member still in school";
  attrib HV111    length=4 label="Mother alive -NA";
  attrib HV112    length=4 label="Mother's line number -NA";
  attrib HV113    length=4 label="Father alive -NA";
  attrib HV114    length=4 label="Father's line number -NA";
  attrib HV115    length=4 label="Current marital status -NA";
  attrib HV116    length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117    length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118    length=4 format=HV118_. label="Eligibility for male interview";
  attrib IDXH4    length=4 label="Index to household schedule";
  attrib SH14     length=4 format=SH14_. label="Works for money";
  attrib SH17     length=4 format=SH17_. label="Husbands line number";
infile 'BDPR31FL.DAT' LRECL=190 MISSOVER ;
    input
    @1    HHID     $12.
    @13   HV000    $3.
    @16   HV001    8.0
    @24   HV002    4.0
    @28   HV003    3.0
    @31   HV004    4.0
    @35   HV005    8.0
    @43   HV006    2.0
    @45   HV007    2.0
    @47   HV008    4.0
    @51   HV009    2.0
    @53   HV010    2.0
    @55   HV011    2.0
    @57   HV012    2.0
    @59   HV013    2.0
    @61   HV014    2.0
    @63   HV015    1.0
    @64   HV016    2.0
    @66   HV017    1.0
    @67   HV018    3.0
    @70   HV019    2.0
    @72   HV020    1.0
    @73   HV021    4.0
    @77   HV022    4.0
    @81   HV023    2.0
    @83   HV024    2.0
    @85   HV025    1.0
    @86   HV026    1.0
    @87   HV027    1.0
    @88   HV028    8.0
    @96   HV030    2.0
    @98   HV031    2.0
    @100  HV032    2.0
    @102  HV033    8.0
    @110  HV201    2.0
    @112  HV202    2.0
    @114  HV203    1.0
    @115  HV204    3.0
    @118  HV205    2.0
    @120  HV206    1.0
    @121  HV207    1.0
    @122  HV208    1.0
    @123  HV209    1.0
    @124  HV210    1.0
    @125  HV211    1.0
    @126  HV212    1.0
    @127  HV213    2.0
    @129  HV214    2.0
    @131  HV215    2.0
    @133  HV216    2.0
    @135  HV217    1.0
    @136  HV218    2.0
    @138  HV219    1.0
    @139  HV220    2.0
    @141  HV221    1.0
    @142  HV222    2.0
    @144  SHDISTR  2.0
    @146  SH20     3.0
    @149  SH20A    3.0
    @152  SH25     2.0
    @154  SH26A    1.0
    @155  SH26B    1.0
    @156  SH26C    1.0
    @157  SH26D    1.0
    @158  SH26I    1.0
    @159  SH31     1.0
    @160  HVIDX    2.0
    @162  HV101    2.0
    @164  HV102    1.0
    @165  HV103    1.0
    @166  HV104    1.0
    @167  HV105    2.0
    @169  HV106    1.0
    @170  HV107    2.0
    @172  HV108    2.0
    @174  HV109    1.0
    @175  HV110    1.0
    @176  HV111    1.0
    @177  HV112    2.0
    @179  HV113    1.0
    @180  HV114    2.0
    @182  HV115    1.0
    @183  HV116    1.0
    @184  HV117    1.0
    @185  HV118    1.0
    @186  IDXH4    2.0
    @188  SH14     1.0
    @189  SH17     2.0
    ;
    if HV018    =    999 then HV018 = .;
    if HV019    =     99 then HV019 = .;
    if HV026    =      9 then HV026 = .;
    if HV030    =     99 then HV030 = .;
    if HV031    =     99 then HV031 = .;
    if HV032    =     99 then HV032 = .;
    if HV201    =     99 then HV201 = .;
    if HV202    =     99 then HV202 = .;
    if HV203    =      9 then HV203 = .;
    if HV204    =    999 then HV204 = .;
    if HV205    =     99 then HV205 = .;
    if HV206    =      9 then HV206 = .;
    if HV207    =      9 then HV207 = .;
    if HV208    =      9 then HV208 = .;
    if HV209    =      9 then HV209 = .;
    if HV210    =      9 then HV210 = .;
    if HV211    =      9 then HV211 = .;
    if HV212    =      9 then HV212 = .;
    if HV213    =     99 then HV213 = .;
    if HV214    =     99 then HV214 = .;
    if HV215    =     99 then HV215 = .;
    if HV216    =     99 then HV216 = .;
    if HV219    =      9 then HV219 = .;
    if HV220    =     99 then HV220 = .;
    if HV221    =      9 then HV221 = .;
    if HV222    =     99 then HV222 = .;
    if SH20     =    999 then SH20 = .;
    if SH20A    =    999 then SH20A = .;
    if SH25     =     99 then SH25 = .;
    if SH26A    =      9 then SH26A = .;
    if SH26B    =      9 then SH26B = .;
    if SH26C    =      9 then SH26C = .;
    if SH26D    =      9 then SH26D = .;
    if SH26I    =      9 then SH26I = .;
    if SH31     =      9 then SH31 = .;
    if HV101    =     99 then HV101 = .;
    if HV102    =      9 then HV102 = .;
    if HV103    =      9 then HV103 = .;
    if HV104    =      9 then HV104 = .;
    if HV105    =     99 then HV105 = .;
    if HV106    =      9 then HV106 = .;
    if HV107    =     99 then HV107 = .;
    if HV108    =     99 then HV108 = .;
    if HV109    =      9 then HV109 = .;
    if HV110    =      9 then HV110 = .;
    if HV111    =      9 then HV111 = .;
    if HV112    =     99 then HV112 = .;
    if HV113    =      9 then HV113 = .;
    if HV114    =     99 then HV114 = .;
    if HV115    =      9 then HV115 = .;
    if HV116    =      9 then HV116 = .;
    if SH14     =      9 then SH14 = .;
