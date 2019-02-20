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
  value SH14_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value SH17_
     0 = "Not marr./husb away"
     ;
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
  attrib HVIDX_01 length=4 label="Line number";
  attrib HV101_01 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_01 length=4 format=HV102_. label="Usual resident";
  attrib HV103_01 length=4 format=HV103_. label="Slept last night";
  attrib HV104_01 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_01 length=4 format=HV105_. label="Age of household members";
  attrib HV106_01 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_01 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_01 length=4 format=HV108_. label="Education in single years";
  attrib HV109_01 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_01 length=4 format=HV110_. label="Member still in school";
  attrib HV111_01 length=4 label="Mother alive -NA";
  attrib HV112_01 length=4 label="Mother's line number -NA";
  attrib HV113_01 length=4 label="Father alive -NA";
  attrib HV114_01 length=4 label="Father's line number -NA";
  attrib HV115_01 length=4 label="Current marital status -NA";
  attrib HV116_01 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_01 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_01 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_02 length=4 label="Line number";
  attrib HV101_02 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_02 length=4 format=HV102_. label="Usual resident";
  attrib HV103_02 length=4 format=HV103_. label="Slept last night";
  attrib HV104_02 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_02 length=4 format=HV105_. label="Age of household members";
  attrib HV106_02 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_02 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_02 length=4 format=HV108_. label="Education in single years";
  attrib HV109_02 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_02 length=4 format=HV110_. label="Member still in school";
  attrib HV111_02 length=4 label="Mother alive -NA";
  attrib HV112_02 length=4 label="Mother's line number -NA";
  attrib HV113_02 length=4 label="Father alive -NA";
  attrib HV114_02 length=4 label="Father's line number -NA";
  attrib HV115_02 length=4 label="Current marital status -NA";
  attrib HV116_02 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_02 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_02 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_03 length=4 label="Line number";
  attrib HV101_03 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_03 length=4 format=HV102_. label="Usual resident";
  attrib HV103_03 length=4 format=HV103_. label="Slept last night";
  attrib HV104_03 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_03 length=4 format=HV105_. label="Age of household members";
  attrib HV106_03 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_03 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_03 length=4 format=HV108_. label="Education in single years";
  attrib HV109_03 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_03 length=4 format=HV110_. label="Member still in school";
  attrib HV111_03 length=4 label="Mother alive -NA";
  attrib HV112_03 length=4 label="Mother's line number -NA";
  attrib HV113_03 length=4 label="Father alive -NA";
  attrib HV114_03 length=4 label="Father's line number -NA";
  attrib HV115_03 length=4 label="Current marital status -NA";
  attrib HV116_03 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_03 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_03 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_04 length=4 label="Line number";
  attrib HV101_04 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_04 length=4 format=HV102_. label="Usual resident";
  attrib HV103_04 length=4 format=HV103_. label="Slept last night";
  attrib HV104_04 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_04 length=4 format=HV105_. label="Age of household members";
  attrib HV106_04 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_04 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_04 length=4 format=HV108_. label="Education in single years";
  attrib HV109_04 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_04 length=4 format=HV110_. label="Member still in school";
  attrib HV111_04 length=4 label="Mother alive -NA";
  attrib HV112_04 length=4 label="Mother's line number -NA";
  attrib HV113_04 length=4 label="Father alive -NA";
  attrib HV114_04 length=4 label="Father's line number -NA";
  attrib HV115_04 length=4 label="Current marital status -NA";
  attrib HV116_04 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_04 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_04 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_05 length=4 label="Line number";
  attrib HV101_05 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_05 length=4 format=HV102_. label="Usual resident";
  attrib HV103_05 length=4 format=HV103_. label="Slept last night";
  attrib HV104_05 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_05 length=4 format=HV105_. label="Age of household members";
  attrib HV106_05 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_05 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_05 length=4 format=HV108_. label="Education in single years";
  attrib HV109_05 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_05 length=4 format=HV110_. label="Member still in school";
  attrib HV111_05 length=4 label="Mother alive -NA";
  attrib HV112_05 length=4 label="Mother's line number -NA";
  attrib HV113_05 length=4 label="Father alive -NA";
  attrib HV114_05 length=4 label="Father's line number -NA";
  attrib HV115_05 length=4 label="Current marital status -NA";
  attrib HV116_05 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_05 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_05 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_06 length=4 label="Line number";
  attrib HV101_06 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_06 length=4 format=HV102_. label="Usual resident";
  attrib HV103_06 length=4 format=HV103_. label="Slept last night";
  attrib HV104_06 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_06 length=4 format=HV105_. label="Age of household members";
  attrib HV106_06 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_06 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_06 length=4 format=HV108_. label="Education in single years";
  attrib HV109_06 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_06 length=4 format=HV110_. label="Member still in school";
  attrib HV111_06 length=4 label="Mother alive -NA";
  attrib HV112_06 length=4 label="Mother's line number -NA";
  attrib HV113_06 length=4 label="Father alive -NA";
  attrib HV114_06 length=4 label="Father's line number -NA";
  attrib HV115_06 length=4 label="Current marital status -NA";
  attrib HV116_06 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_06 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_06 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_07 length=4 label="Line number";
  attrib HV101_07 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_07 length=4 format=HV102_. label="Usual resident";
  attrib HV103_07 length=4 format=HV103_. label="Slept last night";
  attrib HV104_07 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_07 length=4 format=HV105_. label="Age of household members";
  attrib HV106_07 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_07 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_07 length=4 format=HV108_. label="Education in single years";
  attrib HV109_07 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_07 length=4 format=HV110_. label="Member still in school";
  attrib HV111_07 length=4 label="Mother alive -NA";
  attrib HV112_07 length=4 label="Mother's line number -NA";
  attrib HV113_07 length=4 label="Father alive -NA";
  attrib HV114_07 length=4 label="Father's line number -NA";
  attrib HV115_07 length=4 label="Current marital status -NA";
  attrib HV116_07 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_07 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_07 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_08 length=4 label="Line number";
  attrib HV101_08 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_08 length=4 format=HV102_. label="Usual resident";
  attrib HV103_08 length=4 format=HV103_. label="Slept last night";
  attrib HV104_08 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_08 length=4 format=HV105_. label="Age of household members";
  attrib HV106_08 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_08 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_08 length=4 format=HV108_. label="Education in single years";
  attrib HV109_08 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_08 length=4 format=HV110_. label="Member still in school";
  attrib HV111_08 length=4 label="Mother alive -NA";
  attrib HV112_08 length=4 label="Mother's line number -NA";
  attrib HV113_08 length=4 label="Father alive -NA";
  attrib HV114_08 length=4 label="Father's line number -NA";
  attrib HV115_08 length=4 label="Current marital status -NA";
  attrib HV116_08 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_08 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_08 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_09 length=4 label="Line number";
  attrib HV101_09 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_09 length=4 format=HV102_. label="Usual resident";
  attrib HV103_09 length=4 format=HV103_. label="Slept last night";
  attrib HV104_09 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_09 length=4 format=HV105_. label="Age of household members";
  attrib HV106_09 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_09 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_09 length=4 format=HV108_. label="Education in single years";
  attrib HV109_09 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_09 length=4 format=HV110_. label="Member still in school";
  attrib HV111_09 length=4 label="Mother alive -NA";
  attrib HV112_09 length=4 label="Mother's line number -NA";
  attrib HV113_09 length=4 label="Father alive -NA";
  attrib HV114_09 length=4 label="Father's line number -NA";
  attrib HV115_09 length=4 label="Current marital status -NA";
  attrib HV116_09 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_09 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_09 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_10 length=4 label="Line number";
  attrib HV101_10 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_10 length=4 format=HV102_. label="Usual resident";
  attrib HV103_10 length=4 format=HV103_. label="Slept last night";
  attrib HV104_10 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_10 length=4 format=HV105_. label="Age of household members";
  attrib HV106_10 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_10 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_10 length=4 format=HV108_. label="Education in single years";
  attrib HV109_10 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_10 length=4 format=HV110_. label="Member still in school";
  attrib HV111_10 length=4 label="Mother alive -NA";
  attrib HV112_10 length=4 label="Mother's line number -NA";
  attrib HV113_10 length=4 label="Father alive -NA";
  attrib HV114_10 length=4 label="Father's line number -NA";
  attrib HV115_10 length=4 label="Current marital status -NA";
  attrib HV116_10 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_10 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_10 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_11 length=4 label="Line number";
  attrib HV101_11 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_11 length=4 format=HV102_. label="Usual resident";
  attrib HV103_11 length=4 format=HV103_. label="Slept last night";
  attrib HV104_11 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_11 length=4 format=HV105_. label="Age of household members";
  attrib HV106_11 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_11 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_11 length=4 format=HV108_. label="Education in single years";
  attrib HV109_11 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_11 length=4 format=HV110_. label="Member still in school";
  attrib HV111_11 length=4 label="Mother alive -NA";
  attrib HV112_11 length=4 label="Mother's line number -NA";
  attrib HV113_11 length=4 label="Father alive -NA";
  attrib HV114_11 length=4 label="Father's line number -NA";
  attrib HV115_11 length=4 label="Current marital status -NA";
  attrib HV116_11 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_11 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_11 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_12 length=4 label="Line number";
  attrib HV101_12 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_12 length=4 format=HV102_. label="Usual resident";
  attrib HV103_12 length=4 format=HV103_. label="Slept last night";
  attrib HV104_12 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_12 length=4 format=HV105_. label="Age of household members";
  attrib HV106_12 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_12 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_12 length=4 format=HV108_. label="Education in single years";
  attrib HV109_12 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_12 length=4 format=HV110_. label="Member still in school";
  attrib HV111_12 length=4 label="Mother alive -NA";
  attrib HV112_12 length=4 label="Mother's line number -NA";
  attrib HV113_12 length=4 label="Father alive -NA";
  attrib HV114_12 length=4 label="Father's line number -NA";
  attrib HV115_12 length=4 label="Current marital status -NA";
  attrib HV116_12 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_12 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_12 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_13 length=4 label="Line number";
  attrib HV101_13 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_13 length=4 format=HV102_. label="Usual resident";
  attrib HV103_13 length=4 format=HV103_. label="Slept last night";
  attrib HV104_13 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_13 length=4 format=HV105_. label="Age of household members";
  attrib HV106_13 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_13 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_13 length=4 format=HV108_. label="Education in single years";
  attrib HV109_13 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_13 length=4 format=HV110_. label="Member still in school";
  attrib HV111_13 length=4 label="Mother alive -NA";
  attrib HV112_13 length=4 label="Mother's line number -NA";
  attrib HV113_13 length=4 label="Father alive -NA";
  attrib HV114_13 length=4 label="Father's line number -NA";
  attrib HV115_13 length=4 label="Current marital status -NA";
  attrib HV116_13 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_13 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_13 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_14 length=4 label="Line number";
  attrib HV101_14 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_14 length=4 format=HV102_. label="Usual resident";
  attrib HV103_14 length=4 format=HV103_. label="Slept last night";
  attrib HV104_14 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_14 length=4 format=HV105_. label="Age of household members";
  attrib HV106_14 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_14 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_14 length=4 format=HV108_. label="Education in single years";
  attrib HV109_14 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_14 length=4 format=HV110_. label="Member still in school";
  attrib HV111_14 length=4 label="Mother alive -NA";
  attrib HV112_14 length=4 label="Mother's line number -NA";
  attrib HV113_14 length=4 label="Father alive -NA";
  attrib HV114_14 length=4 label="Father's line number -NA";
  attrib HV115_14 length=4 label="Current marital status -NA";
  attrib HV116_14 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_14 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_14 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_15 length=4 label="Line number";
  attrib HV101_15 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_15 length=4 format=HV102_. label="Usual resident";
  attrib HV103_15 length=4 format=HV103_. label="Slept last night";
  attrib HV104_15 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_15 length=4 format=HV105_. label="Age of household members";
  attrib HV106_15 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_15 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_15 length=4 format=HV108_. label="Education in single years";
  attrib HV109_15 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_15 length=4 format=HV110_. label="Member still in school";
  attrib HV111_15 length=4 label="Mother alive -NA";
  attrib HV112_15 length=4 label="Mother's line number -NA";
  attrib HV113_15 length=4 label="Father alive -NA";
  attrib HV114_15 length=4 label="Father's line number -NA";
  attrib HV115_15 length=4 label="Current marital status -NA";
  attrib HV116_15 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_15 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_15 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_16 length=4 label="Line number";
  attrib HV101_16 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_16 length=4 format=HV102_. label="Usual resident";
  attrib HV103_16 length=4 format=HV103_. label="Slept last night";
  attrib HV104_16 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_16 length=4 format=HV105_. label="Age of household members";
  attrib HV106_16 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_16 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_16 length=4 format=HV108_. label="Education in single years";
  attrib HV109_16 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_16 length=4 format=HV110_. label="Member still in school";
  attrib HV111_16 length=4 label="Mother alive -NA";
  attrib HV112_16 length=4 label="Mother's line number -NA";
  attrib HV113_16 length=4 label="Father alive -NA";
  attrib HV114_16 length=4 label="Father's line number -NA";
  attrib HV115_16 length=4 label="Current marital status -NA";
  attrib HV116_16 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_16 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_16 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_17 length=4 label="Line number";
  attrib HV101_17 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_17 length=4 format=HV102_. label="Usual resident";
  attrib HV103_17 length=4 format=HV103_. label="Slept last night";
  attrib HV104_17 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_17 length=4 format=HV105_. label="Age of household members";
  attrib HV106_17 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_17 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_17 length=4 format=HV108_. label="Education in single years";
  attrib HV109_17 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_17 length=4 format=HV110_. label="Member still in school";
  attrib HV111_17 length=4 label="Mother alive -NA";
  attrib HV112_17 length=4 label="Mother's line number -NA";
  attrib HV113_17 length=4 label="Father alive -NA";
  attrib HV114_17 length=4 label="Father's line number -NA";
  attrib HV115_17 length=4 label="Current marital status -NA";
  attrib HV116_17 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_17 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_17 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_18 length=4 label="Line number";
  attrib HV101_18 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_18 length=4 format=HV102_. label="Usual resident";
  attrib HV103_18 length=4 format=HV103_. label="Slept last night";
  attrib HV104_18 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_18 length=4 format=HV105_. label="Age of household members";
  attrib HV106_18 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_18 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_18 length=4 format=HV108_. label="Education in single years";
  attrib HV109_18 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_18 length=4 format=HV110_. label="Member still in school";
  attrib HV111_18 length=4 label="Mother alive -NA";
  attrib HV112_18 length=4 label="Mother's line number -NA";
  attrib HV113_18 length=4 label="Father alive -NA";
  attrib HV114_18 length=4 label="Father's line number -NA";
  attrib HV115_18 length=4 label="Current marital status -NA";
  attrib HV116_18 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_18 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_18 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_19 length=4 label="Line number";
  attrib HV101_19 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_19 length=4 format=HV102_. label="Usual resident";
  attrib HV103_19 length=4 format=HV103_. label="Slept last night";
  attrib HV104_19 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_19 length=4 format=HV105_. label="Age of household members";
  attrib HV106_19 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_19 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_19 length=4 format=HV108_. label="Education in single years";
  attrib HV109_19 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_19 length=4 format=HV110_. label="Member still in school";
  attrib HV111_19 length=4 label="Mother alive -NA";
  attrib HV112_19 length=4 label="Mother's line number -NA";
  attrib HV113_19 length=4 label="Father alive -NA";
  attrib HV114_19 length=4 label="Father's line number -NA";
  attrib HV115_19 length=4 label="Current marital status -NA";
  attrib HV116_19 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_19 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_19 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_20 length=4 label="Line number";
  attrib HV101_20 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_20 length=4 format=HV102_. label="Usual resident";
  attrib HV103_20 length=4 format=HV103_. label="Slept last night";
  attrib HV104_20 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_20 length=4 format=HV105_. label="Age of household members";
  attrib HV106_20 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_20 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_20 length=4 format=HV108_. label="Education in single years";
  attrib HV109_20 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_20 length=4 format=HV110_. label="Member still in school";
  attrib HV111_20 length=4 label="Mother alive -NA";
  attrib HV112_20 length=4 label="Mother's line number -NA";
  attrib HV113_20 length=4 label="Father alive -NA";
  attrib HV114_20 length=4 label="Father's line number -NA";
  attrib HV115_20 length=4 label="Current marital status -NA";
  attrib HV116_20 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_20 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_20 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_21 length=4 label="Line number";
  attrib HV101_21 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_21 length=4 format=HV102_. label="Usual resident";
  attrib HV103_21 length=4 format=HV103_. label="Slept last night";
  attrib HV104_21 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_21 length=4 format=HV105_. label="Age of household members";
  attrib HV106_21 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_21 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_21 length=4 format=HV108_. label="Education in single years";
  attrib HV109_21 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_21 length=4 format=HV110_. label="Member still in school";
  attrib HV111_21 length=4 label="Mother alive -NA";
  attrib HV112_21 length=4 label="Mother's line number -NA";
  attrib HV113_21 length=4 label="Father alive -NA";
  attrib HV114_21 length=4 label="Father's line number -NA";
  attrib HV115_21 length=4 label="Current marital status -NA";
  attrib HV116_21 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_21 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_21 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_22 length=4 label="Line number";
  attrib HV101_22 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_22 length=4 format=HV102_. label="Usual resident";
  attrib HV103_22 length=4 format=HV103_. label="Slept last night";
  attrib HV104_22 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_22 length=4 format=HV105_. label="Age of household members";
  attrib HV106_22 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_22 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_22 length=4 format=HV108_. label="Education in single years";
  attrib HV109_22 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_22 length=4 format=HV110_. label="Member still in school";
  attrib HV111_22 length=4 label="Mother alive -NA";
  attrib HV112_22 length=4 label="Mother's line number -NA";
  attrib HV113_22 length=4 label="Father alive -NA";
  attrib HV114_22 length=4 label="Father's line number -NA";
  attrib HV115_22 length=4 label="Current marital status -NA";
  attrib HV116_22 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_22 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_22 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_23 length=4 label="Line number";
  attrib HV101_23 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_23 length=4 format=HV102_. label="Usual resident";
  attrib HV103_23 length=4 format=HV103_. label="Slept last night";
  attrib HV104_23 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_23 length=4 format=HV105_. label="Age of household members";
  attrib HV106_23 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_23 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_23 length=4 format=HV108_. label="Education in single years";
  attrib HV109_23 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_23 length=4 format=HV110_. label="Member still in school";
  attrib HV111_23 length=4 label="Mother alive -NA";
  attrib HV112_23 length=4 label="Mother's line number -NA";
  attrib HV113_23 length=4 label="Father alive -NA";
  attrib HV114_23 length=4 label="Father's line number -NA";
  attrib HV115_23 length=4 label="Current marital status -NA";
  attrib HV116_23 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_23 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_23 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_24 length=4 label="Line number";
  attrib HV101_24 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_24 length=4 format=HV102_. label="Usual resident";
  attrib HV103_24 length=4 format=HV103_. label="Slept last night";
  attrib HV104_24 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_24 length=4 format=HV105_. label="Age of household members";
  attrib HV106_24 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_24 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_24 length=4 format=HV108_. label="Education in single years";
  attrib HV109_24 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_24 length=4 format=HV110_. label="Member still in school";
  attrib HV111_24 length=4 label="Mother alive -NA";
  attrib HV112_24 length=4 label="Mother's line number -NA";
  attrib HV113_24 length=4 label="Father alive -NA";
  attrib HV114_24 length=4 label="Father's line number -NA";
  attrib HV115_24 length=4 label="Current marital status -NA";
  attrib HV116_24 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_24 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_24 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_25 length=4 label="Line number";
  attrib HV101_25 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_25 length=4 format=HV102_. label="Usual resident";
  attrib HV103_25 length=4 format=HV103_. label="Slept last night";
  attrib HV104_25 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_25 length=4 format=HV105_. label="Age of household members";
  attrib HV106_25 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_25 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_25 length=4 format=HV108_. label="Education in single years";
  attrib HV109_25 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_25 length=4 format=HV110_. label="Member still in school";
  attrib HV111_25 length=4 label="Mother alive -NA";
  attrib HV112_25 length=4 label="Mother's line number -NA";
  attrib HV113_25 length=4 label="Father alive -NA";
  attrib HV114_25 length=4 label="Father's line number -NA";
  attrib HV115_25 length=4 label="Current marital status -NA";
  attrib HV116_25 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_25 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_25 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_26 length=4 label="Line number";
  attrib HV101_26 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_26 length=4 format=HV102_. label="Usual resident";
  attrib HV103_26 length=4 format=HV103_. label="Slept last night";
  attrib HV104_26 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_26 length=4 format=HV105_. label="Age of household members";
  attrib HV106_26 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_26 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_26 length=4 format=HV108_. label="Education in single years";
  attrib HV109_26 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_26 length=4 format=HV110_. label="Member still in school";
  attrib HV111_26 length=4 label="Mother alive -NA";
  attrib HV112_26 length=4 label="Mother's line number -NA";
  attrib HV113_26 length=4 label="Father alive -NA";
  attrib HV114_26 length=4 label="Father's line number -NA";
  attrib HV115_26 length=4 label="Current marital status -NA";
  attrib HV116_26 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_26 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_26 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_27 length=4 label="Line number";
  attrib HV101_27 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_27 length=4 format=HV102_. label="Usual resident";
  attrib HV103_27 length=4 format=HV103_. label="Slept last night";
  attrib HV104_27 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_27 length=4 format=HV105_. label="Age of household members";
  attrib HV106_27 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_27 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_27 length=4 format=HV108_. label="Education in single years";
  attrib HV109_27 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_27 length=4 format=HV110_. label="Member still in school";
  attrib HV111_27 length=4 label="Mother alive -NA";
  attrib HV112_27 length=4 label="Mother's line number -NA";
  attrib HV113_27 length=4 label="Father alive -NA";
  attrib HV114_27 length=4 label="Father's line number -NA";
  attrib HV115_27 length=4 label="Current marital status -NA";
  attrib HV116_27 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_27 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_27 length=4 format=HV118_. label="Eligibility for male interview";
  attrib HVIDX_28 length=4 label="Line number";
  attrib HV101_28 length=4 format=HV101_. label="Relationship to head";
  attrib HV102_28 length=4 format=HV102_. label="Usual resident";
  attrib HV103_28 length=4 format=HV103_. label="Slept last night";
  attrib HV104_28 length=4 format=HV104_. label="Sex of household member";
  attrib HV105_28 length=4 format=HV105_. label="Age of household members";
  attrib HV106_28 length=4 format=HV106_. label="Highest educational level";
  attrib HV107_28 length=4 format=HV107_. label="Highest year of education";
  attrib HV108_28 length=4 format=HV108_. label="Education in single years";
  attrib HV109_28 length=4 format=HV109_. label="Educational attainment";
  attrib HV110_28 length=4 format=HV110_. label="Member still in school";
  attrib HV111_28 length=4 label="Mother alive -NA";
  attrib HV112_28 length=4 label="Mother's line number -NA";
  attrib HV113_28 length=4 label="Father alive -NA";
  attrib HV114_28 length=4 label="Father's line number -NA";
  attrib HV115_28 length=4 label="Current marital status -NA";
  attrib HV116_28 length=4 format=HV116_. label="Currently, formerly, never m.";
  attrib HV117_28 length=4 format=HV117_. label="Eligibility for female intview";
  attrib HV118_28 length=4 format=HV118_. label="Eligibility for male interview";
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
  attrib IDXH4_01 length=4 label="Index to household schedule";
  attrib SH14_01  length=4 format=SH14_. label="Works for money";
  attrib SH17_01  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_02 length=4 label="Index to household schedule";
  attrib SH14_02  length=4 format=SH14_. label="Works for money";
  attrib SH17_02  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_03 length=4 label="Index to household schedule";
  attrib SH14_03  length=4 format=SH14_. label="Works for money";
  attrib SH17_03  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_04 length=4 label="Index to household schedule";
  attrib SH14_04  length=4 format=SH14_. label="Works for money";
  attrib SH17_04  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_05 length=4 label="Index to household schedule";
  attrib SH14_05  length=4 format=SH14_. label="Works for money";
  attrib SH17_05  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_06 length=4 label="Index to household schedule";
  attrib SH14_06  length=4 format=SH14_. label="Works for money";
  attrib SH17_06  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_07 length=4 label="Index to household schedule";
  attrib SH14_07  length=4 format=SH14_. label="Works for money";
  attrib SH17_07  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_08 length=4 label="Index to household schedule";
  attrib SH14_08  length=4 format=SH14_. label="Works for money";
  attrib SH17_08  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_09 length=4 label="Index to household schedule";
  attrib SH14_09  length=4 format=SH14_. label="Works for money";
  attrib SH17_09  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_10 length=4 label="Index to household schedule";
  attrib SH14_10  length=4 format=SH14_. label="Works for money";
  attrib SH17_10  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_11 length=4 label="Index to household schedule";
  attrib SH14_11  length=4 format=SH14_. label="Works for money";
  attrib SH17_11  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_12 length=4 label="Index to household schedule";
  attrib SH14_12  length=4 format=SH14_. label="Works for money";
  attrib SH17_12  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_13 length=4 label="Index to household schedule";
  attrib SH14_13  length=4 format=SH14_. label="Works for money";
  attrib SH17_13  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_14 length=4 label="Index to household schedule";
  attrib SH14_14  length=4 format=SH14_. label="Works for money";
  attrib SH17_14  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_15 length=4 label="Index to household schedule";
  attrib SH14_15  length=4 format=SH14_. label="Works for money";
  attrib SH17_15  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_16 length=4 label="Index to household schedule";
  attrib SH14_16  length=4 format=SH14_. label="Works for money";
  attrib SH17_16  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_17 length=4 label="Index to household schedule";
  attrib SH14_17  length=4 format=SH14_. label="Works for money";
  attrib SH17_17  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_18 length=4 label="Index to household schedule";
  attrib SH14_18  length=4 format=SH14_. label="Works for money";
  attrib SH17_18  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_19 length=4 label="Index to household schedule";
  attrib SH14_19  length=4 format=SH14_. label="Works for money";
  attrib SH17_19  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_20 length=4 label="Index to household schedule";
  attrib SH14_20  length=4 format=SH14_. label="Works for money";
  attrib SH17_20  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_21 length=4 label="Index to household schedule";
  attrib SH14_21  length=4 format=SH14_. label="Works for money";
  attrib SH17_21  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_22 length=4 label="Index to household schedule";
  attrib SH14_22  length=4 format=SH14_. label="Works for money";
  attrib SH17_22  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_23 length=4 label="Index to household schedule";
  attrib SH14_23  length=4 format=SH14_. label="Works for money";
  attrib SH17_23  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_24 length=4 label="Index to household schedule";
  attrib SH14_24  length=4 format=SH14_. label="Works for money";
  attrib SH17_24  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_25 length=4 label="Index to household schedule";
  attrib SH14_25  length=4 format=SH14_. label="Works for money";
  attrib SH17_25  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_26 length=4 label="Index to household schedule";
  attrib SH14_26  length=4 format=SH14_. label="Works for money";
  attrib SH17_26  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_27 length=4 label="Index to household schedule";
  attrib SH14_27  length=4 format=SH14_. label="Works for money";
  attrib SH17_27  length=4 format=SH17_. label="Husbands line number";
  attrib IDXH4_28 length=4 label="Index to household schedule";
  attrib SH14_28  length=4 format=SH14_. label="Works for money";
  attrib SH17_28  length=4 format=SH17_. label="Husbands line number";
Data BDHR31FL;
infile 'BDHR31FL.DAT' LRECL=1028 MISSOVER ;
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
    @144  HVIDX_01 2.0
    @146  HV101_01 2.0
    @148  HV102_01 1.0
    @149  HV103_01 1.0
    @150  HV104_01 1.0
    @151  HV105_01 2.0
    @153  HV106_01 1.0
    @154  HV107_01 2.0
    @156  HV108_01 2.0
    @158  HV109_01 1.0
    @159  HV110_01 1.0
    @160  HV111_01 1.0
    @161  HV112_01 2.0
    @163  HV113_01 1.0
    @164  HV114_01 2.0
    @166  HV115_01 1.0
    @167  HV116_01 1.0
    @168  HV117_01 1.0
    @169  HV118_01 1.0
    @170  HVIDX_02 2.0
    @172  HV101_02 2.0
    @174  HV102_02 1.0
    @175  HV103_02 1.0
    @176  HV104_02 1.0
    @177  HV105_02 2.0
    @179  HV106_02 1.0
    @180  HV107_02 2.0
    @182  HV108_02 2.0
    @184  HV109_02 1.0
    @185  HV110_02 1.0
    @186  HV111_02 1.0
    @187  HV112_02 2.0
    @189  HV113_02 1.0
    @190  HV114_02 2.0
    @192  HV115_02 1.0
    @193  HV116_02 1.0
    @194  HV117_02 1.0
    @195  HV118_02 1.0
    @196  HVIDX_03 2.0
    @198  HV101_03 2.0
    @200  HV102_03 1.0
    @201  HV103_03 1.0
    @202  HV104_03 1.0
    @203  HV105_03 2.0
    @205  HV106_03 1.0
    @206  HV107_03 2.0
    @208  HV108_03 2.0
    @210  HV109_03 1.0
    @211  HV110_03 1.0
    @212  HV111_03 1.0
    @213  HV112_03 2.0
    @215  HV113_03 1.0
    @216  HV114_03 2.0
    @218  HV115_03 1.0
    @219  HV116_03 1.0
    @220  HV117_03 1.0
    @221  HV118_03 1.0
    @222  HVIDX_04 2.0
    @224  HV101_04 2.0
    @226  HV102_04 1.0
    @227  HV103_04 1.0
    @228  HV104_04 1.0
    @229  HV105_04 2.0
    @231  HV106_04 1.0
    @232  HV107_04 2.0
    @234  HV108_04 2.0
    @236  HV109_04 1.0
    @237  HV110_04 1.0
    @238  HV111_04 1.0
    @239  HV112_04 2.0
    @241  HV113_04 1.0
    @242  HV114_04 2.0
    @244  HV115_04 1.0
    @245  HV116_04 1.0
    @246  HV117_04 1.0
    @247  HV118_04 1.0
    @248  HVIDX_05 2.0
    @250  HV101_05 2.0
    @252  HV102_05 1.0
    @253  HV103_05 1.0
    @254  HV104_05 1.0
    @255  HV105_05 2.0
    @257  HV106_05 1.0
    @258  HV107_05 2.0
    @260  HV108_05 2.0
    @262  HV109_05 1.0
    @263  HV110_05 1.0
    @264  HV111_05 1.0
    @265  HV112_05 2.0
    @267  HV113_05 1.0
    @268  HV114_05 2.0
    @270  HV115_05 1.0
    @271  HV116_05 1.0
    @272  HV117_05 1.0
    @273  HV118_05 1.0
    @274  HVIDX_06 2.0
    @276  HV101_06 2.0
    @278  HV102_06 1.0
    @279  HV103_06 1.0
    @280  HV104_06 1.0
    @281  HV105_06 2.0
    @283  HV106_06 1.0
    @284  HV107_06 2.0
    @286  HV108_06 2.0
    @288  HV109_06 1.0
    @289  HV110_06 1.0
    @290  HV111_06 1.0
    @291  HV112_06 2.0
    @293  HV113_06 1.0
    @294  HV114_06 2.0
    @296  HV115_06 1.0
    @297  HV116_06 1.0
    @298  HV117_06 1.0
    @299  HV118_06 1.0
    @300  HVIDX_07 2.0
    @302  HV101_07 2.0
    @304  HV102_07 1.0
    @305  HV103_07 1.0
    @306  HV104_07 1.0
    @307  HV105_07 2.0
    @309  HV106_07 1.0
    @310  HV107_07 2.0
    @312  HV108_07 2.0
    @314  HV109_07 1.0
    @315  HV110_07 1.0
    @316  HV111_07 1.0
    @317  HV112_07 2.0
    @319  HV113_07 1.0
    @320  HV114_07 2.0
    @322  HV115_07 1.0
    @323  HV116_07 1.0
    @324  HV117_07 1.0
    @325  HV118_07 1.0
    @326  HVIDX_08 2.0
    @328  HV101_08 2.0
    @330  HV102_08 1.0
    @331  HV103_08 1.0
    @332  HV104_08 1.0
    @333  HV105_08 2.0
    @335  HV106_08 1.0
    @336  HV107_08 2.0
    @338  HV108_08 2.0
    @340  HV109_08 1.0
    @341  HV110_08 1.0
    @342  HV111_08 1.0
    @343  HV112_08 2.0
    @345  HV113_08 1.0
    @346  HV114_08 2.0
    @348  HV115_08 1.0
    @349  HV116_08 1.0
    @350  HV117_08 1.0
    @351  HV118_08 1.0
    @352  HVIDX_09 2.0
    @354  HV101_09 2.0
    @356  HV102_09 1.0
    @357  HV103_09 1.0
    @358  HV104_09 1.0
    @359  HV105_09 2.0
    @361  HV106_09 1.0
    @362  HV107_09 2.0
    @364  HV108_09 2.0
    @366  HV109_09 1.0
    @367  HV110_09 1.0
    @368  HV111_09 1.0
    @369  HV112_09 2.0
    @371  HV113_09 1.0
    @372  HV114_09 2.0
    @374  HV115_09 1.0
    @375  HV116_09 1.0
    @376  HV117_09 1.0
    @377  HV118_09 1.0
    @378  HVIDX_10 2.0
    @380  HV101_10 2.0
    @382  HV102_10 1.0
    @383  HV103_10 1.0
    @384  HV104_10 1.0
    @385  HV105_10 2.0
    @387  HV106_10 1.0
    @388  HV107_10 2.0
    @390  HV108_10 2.0
    @392  HV109_10 1.0
    @393  HV110_10 1.0
    @394  HV111_10 1.0
    @395  HV112_10 2.0
    @397  HV113_10 1.0
    @398  HV114_10 2.0
    @400  HV115_10 1.0
    @401  HV116_10 1.0
    @402  HV117_10 1.0
    @403  HV118_10 1.0
    @404  HVIDX_11 2.0
    @406  HV101_11 2.0
    @408  HV102_11 1.0
    @409  HV103_11 1.0
    @410  HV104_11 1.0
    @411  HV105_11 2.0
    @413  HV106_11 1.0
    @414  HV107_11 2.0
    @416  HV108_11 2.0
    @418  HV109_11 1.0
    @419  HV110_11 1.0
    @420  HV111_11 1.0
    @421  HV112_11 2.0
    @423  HV113_11 1.0
    @424  HV114_11 2.0
    @426  HV115_11 1.0
    @427  HV116_11 1.0
    @428  HV117_11 1.0
    @429  HV118_11 1.0
    @430  HVIDX_12 2.0
    @432  HV101_12 2.0
    @434  HV102_12 1.0
    @435  HV103_12 1.0
    @436  HV104_12 1.0
    @437  HV105_12 2.0
    @439  HV106_12 1.0
    @440  HV107_12 2.0
    @442  HV108_12 2.0
    @444  HV109_12 1.0
    @445  HV110_12 1.0
    @446  HV111_12 1.0
    @447  HV112_12 2.0
    @449  HV113_12 1.0
    @450  HV114_12 2.0
    @452  HV115_12 1.0
    @453  HV116_12 1.0
    @454  HV117_12 1.0
    @455  HV118_12 1.0
    @456  HVIDX_13 2.0
    @458  HV101_13 2.0
    @460  HV102_13 1.0
    @461  HV103_13 1.0
    @462  HV104_13 1.0
    @463  HV105_13 2.0
    @465  HV106_13 1.0
    @466  HV107_13 2.0
    @468  HV108_13 2.0
    @470  HV109_13 1.0
    @471  HV110_13 1.0
    @472  HV111_13 1.0
    @473  HV112_13 2.0
    @475  HV113_13 1.0
    @476  HV114_13 2.0
    @478  HV115_13 1.0
    @479  HV116_13 1.0
    @480  HV117_13 1.0
    @481  HV118_13 1.0
    @482  HVIDX_14 2.0
    @484  HV101_14 2.0
    @486  HV102_14 1.0
    @487  HV103_14 1.0
    @488  HV104_14 1.0
    @489  HV105_14 2.0
    @491  HV106_14 1.0
    @492  HV107_14 2.0
    @494  HV108_14 2.0
    @496  HV109_14 1.0
    @497  HV110_14 1.0
    @498  HV111_14 1.0
    @499  HV112_14 2.0
    @501  HV113_14 1.0
    @502  HV114_14 2.0
    @504  HV115_14 1.0
    @505  HV116_14 1.0
    @506  HV117_14 1.0
    @507  HV118_14 1.0
    @508  HVIDX_15 2.0
    @510  HV101_15 2.0
    @512  HV102_15 1.0
    @513  HV103_15 1.0
    @514  HV104_15 1.0
    @515  HV105_15 2.0
    @517  HV106_15 1.0
    @518  HV107_15 2.0
    @520  HV108_15 2.0
    @522  HV109_15 1.0
    @523  HV110_15 1.0
    @524  HV111_15 1.0
    @525  HV112_15 2.0
    @527  HV113_15 1.0
    @528  HV114_15 2.0
    @530  HV115_15 1.0
    @531  HV116_15 1.0
    @532  HV117_15 1.0
    @533  HV118_15 1.0
    @534  HVIDX_16 2.0
    @536  HV101_16 2.0
    @538  HV102_16 1.0
    @539  HV103_16 1.0
    @540  HV104_16 1.0
    @541  HV105_16 2.0
    @543  HV106_16 1.0
    @544  HV107_16 2.0
    @546  HV108_16 2.0
    @548  HV109_16 1.0
    @549  HV110_16 1.0
    @550  HV111_16 1.0
    @551  HV112_16 2.0
    @553  HV113_16 1.0
    @554  HV114_16 2.0
    @556  HV115_16 1.0
    @557  HV116_16 1.0
    @558  HV117_16 1.0
    @559  HV118_16 1.0
    @560  HVIDX_17 2.0
    @562  HV101_17 2.0
    @564  HV102_17 1.0
    @565  HV103_17 1.0
    @566  HV104_17 1.0
    @567  HV105_17 2.0
    @569  HV106_17 1.0
    @570  HV107_17 2.0
    @572  HV108_17 2.0
    @574  HV109_17 1.0
    @575  HV110_17 1.0
    @576  HV111_17 1.0
    @577  HV112_17 2.0
    @579  HV113_17 1.0
    @580  HV114_17 2.0
    @582  HV115_17 1.0
    @583  HV116_17 1.0
    @584  HV117_17 1.0
    @585  HV118_17 1.0
    @586  HVIDX_18 2.0
    @588  HV101_18 2.0
    @590  HV102_18 1.0
    @591  HV103_18 1.0
    @592  HV104_18 1.0
    @593  HV105_18 2.0
    @595  HV106_18 1.0
    @596  HV107_18 2.0
    @598  HV108_18 2.0
    @600  HV109_18 1.0
    @601  HV110_18 1.0
    @602  HV111_18 1.0
    @603  HV112_18 2.0
    @605  HV113_18 1.0
    @606  HV114_18 2.0
    @608  HV115_18 1.0
    @609  HV116_18 1.0
    @610  HV117_18 1.0
    @611  HV118_18 1.0
    @612  HVIDX_19 2.0
    @614  HV101_19 2.0
    @616  HV102_19 1.0
    @617  HV103_19 1.0
    @618  HV104_19 1.0
    @619  HV105_19 2.0
    @621  HV106_19 1.0
    @622  HV107_19 2.0
    @624  HV108_19 2.0
    @626  HV109_19 1.0
    @627  HV110_19 1.0
    @628  HV111_19 1.0
    @629  HV112_19 2.0
    @631  HV113_19 1.0
    @632  HV114_19 2.0
    @634  HV115_19 1.0
    @635  HV116_19 1.0
    @636  HV117_19 1.0
    @637  HV118_19 1.0
    @638  HVIDX_20 2.0
    @640  HV101_20 2.0
    @642  HV102_20 1.0
    @643  HV103_20 1.0
    @644  HV104_20 1.0
    @645  HV105_20 2.0
    @647  HV106_20 1.0
    @648  HV107_20 2.0
    @650  HV108_20 2.0
    @652  HV109_20 1.0
    @653  HV110_20 1.0
    @654  HV111_20 1.0
    @655  HV112_20 2.0
    @657  HV113_20 1.0
    @658  HV114_20 2.0
    @660  HV115_20 1.0
    @661  HV116_20 1.0
    @662  HV117_20 1.0
    @663  HV118_20 1.0
    @664  HVIDX_21 2.0
    @666  HV101_21 2.0
    @668  HV102_21 1.0
    @669  HV103_21 1.0
    @670  HV104_21 1.0
    @671  HV105_21 2.0
    @673  HV106_21 1.0
    @674  HV107_21 2.0
    @676  HV108_21 2.0
    @678  HV109_21 1.0
    @679  HV110_21 1.0
    @680  HV111_21 1.0
    @681  HV112_21 2.0
    @683  HV113_21 1.0
    @684  HV114_21 2.0
    @686  HV115_21 1.0
    @687  HV116_21 1.0
    @688  HV117_21 1.0
    @689  HV118_21 1.0
    @690  HVIDX_22 2.0
    @692  HV101_22 2.0
    @694  HV102_22 1.0
    @695  HV103_22 1.0
    @696  HV104_22 1.0
    @697  HV105_22 2.0
    @699  HV106_22 1.0
    @700  HV107_22 2.0
    @702  HV108_22 2.0
    @704  HV109_22 1.0
    @705  HV110_22 1.0
    @706  HV111_22 1.0
    @707  HV112_22 2.0
    @709  HV113_22 1.0
    @710  HV114_22 2.0
    @712  HV115_22 1.0
    @713  HV116_22 1.0
    @714  HV117_22 1.0
    @715  HV118_22 1.0
    @716  HVIDX_23 2.0
    @718  HV101_23 2.0
    @720  HV102_23 1.0
    @721  HV103_23 1.0
    @722  HV104_23 1.0
    @723  HV105_23 2.0
    @725  HV106_23 1.0
    @726  HV107_23 2.0
    @728  HV108_23 2.0
    @730  HV109_23 1.0
    @731  HV110_23 1.0
    @732  HV111_23 1.0
    @733  HV112_23 2.0
    @735  HV113_23 1.0
    @736  HV114_23 2.0
    @738  HV115_23 1.0
    @739  HV116_23 1.0
    @740  HV117_23 1.0
    @741  HV118_23 1.0
    @742  HVIDX_24 2.0
    @744  HV101_24 2.0
    @746  HV102_24 1.0
    @747  HV103_24 1.0
    @748  HV104_24 1.0
    @749  HV105_24 2.0
    @751  HV106_24 1.0
    @752  HV107_24 2.0
    @754  HV108_24 2.0
    @756  HV109_24 1.0
    @757  HV110_24 1.0
    @758  HV111_24 1.0
    @759  HV112_24 2.0
    @761  HV113_24 1.0
    @762  HV114_24 2.0
    @764  HV115_24 1.0
    @765  HV116_24 1.0
    @766  HV117_24 1.0
    @767  HV118_24 1.0
    @768  HVIDX_25 2.0
    @770  HV101_25 2.0
    @772  HV102_25 1.0
    @773  HV103_25 1.0
    @774  HV104_25 1.0
    @775  HV105_25 2.0
    @777  HV106_25 1.0
    @778  HV107_25 2.0
    @780  HV108_25 2.0
    @782  HV109_25 1.0
    @783  HV110_25 1.0
    @784  HV111_25 1.0
    @785  HV112_25 2.0
    @787  HV113_25 1.0
    @788  HV114_25 2.0
    @790  HV115_25 1.0
    @791  HV116_25 1.0
    @792  HV117_25 1.0
    @793  HV118_25 1.0
    @794  HVIDX_26 2.0
    @796  HV101_26 2.0
    @798  HV102_26 1.0
    @799  HV103_26 1.0
    @800  HV104_26 1.0
    @801  HV105_26 2.0
    @803  HV106_26 1.0
    @804  HV107_26 2.0
    @806  HV108_26 2.0
    @808  HV109_26 1.0
    @809  HV110_26 1.0
    @810  HV111_26 1.0
    @811  HV112_26 2.0
    @813  HV113_26 1.0
    @814  HV114_26 2.0
    @816  HV115_26 1.0
    @817  HV116_26 1.0
    @818  HV117_26 1.0
    @819  HV118_26 1.0
    @820  HVIDX_27 2.0
    @822  HV101_27 2.0
    @824  HV102_27 1.0
    @825  HV103_27 1.0
    @826  HV104_27 1.0
    @827  HV105_27 2.0
    @829  HV106_27 1.0
    @830  HV107_27 2.0
    @832  HV108_27 2.0
    @834  HV109_27 1.0
    @835  HV110_27 1.0
    @836  HV111_27 1.0
    @837  HV112_27 2.0
    @839  HV113_27 1.0
    @840  HV114_27 2.0
    @842  HV115_27 1.0
    @843  HV116_27 1.0
    @844  HV117_27 1.0
    @845  HV118_27 1.0
    @846  HVIDX_28 2.0
    @848  HV101_28 2.0
    @850  HV102_28 1.0
    @851  HV103_28 1.0
    @852  HV104_28 1.0
    @853  HV105_28 2.0
    @855  HV106_28 1.0
    @856  HV107_28 2.0
    @858  HV108_28 2.0
    @860  HV109_28 1.0
    @861  HV110_28 1.0
    @862  HV111_28 1.0
    @863  HV112_28 2.0
    @865  HV113_28 1.0
    @866  HV114_28 2.0
    @868  HV115_28 1.0
    @869  HV116_28 1.0
    @870  HV117_28 1.0
    @871  HV118_28 1.0
    @872  SHDISTR  2.0
    @874  SH20     3.0
    @877  SH20A    3.0
    @880  SH25     2.0
    @882  SH26A    1.0
    @883  SH26B    1.0
    @884  SH26C    1.0
    @885  SH26D    1.0
    @886  SH26I    1.0
    @887  SH31     1.0
    @888  IDXH4_01 2.0
    @890  SH14_01  1.0
    @891  SH17_01  2.0
    @893  IDXH4_02 2.0
    @895  SH14_02  1.0
    @896  SH17_02  2.0
    @898  IDXH4_03 2.0
    @900  SH14_03  1.0
    @901  SH17_03  2.0
    @903  IDXH4_04 2.0
    @905  SH14_04  1.0
    @906  SH17_04  2.0
    @908  IDXH4_05 2.0
    @910  SH14_05  1.0
    @911  SH17_05  2.0
    @913  IDXH4_06 2.0
    @915  SH14_06  1.0
    @916  SH17_06  2.0
    @918  IDXH4_07 2.0
    @920  SH14_07  1.0
    @921  SH17_07  2.0
    @923  IDXH4_08 2.0
    @925  SH14_08  1.0
    @926  SH17_08  2.0
    @928  IDXH4_09 2.0
    @930  SH14_09  1.0
    @931  SH17_09  2.0
    @933  IDXH4_10 2.0
    @935  SH14_10  1.0
    @936  SH17_10  2.0
    @938  IDXH4_11 2.0
    @940  SH14_11  1.0
    @941  SH17_11  2.0
    @943  IDXH4_12 2.0
    @945  SH14_12  1.0
    @946  SH17_12  2.0
    @948  IDXH4_13 2.0
    @950  SH14_13  1.0
    @951  SH17_13  2.0
    @953  IDXH4_14 2.0
    @955  SH14_14  1.0
    @956  SH17_14  2.0
    @958  IDXH4_15 2.0
    @960  SH14_15  1.0
    @961  SH17_15  2.0
    @963  IDXH4_16 2.0
    @965  SH14_16  1.0
    @966  SH17_16  2.0
    @968  IDXH4_17 2.0
    @970  SH14_17  1.0
    @971  SH17_17  2.0
    @973  IDXH4_18 2.0
    @975  SH14_18  1.0
    @976  SH17_18  2.0
    @978  IDXH4_19 2.0
    @980  SH14_19  1.0
    @981  SH17_19  2.0
    @983  IDXH4_20 2.0
    @985  SH14_20  1.0
    @986  SH17_20  2.0
    @988  IDXH4_21 2.0
    @990  SH14_21  1.0
    @991  SH17_21  2.0
    @993  IDXH4_22 2.0
    @995  SH14_22  1.0
    @996  SH17_22  2.0
    @998  IDXH4_23 2.0
    @1000 SH14_23  1.0
    @1001 SH17_23  2.0
    @1003 IDXH4_24 2.0
    @1005 SH14_24  1.0
    @1006 SH17_24  2.0
    @1008 IDXH4_25 2.0
    @1010 SH14_25  1.0
    @1011 SH17_25  2.0
    @1013 IDXH4_26 2.0
    @1015 SH14_26  1.0
    @1016 SH17_26  2.0
    @1018 IDXH4_27 2.0
    @1020 SH14_27  1.0
    @1021 SH17_27  2.0
    @1023 IDXH4_28 2.0
    @1025 SH14_28  1.0
    @1026 SH17_28  2.0
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
    if HV101_01 =     99 then HV101_01 = .;
    if HV102_01 =      9 then HV102_01 = .;
    if HV103_01 =      9 then HV103_01 = .;
    if HV104_01 =      9 then HV104_01 = .;
    if HV105_01 =     99 then HV105_01 = .;
    if HV106_01 =      9 then HV106_01 = .;
    if HV107_01 =     99 then HV107_01 = .;
    if HV108_01 =     99 then HV108_01 = .;
    if HV109_01 =      9 then HV109_01 = .;
    if HV110_01 =      9 then HV110_01 = .;
    if HV111_01 =      9 then HV111_01 = .;
    if HV112_01 =     99 then HV112_01 = .;
    if HV113_01 =      9 then HV113_01 = .;
    if HV114_01 =     99 then HV114_01 = .;
    if HV115_01 =      9 then HV115_01 = .;
    if HV116_01 =      9 then HV116_01 = .;
    if HV101_02 =     99 then HV101_02 = .;
    if HV102_02 =      9 then HV102_02 = .;
    if HV103_02 =      9 then HV103_02 = .;
    if HV104_02 =      9 then HV104_02 = .;
    if HV105_02 =     99 then HV105_02 = .;
    if HV106_02 =      9 then HV106_02 = .;
    if HV107_02 =     99 then HV107_02 = .;
    if HV108_02 =     99 then HV108_02 = .;
    if HV109_02 =      9 then HV109_02 = .;
    if HV110_02 =      9 then HV110_02 = .;
    if HV111_02 =      9 then HV111_02 = .;
    if HV112_02 =     99 then HV112_02 = .;
    if HV113_02 =      9 then HV113_02 = .;
    if HV114_02 =     99 then HV114_02 = .;
    if HV115_02 =      9 then HV115_02 = .;
    if HV116_02 =      9 then HV116_02 = .;
    if HV101_03 =     99 then HV101_03 = .;
    if HV102_03 =      9 then HV102_03 = .;
    if HV103_03 =      9 then HV103_03 = .;
    if HV104_03 =      9 then HV104_03 = .;
    if HV105_03 =     99 then HV105_03 = .;
    if HV106_03 =      9 then HV106_03 = .;
    if HV107_03 =     99 then HV107_03 = .;
    if HV108_03 =     99 then HV108_03 = .;
    if HV109_03 =      9 then HV109_03 = .;
    if HV110_03 =      9 then HV110_03 = .;
    if HV111_03 =      9 then HV111_03 = .;
    if HV112_03 =     99 then HV112_03 = .;
    if HV113_03 =      9 then HV113_03 = .;
    if HV114_03 =     99 then HV114_03 = .;
    if HV115_03 =      9 then HV115_03 = .;
    if HV116_03 =      9 then HV116_03 = .;
    if HV101_04 =     99 then HV101_04 = .;
    if HV102_04 =      9 then HV102_04 = .;
    if HV103_04 =      9 then HV103_04 = .;
    if HV104_04 =      9 then HV104_04 = .;
    if HV105_04 =     99 then HV105_04 = .;
    if HV106_04 =      9 then HV106_04 = .;
    if HV107_04 =     99 then HV107_04 = .;
    if HV108_04 =     99 then HV108_04 = .;
    if HV109_04 =      9 then HV109_04 = .;
    if HV110_04 =      9 then HV110_04 = .;
    if HV111_04 =      9 then HV111_04 = .;
    if HV112_04 =     99 then HV112_04 = .;
    if HV113_04 =      9 then HV113_04 = .;
    if HV114_04 =     99 then HV114_04 = .;
    if HV115_04 =      9 then HV115_04 = .;
    if HV116_04 =      9 then HV116_04 = .;
    if HV101_05 =     99 then HV101_05 = .;
    if HV102_05 =      9 then HV102_05 = .;
    if HV103_05 =      9 then HV103_05 = .;
    if HV104_05 =      9 then HV104_05 = .;
    if HV105_05 =     99 then HV105_05 = .;
    if HV106_05 =      9 then HV106_05 = .;
    if HV107_05 =     99 then HV107_05 = .;
    if HV108_05 =     99 then HV108_05 = .;
    if HV109_05 =      9 then HV109_05 = .;
    if HV110_05 =      9 then HV110_05 = .;
    if HV111_05 =      9 then HV111_05 = .;
    if HV112_05 =     99 then HV112_05 = .;
    if HV113_05 =      9 then HV113_05 = .;
    if HV114_05 =     99 then HV114_05 = .;
    if HV115_05 =      9 then HV115_05 = .;
    if HV116_05 =      9 then HV116_05 = .;
    if HV101_06 =     99 then HV101_06 = .;
    if HV102_06 =      9 then HV102_06 = .;
    if HV103_06 =      9 then HV103_06 = .;
    if HV104_06 =      9 then HV104_06 = .;
    if HV105_06 =     99 then HV105_06 = .;
    if HV106_06 =      9 then HV106_06 = .;
    if HV107_06 =     99 then HV107_06 = .;
    if HV108_06 =     99 then HV108_06 = .;
    if HV109_06 =      9 then HV109_06 = .;
    if HV110_06 =      9 then HV110_06 = .;
    if HV111_06 =      9 then HV111_06 = .;
    if HV112_06 =     99 then HV112_06 = .;
    if HV113_06 =      9 then HV113_06 = .;
    if HV114_06 =     99 then HV114_06 = .;
    if HV115_06 =      9 then HV115_06 = .;
    if HV116_06 =      9 then HV116_06 = .;
    if HV101_07 =     99 then HV101_07 = .;
    if HV102_07 =      9 then HV102_07 = .;
    if HV103_07 =      9 then HV103_07 = .;
    if HV104_07 =      9 then HV104_07 = .;
    if HV105_07 =     99 then HV105_07 = .;
    if HV106_07 =      9 then HV106_07 = .;
    if HV107_07 =     99 then HV107_07 = .;
    if HV108_07 =     99 then HV108_07 = .;
    if HV109_07 =      9 then HV109_07 = .;
    if HV110_07 =      9 then HV110_07 = .;
    if HV111_07 =      9 then HV111_07 = .;
    if HV112_07 =     99 then HV112_07 = .;
    if HV113_07 =      9 then HV113_07 = .;
    if HV114_07 =     99 then HV114_07 = .;
    if HV115_07 =      9 then HV115_07 = .;
    if HV116_07 =      9 then HV116_07 = .;
    if HV101_08 =     99 then HV101_08 = .;
    if HV102_08 =      9 then HV102_08 = .;
    if HV103_08 =      9 then HV103_08 = .;
    if HV104_08 =      9 then HV104_08 = .;
    if HV105_08 =     99 then HV105_08 = .;
    if HV106_08 =      9 then HV106_08 = .;
    if HV107_08 =     99 then HV107_08 = .;
    if HV108_08 =     99 then HV108_08 = .;
    if HV109_08 =      9 then HV109_08 = .;
    if HV110_08 =      9 then HV110_08 = .;
    if HV111_08 =      9 then HV111_08 = .;
    if HV112_08 =     99 then HV112_08 = .;
    if HV113_08 =      9 then HV113_08 = .;
    if HV114_08 =     99 then HV114_08 = .;
    if HV115_08 =      9 then HV115_08 = .;
    if HV116_08 =      9 then HV116_08 = .;
    if HV101_09 =     99 then HV101_09 = .;
    if HV102_09 =      9 then HV102_09 = .;
    if HV103_09 =      9 then HV103_09 = .;
    if HV104_09 =      9 then HV104_09 = .;
    if HV105_09 =     99 then HV105_09 = .;
    if HV106_09 =      9 then HV106_09 = .;
    if HV107_09 =     99 then HV107_09 = .;
    if HV108_09 =     99 then HV108_09 = .;
    if HV109_09 =      9 then HV109_09 = .;
    if HV110_09 =      9 then HV110_09 = .;
    if HV111_09 =      9 then HV111_09 = .;
    if HV112_09 =     99 then HV112_09 = .;
    if HV113_09 =      9 then HV113_09 = .;
    if HV114_09 =     99 then HV114_09 = .;
    if HV115_09 =      9 then HV115_09 = .;
    if HV116_09 =      9 then HV116_09 = .;
    if HV101_10 =     99 then HV101_10 = .;
    if HV102_10 =      9 then HV102_10 = .;
    if HV103_10 =      9 then HV103_10 = .;
    if HV104_10 =      9 then HV104_10 = .;
    if HV105_10 =     99 then HV105_10 = .;
    if HV106_10 =      9 then HV106_10 = .;
    if HV107_10 =     99 then HV107_10 = .;
    if HV108_10 =     99 then HV108_10 = .;
    if HV109_10 =      9 then HV109_10 = .;
    if HV110_10 =      9 then HV110_10 = .;
    if HV111_10 =      9 then HV111_10 = .;
    if HV112_10 =     99 then HV112_10 = .;
    if HV113_10 =      9 then HV113_10 = .;
    if HV114_10 =     99 then HV114_10 = .;
    if HV115_10 =      9 then HV115_10 = .;
    if HV116_10 =      9 then HV116_10 = .;
    if HV101_11 =     99 then HV101_11 = .;
    if HV102_11 =      9 then HV102_11 = .;
    if HV103_11 =      9 then HV103_11 = .;
    if HV104_11 =      9 then HV104_11 = .;
    if HV105_11 =     99 then HV105_11 = .;
    if HV106_11 =      9 then HV106_11 = .;
    if HV107_11 =     99 then HV107_11 = .;
    if HV108_11 =     99 then HV108_11 = .;
    if HV109_11 =      9 then HV109_11 = .;
    if HV110_11 =      9 then HV110_11 = .;
    if HV111_11 =      9 then HV111_11 = .;
    if HV112_11 =     99 then HV112_11 = .;
    if HV113_11 =      9 then HV113_11 = .;
    if HV114_11 =     99 then HV114_11 = .;
    if HV115_11 =      9 then HV115_11 = .;
    if HV116_11 =      9 then HV116_11 = .;
    if HV101_12 =     99 then HV101_12 = .;
    if HV102_12 =      9 then HV102_12 = .;
    if HV103_12 =      9 then HV103_12 = .;
    if HV104_12 =      9 then HV104_12 = .;
    if HV105_12 =     99 then HV105_12 = .;
    if HV106_12 =      9 then HV106_12 = .;
    if HV107_12 =     99 then HV107_12 = .;
    if HV108_12 =     99 then HV108_12 = .;
    if HV109_12 =      9 then HV109_12 = .;
    if HV110_12 =      9 then HV110_12 = .;
    if HV111_12 =      9 then HV111_12 = .;
    if HV112_12 =     99 then HV112_12 = .;
    if HV113_12 =      9 then HV113_12 = .;
    if HV114_12 =     99 then HV114_12 = .;
    if HV115_12 =      9 then HV115_12 = .;
    if HV116_12 =      9 then HV116_12 = .;
    if HV101_13 =     99 then HV101_13 = .;
    if HV102_13 =      9 then HV102_13 = .;
    if HV103_13 =      9 then HV103_13 = .;
    if HV104_13 =      9 then HV104_13 = .;
    if HV105_13 =     99 then HV105_13 = .;
    if HV106_13 =      9 then HV106_13 = .;
    if HV107_13 =     99 then HV107_13 = .;
    if HV108_13 =     99 then HV108_13 = .;
    if HV109_13 =      9 then HV109_13 = .;
    if HV110_13 =      9 then HV110_13 = .;
    if HV111_13 =      9 then HV111_13 = .;
    if HV112_13 =     99 then HV112_13 = .;
    if HV113_13 =      9 then HV113_13 = .;
    if HV114_13 =     99 then HV114_13 = .;
    if HV115_13 =      9 then HV115_13 = .;
    if HV116_13 =      9 then HV116_13 = .;
    if HV101_14 =     99 then HV101_14 = .;
    if HV102_14 =      9 then HV102_14 = .;
    if HV103_14 =      9 then HV103_14 = .;
    if HV104_14 =      9 then HV104_14 = .;
    if HV105_14 =     99 then HV105_14 = .;
    if HV106_14 =      9 then HV106_14 = .;
    if HV107_14 =     99 then HV107_14 = .;
    if HV108_14 =     99 then HV108_14 = .;
    if HV109_14 =      9 then HV109_14 = .;
    if HV110_14 =      9 then HV110_14 = .;
    if HV111_14 =      9 then HV111_14 = .;
    if HV112_14 =     99 then HV112_14 = .;
    if HV113_14 =      9 then HV113_14 = .;
    if HV114_14 =     99 then HV114_14 = .;
    if HV115_14 =      9 then HV115_14 = .;
    if HV116_14 =      9 then HV116_14 = .;
    if HV101_15 =     99 then HV101_15 = .;
    if HV102_15 =      9 then HV102_15 = .;
    if HV103_15 =      9 then HV103_15 = .;
    if HV104_15 =      9 then HV104_15 = .;
    if HV105_15 =     99 then HV105_15 = .;
    if HV106_15 =      9 then HV106_15 = .;
    if HV107_15 =     99 then HV107_15 = .;
    if HV108_15 =     99 then HV108_15 = .;
    if HV109_15 =      9 then HV109_15 = .;
    if HV110_15 =      9 then HV110_15 = .;
    if HV111_15 =      9 then HV111_15 = .;
    if HV112_15 =     99 then HV112_15 = .;
    if HV113_15 =      9 then HV113_15 = .;
    if HV114_15 =     99 then HV114_15 = .;
    if HV115_15 =      9 then HV115_15 = .;
    if HV116_15 =      9 then HV116_15 = .;
    if HV101_16 =     99 then HV101_16 = .;
    if HV102_16 =      9 then HV102_16 = .;
    if HV103_16 =      9 then HV103_16 = .;
    if HV104_16 =      9 then HV104_16 = .;
    if HV105_16 =     99 then HV105_16 = .;
    if HV106_16 =      9 then HV106_16 = .;
    if HV107_16 =     99 then HV107_16 = .;
    if HV108_16 =     99 then HV108_16 = .;
    if HV109_16 =      9 then HV109_16 = .;
    if HV110_16 =      9 then HV110_16 = .;
    if HV111_16 =      9 then HV111_16 = .;
    if HV112_16 =     99 then HV112_16 = .;
    if HV113_16 =      9 then HV113_16 = .;
    if HV114_16 =     99 then HV114_16 = .;
    if HV115_16 =      9 then HV115_16 = .;
    if HV116_16 =      9 then HV116_16 = .;
    if HV101_17 =     99 then HV101_17 = .;
    if HV102_17 =      9 then HV102_17 = .;
    if HV103_17 =      9 then HV103_17 = .;
    if HV104_17 =      9 then HV104_17 = .;
    if HV105_17 =     99 then HV105_17 = .;
    if HV106_17 =      9 then HV106_17 = .;
    if HV107_17 =     99 then HV107_17 = .;
    if HV108_17 =     99 then HV108_17 = .;
    if HV109_17 =      9 then HV109_17 = .;
    if HV110_17 =      9 then HV110_17 = .;
    if HV111_17 =      9 then HV111_17 = .;
    if HV112_17 =     99 then HV112_17 = .;
    if HV113_17 =      9 then HV113_17 = .;
    if HV114_17 =     99 then HV114_17 = .;
    if HV115_17 =      9 then HV115_17 = .;
    if HV116_17 =      9 then HV116_17 = .;
    if HV101_18 =     99 then HV101_18 = .;
    if HV102_18 =      9 then HV102_18 = .;
    if HV103_18 =      9 then HV103_18 = .;
    if HV104_18 =      9 then HV104_18 = .;
    if HV105_18 =     99 then HV105_18 = .;
    if HV106_18 =      9 then HV106_18 = .;
    if HV107_18 =     99 then HV107_18 = .;
    if HV108_18 =     99 then HV108_18 = .;
    if HV109_18 =      9 then HV109_18 = .;
    if HV110_18 =      9 then HV110_18 = .;
    if HV111_18 =      9 then HV111_18 = .;
    if HV112_18 =     99 then HV112_18 = .;
    if HV113_18 =      9 then HV113_18 = .;
    if HV114_18 =     99 then HV114_18 = .;
    if HV115_18 =      9 then HV115_18 = .;
    if HV116_18 =      9 then HV116_18 = .;
    if HV101_19 =     99 then HV101_19 = .;
    if HV102_19 =      9 then HV102_19 = .;
    if HV103_19 =      9 then HV103_19 = .;
    if HV104_19 =      9 then HV104_19 = .;
    if HV105_19 =     99 then HV105_19 = .;
    if HV106_19 =      9 then HV106_19 = .;
    if HV107_19 =     99 then HV107_19 = .;
    if HV108_19 =     99 then HV108_19 = .;
    if HV109_19 =      9 then HV109_19 = .;
    if HV110_19 =      9 then HV110_19 = .;
    if HV111_19 =      9 then HV111_19 = .;
    if HV112_19 =     99 then HV112_19 = .;
    if HV113_19 =      9 then HV113_19 = .;
    if HV114_19 =     99 then HV114_19 = .;
    if HV115_19 =      9 then HV115_19 = .;
    if HV116_19 =      9 then HV116_19 = .;
    if HV101_20 =     99 then HV101_20 = .;
    if HV102_20 =      9 then HV102_20 = .;
    if HV103_20 =      9 then HV103_20 = .;
    if HV104_20 =      9 then HV104_20 = .;
    if HV105_20 =     99 then HV105_20 = .;
    if HV106_20 =      9 then HV106_20 = .;
    if HV107_20 =     99 then HV107_20 = .;
    if HV108_20 =     99 then HV108_20 = .;
    if HV109_20 =      9 then HV109_20 = .;
    if HV110_20 =      9 then HV110_20 = .;
    if HV111_20 =      9 then HV111_20 = .;
    if HV112_20 =     99 then HV112_20 = .;
    if HV113_20 =      9 then HV113_20 = .;
    if HV114_20 =     99 then HV114_20 = .;
    if HV115_20 =      9 then HV115_20 = .;
    if HV116_20 =      9 then HV116_20 = .;
    if HV101_21 =     99 then HV101_21 = .;
    if HV102_21 =      9 then HV102_21 = .;
    if HV103_21 =      9 then HV103_21 = .;
    if HV104_21 =      9 then HV104_21 = .;
    if HV105_21 =     99 then HV105_21 = .;
    if HV106_21 =      9 then HV106_21 = .;
    if HV107_21 =     99 then HV107_21 = .;
    if HV108_21 =     99 then HV108_21 = .;
    if HV109_21 =      9 then HV109_21 = .;
    if HV110_21 =      9 then HV110_21 = .;
    if HV111_21 =      9 then HV111_21 = .;
    if HV112_21 =     99 then HV112_21 = .;
    if HV113_21 =      9 then HV113_21 = .;
    if HV114_21 =     99 then HV114_21 = .;
    if HV115_21 =      9 then HV115_21 = .;
    if HV116_21 =      9 then HV116_21 = .;
    if HV101_22 =     99 then HV101_22 = .;
    if HV102_22 =      9 then HV102_22 = .;
    if HV103_22 =      9 then HV103_22 = .;
    if HV104_22 =      9 then HV104_22 = .;
    if HV105_22 =     99 then HV105_22 = .;
    if HV106_22 =      9 then HV106_22 = .;
    if HV107_22 =     99 then HV107_22 = .;
    if HV108_22 =     99 then HV108_22 = .;
    if HV109_22 =      9 then HV109_22 = .;
    if HV110_22 =      9 then HV110_22 = .;
    if HV111_22 =      9 then HV111_22 = .;
    if HV112_22 =     99 then HV112_22 = .;
    if HV113_22 =      9 then HV113_22 = .;
    if HV114_22 =     99 then HV114_22 = .;
    if HV115_22 =      9 then HV115_22 = .;
    if HV116_22 =      9 then HV116_22 = .;
    if HV101_23 =     99 then HV101_23 = .;
    if HV102_23 =      9 then HV102_23 = .;
    if HV103_23 =      9 then HV103_23 = .;
    if HV104_23 =      9 then HV104_23 = .;
    if HV105_23 =     99 then HV105_23 = .;
    if HV106_23 =      9 then HV106_23 = .;
    if HV107_23 =     99 then HV107_23 = .;
    if HV108_23 =     99 then HV108_23 = .;
    if HV109_23 =      9 then HV109_23 = .;
    if HV110_23 =      9 then HV110_23 = .;
    if HV111_23 =      9 then HV111_23 = .;
    if HV112_23 =     99 then HV112_23 = .;
    if HV113_23 =      9 then HV113_23 = .;
    if HV114_23 =     99 then HV114_23 = .;
    if HV115_23 =      9 then HV115_23 = .;
    if HV116_23 =      9 then HV116_23 = .;
    if HV101_24 =     99 then HV101_24 = .;
    if HV102_24 =      9 then HV102_24 = .;
    if HV103_24 =      9 then HV103_24 = .;
    if HV104_24 =      9 then HV104_24 = .;
    if HV105_24 =     99 then HV105_24 = .;
    if HV106_24 =      9 then HV106_24 = .;
    if HV107_24 =     99 then HV107_24 = .;
    if HV108_24 =     99 then HV108_24 = .;
    if HV109_24 =      9 then HV109_24 = .;
    if HV110_24 =      9 then HV110_24 = .;
    if HV111_24 =      9 then HV111_24 = .;
    if HV112_24 =     99 then HV112_24 = .;
    if HV113_24 =      9 then HV113_24 = .;
    if HV114_24 =     99 then HV114_24 = .;
    if HV115_24 =      9 then HV115_24 = .;
    if HV116_24 =      9 then HV116_24 = .;
    if HV101_25 =     99 then HV101_25 = .;
    if HV102_25 =      9 then HV102_25 = .;
    if HV103_25 =      9 then HV103_25 = .;
    if HV104_25 =      9 then HV104_25 = .;
    if HV105_25 =     99 then HV105_25 = .;
    if HV106_25 =      9 then HV106_25 = .;
    if HV107_25 =     99 then HV107_25 = .;
    if HV108_25 =     99 then HV108_25 = .;
    if HV109_25 =      9 then HV109_25 = .;
    if HV110_25 =      9 then HV110_25 = .;
    if HV111_25 =      9 then HV111_25 = .;
    if HV112_25 =     99 then HV112_25 = .;
    if HV113_25 =      9 then HV113_25 = .;
    if HV114_25 =     99 then HV114_25 = .;
    if HV115_25 =      9 then HV115_25 = .;
    if HV116_25 =      9 then HV116_25 = .;
    if HV101_26 =     99 then HV101_26 = .;
    if HV102_26 =      9 then HV102_26 = .;
    if HV103_26 =      9 then HV103_26 = .;
    if HV104_26 =      9 then HV104_26 = .;
    if HV105_26 =     99 then HV105_26 = .;
    if HV106_26 =      9 then HV106_26 = .;
    if HV107_26 =     99 then HV107_26 = .;
    if HV108_26 =     99 then HV108_26 = .;
    if HV109_26 =      9 then HV109_26 = .;
    if HV110_26 =      9 then HV110_26 = .;
    if HV111_26 =      9 then HV111_26 = .;
    if HV112_26 =     99 then HV112_26 = .;
    if HV113_26 =      9 then HV113_26 = .;
    if HV114_26 =     99 then HV114_26 = .;
    if HV115_26 =      9 then HV115_26 = .;
    if HV116_26 =      9 then HV116_26 = .;
    if HV101_27 =     99 then HV101_27 = .;
    if HV102_27 =      9 then HV102_27 = .;
    if HV103_27 =      9 then HV103_27 = .;
    if HV104_27 =      9 then HV104_27 = .;
    if HV105_27 =     99 then HV105_27 = .;
    if HV106_27 =      9 then HV106_27 = .;
    if HV107_27 =     99 then HV107_27 = .;
    if HV108_27 =     99 then HV108_27 = .;
    if HV109_27 =      9 then HV109_27 = .;
    if HV110_27 =      9 then HV110_27 = .;
    if HV111_27 =      9 then HV111_27 = .;
    if HV112_27 =     99 then HV112_27 = .;
    if HV113_27 =      9 then HV113_27 = .;
    if HV114_27 =     99 then HV114_27 = .;
    if HV115_27 =      9 then HV115_27 = .;
    if HV116_27 =      9 then HV116_27 = .;
    if HV101_28 =     99 then HV101_28 = .;
    if HV102_28 =      9 then HV102_28 = .;
    if HV103_28 =      9 then HV103_28 = .;
    if HV104_28 =      9 then HV104_28 = .;
    if HV105_28 =     99 then HV105_28 = .;
    if HV106_28 =      9 then HV106_28 = .;
    if HV107_28 =     99 then HV107_28 = .;
    if HV108_28 =     99 then HV108_28 = .;
    if HV109_28 =      9 then HV109_28 = .;
    if HV110_28 =      9 then HV110_28 = .;
    if HV111_28 =      9 then HV111_28 = .;
    if HV112_28 =     99 then HV112_28 = .;
    if HV113_28 =      9 then HV113_28 = .;
    if HV114_28 =     99 then HV114_28 = .;
    if HV115_28 =      9 then HV115_28 = .;
    if HV116_28 =      9 then HV116_28 = .;
    if SH20     =    999 then SH20 = .;
    if SH20A    =    999 then SH20A = .;
    if SH25     =     99 then SH25 = .;
    if SH26A    =      9 then SH26A = .;
    if SH26B    =      9 then SH26B = .;
    if SH26C    =      9 then SH26C = .;
    if SH26D    =      9 then SH26D = .;
    if SH26I    =      9 then SH26I = .;
    if SH31     =      9 then SH31 = .;
    if SH14_01  =      9 then SH14_01 = .;
    if SH14_02  =      9 then SH14_02 = .;
    if SH14_03  =      9 then SH14_03 = .;
    if SH14_04  =      9 then SH14_04 = .;
    if SH14_05  =      9 then SH14_05 = .;
    if SH14_06  =      9 then SH14_06 = .;
    if SH14_07  =      9 then SH14_07 = .;
    if SH14_08  =      9 then SH14_08 = .;
    if SH14_09  =      9 then SH14_09 = .;
    if SH14_10  =      9 then SH14_10 = .;
    if SH14_11  =      9 then SH14_11 = .;
    if SH14_12  =      9 then SH14_12 = .;
    if SH14_13  =      9 then SH14_13 = .;
    if SH14_14  =      9 then SH14_14 = .;
    if SH14_15  =      9 then SH14_15 = .;
    if SH14_16  =      9 then SH14_16 = .;
    if SH14_17  =      9 then SH14_17 = .;
    if SH14_18  =      9 then SH14_18 = .;
    if SH14_19  =      9 then SH14_19 = .;
    if SH14_20  =      9 then SH14_20 = .;
    if SH14_21  =      9 then SH14_21 = .;
    if SH14_22  =      9 then SH14_22 = .;
    if SH14_23  =      9 then SH14_23 = .;
    if SH14_24  =      9 then SH14_24 = .;
    if SH14_25  =      9 then SH14_25 = .;
    if SH14_26  =      9 then SH14_26 = .;
    if SH14_27  =      9 then SH14_27 = .;
    if SH14_28  =      9 then SH14_28 = .;
