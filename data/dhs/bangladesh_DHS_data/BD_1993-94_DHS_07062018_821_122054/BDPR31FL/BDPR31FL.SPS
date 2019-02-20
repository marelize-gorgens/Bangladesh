DATA LIST FILE='BDPR31FL.DAT' RECORDS=1
/
 HHID        1-12   (A)
 HV000      13-15   (A)
 HV001      16-23  
 HV002      24-27  
 HV003      28-30  
 HV004      31-34  
 HV005      35-42  
 HV006      43-44  
 HV007      45-46  
 HV008      47-50  
 HV009      51-52  
 HV010      53-54  
 HV011      55-56  
 HV012      57-58  
 HV013      59-60  
 HV014      61-62  
 HV015      63-63  
 HV016      64-65  
 HV017      66-66  
 HV018      67-69  
 HV019      70-71  
 HV020      72-72  
 HV021      73-76  
 HV022      77-80  
 HV023      81-82  
 HV024      83-84  
 HV025      85-85  
 HV026      86-86  
 HV027      87-87  
 HV028      88-95  
 HV030      96-97  
 HV031      98-99  
 HV032     100-101 
 HV033     102-109 
 HV201     110-111 
 HV202     112-113 
 HV203     114-114 
 HV204     115-117 
 HV205     118-119 
 HV206     120-120 
 HV207     121-121 
 HV208     122-122 
 HV209     123-123 
 HV210     124-124 
 HV211     125-125 
 HV212     126-126 
 HV213     127-128 
 HV214     129-130 
 HV215     131-132 
 HV216     133-134 
 HV217     135-135 
 HV218     136-137 
 HV219     138-138 
 HV220     139-140 
 HV221     141-141 
 HV222     142-143 
 SHDISTR   144-145 
 SH20      146-148 
 SH20A     149-151 
 SH25      152-153 
 SH26A     154-154 
 SH26B     155-155 
 SH26C     156-156 
 SH26D     157-157 
 SH26I     158-158 
 SH31      159-159 
 HVIDX     160-161 
 HV101     162-163 
 HV102     164-164 
 HV103     165-165 
 HV104     166-166 
 HV105     167-168 
 HV106     169-169 
 HV107     170-171 
 HV108     172-173 
 HV109     174-174 
 HV110     175-175 
 HV111     176-176 
 HV112     177-178 
 HV113     179-179 
 HV114     180-181 
 HV115     182-182 
 HV116     183-183 
 HV117     184-184 
 HV118     185-185 
 IDXH4     186-187 
 SH14      188-188 
 SH17      189-190 
.
VARIABLE LABELS
 HHID     "Case Identification"
/HV000    "Country code and phase"
/HV001    "Cluster number"
/HV002    "Household number"
/HV003    "Respondent's line number"
/HV004    "Ultimate area unit"
/HV005    "Sample weight"
/HV006    "Month of interview"
/HV007    "Year of interview"
/HV008    "Date of interview (CMC)"
/HV009    "Number of household members"
/HV010    "Number of eligible women in HH"
/HV011    "Number of eligible men in HH"
/HV012    "Number of de jure members"
/HV013    "Number of de facto members"
/HV014    "Number of children 5 and under"
/HV015    "Result of household interview"
/HV016    "Day of interview"
/HV017    "Number of visits"
/HV018    "Interviewer identification"
/HV019    "Keyer identification"
/HV020    "Ever-married sample"
/HV021    "Primary sampling unit"
/HV022    "Sample stratum number"
/HV023    "Sample domain"
/HV024    "Region"
/HV025    "Type of place of residence"
/HV026    "Place of residence"
/HV027    "Selection for male/husb. int."
/HV028    "Sample weight for male subsamp"
/HV030    "Field supervisor -NA"
/HV031    "Field editor -NA"
/HV032    "Office editor -NA"
/HV033    "Ultimate area selection prob."
/HV201    "Source of drinking water"
/HV202    "Source of non-drinking water"
/HV203    "Same source of water"
/HV204    "Time to get to water source"
/HV205    "Type of toilet facility"
/HV206    "Has electricity"
/HV207    "Has radio"
/HV208    "Has television"
/HV209    "Has refrigerator -NA"
/HV210    "Has bicycle"
/HV211    "Has motorcycle -NA"
/HV212    "Has car -NA"
/HV213    "Main floor material"
/HV214    "Main wall material"
/HV215    "Main roof material"
/HV216    "Rooms for sleeping"
/HV217    "Relationship structure"
/HV218    "Line number of head of househ."
/HV219    "Sex of head of household"
/HV220    "Age of head of household"
/HV221    "Has telephone -NA"
/HV222    "Type of salt used for cook -NA"
/SHDISTR  "District of residence"
/SH20     "Time to get to water source"
/SH20A    "Time to wait to get water:mins"
/SH25     "Toilet facility : children"
/SH26A    "Almirah"
/SH26B    "Table/chair/bench"
/SH26C    "Watch/clock"
/SH26D    "Cot/bed"
/SH26I    "Agricultural land"
/SH31     "Urban slum"
/HVIDX    "Line number"
/HV101    "Relationship to head"
/HV102    "Usual resident"
/HV103    "Slept last night"
/HV104    "Sex of household member"
/HV105    "Age of household members"
/HV106    "Highest educational level"
/HV107    "Highest year of education"
/HV108    "Education in single years"
/HV109    "Educational attainment"
/HV110    "Member still in school"
/HV111    "Mother alive -NA"
/HV112    "Mother's line number -NA"
/HV113    "Father alive -NA"
/HV114    "Father's line number -NA"
/HV115    "Current marital status -NA"
/HV116    "Currently, formerly, never m."
/HV117    "Eligibility for female intview"
/HV118    "Eligibility for male interview"
/IDXH4    "Index to household schedule"
/SH14     "Works for money"
/SH17     "Husbands line number"
.
MISSING VALUE
 HV018    (999)
/HV019    (99)
/HV026    (9)
/HV030    (99)
/HV031    (99)
/HV032    (99)
/HV201    (99)
/HV202    (99)
/HV203    (9)
/HV204    (999)
/HV205    (99)
/HV206    (9)
/HV207    (9)
/HV208    (9)
/HV209    (9)
/HV210    (9)
/HV211    (9)
/HV212    (9)
/HV213    (99)
/HV214    (99)
/HV215    (99)
/HV216    (99)
/HV219    (9)
/HV220    (99)
/HV221    (9)
/HV222    (99)
/SH20     (999)
/SH20A    (999)
/SH25     (99)
/SH26A    (9)
/SH26B    (9)
/SH26C    (9)
/SH26D    (9)
/SH26I    (9)
/SH31     (9)
/HV101    (99)
/HV102    (9)
/HV103    (9)
/HV104    (9)
/HV105    (99)
/HV106    (9)
/HV107    (99)
/HV108    (99)
/HV109    (9)
/HV110    (9)
/HV111    (9)
/HV112    (99)
/HV113    (9)
/HV114    (99)
/HV115    (9)
/HV116    (9)
/SH14     (9)
.
VALUE LABELS
 HV015   
     1 "Completed"
     2 "HH present, no resp"
     3 "HH absent"
     4 "Postponed"
     5 "Refused"
     6 "Dwelling vacant"
     7 "Dwelling destroyed"
     8 "Dwelling not found"
     9 "Other"
/HV020   
     0 "All woman sample"
     1 "Ever married sample"
/HV023   
     0 "National"
     1 "Country specific"
/HV024   
     1 "Barishal"
     2 "Chittagong"
     3 "Dhaka"
     4 "Khulna"
     5 "Rajshani"
/HV025   
     1 "Urban"
     2 "Rural"
/HV026   
     0 "Dhaka/Chittagong"
     1 "Small city"
     2 "Town"
     3 "Village"
/HV027   
     0 "Not selected"
     1 "Men's survey"
     2 "Husband's survey"
/HV201   
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
/HV202   
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
/HV203   
     0 "No"
     1 "Yes"
/HV204   
   996 "On premises"
/HV205   
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
/HV206   
     0 "No"
     1 "Yes"
/HV207   
     0 "No"
     1 "Yes"
/HV208   
     0 "No"
     1 "Yes"
/HV210   
     0 "No"
     1 "Yes"
/HV213   
    10 "NATURAL"
    11 "Earth"
    20 "RUDIMENTARY"
    21 "Wood"
    30 "FINISHED"
    31 "Cement/concrete"
    96 "OTHER"
/HV214   
    10 "NATURAL"
    11 "Jute/bamboo/mud"
    20 "RUDIMENTARY"
    21 "Wood"
    30 "FINISHED"
    31 "Brick/cement"
    32 "Tin"
    96 "OTHER"
/HV215   
    10 "NATURAL"
    11 "Jute/bamboo/mud"
    20 "RUDIMENTARY"
    21 "Tin"
    30 "FINISHED"
    31 "Cement/concrete"
    96 "OTHER"
/HV217   
     0 "No adults"
     1 "One adult"
     2 "Two adults, opp. sex"
     3 "Two adults, same sex"
     4 "Three+ related adult"
     5 "Unrelated adults"
/HV219   
     1 "Male"
     2 "Female"
/HV220   
    97 "97+"
    98 "DK"
/SH20    
   996 "On premises"
/SH25    
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
/SH26A   
     0 "No"
     1 "Yes"
/SH26B   
     0 "No"
     1 "Yes"
/SH26C   
     0 "No"
     1 "Yes"
/SH26D   
     0 "No"
     1 "Yes"
/SH26I   
     0 "No"
     1 "Yes"
/SH31    
     0 "No"
     1 "Yes"
/HV101   
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
/HV102   
     0 "No"
     1 "Yes"
/HV103   
     0 "No"
     1 "Yes"
/HV104   
     1 "Male"
     2 "Female"
/HV105   
    96 "96 or older"
    98 "DK"
/HV106   
     0 "No education"
     1 "Primary"
     2 "Secondary"
     3 "Higher"
     8 "DK"
/HV107   
    98 "DK"
/HV108   
    97 "Inconsistent"
    98 "DK"
/HV109   
     0 "No education"
     1 "Incomplete primary"
     2 "Complete primary"
     3 "Incomplete secondary"
     4 "Complete secondary"
     5 "Higher"
     8 "DK"
/HV110   
     0 "No"
     1 "Yes"
/HV116   
     0 "Never married"
     1 "Currently married"
     2 "Formerly/ever marr."
/HV117   
     0 "Not eligible"
     1 "Eligible"
/HV118   
     0 "Not eligible"
     1 "Eligible"
/SH14    
     0 "No"
     1 "Yes"
     8 "Don't know"
/SH17    
     0 "Not marr./husb away"
.
