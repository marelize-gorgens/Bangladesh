libname user ' ';
Proc format;
  value V013_
     0 = "10-14"
     1 = "15-19"
     2 = "20-24"
     3 = "25-29"
     4 = "30-34"
     5 = "35-39"
     6 = "40-44"
     7 = "45-49"
     ;
  value V014_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     4 = "Y & age - y ignored"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value V015_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partly completed"
     6 = "Incapacitated"
     7 = "Other"
     ;
  value V018_
     0 = "No calendar"
     ;
  value V019_
     0 = "No calendar"
     ;
  value V020_
     0 = "All woman sample"
     1 = "Ever married sample"
     ;
  value V023_
     0 = "National"
     1 = "Country specific"
     ;
  value V024_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value V025_
     1 = "Urban"
     2 = "Rural"
     ;
  value V026_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value V034_
     0 = "Husband not in HH"
     ;
  value V101_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value V102_
     1 = "Urban"
     2 = "Rural"
     ;
  value V104_
    95 = "Always"
    96 = "Visitor"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value V105_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value V106_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     ;
  value V108_
     1 = "Reads easily"
     2 = "Reads with difficult"
     3 = "Cannot read"
     ;
  value V109_
     0 = "No"
     1 = "Yes"
     ;
  value V110_
     0 = "No"
     1 = "Yes"
     ;
  value V112_
     0 = "No"
     1 = "Yes"
     ;
  value V113_
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
  value V114_
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
  value V115_
   996 = "On premises"
     ;
  value V116_
    10 = "FLUSH TOILET"
    20 = "PIT TOILET LATRINE"
    30 = "NO FACILITY"
    96 = "OTHER"
     ;
  value V119_
     0 = "No"
     1 = "Yes"
     ;
  value V120_
     0 = "No"
     1 = "Yes"
     ;
  value V121_
     0 = "No"
     1 = "Yes"
     ;
  value V123_
     0 = "No"
     1 = "Yes"
     ;
  value V127_
    10 = "NATURAL"
    11 = "Earth"
    20 = "RUDIMENTARY"
    21 = "Wood"
    30 = "FINISHED"
    31 = "Cement/concrete"
    96 = "OTHER"
     ;
  value V128_
    10 = "NATURAL"
    11 = "Jute/bamboo/mud"
    20 = "RUDIMENTARY"
    21 = "Wood"
    30 = "FINISHED"
    31 = "Brick/cement"
    32 = "Tin"
    96 = "OTHER"
     ;
  value V129_
    10 = "NATURAL"
    11 = "Jute/bamboo/mud"
    20 = "RUDIMENTARY"
    21 = "Tin"
    30 = "FINISHED"
    31 = "Cement/concrete"
    96 = "OTHER"
     ;
  value V130_
     1 = "Islam"
     2 = "Christianity"
     3 = "Hinduism"
     4 = "Buddhism"
     5 = "Other"
     ;
  value V133_
    97 = "Inconsistent"
     ;
  value V134_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value V135_
     1 = "Usual resident"
     2 = "Visitor"
     ;
  value V139_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value V140_
     0 = "Abroad"
     1 = "Urban"
     2 = "Rural"
     ;
  value V141_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value V142_
     0 = "No"
     1 = "Yes"
     ;
  value V148_
     0 = "No"
     1 = "Yes"
     ;
  value V149_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     ;
  value V150_
     1 = "Head"
     2 = "Wife"
     3 = "Daughter"
     4 = "Daughter-in-law"
     5 = "Grand-daughter"
     6 = "Mother"
     7 = "Mother-in-law"
     8 = "Sister"
     9 = "Co-spouse"
    10 = "Other relative"
    11 = "Adopted/foster child"
    12 = "Not related"
    98 = "DK"
     ;
  value V151_
     1 = "Male"
     2 = "Female"
     ;
  value V152_
    96 = "96+"
     ;
  value B0_
     0 = "Single birth"
     1 = "1st of multiple"
     2 = "2nd of multiple"
     3 = "3rd of multiple"
     4 = "4th of multiple"
     5 = "5th of multiple"
     ;
  value B4_
     1 = "Male"
     2 = "Female"
     ;
  value B5_
     0 = "No"
     1 = "Yes"
     ;
  value B6_
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value B9_
     0 = "Respondent"
     1 = "Father"
     2 = "Other relative"
     3 = "Someone else"
     4 = "Lives elsewhere"
     ;
  value B10_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     4 = "Y & age - y ignored"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value B13_
     0 = "No flag"
     1 = "> interview"
     2 = "< breastfeeding"
     3 = "< age supplemented"
     4 = "< first breastfed"
     5 = "< last vaccination"
     6 = "Outside range"
     7 = "Imputed, units given"
     8 = "Imputed, no units"
     ;
  value V213_
     0 = "No or unsure"
     1 = "Yes"
     ;
  value V215_
   994 = "In menopause"
   995 = "Before last birth"
   996 = "Never menstruated"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value V216_
     0 = "No"
     1 = "Yes"
     ;
  value V220_
     6 = "6+"
     ;
  value V221_
   996 = "Negative interval"
     ;
  value V223_
     7 = "Month - exact date"
     8 = "None - date placed"
     ;
  value V225_
     1 = "Then"
     2 = "Later"
     3 = "Not at all"
     ;
  value V226_
   994 = "In menopause"
   995 = "Before last birth"
   996 = "Never menstruated"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value V227_
     0 = "No flag"
     1 = "N > int. since birth"
     2 = "N + amen. > interval"
     3 = "N, but per. not ret."
     4 = "95, but no birth"
     5 = "96, but per.returned"
     6 = "95, but per.returned"
     7 = "N, last per. in preg"
     8 = "96, had children"
     9 = "N, per.bef.last birt"
     ;
  value V228_
     0 = "No"
     1 = "Yes"
     ;
  value V229_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value V230_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value V231_
  9997 = "Inconsistent"
  9998 = "DK"
     ;
  value V232_
     1 = "Month and year"
     2 = "From calendar"
     5 = "Year"
     7 = "Month"
     8 = "None"
     ;
  value V233_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value V234_
     0 = "No"
     1 = "Yes"
     ;
  value V235_
     0 = "No prior child"
     ;
  value V301_
     0 = "Knows no method"
     1 = "Knows only folkloric"
     2 = "Knows only trad. mth"
     3 = "Knows modern method"
     ;
  value V302_
     0 = "Never used"
     1 = "Used only folkloric"
     2 = "Used only trad. meth"
     3 = "Used modern method"
     ;
  value V304A_
     1 = "Modern method"
     2 = "Traditional method"
     3 = "Folkloric method"
     ;
  value V304_
     0 = "No"
     1 = "Yes, spontaneously"
     2 = "Yes, probed"
     8 = "Not asked"
     ;
  value V305_
     0 = "No"
     1 = "Yes"
     ;
  value V311_
     4 = "4+"
     5 = "Never used"
     ;
  value V312_
     0 = "Not using"
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
     ;
  value V313_
     0 = "No method"
     1 = "Folkloric method"
     2 = "Traditional method"
     3 = "Modern method"
     ;
  value V318_
     1 = "Month and year"
     5 = "Year - m imp, a calc"
     7 = "Month - date placed"
     8 = "None - date placed"
     ;
  value V319_
     1 = "<2"
     2 = "2-3"
     3 = "4-5"
     4 = "6-7"
     5 = "8-9"
     6 = "10+"
     ;
  value V320_
     1 = "<25"
     2 = "25-29"
     3 = "30-34"
     4 = "35-39"
     5 = "40-44"
     6 = "45-49"
     ;
  value V321_
     0 = "Single,before marr."
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25+"
     ;
  value V322_
     5 = "5+"
     ;
  value V323_
     1 = "Maya"
     2 = "Ovacon"
     3 = "Norquest"
     4 = "Noriday"
     5 = "Ovral"
     6 = "Combination-5"
     7 = "Ovostat"
     8 = "Restovar-NA"
     9 = "Lyndiol"
    10 = "Nordette"
    11 = "Marvelou"
    96 = "Other pill brand"
     ;
  value V326_
    10 = "PUBLIC"
    11 = "Hospital/Medical clg"
    12 = "Family welfare cent."
    13 = "Thana health complex"
    14 = "Satellite clinic"
    20 = "PRIVATE MEDICAL"
    21 = "Private clinic,doct."
    22 = "Traditional doctor"
    23 = "Pharmacy"
    30 = "OTHER PRIVATE"
    31 = "Shop"
    32 = "Friends, relatives"
    41 = "Fieldworker, FWA"
    96 = "OTHER"
    98 = "DK"
     ;
  value V327_
     1 = "Govt Clinical/Pharm"
     2 = "Govt Home/Comm deliv"
     3 = "NGO"
     4 = "Private Clin/Deliv"
     5 = "Private Pharmacy"
     6 = "Shop, church, friend"
     7 = "Other"
     8 = "Don't know"
     ;
  value V337_
    95 = "All cal. {A core}"
    96 = "96+ {B core}"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value V359_
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
     ;
  value V360_
     1 = "Became pregnant"
     2 = "Wanted to become prg"
     3 = "Husband disapproved"
     4 = "Side effects"
     5 = "Health concerns"
     6 = "Access, availability"
     7 = "Wanted more eff.meth"
     8 = "Inconvenient to use"
     9 = "Infreq.sex,husb away"
    10 = "Cost"
    11 = "Fatalistic"
    12 = "Diff.preg.,menopause"
    13 = "Marital dissolution"
    14 = "Other"
    98 = "Don't know"
     ;
  value V361_
     1 = "Currently using"
     2 = "Used since last brth"
     3 = "Used before last brt"
     4 = "Never used"
     ;
  value V362_
     1 = "In next 12 months"
     2 = "Use later"
     3 = "Unsure about timing"
     4 = "Unsure about use"
     5 = "Does not intend"
     6 = "Never had sex"
     ;
  value V363_
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
    98 = "Don't know"
     ;
  value V364_
     1 = "Using modern method"
     2 = "Using tradit. method"
     3 = "Non-user intend to"
     4 = "Does not intend to"
     5 = "Never had sex"
     ;
  value V366A_
     0 = "Not acceptable"
     1 = "Acceptable"
     8 = "Don't know"
     ;
  value V367_
     1 = "Wanted then"
     2 = "Wanted later"
     3 = "Wanted no more"
     ;
  value V369_
     0 = "Never used"
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
     ;
  value V372_
     0 = "Package not seen"
     1 = "Package seen"
     ;
  value V375A_
    11 = "Not married"
    20 = "FERTILITY-RELATED"
    21 = "Not having sex"
    22 = "Infrequent sex"
    23 = "Menopausal, hyster."
    24 = "Subfecund, infecund"
    25 = "Postpartum, breastf."
    26 = "Wants more children"
    27 = "Pregnant"
    30 = "OPPOSITION TO USE"
    31 = "Respondent opposed"
    32 = "Husband opposed"
    33 = "Others opposed"
    34 = "Religious prohibit."
    35 = "Fatalistic"
    40 = "LACK OF KNOWLEDGE"
    41 = "Knows no method"
    42 = "Knows no source"
    50 = "METHOD-RELATED"
    51 = "Health concerns"
    52 = "Fear side effects"
    53 = "Lack of access"
    54 = "Cost too much"
    55 = "Inconvenient to use"
    56 = "Interfere with body"
    96 = "Other"
    98 = "DK"
     ;
  value V376_
    11 = "Not married"
    20 = "FERTILITY-RELATED"
    22 = "Infrequent sex"
    23 = "Menopausal, hyster."
    24 = "Subfecund, infecund"
    26 = "Wants more children"
    30 = "OPPOSITION TO USE"
    31 = "Respondent opposed"
    32 = "Husband opposed"
    33 = "Others opposed"
    34 = "Religious prohibit."
    35 = "Fatalistic"
    40 = "LACK OF KNOWLEDGE"
    41 = "Knows no method"
    42 = "Knows no source"
    50 = "METHOD-RELATED"
    51 = "Health concerns"
    52 = "Fear side effects"
    53 = "Lack of access"
    54 = "Cost too much"
    55 = "Inconvenient to use"
    56 = "Interfere with body"
    96 = "Other"
    98 = "DK"
     ;
  value V379_
    10 = "PUBLIC"
    11 = "Hospital/Medical clg"
    12 = "Family welfare cent."
    13 = "Thana health complex"
    14 = "Satellite clinic"
    20 = "PRIVATE MEDICAL"
    21 = "Private clinic,doct."
    22 = "Traditional doctor"
    23 = "Pharmacy"
    30 = "OTHER PRIVATE"
    31 = "Shop"
    32 = "Friends, relatives"
    41 = "Fieldworker, FWA"
    96 = "OTHER"
    98 = "DK"
     ;
  value V380_
     1 = "Govt Clinical/Pharm"
     2 = "Govt Home/Comm deliv"
     3 = "NGO"
     4 = "Private Clin/Deliv"
     5 = "Private Pharmacy"
     6 = "Shop, church, friend"
     7 = "Other"
     8 = "Don't know"
     ;
  value V384A_
     0 = "No"
     1 = "Yes"
     ;
  value V384B_
     0 = "No"
     1 = "Yes"
     ;
  value V384D_
     0 = "No"
     1 = "Yes"
     ;
  value V393_
     0 = "No"
     1 = "Yes - 6 months"
     8 = "Don't know"
     ;
  value V394_
     0 = "No"
     1 = "Yes"
     ;
  value V395_
     0 = "No"
     1 = "Yes"
     ;
  value V396_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M1_
     0 = "Received no inject."
     7 = "7+"
     8 = "Don't know"
     ;
  value M2A_
     0 = "No"
     1 = "Yes: doctor"
     ;
  value M2B_
     0 = "No"
     1 = "Yes: nurse/midwife"
     ;
  value M2D_
     0 = "No"
     1 = "Yes: FW Visitor ."
     ;
  value M2F_
     0 = "No"
     1 = "Yes: trained birth A"
     ;
  value M2G_
     0 = "No"
     1 = "Yes: trad.birth att."
     ;
  value M2K_
     0 = "No"
     1 = "Yes: other responses"
     ;
  value M2N_
     0 = "No: some care"
     1 = "Yes: no one"
     ;
  value M3A_
     0 = "No"
     1 = "Yes: doctor"
     ;
  value M3B_
     0 = "No"
     1 = "Yes: nurse/midwife"
     ;
  value M3D_
     0 = "No"
     1 = "Yes: FW Visitor ."
     ;
  value M3F_
     0 = "No"
     1 = "Yes: trained birth A"
     ;
  value M3G_
     0 = "No"
     1 = "Yes: trad.birth att."
     ;
  value M3H_
     0 = "No"
     1 = "Yes: relative,friend"
     ;
  value M3K_
     0 = "No"
     1 = "Yes: other responses"
     ;
  value M3M_
     0 = "No"
     1 = "Yes: CS other"
     ;
  value M3N_
     0 = "No: some assistance"
     1 = "Yes: no one"
     ;
  value M4_
    94 = "Never breastfed"
    95 = "Still breastfeeding"
    96 = "Breastfed until died"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value M5_
    94 = "Never breastfed"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value M6_
    96 = "Period not returned"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value M7_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value M8_
    96 = "Still abstaining"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value M9_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value M10_
     1 = "Then"
     2 = "Later"
     3 = "No more"
     ;
  value M13_
    98 = "DK"
     ;
  value M14_
     0 = "No antenatal visits"
    98 = "DK"
     ;
  value M15_
    10 = "HOMES"
    11 = "Respondents home"
    12 = "Other home"
    20 = "PUBLIC SECTOR"
    21 = "Govt. hospital"
    22 = "Govt. health center"
    23 = "Govt. health post"
    26 = "Other public"
    30 = "PRIVATE SECTOR"
    31 = "Private hosp/clinic"
    36 = "Other private medica"
    96 = "OTHER"
     ;
  value M21_
     1 = "Mother ill, weak"
     2 = "Child ill, weak"
     3 = "Child died"
     4 = "Nipple, breast prob."
     5 = "Insufficient milk"
     6 = "Working"
     7 = "Child refused"
     8 = "Weaning age"
     9 = "Became pregnant"
    10 = "Started using method"
    96 = "Other"
    97 = "Missing ever breastf"
     ;
  value M27_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 mnth"
     3 = "> age at death"
     4 = "During preg in cal."
     ;
  value M28_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 mnth"
     4 = "During preg in cal."
     ;
  value M29_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 mnth"
     4 = "During preg in cal."
     ;
  value M34_
     0 = "Immediately"
   101 = "Hours: 1"
   201 = "Days: 1"
     ;
  value M35_
    96 = "On demand"
     ;
  value M36_
    96 = "On demand"
     ;
  value M37A_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37B_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37C_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37D_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37F_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37G_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37L_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37P_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value M37T_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value V401_
     0 = "No"
     1 = "Yes"
     ;
  value V404_
     0 = "No"
     1 = "Yes"
     ;
  value V405_
     0 = "No"
     1 = "Yes"
     ;
  value V406_
     0 = "No"
     1 = "Yes"
     ;
  value V407_
    96 = "On demand"
     ;
  value V408_
    96 = "On demand"
     ;
  value V409_
     0 = "No"
     1 = "Yes"
     ;
  value V409A_
     0 = "No"
     1 = "Yes"
     ;
  value V410_
     0 = "No"
     1 = "Yes"
     ;
  value V410A_
     0 = "No"
     1 = "Yes"
     ;
  value V411A_
     0 = "No"
     1 = "Yes"
     ;
  value V412_
     0 = "No"
     1 = "Yes"
     ;
  value V413_
     0 = "No"
     1 = "Yes"
     ;
  value V414D_
     0 = "No"
     1 = "Yes"
     ;
  value V414E_
     0 = "No"
     1 = "Yes"
     ;
  value V414F_
     0 = "No"
     1 = "Yes"
     ;
  value V414G_
     0 = "No"
     1 = "Yes"
     ;
  value V414H_
     0 = "No"
     1 = "Yes"
     ;
  value V416_
     0 = "Never heard of"
     1 = "Used ORS"
     2 = "Heard of ORS"
     ;
  value V426_
     0 = "Immediately"
     ;
  value H1_
     0 = "No card"
     1 = "Yes, seen"
     2 = "Yes, not seen"
     3 = "No longer has card"
     ;
  value H2_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H2D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H2M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H2Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H3_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H3D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H3M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H3Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H4_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H4D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H4M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H4Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H5_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H5D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H5M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H5Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H6_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H6D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H6M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H6Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H7_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H7D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H7M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H7Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H8_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H8D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H8M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H8Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H9_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value H9D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H9M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H9Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H0D_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H0M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H0Y_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value H10_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value H11_
     0 = "No"
     1 = "Yes, last 24 hours"
     2 = "Yes, last two weeks"
     8 = "Don't know"
     ;
  value H11B_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value H12A_
     0 = "No"
     1 = "Government hospital"
     ;
  value H12B_
     0 = "No"
     1 = "Govt. health center"
     ;
  value H12E_
     0 = "No"
     1 = "Comm.health worker"
     ;
  value H12F_
     0 = "No"
     1 = "Satellite clinic"
     ;
  value H12J_
     0 = "No"
     1 = "Private hosp/clinic"
     ;
  value H12K_
     0 = "No"
     1 = "Private pharmacy"
     ;
  value H12L_
     0 = "No"
     1 = "Private doctor"
     ;
  value H12S_
     0 = "No"
     1 = "Shop"
     ;
  value H12T_
     0 = "No"
     1 = "Traditional pract."
     ;
  value H12X_
     0 = "No"
     1 = "Other"
     ;
  value H12Y_
     0 = "No: received treatm."
     1 = "Yes: no treatment"
     ;
  value H12Z_
     0 = "No"
     1 = "Yes: medical treatm."
     ;
  value H13_
     0 = "No"
     1 = "Yes: ORS - spontan."
     2 = "Yes: ORS - probed"
     8 = "Don't know"
     ;
  value H14_
     0 = "No"
     1 = "Yes: RHS - spontan."
     2 = "Yes: RHS - probed"
     8 = "Don't know"
     ;
  value H15_
     0 = "No"
     1 = "Yes: oth pill, syrup"
     ;
  value H15B_
     0 = "No"
     1 = "Yes: injection"
     ;
  value H15C_
     0 = "No"
     1 = "Yes: intravenous (IV"
     ;
  value H15D_
     0 = "No"
     1 = "Yes: home rem, herb."
     ;
  value H16_
     0 = "Same"
     1 = "Increase"
     2 = "Decrease"
     8 = "DK"
     ;
  value H20_
     0 = "No"
     1 = "Yes"
     ;
  value H21A_
     0 = "No: received treatm."
     1 = "Yes: no treatment"
     8 = "DK"
     ;
  value H21_
     0 = "No"
     1 = "Yes"
     ;
  value H31_
     0 = "No"
     1 = "Yes, last 24 hours"
     2 = "Yes, last two weeks"
     8 = "Don't know"
     ;
  value H31B_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value H32A_
     0 = "No"
     1 = "Government hospital"
     ;
  value H32B_
     0 = "No"
     1 = "Govt. health center"
     ;
  value H32E_
     0 = "No"
     1 = "Community health wrk"
     ;
  value H32F_
     0 = "No"
     1 = "Satellite clinic"
     ;
  value H32J_
     0 = "No"
     1 = "Private hosp./clinic"
     ;
  value H32K_
     0 = "No"
     1 = "Private pharmacy"
     ;
  value H32L_
     0 = "No"
     1 = "Private doctor"
     ;
  value H32S_
     0 = "No"
     1 = "Shop"
     ;
  value H32T_
     0 = "No"
     1 = "Traditional pract."
     ;
  value H32X_
     0 = "No"
     1 = "Other"
     ;
  value H32Y_
     0 = "No: received treatm."
     1 = "Yes: no treatment"
     ;
  value H32Z_
     0 = "No"
     1 = "Yes: medical treatm."
     ;
  value V501_
     0 = "Never married"
     1 = "Married"
     2 = "Living together"
     3 = "Widowed"
     4 = "Divorced"
     5 = "Not living together"
     ;
  value V502_
     0 = "Never married"
     1 = "Currently married"
     2 = "Formerly married"
     ;
  value V503_
     1 = "Once"
     2 = "More than once"
     ;
  value V504_
     1 = "Living with her"
     2 = "Staying elsewhere"
     ;
  value V510_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value V513_
     0 = "Never married"
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25-29"
     7 = "30+"
     ;
  value V527_
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value V528_
    31 = "31+ days"
    95 = "Within last 4 weeks"
    96 = "Before last birth"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value V529_
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value V530_
     0 = "No flag"
     1 = "N > int. since birth"
     2 = "N + abst. > interval"
     3 = "N, but sex not res."
     4 = "96, but no birth"
     5 = "96, but curr.preg."
     6 = "96, but resumed sex"
     7 = "N, last sex bef preg"
     8 = "N, inconsis 4 weeks"
     9 = "N, sex bef last birt"
     ;
  value V535_
     0 = "No"
     1 = "Formerly married"
     2 = "Lived with a man"
     ;
  value V602_
     1 = "Have another"
     2 = "Undecided"
     3 = "No more"
     4 = "Sterilized"
     5 = "Declared infecund"
     6 = "Never had sex"
     ;
  value V603_
   993 = "After marriage"
   994 = "Soon, now"
   996 = "Other, Non-numeric"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value V604_
     0 = "<12 months"
     1 = "1 year"
     2 = "2 years"
     3 = "3 years"
     4 = "4 years"
     5 = "5 years"
     6 = "6+ years"
     7 = "Non-numeric"
     8 = "Don't know"
     ;
  value V605_
     1 = "Wants within 2 years"
     2 = "Wants after 2+ years"
     3 = "Wants, unsure timing"
     4 = "Undecided"
     5 = "Wants no more"
     6 = "Sterilized"
     7 = "Declared infecund"
     8 = "Never had sex"
     ;
  value V606_
     1 = "Happy"
     2 = "Unhappy"
     3 = "Would not matter"
     8 = "DK"
     ;
  value V610_
     0 = "Disapproves"
     1 = "Approves"
     8 = "Don't know"
     ;
  value V611_
     0 = "Never"
     1 = "Once or twice"
     2 = "More often"
     ;
  value V612_
     0 = "Disapproves"
     1 = "Approves"
     8 = "Don't know"
     ;
  value V613_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value V614_
     6 = "6+"
     7 = "Non-numeric response"
     ;
  value V616_
   993 = "After marriage"
   994 = "Soon/now"
   995 = "Can't get pregnant"
   996 = "Other"
   998 = "DK"
     ;
  value V618_
     0 = "No regret"
     1 = "Resp. wants another"
     2 = "Part. wants another"
     3 = "Side effects"
     4 = "Child died"
     6 = "Other reason"
     ;
  value V621_
     1 = "Both want same"
     2 = "Husband wants more"
     3 = "Husband wants fewer"
     8 = "DK"
     ;
  value V623_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value V624_
     0 = "Never had sex"
     1 = "Unmet need to space"
     2 = "Unmet need to limit"
     3 = "Using to space"
     4 = "Using to limit"
     5 = "Spacing failure"
     6 = "Limiting failure"
     7 = "Desire birth < 2 yrs"
     8 = "No sex, want to wait"
     9 = "Infecund, menopausal"
     ;
  value V625_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value V626_
     0 = "Never had sex"
     1 = "Unmet need to space"
     2 = "Unmet need to limit"
     3 = "Using to space"
     4 = "Using to limit"
     5 = "Spacing failure"
     6 = "Limiting failure"
     7 = "Desire birth < 2 yrs"
     8 = "No sex, want to wait"
     9 = "Infecund, menopausal"
     ;
  value V627_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value V628_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value V629_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value V701_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value V702_
    98 = "Don't know"
     ;
  value V704_
    11 = "Land owner"
    12 = "Farmer"
    13 = "Agricultural worker"
    14 = "Fisherman"
    21 = "Unskilled labourers"
    22 = "Domestic servant"
    23 = "Non-agricultural wkr"
    31 = "Semi-skilled labour"
    41 = "Professional worker"
    51 = "Business"
    52 = "Small business/tradr"
    61 = "Unemployed"
    62 = "Retired"
    96 = "Others"
    98 = "Don't know"
     ;
  value V705_
     0 = "Did not work"
     1 = "Prof., Tech., Manag."
     2 = "Clerical"
     3 = "Sales"
     4 = "Agric-self employed"
     5 = "Agric-employee"
     6 = "Household & domestic"
     7 = "Services"
     8 = "Skilled manual"
     9 = "Unskilled manual"
    98 = "Don't know"
     ;
  value V707_
     0 = "His land"
     1 = "Family land"
     2 = "Someone else's land"
     3 = "Rented land"
     8 = "Don't know"
     ;
  value V714_
     0 = "No"
     1 = "Yes"
     ;
  value V715_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value V716_
     0 = "Not working"
    11 = "Land owner"
    12 = "Farmer"
    13 = "Agricultural worker"
    14 = "Fisherman"
    21 = "Unskilled labourers"
    22 = "Domestic servant"
    23 = "Non-agricultural wkr"
    31 = "Semi-skilled labour"
    41 = "Professional worker"
    51 = "Business"
    52 = "Small business/tradr"
    61 = "Unemployed"
    62 = "Retired"
    96 = "Others"
    98 = "Don't know"
     ;
  value V717_
     0 = "Not working"
     1 = "Prof., Tech., Manag."
     2 = "Clerical"
     3 = "Sales"
     4 = "Agric-self employed"
     5 = "Agric-employee"
     6 = "Household & domestic"
     7 = "Services"
     8 = "Skilled manual"
     9 = "Unskilled manual"
    98 = "Don't know"
     ;
  value V718_
     0 = "Did not work"
     1 = "Paid employee, away"
     2 = "Paid employee, home"
     3 = "Self-employed, away"
     4 = "Self-employed, home"
     5 = "Unpaid worker, away"
     6 = "Unpaid worker, home"
     ;
  value V719_
     1 = "For family member"
     2 = "For someone else"
     3 = "Self-employed"
     ;
  value V720_
     0 = "No"
     1 = "Yes"
     ;
  value V721_
     1 = "At home"
     2 = "Away"
     ;
  value V723_
     0 = "Respondent"
     1 = "Husband/partner"
     2 = "Other child"
     3 = "Other relatives"
     4 = "Neighbors"
     5 = "Friends"
     6 = "Servants/hired help"
     7 = "Child is in school"
     8 = "Institutional care"
    10 = "Other female child"
    11 = "Other male child"
    95 = "Not worked since bth"
    96 = "Other"
     ;
  value V729_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     8 = "Don't know"
     ;
  value V732_
     1 = "All year"
     2 = "Seasonal"
     3 = "Occasional"
     ;
  value V739_
     1 = "Respondent"
     2 = "Partner"
     3 = "Jointly with partner"
     4 = "Someone else"
     5 = "Jointly with someone"
     ;
  value V740_
     0 = "Own land"
     1 = "Family land"
     2 = "Someone else's land"
     3 = "Rented land"
     8 = "Don't know"
     ;
  value V803_
    95 = "95+"
    96 = "2+ visits"
    97 = "Inconsistent"
     ;
  value V811_
     0 = "No"
     1 = "Yes"
     ;
  value V812_
     0 = "No"
     1 = "Yes"
     ;
  value V813_
     0 = "No"
     1 = "Yes"
     ;
  value V814_
     0 = "No"
     1 = "Yes"
     ;
  value VCOL_
     1 = "Pregs & contracept."
     2 = "Reason for discont."
     3 = "Marriage & unions"
     4 = "Moves & communities"
     5 = "Country specific"
     ;
  value SDFORMA_
     1 = "Bengali"
     2 = "English"
     ;
  value SADJUST_
     0 = "Not adjusted"
     1 = "Adjusted"
     ;
  value SDIV_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value S114A_
     0 = "No"
     1 = "Yes"
     ;
  value S114B_
     0 = "No"
     1 = "Yes"
     ;
  value S114C_
     0 = "No"
     1 = "Yes"
     ;
  value S114D_
     0 = "No"
     1 = "Yes"
     ;
  value S114E_
     0 = "No"
     1 = "Yes"
     ;
  value S119_
   996 = "On premises"
     ;
  value S123_
     0 = "No children"
    11 = "Sep.tank/modern tlet"
    21 = "Water sld/Slab latr."
    22 = "Pit latrine"
    23 = "Open latrine"
    24 = "Hanging latrine"
    31 = "No facility,bush"
    96 = "Other"
     ;
  value S124A_
     0 = "No"
     1 = "Yes"
     ;
  value S124B_
     0 = "No"
     1 = "Yes"
     ;
  value S124C_
     0 = "No"
     1 = "Yes"
     ;
  value S124D_
     0 = "No"
     1 = "Yes"
     ;
  value S124I_
     0 = "No"
     1 = "Yes"
     ;
  value S229M_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value S229Y_
  9997 = "Inconsistent"
  9998 = "DK"
     ;
  value S233A_
     0 = "No"
     1 = "Yes"
     ;
  value S233B_
     0 = "No"
     1 = "Yes"
     ;
  value S233C_
     1 = "Menstrual regulation"
     2 = "Induced abortion/D&C"
     3 = "Other"
     ;
  value S233DY_
  1300 = "Bengali (Start)"
  1401 = "Bengali (end)"
  1900 = "English (Start)"
  1994 = "English(end)"
  9997 = "Inconsistent"
  9998 = "Don't Know"
     ;
  value S304_
     0 = "No"
     1 = "Yes"
     ;
  value S309_
     0 = "Never used a method"
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
     ;
  value S310_
    11 = "Hospital/Medical clg"
    12 = "Family welfare cent."
    13 = "Thana health complex"
    14 = "Satellite clinic"
    21 = "Private clinic,doct."
    22 = "Traditional doctor"
    23 = "Pharmacy"
    31 = "Shop"
    32 = "Friends, relatives"
    41 = "Fieldworker, FWA"
    96 = "Other"
    98 = "DK"
     ;
  value S316_
     0 = "No"
     1 = "Yes"
     ;
  value S317_
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
     ;
  value S318B_
     1 = "Missing in order"
     2 = "Missing out of order"
     3 = "No pills missing"
     ;
  value S318C_
     1 = "DK what to do"
     2 = "Health reasons"
     3 = "Following instr'tns"
     4 = "New packet"
     5 = "Menstruating"
     6 = "Other"
     ;
  value S318EA_
     0 = "No"
     1 = "Yes"
     ;
  value S318EB_
     0 = "No"
     1 = "Yes"
     ;
  value S318EC_
     0 = "No"
     1 = "Yes"
     ;
  value S318ED_
     0 = "No"
     1 = "Yes"
     ;
  value S318EE_
     0 = "No"
     1 = "Yes"
     ;
  value S318EF_
     0 = "No"
     1 = "Yes"
     ;
  value S318EG_
     0 = "No"
     1 = "Yes"
     ;
  value S318F_
    97 = "More than 1 mnth.ago"
     ;
  value S318HA_
     0 = "No"
     1 = "Yes"
     ;
  value S318HB_
     0 = "No"
     1 = "Yes"
     ;
  value S318HC_
     0 = "No"
     1 = "Yes"
     ;
  value S318HD_
     0 = "No"
     1 = "Yes"
     ;
  value S318HE_
     0 = "No"
     1 = "Yes"
     ;
  value S318HF_
     0 = "No"
     1 = "Yes"
     ;
  value S318HG_
     0 = "No"
     1 = "Yes"
     ;
  value S318HH_
     0 = "No"
     1 = "Yes"
     ;
  value S318HI_
     0 = "No"
     1 = "Yes"
     ;
  value S318I_
     0 = "No"
     1 = "Yes"
     ;
  value S318K_
     1 = "Start taking again"
     2 = "Use extra/missed pll"
     3 = "Use another method"
     4 = "Extra pill+anoth.mth"
     5 = "Other"
     6 = "Never forgot"
     ;
  value S319B_
     1 = "Husband away"
     2 = "Forgot"
     3 = "Health reasons"
     4 = "Cost too much"
     5 = "Not available at pr."
     6 = "Other"
     ;
  value S320S_
     0 = "No"
     1 = "Yes"
     ;
  value S320_
     1 = "Raja"
     2 = "Panther"
     3 = "Majestic"
     4 = "Sensation"
     5 = "Latex"
     6 = "Tahiti"
     7 = "Circle rubber"
     8 = "Sultan"
     9 = "Gent"
    10 = "Durex"
    96 = "Other condom brand"
    98 = "Don't know brand"
     ;
  value S320A_
     1 = "Husband keeps"
     2 = "Ran out"
     3 = "Other"
     ;
  value S320C_
     0 = "No"
     1 = "Yes"
     ;
  value S322C_
     1 = "During period"
     2 = "Imm.after per'd ends"
     3 = "Middle of cycle"
     4 = "Imm.bef. per'd ends"
    96 = "Other"
    98 = "Don't know"
     ;
  value S322D_
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Specific method 1"
    14 = "Specific method 2"
    15 = "Specific method 3"
     ;
  value S323B_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value S325_
     1 = "Respondent"
     2 = "Husband"
     3 = "Son/daughter"
     4 = "Other relative"
     5 = "Other"
     ;
  value S325A_
     0 = "No"
     1 = "Yes"
     ;
  value S325B_
     0 = "No"
     1 = "Yes"
     8 = "Cannot remember"
     ;
  value S325C_
     0 = "No"
     1 = "Yes"
     8 = "Cannot remember"
     ;
  value S325E_
     0 = "No"
     1 = "Yes"
     8 = "Cannot remember"
     ;
  value S325G_
     0 = "No"
     1 = "Yes"
     8 = "Cannot remember"
     ;
  value S325H_
     0 = "No"
     1 = "Yes"
     ;
  value S325I_
     1 = "Pill, Maya"
     2 = "IUD, Copper T"
     3 = "Injections"
     4 = "Condom, Raja"
     5 = "Female Sterilization"
     6 = "Male Sterilization"
     7 = "Safe period,calendar"
     8 = "Withdrawal"
     9 = "Other"
     ;
  value S325J_
     1 = "Respondent had most"
     2 = "Husband had most"
     3 = "Husb. and wife equal"
     4 = "Other relative"
     5 = "Other"
     ;
  value S326_
     1 = "Recomm. FP worker"
     2 = "Recomm. friend, rel."
     3 = "Side effects others"
     4 = "Method easy to use"
     5 = "Access, availability"
     6 = "Cost"
     7 = "Wanted permanent m."
     8 = "Husband preferred"
     9 = "Wanted more effect."
    10 = "Other"
    98 = "DK"
     ;
  value S327_
     0 = "No"
     1 = "Yes"
     ;
  value S328PA_
     0 = "No"
     1 = "Yes"
     ;
  value S328PB_
     0 = "No"
     1 = "Yes"
     ;
  value S328PC_
     0 = "No"
     1 = "Yes"
     ;
  value S328PD_
     0 = "No"
     1 = "Yes"
     ;
  value S328PE_
     0 = "No"
     1 = "Yes"
     ;
  value S328PF_
     0 = "No"
     1 = "Yes"
     ;
  value S328PG_
     0 = "No"
     1 = "Yes"
     ;
  value S328PH_
     0 = "No"
     1 = "Yes"
     ;
  value S328PI_
     0 = "No"
     1 = "Yes"
     ;
  value S328PJ_
     0 = "No"
     1 = "Yes"
     ;
  value S328PK_
     0 = "No"
     1 = "Yes"
     ;
  value S328A_
     0 = "No"
     1 = "Yes"
     ;
  value S328B_
     1 = "Fieldworker, FWA"
     2 = "Staff satellite cl."
     3 = "Staff Family Welf.Cl"
     4 = "Relative, friend"
     5 = "Other"
     ;
  value S328C_
     0 = "No"
     1 = "Yes"
     ;
  value S328DA_
     0 = "No"
     1 = "Yes"
     ;
  value S328DB_
     0 = "No"
     1 = "Yes"
     ;
  value S328DC_
     0 = "No"
     1 = "Yes"
     ;
  value S328DD_
     0 = "No"
     1 = "Yes"
     ;
  value S328DE_
     0 = "No"
     1 = "Yes"
     ;
  value S328DF_
     0 = "No"
     1 = "Yes"
     ;
  value S328DG_
     0 = "No"
     1 = "Yes"
     ;
  value S328DH_
     0 = "No"
     1 = "Yes"
     ;
  value S328DI_
     0 = "No"
     1 = "Yes"
     ;
  value S347A_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value S347B_
     0 = "No"
     1 = "Yes"
     ;
  value S347CA_
     0 = "No"
     1 = "Yes"
     ;
  value S347CB_
     0 = "No"
     1 = "Yes"
     ;
  value S347CC_
     0 = "No"
     1 = "Yes"
     ;
  value S347CD_
     0 = "No"
     1 = "Yes"
     ;
  value S347CE_
     0 = "No"
     1 = "Yes"
     ;
  value S348C_
     0 = "No"
     1 = "Yes"
     ;
  value S349A_
     0 = "No"
     1 = "Yes"
     ;
  value S349B_
     0 = "No"
     1 = "Yes"
     ;
  value S349C_
     0 = "No"
     1 = "Yes"
     ;
  value S349D_
     0 = "No"
     1 = "Yes"
     ;
  value S349E_
     0 = "No"
     1 = "Yes"
     ;
  value S352_
    98 = "DK"
     ;
  value S353_
     0 = "Less than 1 mth ago"
     8 = "DK"
     ;
  value S354_
     0 = "No"
     1 = "Yes"
     ;
  value S355_
     1 = "Pills"
     2 = "Condoms"
     3 = "Injection"
     4 = "Other"
     ;
  value S357MA_
     0 = "Yes"
     1 = "No"
     ;
  value S357MB_
     0 = "Yes"
     1 = "No"
     ;
  value S357MC_
     0 = "Yes"
     1 = "No"
     ;
  value S357MD_
     0 = "Yes"
     1 = "No"
     ;
  value S357ME_
     0 = "Yes"
     1 = "No"
     ;
  value S357MF_
     0 = "Yes"
     1 = "No"
     ;
  value S357MG_
     0 = "Yes"
     1 = "No"
     ;
  value S357A_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value S357B_
     1 = "For sterilisation"
     2 = "IUD insertion"
     3 = "For an Injection"
     4 = "Get other FP methods"
     5 = "Treatment side effs."
     6 = "Other health reasons"
     7 = "Other"
     ;
  value S358_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value S359_
     0 = "No"
     1 = "Yes"
     ;
  value S361_
     0 = "No"
     1 = "Yes"
     ;
  value S439_
     0 = "No"
     1 = "Yes"
     ;
  value S440_
     1 = "Doctor"
     2 = "Nurse/midwife"
     3 = "Pharmacist"
     4 = "Trad.birth attendant"
     5 = "Fieldworker,FWA"
     6 = "Husband"
     7 = "Mother"
     8 = "Sister"
     9 = "Mother-in-law"
    10 = "Sister-in-law"
    11 = "Other relative"
    12 = "Other"
     ;
  value S441_
    96 = "Did not discuss"
    98 = "DK/Can't remember"
     ;
  value S442A_
     0 = "No"
     1 = "Yes"
     ;
  value S442B_
     0 = "No one else"
     1 = "Doctor"
     2 = "Nurse/midwife/FWV"
     3 = "Pharmacist"
     4 = "Trad.birth attendant"
     5 = "Fieldworker,FWA"
     6 = "Husband"
     7 = "Mother"
     8 = "Sister"
     9 = "Mother-in-law"
    10 = "Sister-in-law"
    11 = "Other relative"
    96 = "Other"
     ;
  value S442C_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value S442DA_
     0 = "No"
     1 = "Yes"
     ;
  value S442DB_
     0 = "No"
     1 = "Yes"
     ;
  value S442DC_
     0 = "No"
     1 = "Yes"
     ;
  value S442DD_
     0 = "No"
     1 = "Yes"
     ;
  value S442DE_
     0 = "No"
     1 = "Yes"
     ;
  value S442DF_
     0 = "No"
     1 = "Yes"
     ;
  value S442DG_
     0 = "No"
     1 = "Yes"
     ;
  value S442DH_
     0 = "No"
     1 = "Yes"
     ;
  value S442DI_
     0 = "No"
     1 = "Yes"
     ;
  value S442DJ_
     0 = "No"
     1 = "Yes"
     ;
  value S442DK_
     0 = "No"
     1 = "Yes"
     ;
  value S442EM1_
     0 = "No method rec'd."
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Prolonged Bfdng."
     ;
  value S442FM1_
    95 = "After menst.begins"
    96 = "Not discussed"
    98 = "Forgot/DK"
     ;
  value S442EM2_
     0 = "No method rec'd."
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     6 = "Female Sterilization"
     7 = "Male Sterilization"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
    12 = "Abstinence"
    13 = "Prolonged Bfdng."
     ;
  value S442FM2_
    95 = "After menst.begins"
    96 = "Not discussed"
    98 = "Forgot/DK"
     ;
  value S442H_
     1 = "Doctor"
     2 = "Nurse/midwife"
     3 = "Pharmacist"
     4 = "Trad.birth attendant"
     5 = "Fieldworker,FWA"
     6 = "Husband"
     7 = "Mother"
     8 = "Sister"
     9 = "Mother-in-law"
    10 = "Sister-in-law"
    11 = "Other relative"
    12 = "Other"
     ;
  value S442IA_
     0 = "No"
     1 = "Yes"
     ;
  value S442IB_
     0 = "No"
     1 = "Yes"
     ;
  value S442IC_
     0 = "No"
     1 = "Yes"
     ;
  value S442ID_
     0 = "No"
     1 = "Yes"
     ;
  value S442IE_
     0 = "No"
     1 = "Yes"
     ;
  value S442IF_
     0 = "No"
     1 = "Yes"
     ;
  value S442IG_
     0 = "No"
     1 = "Yes"
     ;
  value S442J_
     1 = "Doctor"
     2 = "Nurse/midwife"
     3 = "Pharmacist"
     4 = "Trad.birth attendant"
     5 = "Fieldworker,FWA"
     6 = "Husband"
     7 = "Mother"
     8 = "Sister"
     9 = "Mother-in-law"
    10 = "Sister-in-law"
    11 = "Other relative"
    12 = "Other"
     ;
  value S442KA_
     0 = "No"
     1 = "Yes"
     ;
  value S442KB_
     0 = "No"
     1 = "Yes"
     ;
  value S442KC_
     0 = "No"
     1 = "Yes"
     ;
  value S442KD_
     0 = "No"
     1 = "Yes"
     ;
  value S442KE_
     0 = "No"
     1 = "Yes"
     ;
  value S442KF_
     0 = "No"
     1 = "Yes"
     ;
  value S442L_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value S442MA_
     0 = "No"
     1 = "Yes"
     ;
  value S442MB_
     0 = "No"
     1 = "Yes"
     ;
  value S442MC_
     0 = "No"
     1 = "Yes"
     ;
  value S442MD_
     0 = "No"
     1 = "Yes"
     ;
  value S442ME_
     0 = "No"
     1 = "Yes"
     ;
  value S442MF_
     0 = "No"
     1 = "Yes"
     ;
  value S442MG_
     0 = "No"
     1 = "Yes"
     ;
  value S442MH_
     0 = "No"
     1 = "Yes"
     ;
  value S442MI_
     0 = "No"
     1 = "Yes"
     ;
  value S442N_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value S442O_
    96 = "Until next period"
    98 = "DK"
     ;
  value S609_
    11 = "Hospital/Medical clg"
    12 = "Family welfare cent."
    13 = "Thana health complex"
    14 = "Satellite clinic"
    21 = "Traditional doctor"
    22 = "Qualified doctor"
    23 = "Pharmacy"
    31 = "Shop"
    32 = "Friends, relatives"
    41 = "Fieldworker, FWA"
    96 = "Other"
    98 = "DK"
     ;
  value S613_
     0 = "No"
     1 = "Yes"
     ;
  value S615_
     0 = "Not allowd to go out"
     1 = "Yes, alone"
     2 = "Yes, with children"
     6 = "Other"
     ;
  value S616_
     1 = "1x month or more"
     2 = "Several times a year"
     3 = "Less than 1x a year"
     4 = "Never"
     ;
  value S617_
     1 = "1x month or more"
     2 = "Several times a year"
     3 = "Less than 1x a year"
     4 = "Never"
     ;
  value S618_
     0 = "Not allowd to go out"
     1 = "Yes, alone"
     2 = "Yes, with children"
     6 = "Other"
     ;
  attrib CASEID   length=$15 label="Case Identification";
  attrib V000     length=$3 label="Country code and phase";
  attrib V001     length=8 label="Cluster number";
  attrib V002     length=4 label="Household number";
  attrib V003     length=4 label="Respondent's line number";
  attrib V004     length=4 label="Ultimate area unit";
  attrib V005     length=8 label="Sample weight";
  attrib V006     length=4 label="Month of interview";
  attrib V007     length=4 label="Year of interview";
  attrib V008     length=4 label="Date of interview (CMC)";
  attrib V009     length=4 label="Respondent's month of birth";
  attrib V010     length=4 label="Respondent's year of birth";
  attrib V011     length=4 label="Date of birth (CMC)";
  attrib V012     length=4 label="Current age - respondent";
  attrib V013     length=4 format=V013_. label="Age 5-year groups";
  attrib V014     length=4 format=V014_. label="Completeness of information";
  attrib V015     length=4 format=V015_. label="Result of individual interview";
  attrib V016     length=4 label="Day of interview";
  attrib V017     length=4 label="CMC start of calendar";
  attrib V018     length=4 format=V018_. label="Row of month of interview";
  attrib V019     length=4 format=V019_. label="Length of calendar";
  attrib V020     length=4 format=V020_. label="Ever-married sample";
  attrib V021     length=4 label="Primary sampling unit";
  attrib V022     length=4 label="Sample stratum number";
  attrib V023     length=4 format=V023_. label="Sample domain";
  attrib V024     length=4 format=V024_. label="Region";
  attrib V025     length=4 format=V025_. label="Type of place of residence";
  attrib V026     length=4 format=V026_. label="De facto place of residence";
  attrib V027     length=4 label="Number of visits";
  attrib V028     length=4 label="Interviewer identification";
  attrib V029     length=4 label="Keyer identification";
  attrib V030     length=4 label="Field supervisor -NA";
  attrib V031     length=4 label="Field editor -NA";
  attrib V032     length=4 label="Office editor -NA";
  attrib V033     length=8 label="Ultimate area selection prob.";
  attrib V034     length=4 format=V034_. label="Line number of husband";
  attrib V101     length=4 format=V101_. label="Region";
  attrib V102     length=4 format=V102_. label="Type of place of residence";
  attrib V103     length=4 label="Childhood place of residen -NA";
  attrib V104     length=4 format=V104_. label="Years lived in place of res.";
  attrib V105     length=4 format=V105_. label="Type of place of previous res.";
  attrib V106     length=4 format=V106_. label="Highest educational level";
  attrib V107     length=4 label="Highest year of education";
  attrib V108     length=4 format=V108_. label="Literacy";
  attrib V109     length=4 format=V109_. label="Reads newspaper once a week";
  attrib V110     length=4 format=V110_. label="Watches TV every week";
  attrib V111     length=4 label="Listens to radio every day -NA";
  attrib V112     length=4 format=V112_. label="Listens to radio every week";
  attrib V113     length=4 format=V113_. label="Source of drinking water";
  attrib V114     length=4 format=V114_. label="Source of non-drinking water";
  attrib V115     length=4 format=V115_. label="Time to get to water source";
  attrib V116     length=4 format=V116_. label="Type of toilet facility";
  attrib V119     length=4 format=V119_. label="Has electricity";
  attrib V120     length=4 format=V120_. label="Has radio";
  attrib V121     length=4 format=V121_. label="Has television";
  attrib V122     length=4 label="Has refrigerator -NA";
  attrib V123     length=4 format=V123_. label="Has bicycle";
  attrib V124     length=4 label="Has motorcycle -NA";
  attrib V125     length=4 label="Has car -NA";
  attrib V127     length=4 format=V127_. label="Main floor material";
  attrib V128     length=4 format=V128_. label="Main wall material";
  attrib V129     length=4 format=V129_. label="Main roof material";
  attrib V130     length=4 format=V130_. label="Religion";
  attrib V131     length=4 label="Ethnicity -NA";
  attrib V133     length=4 format=V133_. label="Education in single years";
  attrib V134     length=4 format=V134_. label="De facto place of residence";
  attrib V135     length=4 format=V135_. label="Usual resident or visitor";
  attrib V136     length=4 label="Number of household members";
  attrib V137     length=4 label="Number of children 5 and under";
  attrib V138     length=4 label="Number of eligible women in HH";
  attrib V139     length=4 format=V139_. label="De jure region of residence";
  attrib V140     length=4 format=V140_. label="De jure type of place of res.";
  attrib V141     length=4 format=V141_. label="De jure place of residence";
  attrib V142     length=4 format=V142_. label="Same source of water";
  attrib V144     length=4 label="Communities lived in >Jan.8-NA";
  attrib V148     length=4 format=V148_. label="Respondent still in school";
  attrib V149     length=4 format=V149_. label="Educational attainment";
  attrib V150     length=4 format=V150_. label="Relationship to household head";
  attrib V151     length=4 format=V151_. label="Sex of household head";
  attrib V152     length=4 format=V152_. label="Age of household head";
  attrib V153     length=4 label="Has telephone -NA";
  attrib V154     length=4 label="Reason stop attending schoo-NA";
  attrib AWFACTT  length=4 label="All woman factor - total";
  attrib AWFACTU  length=4 label="All woman factor - urban/rural";
  attrib AWFACTR  length=4 label="All woman factor - regional";
  attrib AWFACTE  length=4 label="All woman factor - educational";
  attrib BIDX_01  length=4 label="Birth column number";
  attrib BORD_01  length=4 label="Birth order number";
  attrib B0_01    length=4 format=B0_. label="Child is twin";
  attrib B1_01    length=4 label="Month of birth";
  attrib B2_01    length=4 label="Year of birth";
  attrib B3_01    length=4 label="Date of birth (CMC)";
  attrib B4_01    length=4 format=B4_. label="Sex of child";
  attrib B5_01    length=4 format=B5_. label="Child is alive";
  attrib B6_01    length=4 format=B6_. label="Age at death";
  attrib B7_01    length=4 label="Age at death (months-imputed)";
  attrib B8_01    length=4 label="Current age of child";
  attrib B9_01    length=4 format=B9_. label="Who child lives with";
  attrib B10_01   length=4 format=B10_. label="Completeness of information";
  attrib B11_01   length=4 label="Preceding birth interval";
  attrib B12_01   length=4 label="Succeeding birth interval";
  attrib B13_01   length=4 format=B13_. label="Flag for age at death";
  attrib B14_01   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_01   length=4 label="Live birth between births -NA";
  attrib BIDX_02  length=4 label="Birth column number";
  attrib BORD_02  length=4 label="Birth order number";
  attrib B0_02    length=4 format=B0_. label="Child is twin";
  attrib B1_02    length=4 label="Month of birth";
  attrib B2_02    length=4 label="Year of birth";
  attrib B3_02    length=4 label="Date of birth (CMC)";
  attrib B4_02    length=4 format=B4_. label="Sex of child";
  attrib B5_02    length=4 format=B5_. label="Child is alive";
  attrib B6_02    length=4 format=B6_. label="Age at death";
  attrib B7_02    length=4 label="Age at death (months-imputed)";
  attrib B8_02    length=4 label="Current age of child";
  attrib B9_02    length=4 format=B9_. label="Who child lives with";
  attrib B10_02   length=4 format=B10_. label="Completeness of information";
  attrib B11_02   length=4 label="Preceding birth interval";
  attrib B12_02   length=4 label="Succeeding birth interval";
  attrib B13_02   length=4 format=B13_. label="Flag for age at death";
  attrib B14_02   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_02   length=4 label="Live birth between births -NA";
  attrib BIDX_03  length=4 label="Birth column number";
  attrib BORD_03  length=4 label="Birth order number";
  attrib B0_03    length=4 format=B0_. label="Child is twin";
  attrib B1_03    length=4 label="Month of birth";
  attrib B2_03    length=4 label="Year of birth";
  attrib B3_03    length=4 label="Date of birth (CMC)";
  attrib B4_03    length=4 format=B4_. label="Sex of child";
  attrib B5_03    length=4 format=B5_. label="Child is alive";
  attrib B6_03    length=4 format=B6_. label="Age at death";
  attrib B7_03    length=4 label="Age at death (months-imputed)";
  attrib B8_03    length=4 label="Current age of child";
  attrib B9_03    length=4 format=B9_. label="Who child lives with";
  attrib B10_03   length=4 format=B10_. label="Completeness of information";
  attrib B11_03   length=4 label="Preceding birth interval";
  attrib B12_03   length=4 label="Succeeding birth interval";
  attrib B13_03   length=4 format=B13_. label="Flag for age at death";
  attrib B14_03   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_03   length=4 label="Live birth between births -NA";
  attrib BIDX_04  length=4 label="Birth column number";
  attrib BORD_04  length=4 label="Birth order number";
  attrib B0_04    length=4 format=B0_. label="Child is twin";
  attrib B1_04    length=4 label="Month of birth";
  attrib B2_04    length=4 label="Year of birth";
  attrib B3_04    length=4 label="Date of birth (CMC)";
  attrib B4_04    length=4 format=B4_. label="Sex of child";
  attrib B5_04    length=4 format=B5_. label="Child is alive";
  attrib B6_04    length=4 format=B6_. label="Age at death";
  attrib B7_04    length=4 label="Age at death (months-imputed)";
  attrib B8_04    length=4 label="Current age of child";
  attrib B9_04    length=4 format=B9_. label="Who child lives with";
  attrib B10_04   length=4 format=B10_. label="Completeness of information";
  attrib B11_04   length=4 label="Preceding birth interval";
  attrib B12_04   length=4 label="Succeeding birth interval";
  attrib B13_04   length=4 format=B13_. label="Flag for age at death";
  attrib B14_04   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_04   length=4 label="Live birth between births -NA";
  attrib BIDX_05  length=4 label="Birth column number";
  attrib BORD_05  length=4 label="Birth order number";
  attrib B0_05    length=4 format=B0_. label="Child is twin";
  attrib B1_05    length=4 label="Month of birth";
  attrib B2_05    length=4 label="Year of birth";
  attrib B3_05    length=4 label="Date of birth (CMC)";
  attrib B4_05    length=4 format=B4_. label="Sex of child";
  attrib B5_05    length=4 format=B5_. label="Child is alive";
  attrib B6_05    length=4 format=B6_. label="Age at death";
  attrib B7_05    length=4 label="Age at death (months-imputed)";
  attrib B8_05    length=4 label="Current age of child";
  attrib B9_05    length=4 format=B9_. label="Who child lives with";
  attrib B10_05   length=4 format=B10_. label="Completeness of information";
  attrib B11_05   length=4 label="Preceding birth interval";
  attrib B12_05   length=4 label="Succeeding birth interval";
  attrib B13_05   length=4 format=B13_. label="Flag for age at death";
  attrib B14_05   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_05   length=4 label="Live birth between births -NA";
  attrib BIDX_06  length=4 label="Birth column number";
  attrib BORD_06  length=4 label="Birth order number";
  attrib B0_06    length=4 format=B0_. label="Child is twin";
  attrib B1_06    length=4 label="Month of birth";
  attrib B2_06    length=4 label="Year of birth";
  attrib B3_06    length=4 label="Date of birth (CMC)";
  attrib B4_06    length=4 format=B4_. label="Sex of child";
  attrib B5_06    length=4 format=B5_. label="Child is alive";
  attrib B6_06    length=4 format=B6_. label="Age at death";
  attrib B7_06    length=4 label="Age at death (months-imputed)";
  attrib B8_06    length=4 label="Current age of child";
  attrib B9_06    length=4 format=B9_. label="Who child lives with";
  attrib B10_06   length=4 format=B10_. label="Completeness of information";
  attrib B11_06   length=4 label="Preceding birth interval";
  attrib B12_06   length=4 label="Succeeding birth interval";
  attrib B13_06   length=4 format=B13_. label="Flag for age at death";
  attrib B14_06   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_06   length=4 label="Live birth between births -NA";
  attrib BIDX_07  length=4 label="Birth column number";
  attrib BORD_07  length=4 label="Birth order number";
  attrib B0_07    length=4 format=B0_. label="Child is twin";
  attrib B1_07    length=4 label="Month of birth";
  attrib B2_07    length=4 label="Year of birth";
  attrib B3_07    length=4 label="Date of birth (CMC)";
  attrib B4_07    length=4 format=B4_. label="Sex of child";
  attrib B5_07    length=4 format=B5_. label="Child is alive";
  attrib B6_07    length=4 format=B6_. label="Age at death";
  attrib B7_07    length=4 label="Age at death (months-imputed)";
  attrib B8_07    length=4 label="Current age of child";
  attrib B9_07    length=4 format=B9_. label="Who child lives with";
  attrib B10_07   length=4 format=B10_. label="Completeness of information";
  attrib B11_07   length=4 label="Preceding birth interval";
  attrib B12_07   length=4 label="Succeeding birth interval";
  attrib B13_07   length=4 format=B13_. label="Flag for age at death";
  attrib B14_07   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_07   length=4 label="Live birth between births -NA";
  attrib BIDX_08  length=4 label="Birth column number";
  attrib BORD_08  length=4 label="Birth order number";
  attrib B0_08    length=4 format=B0_. label="Child is twin";
  attrib B1_08    length=4 label="Month of birth";
  attrib B2_08    length=4 label="Year of birth";
  attrib B3_08    length=4 label="Date of birth (CMC)";
  attrib B4_08    length=4 format=B4_. label="Sex of child";
  attrib B5_08    length=4 format=B5_. label="Child is alive";
  attrib B6_08    length=4 format=B6_. label="Age at death";
  attrib B7_08    length=4 label="Age at death (months-imputed)";
  attrib B8_08    length=4 label="Current age of child";
  attrib B9_08    length=4 format=B9_. label="Who child lives with";
  attrib B10_08   length=4 format=B10_. label="Completeness of information";
  attrib B11_08   length=4 label="Preceding birth interval";
  attrib B12_08   length=4 label="Succeeding birth interval";
  attrib B13_08   length=4 format=B13_. label="Flag for age at death";
  attrib B14_08   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_08   length=4 label="Live birth between births -NA";
  attrib BIDX_09  length=4 label="Birth column number";
  attrib BORD_09  length=4 label="Birth order number";
  attrib B0_09    length=4 format=B0_. label="Child is twin";
  attrib B1_09    length=4 label="Month of birth";
  attrib B2_09    length=4 label="Year of birth";
  attrib B3_09    length=4 label="Date of birth (CMC)";
  attrib B4_09    length=4 format=B4_. label="Sex of child";
  attrib B5_09    length=4 format=B5_. label="Child is alive";
  attrib B6_09    length=4 format=B6_. label="Age at death";
  attrib B7_09    length=4 label="Age at death (months-imputed)";
  attrib B8_09    length=4 label="Current age of child";
  attrib B9_09    length=4 format=B9_. label="Who child lives with";
  attrib B10_09   length=4 format=B10_. label="Completeness of information";
  attrib B11_09   length=4 label="Preceding birth interval";
  attrib B12_09   length=4 label="Succeeding birth interval";
  attrib B13_09   length=4 format=B13_. label="Flag for age at death";
  attrib B14_09   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_09   length=4 label="Live birth between births -NA";
  attrib BIDX_10  length=4 label="Birth column number";
  attrib BORD_10  length=4 label="Birth order number";
  attrib B0_10    length=4 format=B0_. label="Child is twin";
  attrib B1_10    length=4 label="Month of birth";
  attrib B2_10    length=4 label="Year of birth";
  attrib B3_10    length=4 label="Date of birth (CMC)";
  attrib B4_10    length=4 format=B4_. label="Sex of child";
  attrib B5_10    length=4 format=B5_. label="Child is alive";
  attrib B6_10    length=4 format=B6_. label="Age at death";
  attrib B7_10    length=4 label="Age at death (months-imputed)";
  attrib B8_10    length=4 label="Current age of child";
  attrib B9_10    length=4 format=B9_. label="Who child lives with";
  attrib B10_10   length=4 format=B10_. label="Completeness of information";
  attrib B11_10   length=4 label="Preceding birth interval";
  attrib B12_10   length=4 label="Succeeding birth interval";
  attrib B13_10   length=4 format=B13_. label="Flag for age at death";
  attrib B14_10   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_10   length=4 label="Live birth between births -NA";
  attrib BIDX_11  length=4 label="Birth column number";
  attrib BORD_11  length=4 label="Birth order number";
  attrib B0_11    length=4 format=B0_. label="Child is twin";
  attrib B1_11    length=4 label="Month of birth";
  attrib B2_11    length=4 label="Year of birth";
  attrib B3_11    length=4 label="Date of birth (CMC)";
  attrib B4_11    length=4 format=B4_. label="Sex of child";
  attrib B5_11    length=4 format=B5_. label="Child is alive";
  attrib B6_11    length=4 format=B6_. label="Age at death";
  attrib B7_11    length=4 label="Age at death (months-imputed)";
  attrib B8_11    length=4 label="Current age of child";
  attrib B9_11    length=4 format=B9_. label="Who child lives with";
  attrib B10_11   length=4 format=B10_. label="Completeness of information";
  attrib B11_11   length=4 label="Preceding birth interval";
  attrib B12_11   length=4 label="Succeeding birth interval";
  attrib B13_11   length=4 format=B13_. label="Flag for age at death";
  attrib B14_11   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_11   length=4 label="Live birth between births -NA";
  attrib BIDX_12  length=4 label="Birth column number";
  attrib BORD_12  length=4 label="Birth order number";
  attrib B0_12    length=4 format=B0_. label="Child is twin";
  attrib B1_12    length=4 label="Month of birth";
  attrib B2_12    length=4 label="Year of birth";
  attrib B3_12    length=4 label="Date of birth (CMC)";
  attrib B4_12    length=4 format=B4_. label="Sex of child";
  attrib B5_12    length=4 format=B5_. label="Child is alive";
  attrib B6_12    length=4 format=B6_. label="Age at death";
  attrib B7_12    length=4 label="Age at death (months-imputed)";
  attrib B8_12    length=4 label="Current age of child";
  attrib B9_12    length=4 format=B9_. label="Who child lives with";
  attrib B10_12   length=4 format=B10_. label="Completeness of information";
  attrib B11_12   length=4 label="Preceding birth interval";
  attrib B12_12   length=4 label="Succeeding birth interval";
  attrib B13_12   length=4 format=B13_. label="Flag for age at death";
  attrib B14_12   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_12   length=4 label="Live birth between births -NA";
  attrib BIDX_13  length=4 label="Birth column number";
  attrib BORD_13  length=4 label="Birth order number";
  attrib B0_13    length=4 format=B0_. label="Child is twin";
  attrib B1_13    length=4 label="Month of birth";
  attrib B2_13    length=4 label="Year of birth";
  attrib B3_13    length=4 label="Date of birth (CMC)";
  attrib B4_13    length=4 format=B4_. label="Sex of child";
  attrib B5_13    length=4 format=B5_. label="Child is alive";
  attrib B6_13    length=4 format=B6_. label="Age at death";
  attrib B7_13    length=4 label="Age at death (months-imputed)";
  attrib B8_13    length=4 label="Current age of child";
  attrib B9_13    length=4 format=B9_. label="Who child lives with";
  attrib B10_13   length=4 format=B10_. label="Completeness of information";
  attrib B11_13   length=4 label="Preceding birth interval";
  attrib B12_13   length=4 label="Succeeding birth interval";
  attrib B13_13   length=4 format=B13_. label="Flag for age at death";
  attrib B14_13   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_13   length=4 label="Live birth between births -NA";
  attrib BIDX_14  length=4 label="Birth column number";
  attrib BORD_14  length=4 label="Birth order number";
  attrib B0_14    length=4 format=B0_. label="Child is twin";
  attrib B1_14    length=4 label="Month of birth";
  attrib B2_14    length=4 label="Year of birth";
  attrib B3_14    length=4 label="Date of birth (CMC)";
  attrib B4_14    length=4 format=B4_. label="Sex of child";
  attrib B5_14    length=4 format=B5_. label="Child is alive";
  attrib B6_14    length=4 format=B6_. label="Age at death";
  attrib B7_14    length=4 label="Age at death (months-imputed)";
  attrib B8_14    length=4 label="Current age of child";
  attrib B9_14    length=4 format=B9_. label="Who child lives with";
  attrib B10_14   length=4 format=B10_. label="Completeness of information";
  attrib B11_14   length=4 label="Preceding birth interval";
  attrib B12_14   length=4 label="Succeeding birth interval";
  attrib B13_14   length=4 format=B13_. label="Flag for age at death";
  attrib B14_14   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_14   length=4 label="Live birth between births -NA";
  attrib BIDX_15  length=4 label="Birth column number";
  attrib BORD_15  length=4 label="Birth order number";
  attrib B0_15    length=4 format=B0_. label="Child is twin";
  attrib B1_15    length=4 label="Month of birth";
  attrib B2_15    length=4 label="Year of birth";
  attrib B3_15    length=4 label="Date of birth (CMC)";
  attrib B4_15    length=4 format=B4_. label="Sex of child";
  attrib B5_15    length=4 format=B5_. label="Child is alive";
  attrib B6_15    length=4 format=B6_. label="Age at death";
  attrib B7_15    length=4 label="Age at death (months-imputed)";
  attrib B8_15    length=4 label="Current age of child";
  attrib B9_15    length=4 format=B9_. label="Who child lives with";
  attrib B10_15   length=4 format=B10_. label="Completeness of information";
  attrib B11_15   length=4 label="Preceding birth interval";
  attrib B12_15   length=4 label="Succeeding birth interval";
  attrib B13_15   length=4 format=B13_. label="Flag for age at death";
  attrib B14_15   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_15   length=4 label="Live birth between births -NA";
  attrib BIDX_16  length=4 label="Birth column number";
  attrib BORD_16  length=4 label="Birth order number";
  attrib B0_16    length=4 format=B0_. label="Child is twin";
  attrib B1_16    length=4 label="Month of birth";
  attrib B2_16    length=4 label="Year of birth";
  attrib B3_16    length=4 label="Date of birth (CMC)";
  attrib B4_16    length=4 format=B4_. label="Sex of child";
  attrib B5_16    length=4 format=B5_. label="Child is alive";
  attrib B6_16    length=4 format=B6_. label="Age at death";
  attrib B7_16    length=4 label="Age at death (months-imputed)";
  attrib B8_16    length=4 label="Current age of child";
  attrib B9_16    length=4 format=B9_. label="Who child lives with";
  attrib B10_16   length=4 format=B10_. label="Completeness of information";
  attrib B11_16   length=4 label="Preceding birth interval";
  attrib B12_16   length=4 label="Succeeding birth interval";
  attrib B13_16   length=4 format=B13_. label="Flag for age at death";
  attrib B14_16   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_16   length=4 label="Live birth between births -NA";
  attrib BIDX_17  length=4 label="Birth column number";
  attrib BORD_17  length=4 label="Birth order number";
  attrib B0_17    length=4 format=B0_. label="Child is twin";
  attrib B1_17    length=4 label="Month of birth";
  attrib B2_17    length=4 label="Year of birth";
  attrib B3_17    length=4 label="Date of birth (CMC)";
  attrib B4_17    length=4 format=B4_. label="Sex of child";
  attrib B5_17    length=4 format=B5_. label="Child is alive";
  attrib B6_17    length=4 format=B6_. label="Age at death";
  attrib B7_17    length=4 label="Age at death (months-imputed)";
  attrib B8_17    length=4 label="Current age of child";
  attrib B9_17    length=4 format=B9_. label="Who child lives with";
  attrib B10_17   length=4 format=B10_. label="Completeness of information";
  attrib B11_17   length=4 label="Preceding birth interval";
  attrib B12_17   length=4 label="Succeeding birth interval";
  attrib B13_17   length=4 format=B13_. label="Flag for age at death";
  attrib B14_17   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_17   length=4 label="Live birth between births -NA";
  attrib BIDX_18  length=4 label="Birth column number";
  attrib BORD_18  length=4 label="Birth order number";
  attrib B0_18    length=4 format=B0_. label="Child is twin";
  attrib B1_18    length=4 label="Month of birth";
  attrib B2_18    length=4 label="Year of birth";
  attrib B3_18    length=4 label="Date of birth (CMC)";
  attrib B4_18    length=4 format=B4_. label="Sex of child";
  attrib B5_18    length=4 format=B5_. label="Child is alive";
  attrib B6_18    length=4 format=B6_. label="Age at death";
  attrib B7_18    length=4 label="Age at death (months-imputed)";
  attrib B8_18    length=4 label="Current age of child";
  attrib B9_18    length=4 format=B9_. label="Who child lives with";
  attrib B10_18   length=4 format=B10_. label="Completeness of information";
  attrib B11_18   length=4 label="Preceding birth interval";
  attrib B12_18   length=4 label="Succeeding birth interval";
  attrib B13_18   length=4 format=B13_. label="Flag for age at death";
  attrib B14_18   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_18   length=4 label="Live birth between births -NA";
  attrib BIDX_19  length=4 label="Birth column number";
  attrib BORD_19  length=4 label="Birth order number";
  attrib B0_19    length=4 format=B0_. label="Child is twin";
  attrib B1_19    length=4 label="Month of birth";
  attrib B2_19    length=4 label="Year of birth";
  attrib B3_19    length=4 label="Date of birth (CMC)";
  attrib B4_19    length=4 format=B4_. label="Sex of child";
  attrib B5_19    length=4 format=B5_. label="Child is alive";
  attrib B6_19    length=4 format=B6_. label="Age at death";
  attrib B7_19    length=4 label="Age at death (months-imputed)";
  attrib B8_19    length=4 label="Current age of child";
  attrib B9_19    length=4 format=B9_. label="Who child lives with";
  attrib B10_19   length=4 format=B10_. label="Completeness of information";
  attrib B11_19   length=4 label="Preceding birth interval";
  attrib B12_19   length=4 label="Succeeding birth interval";
  attrib B13_19   length=4 format=B13_. label="Flag for age at death";
  attrib B14_19   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_19   length=4 label="Live birth between births -NA";
  attrib BIDX_20  length=4 label="Birth column number";
  attrib BORD_20  length=4 label="Birth order number";
  attrib B0_20    length=4 format=B0_. label="Child is twin";
  attrib B1_20    length=4 label="Month of birth";
  attrib B2_20    length=4 label="Year of birth";
  attrib B3_20    length=4 label="Date of birth (CMC)";
  attrib B4_20    length=4 format=B4_. label="Sex of child";
  attrib B5_20    length=4 format=B5_. label="Child is alive";
  attrib B6_20    length=4 format=B6_. label="Age at death";
  attrib B7_20    length=4 label="Age at death (months-imputed)";
  attrib B8_20    length=4 label="Current age of child";
  attrib B9_20    length=4 format=B9_. label="Who child lives with";
  attrib B10_20   length=4 format=B10_. label="Completeness of information";
  attrib B11_20   length=4 label="Preceding birth interval";
  attrib B12_20   length=4 label="Succeeding birth interval";
  attrib B13_20   length=4 format=B13_. label="Flag for age at death";
  attrib B14_20   length=4 label="Birth interval >= 4 years -NA";
  attrib B15_20   length=4 label="Live birth between births -NA";
  attrib V201     length=4 label="Total children ever born";
  attrib V202     length=4 label="Sons at home";
  attrib V203     length=4 label="Daughters at home";
  attrib V204     length=4 label="Sons elsewhere";
  attrib V205     length=4 label="Daughters elsewhere";
  attrib V206     length=4 label="Sons who have died";
  attrib V207     length=4 label="Daughters who have died";
  attrib V208     length=4 label="Births in last five years";
  attrib V209     length=4 label="Births in past year";
  attrib V210     length=4 label="Births in month of interview";
  attrib V211     length=4 label="Date of first birth (CMC)";
  attrib V212     length=4 label="Age of respondent at 1st birth";
  attrib V213     length=4 format=V213_. label="Currently pregnant";
  attrib V214     length=4 label="Duration of current pregnancy";
  attrib V215     length=4 format=V215_. label="Time since last menstrual perd";
  attrib V216     length=4 format=V216_. label="Menstruated in last six weeks";
  attrib V217     length=4 label="Knowledge of ovulatory cycl-NA";
  attrib V218     length=4 label="Number of living children";
  attrib V219     length=4 label="Living children + current preg";
  attrib V220     length=4 format=V220_. label="Living children + curr preg 6+";
  attrib V221     length=4 format=V221_. label="Marriage to first birth int.";
  attrib V222     length=4 label="Last birth to interview";
  attrib V223     length=4 format=V223_. label="Completeness of curr preg. inf";
  attrib V224     length=4 label="Entries in birth history";
  attrib V225     length=4 format=V225_. label="Current pregnancy wanted";
  attrib V226     length=4 format=V226_. label="Time since last period (comp)";
  attrib V227     length=4 format=V227_. label="Flag for last period";
  attrib V228     length=4 format=V228_. label="Ever had a terminated preg.";
  attrib V229     length=4 format=V229_. label="Month pregnancy ended";
  attrib V230     length=4 format=V230_. label="Year pregnancy ended";
  attrib V231     length=4 format=V231_. label="CMC pregnancy ended";
  attrib V232     length=4 format=V232_. label="Completeness of last term. inf";
  attrib V233     length=4 format=V233_. label="Months when pregnancy ended";
  attrib V234     length=4 format=V234_. label="Other such pregnancies";
  attrib V235     length=4 format=V235_. label="Index last child prior to cal.";
  attrib V236     length=4 label="Last birth interval >= 4 ye-NA";
  attrib V237     length=4 label="Birth between last & interv-NA";
  attrib V238     length=4 label="Births in last three years";
  attrib V301     length=4 format=V301_. label="Knowledge of any method";
  attrib V302     length=4 format=V302_. label="Ever use of any method";
  attrib V304A_01 length=4 format=V304A_. label="Type of method";
  attrib V304_01  length=4 format=V304_. label="Knows method";
  attrib V305_01  length=4 format=V305_. label="Ever used method";
  attrib V304A_02 length=4 format=V304A_. label="Type of method";
  attrib V304_02  length=4 format=V304_. label="Knows method";
  attrib V305_02  length=4 format=V305_. label="Ever used method";
  attrib V304A_03 length=4 format=V304A_. label="Type of method";
  attrib V304_03  length=4 format=V304_. label="Knows method";
  attrib V305_03  length=4 format=V305_. label="Ever used method";
  attrib V304A_04 length=4 format=V304A_. label="Type of method";
  attrib V304_04  length=4 format=V304_. label="Knows method";
  attrib V305_04  length=4 format=V305_. label="Ever used method";
  attrib V304A_05 length=4 format=V304A_. label="Type of method";
  attrib V304_05  length=4 format=V304_. label="Knows method";
  attrib V305_05  length=4 format=V305_. label="Ever used method";
  attrib V304A_06 length=4 format=V304A_. label="Type of method";
  attrib V304_06  length=4 format=V304_. label="Knows method";
  attrib V305_06  length=4 format=V305_. label="Ever used method";
  attrib V304A_07 length=4 format=V304A_. label="Type of method";
  attrib V304_07  length=4 format=V304_. label="Knows method";
  attrib V305_07  length=4 format=V305_. label="Ever used method";
  attrib V304A_08 length=4 format=V304A_. label="Type of method";
  attrib V304_08  length=4 format=V304_. label="Knows method";
  attrib V305_08  length=4 format=V305_. label="Ever used method";
  attrib V304A_09 length=4 format=V304A_. label="Type of method";
  attrib V304_09  length=4 format=V304_. label="Knows method";
  attrib V305_09  length=4 format=V305_. label="Ever used method";
  attrib V304A_10 length=4 format=V304A_. label="Type of method";
  attrib V304_10  length=4 format=V304_. label="Knows method";
  attrib V305_10  length=4 format=V305_. label="Ever used method";
  attrib V304A_11 length=4 format=V304A_. label="Type of method";
  attrib V304_11  length=4 format=V304_. label="Knows method";
  attrib V305_11  length=4 format=V305_. label="Ever used method";
  attrib V304A_12 length=4 format=V304A_. label="Type of method";
  attrib V304_12  length=4 format=V304_. label="Knows method";
  attrib V305_12  length=4 format=V305_. label="Ever used method";
  attrib V304A_13 length=4 format=V304A_. label="Type of method";
  attrib V304_13  length=4 format=V304_. label="Knows method";
  attrib V305_13  length=4 format=V305_. label="Ever used method";
  attrib V304A_14 length=4 format=V304A_. label="Type of method";
  attrib V304_14  length=4 format=V304_. label="Knows method";
  attrib V305_14  length=4 format=V305_. label="Ever used method";
  attrib V304A_15 length=4 format=V304A_. label="Type of method";
  attrib V304_15  length=4 format=V304_. label="Knows method";
  attrib V305_15  length=4 format=V305_. label="Ever used method";
  attrib V310     length=4 label="Living children at first use";
  attrib V311     length=4 format=V311_. label="Children at first use (grpd)";
  attrib V312     length=4 format=V312_. label="Current contraceptive method";
  attrib V313     length=4 format=V313_. label="Current use by method type";
  attrib V314     length=4 label="Method of periodic abstinen-NA";
  attrib V315     length=4 label="Month of sterilization";
  attrib V316     length=4 label="Year of sterilization";
  attrib V317     length=4 label="Date of sterilization (CMC)";
  attrib V318     length=4 format=V318_. label="Completeness of information";
  attrib V319     length=4 format=V319_. label="Years since sterilization";
  attrib V320     length=4 format=V320_. label="Age at sterilization";
  attrib V321     length=4 format=V321_. label="Marital duration at steriliz.";
  attrib V322     length=4 format=V322_. label="Parity at sterilization";
  attrib V323     length=4 format=V323_. label="Brand of pill used";
  attrib V325     length=4 label="Cost of pills";
  attrib V326     length=4 format=V326_. label="Last source for current users";
  attrib V327     length=4 format=V327_. label="Last source for users by type";
  attrib V328     length=4 label="Main reason use current sou-NA";
  attrib V337     length=4 format=V337_. label="Months of use of current meth";
  attrib V359     length=4 format=V359_. label="Last method disc. last 5 years";
  attrib V360     length=4 format=V360_. label="Reason of last discontinuation";
  attrib V361     length=4 format=V361_. label="Pattern of use";
  attrib V362     length=4 format=V362_. label="Intention to use";
  attrib V363     length=4 format=V363_. label="Preferred future method";
  attrib V364     length=4 format=V364_. label="Contraceptive use & intention";
  attrib V366A    length=4 format=V366A_. label="Acceptability of radio message";
  attrib V366B    length=4 label="Acceptability of TV message-NA";
  attrib V367     length=4 format=V367_. label="Wanted last child";
  attrib V369     length=4 format=V369_. label="First method used";
  attrib V369B    length=4 label="First use to space or limit-NA";
  attrib V372     length=4 format=V372_. label="Shown pill package";
  attrib V375A    length=4 format=V375A_. label="Main reason not using a method";
  attrib V376     length=4 format=V376_. label="Main reason not to use a meth.";
  attrib V376A    length=4 label="Would ever use method if ma-NA";
  attrib V379     length=4 format=V379_. label="Source known for any method";
  attrib V380     length=4 format=V380_. label="Source known for any method";
  attrib V384A    length=4 format=V384A_. label="Heard FP on radio last months";
  attrib V384B    length=4 format=V384B_. label="Heard FP on TV last months";
  attrib V384C    length=4 label="Heard FP newspaper last mon-NA";
  attrib V384D    length=4 format=V384D_. label="Heard FP on poster last months";
  attrib V384E    length=4 label="Heard FP brochures last mon-NA";
  attrib V393     length=4 format=V393_. label="Visited by FP worker last 6m";
  attrib V394     length=4 format=V394_. label="Visited health facil. last 12m";
  attrib V395     length=4 format=V395_. label="At health facility, told of FP";
  attrib V396     length=4 format=V396_. label="Breastfeeding affect pregnancy";
  attrib V397     length=4 label="Pregnancy risk of breastfee-NA";
  attrib V398     length=4 label="Relied breastfeed. avoid pr-NA";
  attrib V399     length=4 label="Currently rely on breastfee-NA";
  attrib MIDX_1   length=4 label="Index to birth history";
  attrib M1_1     length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A_1    length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B_1    length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C_1    length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D_1    length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E_1    length=4 label="Prenatal: CS health profess-NA";
  attrib M2F_1    length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G_1    length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H_1    length=4 label="Prenatal: relative -NA";
  attrib M2I_1    length=4 label="Prenatal: CS other person -NA";
  attrib M2J_1    length=4 label="Prenatal: CS other person -NA";
  attrib M2K_1    length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L_1    length=4 label="Prenatal: CS other -NA";
  attrib M2M_1    length=4 label="Prenatal: CS other -NA";
  attrib M2N_1    length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A_1    length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B_1    length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C_1    length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D_1    length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E_1    length=4 label="Assistance: CS health profe-NA";
  attrib M3F_1    length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G_1    length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H_1    length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I_1    length=4 label="Assistance: CS other person-NA";
  attrib M3J_1    length=4 label="Assistance: CS other person-NA";
  attrib M3K_1    length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L_1    length=4 label="Assistance: CS other -NA";
  attrib M3M_1    length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N_1    length=4 format=M3N_. label="Assistance: no one";
  attrib M4_1     length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5_1     length=4 format=M5_. label="Months of breastfeeding";
  attrib M6_1     length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7_1     length=4 format=M7_. label="Months of amenorrhea";
  attrib M8_1     length=4 format=M8_. label="Duration of abstinence";
  attrib M9_1     length=4 format=M9_. label="Months of abstinence";
  attrib M10_1    length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11_1    length=4 label="Time would have waited -NA";
  attrib M13_1    length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14_1    length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15_1    length=4 format=M15_. label="Place of delivery";
  attrib M17_1    length=4 label="Delivery by caesarian secti-NA";
  attrib M18_1    length=4 label="Size of child at birth -NA";
  attrib M19_1    length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A_1   length=4 label="Weight at birth recall -NA";
  attrib M21_1    length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27_1    length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28_1    length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29_1    length=4 format=M29_. label="Flag for abstinence";
  attrib M30_1    length=4 label="At birth - prolonged labor -NA";
  attrib M31_1    length=4 label="At birth - excessive bleedi-NA";
  attrib M32_1    length=4 label="At birth - high fever/disch-NA";
  attrib M33_1    length=4 label="At birth - convulsions -NA";
  attrib M34_1    length=4 format=M34_. label="When child put to breast";
  attrib M35_1    length=4 format=M35_. label="Times breastfed during night";
  attrib M36_1    length=4 format=M36_. label="Times breastfed during day";
  attrib M37A_1   length=4 format=M37A_. label="Gave child plain water";
  attrib M37B_1   length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C_1   length=4 format=M37C_. label="Gave child juice";
  attrib M37D_1   length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E_1   length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F_1   length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G_1   length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H_1   length=4 label="Country specific other liqu-NA";
  attrib M37I_1   length=4 label="Country specific other liqu-NA";
  attrib M37J_1   length=4 label="Country specific other liqu-NA";
  attrib M37K_1   length=4 label="Country specific other liqu-NA";
  attrib M37L_1   length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M_1   length=4 label="Country specific other food-NA";
  attrib M37N_1   length=4 label="Country specific other food-NA";
  attrib M37O_1   length=4 label="Country specific other food-NA";
  attrib M37P_1   length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q_1   length=4 label="Food made from local grain -NA";
  attrib M37R_1   length=4 label="Food made from local tuber -NA";
  attrib M37S_1   length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T_1   length=4 format=M37T_. label="Gave child meat";
  attrib M38_1    length=4 label="Drank from bottle with nipp-NA";
  attrib M39_1    length=4 label="Times ate other food yester-NA";
  attrib M40A_1   length=4 label="Last 7 days - plain water -NA";
  attrib M40B_1   length=4 label="Last 7 days - milk -NA";
  attrib M40C_1   length=4 label="Last 7 days - other liquids-NA";
  attrib M40D_1   length=4 label="Last 7 days - local grain f-NA";
  attrib M40E_1   length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F_1   length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G_1   length=4 label="Last 7 days - meat -NA";
  attrib M40H_1   length=4 label="Last 7 days - other solid f-NA";
  attrib M40I_1   length=4 label="Last 7 days - country speci-NA";
  attrib M40J_1   length=4 label="Last 7 days - country speci-NA";
  attrib M40K_1   length=4 label="Last 7 days - country speci-NA";
  attrib M40L_1   length=4 label="Last 7 days - country speci-NA";
  attrib M40M_1   length=4 label="Last 7 days - country speci-NA";
  attrib M40N_1   length=4 label="Last 7 days - country speci-NA";
  attrib M40O_1   length=4 label="Last 7 days - country speci-NA";
  attrib MIDX_2   length=4 label="Index to birth history";
  attrib M1_2     length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A_2    length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B_2    length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C_2    length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D_2    length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E_2    length=4 label="Prenatal: CS health profess-NA";
  attrib M2F_2    length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G_2    length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H_2    length=4 label="Prenatal: relative -NA";
  attrib M2I_2    length=4 label="Prenatal: CS other person -NA";
  attrib M2J_2    length=4 label="Prenatal: CS other person -NA";
  attrib M2K_2    length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L_2    length=4 label="Prenatal: CS other -NA";
  attrib M2M_2    length=4 label="Prenatal: CS other -NA";
  attrib M2N_2    length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A_2    length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B_2    length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C_2    length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D_2    length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E_2    length=4 label="Assistance: CS health profe-NA";
  attrib M3F_2    length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G_2    length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H_2    length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I_2    length=4 label="Assistance: CS other person-NA";
  attrib M3J_2    length=4 label="Assistance: CS other person-NA";
  attrib M3K_2    length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L_2    length=4 label="Assistance: CS other -NA";
  attrib M3M_2    length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N_2    length=4 format=M3N_. label="Assistance: no one";
  attrib M4_2     length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5_2     length=4 format=M5_. label="Months of breastfeeding";
  attrib M6_2     length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7_2     length=4 format=M7_. label="Months of amenorrhea";
  attrib M8_2     length=4 format=M8_. label="Duration of abstinence";
  attrib M9_2     length=4 format=M9_. label="Months of abstinence";
  attrib M10_2    length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11_2    length=4 label="Time would have waited -NA";
  attrib M13_2    length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14_2    length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15_2    length=4 format=M15_. label="Place of delivery";
  attrib M17_2    length=4 label="Delivery by caesarian secti-NA";
  attrib M18_2    length=4 label="Size of child at birth -NA";
  attrib M19_2    length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A_2   length=4 label="Weight at birth recall -NA";
  attrib M21_2    length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27_2    length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28_2    length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29_2    length=4 format=M29_. label="Flag for abstinence";
  attrib M30_2    length=4 label="At birth - prolonged labor -NA";
  attrib M31_2    length=4 label="At birth - excessive bleedi-NA";
  attrib M32_2    length=4 label="At birth - high fever/disch-NA";
  attrib M33_2    length=4 label="At birth - convulsions -NA";
  attrib M34_2    length=4 format=M34_. label="When child put to breast";
  attrib M35_2    length=4 format=M35_. label="Times breastfed during night";
  attrib M36_2    length=4 format=M36_. label="Times breastfed during day";
  attrib M37A_2   length=4 format=M37A_. label="Gave child plain water";
  attrib M37B_2   length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C_2   length=4 format=M37C_. label="Gave child juice";
  attrib M37D_2   length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E_2   length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F_2   length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G_2   length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H_2   length=4 label="Country specific other liqu-NA";
  attrib M37I_2   length=4 label="Country specific other liqu-NA";
  attrib M37J_2   length=4 label="Country specific other liqu-NA";
  attrib M37K_2   length=4 label="Country specific other liqu-NA";
  attrib M37L_2   length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M_2   length=4 label="Country specific other food-NA";
  attrib M37N_2   length=4 label="Country specific other food-NA";
  attrib M37O_2   length=4 label="Country specific other food-NA";
  attrib M37P_2   length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q_2   length=4 label="Food made from local grain -NA";
  attrib M37R_2   length=4 label="Food made from local tuber -NA";
  attrib M37S_2   length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T_2   length=4 format=M37T_. label="Gave child meat";
  attrib M38_2    length=4 label="Drank from bottle with nipp-NA";
  attrib M39_2    length=4 label="Times ate other food yester-NA";
  attrib M40A_2   length=4 label="Last 7 days - plain water -NA";
  attrib M40B_2   length=4 label="Last 7 days - milk -NA";
  attrib M40C_2   length=4 label="Last 7 days - other liquids-NA";
  attrib M40D_2   length=4 label="Last 7 days - local grain f-NA";
  attrib M40E_2   length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F_2   length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G_2   length=4 label="Last 7 days - meat -NA";
  attrib M40H_2   length=4 label="Last 7 days - other solid f-NA";
  attrib M40I_2   length=4 label="Last 7 days - country speci-NA";
  attrib M40J_2   length=4 label="Last 7 days - country speci-NA";
  attrib M40K_2   length=4 label="Last 7 days - country speci-NA";
  attrib M40L_2   length=4 label="Last 7 days - country speci-NA";
  attrib M40M_2   length=4 label="Last 7 days - country speci-NA";
  attrib M40N_2   length=4 label="Last 7 days - country speci-NA";
  attrib M40O_2   length=4 label="Last 7 days - country speci-NA";
  attrib MIDX_3   length=4 label="Index to birth history";
  attrib M1_3     length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A_3    length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B_3    length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C_3    length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D_3    length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E_3    length=4 label="Prenatal: CS health profess-NA";
  attrib M2F_3    length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G_3    length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H_3    length=4 label="Prenatal: relative -NA";
  attrib M2I_3    length=4 label="Prenatal: CS other person -NA";
  attrib M2J_3    length=4 label="Prenatal: CS other person -NA";
  attrib M2K_3    length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L_3    length=4 label="Prenatal: CS other -NA";
  attrib M2M_3    length=4 label="Prenatal: CS other -NA";
  attrib M2N_3    length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A_3    length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B_3    length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C_3    length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D_3    length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E_3    length=4 label="Assistance: CS health profe-NA";
  attrib M3F_3    length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G_3    length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H_3    length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I_3    length=4 label="Assistance: CS other person-NA";
  attrib M3J_3    length=4 label="Assistance: CS other person-NA";
  attrib M3K_3    length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L_3    length=4 label="Assistance: CS other -NA";
  attrib M3M_3    length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N_3    length=4 format=M3N_. label="Assistance: no one";
  attrib M4_3     length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5_3     length=4 format=M5_. label="Months of breastfeeding";
  attrib M6_3     length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7_3     length=4 format=M7_. label="Months of amenorrhea";
  attrib M8_3     length=4 format=M8_. label="Duration of abstinence";
  attrib M9_3     length=4 format=M9_. label="Months of abstinence";
  attrib M10_3    length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11_3    length=4 label="Time would have waited -NA";
  attrib M13_3    length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14_3    length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15_3    length=4 format=M15_. label="Place of delivery";
  attrib M17_3    length=4 label="Delivery by caesarian secti-NA";
  attrib M18_3    length=4 label="Size of child at birth -NA";
  attrib M19_3    length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A_3   length=4 label="Weight at birth recall -NA";
  attrib M21_3    length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27_3    length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28_3    length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29_3    length=4 format=M29_. label="Flag for abstinence";
  attrib M30_3    length=4 label="At birth - prolonged labor -NA";
  attrib M31_3    length=4 label="At birth - excessive bleedi-NA";
  attrib M32_3    length=4 label="At birth - high fever/disch-NA";
  attrib M33_3    length=4 label="At birth - convulsions -NA";
  attrib M34_3    length=4 format=M34_. label="When child put to breast";
  attrib M35_3    length=4 format=M35_. label="Times breastfed during night";
  attrib M36_3    length=4 format=M36_. label="Times breastfed during day";
  attrib M37A_3   length=4 format=M37A_. label="Gave child plain water";
  attrib M37B_3   length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C_3   length=4 format=M37C_. label="Gave child juice";
  attrib M37D_3   length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E_3   length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F_3   length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G_3   length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H_3   length=4 label="Country specific other liqu-NA";
  attrib M37I_3   length=4 label="Country specific other liqu-NA";
  attrib M37J_3   length=4 label="Country specific other liqu-NA";
  attrib M37K_3   length=4 label="Country specific other liqu-NA";
  attrib M37L_3   length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M_3   length=4 label="Country specific other food-NA";
  attrib M37N_3   length=4 label="Country specific other food-NA";
  attrib M37O_3   length=4 label="Country specific other food-NA";
  attrib M37P_3   length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q_3   length=4 label="Food made from local grain -NA";
  attrib M37R_3   length=4 label="Food made from local tuber -NA";
  attrib M37S_3   length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T_3   length=4 format=M37T_. label="Gave child meat";
  attrib M38_3    length=4 label="Drank from bottle with nipp-NA";
  attrib M39_3    length=4 label="Times ate other food yester-NA";
  attrib M40A_3   length=4 label="Last 7 days - plain water -NA";
  attrib M40B_3   length=4 label="Last 7 days - milk -NA";
  attrib M40C_3   length=4 label="Last 7 days - other liquids-NA";
  attrib M40D_3   length=4 label="Last 7 days - local grain f-NA";
  attrib M40E_3   length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F_3   length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G_3   length=4 label="Last 7 days - meat -NA";
  attrib M40H_3   length=4 label="Last 7 days - other solid f-NA";
  attrib M40I_3   length=4 label="Last 7 days - country speci-NA";
  attrib M40J_3   length=4 label="Last 7 days - country speci-NA";
  attrib M40K_3   length=4 label="Last 7 days - country speci-NA";
  attrib M40L_3   length=4 label="Last 7 days - country speci-NA";
  attrib M40M_3   length=4 label="Last 7 days - country speci-NA";
  attrib M40N_3   length=4 label="Last 7 days - country speci-NA";
  attrib M40O_3   length=4 label="Last 7 days - country speci-NA";
  attrib MIDX_4   length=4 label="Index to birth history";
  attrib M1_4     length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A_4    length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B_4    length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C_4    length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D_4    length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E_4    length=4 label="Prenatal: CS health profess-NA";
  attrib M2F_4    length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G_4    length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H_4    length=4 label="Prenatal: relative -NA";
  attrib M2I_4    length=4 label="Prenatal: CS other person -NA";
  attrib M2J_4    length=4 label="Prenatal: CS other person -NA";
  attrib M2K_4    length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L_4    length=4 label="Prenatal: CS other -NA";
  attrib M2M_4    length=4 label="Prenatal: CS other -NA";
  attrib M2N_4    length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A_4    length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B_4    length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C_4    length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D_4    length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E_4    length=4 label="Assistance: CS health profe-NA";
  attrib M3F_4    length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G_4    length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H_4    length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I_4    length=4 label="Assistance: CS other person-NA";
  attrib M3J_4    length=4 label="Assistance: CS other person-NA";
  attrib M3K_4    length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L_4    length=4 label="Assistance: CS other -NA";
  attrib M3M_4    length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N_4    length=4 format=M3N_. label="Assistance: no one";
  attrib M4_4     length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5_4     length=4 format=M5_. label="Months of breastfeeding";
  attrib M6_4     length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7_4     length=4 format=M7_. label="Months of amenorrhea";
  attrib M8_4     length=4 format=M8_. label="Duration of abstinence";
  attrib M9_4     length=4 format=M9_. label="Months of abstinence";
  attrib M10_4    length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11_4    length=4 label="Time would have waited -NA";
  attrib M13_4    length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14_4    length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15_4    length=4 format=M15_. label="Place of delivery";
  attrib M17_4    length=4 label="Delivery by caesarian secti-NA";
  attrib M18_4    length=4 label="Size of child at birth -NA";
  attrib M19_4    length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A_4   length=4 label="Weight at birth recall -NA";
  attrib M21_4    length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27_4    length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28_4    length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29_4    length=4 format=M29_. label="Flag for abstinence";
  attrib M30_4    length=4 label="At birth - prolonged labor -NA";
  attrib M31_4    length=4 label="At birth - excessive bleedi-NA";
  attrib M32_4    length=4 label="At birth - high fever/disch-NA";
  attrib M33_4    length=4 label="At birth - convulsions -NA";
  attrib M34_4    length=4 format=M34_. label="When child put to breast";
  attrib M35_4    length=4 format=M35_. label="Times breastfed during night";
  attrib M36_4    length=4 format=M36_. label="Times breastfed during day";
  attrib M37A_4   length=4 format=M37A_. label="Gave child plain water";
  attrib M37B_4   length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C_4   length=4 format=M37C_. label="Gave child juice";
  attrib M37D_4   length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E_4   length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F_4   length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G_4   length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H_4   length=4 label="Country specific other liqu-NA";
  attrib M37I_4   length=4 label="Country specific other liqu-NA";
  attrib M37J_4   length=4 label="Country specific other liqu-NA";
  attrib M37K_4   length=4 label="Country specific other liqu-NA";
  attrib M37L_4   length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M_4   length=4 label="Country specific other food-NA";
  attrib M37N_4   length=4 label="Country specific other food-NA";
  attrib M37O_4   length=4 label="Country specific other food-NA";
  attrib M37P_4   length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q_4   length=4 label="Food made from local grain -NA";
  attrib M37R_4   length=4 label="Food made from local tuber -NA";
  attrib M37S_4   length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T_4   length=4 format=M37T_. label="Gave child meat";
  attrib M38_4    length=4 label="Drank from bottle with nipp-NA";
  attrib M39_4    length=4 label="Times ate other food yester-NA";
  attrib M40A_4   length=4 label="Last 7 days - plain water -NA";
  attrib M40B_4   length=4 label="Last 7 days - milk -NA";
  attrib M40C_4   length=4 label="Last 7 days - other liquids-NA";
  attrib M40D_4   length=4 label="Last 7 days - local grain f-NA";
  attrib M40E_4   length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F_4   length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G_4   length=4 label="Last 7 days - meat -NA";
  attrib M40H_4   length=4 label="Last 7 days - other solid f-NA";
  attrib M40I_4   length=4 label="Last 7 days - country speci-NA";
  attrib M40J_4   length=4 label="Last 7 days - country speci-NA";
  attrib M40K_4   length=4 label="Last 7 days - country speci-NA";
  attrib M40L_4   length=4 label="Last 7 days - country speci-NA";
  attrib M40M_4   length=4 label="Last 7 days - country speci-NA";
  attrib M40N_4   length=4 label="Last 7 days - country speci-NA";
  attrib M40O_4   length=4 label="Last 7 days - country speci-NA";
  attrib MIDX_5   length=4 label="Index to birth history";
  attrib M1_5     length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A_5    length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B_5    length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C_5    length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D_5    length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E_5    length=4 label="Prenatal: CS health profess-NA";
  attrib M2F_5    length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G_5    length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H_5    length=4 label="Prenatal: relative -NA";
  attrib M2I_5    length=4 label="Prenatal: CS other person -NA";
  attrib M2J_5    length=4 label="Prenatal: CS other person -NA";
  attrib M2K_5    length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L_5    length=4 label="Prenatal: CS other -NA";
  attrib M2M_5    length=4 label="Prenatal: CS other -NA";
  attrib M2N_5    length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A_5    length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B_5    length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C_5    length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D_5    length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E_5    length=4 label="Assistance: CS health profe-NA";
  attrib M3F_5    length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G_5    length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H_5    length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I_5    length=4 label="Assistance: CS other person-NA";
  attrib M3J_5    length=4 label="Assistance: CS other person-NA";
  attrib M3K_5    length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L_5    length=4 label="Assistance: CS other -NA";
  attrib M3M_5    length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N_5    length=4 format=M3N_. label="Assistance: no one";
  attrib M4_5     length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5_5     length=4 format=M5_. label="Months of breastfeeding";
  attrib M6_5     length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7_5     length=4 format=M7_. label="Months of amenorrhea";
  attrib M8_5     length=4 format=M8_. label="Duration of abstinence";
  attrib M9_5     length=4 format=M9_. label="Months of abstinence";
  attrib M10_5    length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11_5    length=4 label="Time would have waited -NA";
  attrib M13_5    length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14_5    length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15_5    length=4 format=M15_. label="Place of delivery";
  attrib M17_5    length=4 label="Delivery by caesarian secti-NA";
  attrib M18_5    length=4 label="Size of child at birth -NA";
  attrib M19_5    length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A_5   length=4 label="Weight at birth recall -NA";
  attrib M21_5    length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27_5    length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28_5    length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29_5    length=4 format=M29_. label="Flag for abstinence";
  attrib M30_5    length=4 label="At birth - prolonged labor -NA";
  attrib M31_5    length=4 label="At birth - excessive bleedi-NA";
  attrib M32_5    length=4 label="At birth - high fever/disch-NA";
  attrib M33_5    length=4 label="At birth - convulsions -NA";
  attrib M34_5    length=4 format=M34_. label="When child put to breast";
  attrib M35_5    length=4 format=M35_. label="Times breastfed during night";
  attrib M36_5    length=4 format=M36_. label="Times breastfed during day";
  attrib M37A_5   length=4 format=M37A_. label="Gave child plain water";
  attrib M37B_5   length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C_5   length=4 format=M37C_. label="Gave child juice";
  attrib M37D_5   length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E_5   length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F_5   length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G_5   length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H_5   length=4 label="Country specific other liqu-NA";
  attrib M37I_5   length=4 label="Country specific other liqu-NA";
  attrib M37J_5   length=4 label="Country specific other liqu-NA";
  attrib M37K_5   length=4 label="Country specific other liqu-NA";
  attrib M37L_5   length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M_5   length=4 label="Country specific other food-NA";
  attrib M37N_5   length=4 label="Country specific other food-NA";
  attrib M37O_5   length=4 label="Country specific other food-NA";
  attrib M37P_5   length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q_5   length=4 label="Food made from local grain -NA";
  attrib M37R_5   length=4 label="Food made from local tuber -NA";
  attrib M37S_5   length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T_5   length=4 format=M37T_. label="Gave child meat";
  attrib M38_5    length=4 label="Drank from bottle with nipp-NA";
  attrib M39_5    length=4 label="Times ate other food yester-NA";
  attrib M40A_5   length=4 label="Last 7 days - plain water -NA";
  attrib M40B_5   length=4 label="Last 7 days - milk -NA";
  attrib M40C_5   length=4 label="Last 7 days - other liquids-NA";
  attrib M40D_5   length=4 label="Last 7 days - local grain f-NA";
  attrib M40E_5   length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F_5   length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G_5   length=4 label="Last 7 days - meat -NA";
  attrib M40H_5   length=4 label="Last 7 days - other solid f-NA";
  attrib M40I_5   length=4 label="Last 7 days - country speci-NA";
  attrib M40J_5   length=4 label="Last 7 days - country speci-NA";
  attrib M40K_5   length=4 label="Last 7 days - country speci-NA";
  attrib M40L_5   length=4 label="Last 7 days - country speci-NA";
  attrib M40M_5   length=4 label="Last 7 days - country speci-NA";
  attrib M40N_5   length=4 label="Last 7 days - country speci-NA";
  attrib M40O_5   length=4 label="Last 7 days - country speci-NA";
  attrib MIDX_6   length=4 label="Index to birth history";
  attrib M1_6     length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A_6    length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B_6    length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C_6    length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D_6    length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E_6    length=4 label="Prenatal: CS health profess-NA";
  attrib M2F_6    length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G_6    length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H_6    length=4 label="Prenatal: relative -NA";
  attrib M2I_6    length=4 label="Prenatal: CS other person -NA";
  attrib M2J_6    length=4 label="Prenatal: CS other person -NA";
  attrib M2K_6    length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L_6    length=4 label="Prenatal: CS other -NA";
  attrib M2M_6    length=4 label="Prenatal: CS other -NA";
  attrib M2N_6    length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A_6    length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B_6    length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C_6    length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D_6    length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E_6    length=4 label="Assistance: CS health profe-NA";
  attrib M3F_6    length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G_6    length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H_6    length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I_6    length=4 label="Assistance: CS other person-NA";
  attrib M3J_6    length=4 label="Assistance: CS other person-NA";
  attrib M3K_6    length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L_6    length=4 label="Assistance: CS other -NA";
  attrib M3M_6    length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N_6    length=4 format=M3N_. label="Assistance: no one";
  attrib M4_6     length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5_6     length=4 format=M5_. label="Months of breastfeeding";
  attrib M6_6     length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7_6     length=4 format=M7_. label="Months of amenorrhea";
  attrib M8_6     length=4 format=M8_. label="Duration of abstinence";
  attrib M9_6     length=4 format=M9_. label="Months of abstinence";
  attrib M10_6    length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11_6    length=4 label="Time would have waited -NA";
  attrib M13_6    length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14_6    length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15_6    length=4 format=M15_. label="Place of delivery";
  attrib M17_6    length=4 label="Delivery by caesarian secti-NA";
  attrib M18_6    length=4 label="Size of child at birth -NA";
  attrib M19_6    length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A_6   length=4 label="Weight at birth recall -NA";
  attrib M21_6    length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27_6    length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28_6    length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29_6    length=4 format=M29_. label="Flag for abstinence";
  attrib M30_6    length=4 label="At birth - prolonged labor -NA";
  attrib M31_6    length=4 label="At birth - excessive bleedi-NA";
  attrib M32_6    length=4 label="At birth - high fever/disch-NA";
  attrib M33_6    length=4 label="At birth - convulsions -NA";
  attrib M34_6    length=4 format=M34_. label="When child put to breast";
  attrib M35_6    length=4 format=M35_. label="Times breastfed during night";
  attrib M36_6    length=4 format=M36_. label="Times breastfed during day";
  attrib M37A_6   length=4 format=M37A_. label="Gave child plain water";
  attrib M37B_6   length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C_6   length=4 format=M37C_. label="Gave child juice";
  attrib M37D_6   length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E_6   length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F_6   length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G_6   length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H_6   length=4 label="Country specific other liqu-NA";
  attrib M37I_6   length=4 label="Country specific other liqu-NA";
  attrib M37J_6   length=4 label="Country specific other liqu-NA";
  attrib M37K_6   length=4 label="Country specific other liqu-NA";
  attrib M37L_6   length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M_6   length=4 label="Country specific other food-NA";
  attrib M37N_6   length=4 label="Country specific other food-NA";
  attrib M37O_6   length=4 label="Country specific other food-NA";
  attrib M37P_6   length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q_6   length=4 label="Food made from local grain -NA";
  attrib M37R_6   length=4 label="Food made from local tuber -NA";
  attrib M37S_6   length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T_6   length=4 format=M37T_. label="Gave child meat";
  attrib M38_6    length=4 label="Drank from bottle with nipp-NA";
  attrib M39_6    length=4 label="Times ate other food yester-NA";
  attrib M40A_6   length=4 label="Last 7 days - plain water -NA";
  attrib M40B_6   length=4 label="Last 7 days - milk -NA";
  attrib M40C_6   length=4 label="Last 7 days - other liquids-NA";
  attrib M40D_6   length=4 label="Last 7 days - local grain f-NA";
  attrib M40E_6   length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F_6   length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G_6   length=4 label="Last 7 days - meat -NA";
  attrib M40H_6   length=4 label="Last 7 days - other solid f-NA";
  attrib M40I_6   length=4 label="Last 7 days - country speci-NA";
  attrib M40J_6   length=4 label="Last 7 days - country speci-NA";
  attrib M40K_6   length=4 label="Last 7 days - country speci-NA";
  attrib M40L_6   length=4 label="Last 7 days - country speci-NA";
  attrib M40M_6   length=4 label="Last 7 days - country speci-NA";
  attrib M40N_6   length=4 label="Last 7 days - country speci-NA";
  attrib M40O_6   length=4 label="Last 7 days - country speci-NA";
  attrib V401     length=4 format=V401_. label="Last birth ceasarean sectio-NA";
  attrib V404     length=4 format=V404_. label="Currently breastfeeding";
  attrib V405     length=4 format=V405_. label="Currently amenorrheic";
  attrib V406     length=4 format=V406_. label="Currently abstaining";
  attrib V407     length=4 format=V407_. label="Times breastfed during night";
  attrib V408     length=4 format=V408_. label="Times breastfed during day";
  attrib V409     length=4 format=V409_. label="Gave child plain water";
  attrib V409A    length=4 format=V409A_. label="Gave child sugar water";
  attrib V410     length=4 format=V410_. label="Gave child juice";
  attrib V410A    length=4 format=V410A_. label="Gave child herbal tea";
  attrib V411     length=4 label="Gave child powder/tinned mi-NA";
  attrib V411A    length=4 format=V411A_. label="Gave child baby formula";
  attrib V412     length=4 format=V412_. label="Gave child fresh milk";
  attrib V413A    length=4 label="Country specific other liqu-NA";
  attrib V413B    length=4 label="Country specific other liqu-NA";
  attrib V413C    length=4 label="Country specific other liqu-NA";
  attrib V413D    length=4 label="Country specific other liqu-NA";
  attrib V413     length=4 format=V413_. label="Gave child other liquid";
  attrib V414A    length=4 label="Country specific other food-NA";
  attrib V414B    length=4 label="Country specific other food-NA";
  attrib V414C    length=4 label="Country specific other food-NA";
  attrib V414D    length=4 format=V414D_. label="Other solid, semi-solid foods";
  attrib V414E    length=4 format=V414E_. label="Food made from local grain -NA";
  attrib V414F    length=4 format=V414F_. label="Food made from local tuber -NA";
  attrib V414G    length=4 format=V414G_. label="Gave child eggs, fish, poul-NA";
  attrib V414H    length=4 format=V414H_. label="Gave child meat";
  attrib V415     length=4 label="Drank from bottle with nipp-NA";
  attrib V416     length=4 format=V416_. label="Heard of oral rehydration";
  attrib V417     length=4 label="Entries in maternity table";
  attrib V418     length=4 label="Entries in health table";
  attrib V419     length=4 label="Entries in height/weight ta-NA";
  attrib V420     length=4 label="Measurer's code -NA";
  attrib V421     length=4 label="Assistant measurer's code -NA";
  attrib V426     length=4 format=V426_. label="When child put to breast";
  attrib V436     length=4 label="Arm circumference (cms-1d) -NA";
  attrib V437     length=4 label="Respondent's weight (kilos--NA";
  attrib V438     length=4 label="Respondent's height (cms-1d-NA";
  attrib V439     length=4 label="Ht/A Percentile (resp.) -NA";
  attrib V440     length=4 label="Ht/A Standard deviations (r-NA";
  attrib V441     length=4 label="Ht/A Percent ref. median (r-NA";
  attrib V442     length=4 label="Wt/Ht Percent ref. median (-NA";
  attrib V443     length=4 label="Wt/Ht Percent ref. median (-NA";
  attrib V444     length=4 label="Wt/Ht Percent ref. median (-NA";
  attrib V444A    length=4 label="Wt/Ht Std deviations(resp) -NA";
  attrib V445     length=4 label="Body mass index for respond-NA";
  attrib V446     length=4 label="Rohrer's index for responde-NA";
  attrib V447     length=4 label="Result of measurement of re-NA";
  attrib V448     length=4 label="Drinking pattern with diarr-NA";
  attrib V449     length=4 label="Eating pattern with diarrhe-NA";
  attrib V450A    length=4 label="Diarrhea: repeat watery sto-NA";
  attrib V450B    length=4 label="Diarrhea: Any watery stools-NA";
  attrib V450C    length=4 label="Diarrhea: Repeated vomiting-NA";
  attrib V450D    length=4 label="Diarrhea: Any vomiting -NA";
  attrib V450E    length=4 label="Diarrhea: Blood in stools -NA";
  attrib V450F    length=4 label="Diarrhea: Fever -NA";
  attrib V450G    length=4 label="Diarrhea: Marked thirst -NA";
  attrib V450H    length=4 label="Diarrhea: Not eating/drinki-NA";
  attrib V450I    length=4 label="Diarrhea: Getting sicker -NA";
  attrib V450J    length=4 label="Diarrhea: Not getting bette-NA";
  attrib V450K    length=4 label="Diarrhea: Country specific -NA";
  attrib V450L    length=4 label="Diarrhea: Country specific -NA";
  attrib V450M    length=4 label="Diarrhea: Country specific -NA";
  attrib V450X    length=4 label="Diarrhea: Other responses -NA";
  attrib V450Z    length=4 label="Diarrhea: Does not know -NA";
  attrib V451A    length=4 label="Cough: Fast breathing -NA";
  attrib V451B    length=4 label="Cough: Difficult breathing -NA";
  attrib V451C    length=4 label="Cough: Noisy breathing -NA";
  attrib V451D    length=4 label="Cough: Fever -NA";
  attrib V451E    length=4 label="Cough: Unable to drink -NA";
  attrib V451F    length=4 label="Cough: Not eating/drinking -NA";
  attrib V451G    length=4 label="Cough: Getting sicker -NA";
  attrib V451H    length=4 label="Cough: Not getting better -NA";
  attrib V451I    length=4 label="Cough: Country specific -NA";
  attrib V451J    length=4 label="Cough: Country specific -NA";
  attrib V451K    length=4 label="Cough: Country specific -NA";
  attrib V451X    length=4 label="Cough: Other responses -NA";
  attrib V451Z    length=4 label="Knows no sign of illness -NA";
  attrib HIDX_1   length=4 label="Index to birth history";
  attrib H1_1     length=4 format=H1_. label="Has health card";
  attrib H2_1     length=4 format=H2_. label="Received BCG";
  attrib H2D_1    length=4 format=H2D_. label="BCG day";
  attrib H2M_1    length=4 format=H2M_. label="BCG month";
  attrib H2Y_1    length=4 format=H2Y_. label="BCG year";
  attrib H3_1     length=4 format=H3_. label="Received DPT 1";
  attrib H3D_1    length=4 format=H3D_. label="DPT 1 day";
  attrib H3M_1    length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y_1    length=4 format=H3Y_. label="DPT 1 year";
  attrib H4_1     length=4 format=H4_. label="Received POLIO 1";
  attrib H4D_1    length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M_1    length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y_1    length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5_1     length=4 format=H5_. label="Received DPT 2";
  attrib H5D_1    length=4 format=H5D_. label="DPT 2 day";
  attrib H5M_1    length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y_1    length=4 format=H5Y_. label="DPT 2 year";
  attrib H6_1     length=4 format=H6_. label="Received POLIO 2";
  attrib H6D_1    length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M_1    length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y_1    length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7_1     length=4 format=H7_. label="Received DPT 3";
  attrib H7D_1    length=4 format=H7D_. label="DPT 3 day";
  attrib H7M_1    length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y_1    length=4 format=H7Y_. label="DPT 3 year";
  attrib H8_1     length=4 format=H8_. label="Received POLIO 3";
  attrib H8D_1    length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M_1    length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y_1    length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9_1     length=4 format=H9_. label="Received MEASLES";
  attrib H9D_1    length=4 format=H9D_. label="MEASLES day";
  attrib H9M_1    length=4 format=H9M_. label="MEASLES month";
  attrib H9Y_1    length=4 format=H9Y_. label="MEASLES year";
  attrib H0_1     length=4 label="Polio 0 -NA";
  attrib H0D_1    length=4 format=H0D_. label="Polio 0 day";
  attrib H0M_1    length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y_1    length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10_1    length=4 format=H10_. label="Ever had vaccination";
  attrib H11_1    length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B_1   length=4 format=H11B_. label="Blood in the stools";
  attrib H11C_1   length=4 label="Bowel movements in worst da-NA";
  attrib H12A_1   length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B_1   length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C_1   length=4 label="Diarrhea: govt health post -NA";
  attrib H12D_1   length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E_1   length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F_1   length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G_1   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H_1   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I_1   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J_1   length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K_1   length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L_1   length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M_1   length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N_1   length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O_1   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P_1   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q_1   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R_1   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S_1   length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T_1   length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U_1   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V_1   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W_1   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X_1   length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y_1   length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z_1   length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13_1    length=4 format=H13_. label="Given oral rehydration";
  attrib H14_1    length=4 format=H14_. label="Given recommend. home solution";
  attrib H15_1    length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A_1   length=4 label="Given antibiotics (CS) -NA";
  attrib H15B_1   length=4 format=H15B_. label="Given an injection";
  attrib H15C_1   length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D_1   length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E_1   length=4 label="Given CS other treatment -NA";
  attrib H15F_1   length=4 label="Given CS other treatment -NA";
  attrib H15G_1   length=4 label="Given CS other treatment -NA";
  attrib H15H_1   length=4 label="Given CS other treatment -NA";
  attrib H16_1    length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18_1    length=4 label="Increase or decrease in foo-NA";
  attrib H20_1    length=4 format=H20_. label="Given other treatment";
  attrib H21A_1   length=4 format=H21A_. label="Given no treatment";
  attrib H21_1    length=4 format=H21_. label="Received any treatment";
  attrib H22_1    length=4 label="Had fever in last two weeks-NA";
  attrib H31_1    length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B_1   length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A_1   length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B_1   length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C_1   length=4 label="Fever/cough: govt health po-NA";
  attrib H32D_1   length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E_1   length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F_1   length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G_1   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H_1   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I_1   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J_1   length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K_1   length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L_1   length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M_1   length=4 label="Fever/cough: private mobile-NA";
  attrib H32N_1   length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O_1   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P_1   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q_1   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R_1   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S_1   length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T_1   length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U_1   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V_1   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W_1   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X_1   length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y_1   length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z_1   length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HIDX_2   length=4 label="Index to birth history";
  attrib H1_2     length=4 format=H1_. label="Has health card";
  attrib H2_2     length=4 format=H2_. label="Received BCG";
  attrib H2D_2    length=4 format=H2D_. label="BCG day";
  attrib H2M_2    length=4 format=H2M_. label="BCG month";
  attrib H2Y_2    length=4 format=H2Y_. label="BCG year";
  attrib H3_2     length=4 format=H3_. label="Received DPT 1";
  attrib H3D_2    length=4 format=H3D_. label="DPT 1 day";
  attrib H3M_2    length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y_2    length=4 format=H3Y_. label="DPT 1 year";
  attrib H4_2     length=4 format=H4_. label="Received POLIO 1";
  attrib H4D_2    length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M_2    length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y_2    length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5_2     length=4 format=H5_. label="Received DPT 2";
  attrib H5D_2    length=4 format=H5D_. label="DPT 2 day";
  attrib H5M_2    length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y_2    length=4 format=H5Y_. label="DPT 2 year";
  attrib H6_2     length=4 format=H6_. label="Received POLIO 2";
  attrib H6D_2    length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M_2    length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y_2    length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7_2     length=4 format=H7_. label="Received DPT 3";
  attrib H7D_2    length=4 format=H7D_. label="DPT 3 day";
  attrib H7M_2    length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y_2    length=4 format=H7Y_. label="DPT 3 year";
  attrib H8_2     length=4 format=H8_. label="Received POLIO 3";
  attrib H8D_2    length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M_2    length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y_2    length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9_2     length=4 format=H9_. label="Received MEASLES";
  attrib H9D_2    length=4 format=H9D_. label="MEASLES day";
  attrib H9M_2    length=4 format=H9M_. label="MEASLES month";
  attrib H9Y_2    length=4 format=H9Y_. label="MEASLES year";
  attrib H0_2     length=4 label="Polio 0 -NA";
  attrib H0D_2    length=4 format=H0D_. label="Polio 0 day";
  attrib H0M_2    length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y_2    length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10_2    length=4 format=H10_. label="Ever had vaccination";
  attrib H11_2    length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B_2   length=4 format=H11B_. label="Blood in the stools";
  attrib H11C_2   length=4 label="Bowel movements in worst da-NA";
  attrib H12A_2   length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B_2   length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C_2   length=4 label="Diarrhea: govt health post -NA";
  attrib H12D_2   length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E_2   length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F_2   length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G_2   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H_2   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I_2   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J_2   length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K_2   length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L_2   length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M_2   length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N_2   length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O_2   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P_2   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q_2   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R_2   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S_2   length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T_2   length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U_2   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V_2   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W_2   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X_2   length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y_2   length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z_2   length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13_2    length=4 format=H13_. label="Given oral rehydration";
  attrib H14_2    length=4 format=H14_. label="Given recommend. home solution";
  attrib H15_2    length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A_2   length=4 label="Given antibiotics (CS) -NA";
  attrib H15B_2   length=4 format=H15B_. label="Given an injection";
  attrib H15C_2   length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D_2   length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E_2   length=4 label="Given CS other treatment -NA";
  attrib H15F_2   length=4 label="Given CS other treatment -NA";
  attrib H15G_2   length=4 label="Given CS other treatment -NA";
  attrib H15H_2   length=4 label="Given CS other treatment -NA";
  attrib H16_2    length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18_2    length=4 label="Increase or decrease in foo-NA";
  attrib H20_2    length=4 format=H20_. label="Given other treatment";
  attrib H21A_2   length=4 format=H21A_. label="Given no treatment";
  attrib H21_2    length=4 format=H21_. label="Received any treatment";
  attrib H22_2    length=4 label="Had fever in last two weeks-NA";
  attrib H31_2    length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B_2   length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A_2   length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B_2   length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C_2   length=4 label="Fever/cough: govt health po-NA";
  attrib H32D_2   length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E_2   length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F_2   length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G_2   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H_2   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I_2   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J_2   length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K_2   length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L_2   length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M_2   length=4 label="Fever/cough: private mobile-NA";
  attrib H32N_2   length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O_2   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P_2   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q_2   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R_2   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S_2   length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T_2   length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U_2   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V_2   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W_2   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X_2   length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y_2   length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z_2   length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HIDX_3   length=4 label="Index to birth history";
  attrib H1_3     length=4 format=H1_. label="Has health card";
  attrib H2_3     length=4 format=H2_. label="Received BCG";
  attrib H2D_3    length=4 format=H2D_. label="BCG day";
  attrib H2M_3    length=4 format=H2M_. label="BCG month";
  attrib H2Y_3    length=4 format=H2Y_. label="BCG year";
  attrib H3_3     length=4 format=H3_. label="Received DPT 1";
  attrib H3D_3    length=4 format=H3D_. label="DPT 1 day";
  attrib H3M_3    length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y_3    length=4 format=H3Y_. label="DPT 1 year";
  attrib H4_3     length=4 format=H4_. label="Received POLIO 1";
  attrib H4D_3    length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M_3    length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y_3    length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5_3     length=4 format=H5_. label="Received DPT 2";
  attrib H5D_3    length=4 format=H5D_. label="DPT 2 day";
  attrib H5M_3    length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y_3    length=4 format=H5Y_. label="DPT 2 year";
  attrib H6_3     length=4 format=H6_. label="Received POLIO 2";
  attrib H6D_3    length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M_3    length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y_3    length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7_3     length=4 format=H7_. label="Received DPT 3";
  attrib H7D_3    length=4 format=H7D_. label="DPT 3 day";
  attrib H7M_3    length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y_3    length=4 format=H7Y_. label="DPT 3 year";
  attrib H8_3     length=4 format=H8_. label="Received POLIO 3";
  attrib H8D_3    length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M_3    length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y_3    length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9_3     length=4 format=H9_. label="Received MEASLES";
  attrib H9D_3    length=4 format=H9D_. label="MEASLES day";
  attrib H9M_3    length=4 format=H9M_. label="MEASLES month";
  attrib H9Y_3    length=4 format=H9Y_. label="MEASLES year";
  attrib H0_3     length=4 label="Polio 0 -NA";
  attrib H0D_3    length=4 format=H0D_. label="Polio 0 day";
  attrib H0M_3    length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y_3    length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10_3    length=4 format=H10_. label="Ever had vaccination";
  attrib H11_3    length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B_3   length=4 format=H11B_. label="Blood in the stools";
  attrib H11C_3   length=4 label="Bowel movements in worst da-NA";
  attrib H12A_3   length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B_3   length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C_3   length=4 label="Diarrhea: govt health post -NA";
  attrib H12D_3   length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E_3   length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F_3   length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G_3   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H_3   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I_3   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J_3   length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K_3   length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L_3   length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M_3   length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N_3   length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O_3   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P_3   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q_3   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R_3   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S_3   length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T_3   length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U_3   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V_3   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W_3   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X_3   length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y_3   length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z_3   length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13_3    length=4 format=H13_. label="Given oral rehydration";
  attrib H14_3    length=4 format=H14_. label="Given recommend. home solution";
  attrib H15_3    length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A_3   length=4 label="Given antibiotics (CS) -NA";
  attrib H15B_3   length=4 format=H15B_. label="Given an injection";
  attrib H15C_3   length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D_3   length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E_3   length=4 label="Given CS other treatment -NA";
  attrib H15F_3   length=4 label="Given CS other treatment -NA";
  attrib H15G_3   length=4 label="Given CS other treatment -NA";
  attrib H15H_3   length=4 label="Given CS other treatment -NA";
  attrib H16_3    length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18_3    length=4 label="Increase or decrease in foo-NA";
  attrib H20_3    length=4 format=H20_. label="Given other treatment";
  attrib H21A_3   length=4 format=H21A_. label="Given no treatment";
  attrib H21_3    length=4 format=H21_. label="Received any treatment";
  attrib H22_3    length=4 label="Had fever in last two weeks-NA";
  attrib H31_3    length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B_3   length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A_3   length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B_3   length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C_3   length=4 label="Fever/cough: govt health po-NA";
  attrib H32D_3   length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E_3   length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F_3   length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G_3   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H_3   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I_3   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J_3   length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K_3   length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L_3   length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M_3   length=4 label="Fever/cough: private mobile-NA";
  attrib H32N_3   length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O_3   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P_3   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q_3   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R_3   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S_3   length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T_3   length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U_3   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V_3   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W_3   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X_3   length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y_3   length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z_3   length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HIDX_4   length=4 label="Index to birth history";
  attrib H1_4     length=4 format=H1_. label="Has health card";
  attrib H2_4     length=4 format=H2_. label="Received BCG";
  attrib H2D_4    length=4 format=H2D_. label="BCG day";
  attrib H2M_4    length=4 format=H2M_. label="BCG month";
  attrib H2Y_4    length=4 format=H2Y_. label="BCG year";
  attrib H3_4     length=4 format=H3_. label="Received DPT 1";
  attrib H3D_4    length=4 format=H3D_. label="DPT 1 day";
  attrib H3M_4    length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y_4    length=4 format=H3Y_. label="DPT 1 year";
  attrib H4_4     length=4 format=H4_. label="Received POLIO 1";
  attrib H4D_4    length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M_4    length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y_4    length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5_4     length=4 format=H5_. label="Received DPT 2";
  attrib H5D_4    length=4 format=H5D_. label="DPT 2 day";
  attrib H5M_4    length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y_4    length=4 format=H5Y_. label="DPT 2 year";
  attrib H6_4     length=4 format=H6_. label="Received POLIO 2";
  attrib H6D_4    length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M_4    length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y_4    length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7_4     length=4 format=H7_. label="Received DPT 3";
  attrib H7D_4    length=4 format=H7D_. label="DPT 3 day";
  attrib H7M_4    length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y_4    length=4 format=H7Y_. label="DPT 3 year";
  attrib H8_4     length=4 format=H8_. label="Received POLIO 3";
  attrib H8D_4    length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M_4    length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y_4    length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9_4     length=4 format=H9_. label="Received MEASLES";
  attrib H9D_4    length=4 format=H9D_. label="MEASLES day";
  attrib H9M_4    length=4 format=H9M_. label="MEASLES month";
  attrib H9Y_4    length=4 format=H9Y_. label="MEASLES year";
  attrib H0_4     length=4 label="Polio 0 -NA";
  attrib H0D_4    length=4 format=H0D_. label="Polio 0 day";
  attrib H0M_4    length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y_4    length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10_4    length=4 format=H10_. label="Ever had vaccination";
  attrib H11_4    length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B_4   length=4 format=H11B_. label="Blood in the stools";
  attrib H11C_4   length=4 label="Bowel movements in worst da-NA";
  attrib H12A_4   length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B_4   length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C_4   length=4 label="Diarrhea: govt health post -NA";
  attrib H12D_4   length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E_4   length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F_4   length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G_4   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H_4   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I_4   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J_4   length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K_4   length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L_4   length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M_4   length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N_4   length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O_4   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P_4   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q_4   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R_4   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S_4   length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T_4   length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U_4   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V_4   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W_4   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X_4   length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y_4   length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z_4   length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13_4    length=4 format=H13_. label="Given oral rehydration";
  attrib H14_4    length=4 format=H14_. label="Given recommend. home solution";
  attrib H15_4    length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A_4   length=4 label="Given antibiotics (CS) -NA";
  attrib H15B_4   length=4 format=H15B_. label="Given an injection";
  attrib H15C_4   length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D_4   length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E_4   length=4 label="Given CS other treatment -NA";
  attrib H15F_4   length=4 label="Given CS other treatment -NA";
  attrib H15G_4   length=4 label="Given CS other treatment -NA";
  attrib H15H_4   length=4 label="Given CS other treatment -NA";
  attrib H16_4    length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18_4    length=4 label="Increase or decrease in foo-NA";
  attrib H20_4    length=4 format=H20_. label="Given other treatment";
  attrib H21A_4   length=4 format=H21A_. label="Given no treatment";
  attrib H21_4    length=4 format=H21_. label="Received any treatment";
  attrib H22_4    length=4 label="Had fever in last two weeks-NA";
  attrib H31_4    length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B_4   length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A_4   length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B_4   length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C_4   length=4 label="Fever/cough: govt health po-NA";
  attrib H32D_4   length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E_4   length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F_4   length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G_4   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H_4   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I_4   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J_4   length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K_4   length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L_4   length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M_4   length=4 label="Fever/cough: private mobile-NA";
  attrib H32N_4   length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O_4   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P_4   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q_4   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R_4   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S_4   length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T_4   length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U_4   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V_4   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W_4   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X_4   length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y_4   length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z_4   length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HIDX_5   length=4 label="Index to birth history";
  attrib H1_5     length=4 format=H1_. label="Has health card";
  attrib H2_5     length=4 format=H2_. label="Received BCG";
  attrib H2D_5    length=4 format=H2D_. label="BCG day";
  attrib H2M_5    length=4 format=H2M_. label="BCG month";
  attrib H2Y_5    length=4 format=H2Y_. label="BCG year";
  attrib H3_5     length=4 format=H3_. label="Received DPT 1";
  attrib H3D_5    length=4 format=H3D_. label="DPT 1 day";
  attrib H3M_5    length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y_5    length=4 format=H3Y_. label="DPT 1 year";
  attrib H4_5     length=4 format=H4_. label="Received POLIO 1";
  attrib H4D_5    length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M_5    length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y_5    length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5_5     length=4 format=H5_. label="Received DPT 2";
  attrib H5D_5    length=4 format=H5D_. label="DPT 2 day";
  attrib H5M_5    length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y_5    length=4 format=H5Y_. label="DPT 2 year";
  attrib H6_5     length=4 format=H6_. label="Received POLIO 2";
  attrib H6D_5    length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M_5    length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y_5    length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7_5     length=4 format=H7_. label="Received DPT 3";
  attrib H7D_5    length=4 format=H7D_. label="DPT 3 day";
  attrib H7M_5    length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y_5    length=4 format=H7Y_. label="DPT 3 year";
  attrib H8_5     length=4 format=H8_. label="Received POLIO 3";
  attrib H8D_5    length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M_5    length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y_5    length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9_5     length=4 format=H9_. label="Received MEASLES";
  attrib H9D_5    length=4 format=H9D_. label="MEASLES day";
  attrib H9M_5    length=4 format=H9M_. label="MEASLES month";
  attrib H9Y_5    length=4 format=H9Y_. label="MEASLES year";
  attrib H0_5     length=4 label="Polio 0 -NA";
  attrib H0D_5    length=4 format=H0D_. label="Polio 0 day";
  attrib H0M_5    length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y_5    length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10_5    length=4 format=H10_. label="Ever had vaccination";
  attrib H11_5    length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B_5   length=4 format=H11B_. label="Blood in the stools";
  attrib H11C_5   length=4 label="Bowel movements in worst da-NA";
  attrib H12A_5   length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B_5   length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C_5   length=4 label="Diarrhea: govt health post -NA";
  attrib H12D_5   length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E_5   length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F_5   length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G_5   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H_5   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I_5   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J_5   length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K_5   length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L_5   length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M_5   length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N_5   length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O_5   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P_5   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q_5   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R_5   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S_5   length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T_5   length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U_5   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V_5   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W_5   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X_5   length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y_5   length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z_5   length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13_5    length=4 format=H13_. label="Given oral rehydration";
  attrib H14_5    length=4 format=H14_. label="Given recommend. home solution";
  attrib H15_5    length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A_5   length=4 label="Given antibiotics (CS) -NA";
  attrib H15B_5   length=4 format=H15B_. label="Given an injection";
  attrib H15C_5   length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D_5   length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E_5   length=4 label="Given CS other treatment -NA";
  attrib H15F_5   length=4 label="Given CS other treatment -NA";
  attrib H15G_5   length=4 label="Given CS other treatment -NA";
  attrib H15H_5   length=4 label="Given CS other treatment -NA";
  attrib H16_5    length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18_5    length=4 label="Increase or decrease in foo-NA";
  attrib H20_5    length=4 format=H20_. label="Given other treatment";
  attrib H21A_5   length=4 format=H21A_. label="Given no treatment";
  attrib H21_5    length=4 format=H21_. label="Received any treatment";
  attrib H22_5    length=4 label="Had fever in last two weeks-NA";
  attrib H31_5    length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B_5   length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A_5   length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B_5   length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C_5   length=4 label="Fever/cough: govt health po-NA";
  attrib H32D_5   length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E_5   length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F_5   length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G_5   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H_5   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I_5   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J_5   length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K_5   length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L_5   length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M_5   length=4 label="Fever/cough: private mobile-NA";
  attrib H32N_5   length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O_5   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P_5   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q_5   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R_5   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S_5   length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T_5   length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U_5   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V_5   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W_5   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X_5   length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y_5   length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z_5   length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HIDX_6   length=4 label="Index to birth history";
  attrib H1_6     length=4 format=H1_. label="Has health card";
  attrib H2_6     length=4 format=H2_. label="Received BCG";
  attrib H2D_6    length=4 format=H2D_. label="BCG day";
  attrib H2M_6    length=4 format=H2M_. label="BCG month";
  attrib H2Y_6    length=4 format=H2Y_. label="BCG year";
  attrib H3_6     length=4 format=H3_. label="Received DPT 1";
  attrib H3D_6    length=4 format=H3D_. label="DPT 1 day";
  attrib H3M_6    length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y_6    length=4 format=H3Y_. label="DPT 1 year";
  attrib H4_6     length=4 format=H4_. label="Received POLIO 1";
  attrib H4D_6    length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M_6    length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y_6    length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5_6     length=4 format=H5_. label="Received DPT 2";
  attrib H5D_6    length=4 format=H5D_. label="DPT 2 day";
  attrib H5M_6    length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y_6    length=4 format=H5Y_. label="DPT 2 year";
  attrib H6_6     length=4 format=H6_. label="Received POLIO 2";
  attrib H6D_6    length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M_6    length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y_6    length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7_6     length=4 format=H7_. label="Received DPT 3";
  attrib H7D_6    length=4 format=H7D_. label="DPT 3 day";
  attrib H7M_6    length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y_6    length=4 format=H7Y_. label="DPT 3 year";
  attrib H8_6     length=4 format=H8_. label="Received POLIO 3";
  attrib H8D_6    length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M_6    length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y_6    length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9_6     length=4 format=H9_. label="Received MEASLES";
  attrib H9D_6    length=4 format=H9D_. label="MEASLES day";
  attrib H9M_6    length=4 format=H9M_. label="MEASLES month";
  attrib H9Y_6    length=4 format=H9Y_. label="MEASLES year";
  attrib H0_6     length=4 label="Polio 0 -NA";
  attrib H0D_6    length=4 format=H0D_. label="Polio 0 day";
  attrib H0M_6    length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y_6    length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10_6    length=4 format=H10_. label="Ever had vaccination";
  attrib H11_6    length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B_6   length=4 format=H11B_. label="Blood in the stools";
  attrib H11C_6   length=4 label="Bowel movements in worst da-NA";
  attrib H12A_6   length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B_6   length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C_6   length=4 label="Diarrhea: govt health post -NA";
  attrib H12D_6   length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E_6   length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F_6   length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G_6   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H_6   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I_6   length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J_6   length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K_6   length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L_6   length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M_6   length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N_6   length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O_6   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P_6   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q_6   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R_6   length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S_6   length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T_6   length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U_6   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V_6   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W_6   length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X_6   length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y_6   length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z_6   length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13_6    length=4 format=H13_. label="Given oral rehydration";
  attrib H14_6    length=4 format=H14_. label="Given recommend. home solution";
  attrib H15_6    length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A_6   length=4 label="Given antibiotics (CS) -NA";
  attrib H15B_6   length=4 format=H15B_. label="Given an injection";
  attrib H15C_6   length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D_6   length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E_6   length=4 label="Given CS other treatment -NA";
  attrib H15F_6   length=4 label="Given CS other treatment -NA";
  attrib H15G_6   length=4 label="Given CS other treatment -NA";
  attrib H15H_6   length=4 label="Given CS other treatment -NA";
  attrib H16_6    length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18_6    length=4 label="Increase or decrease in foo-NA";
  attrib H20_6    length=4 format=H20_. label="Given other treatment";
  attrib H21A_6   length=4 format=H21A_. label="Given no treatment";
  attrib H21_6    length=4 format=H21_. label="Received any treatment";
  attrib H22_6    length=4 label="Had fever in last two weeks-NA";
  attrib H31_6    length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B_6   length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A_6   length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B_6   length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C_6   length=4 label="Fever/cough: govt health po-NA";
  attrib H32D_6   length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E_6   length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F_6   length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G_6   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H_6   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I_6   length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J_6   length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K_6   length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L_6   length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M_6   length=4 label="Fever/cough: private mobile-NA";
  attrib H32N_6   length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O_6   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P_6   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q_6   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R_6   length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S_6   length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T_6   length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U_6   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V_6   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W_6   length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X_6   length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y_6   length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z_6   length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HWIDX_1  length=4 label="Index to birth history - NA";
  attrib HW1_1    length=4 label="Age in months - NA";
  attrib HW2_1    length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3_1    length=4 label="Height in centimeters (1 - NA";
  attrib HW4_1    length=4 label="Ht/A Percentile - NA";
  attrib HW5_1    length=4 label="Ht/A Standard deviations - NA";
  attrib HW6_1    length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7_1    length=4 label="Wt/A Percentile - NA";
  attrib HW8_1    length=4 label="Wt/A Standard deviations - NA";
  attrib HW9_1    length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10_1   length=4 label="Wt/Ht Percentile - NA";
  attrib HW11_1   length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12_1   length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13_1   length=4 label="Reason not measured";
  attrib HW14_1   length=4 label="BCG scar on arm or should - NA";
  attrib HW15_1   length=4 label="Height: lying or standing - NA";
  attrib HW16_1   length=4 label="Day of birth of child - NA";
  attrib HW17_1   length=4 label="Date measured (day) - NA";
  attrib HW18_1   length=4 label="Date measured (month) - NA";
  attrib HW19_1   length=4 label="Date measured (year) - NA";
  attrib HW20_1   length=4 label="Arm circumference (cms - - NA";
  attrib HW21_1   length=4 label="Arm circ/A Percentile - NA";
  attrib HW22_1   length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23_1   length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24_1   length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25_1   length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26_1   length=4 label="Arm cir/Ht Percent ref. m - NA";
  attrib HWIDX_2  length=4 label="Index to birth history - NA";
  attrib HW1_2    length=4 label="Age in months - NA";
  attrib HW2_2    length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3_2    length=4 label="Height in centimeters (1 - NA";
  attrib HW4_2    length=4 label="Ht/A Percentile - NA";
  attrib HW5_2    length=4 label="Ht/A Standard deviations - NA";
  attrib HW6_2    length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7_2    length=4 label="Wt/A Percentile - NA";
  attrib HW8_2    length=4 label="Wt/A Standard deviations - NA";
  attrib HW9_2    length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10_2   length=4 label="Wt/Ht Percentile - NA";
  attrib HW11_2   length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12_2   length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13_2   length=4 label="Reason not measured";
  attrib HW14_2   length=4 label="BCG scar on arm or should - NA";
  attrib HW15_2   length=4 label="Height: lying or standing - NA";
  attrib HW16_2   length=4 label="Day of birth of child - NA";
  attrib HW17_2   length=4 label="Date measured (day) - NA";
  attrib HW18_2   length=4 label="Date measured (month) - NA";
  attrib HW19_2   length=4 label="Date measured (year) - NA";
  attrib HW20_2   length=4 label="Arm circumference (cms - - NA";
  attrib HW21_2   length=4 label="Arm circ/A Percentile - NA";
  attrib HW22_2   length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23_2   length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24_2   length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25_2   length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26_2   length=4 label="Arm cir/Ht Percent ref. m - NA";
  attrib HWIDX_3  length=4 label="Index to birth history - NA";
  attrib HW1_3    length=4 label="Age in months - NA";
  attrib HW2_3    length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3_3    length=4 label="Height in centimeters (1 - NA";
  attrib HW4_3    length=4 label="Ht/A Percentile - NA";
  attrib HW5_3    length=4 label="Ht/A Standard deviations - NA";
  attrib HW6_3    length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7_3    length=4 label="Wt/A Percentile - NA";
  attrib HW8_3    length=4 label="Wt/A Standard deviations - NA";
  attrib HW9_3    length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10_3   length=4 label="Wt/Ht Percentile - NA";
  attrib HW11_3   length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12_3   length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13_3   length=4 label="Reason not measured";
  attrib HW14_3   length=4 label="BCG scar on arm or should - NA";
  attrib HW15_3   length=4 label="Height: lying or standing - NA";
  attrib HW16_3   length=4 label="Day of birth of child - NA";
  attrib HW17_3   length=4 label="Date measured (day) - NA";
  attrib HW18_3   length=4 label="Date measured (month) - NA";
  attrib HW19_3   length=4 label="Date measured (year) - NA";
  attrib HW20_3   length=4 label="Arm circumference (cms - - NA";
  attrib HW21_3   length=4 label="Arm circ/A Percentile - NA";
  attrib HW22_3   length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23_3   length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24_3   length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25_3   length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26_3   length=4 label="Arm cir/Ht Percent ref. m - NA";
  attrib HWIDX_4  length=4 label="Index to birth history - NA";
  attrib HW1_4    length=4 label="Age in months - NA";
  attrib HW2_4    length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3_4    length=4 label="Height in centimeters (1 - NA";
  attrib HW4_4    length=4 label="Ht/A Percentile - NA";
  attrib HW5_4    length=4 label="Ht/A Standard deviations - NA";
  attrib HW6_4    length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7_4    length=4 label="Wt/A Percentile - NA";
  attrib HW8_4    length=4 label="Wt/A Standard deviations - NA";
  attrib HW9_4    length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10_4   length=4 label="Wt/Ht Percentile - NA";
  attrib HW11_4   length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12_4   length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13_4   length=4 label="Reason not measured";
  attrib HW14_4   length=4 label="BCG scar on arm or should - NA";
  attrib HW15_4   length=4 label="Height: lying or standing - NA";
  attrib HW16_4   length=4 label="Day of birth of child - NA";
  attrib HW17_4   length=4 label="Date measured (day) - NA";
  attrib HW18_4   length=4 label="Date measured (month) - NA";
  attrib HW19_4   length=4 label="Date measured (year) - NA";
  attrib HW20_4   length=4 label="Arm circumference (cms - - NA";
  attrib HW21_4   length=4 label="Arm circ/A Percentile - NA";
  attrib HW22_4   length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23_4   length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24_4   length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25_4   length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26_4   length=4 label="Arm cir/Ht Percent ref. m - NA";
  attrib HWIDX_5  length=4 label="Index to birth history - NA";
  attrib HW1_5    length=4 label="Age in months - NA";
  attrib HW2_5    length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3_5    length=4 label="Height in centimeters (1 - NA";
  attrib HW4_5    length=4 label="Ht/A Percentile - NA";
  attrib HW5_5    length=4 label="Ht/A Standard deviations - NA";
  attrib HW6_5    length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7_5    length=4 label="Wt/A Percentile - NA";
  attrib HW8_5    length=4 label="Wt/A Standard deviations - NA";
  attrib HW9_5    length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10_5   length=4 label="Wt/Ht Percentile - NA";
  attrib HW11_5   length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12_5   length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13_5   length=4 label="Reason not measured";
  attrib HW14_5   length=4 label="BCG scar on arm or should - NA";
  attrib HW15_5   length=4 label="Height: lying or standing - NA";
  attrib HW16_5   length=4 label="Day of birth of child - NA";
  attrib HW17_5   length=4 label="Date measured (day) - NA";
  attrib HW18_5   length=4 label="Date measured (month) - NA";
  attrib HW19_5   length=4 label="Date measured (year) - NA";
  attrib HW20_5   length=4 label="Arm circumference (cms - - NA";
  attrib HW21_5   length=4 label="Arm circ/A Percentile - NA";
  attrib HW22_5   length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23_5   length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24_5   length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25_5   length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26_5   length=4 label="Arm cir/Ht Percent ref. m - NA";
  attrib HWIDX_6  length=4 label="Index to birth history - NA";
  attrib HW1_6    length=4 label="Age in months - NA";
  attrib HW2_6    length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3_6    length=4 label="Height in centimeters (1 - NA";
  attrib HW4_6    length=4 label="Ht/A Percentile - NA";
  attrib HW5_6    length=4 label="Ht/A Standard deviations - NA";
  attrib HW6_6    length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7_6    length=4 label="Wt/A Percentile - NA";
  attrib HW8_6    length=4 label="Wt/A Standard deviations - NA";
  attrib HW9_6    length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10_6   length=4 label="Wt/Ht Percentile - NA";
  attrib HW11_6   length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12_6   length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13_6   length=4 label="Reason not measured";
  attrib HW14_6   length=4 label="BCG scar on arm or should - NA";
  attrib HW15_6   length=4 label="Height: lying or standing - NA";
  attrib HW16_6   length=4 label="Day of birth of child - NA";
  attrib HW17_6   length=4 label="Date measured (day) - NA";
  attrib HW18_6   length=4 label="Date measured (month) - NA";
  attrib HW19_6   length=4 label="Date measured (year) - NA";
  attrib HW20_6   length=4 label="Arm circumference (cms - - NA";
  attrib HW21_6   length=4 label="Arm circ/A Percentile - NA";
  attrib HW22_6   length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23_6   length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24_6   length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25_6   length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26_6   length=4 label="Arm cir/Ht Percent ref. m - NA";
  attrib V501     length=4 format=V501_. label="Current marital status";
  attrib V502     length=4 format=V502_. label="Currently/formerly/never marr.";
  attrib V503     length=4 format=V503_. label="Number of unions";
  attrib V504     length=4 format=V504_. label="Husband lives in house";
  attrib V505     length=4 label="Number of other wives - NA";
  attrib V506     length=4 label="Wife rank number - NA";
  attrib V507     length=4 label="Month of first marriage";
  attrib V508     length=4 label="Year of first marriage";
  attrib V509     length=4 label="Date of first marriage (CMC)";
  attrib V510     length=4 format=V510_. label="Completeness of date inform.";
  attrib V511     length=4 label="Age at first marriage";
  attrib V512     length=4 label="Years since first marriage";
  attrib V513     length=4 format=V513_. label="Marital duration (grouped)";
  attrib V525     length=4 label="Age at first intercourse - NA";
  attrib V527     length=4 format=V527_. label="Time since last intercourse";
  attrib V528     length=4 format=V528_. label="Time since last intercourse(g)";
  attrib V529     length=4 format=V529_. label="Time since last intercourse(im";
  attrib V530     length=4 format=V530_. label="Flag for V529";
  attrib V531     length=4 label="Age at first intercourse - NA";
  attrib V532     length=4 label="Flag for V531 -NA";
  attrib V534     length=4 label="Have a regular sex partne - NA";
  attrib V535     length=4 format=V535_. label="Have ever been married";
  attrib V602     length=4 format=V602_. label="Fertility preference";
  attrib V603     length=4 format=V603_. label="Preferred waiting time";
  attrib V604     length=4 format=V604_. label="Preferred waiting time (grpd)";
  attrib V605     length=4 format=V605_. label="Desire for more children";
  attrib V606     length=4 format=V606_. label="Attitude toward becoming preg.";
  attrib V610     length=4 format=V610_. label="Husband approves FP";
  attrib V611     length=4 format=V611_. label="Discussed FP with partner";
  attrib V612     length=4 format=V612_. label="Respondent approves FP";
  attrib V613     length=4 format=V613_. label="Ideal number of children";
  attrib V614     length=4 format=V614_. label="Ideal number of children (grp)";
  attrib V616     length=4 format=V616_. label="Time for future birth";
  attrib V618     length=4 format=V618_. label="Reason regrets sterilization";
  attrib V621     length=4 format=V621_. label="Husbands desire for children";
  attrib V623     length=4 format=V623_. label="Exposure";
  attrib V624     length=4 format=V624_. label="Unmet need";
  attrib V625     length=4 format=V625_. label="Exposure (definition 2)";
  attrib V626     length=4 format=V626_. label="Unmet need (definition 2)";
  attrib V627     length=4 format=V627_. label="Ideal number of boys";
  attrib V628     length=4 format=V628_. label="Ideal number of girls";
  attrib V629     length=4 format=V629_. label="Ideal number of either sex";
  attrib V630A    length=4 label="Discussed FP with partner - NA";
  attrib V630B    length=4 label="Discussed FP with mother - NA";
  attrib V630C    length=4 label="Discussed FP with father - NA";
  attrib V630D    length=4 label="Discussed FP with sister( - NA";
  attrib V630E    length=4 label="Discussed FP with brother - NA";
  attrib V630F    length=4 label="Discussed FP with daughte - NA";
  attrib V630G    length=4 label="Discussed FP with mother- - NA";
  attrib V630H    length=4 label="Discussed FP with friends - NA";
  attrib V630I    length=4 label="Discussed FP -country spe - NA";
  attrib V630J    length=4 label="Discussed FP -country spe - NA";
  attrib V630K    length=4 label="Discussed FP -country spe - NA";
  attrib V630L    length=4 label="Discussed FP -country spe - NA";
  attrib V630X    length=4 label="Discussed FP with others - NA";
  attrib V701     length=4 format=V701_. label="Partner's education level";
  attrib V702     length=4 format=V702_. label="Highest year of education";
  attrib V704     length=4 format=V704_. label="Partner's occupation";
  attrib V705     length=4 format=V705_. label="Partner's occupation";
  attrib V707     length=4 format=V707_. label="Type of land where works";
  attrib V714     length=4 format=V714_. label="Respondent currently working";
  attrib V715     length=4 format=V715_. label="Husbands education-single yrs";
  attrib V716     length=4 format=V716_. label="Respondent's occupation";
  attrib V717     length=4 format=V717_. label="Respondent's occupation";
  attrib V718     length=4 format=V718_. label="Current type of employment";
  attrib V719     length=4 format=V719_. label="Work for family, others, self";
  attrib V720     length=4 format=V720_. label="Earns cash for work";
  attrib V721     length=4 format=V721_. label="Works at home or away";
  attrib V723     length=4 format=V723_. label="Child minder";
  attrib V729     length=4 format=V729_. label="Partner's educational attainm.";
  attrib V730     length=4 label="Partners age -NA";
  attrib V731     length=4 label="Worked in last 12 months - NA";
  attrib V732     length=4 format=V732_. label="Employment all year/seasonal";
  attrib V733     length=4 label="Months worked in last 12 - NA";
  attrib V734     length=4 label="Days in a week normally w - NA";
  attrib V735     length=4 label="Days worked in last 12 mo - NA";
  attrib V736     length=8 label="Earnings for work - NA";
  attrib V737     length=8 label="Daily earnings for work - NA";
  attrib V738     length=8 label="Yearly earnings for work - NA";
  attrib V739     length=4 format=V739_. label="Who decides how to spend money";
  attrib V740     length=4 format=V740_. label="Type of land where resp. works";
  attrib V751     length=4 label="Ever heard of AIDS -NA";
  attrib V752A    length=4 label="AIDS: radio - NA";
  attrib V752B    length=4 label="AIDS: television - NA";
  attrib V752C    length=4 label="AIDS: newspapers/magazine - NA";
  attrib V752D    length=4 label="AIDS: pamphlets/posters - NA";
  attrib V752E    length=4 label="AIDS: clinic/health worke - NA";
  attrib V752F    length=4 label="AIDS: churches/mosques - NA";
  attrib V752G    length=4 label="AIDS: schools/teachers - NA";
  attrib V752H    length=4 label="AIDS: community meetings - NA";
  attrib V752I    length=4 label="AIDS: friends/relatives - NA";
  attrib V752J    length=4 label="AIDS: work place - NA";
  attrib V752K    length=4 label="AIDS: country specific - NA";
  attrib V752L    length=4 label="AIDS: country specific - NA";
  attrib V752M    length=4 label="AIDS: country specific - NA";
  attrib V752N    length=4 label="AIDS: country specific - NA";
  attrib V752O    length=4 label="AIDS: country specific - NA";
  attrib V752X    length=4 label="AIDS: other responses - NA";
  attrib V753     length=4 label="Ways to avoid AIDS - NA";
  attrib V754A    length=4 label="AIDS: Safe sex - NA";
  attrib V754B    length=4 label="AIDS: abstain from sex - NA";
  attrib V754C    length=4 label="AIDS: use condoms during - NA";
  attrib V754D    length=4 label="AIDS: only one sex partne - NA";
  attrib V754E    length=4 label="AIDS: avoid sex prostitut - NA";
  attrib V754F    length=4 label="AIDS: avoid sex homosexua - NA";
  attrib V754G    length=4 label="AIDS: avoid blood transfu - NA";
  attrib V754H    length=4 label="AIDS: avoid injections - NA";
  attrib V754I    length=4 label="AIDS: avoid kissing - NA";
  attrib V754J    length=4 label="AIDS: avoid mosquito bite - NA";
  attrib V754K    length=4 label="AIDS: protection trad. he - NA";
  attrib V754L    length=4 label="AIDS: country specific - NA";
  attrib V754M    length=4 label="AIDS: country specific - NA";
  attrib V754N    length=4 label="AIDS: country specific - NA";
  attrib V754O    length=4 label="AIDS: country specific - NA";
  attrib V754X    length=4 label="AIDS: other responses - NA";
  attrib V754Z    length=4 label="AIDS: DK ways to avoid AI - NA";
  attrib V755B    length=4 label="Safe sex: abstain from se - NA";
  attrib V755C    length=4 label="Safe sex: use condoms dur - NA";
  attrib V755D    length=4 label="Safe sex: only one partne - NA";
  attrib V755E    length=4 label="Safe sex: avoid prostitut - NA";
  attrib V755F    length=4 label="Safe sex: avoid homosexua - NA";
  attrib V755L    length=4 label="Safe sex: country specifi - NA";
  attrib V755M    length=4 label="Safe sex: country specifi - NA";
  attrib V755N    length=4 label="Safe sex: country specifi - NA";
  attrib V755O    length=4 label="Safe sex: country specifi - NA";
  attrib V755X    length=4 label="Safe sex: other responses - NA";
  attrib V755Z    length=4 label="Safe sex: DK meaning - NA";
  attrib V756     length=4 label="Can a healthy person have - NA";
  attrib V757     length=4 label="Is AIDS a fatal disease - NA";
  attrib V758     length=4 label="Chances of getting AIDS - NA";
  attrib V760A    length=4 label="Behavior: Did not start s - NA";
  attrib V760B    length=4 label="Behavior: Stopped all sex - NA";
  attrib V760C    length=4 label="Behavior: Started using c - NA";
  attrib V760D    length=4 label="Behavior: Only one sex pa - NA";
  attrib V760E    length=4 label="Behavior: Reduced partner - NA";
  attrib V760F    length=4 label="Behavior: Ask spouse fait - NA";
  attrib V760G    length=4 label="Behavior: No homosexual c - NA";
  attrib V760I    length=4 label="Behavior: Stopped injecti - NA";
  attrib V760L    length=4 label="Behavior: Country specifi - NA";
  attrib V760M    length=4 label="Behavior: Country specifi - NA";
  attrib V760N    length=4 label="Behavior: Country specifi - NA";
  attrib V760O    length=4 label="Behavior: Country specifi - NA";
  attrib V760P    length=4 label="Behavior: Spouse avoid pro- NA";
  attrib V760V    length=4 label="Behavior: No nonsexual ch - NA";
  attrib V760W    length=4 label="Behavior: Other nonsexual - NA";
  attrib V760X    length=4 label="Behavior: Other responses - NA";
  attrib V760Y    length=4 label="Behavior: No change - NA";
  attrib V760Z    length=4 label="Behavior: Don't know - NA";
  attrib V761     length=4 label="Last intercourse used con - NA";
  attrib V762     length=4 label="Source for condoms -NA";
  attrib V764     length=4 label="Knowledge of condom -NA";
  attrib V765     length=4 label="Ever use of condom -NA";
  attrib V801     length=4 label="Time interview started";
  attrib V802     length=4 label="Time interview ended";
  attrib V803     length=4 format=V803_. label="Length of interview in minutes";
  attrib V804     length=4 label="Number of visits";
  attrib V805     length=4 label="Interviewer identification";
  attrib V806     length=4 label="Keyer identification";
  attrib V811     length=4 format=V811_. label="Pres. children < 10 (sec 5)";
  attrib V812     length=4 format=V812_. label="Pres. husband (sec 5)";
  attrib V813     length=4 format=V813_. label="Pres. other males (sec 5)";
  attrib V814     length=4 format=V814_. label="Pres. other females (sec 5)";
  attrib VCOL_1   length=4 format=VCOL_. label="Calendar column number";
  attrib VCAL_1   length=$80 label="Calendar";
  attrib VCOL_2   length=4 format=VCOL_. label="Calendar column number";
  attrib VCAL_2   length=$80 label="Calendar";
  attrib VCOL_3   length=4 format=VCOL_. label="Calendar column number";
  attrib VCAL_3   length=$80 label="Calendar";
  attrib VCOL_4   length=4 format=VCOL_. label="Calendar column number";
  attrib VCAL_4   length=$80 label="Calendar";
  attrib VCOL_5   length=4 format=VCOL_. label="Calendar column number";
  attrib VCAL_5   length=$80 label="Calendar";
  attrib SDFORMAT length=4 format=SDFORMA_. label="Date format of questionnaire";
  attrib SADJUST  length=4 format=SADJUST_. label="Month of interview adjusted 1m";
  attrib SINTM    length=4 label="Month of interview(unadjusted)";
  attrib SINTY    length=4 label="Year of interview(unadjusted)";
  attrib SDIV     length=4 format=SDIV_. label="Division";
  attrib SDISTR   length=4 label="District of residence";
  attrib S114A    length=4 format=S114A_. label="Grameen Bank";
  attrib S114B    length=4 format=S114B_. label="BRAC";
  attrib S114C    length=4 format=S114C_. label="BRDP";
  attrib S114D    length=4 format=S114D_. label="Mothers club";
  attrib S114E    length=4 format=S114E_. label="Other";
  attrib S119     length=4 format=S119_. label="Time to get to water (mins)";
  attrib S119A    length=4 label="Time to wait to get water:mins";
  attrib S123     length=4 format=S123_. label="Toilet facility : children";
  attrib S124A    length=4 format=S124A_. label="Almirah";
  attrib S124B    length=4 format=S124B_. label="Table/chair/bench";
  attrib S124C    length=4 format=S124C_. label="Watch/clock";
  attrib S124D    length=4 format=S124D_. label="Cot/bed";
  attrib S124I    length=4 format=S124I_. label="Agricultural land";
  attrib S229M    length=4 format=S229M_. label="Month pregnancy ended";
  attrib S229Y    length=4 format=S229Y_. label="Year pregnancy ended";
  attrib S233A    length=4 format=S233A_. label="Ever had unwanted pregnancy";
  attrib S233B    length=4 format=S233B_. label="Pregancy aborted/menst.reg'ltn";
  attrib S233C    length=4 format=S233C_. label="Pregnancy ended by MR/D&C";
  attrib S233DM   length=4 label="When pr. ended : Month";
  attrib S233DY   length=4 format=S233DY_. label="When pregnancy ended :Year";
  attrib S304_01  length=4 format=S304_. label="Knows source for method";
  attrib S304_02  length=4 format=S304_. label="Knows source for method";
  attrib S304_03  length=4 format=S304_. label="Knows source for method";
  attrib S304_04  length=4 format=S304_. label="Knows source for method";
  attrib S304_05  length=4 format=S304_. label="Knows source for method";
  attrib S304_06  length=4 format=S304_. label="Knows source for method";
  attrib S304_07  length=4 format=S304_. label="Knows source for method";
  attrib S304_08  length=4 format=S304_. label="Knows source for method";
  attrib S304_09  length=4 format=S304_. label="Knows source for method";
  attrib S304_10  length=4 format=S304_. label="Knows source for method";
  attrib S304_11  length=4 format=S304_. label="Knows source for method";
  attrib S304_12  length=4 format=S304_. label="Knows source for method";
  attrib S304_13  length=4 format=S304_. label="Knows source for method";
  attrib S304_14  length=4 format=S304_. label="Knows source for method";
  attrib S304_15  length=4 format=S304_. label="Knows source for method";
  attrib S309     length=4 format=S309_. label="First contraceptive method";
  attrib S310     length=4 format=S310_. label="Source of first method";
  attrib S316     length=4 format=S316_. label="Currently using second method";
  attrib S317     length=4 format=S317_. label="Second method currently used";
  attrib S318B    length=4 format=S318B_. label="Pill usage";
  attrib S318C    length=4 format=S318C_. label="Why not taken pills in order";
  attrib S318EA   length=4 format=S318EA_. label="No pill pack: Ran out";
  attrib S318EB   length=4 format=S318EB_. label="No pill pack: Cost too much";
  attrib S318EC   length=4 format=S318EC_. label="No pill pack: Husband away";
  attrib S318ED   length=4 format=S318ED_. label="No pill pack: Has menstrual pd";
  attrib S318EE   length=4 format=S318EE_. label="No pill pack:none at her s'rce";
  attrib S318EF   length=4 format=S318EF_. label="No pill pack:FWA didn't supply";
  attrib S318EG   length=4 format=S318EG_. label="No pill pack: Other reason";
  attrib S318F    length=4 format=S318F_. label="Days since last took a pill";
  attrib S318HA   length=4 format=S318HA_. label="No curr.pill use:husband away";
  attrib S318HB   length=4 format=S318HB_. label="No curr.pill use:forgot";
  attrib S318HC   length=4 format=S318HC_. label="No curr.pill use:health reason";
  attrib S318HD   length=4 format=S318HD_. label="No curr.pill use:cost too much";
  attrib S318HE   length=4 format=S318HE_. label="No curr.pill use:daily use N/R";
  attrib S318HF   length=4 format=S318HF_. label="No curr.pill use:ran out";
  attrib S318HG   length=4 format=S318HG_. label="No curr.pill use:FWA not resup";
  attrib S318HH   length=4 format=S318HH_. label="No curr.pill use:menstruating";
  attrib S318HI   length=4 format=S318HI_. label="No curr.pill use:other reason";
  attrib S318I    length=4 format=S318I_. label="Delays use between pill pk'age";
  attrib S318K    length=4 format=S318K_. label="Action after forgetting pill";
  attrib S319     length=4 label="Months since last injection";
  attrib S319B    length=4 format=S319B_. label="Reason why no recent injection";
  attrib S320S    length=4 format=S320S_. label="Condom package seen";
  attrib S320     length=4 format=S320_. label="Brand of condoms used";
  attrib S320A    length=4 format=S320A_. label="Reason can't show condom pack";
  attrib S320C    length=4 format=S320C_. label="Use condom every time have sex";
  attrib S322C    length=4 format=S322C_. label="Which days of cycle not safe";
  attrib S320D    length=4 label="Times used condoms last month";
  attrib S322D    length=4 format=S322D_. label="Method used most recently";
  attrib S323B    length=4 format=S323B_. label="Paid for use of contr.source";
  attrib S325     length=4 format=S325_. label="Who obtained pills/condoms";
  attrib S325A    length=4 format=S325A_. label="Got pls,cndm at HF,doctor,shop";
  attrib S325B    length=4 format=S325B_. label="Informed of side effects/probs";
  attrib S325C    length=4 format=S325C_. label="Informed of other methods";
  attrib S325E    length=4 format=S325E_. label="Inf. of side effs by FWA(ever)";
  attrib S325G    length=4 format=S325G_. label="Inf. of oth.meths by FWA(ever)";
  attrib S325H    length=4 format=S325H_. label="Got method she wanted";
  attrib S325I    length=4 format=S325I_. label="Method wanted at hlth.facility";
  attrib S325J    length=4 format=S325J_. label="Who had most inflc. at 1st use";
  attrib S326     length=4 format=S326_. label="Reason for current method";
  attrib S327     length=4 format=S327_. label="Problems with current method";
  attrib S328PA   length=4 format=S328PA_. label="Probs,curr.meth: weight gain";
  attrib S328PB   length=4 format=S328PB_. label="Probs,curr.meth: weight loss";
  attrib S328PC   length=4 format=S328PC_. label="Probs,curr.meth: excess bleed.";
  attrib S328PD   length=4 format=S328PD_. label="Probs,curr.meth: hypertension";
  attrib S328PE   length=4 format=S328PE_. label="Probs,curr.meth: headache";
  attrib S328PF   length=4 format=S328PF_. label="Probs,curr.meth: nausea";
  attrib S328PG   length=4 format=S328PG_. label="Probs,curr.meth: no menstruatn";
  attrib S328PH   length=4 format=S328PH_. label="Probs,curr.meth: weak/tired";
  attrib S328PI   length=4 format=S328PI_. label="Probs,curr.meth: dizziness";
  attrib S328PJ   length=4 format=S328PJ_. label="Probs,curr.meth: other";
  attrib S328PK   length=4 format=S328PK_. label="Probs,curr.meth: does not know";
  attrib S328A    length=4 format=S328A_. label="Advice when 1st had problems";
  attrib S328B    length=4 format=S328B_. label="Who gave advice when 1st probs";
  attrib S328C    length=4 format=S328C_. label="Any other probs. using meth.";
  attrib S328DA   length=4 format=S328DA_. label="Other probs: husb.disapproves";
  attrib S328DB   length=4 format=S328DB_. label="Other probs: Oth.rel. disappr.";
  attrib S328DC   length=4 format=S328DC_. label="Other probs: relig.disapproves";
  attrib S328DD   length=4 format=S328DD_. label="Other probs: access/availab'ty";
  attrib S328DE   length=4 format=S328DE_. label="Other probs: cost too much";
  attrib S328DF   length=4 format=S328DF_. label="Other probs: inconvenient use";
  attrib S328DG   length=4 format=S328DG_. label="Other probs : ster.,wants ch.";
  attrib S328DH   length=4 format=S328DH_. label="Other probs: Other";
  attrib S328DI   length=4 format=S328DI_. label="Other probs: does not know";
  attrib S347A    length=4 format=S347A_. label="Satellite clinic last 3 months";
  attrib S347B    length=4 format=S347B_. label="Visited satellite clinic";
  attrib S347CA   length=4 format=S347CA_. label="Sat.Clinic: Family Plng.Meths.";
  attrib S347CB   length=4 format=S347CB_. label="Sat.Clinic: Immunization";
  attrib S347CC   length=4 format=S347CC_. label="Sat.Clinic: Ch.growth monitor.";
  attrib S347CD   length=4 format=S347CD_. label="Sat.Clinic: Other";
  attrib S347CE   length=4 format=S347CE_. label="Sat.Clinic: Does not know";
  attrib S348C    length=4 format=S348C_. label="Read about FP on Billboard";
  attrib S349A    length=4 format=S349A_. label="Inf. on radio acceptable:pill";
  attrib S349B    length=4 format=S349B_. label="Inf. on radio acct'ble:condoms";
  attrib S349C    length=4 format=S349C_. label="Inf. on radio acct'ble:injctns";
  attrib S349D    length=4 format=S349D_. label="Inf. on radio acct'ble:injctns";
  attrib S349E    length=4 format=S349E_. label="Inf. on radio acct'ble:sterlzn";
  attrib S352     length=4 format=S352_. label="No. visits by FPW last 6 mnths";
  attrib S353     length=4 format=S353_. label="Months since last visit by FPW";
  attrib S354     length=4 format=S354_. label="Received supplies from FPW";
  attrib S355     length=4 format=S355_. label="Supplies received from FPW";
  attrib S356     length=4 label="How many cycles/condoms rec'vd";
  attrib S357MA   length=4 format=S357MA_. label="Meths.disc.w FPW: Pills";
  attrib S357MB   length=4 format=S357MB_. label="Meths.disc.w FPW: IUD";
  attrib S357MC   length=4 format=S357MC_. label="Meths.disc.w FPW: Injection";
  attrib S357MD   length=4 format=S357MD_. label="Meths.disc.w FPW: Condom";
  attrib S357ME   length=4 format=S357ME_. label="Meths.disc.w FPW: Female Ster.";
  attrib S357MF   length=4 format=S357MF_. label="Meths.disc.w FPW: Male Ster.";
  attrib S357MG   length=4 format=S357MG_. label="Meths.disc.w FPW: Never disc.";
  attrib S357A    length=4 format=S357A_. label="Referred to clinic by FPW";
  attrib S357B    length=4 format=S357B_. label="Why FPW referred to clinic";
  attrib S358     length=4 format=S358_. label="Most women known by rsp use FP";
  attrib S359     length=4 format=S359_. label="Has recommended FP to someone";
  attrib S361     length=4 format=S361_. label="Spoke with psn at HF about FP";
  attrib S439     length=4 format=S439_. label="Rec.advice on duration of bfdg";
  attrib S440     length=4 format=S440_. label="Dec dur'tn BF:who helped most";
  attrib S441     length=4 format=S441_. label="No.months advised for BF";
  attrib S442A    length=4 format=S442A_. label="Anyone give FP adv.after birth";
  attrib S442B_1  length=4 format=S442B_. label="Who gave specific advice on FP";
  attrib S442C_1  length=4 format=S442C_. label="Asked how planned to feed baby";
  attrib S442DA_1 length=4 format=S442DA_. label="Methods discussed: Pill";
  attrib S442DB_1 length=4 format=S442DB_. label="Methods discussed: IUD";
  attrib S442DC_1 length=4 format=S442DC_. label="Methods discussed: Injections";
  attrib S442DD_1 length=4 format=S442DD_. label="Methods discussed: Condom";
  attrib S442DE_1 length=4 format=S442DE_. label="Methods discussed: Female Ster";
  attrib S442DF_1 length=4 format=S442DF_. label="Methods discussed: Male Ster.";
  attrib S442DG_1 length=4 format=S442DG_. label="Methods discussed: Calendar";
  attrib S442DH_1 length=4 format=S442DH_. label="Methods discussed: Withdrawal.";
  attrib S442DI_1 length=4 format=S442DI_. label="Methods discussed: Prol.BrFeed";
  attrib S442DJ_1 length=4 format=S442DJ_. label="Methods discussed: Other";
  attrib S442DK_1 length=4 format=S442DK_. label="Methods discussed: None";
  attrib S442EM_1 length=4 format=S442EM1_. label="1st recommended method";
  attrib S442FM_1 length=4 format=S442FM1_. label="Rec.interval before use mthd 1";
  attrib S442EM_1 length=4 format=S442EM2_. label="2nd recommended method";
  attrib S442FM_1 length=4 format=S442FM2_. label="Rec.interval before use mthd 2";
  attrib S442B_2  length=4 format=S442B_. label="Who gave specific advice on FP";
  attrib S442C_2  length=4 format=S442C_. label="Asked how planned to feed baby";
  attrib S442DA_2 length=4 format=S442DA_. label="Methods discussed: Pill";
  attrib S442DB_2 length=4 format=S442DB_. label="Methods discussed: IUD";
  attrib S442DC_2 length=4 format=S442DC_. label="Methods discussed: Injections";
  attrib S442DD_2 length=4 format=S442DD_. label="Methods discussed: Condom";
  attrib S442DE_2 length=4 format=S442DE_. label="Methods discussed: Female Ster";
  attrib S442DF_2 length=4 format=S442DF_. label="Methods discussed: Male Ster.";
  attrib S442DG_2 length=4 format=S442DG_. label="Methods discussed: Calendar";
  attrib S442DH_2 length=4 format=S442DH_. label="Methods discussed: Withdrawal.";
  attrib S442DI_2 length=4 format=S442DI_. label="Methods discussed: Prol.BrFeed";
  attrib S442DJ_2 length=4 format=S442DJ_. label="Methods discussed: Other";
  attrib S442DK_2 length=4 format=S442DK_. label="Methods discussed: None";
  attrib S442EM_2 length=4 format=S442EM1_. label="1st recommended method";
  attrib S442FM_2 length=4 format=S442FM1_. label="Rec.interval before use mthd 1";
  attrib S442EM_2 length=4 format=S442EM2_. label="2nd recommended method";
  attrib S442FM_2 length=4 format=S442FM2_. label="Rec.interval before use mthd 2";
  attrib S442H    length=4 format=S442H_. label="Dec to use FP :who helped most";
  attrib S442IA   length=4 format=S442IA_. label="Infl.to use FP: Resumed sex";
  attrib S442IB   length=4 format=S442IB_. label="Infl.to use FP: Resumed period";
  attrib S442IC   length=4 format=S442IC_. label="Infl.to use FP: Breastfeeding";
  attrib S442ID   length=4 format=S442ID_. label="Infl.to use FP:des.to have ch.";
  attrib S442IE   length=4 format=S442IE_. label="Infl.to use FP: Radio/TV msges";
  attrib S442IF   length=4 format=S442IF_. label="Infl.to use FP: Other";
  attrib S442IG   length=4 format=S442IG_. label="Infl.to use FP: Nothing else";
  attrib S442J    length=4 format=S442J_. label="Helped mst to decide which mth";
  attrib S442KA   length=4 format=S442KA_. label="Oth.infl.on meth:Effic.method";
  attrib S442KB   length=4 format=S442KB_. label="Oth.infl.on meth: Prev.experi.";
  attrib S442KC   length=4 format=S442KC_. label="Oth.infl.on meth:Safeness meth";
  attrib S442KD   length=4 format=S442KD_. label="Oth.infl.on meth:Comfort/disc.";
  attrib S442KE   length=4 format=S442KE_. label="Oth.infl.on meth: Radio/TV msg";
  attrib S442KF   length=4 format=S442KF_. label="Oth.infl.on meth: Other";
  attrib S442L    length=4 format=S442L_. label="FP meths.not good while brstfg";
  attrib S442MA   length=4 format=S442MA_. label="Bad for Bfdg.Wom.: Pill";
  attrib S442MB   length=4 format=S442MB_. label="Bad for Bfdg.Wom.: IUD";
  attrib S442MC   length=4 format=S442MC_. label="Bad for Bfdg.Wom.: Injections";
  attrib S442MD   length=4 format=S442MD_. label="Bad for Bfdg.Wom.: Condom";
  attrib S442ME   length=4 format=S442ME_. label="Bad for Bfdg.Wom.: Female sTer";
  attrib S442MF   length=4 format=S442MF_. label="Bad for Bfdg.Wom.: Male Ster.";
  attrib S442MG   length=4 format=S442MG_. label="Bad for Bfdg.Wom.: Safe period";
  attrib S442MH   length=4 format=S442MH_. label="Bad for Bfdg.Wom.: Withdrawal";
  attrib S442MI   length=4 format=S442MI_. label="Bad for Bfdg.Wom.: Other";
  attrib S442N    length=4 format=S442N_. label="EFf. of BF/amen against prgncy";
  attrib S442O    length=4 format=S442O_. label="How long protected by b.feedin";
  attrib S609     length=4 format=S609_. label="Source of preferred method";
  attrib S613     length=4 format=S613_. label="Discussed children";
  attrib S615     length=4 format=S615_. label="Resp.allowed to go out alone";
  attrib S616     length=4 format=S616_. label="Freq.visits to oth.pts localit";
  attrib S617     length=4 format=S617_. label="Freq.of shopping/marketing";
  attrib S618     length=4 format=S618_. label="Allowed to visit H.cntr,hosp.";
Data BDIR31FL;
infile 'BDIR31FL.DAT' LRECL=4134 MISSOVER ;
    input
    @1    CASEID   $15.
    @16   V000     $3.
    @19   V001     8.0
    @27   V002     4.0
    @31   V003     3.0
    @34   V004     4.0
    @38   V005     8.0
    @46   V006     2.0
    @48   V007     2.0
    @50   V008     4.0
    @54   V009     2.0
    @56   V010     2.0
    @58   V011     4.0
    @62   V012     2.0
    @64   V013     1.0
    @65   V014     1.0
    @66   V015     1.0
    @67   V016     2.0
    @69   V017     4.0
    @73   V018     2.0
    @75   V019     2.0
    @77   V020     1.0
    @78   V021     4.0
    @82   V022     4.0
    @86   V023     2.0
    @88   V024     2.0
    @90   V025     1.0
    @91   V026     1.0
    @92   V027     1.0
    @93   V028     3.0
    @96   V029     2.0
    @98   V030     2.0
    @100  V031     2.0
    @102  V032     2.0
    @104  V033     8.0
    @112  V034     2.0
    @114  V101     2.0
    @116  V102     1.0
    @117  V103     1.0
    @118  V104     2.0
    @120  V105     1.0
    @121  V106     1.0
    @122  V107     2.0
    @124  V108     1.0
    @125  V109     1.0
    @126  V110     1.0
    @127  V111     1.0
    @128  V112     1.0
    @129  V113     2.0
    @131  V114     2.0
    @133  V115     3.0
    @136  V116     2.0
    @138  V119     1.0
    @139  V120     1.0
    @140  V121     1.0
    @141  V122     1.0
    @142  V123     1.0
    @143  V124     1.0
    @144  V125     1.0
    @145  V127     2.0
    @147  V128     2.0
    @149  V129     2.0
    @151  V130     1.0
    @152  V131     2.0
    @154  V133     2.0
    @156  V134     1.0
    @157  V135     1.0
    @158  V136     2.0
    @160  V137     2.0
    @162  V138     2.0
    @164  V139     2.0
    @166  V140     1.0
    @167  V141     1.0
    @168  V142     1.0
    @169  V144     1.0
    @170  V148     1.0
    @171  V149     1.0
    @172  V150     2.0
    @174  V151     1.0
    @175  V152     2.0
    @177  V153     1.0
    @178  V154     2.0
    @180  AWFACTT  5.0
    @185  AWFACTU  5.0
    @190  AWFACTR  5.0
    @195  AWFACTE  5.0
    @200  BIDX_01  2.0
    @202  BORD_01  2.0
    @204  B0_01    1.0
    @205  B1_01    2.0
    @207  B2_01    2.0
    @209  B3_01    4.0
    @213  B4_01    1.0
    @214  B5_01    1.0
    @215  B6_01    3.0
    @218  B7_01    3.0
    @221  B8_01    2.0
    @223  B9_01    1.0
    @224  B10_01   1.0
    @225  B11_01   3.0
    @228  B12_01   3.0
    @231  B13_01   1.0
    @232  B14_01   1.0
    @233  B15_01   1.0
    @234  BIDX_02  2.0
    @236  BORD_02  2.0
    @238  B0_02    1.0
    @239  B1_02    2.0
    @241  B2_02    2.0
    @243  B3_02    4.0
    @247  B4_02    1.0
    @248  B5_02    1.0
    @249  B6_02    3.0
    @252  B7_02    3.0
    @255  B8_02    2.0
    @257  B9_02    1.0
    @258  B10_02   1.0
    @259  B11_02   3.0
    @262  B12_02   3.0
    @265  B13_02   1.0
    @266  B14_02   1.0
    @267  B15_02   1.0
    @268  BIDX_03  2.0
    @270  BORD_03  2.0
    @272  B0_03    1.0
    @273  B1_03    2.0
    @275  B2_03    2.0
    @277  B3_03    4.0
    @281  B4_03    1.0
    @282  B5_03    1.0
    @283  B6_03    3.0
    @286  B7_03    3.0
    @289  B8_03    2.0
    @291  B9_03    1.0
    @292  B10_03   1.0
    @293  B11_03   3.0
    @296  B12_03   3.0
    @299  B13_03   1.0
    @300  B14_03   1.0
    @301  B15_03   1.0
    @302  BIDX_04  2.0
    @304  BORD_04  2.0
    @306  B0_04    1.0
    @307  B1_04    2.0
    @309  B2_04    2.0
    @311  B3_04    4.0
    @315  B4_04    1.0
    @316  B5_04    1.0
    @317  B6_04    3.0
    @320  B7_04    3.0
    @323  B8_04    2.0
    @325  B9_04    1.0
    @326  B10_04   1.0
    @327  B11_04   3.0
    @330  B12_04   3.0
    @333  B13_04   1.0
    @334  B14_04   1.0
    @335  B15_04   1.0
    @336  BIDX_05  2.0
    @338  BORD_05  2.0
    @340  B0_05    1.0
    @341  B1_05    2.0
    @343  B2_05    2.0
    @345  B3_05    4.0
    @349  B4_05    1.0
    @350  B5_05    1.0
    @351  B6_05    3.0
    @354  B7_05    3.0
    @357  B8_05    2.0
    @359  B9_05    1.0
    @360  B10_05   1.0
    @361  B11_05   3.0
    @364  B12_05   3.0
    @367  B13_05   1.0
    @368  B14_05   1.0
    @369  B15_05   1.0
    @370  BIDX_06  2.0
    @372  BORD_06  2.0
    @374  B0_06    1.0
    @375  B1_06    2.0
    @377  B2_06    2.0
    @379  B3_06    4.0
    @383  B4_06    1.0
    @384  B5_06    1.0
    @385  B6_06    3.0
    @388  B7_06    3.0
    @391  B8_06    2.0
    @393  B9_06    1.0
    @394  B10_06   1.0
    @395  B11_06   3.0
    @398  B12_06   3.0
    @401  B13_06   1.0
    @402  B14_06   1.0
    @403  B15_06   1.0
    @404  BIDX_07  2.0
    @406  BORD_07  2.0
    @408  B0_07    1.0
    @409  B1_07    2.0
    @411  B2_07    2.0
    @413  B3_07    4.0
    @417  B4_07    1.0
    @418  B5_07    1.0
    @419  B6_07    3.0
    @422  B7_07    3.0
    @425  B8_07    2.0
    @427  B9_07    1.0
    @428  B10_07   1.0
    @429  B11_07   3.0
    @432  B12_07   3.0
    @435  B13_07   1.0
    @436  B14_07   1.0
    @437  B15_07   1.0
    @438  BIDX_08  2.0
    @440  BORD_08  2.0
    @442  B0_08    1.0
    @443  B1_08    2.0
    @445  B2_08    2.0
    @447  B3_08    4.0
    @451  B4_08    1.0
    @452  B5_08    1.0
    @453  B6_08    3.0
    @456  B7_08    3.0
    @459  B8_08    2.0
    @461  B9_08    1.0
    @462  B10_08   1.0
    @463  B11_08   3.0
    @466  B12_08   3.0
    @469  B13_08   1.0
    @470  B14_08   1.0
    @471  B15_08   1.0
    @472  BIDX_09  2.0
    @474  BORD_09  2.0
    @476  B0_09    1.0
    @477  B1_09    2.0
    @479  B2_09    2.0
    @481  B3_09    4.0
    @485  B4_09    1.0
    @486  B5_09    1.0
    @487  B6_09    3.0
    @490  B7_09    3.0
    @493  B8_09    2.0
    @495  B9_09    1.0
    @496  B10_09   1.0
    @497  B11_09   3.0
    @500  B12_09   3.0
    @503  B13_09   1.0
    @504  B14_09   1.0
    @505  B15_09   1.0
    @506  BIDX_10  2.0
    @508  BORD_10  2.0
    @510  B0_10    1.0
    @511  B1_10    2.0
    @513  B2_10    2.0
    @515  B3_10    4.0
    @519  B4_10    1.0
    @520  B5_10    1.0
    @521  B6_10    3.0
    @524  B7_10    3.0
    @527  B8_10    2.0
    @529  B9_10    1.0
    @530  B10_10   1.0
    @531  B11_10   3.0
    @534  B12_10   3.0
    @537  B13_10   1.0
    @538  B14_10   1.0
    @539  B15_10   1.0
    @540  BIDX_11  2.0
    @542  BORD_11  2.0
    @544  B0_11    1.0
    @545  B1_11    2.0
    @547  B2_11    2.0
    @549  B3_11    4.0
    @553  B4_11    1.0
    @554  B5_11    1.0
    @555  B6_11    3.0
    @558  B7_11    3.0
    @561  B8_11    2.0
    @563  B9_11    1.0
    @564  B10_11   1.0
    @565  B11_11   3.0
    @568  B12_11   3.0
    @571  B13_11   1.0
    @572  B14_11   1.0
    @573  B15_11   1.0
    @574  BIDX_12  2.0
    @576  BORD_12  2.0
    @578  B0_12    1.0
    @579  B1_12    2.0
    @581  B2_12    2.0
    @583  B3_12    4.0
    @587  B4_12    1.0
    @588  B5_12    1.0
    @589  B6_12    3.0
    @592  B7_12    3.0
    @595  B8_12    2.0
    @597  B9_12    1.0
    @598  B10_12   1.0
    @599  B11_12   3.0
    @602  B12_12   3.0
    @605  B13_12   1.0
    @606  B14_12   1.0
    @607  B15_12   1.0
    @608  BIDX_13  2.0
    @610  BORD_13  2.0
    @612  B0_13    1.0
    @613  B1_13    2.0
    @615  B2_13    2.0
    @617  B3_13    4.0
    @621  B4_13    1.0
    @622  B5_13    1.0
    @623  B6_13    3.0
    @626  B7_13    3.0
    @629  B8_13    2.0
    @631  B9_13    1.0
    @632  B10_13   1.0
    @633  B11_13   3.0
    @636  B12_13   3.0
    @639  B13_13   1.0
    @640  B14_13   1.0
    @641  B15_13   1.0
    @642  BIDX_14  2.0
    @644  BORD_14  2.0
    @646  B0_14    1.0
    @647  B1_14    2.0
    @649  B2_14    2.0
    @651  B3_14    4.0
    @655  B4_14    1.0
    @656  B5_14    1.0
    @657  B6_14    3.0
    @660  B7_14    3.0
    @663  B8_14    2.0
    @665  B9_14    1.0
    @666  B10_14   1.0
    @667  B11_14   3.0
    @670  B12_14   3.0
    @673  B13_14   1.0
    @674  B14_14   1.0
    @675  B15_14   1.0
    @676  BIDX_15  2.0
    @678  BORD_15  2.0
    @680  B0_15    1.0
    @681  B1_15    2.0
    @683  B2_15    2.0
    @685  B3_15    4.0
    @689  B4_15    1.0
    @690  B5_15    1.0
    @691  B6_15    3.0
    @694  B7_15    3.0
    @697  B8_15    2.0
    @699  B9_15    1.0
    @700  B10_15   1.0
    @701  B11_15   3.0
    @704  B12_15   3.0
    @707  B13_15   1.0
    @708  B14_15   1.0
    @709  B15_15   1.0
    @710  BIDX_16  2.0
    @712  BORD_16  2.0
    @714  B0_16    1.0
    @715  B1_16    2.0
    @717  B2_16    2.0
    @719  B3_16    4.0
    @723  B4_16    1.0
    @724  B5_16    1.0
    @725  B6_16    3.0
    @728  B7_16    3.0
    @731  B8_16    2.0
    @733  B9_16    1.0
    @734  B10_16   1.0
    @735  B11_16   3.0
    @738  B12_16   3.0
    @741  B13_16   1.0
    @742  B14_16   1.0
    @743  B15_16   1.0
    @744  BIDX_17  2.0
    @746  BORD_17  2.0
    @748  B0_17    1.0
    @749  B1_17    2.0
    @751  B2_17    2.0
    @753  B3_17    4.0
    @757  B4_17    1.0
    @758  B5_17    1.0
    @759  B6_17    3.0
    @762  B7_17    3.0
    @765  B8_17    2.0
    @767  B9_17    1.0
    @768  B10_17   1.0
    @769  B11_17   3.0
    @772  B12_17   3.0
    @775  B13_17   1.0
    @776  B14_17   1.0
    @777  B15_17   1.0
    @778  BIDX_18  2.0
    @780  BORD_18  2.0
    @782  B0_18    1.0
    @783  B1_18    2.0
    @785  B2_18    2.0
    @787  B3_18    4.0
    @791  B4_18    1.0
    @792  B5_18    1.0
    @793  B6_18    3.0
    @796  B7_18    3.0
    @799  B8_18    2.0
    @801  B9_18    1.0
    @802  B10_18   1.0
    @803  B11_18   3.0
    @806  B12_18   3.0
    @809  B13_18   1.0
    @810  B14_18   1.0
    @811  B15_18   1.0
    @812  BIDX_19  2.0
    @814  BORD_19  2.0
    @816  B0_19    1.0
    @817  B1_19    2.0
    @819  B2_19    2.0
    @821  B3_19    4.0
    @825  B4_19    1.0
    @826  B5_19    1.0
    @827  B6_19    3.0
    @830  B7_19    3.0
    @833  B8_19    2.0
    @835  B9_19    1.0
    @836  B10_19   1.0
    @837  B11_19   3.0
    @840  B12_19   3.0
    @843  B13_19   1.0
    @844  B14_19   1.0
    @845  B15_19   1.0
    @846  BIDX_20  2.0
    @848  BORD_20  2.0
    @850  B0_20    1.0
    @851  B1_20    2.0
    @853  B2_20    2.0
    @855  B3_20    4.0
    @859  B4_20    1.0
    @860  B5_20    1.0
    @861  B6_20    3.0
    @864  B7_20    3.0
    @867  B8_20    2.0
    @869  B9_20    1.0
    @870  B10_20   1.0
    @871  B11_20   3.0
    @874  B12_20   3.0
    @877  B13_20   1.0
    @878  B14_20   1.0
    @879  B15_20   1.0
    @880  V201     2.0
    @882  V202     2.0
    @884  V203     2.0
    @886  V204     2.0
    @888  V205     2.0
    @890  V206     2.0
    @892  V207     2.0
    @894  V208     1.0
    @895  V209     1.0
    @896  V210     1.0
    @897  V211     4.0
    @901  V212     2.0
    @903  V213     1.0
    @904  V214     2.0
    @906  V215     3.0
    @909  V216     1.0
    @910  V217     1.0
    @911  V218     2.0
    @913  V219     2.0
    @915  V220     1.0
    @916  V221     3.0
    @919  V222     3.0
    @922  V223     1.0
    @923  V224     2.0
    @925  V225     1.0
    @926  V226     3.0
    @929  V227     1.0
    @930  V228     1.0
    @931  V229     2.0
    @933  V230     2.0
    @935  V231     4.0
    @939  V232     1.0
    @940  V233     2.0
    @942  V234     1.0
    @943  V235     2.0
    @945  V236     1.0
    @946  V237     1.0
    @947  V238     1.0
    @948  V301     1.0
    @949  V302     1.0
    @950  V304A_01 1.0
    @951  V304_01  1.0
    @952  V305_01  1.0
    @953  V304A_02 1.0
    @954  V304_02  1.0
    @955  V305_02  1.0
    @956  V304A_03 1.0
    @957  V304_03  1.0
    @958  V305_03  1.0
    @959  V304A_04 1.0
    @960  V304_04  1.0
    @961  V305_04  1.0
    @962  V304A_05 1.0
    @963  V304_05  1.0
    @964  V305_05  1.0
    @965  V304A_06 1.0
    @966  V304_06  1.0
    @967  V305_06  1.0
    @968  V304A_07 1.0
    @969  V304_07  1.0
    @970  V305_07  1.0
    @971  V304A_08 1.0
    @972  V304_08  1.0
    @973  V305_08  1.0
    @974  V304A_09 1.0
    @975  V304_09  1.0
    @976  V305_09  1.0
    @977  V304A_10 1.0
    @978  V304_10  1.0
    @979  V305_10  1.0
    @980  V304A_11 1.0
    @981  V304_11  1.0
    @982  V305_11  1.0
    @983  V304A_12 1.0
    @984  V304_12  1.0
    @985  V305_12  1.0
    @986  V304A_13 1.0
    @987  V304_13  1.0
    @988  V305_13  1.0
    @989  V304A_14 1.0
    @990  V304_14  1.0
    @991  V305_14  1.0
    @992  V304A_15 1.0
    @993  V304_15  1.0
    @994  V305_15  1.0
    @995  V310     2.0
    @997  V311     1.0
    @998  V312     2.0
    @1000 V313     1.0
    @1001 V314     1.0
    @1002 V315     2.0
    @1004 V316     2.0
    @1006 V317     4.0
    @1010 V318     1.0
    @1011 V319     1.0
    @1012 V320     1.0
    @1013 V321     1.0
    @1014 V322     1.0
    @1015 V323     2.0
    @1017 V325     6.0
    @1023 V326     2.0
    @1025 V327     1.0
    @1026 V328     2.0
    @1028 V337     3.0
    @1031 V359     2.0
    @1033 V360     2.0
    @1035 V361     1.0
    @1036 V362     1.0
    @1037 V363     2.0
    @1039 V364     1.0
    @1040 V366A    1.0
    @1041 V366B    1.0
    @1042 V367     1.0
    @1043 V369     2.0
    @1045 V369B    1.0
    @1046 V372     1.0
    @1047 V375A    2.0
    @1049 V376     2.0
    @1051 V376A    1.0
    @1052 V379     2.0
    @1054 V380     1.0
    @1055 V384A    1.0
    @1056 V384B    1.0
    @1057 V384C    1.0
    @1058 V384D    1.0
    @1059 V384E    1.0
    @1060 V393     1.0
    @1061 V394     1.0
    @1062 V395     1.0
    @1063 V396     1.0
    @1064 V397     1.0
    @1065 V398     1.0
    @1066 V399     1.0
    @1067 MIDX_1   1.0
    @1068 M1_1     1.0
    @1069 M2A_1    1.0
    @1070 M2B_1    1.0
    @1071 M2C_1    1.0
    @1072 M2D_1    1.0
    @1073 M2E_1    1.0
    @1074 M2F_1    1.0
    @1075 M2G_1    1.0
    @1076 M2H_1    1.0
    @1077 M2I_1    1.0
    @1078 M2J_1    1.0
    @1079 M2K_1    1.0
    @1080 M2L_1    1.0
    @1081 M2M_1    1.0
    @1082 M2N_1    1.0
    @1083 M3A_1    1.0
    @1084 M3B_1    1.0
    @1085 M3C_1    1.0
    @1086 M3D_1    1.0
    @1087 M3E_1    1.0
    @1088 M3F_1    1.0
    @1089 M3G_1    1.0
    @1090 M3H_1    1.0
    @1091 M3I_1    1.0
    @1092 M3J_1    1.0
    @1093 M3K_1    1.0
    @1094 M3L_1    1.0
    @1095 M3M_1    1.0
    @1096 M3N_1    1.0
    @1097 M4_1     2.0
    @1099 M5_1     2.0
    @1101 M6_1     2.0
    @1103 M7_1     2.0
    @1105 M8_1     2.0
    @1107 M9_1     2.0
    @1109 M10_1    1.0
    @1110 M11_1    3.0
    @1113 M13_1    2.0
    @1115 M14_1    2.0
    @1117 M15_1    2.0
    @1119 M17_1    1.0
    @1120 M18_1    1.0
    @1121 M19_1    4.0
    @1125 M19A_1   1.0
    @1126 M21_1    2.0
    @1128 M27_1    1.0
    @1129 M28_1    1.0
    @1130 M29_1    1.0
    @1131 M30_1    1.0
    @1132 M31_1    1.0
    @1133 M32_1    1.0
    @1134 M33_1    1.0
    @1135 M34_1    3.0
    @1138 M35_1    2.0
    @1140 M36_1    2.0
    @1142 M37A_1   1.0
    @1143 M37B_1   1.0
    @1144 M37C_1   1.0
    @1145 M37D_1   1.0
    @1146 M37E_1   1.0
    @1147 M37F_1   1.0
    @1148 M37G_1   1.0
    @1149 M37H_1   1.0
    @1150 M37I_1   1.0
    @1151 M37J_1   1.0
    @1152 M37K_1   1.0
    @1153 M37L_1   1.0
    @1154 M37M_1   1.0
    @1155 M37N_1   1.0
    @1156 M37O_1   1.0
    @1157 M37P_1   1.0
    @1158 M37Q_1   1.0
    @1159 M37R_1   1.0
    @1160 M37S_1   1.0
    @1161 M37T_1   1.0
    @1162 M38_1    1.0
    @1163 M39_1    1.0
    @1164 M40A_1   1.0
    @1165 M40B_1   1.0
    @1166 M40C_1   1.0
    @1167 M40D_1   1.0
    @1168 M40E_1   1.0
    @1169 M40F_1   1.0
    @1170 M40G_1   1.0
    @1171 M40H_1   1.0
    @1172 M40I_1   1.0
    @1173 M40J_1   1.0
    @1174 M40K_1   1.0
    @1175 M40L_1   1.0
    @1176 M40M_1   1.0
    @1177 M40N_1   1.0
    @1178 M40O_1   1.0
    @1179 MIDX_2   1.0
    @1180 M1_2     1.0
    @1181 M2A_2    1.0
    @1182 M2B_2    1.0
    @1183 M2C_2    1.0
    @1184 M2D_2    1.0
    @1185 M2E_2    1.0
    @1186 M2F_2    1.0
    @1187 M2G_2    1.0
    @1188 M2H_2    1.0
    @1189 M2I_2    1.0
    @1190 M2J_2    1.0
    @1191 M2K_2    1.0
    @1192 M2L_2    1.0
    @1193 M2M_2    1.0
    @1194 M2N_2    1.0
    @1195 M3A_2    1.0
    @1196 M3B_2    1.0
    @1197 M3C_2    1.0
    @1198 M3D_2    1.0
    @1199 M3E_2    1.0
    @1200 M3F_2    1.0
    @1201 M3G_2    1.0
    @1202 M3H_2    1.0
    @1203 M3I_2    1.0
    @1204 M3J_2    1.0
    @1205 M3K_2    1.0
    @1206 M3L_2    1.0
    @1207 M3M_2    1.0
    @1208 M3N_2    1.0
    @1209 M4_2     2.0
    @1211 M5_2     2.0
    @1213 M6_2     2.0
    @1215 M7_2     2.0
    @1217 M8_2     2.0
    @1219 M9_2     2.0
    @1221 M10_2    1.0
    @1222 M11_2    3.0
    @1225 M13_2    2.0
    @1227 M14_2    2.0
    @1229 M15_2    2.0
    @1231 M17_2    1.0
    @1232 M18_2    1.0
    @1233 M19_2    4.0
    @1237 M19A_2   1.0
    @1238 M21_2    2.0
    @1240 M27_2    1.0
    @1241 M28_2    1.0
    @1242 M29_2    1.0
    @1243 M30_2    1.0
    @1244 M31_2    1.0
    @1245 M32_2    1.0
    @1246 M33_2    1.0
    @1247 M34_2    3.0
    @1250 M35_2    2.0
    @1252 M36_2    2.0
    @1254 M37A_2   1.0
    @1255 M37B_2   1.0
    @1256 M37C_2   1.0
    @1257 M37D_2   1.0
    @1258 M37E_2   1.0
    @1259 M37F_2   1.0
    @1260 M37G_2   1.0
    @1261 M37H_2   1.0
    @1262 M37I_2   1.0
    @1263 M37J_2   1.0
    @1264 M37K_2   1.0
    @1265 M37L_2   1.0
    @1266 M37M_2   1.0
    @1267 M37N_2   1.0
    @1268 M37O_2   1.0
    @1269 M37P_2   1.0
    @1270 M37Q_2   1.0
    @1271 M37R_2   1.0
    @1272 M37S_2   1.0
    @1273 M37T_2   1.0
    @1274 M38_2    1.0
    @1275 M39_2    1.0
    @1276 M40A_2   1.0
    @1277 M40B_2   1.0
    @1278 M40C_2   1.0
    @1279 M40D_2   1.0
    @1280 M40E_2   1.0
    @1281 M40F_2   1.0
    @1282 M40G_2   1.0
    @1283 M40H_2   1.0
    @1284 M40I_2   1.0
    @1285 M40J_2   1.0
    @1286 M40K_2   1.0
    @1287 M40L_2   1.0
    @1288 M40M_2   1.0
    @1289 M40N_2   1.0
    @1290 M40O_2   1.0
    @1291 MIDX_3   1.0
    @1292 M1_3     1.0
    @1293 M2A_3    1.0
    @1294 M2B_3    1.0
    @1295 M2C_3    1.0
    @1296 M2D_3    1.0
    @1297 M2E_3    1.0
    @1298 M2F_3    1.0
    @1299 M2G_3    1.0
    @1300 M2H_3    1.0
    @1301 M2I_3    1.0
    @1302 M2J_3    1.0
    @1303 M2K_3    1.0
    @1304 M2L_3    1.0
    @1305 M2M_3    1.0
    @1306 M2N_3    1.0
    @1307 M3A_3    1.0
    @1308 M3B_3    1.0
    @1309 M3C_3    1.0
    @1310 M3D_3    1.0
    @1311 M3E_3    1.0
    @1312 M3F_3    1.0
    @1313 M3G_3    1.0
    @1314 M3H_3    1.0
    @1315 M3I_3    1.0
    @1316 M3J_3    1.0
    @1317 M3K_3    1.0
    @1318 M3L_3    1.0
    @1319 M3M_3    1.0
    @1320 M3N_3    1.0
    @1321 M4_3     2.0
    @1323 M5_3     2.0
    @1325 M6_3     2.0
    @1327 M7_3     2.0
    @1329 M8_3     2.0
    @1331 M9_3     2.0
    @1333 M10_3    1.0
    @1334 M11_3    3.0
    @1337 M13_3    2.0
    @1339 M14_3    2.0
    @1341 M15_3    2.0
    @1343 M17_3    1.0
    @1344 M18_3    1.0
    @1345 M19_3    4.0
    @1349 M19A_3   1.0
    @1350 M21_3    2.0
    @1352 M27_3    1.0
    @1353 M28_3    1.0
    @1354 M29_3    1.0
    @1355 M30_3    1.0
    @1356 M31_3    1.0
    @1357 M32_3    1.0
    @1358 M33_3    1.0
    @1359 M34_3    3.0
    @1362 M35_3    2.0
    @1364 M36_3    2.0
    @1366 M37A_3   1.0
    @1367 M37B_3   1.0
    @1368 M37C_3   1.0
    @1369 M37D_3   1.0
    @1370 M37E_3   1.0
    @1371 M37F_3   1.0
    @1372 M37G_3   1.0
    @1373 M37H_3   1.0
    @1374 M37I_3   1.0
    @1375 M37J_3   1.0
    @1376 M37K_3   1.0
    @1377 M37L_3   1.0
    @1378 M37M_3   1.0
    @1379 M37N_3   1.0
    @1380 M37O_3   1.0
    @1381 M37P_3   1.0
    @1382 M37Q_3   1.0
    @1383 M37R_3   1.0
    @1384 M37S_3   1.0
    @1385 M37T_3   1.0
    @1386 M38_3    1.0
    @1387 M39_3    1.0
    @1388 M40A_3   1.0
    @1389 M40B_3   1.0
    @1390 M40C_3   1.0
    @1391 M40D_3   1.0
    @1392 M40E_3   1.0
    @1393 M40F_3   1.0
    @1394 M40G_3   1.0
    @1395 M40H_3   1.0
    @1396 M40I_3   1.0
    @1397 M40J_3   1.0
    @1398 M40K_3   1.0
    @1399 M40L_3   1.0
    @1400 M40M_3   1.0
    @1401 M40N_3   1.0
    @1402 M40O_3   1.0
    @1403 MIDX_4   1.0
    @1404 M1_4     1.0
    @1405 M2A_4    1.0
    @1406 M2B_4    1.0
    @1407 M2C_4    1.0
    @1408 M2D_4    1.0
    @1409 M2E_4    1.0
    @1410 M2F_4    1.0
    @1411 M2G_4    1.0
    @1412 M2H_4    1.0
    @1413 M2I_4    1.0
    @1414 M2J_4    1.0
    @1415 M2K_4    1.0
    @1416 M2L_4    1.0
    @1417 M2M_4    1.0
    @1418 M2N_4    1.0
    @1419 M3A_4    1.0
    @1420 M3B_4    1.0
    @1421 M3C_4    1.0
    @1422 M3D_4    1.0
    @1423 M3E_4    1.0
    @1424 M3F_4    1.0
    @1425 M3G_4    1.0
    @1426 M3H_4    1.0
    @1427 M3I_4    1.0
    @1428 M3J_4    1.0
    @1429 M3K_4    1.0
    @1430 M3L_4    1.0
    @1431 M3M_4    1.0
    @1432 M3N_4    1.0
    @1433 M4_4     2.0
    @1435 M5_4     2.0
    @1437 M6_4     2.0
    @1439 M7_4     2.0
    @1441 M8_4     2.0
    @1443 M9_4     2.0
    @1445 M10_4    1.0
    @1446 M11_4    3.0
    @1449 M13_4    2.0
    @1451 M14_4    2.0
    @1453 M15_4    2.0
    @1455 M17_4    1.0
    @1456 M18_4    1.0
    @1457 M19_4    4.0
    @1461 M19A_4   1.0
    @1462 M21_4    2.0
    @1464 M27_4    1.0
    @1465 M28_4    1.0
    @1466 M29_4    1.0
    @1467 M30_4    1.0
    @1468 M31_4    1.0
    @1469 M32_4    1.0
    @1470 M33_4    1.0
    @1471 M34_4    3.0
    @1474 M35_4    2.0
    @1476 M36_4    2.0
    @1478 M37A_4   1.0
    @1479 M37B_4   1.0
    @1480 M37C_4   1.0
    @1481 M37D_4   1.0
    @1482 M37E_4   1.0
    @1483 M37F_4   1.0
    @1484 M37G_4   1.0
    @1485 M37H_4   1.0
    @1486 M37I_4   1.0
    @1487 M37J_4   1.0
    @1488 M37K_4   1.0
    @1489 M37L_4   1.0
    @1490 M37M_4   1.0
    @1491 M37N_4   1.0
    @1492 M37O_4   1.0
    @1493 M37P_4   1.0
    @1494 M37Q_4   1.0
    @1495 M37R_4   1.0
    @1496 M37S_4   1.0
    @1497 M37T_4   1.0
    @1498 M38_4    1.0
    @1499 M39_4    1.0
    @1500 M40A_4   1.0
    @1501 M40B_4   1.0
    @1502 M40C_4   1.0
    @1503 M40D_4   1.0
    @1504 M40E_4   1.0
    @1505 M40F_4   1.0
    @1506 M40G_4   1.0
    @1507 M40H_4   1.0
    @1508 M40I_4   1.0
    @1509 M40J_4   1.0
    @1510 M40K_4   1.0
    @1511 M40L_4   1.0
    @1512 M40M_4   1.0
    @1513 M40N_4   1.0
    @1514 M40O_4   1.0
    @1515 MIDX_5   1.0
    @1516 M1_5     1.0
    @1517 M2A_5    1.0
    @1518 M2B_5    1.0
    @1519 M2C_5    1.0
    @1520 M2D_5    1.0
    @1521 M2E_5    1.0
    @1522 M2F_5    1.0
    @1523 M2G_5    1.0
    @1524 M2H_5    1.0
    @1525 M2I_5    1.0
    @1526 M2J_5    1.0
    @1527 M2K_5    1.0
    @1528 M2L_5    1.0
    @1529 M2M_5    1.0
    @1530 M2N_5    1.0
    @1531 M3A_5    1.0
    @1532 M3B_5    1.0
    @1533 M3C_5    1.0
    @1534 M3D_5    1.0
    @1535 M3E_5    1.0
    @1536 M3F_5    1.0
    @1537 M3G_5    1.0
    @1538 M3H_5    1.0
    @1539 M3I_5    1.0
    @1540 M3J_5    1.0
    @1541 M3K_5    1.0
    @1542 M3L_5    1.0
    @1543 M3M_5    1.0
    @1544 M3N_5    1.0
    @1545 M4_5     2.0
    @1547 M5_5     2.0
    @1549 M6_5     2.0
    @1551 M7_5     2.0
    @1553 M8_5     2.0
    @1555 M9_5     2.0
    @1557 M10_5    1.0
    @1558 M11_5    3.0
    @1561 M13_5    2.0
    @1563 M14_5    2.0
    @1565 M15_5    2.0
    @1567 M17_5    1.0
    @1568 M18_5    1.0
    @1569 M19_5    4.0
    @1573 M19A_5   1.0
    @1574 M21_5    2.0
    @1576 M27_5    1.0
    @1577 M28_5    1.0
    @1578 M29_5    1.0
    @1579 M30_5    1.0
    @1580 M31_5    1.0
    @1581 M32_5    1.0
    @1582 M33_5    1.0
    @1583 M34_5    3.0
    @1586 M35_5    2.0
    @1588 M36_5    2.0
    @1590 M37A_5   1.0
    @1591 M37B_5   1.0
    @1592 M37C_5   1.0
    @1593 M37D_5   1.0
    @1594 M37E_5   1.0
    @1595 M37F_5   1.0
    @1596 M37G_5   1.0
    @1597 M37H_5   1.0
    @1598 M37I_5   1.0
    @1599 M37J_5   1.0
    @1600 M37K_5   1.0
    @1601 M37L_5   1.0
    @1602 M37M_5   1.0
    @1603 M37N_5   1.0
    @1604 M37O_5   1.0
    @1605 M37P_5   1.0
    @1606 M37Q_5   1.0
    @1607 M37R_5   1.0
    @1608 M37S_5   1.0
    @1609 M37T_5   1.0
    @1610 M38_5    1.0
    @1611 M39_5    1.0
    @1612 M40A_5   1.0
    @1613 M40B_5   1.0
    @1614 M40C_5   1.0
    @1615 M40D_5   1.0
    @1616 M40E_5   1.0
    @1617 M40F_5   1.0
    @1618 M40G_5   1.0
    @1619 M40H_5   1.0
    @1620 M40I_5   1.0
    @1621 M40J_5   1.0
    @1622 M40K_5   1.0
    @1623 M40L_5   1.0
    @1624 M40M_5   1.0
    @1625 M40N_5   1.0
    @1626 M40O_5   1.0
    @1627 MIDX_6   1.0
    @1628 M1_6     1.0
    @1629 M2A_6    1.0
    @1630 M2B_6    1.0
    @1631 M2C_6    1.0
    @1632 M2D_6    1.0
    @1633 M2E_6    1.0
    @1634 M2F_6    1.0
    @1635 M2G_6    1.0
    @1636 M2H_6    1.0
    @1637 M2I_6    1.0
    @1638 M2J_6    1.0
    @1639 M2K_6    1.0
    @1640 M2L_6    1.0
    @1641 M2M_6    1.0
    @1642 M2N_6    1.0
    @1643 M3A_6    1.0
    @1644 M3B_6    1.0
    @1645 M3C_6    1.0
    @1646 M3D_6    1.0
    @1647 M3E_6    1.0
    @1648 M3F_6    1.0
    @1649 M3G_6    1.0
    @1650 M3H_6    1.0
    @1651 M3I_6    1.0
    @1652 M3J_6    1.0
    @1653 M3K_6    1.0
    @1654 M3L_6    1.0
    @1655 M3M_6    1.0
    @1656 M3N_6    1.0
    @1657 M4_6     2.0
    @1659 M5_6     2.0
    @1661 M6_6     2.0
    @1663 M7_6     2.0
    @1665 M8_6     2.0
    @1667 M9_6     2.0
    @1669 M10_6    1.0
    @1670 M11_6    3.0
    @1673 M13_6    2.0
    @1675 M14_6    2.0
    @1677 M15_6    2.0
    @1679 M17_6    1.0
    @1680 M18_6    1.0
    @1681 M19_6    4.0
    @1685 M19A_6   1.0
    @1686 M21_6    2.0
    @1688 M27_6    1.0
    @1689 M28_6    1.0
    @1690 M29_6    1.0
    @1691 M30_6    1.0
    @1692 M31_6    1.0
    @1693 M32_6    1.0
    @1694 M33_6    1.0
    @1695 M34_6    3.0
    @1698 M35_6    2.0
    @1700 M36_6    2.0
    @1702 M37A_6   1.0
    @1703 M37B_6   1.0
    @1704 M37C_6   1.0
    @1705 M37D_6   1.0
    @1706 M37E_6   1.0
    @1707 M37F_6   1.0
    @1708 M37G_6   1.0
    @1709 M37H_6   1.0
    @1710 M37I_6   1.0
    @1711 M37J_6   1.0
    @1712 M37K_6   1.0
    @1713 M37L_6   1.0
    @1714 M37M_6   1.0
    @1715 M37N_6   1.0
    @1716 M37O_6   1.0
    @1717 M37P_6   1.0
    @1718 M37Q_6   1.0
    @1719 M37R_6   1.0
    @1720 M37S_6   1.0
    @1721 M37T_6   1.0
    @1722 M38_6    1.0
    @1723 M39_6    1.0
    @1724 M40A_6   1.0
    @1725 M40B_6   1.0
    @1726 M40C_6   1.0
    @1727 M40D_6   1.0
    @1728 M40E_6   1.0
    @1729 M40F_6   1.0
    @1730 M40G_6   1.0
    @1731 M40H_6   1.0
    @1732 M40I_6   1.0
    @1733 M40J_6   1.0
    @1734 M40K_6   1.0
    @1735 M40L_6   1.0
    @1736 M40M_6   1.0
    @1737 M40N_6   1.0
    @1738 M40O_6   1.0
    @1739 V401     1.0
    @1740 V404     1.0
    @1741 V405     1.0
    @1742 V406     1.0
    @1743 V407     2.0
    @1745 V408     2.0
    @1747 V409     1.0
    @1748 V409A    1.0
    @1749 V410     1.0
    @1750 V410A    1.0
    @1751 V411     1.0
    @1752 V411A    1.0
    @1753 V412     1.0
    @1754 V413A    1.0
    @1755 V413B    1.0
    @1756 V413C    1.0
    @1757 V413D    1.0
    @1758 V413     1.0
    @1759 V414A    1.0
    @1760 V414B    1.0
    @1761 V414C    1.0
    @1762 V414D    1.0
    @1763 V414E    1.0
    @1764 V414F    1.0
    @1765 V414G    1.0
    @1766 V414H    1.0
    @1767 V415     1.0
    @1768 V416     1.0
    @1769 V417     1.0
    @1770 V418     1.0
    @1771 V419     1.0
    @1772 V420     2.0
    @1774 V421     2.0
    @1776 V426     3.0
    @1779 V436     3.0
    @1782 V437     4.0
    @1786 V438     4.0
    @1790 V439     4.0
    @1794 V440     4.0
    @1798 V441     5.0
    @1803 V442     5.0
    @1808 V443     5.0
    @1813 V444     5.0
    @1818 V444A    4.0
    @1822 V445     4.0
    @1826 V446     4.0
    @1830 V447     1.0
    @1831 V448     1.0
    @1832 V449     1.0
    @1833 V450A    1.0
    @1834 V450B    1.0
    @1835 V450C    1.0
    @1836 V450D    1.0
    @1837 V450E    1.0
    @1838 V450F    1.0
    @1839 V450G    1.0
    @1840 V450H    1.0
    @1841 V450I    1.0
    @1842 V450J    1.0
    @1843 V450K    1.0
    @1844 V450L    1.0
    @1845 V450M    1.0
    @1846 V450X    1.0
    @1847 V450Z    1.0
    @1848 V451A    1.0
    @1849 V451B    1.0
    @1850 V451C    1.0
    @1851 V451D    1.0
    @1852 V451E    1.0
    @1853 V451F    1.0
    @1854 V451G    1.0
    @1855 V451H    1.0
    @1856 V451I    1.0
    @1857 V451J    1.0
    @1858 V451K    1.0
    @1859 V451X    1.0
    @1860 V451Z    1.0
    @1861 HIDX_1   1.0
    @1862 H1_1     1.0
    @1863 H2_1     1.0
    @1864 H2D_1    2.0
    @1866 H2M_1    2.0
    @1868 H2Y_1    2.0
    @1870 H3_1     1.0
    @1871 H3D_1    2.0
    @1873 H3M_1    2.0
    @1875 H3Y_1    2.0
    @1877 H4_1     1.0
    @1878 H4D_1    2.0
    @1880 H4M_1    2.0
    @1882 H4Y_1    2.0
    @1884 H5_1     1.0
    @1885 H5D_1    2.0
    @1887 H5M_1    2.0
    @1889 H5Y_1    2.0
    @1891 H6_1     1.0
    @1892 H6D_1    2.0
    @1894 H6M_1    2.0
    @1896 H6Y_1    2.0
    @1898 H7_1     1.0
    @1899 H7D_1    2.0
    @1901 H7M_1    2.0
    @1903 H7Y_1    2.0
    @1905 H8_1     1.0
    @1906 H8D_1    2.0
    @1908 H8M_1    2.0
    @1910 H8Y_1    2.0
    @1912 H9_1     1.0
    @1913 H9D_1    2.0
    @1915 H9M_1    2.0
    @1917 H9Y_1    2.0
    @1919 H0_1     1.0
    @1920 H0D_1    2.0
    @1922 H0M_1    2.0
    @1924 H0Y_1    2.0
    @1926 H10_1    1.0
    @1927 H11_1    1.0
    @1928 H11B_1   1.0
    @1929 H11C_1   2.0
    @1931 H12A_1   1.0
    @1932 H12B_1   1.0
    @1933 H12C_1   1.0
    @1934 H12D_1   1.0
    @1935 H12E_1   1.0
    @1936 H12F_1   1.0
    @1937 H12G_1   1.0
    @1938 H12H_1   1.0
    @1939 H12I_1   1.0
    @1940 H12J_1   1.0
    @1941 H12K_1   1.0
    @1942 H12L_1   1.0
    @1943 H12M_1   1.0
    @1944 H12N_1   1.0
    @1945 H12O_1   1.0
    @1946 H12P_1   1.0
    @1947 H12Q_1   1.0
    @1948 H12R_1   1.0
    @1949 H12S_1   1.0
    @1950 H12T_1   1.0
    @1951 H12U_1   1.0
    @1952 H12V_1   1.0
    @1953 H12W_1   1.0
    @1954 H12X_1   1.0
    @1955 H12Y_1   1.0
    @1956 H12Z_1   1.0
    @1957 H13_1    1.0
    @1958 H14_1    1.0
    @1959 H15_1    1.0
    @1960 H15A_1   1.0
    @1961 H15B_1   1.0
    @1962 H15C_1   1.0
    @1963 H15D_1   1.0
    @1964 H15E_1   1.0
    @1965 H15F_1   1.0
    @1966 H15G_1   1.0
    @1967 H15H_1   1.0
    @1968 H16_1    1.0
    @1969 H18_1    1.0
    @1970 H20_1    1.0
    @1971 H21A_1   1.0
    @1972 H21_1    1.0
    @1973 H22_1    1.0
    @1974 H31_1    1.0
    @1975 H31B_1   1.0
    @1976 H32A_1   1.0
    @1977 H32B_1   1.0
    @1978 H32C_1   1.0
    @1979 H32D_1   1.0
    @1980 H32E_1   1.0
    @1981 H32F_1   1.0
    @1982 H32G_1   1.0
    @1983 H32H_1   1.0
    @1984 H32I_1   1.0
    @1985 H32J_1   1.0
    @1986 H32K_1   1.0
    @1987 H32L_1   1.0
    @1988 H32M_1   1.0
    @1989 H32N_1   1.0
    @1990 H32O_1   1.0
    @1991 H32P_1   1.0
    @1992 H32Q_1   1.0
    @1993 H32R_1   1.0
    @1994 H32S_1   1.0
    @1995 H32T_1   1.0
    @1996 H32U_1   1.0
    @1997 H32V_1   1.0
    @1998 H32W_1   1.0
    @1999 H32X_1   1.0
    @2000 H32Y_1   1.0
    @2001 H32Z_1   1.0
    @2002 HIDX_2   1.0
    @2003 H1_2     1.0
    @2004 H2_2     1.0
    @2005 H2D_2    2.0
    @2007 H2M_2    2.0
    @2009 H2Y_2    2.0
    @2011 H3_2     1.0
    @2012 H3D_2    2.0
    @2014 H3M_2    2.0
    @2016 H3Y_2    2.0
    @2018 H4_2     1.0
    @2019 H4D_2    2.0
    @2021 H4M_2    2.0
    @2023 H4Y_2    2.0
    @2025 H5_2     1.0
    @2026 H5D_2    2.0
    @2028 H5M_2    2.0
    @2030 H5Y_2    2.0
    @2032 H6_2     1.0
    @2033 H6D_2    2.0
    @2035 H6M_2    2.0
    @2037 H6Y_2    2.0
    @2039 H7_2     1.0
    @2040 H7D_2    2.0
    @2042 H7M_2    2.0
    @2044 H7Y_2    2.0
    @2046 H8_2     1.0
    @2047 H8D_2    2.0
    @2049 H8M_2    2.0
    @2051 H8Y_2    2.0
    @2053 H9_2     1.0
    @2054 H9D_2    2.0
    @2056 H9M_2    2.0
    @2058 H9Y_2    2.0
    @2060 H0_2     1.0
    @2061 H0D_2    2.0
    @2063 H0M_2    2.0
    @2065 H0Y_2    2.0
    @2067 H10_2    1.0
    @2068 H11_2    1.0
    @2069 H11B_2   1.0
    @2070 H11C_2   2.0
    @2072 H12A_2   1.0
    @2073 H12B_2   1.0
    @2074 H12C_2   1.0
    @2075 H12D_2   1.0
    @2076 H12E_2   1.0
    @2077 H12F_2   1.0
    @2078 H12G_2   1.0
    @2079 H12H_2   1.0
    @2080 H12I_2   1.0
    @2081 H12J_2   1.0
    @2082 H12K_2   1.0
    @2083 H12L_2   1.0
    @2084 H12M_2   1.0
    @2085 H12N_2   1.0
    @2086 H12O_2   1.0
    @2087 H12P_2   1.0
    @2088 H12Q_2   1.0
    @2089 H12R_2   1.0
    @2090 H12S_2   1.0
    @2091 H12T_2   1.0
    @2092 H12U_2   1.0
    @2093 H12V_2   1.0
    @2094 H12W_2   1.0
    @2095 H12X_2   1.0
    @2096 H12Y_2   1.0
    @2097 H12Z_2   1.0
    @2098 H13_2    1.0
    @2099 H14_2    1.0
    @2100 H15_2    1.0
    @2101 H15A_2   1.0
    @2102 H15B_2   1.0
    @2103 H15C_2   1.0
    @2104 H15D_2   1.0
    @2105 H15E_2   1.0
    @2106 H15F_2   1.0
    @2107 H15G_2   1.0
    @2108 H15H_2   1.0
    @2109 H16_2    1.0
    @2110 H18_2    1.0
    @2111 H20_2    1.0
    @2112 H21A_2   1.0
    @2113 H21_2    1.0
    @2114 H22_2    1.0
    @2115 H31_2    1.0
    @2116 H31B_2   1.0
    @2117 H32A_2   1.0
    @2118 H32B_2   1.0
    @2119 H32C_2   1.0
    @2120 H32D_2   1.0
    @2121 H32E_2   1.0
    @2122 H32F_2   1.0
    @2123 H32G_2   1.0
    @2124 H32H_2   1.0
    @2125 H32I_2   1.0
    @2126 H32J_2   1.0
    @2127 H32K_2   1.0
    @2128 H32L_2   1.0
    @2129 H32M_2   1.0
    @2130 H32N_2   1.0
    @2131 H32O_2   1.0
    @2132 H32P_2   1.0
    @2133 H32Q_2   1.0
    @2134 H32R_2   1.0
    @2135 H32S_2   1.0
    @2136 H32T_2   1.0
    @2137 H32U_2   1.0
    @2138 H32V_2   1.0
    @2139 H32W_2   1.0
    @2140 H32X_2   1.0
    @2141 H32Y_2   1.0
    @2142 H32Z_2   1.0
    @2143 HIDX_3   1.0
    @2144 H1_3     1.0
    @2145 H2_3     1.0
    @2146 H2D_3    2.0
    @2148 H2M_3    2.0
    @2150 H2Y_3    2.0
    @2152 H3_3     1.0
    @2153 H3D_3    2.0
    @2155 H3M_3    2.0
    @2157 H3Y_3    2.0
    @2159 H4_3     1.0
    @2160 H4D_3    2.0
    @2162 H4M_3    2.0
    @2164 H4Y_3    2.0
    @2166 H5_3     1.0
    @2167 H5D_3    2.0
    @2169 H5M_3    2.0
    @2171 H5Y_3    2.0
    @2173 H6_3     1.0
    @2174 H6D_3    2.0
    @2176 H6M_3    2.0
    @2178 H6Y_3    2.0
    @2180 H7_3     1.0
    @2181 H7D_3    2.0
    @2183 H7M_3    2.0
    @2185 H7Y_3    2.0
    @2187 H8_3     1.0
    @2188 H8D_3    2.0
    @2190 H8M_3    2.0
    @2192 H8Y_3    2.0
    @2194 H9_3     1.0
    @2195 H9D_3    2.0
    @2197 H9M_3    2.0
    @2199 H9Y_3    2.0
    @2201 H0_3     1.0
    @2202 H0D_3    2.0
    @2204 H0M_3    2.0
    @2206 H0Y_3    2.0
    @2208 H10_3    1.0
    @2209 H11_3    1.0
    @2210 H11B_3   1.0
    @2211 H11C_3   2.0
    @2213 H12A_3   1.0
    @2214 H12B_3   1.0
    @2215 H12C_3   1.0
    @2216 H12D_3   1.0
    @2217 H12E_3   1.0
    @2218 H12F_3   1.0
    @2219 H12G_3   1.0
    @2220 H12H_3   1.0
    @2221 H12I_3   1.0
    @2222 H12J_3   1.0
    @2223 H12K_3   1.0
    @2224 H12L_3   1.0
    @2225 H12M_3   1.0
    @2226 H12N_3   1.0
    @2227 H12O_3   1.0
    @2228 H12P_3   1.0
    @2229 H12Q_3   1.0
    @2230 H12R_3   1.0
    @2231 H12S_3   1.0
    @2232 H12T_3   1.0
    @2233 H12U_3   1.0
    @2234 H12V_3   1.0
    @2235 H12W_3   1.0
    @2236 H12X_3   1.0
    @2237 H12Y_3   1.0
    @2238 H12Z_3   1.0
    @2239 H13_3    1.0
    @2240 H14_3    1.0
    @2241 H15_3    1.0
    @2242 H15A_3   1.0
    @2243 H15B_3   1.0
    @2244 H15C_3   1.0
    @2245 H15D_3   1.0
    @2246 H15E_3   1.0
    @2247 H15F_3   1.0
    @2248 H15G_3   1.0
    @2249 H15H_3   1.0
    @2250 H16_3    1.0
    @2251 H18_3    1.0
    @2252 H20_3    1.0
    @2253 H21A_3   1.0
    @2254 H21_3    1.0
    @2255 H22_3    1.0
    @2256 H31_3    1.0
    @2257 H31B_3   1.0
    @2258 H32A_3   1.0
    @2259 H32B_3   1.0
    @2260 H32C_3   1.0
    @2261 H32D_3   1.0
    @2262 H32E_3   1.0
    @2263 H32F_3   1.0
    @2264 H32G_3   1.0
    @2265 H32H_3   1.0
    @2266 H32I_3   1.0
    @2267 H32J_3   1.0
    @2268 H32K_3   1.0
    @2269 H32L_3   1.0
    @2270 H32M_3   1.0
    @2271 H32N_3   1.0
    @2272 H32O_3   1.0
    @2273 H32P_3   1.0
    @2274 H32Q_3   1.0
    @2275 H32R_3   1.0
    @2276 H32S_3   1.0
    @2277 H32T_3   1.0
    @2278 H32U_3   1.0
    @2279 H32V_3   1.0
    @2280 H32W_3   1.0
    @2281 H32X_3   1.0
    @2282 H32Y_3   1.0
    @2283 H32Z_3   1.0
    @2284 HIDX_4   1.0
    @2285 H1_4     1.0
    @2286 H2_4     1.0
    @2287 H2D_4    2.0
    @2289 H2M_4    2.0
    @2291 H2Y_4    2.0
    @2293 H3_4     1.0
    @2294 H3D_4    2.0
    @2296 H3M_4    2.0
    @2298 H3Y_4    2.0
    @2300 H4_4     1.0
    @2301 H4D_4    2.0
    @2303 H4M_4    2.0
    @2305 H4Y_4    2.0
    @2307 H5_4     1.0
    @2308 H5D_4    2.0
    @2310 H5M_4    2.0
    @2312 H5Y_4    2.0
    @2314 H6_4     1.0
    @2315 H6D_4    2.0
    @2317 H6M_4    2.0
    @2319 H6Y_4    2.0
    @2321 H7_4     1.0
    @2322 H7D_4    2.0
    @2324 H7M_4    2.0
    @2326 H7Y_4    2.0
    @2328 H8_4     1.0
    @2329 H8D_4    2.0
    @2331 H8M_4    2.0
    @2333 H8Y_4    2.0
    @2335 H9_4     1.0
    @2336 H9D_4    2.0
    @2338 H9M_4    2.0
    @2340 H9Y_4    2.0
    @2342 H0_4     1.0
    @2343 H0D_4    2.0
    @2345 H0M_4    2.0
    @2347 H0Y_4    2.0
    @2349 H10_4    1.0
    @2350 H11_4    1.0
    @2351 H11B_4   1.0
    @2352 H11C_4   2.0
    @2354 H12A_4   1.0
    @2355 H12B_4   1.0
    @2356 H12C_4   1.0
    @2357 H12D_4   1.0
    @2358 H12E_4   1.0
    @2359 H12F_4   1.0
    @2360 H12G_4   1.0
    @2361 H12H_4   1.0
    @2362 H12I_4   1.0
    @2363 H12J_4   1.0
    @2364 H12K_4   1.0
    @2365 H12L_4   1.0
    @2366 H12M_4   1.0
    @2367 H12N_4   1.0
    @2368 H12O_4   1.0
    @2369 H12P_4   1.0
    @2370 H12Q_4   1.0
    @2371 H12R_4   1.0
    @2372 H12S_4   1.0
    @2373 H12T_4   1.0
    @2374 H12U_4   1.0
    @2375 H12V_4   1.0
    @2376 H12W_4   1.0
    @2377 H12X_4   1.0
    @2378 H12Y_4   1.0
    @2379 H12Z_4   1.0
    @2380 H13_4    1.0
    @2381 H14_4    1.0
    @2382 H15_4    1.0
    @2383 H15A_4   1.0
    @2384 H15B_4   1.0
    @2385 H15C_4   1.0
    @2386 H15D_4   1.0
    @2387 H15E_4   1.0
    @2388 H15F_4   1.0
    @2389 H15G_4   1.0
    @2390 H15H_4   1.0
    @2391 H16_4    1.0
    @2392 H18_4    1.0
    @2393 H20_4    1.0
    @2394 H21A_4   1.0
    @2395 H21_4    1.0
    @2396 H22_4    1.0
    @2397 H31_4    1.0
    @2398 H31B_4   1.0
    @2399 H32A_4   1.0
    @2400 H32B_4   1.0
    @2401 H32C_4   1.0
    @2402 H32D_4   1.0
    @2403 H32E_4   1.0
    @2404 H32F_4   1.0
    @2405 H32G_4   1.0
    @2406 H32H_4   1.0
    @2407 H32I_4   1.0
    @2408 H32J_4   1.0
    @2409 H32K_4   1.0
    @2410 H32L_4   1.0
    @2411 H32M_4   1.0
    @2412 H32N_4   1.0
    @2413 H32O_4   1.0
    @2414 H32P_4   1.0
    @2415 H32Q_4   1.0
    @2416 H32R_4   1.0
    @2417 H32S_4   1.0
    @2418 H32T_4   1.0
    @2419 H32U_4   1.0
    @2420 H32V_4   1.0
    @2421 H32W_4   1.0
    @2422 H32X_4   1.0
    @2423 H32Y_4   1.0
    @2424 H32Z_4   1.0
    @2425 HIDX_5   1.0
    @2426 H1_5     1.0
    @2427 H2_5     1.0
    @2428 H2D_5    2.0
    @2430 H2M_5    2.0
    @2432 H2Y_5    2.0
    @2434 H3_5     1.0
    @2435 H3D_5    2.0
    @2437 H3M_5    2.0
    @2439 H3Y_5    2.0
    @2441 H4_5     1.0
    @2442 H4D_5    2.0
    @2444 H4M_5    2.0
    @2446 H4Y_5    2.0
    @2448 H5_5     1.0
    @2449 H5D_5    2.0
    @2451 H5M_5    2.0
    @2453 H5Y_5    2.0
    @2455 H6_5     1.0
    @2456 H6D_5    2.0
    @2458 H6M_5    2.0
    @2460 H6Y_5    2.0
    @2462 H7_5     1.0
    @2463 H7D_5    2.0
    @2465 H7M_5    2.0
    @2467 H7Y_5    2.0
    @2469 H8_5     1.0
    @2470 H8D_5    2.0
    @2472 H8M_5    2.0
    @2474 H8Y_5    2.0
    @2476 H9_5     1.0
    @2477 H9D_5    2.0
    @2479 H9M_5    2.0
    @2481 H9Y_5    2.0
    @2483 H0_5     1.0
    @2484 H0D_5    2.0
    @2486 H0M_5    2.0
    @2488 H0Y_5    2.0
    @2490 H10_5    1.0
    @2491 H11_5    1.0
    @2492 H11B_5   1.0
    @2493 H11C_5   2.0
    @2495 H12A_5   1.0
    @2496 H12B_5   1.0
    @2497 H12C_5   1.0
    @2498 H12D_5   1.0
    @2499 H12E_5   1.0
    @2500 H12F_5   1.0
    @2501 H12G_5   1.0
    @2502 H12H_5   1.0
    @2503 H12I_5   1.0
    @2504 H12J_5   1.0
    @2505 H12K_5   1.0
    @2506 H12L_5   1.0
    @2507 H12M_5   1.0
    @2508 H12N_5   1.0
    @2509 H12O_5   1.0
    @2510 H12P_5   1.0
    @2511 H12Q_5   1.0
    @2512 H12R_5   1.0
    @2513 H12S_5   1.0
    @2514 H12T_5   1.0
    @2515 H12U_5   1.0
    @2516 H12V_5   1.0
    @2517 H12W_5   1.0
    @2518 H12X_5   1.0
    @2519 H12Y_5   1.0
    @2520 H12Z_5   1.0
    @2521 H13_5    1.0
    @2522 H14_5    1.0
    @2523 H15_5    1.0
    @2524 H15A_5   1.0
    @2525 H15B_5   1.0
    @2526 H15C_5   1.0
    @2527 H15D_5   1.0
    @2528 H15E_5   1.0
    @2529 H15F_5   1.0
    @2530 H15G_5   1.0
    @2531 H15H_5   1.0
    @2532 H16_5    1.0
    @2533 H18_5    1.0
    @2534 H20_5    1.0
    @2535 H21A_5   1.0
    @2536 H21_5    1.0
    @2537 H22_5    1.0
    @2538 H31_5    1.0
    @2539 H31B_5   1.0
    @2540 H32A_5   1.0
    @2541 H32B_5   1.0
    @2542 H32C_5   1.0
    @2543 H32D_5   1.0
    @2544 H32E_5   1.0
    @2545 H32F_5   1.0
    @2546 H32G_5   1.0
    @2547 H32H_5   1.0
    @2548 H32I_5   1.0
    @2549 H32J_5   1.0
    @2550 H32K_5   1.0
    @2551 H32L_5   1.0
    @2552 H32M_5   1.0
    @2553 H32N_5   1.0
    @2554 H32O_5   1.0
    @2555 H32P_5   1.0
    @2556 H32Q_5   1.0
    @2557 H32R_5   1.0
    @2558 H32S_5   1.0
    @2559 H32T_5   1.0
    @2560 H32U_5   1.0
    @2561 H32V_5   1.0
    @2562 H32W_5   1.0
    @2563 H32X_5   1.0
    @2564 H32Y_5   1.0
    @2565 H32Z_5   1.0
    @2566 HIDX_6   1.0
    @2567 H1_6     1.0
    @2568 H2_6     1.0
    @2569 H2D_6    2.0
    @2571 H2M_6    2.0
    @2573 H2Y_6    2.0
    @2575 H3_6     1.0
    @2576 H3D_6    2.0
    @2578 H3M_6    2.0
    @2580 H3Y_6    2.0
    @2582 H4_6     1.0
    @2583 H4D_6    2.0
    @2585 H4M_6    2.0
    @2587 H4Y_6    2.0
    @2589 H5_6     1.0
    @2590 H5D_6    2.0
    @2592 H5M_6    2.0
    @2594 H5Y_6    2.0
    @2596 H6_6     1.0
    @2597 H6D_6    2.0
    @2599 H6M_6    2.0
    @2601 H6Y_6    2.0
    @2603 H7_6     1.0
    @2604 H7D_6    2.0
    @2606 H7M_6    2.0
    @2608 H7Y_6    2.0
    @2610 H8_6     1.0
    @2611 H8D_6    2.0
    @2613 H8M_6    2.0
    @2615 H8Y_6    2.0
    @2617 H9_6     1.0
    @2618 H9D_6    2.0
    @2620 H9M_6    2.0
    @2622 H9Y_6    2.0
    @2624 H0_6     1.0
    @2625 H0D_6    2.0
    @2627 H0M_6    2.0
    @2629 H0Y_6    2.0
    @2631 H10_6    1.0
    @2632 H11_6    1.0
    @2633 H11B_6   1.0
    @2634 H11C_6   2.0
    @2636 H12A_6   1.0
    @2637 H12B_6   1.0
    @2638 H12C_6   1.0
    @2639 H12D_6   1.0
    @2640 H12E_6   1.0
    @2641 H12F_6   1.0
    @2642 H12G_6   1.0
    @2643 H12H_6   1.0
    @2644 H12I_6   1.0
    @2645 H12J_6   1.0
    @2646 H12K_6   1.0
    @2647 H12L_6   1.0
    @2648 H12M_6   1.0
    @2649 H12N_6   1.0
    @2650 H12O_6   1.0
    @2651 H12P_6   1.0
    @2652 H12Q_6   1.0
    @2653 H12R_6   1.0
    @2654 H12S_6   1.0
    @2655 H12T_6   1.0
    @2656 H12U_6   1.0
    @2657 H12V_6   1.0
    @2658 H12W_6   1.0
    @2659 H12X_6   1.0
    @2660 H12Y_6   1.0
    @2661 H12Z_6   1.0
    @2662 H13_6    1.0
    @2663 H14_6    1.0
    @2664 H15_6    1.0
    @2665 H15A_6   1.0
    @2666 H15B_6   1.0
    @2667 H15C_6   1.0
    @2668 H15D_6   1.0
    @2669 H15E_6   1.0
    @2670 H15F_6   1.0
    @2671 H15G_6   1.0
    @2672 H15H_6   1.0
    @2673 H16_6    1.0
    @2674 H18_6    1.0
    @2675 H20_6    1.0
    @2676 H21A_6   1.0
    @2677 H21_6    1.0
    @2678 H22_6    1.0
    @2679 H31_6    1.0
    @2680 H31B_6   1.0
    @2681 H32A_6   1.0
    @2682 H32B_6   1.0
    @2683 H32C_6   1.0
    @2684 H32D_6   1.0
    @2685 H32E_6   1.0
    @2686 H32F_6   1.0
    @2687 H32G_6   1.0
    @2688 H32H_6   1.0
    @2689 H32I_6   1.0
    @2690 H32J_6   1.0
    @2691 H32K_6   1.0
    @2692 H32L_6   1.0
    @2693 H32M_6   1.0
    @2694 H32N_6   1.0
    @2695 H32O_6   1.0
    @2696 H32P_6   1.0
    @2697 H32Q_6   1.0
    @2698 H32R_6   1.0
    @2699 H32S_6   1.0
    @2700 H32T_6   1.0
    @2701 H32U_6   1.0
    @2702 H32V_6   1.0
    @2703 H32W_6   1.0
    @2704 H32X_6   1.0
    @2705 H32Y_6   1.0
    @2706 H32Z_6   1.0
    @2707 HWIDX_1  1.0
    @2708 HW1_1    2.0
    @2710 HW2_1    3.0
    @2713 HW3_1    4.0
    @2717 HW4_1    4.0
    @2721 HW5_1    4.0
    @2725 HW6_1    5.0
    @2730 HW7_1    4.0
    @2734 HW8_1    4.0
    @2738 HW9_1    5.0
    @2743 HW10_1   4.0
    @2747 HW11_1   4.0
    @2751 HW12_1   5.0
    @2756 HW13_1   1.0
    @2757 HW14_1   1.0
    @2758 HW15_1   1.0
    @2759 HW16_1   2.0
    @2761 HW17_1   2.0
    @2763 HW18_1   2.0
    @2765 HW19_1   2.0
    @2767 HW20_1   3.0
    @2770 HW21_1   4.0
    @2774 HW22_1   4.0
    @2778 HW23_1   5.0
    @2783 HW24_1   4.0
    @2787 HW25_1   4.0
    @2791 HW26_1   5.0
    @2796 HWIDX_2  1.0
    @2797 HW1_2    2.0
    @2799 HW2_2    3.0
    @2802 HW3_2    4.0
    @2806 HW4_2    4.0
    @2810 HW5_2    4.0
    @2814 HW6_2    5.0
    @2819 HW7_2    4.0
    @2823 HW8_2    4.0
    @2827 HW9_2    5.0
    @2832 HW10_2   4.0
    @2836 HW11_2   4.0
    @2840 HW12_2   5.0
    @2845 HW13_2   1.0
    @2846 HW14_2   1.0
    @2847 HW15_2   1.0
    @2848 HW16_2   2.0
    @2850 HW17_2   2.0
    @2852 HW18_2   2.0
    @2854 HW19_2   2.0
    @2856 HW20_2   3.0
    @2859 HW21_2   4.0
    @2863 HW22_2   4.0
    @2867 HW23_2   5.0
    @2872 HW24_2   4.0
    @2876 HW25_2   4.0
    @2880 HW26_2   5.0
    @2885 HWIDX_3  1.0
    @2886 HW1_3    2.0
    @2888 HW2_3    3.0
    @2891 HW3_3    4.0
    @2895 HW4_3    4.0
    @2899 HW5_3    4.0
    @2903 HW6_3    5.0
    @2908 HW7_3    4.0
    @2912 HW8_3    4.0
    @2916 HW9_3    5.0
    @2921 HW10_3   4.0
    @2925 HW11_3   4.0
    @2929 HW12_3   5.0
    @2934 HW13_3   1.0
    @2935 HW14_3   1.0
    @2936 HW15_3   1.0
    @2937 HW16_3   2.0
    @2939 HW17_3   2.0
    @2941 HW18_3   2.0
    @2943 HW19_3   2.0
    @2945 HW20_3   3.0
    @2948 HW21_3   4.0
    @2952 HW22_3   4.0
    @2956 HW23_3   5.0
    @2961 HW24_3   4.0
    @2965 HW25_3   4.0
    @2969 HW26_3   5.0
    @2974 HWIDX_4  1.0
    @2975 HW1_4    2.0
    @2977 HW2_4    3.0
    @2980 HW3_4    4.0
    @2984 HW4_4    4.0
    @2988 HW5_4    4.0
    @2992 HW6_4    5.0
    @2997 HW7_4    4.0
    @3001 HW8_4    4.0
    @3005 HW9_4    5.0
    @3010 HW10_4   4.0
    @3014 HW11_4   4.0
    @3018 HW12_4   5.0
    @3023 HW13_4   1.0
    @3024 HW14_4   1.0
    @3025 HW15_4   1.0
    @3026 HW16_4   2.0
    @3028 HW17_4   2.0
    @3030 HW18_4   2.0
    @3032 HW19_4   2.0
    @3034 HW20_4   3.0
    @3037 HW21_4   4.0
    @3041 HW22_4   4.0
    @3045 HW23_4   5.0
    @3050 HW24_4   4.0
    @3054 HW25_4   4.0
    @3058 HW26_4   5.0
    @3063 HWIDX_5  1.0
    @3064 HW1_5    2.0
    @3066 HW2_5    3.0
    @3069 HW3_5    4.0
    @3073 HW4_5    4.0
    @3077 HW5_5    4.0
    @3081 HW6_5    5.0
    @3086 HW7_5    4.0
    @3090 HW8_5    4.0
    @3094 HW9_5    5.0
    @3099 HW10_5   4.0
    @3103 HW11_5   4.0
    @3107 HW12_5   5.0
    @3112 HW13_5   1.0
    @3113 HW14_5   1.0
    @3114 HW15_5   1.0
    @3115 HW16_5   2.0
    @3117 HW17_5   2.0
    @3119 HW18_5   2.0
    @3121 HW19_5   2.0
    @3123 HW20_5   3.0
    @3126 HW21_5   4.0
    @3130 HW22_5   4.0
    @3134 HW23_5   5.0
    @3139 HW24_5   4.0
    @3143 HW25_5   4.0
    @3147 HW26_5   5.0
    @3152 HWIDX_6  1.0
    @3153 HW1_6    2.0
    @3155 HW2_6    3.0
    @3158 HW3_6    4.0
    @3162 HW4_6    4.0
    @3166 HW5_6    4.0
    @3170 HW6_6    5.0
    @3175 HW7_6    4.0
    @3179 HW8_6    4.0
    @3183 HW9_6    5.0
    @3188 HW10_6   4.0
    @3192 HW11_6   4.0
    @3196 HW12_6   5.0
    @3201 HW13_6   1.0
    @3202 HW14_6   1.0
    @3203 HW15_6   1.0
    @3204 HW16_6   2.0
    @3206 HW17_6   2.0
    @3208 HW18_6   2.0
    @3210 HW19_6   2.0
    @3212 HW20_6   3.0
    @3215 HW21_6   4.0
    @3219 HW22_6   4.0
    @3223 HW23_6   5.0
    @3228 HW24_6   4.0
    @3232 HW25_6   4.0
    @3236 HW26_6   5.0
    @3241 V501     1.0
    @3242 V502     1.0
    @3243 V503     1.0
    @3244 V504     1.0
    @3245 V505     2.0
    @3247 V506     2.0
    @3249 V507     2.0
    @3251 V508     2.0
    @3253 V509     4.0
    @3257 V510     1.0
    @3258 V511     2.0
    @3260 V512     2.0
    @3262 V513     1.0
    @3263 V525     2.0
    @3265 V527     3.0
    @3268 V528     2.0
    @3270 V529     3.0
    @3273 V530     1.0
    @3274 V531     2.0
    @3276 V532     1.0
    @3277 V534     1.0
    @3278 V535     1.0
    @3279 V602     1.0
    @3280 V603     3.0
    @3283 V604     1.0
    @3284 V605     1.0
    @3285 V606     1.0
    @3286 V610     1.0
    @3287 V611     1.0
    @3288 V612     1.0
    @3289 V613     2.0
    @3291 V614     1.0
    @3292 V616     3.0
    @3295 V618     1.0
    @3296 V621     1.0
    @3297 V623     1.0
    @3298 V624     2.0
    @3300 V625     1.0
    @3301 V626     2.0
    @3303 V627     2.0
    @3305 V628     2.0
    @3307 V629     2.0
    @3309 V630A    1.0
    @3310 V630B    1.0
    @3311 V630C    1.0
    @3312 V630D    1.0
    @3313 V630E    1.0
    @3314 V630F    1.0
    @3315 V630G    1.0
    @3316 V630H    1.0
    @3317 V630I    1.0
    @3318 V630J    1.0
    @3319 V630K    1.0
    @3320 V630L    1.0
    @3321 V630X    1.0
    @3322 V701     1.0
    @3323 V702     2.0
    @3325 V704     3.0
    @3328 V705     2.0
    @3330 V707     1.0
    @3331 V714     1.0
    @3332 V715     2.0
    @3334 V716     3.0
    @3337 V717     2.0
    @3339 V718     1.0
    @3340 V719     1.0
    @3341 V720     1.0
    @3342 V721     1.0
    @3343 V723     2.0
    @3345 V729     1.0
    @3346 V730     2.0
    @3348 V731     1.0
    @3349 V732     1.0
    @3350 V733     2.0
    @3352 V734     1.0
    @3353 V735     3.0
    @3356 V736     8.0
    @3364 V737     7.0
    @3371 V738     7.0
    @3378 V739     1.0
    @3379 V740     1.0
    @3380 V751     1.0
    @3381 V752A    1.0
    @3382 V752B    1.0
    @3383 V752C    1.0
    @3384 V752D    1.0
    @3385 V752E    1.0
    @3386 V752F    1.0
    @3387 V752G    1.0
    @3388 V752H    1.0
    @3389 V752I    1.0
    @3390 V752J    1.0
    @3391 V752K    1.0
    @3392 V752L    1.0
    @3393 V752M    1.0
    @3394 V752N    1.0
    @3395 V752O    1.0
    @3396 V752X    1.0
    @3397 V753     1.0
    @3398 V754A    1.0
    @3399 V754B    1.0
    @3400 V754C    1.0
    @3401 V754D    1.0
    @3402 V754E    1.0
    @3403 V754F    1.0
    @3404 V754G    1.0
    @3405 V754H    1.0
    @3406 V754I    1.0
    @3407 V754J    1.0
    @3408 V754K    1.0
    @3409 V754L    1.0
    @3410 V754M    1.0
    @3411 V754N    1.0
    @3412 V754O    1.0
    @3413 V754X    1.0
    @3414 V754Z    1.0
    @3415 V755B    1.0
    @3416 V755C    1.0
    @3417 V755D    1.0
    @3418 V755E    1.0
    @3419 V755F    1.0
    @3420 V755L    1.0
    @3421 V755M    1.0
    @3422 V755N    1.0
    @3423 V755O    1.0
    @3424 V755X    1.0
    @3425 V755Z    1.0
    @3426 V756     1.0
    @3427 V757     1.0
    @3428 V758     1.0
    @3429 V760A    1.0
    @3430 V760B    1.0
    @3431 V760C    1.0
    @3432 V760D    1.0
    @3433 V760E    1.0
    @3434 V760F    1.0
    @3435 V760G    1.0
    @3436 V760I    1.0
    @3437 V760L    1.0
    @3438 V760M    1.0
    @3439 V760N    1.0
    @3440 V760O    1.0
    @3441 V760P    1.0
    @3442 V760V    1.0
    @3443 V760W    1.0
    @3444 V760X    1.0
    @3445 V760Y    1.0
    @3446 V760Z    1.0
    @3447 V761     1.0
    @3448 V762     2.0
    @3450 V764     1.0
    @3451 V765     1.0
    @3452 V801     4.0
    @3456 V802     4.0
    @3460 V803     2.0
    @3462 V804     1.0
    @3463 V805     3.0
    @3466 V806     2.0
    @3468 V811     1.0
    @3469 V812     1.0
    @3470 V813     1.0
    @3471 V814     1.0
    @3472 VCOL_1   1.0
    @3473 VCAL_1   $80.
    @3553 VCOL_2   1.0
    @3554 VCAL_2   $80.
    @3634 VCOL_3   1.0
    @3635 VCAL_3   $80.
    @3715 VCOL_4   1.0
    @3716 VCAL_4   $80.
    @3796 VCOL_5   1.0
    @3797 VCAL_5   $80.
    @3877 SDFORMAT 1.0
    @3878 SADJUST  1.0
    @3879 SINTM    2.0
    @3881 SINTY    2.0
    @3883 SDIV     1.0
    @3884 SDISTR   2.0
    @3886 S114A    1.0
    @3887 S114B    1.0
    @3888 S114C    1.0
    @3889 S114D    1.0
    @3890 S114E    1.0
    @3891 S119     3.0
    @3894 S119A    3.0
    @3897 S123     2.0
    @3899 S124A    1.0
    @3900 S124B    1.0
    @3901 S124C    1.0
    @3902 S124D    1.0
    @3903 S124I    1.0
    @3904 S229M    2.0
    @3906 S229Y    4.0
    @3910 S233A    1.0
    @3911 S233B    1.0
    @3912 S233C    1.0
    @3913 S233DM   2.0
    @3915 S233DY   4.0
    @3919 S304_01  1.0
    @3920 S304_02  1.0
    @3921 S304_03  1.0
    @3922 S304_04  1.0
    @3923 S304_05  1.0
    @3924 S304_06  1.0
    @3925 S304_07  1.0
    @3926 S304_08  1.0
    @3927 S304_09  1.0
    @3928 S304_10  1.0
    @3929 S304_11  1.0
    @3930 S304_12  1.0
    @3931 S304_13  1.0
    @3932 S304_14  1.0
    @3933 S304_15  1.0
    @3934 S309     2.0
    @3936 S310     2.0
    @3938 S316     1.0
    @3939 S317     2.0
    @3941 S318B    1.0
    @3942 S318C    2.0
    @3944 S318EA   1.0
    @3945 S318EB   1.0
    @3946 S318EC   1.0
    @3947 S318ED   1.0
    @3948 S318EE   1.0
    @3949 S318EF   1.0
    @3950 S318EG   1.0
    @3951 S318F    2.0
    @3953 S318HA   1.0
    @3954 S318HB   1.0
    @3955 S318HC   1.0
    @3956 S318HD   1.0
    @3957 S318HE   1.0
    @3958 S318HF   1.0
    @3959 S318HG   1.0
    @3960 S318HH   1.0
    @3961 S318HI   1.0
    @3962 S318I    1.0
    @3963 S318K    1.0
    @3964 S319     2.0
    @3966 S319B    1.0
    @3967 S320S    1.0
    @3968 S320     2.0
    @3970 S320A    1.0
    @3971 S320C    1.0
    @3972 S322C    2.0
    @3974 S320D    2.0
    @3976 S322D    2.0
    @3978 S323B    1.0
    @3979 S325     1.0
    @3980 S325A    1.0
    @3981 S325B    1.0
    @3982 S325C    1.0
    @3983 S325E    1.0
    @3984 S325G    1.0
    @3985 S325H    1.0
    @3986 S325I    2.0
    @3988 S325J    1.0
    @3989 S326     2.0
    @3991 S327     1.0
    @3992 S328PA   1.0
    @3993 S328PB   1.0
    @3994 S328PC   1.0
    @3995 S328PD   1.0
    @3996 S328PE   1.0
    @3997 S328PF   1.0
    @3998 S328PG   1.0
    @3999 S328PH   1.0
    @4000 S328PI   1.0
    @4001 S328PJ   1.0
    @4002 S328PK   1.0
    @4003 S328A    1.0
    @4004 S328B    1.0
    @4005 S328C    1.0
    @4006 S328DA   1.0
    @4007 S328DB   1.0
    @4008 S328DC   1.0
    @4009 S328DD   1.0
    @4010 S328DE   1.0
    @4011 S328DF   1.0
    @4012 S328DG   1.0
    @4013 S328DH   1.0
    @4014 S328DI   1.0
    @4015 S347A    1.0
    @4016 S347B    1.0
    @4017 S347CA   1.0
    @4018 S347CB   1.0
    @4019 S347CC   1.0
    @4020 S347CD   1.0
    @4021 S347CE   1.0
    @4022 S348C    1.0
    @4023 S349A    1.0
    @4024 S349B    1.0
    @4025 S349C    1.0
    @4026 S349D    1.0
    @4027 S349E    1.0
    @4028 S352     2.0
    @4030 S353     1.0
    @4031 S354     1.0
    @4032 S355     1.0
    @4033 S356     2.0
    @4035 S357MA   1.0
    @4036 S357MB   1.0
    @4037 S357MC   1.0
    @4038 S357MD   1.0
    @4039 S357ME   1.0
    @4040 S357MF   1.0
    @4041 S357MG   1.0
    @4042 S357A    1.0
    @4043 S357B    1.0
    @4044 S358     1.0
    @4045 S359     1.0
    @4046 S361     1.0
    @4047 S439     1.0
    @4048 S440     2.0
    @4050 S441     2.0
    @4052 S442A    1.0
    @4053 S442B_1  2.0
    @4055 S442C_1  1.0
    @4056 S442DA_1 1.0
    @4057 S442DB_1 1.0
    @4058 S442DC_1 1.0
    @4059 S442DD_1 1.0
    @4060 S442DE_1 1.0
    @4061 S442DF_1 1.0
    @4062 S442DG_1 1.0
    @4063 S442DH_1 1.0
    @4064 S442DI_1 1.0
    @4065 S442DJ_1 1.0
    @4066 S442DK_1 1.0
    @4067 S442EM_1 2.0
    @4069 S442FM_1 2.0
    @4071 S442EM_1 2.0
    @4073 S442FM_1 2.0
    @4075 S442B_2  2.0
    @4077 S442C_2  1.0
    @4078 S442DA_2 1.0
    @4079 S442DB_2 1.0
    @4080 S442DC_2 1.0
    @4081 S442DD_2 1.0
    @4082 S442DE_2 1.0
    @4083 S442DF_2 1.0
    @4084 S442DG_2 1.0
    @4085 S442DH_2 1.0
    @4086 S442DI_2 1.0
    @4087 S442DJ_2 1.0
    @4088 S442DK_2 1.0
    @4089 S442EM_2 2.0
    @4091 S442FM_2 2.0
    @4093 S442EM_2 2.0
    @4095 S442FM_2 2.0
    @4097 S442H    2.0
    @4099 S442IA   1.0
    @4100 S442IB   1.0
    @4101 S442IC   1.0
    @4102 S442ID   1.0
    @4103 S442IE   1.0
    @4104 S442IF   1.0
    @4105 S442IG   1.0
    @4106 S442J    2.0
    @4108 S442KA   1.0
    @4109 S442KB   1.0
    @4110 S442KC   1.0
    @4111 S442KD   1.0
    @4112 S442KE   1.0
    @4113 S442KF   1.0
    @4114 S442L    1.0
    @4115 S442MA   1.0
    @4116 S442MB   1.0
    @4117 S442MC   1.0
    @4118 S442MD   1.0
    @4119 S442ME   1.0
    @4120 S442MF   1.0
    @4121 S442MG   1.0
    @4122 S442MH   1.0
    @4123 S442MI   1.0
    @4124 S442N    1.0
    @4125 S442O    2.0
    @4127 S609     2.0
    @4129 S613     1.0
    @4130 S615     1.0
    @4131 S616     1.0
    @4132 S617     1.0
    @4133 S618     1.0
    ;
    if V015     =      9 then V015 = .;
    if V026     =      9 then V026 = .;
    if V028     =    999 then V028 = .;
    if V029     =     99 then V029 = .;
    if V030     =     99 then V030 = .;
    if V031     =     99 then V031 = .;
    if V032     =     99 then V032 = .;
    if V034     =     99 then V034 = .;
    if V103     =      9 then V103 = .;
    if V104     =     99 then V104 = .;
    if V105     =      9 then V105 = .;
    if V106     =      9 then V106 = .;
    if V107     =     99 then V107 = .;
    if V108     =      9 then V108 = .;
    if V109     =      9 then V109 = .;
    if V110     =      9 then V110 = .;
    if V111     =      9 then V111 = .;
    if V112     =      9 then V112 = .;
    if V113     =     99 then V113 = .;
    if V114     =     99 then V114 = .;
    if V115     =    999 then V115 = .;
    if V116     =     99 then V116 = .;
    if V119     =      9 then V119 = .;
    if V120     =      9 then V120 = .;
    if V121     =      9 then V121 = .;
    if V122     =      9 then V122 = .;
    if V123     =      9 then V123 = .;
    if V124     =      9 then V124 = .;
    if V125     =      9 then V125 = .;
    if V127     =     99 then V127 = .;
    if V128     =     99 then V128 = .;
    if V129     =     99 then V129 = .;
    if V130     =      9 then V130 = .;
    if V131     =     99 then V131 = .;
    if V133     =     99 then V133 = .;
    if V134     =      9 then V134 = .;
    if V135     =      9 then V135 = .;
    if V139     =     99 then V139 = .;
    if V140     =      9 then V140 = .;
    if V141     =      9 then V141 = .;
    if V142     =      9 then V142 = .;
    if V144     =      9 then V144 = .;
    if V148     =      9 then V148 = .;
    if V149     =      9 then V149 = .;
    if V150     =     99 then V150 = .;
    if V152     =     99 then V152 = .;
    if V153     =      9 then V153 = .;
    if V154     =     99 then V154 = .;
    if B6_01    =    999 then B6_01 = .;
    if B9_01    =      9 then B9_01 = .;
    if B14_01   =      9 then B14_01 = .;
    if B15_01   =      9 then B15_01 = .;
    if B6_02    =    999 then B6_02 = .;
    if B9_02    =      9 then B9_02 = .;
    if B14_02   =      9 then B14_02 = .;
    if B15_02   =      9 then B15_02 = .;
    if B6_03    =    999 then B6_03 = .;
    if B9_03    =      9 then B9_03 = .;
    if B14_03   =      9 then B14_03 = .;
    if B15_03   =      9 then B15_03 = .;
    if B6_04    =    999 then B6_04 = .;
    if B9_04    =      9 then B9_04 = .;
    if B14_04   =      9 then B14_04 = .;
    if B15_04   =      9 then B15_04 = .;
    if B6_05    =    999 then B6_05 = .;
    if B9_05    =      9 then B9_05 = .;
    if B14_05   =      9 then B14_05 = .;
    if B15_05   =      9 then B15_05 = .;
    if B6_06    =    999 then B6_06 = .;
    if B9_06    =      9 then B9_06 = .;
    if B14_06   =      9 then B14_06 = .;
    if B15_06   =      9 then B15_06 = .;
    if B6_07    =    999 then B6_07 = .;
    if B9_07    =      9 then B9_07 = .;
    if B14_07   =      9 then B14_07 = .;
    if B15_07   =      9 then B15_07 = .;
    if B6_08    =    999 then B6_08 = .;
    if B9_08    =      9 then B9_08 = .;
    if B14_08   =      9 then B14_08 = .;
    if B15_08   =      9 then B15_08 = .;
    if B6_09    =    999 then B6_09 = .;
    if B9_09    =      9 then B9_09 = .;
    if B14_09   =      9 then B14_09 = .;
    if B15_09   =      9 then B15_09 = .;
    if B6_10    =    999 then B6_10 = .;
    if B9_10    =      9 then B9_10 = .;
    if B14_10   =      9 then B14_10 = .;
    if B15_10   =      9 then B15_10 = .;
    if B6_11    =    999 then B6_11 = .;
    if B9_11    =      9 then B9_11 = .;
    if B14_11   =      9 then B14_11 = .;
    if B15_11   =      9 then B15_11 = .;
    if B6_12    =    999 then B6_12 = .;
    if B9_12    =      9 then B9_12 = .;
    if B14_12   =      9 then B14_12 = .;
    if B15_12   =      9 then B15_12 = .;
    if B6_13    =    999 then B6_13 = .;
    if B9_13    =      9 then B9_13 = .;
    if B14_13   =      9 then B14_13 = .;
    if B15_13   =      9 then B15_13 = .;
    if B6_14    =    999 then B6_14 = .;
    if B9_14    =      9 then B9_14 = .;
    if B14_14   =      9 then B14_14 = .;
    if B15_14   =      9 then B15_14 = .;
    if B6_15    =    999 then B6_15 = .;
    if B9_15    =      9 then B9_15 = .;
    if B14_15   =      9 then B14_15 = .;
    if B15_15   =      9 then B15_15 = .;
    if B6_16    =    999 then B6_16 = .;
    if B9_16    =      9 then B9_16 = .;
    if B14_16   =      9 then B14_16 = .;
    if B15_16   =      9 then B15_16 = .;
    if B6_17    =    999 then B6_17 = .;
    if B9_17    =      9 then B9_17 = .;
    if B14_17   =      9 then B14_17 = .;
    if B15_17   =      9 then B15_17 = .;
    if B6_18    =    999 then B6_18 = .;
    if B9_18    =      9 then B9_18 = .;
    if B14_18   =      9 then B14_18 = .;
    if B15_18   =      9 then B15_18 = .;
    if B6_19    =    999 then B6_19 = .;
    if B9_19    =      9 then B9_19 = .;
    if B14_19   =      9 then B14_19 = .;
    if B15_19   =      9 then B15_19 = .;
    if B6_20    =    999 then B6_20 = .;
    if B9_20    =      9 then B9_20 = .;
    if B14_20   =      9 then B14_20 = .;
    if B15_20   =      9 then B15_20 = .;
    if V215     =    999 then V215 = .;
    if V225     =      9 then V225 = .;
    if V226     =    999 then V226 = .;
    if V228     =      9 then V228 = .;
    if V229     =     99 then V229 = .;
    if V230     =     99 then V230 = .;
    if V231     =   9999 then V231 = .;
    if V233     =     99 then V233 = .;
    if V234     =      9 then V234 = .;
    if V304_01  =      9 then V304_01 = .;
    if V305_01  =      9 then V305_01 = .;
    if V304_02  =      9 then V304_02 = .;
    if V305_02  =      9 then V305_02 = .;
    if V304_03  =      9 then V304_03 = .;
    if V305_03  =      9 then V305_03 = .;
    if V304_04  =      9 then V304_04 = .;
    if V305_04  =      9 then V305_04 = .;
    if V304_05  =      9 then V304_05 = .;
    if V305_05  =      9 then V305_05 = .;
    if V304_06  =      9 then V304_06 = .;
    if V305_06  =      9 then V305_06 = .;
    if V304_07  =      9 then V304_07 = .;
    if V305_07  =      9 then V305_07 = .;
    if V304_08  =      9 then V304_08 = .;
    if V305_08  =      9 then V305_08 = .;
    if V304_09  =      9 then V304_09 = .;
    if V305_09  =      9 then V305_09 = .;
    if V304_10  =      9 then V304_10 = .;
    if V305_10  =      9 then V305_10 = .;
    if V304_11  =      9 then V304_11 = .;
    if V305_11  =      9 then V305_11 = .;
    if V304_12  =      9 then V304_12 = .;
    if V305_12  =      9 then V305_12 = .;
    if V304_13  =      9 then V304_13 = .;
    if V305_13  =      9 then V305_13 = .;
    if V304_14  =      9 then V304_14 = .;
    if V305_14  =      9 then V305_14 = .;
    if V304_15  =      9 then V304_15 = .;
    if V305_15  =      9 then V305_15 = .;
    if V310     =     99 then V310 = .;
    if V311     =      9 then V311 = .;
    if V312     =     99 then V312 = .;
    if V313     =      9 then V313 = .;
    if V314     =      9 then V314 = .;
    if V323     =     99 then V323 = .;
    if V325     = 999999 then V325 = .;
    if V326     =     99 then V326 = .;
    if V327     =      9 then V327 = .;
    if V328     =     99 then V328 = .;
    if V337     =    999 then V337 = .;
    if V359     =     99 then V359 = .;
    if V360     =     99 then V360 = .;
    if V362     =      9 then V362 = .;
    if V363     =     99 then V363 = .;
    if V366A    =      9 then V366A = .;
    if V367     =      9 then V367 = .;
    if V369     =     99 then V369 = .;
    if V372     =      9 then V372 = .;
    if V375A    =     99 then V375A = .;
    if V376     =     99 then V376 = .;
    if V376A    =      9 then V376A = .;
    if V379     =     99 then V379 = .;
    if V380     =      9 then V380 = .;
    if V384A    =      9 then V384A = .;
    if V384B    =      9 then V384B = .;
    if V384D    =      9 then V384D = .;
    if V393     =      9 then V393 = .;
    if V394     =      9 then V394 = .;
    if V395     =      9 then V395 = .;
    if V396     =      9 then V396 = .;
    if V399     =      9 then V399 = .;
    if M1_1     =      9 then M1_1 = .;
    if M2A_1    =      9 then M2A_1 = .;
    if M2B_1    =      9 then M2B_1 = .;
    if M2C_1    =      9 then M2C_1 = .;
    if M2D_1    =      9 then M2D_1 = .;
    if M2E_1    =      9 then M2E_1 = .;
    if M2F_1    =      9 then M2F_1 = .;
    if M2G_1    =      9 then M2G_1 = .;
    if M2H_1    =      9 then M2H_1 = .;
    if M2I_1    =      9 then M2I_1 = .;
    if M2J_1    =      9 then M2J_1 = .;
    if M2K_1    =      9 then M2K_1 = .;
    if M2N_1    =      9 then M2N_1 = .;
    if M3A_1    =      9 then M3A_1 = .;
    if M3B_1    =      9 then M3B_1 = .;
    if M3D_1    =      9 then M3D_1 = .;
    if M3F_1    =      9 then M3F_1 = .;
    if M3G_1    =      9 then M3G_1 = .;
    if M3H_1    =      9 then M3H_1 = .;
    if M3K_1    =      9 then M3K_1 = .;
    if M3M_1    =      9 then M3M_1 = .;
    if M3N_1    =      9 then M3N_1 = .;
    if M4_1     =     99 then M4_1 = .;
    if M5_1     =     99 then M5_1 = .;
    if M6_1     =     99 then M6_1 = .;
    if M7_1     =     99 then M7_1 = .;
    if M8_1     =     99 then M8_1 = .;
    if M9_1     =     99 then M9_1 = .;
    if M10_1    =      9 then M10_1 = .;
    if M13_1    =     99 then M13_1 = .;
    if M14_1    =     99 then M14_1 = .;
    if M15_1    =     99 then M15_1 = .;
    if M21_1    =     99 then M21_1 = .;
    if M34_1    =    999 then M34_1 = .;
    if M35_1    =     99 then M35_1 = .;
    if M36_1    =     99 then M36_1 = .;
    if M37A_1   =      9 then M37A_1 = .;
    if M37B_1   =      9 then M37B_1 = .;
    if M37C_1   =      9 then M37C_1 = .;
    if M37D_1   =      9 then M37D_1 = .;
    if M37E_1   =      9 then M37E_1 = .;
    if M37F_1   =      9 then M37F_1 = .;
    if M37G_1   =      9 then M37G_1 = .;
    if M37H_1   =      9 then M37H_1 = .;
    if M37I_1   =      9 then M37I_1 = .;
    if M37J_1   =      9 then M37J_1 = .;
    if M37K_1   =      9 then M37K_1 = .;
    if M37L_1   =      9 then M37L_1 = .;
    if M37P_1   =      9 then M37P_1 = .;
    if M37R_1   =      9 then M37R_1 = .;
    if M37T_1   =      9 then M37T_1 = .;
    if M39_1    =      9 then M39_1 = .;
    if M40L_1   =      9 then M40L_1 = .;
    if M1_2     =      9 then M1_2 = .;
    if M2A_2    =      9 then M2A_2 = .;
    if M2B_2    =      9 then M2B_2 = .;
    if M2C_2    =      9 then M2C_2 = .;
    if M2D_2    =      9 then M2D_2 = .;
    if M2E_2    =      9 then M2E_2 = .;
    if M2F_2    =      9 then M2F_2 = .;
    if M2G_2    =      9 then M2G_2 = .;
    if M2H_2    =      9 then M2H_2 = .;
    if M2I_2    =      9 then M2I_2 = .;
    if M2J_2    =      9 then M2J_2 = .;
    if M2K_2    =      9 then M2K_2 = .;
    if M2N_2    =      9 then M2N_2 = .;
    if M3A_2    =      9 then M3A_2 = .;
    if M3B_2    =      9 then M3B_2 = .;
    if M3D_2    =      9 then M3D_2 = .;
    if M3F_2    =      9 then M3F_2 = .;
    if M3G_2    =      9 then M3G_2 = .;
    if M3H_2    =      9 then M3H_2 = .;
    if M3K_2    =      9 then M3K_2 = .;
    if M3M_2    =      9 then M3M_2 = .;
    if M3N_2    =      9 then M3N_2 = .;
    if M4_2     =     99 then M4_2 = .;
    if M5_2     =     99 then M5_2 = .;
    if M6_2     =     99 then M6_2 = .;
    if M7_2     =     99 then M7_2 = .;
    if M8_2     =     99 then M8_2 = .;
    if M9_2     =     99 then M9_2 = .;
    if M10_2    =      9 then M10_2 = .;
    if M13_2    =     99 then M13_2 = .;
    if M14_2    =     99 then M14_2 = .;
    if M15_2    =     99 then M15_2 = .;
    if M21_2    =     99 then M21_2 = .;
    if M34_2    =    999 then M34_2 = .;
    if M35_2    =     99 then M35_2 = .;
    if M36_2    =     99 then M36_2 = .;
    if M37A_2   =      9 then M37A_2 = .;
    if M37B_2   =      9 then M37B_2 = .;
    if M37C_2   =      9 then M37C_2 = .;
    if M37D_2   =      9 then M37D_2 = .;
    if M37E_2   =      9 then M37E_2 = .;
    if M37F_2   =      9 then M37F_2 = .;
    if M37G_2   =      9 then M37G_2 = .;
    if M37H_2   =      9 then M37H_2 = .;
    if M37I_2   =      9 then M37I_2 = .;
    if M37J_2   =      9 then M37J_2 = .;
    if M37K_2   =      9 then M37K_2 = .;
    if M37L_2   =      9 then M37L_2 = .;
    if M37P_2   =      9 then M37P_2 = .;
    if M37R_2   =      9 then M37R_2 = .;
    if M37T_2   =      9 then M37T_2 = .;
    if M39_2    =      9 then M39_2 = .;
    if M40L_2   =      9 then M40L_2 = .;
    if M1_3     =      9 then M1_3 = .;
    if M2A_3    =      9 then M2A_3 = .;
    if M2B_3    =      9 then M2B_3 = .;
    if M2C_3    =      9 then M2C_3 = .;
    if M2D_3    =      9 then M2D_3 = .;
    if M2E_3    =      9 then M2E_3 = .;
    if M2F_3    =      9 then M2F_3 = .;
    if M2G_3    =      9 then M2G_3 = .;
    if M2H_3    =      9 then M2H_3 = .;
    if M2I_3    =      9 then M2I_3 = .;
    if M2J_3    =      9 then M2J_3 = .;
    if M2K_3    =      9 then M2K_3 = .;
    if M2N_3    =      9 then M2N_3 = .;
    if M3A_3    =      9 then M3A_3 = .;
    if M3B_3    =      9 then M3B_3 = .;
    if M3D_3    =      9 then M3D_3 = .;
    if M3F_3    =      9 then M3F_3 = .;
    if M3G_3    =      9 then M3G_3 = .;
    if M3H_3    =      9 then M3H_3 = .;
    if M3K_3    =      9 then M3K_3 = .;
    if M3M_3    =      9 then M3M_3 = .;
    if M3N_3    =      9 then M3N_3 = .;
    if M4_3     =     99 then M4_3 = .;
    if M5_3     =     99 then M5_3 = .;
    if M6_3     =     99 then M6_3 = .;
    if M7_3     =     99 then M7_3 = .;
    if M8_3     =     99 then M8_3 = .;
    if M9_3     =     99 then M9_3 = .;
    if M10_3    =      9 then M10_3 = .;
    if M13_3    =     99 then M13_3 = .;
    if M14_3    =     99 then M14_3 = .;
    if M15_3    =     99 then M15_3 = .;
    if M21_3    =     99 then M21_3 = .;
    if M34_3    =    999 then M34_3 = .;
    if M35_3    =     99 then M35_3 = .;
    if M36_3    =     99 then M36_3 = .;
    if M37A_3   =      9 then M37A_3 = .;
    if M37B_3   =      9 then M37B_3 = .;
    if M37C_3   =      9 then M37C_3 = .;
    if M37D_3   =      9 then M37D_3 = .;
    if M37E_3   =      9 then M37E_3 = .;
    if M37F_3   =      9 then M37F_3 = .;
    if M37G_3   =      9 then M37G_3 = .;
    if M37H_3   =      9 then M37H_3 = .;
    if M37I_3   =      9 then M37I_3 = .;
    if M37J_3   =      9 then M37J_3 = .;
    if M37K_3   =      9 then M37K_3 = .;
    if M37L_3   =      9 then M37L_3 = .;
    if M37P_3   =      9 then M37P_3 = .;
    if M37R_3   =      9 then M37R_3 = .;
    if M37T_3   =      9 then M37T_3 = .;
    if M39_3    =      9 then M39_3 = .;
    if M40L_3   =      9 then M40L_3 = .;
    if M1_4     =      9 then M1_4 = .;
    if M2A_4    =      9 then M2A_4 = .;
    if M2B_4    =      9 then M2B_4 = .;
    if M2C_4    =      9 then M2C_4 = .;
    if M2D_4    =      9 then M2D_4 = .;
    if M2E_4    =      9 then M2E_4 = .;
    if M2F_4    =      9 then M2F_4 = .;
    if M2G_4    =      9 then M2G_4 = .;
    if M2H_4    =      9 then M2H_4 = .;
    if M2I_4    =      9 then M2I_4 = .;
    if M2J_4    =      9 then M2J_4 = .;
    if M2K_4    =      9 then M2K_4 = .;
    if M2N_4    =      9 then M2N_4 = .;
    if M3A_4    =      9 then M3A_4 = .;
    if M3B_4    =      9 then M3B_4 = .;
    if M3D_4    =      9 then M3D_4 = .;
    if M3F_4    =      9 then M3F_4 = .;
    if M3G_4    =      9 then M3G_4 = .;
    if M3H_4    =      9 then M3H_4 = .;
    if M3K_4    =      9 then M3K_4 = .;
    if M3M_4    =      9 then M3M_4 = .;
    if M3N_4    =      9 then M3N_4 = .;
    if M4_4     =     99 then M4_4 = .;
    if M5_4     =     99 then M5_4 = .;
    if M6_4     =     99 then M6_4 = .;
    if M7_4     =     99 then M7_4 = .;
    if M8_4     =     99 then M8_4 = .;
    if M9_4     =     99 then M9_4 = .;
    if M10_4    =      9 then M10_4 = .;
    if M13_4    =     99 then M13_4 = .;
    if M14_4    =     99 then M14_4 = .;
    if M15_4    =     99 then M15_4 = .;
    if M21_4    =     99 then M21_4 = .;
    if M34_4    =    999 then M34_4 = .;
    if M35_4    =     99 then M35_4 = .;
    if M36_4    =     99 then M36_4 = .;
    if M37A_4   =      9 then M37A_4 = .;
    if M37B_4   =      9 then M37B_4 = .;
    if M37C_4   =      9 then M37C_4 = .;
    if M37D_4   =      9 then M37D_4 = .;
    if M37E_4   =      9 then M37E_4 = .;
    if M37F_4   =      9 then M37F_4 = .;
    if M37G_4   =      9 then M37G_4 = .;
    if M37H_4   =      9 then M37H_4 = .;
    if M37I_4   =      9 then M37I_4 = .;
    if M37J_4   =      9 then M37J_4 = .;
    if M37K_4   =      9 then M37K_4 = .;
    if M37L_4   =      9 then M37L_4 = .;
    if M37P_4   =      9 then M37P_4 = .;
    if M37R_4   =      9 then M37R_4 = .;
    if M37T_4   =      9 then M37T_4 = .;
    if M39_4    =      9 then M39_4 = .;
    if M40L_4   =      9 then M40L_4 = .;
    if M1_5     =      9 then M1_5 = .;
    if M2A_5    =      9 then M2A_5 = .;
    if M2B_5    =      9 then M2B_5 = .;
    if M2C_5    =      9 then M2C_5 = .;
    if M2D_5    =      9 then M2D_5 = .;
    if M2E_5    =      9 then M2E_5 = .;
    if M2F_5    =      9 then M2F_5 = .;
    if M2G_5    =      9 then M2G_5 = .;
    if M2H_5    =      9 then M2H_5 = .;
    if M2I_5    =      9 then M2I_5 = .;
    if M2J_5    =      9 then M2J_5 = .;
    if M2K_5    =      9 then M2K_5 = .;
    if M2N_5    =      9 then M2N_5 = .;
    if M3A_5    =      9 then M3A_5 = .;
    if M3B_5    =      9 then M3B_5 = .;
    if M3D_5    =      9 then M3D_5 = .;
    if M3F_5    =      9 then M3F_5 = .;
    if M3G_5    =      9 then M3G_5 = .;
    if M3H_5    =      9 then M3H_5 = .;
    if M3K_5    =      9 then M3K_5 = .;
    if M3M_5    =      9 then M3M_5 = .;
    if M3N_5    =      9 then M3N_5 = .;
    if M4_5     =     99 then M4_5 = .;
    if M5_5     =     99 then M5_5 = .;
    if M6_5     =     99 then M6_5 = .;
    if M7_5     =     99 then M7_5 = .;
    if M8_5     =     99 then M8_5 = .;
    if M9_5     =     99 then M9_5 = .;
    if M10_5    =      9 then M10_5 = .;
    if M13_5    =     99 then M13_5 = .;
    if M14_5    =     99 then M14_5 = .;
    if M15_5    =     99 then M15_5 = .;
    if M21_5    =     99 then M21_5 = .;
    if M34_5    =    999 then M34_5 = .;
    if M35_5    =     99 then M35_5 = .;
    if M36_5    =     99 then M36_5 = .;
    if M37A_5   =      9 then M37A_5 = .;
    if M37B_5   =      9 then M37B_5 = .;
    if M37C_5   =      9 then M37C_5 = .;
    if M37D_5   =      9 then M37D_5 = .;
    if M37E_5   =      9 then M37E_5 = .;
    if M37F_5   =      9 then M37F_5 = .;
    if M37G_5   =      9 then M37G_5 = .;
    if M37H_5   =      9 then M37H_5 = .;
    if M37I_5   =      9 then M37I_5 = .;
    if M37J_5   =      9 then M37J_5 = .;
    if M37K_5   =      9 then M37K_5 = .;
    if M37L_5   =      9 then M37L_5 = .;
    if M37P_5   =      9 then M37P_5 = .;
    if M37R_5   =      9 then M37R_5 = .;
    if M37T_5   =      9 then M37T_5 = .;
    if M39_5    =      9 then M39_5 = .;
    if M40L_5   =      9 then M40L_5 = .;
    if M1_6     =      9 then M1_6 = .;
    if M2A_6    =      9 then M2A_6 = .;
    if M2B_6    =      9 then M2B_6 = .;
    if M2C_6    =      9 then M2C_6 = .;
    if M2D_6    =      9 then M2D_6 = .;
    if M2E_6    =      9 then M2E_6 = .;
    if M2F_6    =      9 then M2F_6 = .;
    if M2G_6    =      9 then M2G_6 = .;
    if M2H_6    =      9 then M2H_6 = .;
    if M2I_6    =      9 then M2I_6 = .;
    if M2J_6    =      9 then M2J_6 = .;
    if M2K_6    =      9 then M2K_6 = .;
    if M2N_6    =      9 then M2N_6 = .;
    if M3A_6    =      9 then M3A_6 = .;
    if M3B_6    =      9 then M3B_6 = .;
    if M3D_6    =      9 then M3D_6 = .;
    if M3F_6    =      9 then M3F_6 = .;
    if M3G_6    =      9 then M3G_6 = .;
    if M3H_6    =      9 then M3H_6 = .;
    if M3K_6    =      9 then M3K_6 = .;
    if M3M_6    =      9 then M3M_6 = .;
    if M3N_6    =      9 then M3N_6 = .;
    if M4_6     =     99 then M4_6 = .;
    if M5_6     =     99 then M5_6 = .;
    if M6_6     =     99 then M6_6 = .;
    if M7_6     =     99 then M7_6 = .;
    if M8_6     =     99 then M8_6 = .;
    if M9_6     =     99 then M9_6 = .;
    if M10_6    =      9 then M10_6 = .;
    if M13_6    =     99 then M13_6 = .;
    if M14_6    =     99 then M14_6 = .;
    if M15_6    =     99 then M15_6 = .;
    if M21_6    =     99 then M21_6 = .;
    if M34_6    =    999 then M34_6 = .;
    if M35_6    =     99 then M35_6 = .;
    if M36_6    =     99 then M36_6 = .;
    if M37A_6   =      9 then M37A_6 = .;
    if M37B_6   =      9 then M37B_6 = .;
    if M37C_6   =      9 then M37C_6 = .;
    if M37D_6   =      9 then M37D_6 = .;
    if M37E_6   =      9 then M37E_6 = .;
    if M37F_6   =      9 then M37F_6 = .;
    if M37G_6   =      9 then M37G_6 = .;
    if M37H_6   =      9 then M37H_6 = .;
    if M37I_6   =      9 then M37I_6 = .;
    if M37J_6   =      9 then M37J_6 = .;
    if M37K_6   =      9 then M37K_6 = .;
    if M37L_6   =      9 then M37L_6 = .;
    if M37P_6   =      9 then M37P_6 = .;
    if M37R_6   =      9 then M37R_6 = .;
    if M37T_6   =      9 then M37T_6 = .;
    if M39_6    =      9 then M39_6 = .;
    if M40L_6   =      9 then M40L_6 = .;
    if V401     =      9 then V401 = .;
    if V407     =     99 then V407 = .;
    if V408     =     99 then V408 = .;
    if V409     =      9 then V409 = .;
    if V409A    =      9 then V409A = .;
    if V410     =      9 then V410 = .;
    if V410A    =      9 then V410A = .;
    if V411     =      9 then V411 = .;
    if V411A    =      9 then V411A = .;
    if V412     =      9 then V412 = .;
    if V413A    =      9 then V413A = .;
    if V413B    =      9 then V413B = .;
    if V413C    =      9 then V413C = .;
    if V413D    =      9 then V413D = .;
    if V413     =      9 then V413 = .;
    if V414A    =      9 then V414A = .;
    if V414B    =      9 then V414B = .;
    if V414C    =      9 then V414C = .;
    if V414D    =      9 then V414D = .;
    if V414E    =      9 then V414E = .;
    if V414F    =      9 then V414F = .;
    if V414G    =      9 then V414G = .;
    if V414H    =      9 then V414H = .;
    if V416     =      9 then V416 = .;
    if V426     =    999 then V426 = .;
    if V450B    =      9 then V450B = .;
    if V450E    =      9 then V450E = .;
    if V450F    =      9 then V450F = .;
    if V450G    =      9 then V450G = .;
    if H1_1     =      9 then H1_1 = .;
    if H2_1     =      9 then H2_1 = .;
    if H2D_1    =     99 then H2D_1 = .;
    if H2M_1    =     99 then H2M_1 = .;
    if H2Y_1    =     99 then H2Y_1 = .;
    if H3_1     =      9 then H3_1 = .;
    if H3D_1    =     99 then H3D_1 = .;
    if H3M_1    =     99 then H3M_1 = .;
    if H3Y_1    =     99 then H3Y_1 = .;
    if H4_1     =      9 then H4_1 = .;
    if H4D_1    =     99 then H4D_1 = .;
    if H4M_1    =     99 then H4M_1 = .;
    if H4Y_1    =     99 then H4Y_1 = .;
    if H5_1     =      9 then H5_1 = .;
    if H5D_1    =     99 then H5D_1 = .;
    if H5M_1    =     99 then H5M_1 = .;
    if H5Y_1    =     99 then H5Y_1 = .;
    if H6_1     =      9 then H6_1 = .;
    if H6D_1    =     99 then H6D_1 = .;
    if H6M_1    =     99 then H6M_1 = .;
    if H6Y_1    =     99 then H6Y_1 = .;
    if H7_1     =      9 then H7_1 = .;
    if H7D_1    =     99 then H7D_1 = .;
    if H7M_1    =     99 then H7M_1 = .;
    if H7Y_1    =     99 then H7Y_1 = .;
    if H8_1     =      9 then H8_1 = .;
    if H8D_1    =     99 then H8D_1 = .;
    if H8M_1    =     99 then H8M_1 = .;
    if H8Y_1    =     99 then H8Y_1 = .;
    if H9_1     =      9 then H9_1 = .;
    if H9D_1    =     99 then H9D_1 = .;
    if H9M_1    =     99 then H9M_1 = .;
    if H9Y_1    =     99 then H9Y_1 = .;
    if H0D_1    =     99 then H0D_1 = .;
    if H0M_1    =     99 then H0M_1 = .;
    if H0Y_1    =     99 then H0Y_1 = .;
    if H10_1    =      9 then H10_1 = .;
    if H11_1    =      9 then H11_1 = .;
    if H11B_1   =      9 then H11B_1 = .;
    if H12A_1   =      9 then H12A_1 = .;
    if H12B_1   =      9 then H12B_1 = .;
    if H12D_1   =      9 then H12D_1 = .;
    if H12E_1   =      9 then H12E_1 = .;
    if H12F_1   =      9 then H12F_1 = .;
    if H12G_1   =      9 then H12G_1 = .;
    if H12J_1   =      9 then H12J_1 = .;
    if H12K_1   =      9 then H12K_1 = .;
    if H12L_1   =      9 then H12L_1 = .;
    if H12S_1   =      9 then H12S_1 = .;
    if H12T_1   =      9 then H12T_1 = .;
    if H12X_1   =      9 then H12X_1 = .;
    if H12Y_1   =      9 then H12Y_1 = .;
    if H12Z_1   =      9 then H12Z_1 = .;
    if H13_1    =      9 then H13_1 = .;
    if H14_1    =      9 then H14_1 = .;
    if H15_1    =      9 then H15_1 = .;
    if H15A_1   =      9 then H15A_1 = .;
    if H15B_1   =      9 then H15B_1 = .;
    if H15C_1   =      9 then H15C_1 = .;
    if H15D_1   =      9 then H15D_1 = .;
    if H15E_1   =      9 then H15E_1 = .;
    if H15F_1   =      9 then H15F_1 = .;
    if H16_1    =      9 then H16_1 = .;
    if H20_1    =      9 then H20_1 = .;
    if H21A_1   =      9 then H21A_1 = .;
    if H21_1    =      9 then H21_1 = .;
    if H31_1    =      9 then H31_1 = .;
    if H31B_1   =      9 then H31B_1 = .;
    if H32A_1   =      9 then H32A_1 = .;
    if H32B_1   =      9 then H32B_1 = .;
    if H32C_1   =      9 then H32C_1 = .;
    if H32D_1   =      9 then H32D_1 = .;
    if H32E_1   =      9 then H32E_1 = .;
    if H32F_1   =      9 then H32F_1 = .;
    if H32G_1   =      9 then H32G_1 = .;
    if H32H_1   =      9 then H32H_1 = .;
    if H32I_1   =      9 then H32I_1 = .;
    if H32J_1   =      9 then H32J_1 = .;
    if H32K_1   =      9 then H32K_1 = .;
    if H32L_1   =      9 then H32L_1 = .;
    if H32M_1   =      9 then H32M_1 = .;
    if H32N_1   =      9 then H32N_1 = .;
    if H32O_1   =      9 then H32O_1 = .;
    if H32P_1   =      9 then H32P_1 = .;
    if H32Q_1   =      9 then H32Q_1 = .;
    if H32R_1   =      9 then H32R_1 = .;
    if H32S_1   =      9 then H32S_1 = .;
    if H32T_1   =      9 then H32T_1 = .;
    if H32U_1   =      9 then H32U_1 = .;
    if H32V_1   =      9 then H32V_1 = .;
    if H32W_1   =      9 then H32W_1 = .;
    if H32X_1   =      9 then H32X_1 = .;
    if H32Y_1   =      9 then H32Y_1 = .;
    if H32Z_1   =      9 then H32Z_1 = .;
    if H1_2     =      9 then H1_2 = .;
    if H2_2     =      9 then H2_2 = .;
    if H2D_2    =     99 then H2D_2 = .;
    if H2M_2    =     99 then H2M_2 = .;
    if H2Y_2    =     99 then H2Y_2 = .;
    if H3_2     =      9 then H3_2 = .;
    if H3D_2    =     99 then H3D_2 = .;
    if H3M_2    =     99 then H3M_2 = .;
    if H3Y_2    =     99 then H3Y_2 = .;
    if H4_2     =      9 then H4_2 = .;
    if H4D_2    =     99 then H4D_2 = .;
    if H4M_2    =     99 then H4M_2 = .;
    if H4Y_2    =     99 then H4Y_2 = .;
    if H5_2     =      9 then H5_2 = .;
    if H5D_2    =     99 then H5D_2 = .;
    if H5M_2    =     99 then H5M_2 = .;
    if H5Y_2    =     99 then H5Y_2 = .;
    if H6_2     =      9 then H6_2 = .;
    if H6D_2    =     99 then H6D_2 = .;
    if H6M_2    =     99 then H6M_2 = .;
    if H6Y_2    =     99 then H6Y_2 = .;
    if H7_2     =      9 then H7_2 = .;
    if H7D_2    =     99 then H7D_2 = .;
    if H7M_2    =     99 then H7M_2 = .;
    if H7Y_2    =     99 then H7Y_2 = .;
    if H8_2     =      9 then H8_2 = .;
    if H8D_2    =     99 then H8D_2 = .;
    if H8M_2    =     99 then H8M_2 = .;
    if H8Y_2    =     99 then H8Y_2 = .;
    if H9_2     =      9 then H9_2 = .;
    if H9D_2    =     99 then H9D_2 = .;
    if H9M_2    =     99 then H9M_2 = .;
    if H9Y_2    =     99 then H9Y_2 = .;
    if H0D_2    =     99 then H0D_2 = .;
    if H0M_2    =     99 then H0M_2 = .;
    if H0Y_2    =     99 then H0Y_2 = .;
    if H10_2    =      9 then H10_2 = .;
    if H11_2    =      9 then H11_2 = .;
    if H11B_2   =      9 then H11B_2 = .;
    if H12A_2   =      9 then H12A_2 = .;
    if H12B_2   =      9 then H12B_2 = .;
    if H12D_2   =      9 then H12D_2 = .;
    if H12E_2   =      9 then H12E_2 = .;
    if H12F_2   =      9 then H12F_2 = .;
    if H12G_2   =      9 then H12G_2 = .;
    if H12J_2   =      9 then H12J_2 = .;
    if H12K_2   =      9 then H12K_2 = .;
    if H12L_2   =      9 then H12L_2 = .;
    if H12S_2   =      9 then H12S_2 = .;
    if H12T_2   =      9 then H12T_2 = .;
    if H12X_2   =      9 then H12X_2 = .;
    if H12Y_2   =      9 then H12Y_2 = .;
    if H12Z_2   =      9 then H12Z_2 = .;
    if H13_2    =      9 then H13_2 = .;
    if H14_2    =      9 then H14_2 = .;
    if H15_2    =      9 then H15_2 = .;
    if H15A_2   =      9 then H15A_2 = .;
    if H15B_2   =      9 then H15B_2 = .;
    if H15C_2   =      9 then H15C_2 = .;
    if H15D_2   =      9 then H15D_2 = .;
    if H15E_2   =      9 then H15E_2 = .;
    if H15F_2   =      9 then H15F_2 = .;
    if H16_2    =      9 then H16_2 = .;
    if H20_2    =      9 then H20_2 = .;
    if H21A_2   =      9 then H21A_2 = .;
    if H21_2    =      9 then H21_2 = .;
    if H31_2    =      9 then H31_2 = .;
    if H31B_2   =      9 then H31B_2 = .;
    if H32A_2   =      9 then H32A_2 = .;
    if H32B_2   =      9 then H32B_2 = .;
    if H32C_2   =      9 then H32C_2 = .;
    if H32D_2   =      9 then H32D_2 = .;
    if H32E_2   =      9 then H32E_2 = .;
    if H32F_2   =      9 then H32F_2 = .;
    if H32G_2   =      9 then H32G_2 = .;
    if H32H_2   =      9 then H32H_2 = .;
    if H32I_2   =      9 then H32I_2 = .;
    if H32J_2   =      9 then H32J_2 = .;
    if H32K_2   =      9 then H32K_2 = .;
    if H32L_2   =      9 then H32L_2 = .;
    if H32M_2   =      9 then H32M_2 = .;
    if H32N_2   =      9 then H32N_2 = .;
    if H32O_2   =      9 then H32O_2 = .;
    if H32P_2   =      9 then H32P_2 = .;
    if H32Q_2   =      9 then H32Q_2 = .;
    if H32R_2   =      9 then H32R_2 = .;
    if H32S_2   =      9 then H32S_2 = .;
    if H32T_2   =      9 then H32T_2 = .;
    if H32U_2   =      9 then H32U_2 = .;
    if H32V_2   =      9 then H32V_2 = .;
    if H32W_2   =      9 then H32W_2 = .;
    if H32X_2   =      9 then H32X_2 = .;
    if H32Y_2   =      9 then H32Y_2 = .;
    if H32Z_2   =      9 then H32Z_2 = .;
    if H1_3     =      9 then H1_3 = .;
    if H2_3     =      9 then H2_3 = .;
    if H2D_3    =     99 then H2D_3 = .;
    if H2M_3    =     99 then H2M_3 = .;
    if H2Y_3    =     99 then H2Y_3 = .;
    if H3_3     =      9 then H3_3 = .;
    if H3D_3    =     99 then H3D_3 = .;
    if H3M_3    =     99 then H3M_3 = .;
    if H3Y_3    =     99 then H3Y_3 = .;
    if H4_3     =      9 then H4_3 = .;
    if H4D_3    =     99 then H4D_3 = .;
    if H4M_3    =     99 then H4M_3 = .;
    if H4Y_3    =     99 then H4Y_3 = .;
    if H5_3     =      9 then H5_3 = .;
    if H5D_3    =     99 then H5D_3 = .;
    if H5M_3    =     99 then H5M_3 = .;
    if H5Y_3    =     99 then H5Y_3 = .;
    if H6_3     =      9 then H6_3 = .;
    if H6D_3    =     99 then H6D_3 = .;
    if H6M_3    =     99 then H6M_3 = .;
    if H6Y_3    =     99 then H6Y_3 = .;
    if H7_3     =      9 then H7_3 = .;
    if H7D_3    =     99 then H7D_3 = .;
    if H7M_3    =     99 then H7M_3 = .;
    if H7Y_3    =     99 then H7Y_3 = .;
    if H8_3     =      9 then H8_3 = .;
    if H8D_3    =     99 then H8D_3 = .;
    if H8M_3    =     99 then H8M_3 = .;
    if H8Y_3    =     99 then H8Y_3 = .;
    if H9_3     =      9 then H9_3 = .;
    if H9D_3    =     99 then H9D_3 = .;
    if H9M_3    =     99 then H9M_3 = .;
    if H9Y_3    =     99 then H9Y_3 = .;
    if H0D_3    =     99 then H0D_3 = .;
    if H0M_3    =     99 then H0M_3 = .;
    if H0Y_3    =     99 then H0Y_3 = .;
    if H10_3    =      9 then H10_3 = .;
    if H11_3    =      9 then H11_3 = .;
    if H11B_3   =      9 then H11B_3 = .;
    if H12A_3   =      9 then H12A_3 = .;
    if H12B_3   =      9 then H12B_3 = .;
    if H12D_3   =      9 then H12D_3 = .;
    if H12E_3   =      9 then H12E_3 = .;
    if H12F_3   =      9 then H12F_3 = .;
    if H12G_3   =      9 then H12G_3 = .;
    if H12J_3   =      9 then H12J_3 = .;
    if H12K_3   =      9 then H12K_3 = .;
    if H12L_3   =      9 then H12L_3 = .;
    if H12S_3   =      9 then H12S_3 = .;
    if H12T_3   =      9 then H12T_3 = .;
    if H12X_3   =      9 then H12X_3 = .;
    if H12Y_3   =      9 then H12Y_3 = .;
    if H12Z_3   =      9 then H12Z_3 = .;
    if H13_3    =      9 then H13_3 = .;
    if H14_3    =      9 then H14_3 = .;
    if H15_3    =      9 then H15_3 = .;
    if H15A_3   =      9 then H15A_3 = .;
    if H15B_3   =      9 then H15B_3 = .;
    if H15C_3   =      9 then H15C_3 = .;
    if H15D_3   =      9 then H15D_3 = .;
    if H15E_3   =      9 then H15E_3 = .;
    if H15F_3   =      9 then H15F_3 = .;
    if H16_3    =      9 then H16_3 = .;
    if H20_3    =      9 then H20_3 = .;
    if H21A_3   =      9 then H21A_3 = .;
    if H21_3    =      9 then H21_3 = .;
    if H31_3    =      9 then H31_3 = .;
    if H31B_3   =      9 then H31B_3 = .;
    if H32A_3   =      9 then H32A_3 = .;
    if H32B_3   =      9 then H32B_3 = .;
    if H32C_3   =      9 then H32C_3 = .;
    if H32D_3   =      9 then H32D_3 = .;
    if H32E_3   =      9 then H32E_3 = .;
    if H32F_3   =      9 then H32F_3 = .;
    if H32G_3   =      9 then H32G_3 = .;
    if H32H_3   =      9 then H32H_3 = .;
    if H32I_3   =      9 then H32I_3 = .;
    if H32J_3   =      9 then H32J_3 = .;
    if H32K_3   =      9 then H32K_3 = .;
    if H32L_3   =      9 then H32L_3 = .;
    if H32M_3   =      9 then H32M_3 = .;
    if H32N_3   =      9 then H32N_3 = .;
    if H32O_3   =      9 then H32O_3 = .;
    if H32P_3   =      9 then H32P_3 = .;
    if H32Q_3   =      9 then H32Q_3 = .;
    if H32R_3   =      9 then H32R_3 = .;
    if H32S_3   =      9 then H32S_3 = .;
    if H32T_3   =      9 then H32T_3 = .;
    if H32U_3   =      9 then H32U_3 = .;
    if H32V_3   =      9 then H32V_3 = .;
    if H32W_3   =      9 then H32W_3 = .;
    if H32X_3   =      9 then H32X_3 = .;
    if H32Y_3   =      9 then H32Y_3 = .;
    if H32Z_3   =      9 then H32Z_3 = .;
    if H1_4     =      9 then H1_4 = .;
    if H2_4     =      9 then H2_4 = .;
    if H2D_4    =     99 then H2D_4 = .;
    if H2M_4    =     99 then H2M_4 = .;
    if H2Y_4    =     99 then H2Y_4 = .;
    if H3_4     =      9 then H3_4 = .;
    if H3D_4    =     99 then H3D_4 = .;
    if H3M_4    =     99 then H3M_4 = .;
    if H3Y_4    =     99 then H3Y_4 = .;
    if H4_4     =      9 then H4_4 = .;
    if H4D_4    =     99 then H4D_4 = .;
    if H4M_4    =     99 then H4M_4 = .;
    if H4Y_4    =     99 then H4Y_4 = .;
    if H5_4     =      9 then H5_4 = .;
    if H5D_4    =     99 then H5D_4 = .;
    if H5M_4    =     99 then H5M_4 = .;
    if H5Y_4    =     99 then H5Y_4 = .;
    if H6_4     =      9 then H6_4 = .;
    if H6D_4    =     99 then H6D_4 = .;
    if H6M_4    =     99 then H6M_4 = .;
    if H6Y_4    =     99 then H6Y_4 = .;
    if H7_4     =      9 then H7_4 = .;
    if H7D_4    =     99 then H7D_4 = .;
    if H7M_4    =     99 then H7M_4 = .;
    if H7Y_4    =     99 then H7Y_4 = .;
    if H8_4     =      9 then H8_4 = .;
    if H8D_4    =     99 then H8D_4 = .;
    if H8M_4    =     99 then H8M_4 = .;
    if H8Y_4    =     99 then H8Y_4 = .;
    if H9_4     =      9 then H9_4 = .;
    if H9D_4    =     99 then H9D_4 = .;
    if H9M_4    =     99 then H9M_4 = .;
    if H9Y_4    =     99 then H9Y_4 = .;
    if H0D_4    =     99 then H0D_4 = .;
    if H0M_4    =     99 then H0M_4 = .;
    if H0Y_4    =     99 then H0Y_4 = .;
    if H10_4    =      9 then H10_4 = .;
    if H11_4    =      9 then H11_4 = .;
    if H11B_4   =      9 then H11B_4 = .;
    if H12A_4   =      9 then H12A_4 = .;
    if H12B_4   =      9 then H12B_4 = .;
    if H12D_4   =      9 then H12D_4 = .;
    if H12E_4   =      9 then H12E_4 = .;
    if H12F_4   =      9 then H12F_4 = .;
    if H12G_4   =      9 then H12G_4 = .;
    if H12J_4   =      9 then H12J_4 = .;
    if H12K_4   =      9 then H12K_4 = .;
    if H12L_4   =      9 then H12L_4 = .;
    if H12S_4   =      9 then H12S_4 = .;
    if H12T_4   =      9 then H12T_4 = .;
    if H12X_4   =      9 then H12X_4 = .;
    if H12Y_4   =      9 then H12Y_4 = .;
    if H12Z_4   =      9 then H12Z_4 = .;
    if H13_4    =      9 then H13_4 = .;
    if H14_4    =      9 then H14_4 = .;
    if H15_4    =      9 then H15_4 = .;
    if H15A_4   =      9 then H15A_4 = .;
    if H15B_4   =      9 then H15B_4 = .;
    if H15C_4   =      9 then H15C_4 = .;
    if H15D_4   =      9 then H15D_4 = .;
    if H15E_4   =      9 then H15E_4 = .;
    if H15F_4   =      9 then H15F_4 = .;
    if H16_4    =      9 then H16_4 = .;
    if H20_4    =      9 then H20_4 = .;
    if H21A_4   =      9 then H21A_4 = .;
    if H21_4    =      9 then H21_4 = .;
    if H31_4    =      9 then H31_4 = .;
    if H31B_4   =      9 then H31B_4 = .;
    if H32A_4   =      9 then H32A_4 = .;
    if H32B_4   =      9 then H32B_4 = .;
    if H32C_4   =      9 then H32C_4 = .;
    if H32D_4   =      9 then H32D_4 = .;
    if H32E_4   =      9 then H32E_4 = .;
    if H32F_4   =      9 then H32F_4 = .;
    if H32G_4   =      9 then H32G_4 = .;
    if H32H_4   =      9 then H32H_4 = .;
    if H32I_4   =      9 then H32I_4 = .;
    if H32J_4   =      9 then H32J_4 = .;
    if H32K_4   =      9 then H32K_4 = .;
    if H32L_4   =      9 then H32L_4 = .;
    if H32M_4   =      9 then H32M_4 = .;
    if H32N_4   =      9 then H32N_4 = .;
    if H32O_4   =      9 then H32O_4 = .;
    if H32P_4   =      9 then H32P_4 = .;
    if H32Q_4   =      9 then H32Q_4 = .;
    if H32R_4   =      9 then H32R_4 = .;
    if H32S_4   =      9 then H32S_4 = .;
    if H32T_4   =      9 then H32T_4 = .;
    if H32U_4   =      9 then H32U_4 = .;
    if H32V_4   =      9 then H32V_4 = .;
    if H32W_4   =      9 then H32W_4 = .;
    if H32X_4   =      9 then H32X_4 = .;
    if H32Y_4   =      9 then H32Y_4 = .;
    if H32Z_4   =      9 then H32Z_4 = .;
    if H1_5     =      9 then H1_5 = .;
    if H2_5     =      9 then H2_5 = .;
    if H2D_5    =     99 then H2D_5 = .;
    if H2M_5    =     99 then H2M_5 = .;
    if H2Y_5    =     99 then H2Y_5 = .;
    if H3_5     =      9 then H3_5 = .;
    if H3D_5    =     99 then H3D_5 = .;
    if H3M_5    =     99 then H3M_5 = .;
    if H3Y_5    =     99 then H3Y_5 = .;
    if H4_5     =      9 then H4_5 = .;
    if H4D_5    =     99 then H4D_5 = .;
    if H4M_5    =     99 then H4M_5 = .;
    if H4Y_5    =     99 then H4Y_5 = .;
    if H5_5     =      9 then H5_5 = .;
    if H5D_5    =     99 then H5D_5 = .;
    if H5M_5    =     99 then H5M_5 = .;
    if H5Y_5    =     99 then H5Y_5 = .;
    if H6_5     =      9 then H6_5 = .;
    if H6D_5    =     99 then H6D_5 = .;
    if H6M_5    =     99 then H6M_5 = .;
    if H6Y_5    =     99 then H6Y_5 = .;
    if H7_5     =      9 then H7_5 = .;
    if H7D_5    =     99 then H7D_5 = .;
    if H7M_5    =     99 then H7M_5 = .;
    if H7Y_5    =     99 then H7Y_5 = .;
    if H8_5     =      9 then H8_5 = .;
    if H8D_5    =     99 then H8D_5 = .;
    if H8M_5    =     99 then H8M_5 = .;
    if H8Y_5    =     99 then H8Y_5 = .;
    if H9_5     =      9 then H9_5 = .;
    if H9D_5    =     99 then H9D_5 = .;
    if H9M_5    =     99 then H9M_5 = .;
    if H9Y_5    =     99 then H9Y_5 = .;
    if H0D_5    =     99 then H0D_5 = .;
    if H0M_5    =     99 then H0M_5 = .;
    if H0Y_5    =     99 then H0Y_5 = .;
    if H10_5    =      9 then H10_5 = .;
    if H11_5    =      9 then H11_5 = .;
    if H11B_5   =      9 then H11B_5 = .;
    if H12A_5   =      9 then H12A_5 = .;
    if H12B_5   =      9 then H12B_5 = .;
    if H12D_5   =      9 then H12D_5 = .;
    if H12E_5   =      9 then H12E_5 = .;
    if H12F_5   =      9 then H12F_5 = .;
    if H12G_5   =      9 then H12G_5 = .;
    if H12J_5   =      9 then H12J_5 = .;
    if H12K_5   =      9 then H12K_5 = .;
    if H12L_5   =      9 then H12L_5 = .;
    if H12S_5   =      9 then H12S_5 = .;
    if H12T_5   =      9 then H12T_5 = .;
    if H12X_5   =      9 then H12X_5 = .;
    if H12Y_5   =      9 then H12Y_5 = .;
    if H12Z_5   =      9 then H12Z_5 = .;
    if H13_5    =      9 then H13_5 = .;
    if H14_5    =      9 then H14_5 = .;
    if H15_5    =      9 then H15_5 = .;
    if H15A_5   =      9 then H15A_5 = .;
    if H15B_5   =      9 then H15B_5 = .;
    if H15C_5   =      9 then H15C_5 = .;
    if H15D_5   =      9 then H15D_5 = .;
    if H15E_5   =      9 then H15E_5 = .;
    if H15F_5   =      9 then H15F_5 = .;
    if H16_5    =      9 then H16_5 = .;
    if H20_5    =      9 then H20_5 = .;
    if H21A_5   =      9 then H21A_5 = .;
    if H21_5    =      9 then H21_5 = .;
    if H31_5    =      9 then H31_5 = .;
    if H31B_5   =      9 then H31B_5 = .;
    if H32A_5   =      9 then H32A_5 = .;
    if H32B_5   =      9 then H32B_5 = .;
    if H32C_5   =      9 then H32C_5 = .;
    if H32D_5   =      9 then H32D_5 = .;
    if H32E_5   =      9 then H32E_5 = .;
    if H32F_5   =      9 then H32F_5 = .;
    if H32G_5   =      9 then H32G_5 = .;
    if H32H_5   =      9 then H32H_5 = .;
    if H32I_5   =      9 then H32I_5 = .;
    if H32J_5   =      9 then H32J_5 = .;
    if H32K_5   =      9 then H32K_5 = .;
    if H32L_5   =      9 then H32L_5 = .;
    if H32M_5   =      9 then H32M_5 = .;
    if H32N_5   =      9 then H32N_5 = .;
    if H32O_5   =      9 then H32O_5 = .;
    if H32P_5   =      9 then H32P_5 = .;
    if H32Q_5   =      9 then H32Q_5 = .;
    if H32R_5   =      9 then H32R_5 = .;
    if H32S_5   =      9 then H32S_5 = .;
    if H32T_5   =      9 then H32T_5 = .;
    if H32U_5   =      9 then H32U_5 = .;
    if H32V_5   =      9 then H32V_5 = .;
    if H32W_5   =      9 then H32W_5 = .;
    if H32X_5   =      9 then H32X_5 = .;
    if H32Y_5   =      9 then H32Y_5 = .;
    if H32Z_5   =      9 then H32Z_5 = .;
    if H1_6     =      9 then H1_6 = .;
    if H2_6     =      9 then H2_6 = .;
    if H2D_6    =     99 then H2D_6 = .;
    if H2M_6    =     99 then H2M_6 = .;
    if H2Y_6    =     99 then H2Y_6 = .;
    if H3_6     =      9 then H3_6 = .;
    if H3D_6    =     99 then H3D_6 = .;
    if H3M_6    =     99 then H3M_6 = .;
    if H3Y_6    =     99 then H3Y_6 = .;
    if H4_6     =      9 then H4_6 = .;
    if H4D_6    =     99 then H4D_6 = .;
    if H4M_6    =     99 then H4M_6 = .;
    if H4Y_6    =     99 then H4Y_6 = .;
    if H5_6     =      9 then H5_6 = .;
    if H5D_6    =     99 then H5D_6 = .;
    if H5M_6    =     99 then H5M_6 = .;
    if H5Y_6    =     99 then H5Y_6 = .;
    if H6_6     =      9 then H6_6 = .;
    if H6D_6    =     99 then H6D_6 = .;
    if H6M_6    =     99 then H6M_6 = .;
    if H6Y_6    =     99 then H6Y_6 = .;
    if H7_6     =      9 then H7_6 = .;
    if H7D_6    =     99 then H7D_6 = .;
    if H7M_6    =     99 then H7M_6 = .;
    if H7Y_6    =     99 then H7Y_6 = .;
    if H8_6     =      9 then H8_6 = .;
    if H8D_6    =     99 then H8D_6 = .;
    if H8M_6    =     99 then H8M_6 = .;
    if H8Y_6    =     99 then H8Y_6 = .;
    if H9_6     =      9 then H9_6 = .;
    if H9D_6    =     99 then H9D_6 = .;
    if H9M_6    =     99 then H9M_6 = .;
    if H9Y_6    =     99 then H9Y_6 = .;
    if H0D_6    =     99 then H0D_6 = .;
    if H0M_6    =     99 then H0M_6 = .;
    if H0Y_6    =     99 then H0Y_6 = .;
    if H10_6    =      9 then H10_6 = .;
    if H11_6    =      9 then H11_6 = .;
    if H11B_6   =      9 then H11B_6 = .;
    if H12A_6   =      9 then H12A_6 = .;
    if H12B_6   =      9 then H12B_6 = .;
    if H12D_6   =      9 then H12D_6 = .;
    if H12E_6   =      9 then H12E_6 = .;
    if H12F_6   =      9 then H12F_6 = .;
    if H12G_6   =      9 then H12G_6 = .;
    if H12J_6   =      9 then H12J_6 = .;
    if H12K_6   =      9 then H12K_6 = .;
    if H12L_6   =      9 then H12L_6 = .;
    if H12S_6   =      9 then H12S_6 = .;
    if H12T_6   =      9 then H12T_6 = .;
    if H12X_6   =      9 then H12X_6 = .;
    if H12Y_6   =      9 then H12Y_6 = .;
    if H12Z_6   =      9 then H12Z_6 = .;
    if H13_6    =      9 then H13_6 = .;
    if H14_6    =      9 then H14_6 = .;
    if H15_6    =      9 then H15_6 = .;
    if H15A_6   =      9 then H15A_6 = .;
    if H15B_6   =      9 then H15B_6 = .;
    if H15C_6   =      9 then H15C_6 = .;
    if H15D_6   =      9 then H15D_6 = .;
    if H15E_6   =      9 then H15E_6 = .;
    if H15F_6   =      9 then H15F_6 = .;
    if H16_6    =      9 then H16_6 = .;
    if H20_6    =      9 then H20_6 = .;
    if H21A_6   =      9 then H21A_6 = .;
    if H21_6    =      9 then H21_6 = .;
    if H31_6    =      9 then H31_6 = .;
    if H31B_6   =      9 then H31B_6 = .;
    if H32A_6   =      9 then H32A_6 = .;
    if H32B_6   =      9 then H32B_6 = .;
    if H32C_6   =      9 then H32C_6 = .;
    if H32D_6   =      9 then H32D_6 = .;
    if H32E_6   =      9 then H32E_6 = .;
    if H32F_6   =      9 then H32F_6 = .;
    if H32G_6   =      9 then H32G_6 = .;
    if H32H_6   =      9 then H32H_6 = .;
    if H32I_6   =      9 then H32I_6 = .;
    if H32J_6   =      9 then H32J_6 = .;
    if H32K_6   =      9 then H32K_6 = .;
    if H32L_6   =      9 then H32L_6 = .;
    if H32M_6   =      9 then H32M_6 = .;
    if H32N_6   =      9 then H32N_6 = .;
    if H32O_6   =      9 then H32O_6 = .;
    if H32P_6   =      9 then H32P_6 = .;
    if H32Q_6   =      9 then H32Q_6 = .;
    if H32R_6   =      9 then H32R_6 = .;
    if H32S_6   =      9 then H32S_6 = .;
    if H32T_6   =      9 then H32T_6 = .;
    if H32U_6   =      9 then H32U_6 = .;
    if H32V_6   =      9 then H32V_6 = .;
    if H32W_6   =      9 then H32W_6 = .;
    if H32X_6   =      9 then H32X_6 = .;
    if H32Y_6   =      9 then H32Y_6 = .;
    if H32Z_6   =      9 then H32Z_6 = .;
    if HW13_1   =      9 then HW13_1 = .;
    if HW13_2   =      9 then HW13_2 = .;
    if HW13_3   =      9 then HW13_3 = .;
    if HW13_4   =      9 then HW13_4 = .;
    if HW13_5   =      9 then HW13_5 = .;
    if HW13_6   =      9 then HW13_6 = .;
    if V501     =      9 then V501 = .;
    if V502     =      9 then V502 = .;
    if V503     =      9 then V503 = .;
    if V504     =      9 then V504 = .;
    if V527     =    999 then V527 = .;
    if V528     =     99 then V528 = .;
    if V529     =    999 then V529 = .;
    if V535     =      9 then V535 = .;
    if V602     =      9 then V602 = .;
    if V603     =    999 then V603 = .;
    if V604     =      9 then V604 = .;
    if V605     =      9 then V605 = .;
    if V606     =      9 then V606 = .;
    if V610     =      9 then V610 = .;
    if V611     =      9 then V611 = .;
    if V612     =      9 then V612 = .;
    if V613     =     99 then V613 = .;
    if V616     =    999 then V616 = .;
    if V618     =      9 then V618 = .;
    if V621     =      9 then V621 = .;
    if V623     =      9 then V623 = .;
    if V624     =     99 then V624 = .;
    if V625     =      9 then V625 = .;
    if V626     =     99 then V626 = .;
    if V627     =     99 then V627 = .;
    if V628     =     99 then V628 = .;
    if V629     =     99 then V629 = .;
    if V701     =      9 then V701 = .;
    if V702     =     99 then V702 = .;
    if V704     =    999 then V704 = .;
    if V705     =     99 then V705 = .;
    if V707     =      9 then V707 = .;
    if V714     =      9 then V714 = .;
    if V715     =     99 then V715 = .;
    if V716     =    999 then V716 = .;
    if V717     =     99 then V717 = .;
    if V718     =      9 then V718 = .;
    if V719     =      9 then V719 = .;
    if V720     =      9 then V720 = .;
    if V721     =      9 then V721 = .;
    if V723     =     99 then V723 = .;
    if V729     =      9 then V729 = .;
    if V730     =     99 then V730 = .;
    if V732     =      9 then V732 = .;
    if V739     =      9 then V739 = .;
    if V740     =      9 then V740 = .;
    if V751     =      9 then V751 = .;
    if V764     =      9 then V764 = .;
    if V765     =      9 then V765 = .;
    if V801     =   9999 then V801 = .;
    if V802     =   9999 then V802 = .;
    if V803     =     99 then V803 = .;
    if V805     =    999 then V805 = .;
    if V806     =     99 then V806 = .;
    if V811     =      9 then V811 = .;
    if V812     =      9 then V812 = .;
    if V813     =      9 then V813 = .;
    if V814     =      9 then V814 = .;
    if S114A    =      9 then S114A = .;
    if S114B    =      9 then S114B = .;
    if S114C    =      9 then S114C = .;
    if S114D    =      9 then S114D = .;
    if S114E    =      9 then S114E = .;
    if S119     =    999 then S119 = .;
    if S119A    =    999 then S119A = .;
    if S123     =     99 then S123 = .;
    if S124A    =      9 then S124A = .;
    if S124B    =      9 then S124B = .;
    if S124C    =      9 then S124C = .;
    if S124D    =      9 then S124D = .;
    if S124I    =      9 then S124I = .;
    if S229M    =     99 then S229M = .;
    if S229Y    =   9999 then S229Y = .;
    if S233A    =      9 then S233A = .;
    if S233B    =      9 then S233B = .;
    if S233C    =      9 then S233C = .;
    if S233DM   =     99 then S233DM = .;
    if S233DY   =   9999 then S233DY = .;
    if S304_01  =      9 then S304_01 = .;
    if S304_02  =      9 then S304_02 = .;
    if S304_03  =      9 then S304_03 = .;
    if S304_04  =      9 then S304_04 = .;
    if S304_05  =      9 then S304_05 = .;
    if S304_06  =      9 then S304_06 = .;
    if S304_07  =      9 then S304_07 = .;
    if S304_08  =      9 then S304_08 = .;
    if S304_09  =      9 then S304_09 = .;
    if S304_10  =      9 then S304_10 = .;
    if S304_11  =      9 then S304_11 = .;
    if S304_12  =      9 then S304_12 = .;
    if S304_13  =      9 then S304_13 = .;
    if S304_14  =      9 then S304_14 = .;
    if S304_15  =      9 then S304_15 = .;
    if S309     =     99 then S309 = .;
    if S310     =     99 then S310 = .;
    if S316     =      9 then S316 = .;
    if S317     =     99 then S317 = .;
    if S318B    =      9 then S318B = .;
    if S318C    =     99 then S318C = .;
    if S318EA   =      9 then S318EA = .;
    if S318EB   =      9 then S318EB = .;
    if S318EC   =      9 then S318EC = .;
    if S318ED   =      9 then S318ED = .;
    if S318EE   =      9 then S318EE = .;
    if S318EF   =      9 then S318EF = .;
    if S318EG   =      9 then S318EG = .;
    if S318F    =     99 then S318F = .;
    if S318HA   =      9 then S318HA = .;
    if S318HB   =      9 then S318HB = .;
    if S318HC   =      9 then S318HC = .;
    if S318HD   =      9 then S318HD = .;
    if S318HE   =      9 then S318HE = .;
    if S318HF   =      9 then S318HF = .;
    if S318HG   =      9 then S318HG = .;
    if S318HH   =      9 then S318HH = .;
    if S318HI   =      9 then S318HI = .;
    if S318I    =      9 then S318I = .;
    if S318K    =      9 then S318K = .;
    if S319     =     99 then S319 = .;
    if S319B    =      9 then S319B = .;
    if S320S    =      9 then S320S = .;
    if S320     =     99 then S320 = .;
    if S320A    =      9 then S320A = .;
    if S320C    =      9 then S320C = .;
    if S322C    =     99 then S322C = .;
    if S320D    =     99 then S320D = .;
    if S322D    =     99 then S322D = .;
    if S323B    =      9 then S323B = .;
    if S325     =      9 then S325 = .;
    if S325A    =      9 then S325A = .;
    if S325B    =      9 then S325B = .;
    if S325C    =      9 then S325C = .;
    if S325E    =      9 then S325E = .;
    if S325G    =      9 then S325G = .;
    if S325H    =      9 then S325H = .;
    if S325I    =     99 then S325I = .;
    if S325J    =      9 then S325J = .;
    if S326     =     99 then S326 = .;
    if S327     =      9 then S327 = .;
    if S328PA   =      9 then S328PA = .;
    if S328PB   =      9 then S328PB = .;
    if S328PC   =      9 then S328PC = .;
    if S328PD   =      9 then S328PD = .;
    if S328PE   =      9 then S328PE = .;
    if S328PF   =      9 then S328PF = .;
    if S328PG   =      9 then S328PG = .;
    if S328PH   =      9 then S328PH = .;
    if S328PI   =      9 then S328PI = .;
    if S328PJ   =      9 then S328PJ = .;
    if S328PK   =      9 then S328PK = .;
    if S328A    =      9 then S328A = .;
    if S328B    =      9 then S328B = .;
    if S328C    =      9 then S328C = .;
    if S328DA   =      9 then S328DA = .;
    if S328DB   =      9 then S328DB = .;
    if S328DC   =      9 then S328DC = .;
    if S328DD   =      9 then S328DD = .;
    if S328DE   =      9 then S328DE = .;
    if S328DF   =      9 then S328DF = .;
    if S328DG   =      9 then S328DG = .;
    if S328DH   =      9 then S328DH = .;
    if S328DI   =      9 then S328DI = .;
    if S347A    =      9 then S347A = .;
    if S347B    =      9 then S347B = .;
    if S347CA   =      9 then S347CA = .;
    if S347CB   =      9 then S347CB = .;
    if S347CC   =      9 then S347CC = .;
    if S347CD   =      9 then S347CD = .;
    if S347CE   =      9 then S347CE = .;
    if S348C    =      9 then S348C = .;
    if S349A    =      9 then S349A = .;
    if S349B    =      9 then S349B = .;
    if S349C    =      9 then S349C = .;
    if S349D    =      9 then S349D = .;
    if S349E    =      9 then S349E = .;
    if S352     =     99 then S352 = .;
    if S353     =      9 then S353 = .;
    if S354     =      9 then S354 = .;
    if S355     =      9 then S355 = .;
    if S356     =     99 then S356 = .;
    if S357MA   =      9 then S357MA = .;
    if S357MB   =      9 then S357MB = .;
    if S357MC   =      9 then S357MC = .;
    if S357MD   =      9 then S357MD = .;
    if S357ME   =      9 then S357ME = .;
    if S357MF   =      9 then S357MF = .;
    if S357MG   =      9 then S357MG = .;
    if S357A    =      9 then S357A = .;
    if S357B    =      9 then S357B = .;
    if S358     =      9 then S358 = .;
    if S359     =      9 then S359 = .;
    if S361     =      9 then S361 = .;
    if S439     =      9 then S439 = .;
    if S440     =     99 then S440 = .;
    if S441     =     99 then S441 = .;
    if S442A    =      9 then S442A = .;
    if S442B_1  =     99 then S442B_1 = .;
    if S442C_1  =      9 then S442C_1 = .;
    if S442DA_1 =      9 then S442DA_1 = .;
    if S442DB_1 =      9 then S442DB_1 = .;
    if S442DC_1 =      9 then S442DC_1 = .;
    if S442DD_1 =      9 then S442DD_1 = .;
    if S442DE_1 =      9 then S442DE_1 = .;
    if S442DF_1 =      9 then S442DF_1 = .;
    if S442DG_1 =      9 then S442DG_1 = .;
    if S442DH_1 =      9 then S442DH_1 = .;
    if S442DI_1 =      9 then S442DI_1 = .;
    if S442DJ_1 =      9 then S442DJ_1 = .;
    if S442DK_1 =      9 then S442DK_1 = .;
    if S442EM_1 =     99 then S442EM_1 = .;
    if S442FM_1 =     99 then S442FM_1 = .;
    if S442EM_1 =     99 then S442EM_1 = .;
    if S442FM_1 =     99 then S442FM_1 = .;
    if S442B_2  =     99 then S442B_2 = .;
    if S442C_2  =      9 then S442C_2 = .;
    if S442DA_2 =      9 then S442DA_2 = .;
    if S442DB_2 =      9 then S442DB_2 = .;
    if S442DC_2 =      9 then S442DC_2 = .;
    if S442DD_2 =      9 then S442DD_2 = .;
    if S442DE_2 =      9 then S442DE_2 = .;
    if S442DF_2 =      9 then S442DF_2 = .;
    if S442DG_2 =      9 then S442DG_2 = .;
    if S442DH_2 =      9 then S442DH_2 = .;
    if S442DI_2 =      9 then S442DI_2 = .;
    if S442DJ_2 =      9 then S442DJ_2 = .;
    if S442DK_2 =      9 then S442DK_2 = .;
    if S442EM_2 =     99 then S442EM_2 = .;
    if S442FM_2 =     99 then S442FM_2 = .;
    if S442EM_2 =     99 then S442EM_2 = .;
    if S442FM_2 =     99 then S442FM_2 = .;
    if S442H    =     99 then S442H = .;
    if S442IA   =      9 then S442IA = .;
    if S442IB   =      9 then S442IB = .;
    if S442IC   =      9 then S442IC = .;
    if S442ID   =      9 then S442ID = .;
    if S442IE   =      9 then S442IE = .;
    if S442IF   =      9 then S442IF = .;
    if S442IG   =      9 then S442IG = .;
    if S442J    =     99 then S442J = .;
    if S442KA   =      9 then S442KA = .;
    if S442KB   =      9 then S442KB = .;
    if S442KC   =      9 then S442KC = .;
    if S442KD   =      9 then S442KD = .;
    if S442KE   =      9 then S442KE = .;
    if S442KF   =      9 then S442KF = .;
    if S442L    =      9 then S442L = .;
    if S442MA   =      9 then S442MA = .;
    if S442MB   =      9 then S442MB = .;
    if S442MC   =      9 then S442MC = .;
    if S442MD   =      9 then S442MD = .;
    if S442ME   =      9 then S442ME = .;
    if S442MF   =      9 then S442MF = .;
    if S442MG   =      9 then S442MG = .;
    if S442MH   =      9 then S442MH = .;
    if S442MI   =      9 then S442MI = .;
    if S442N    =      9 then S442N = .;
    if S442O    =     99 then S442O = .;
    if S609     =     99 then S609 = .;
    if S613     =      9 then S613 = .;
    if S615     =      9 then S615 = .;
    if S616     =      9 then S616 = .;
    if S617     =      9 then S617 = .;
    if S618     =      9 then S618 = .;
