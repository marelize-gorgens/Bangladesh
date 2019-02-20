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
  value MV013_
     1 = "15-19"
     2 = "20-24"
     3 = "25-29"
     4 = "30-34"
     5 = "35-39"
     6 = "40-44"
     7 = "45-49"
     8 = "50-54"
     9 = "55-59"
    10 = "60+"
     ;
  value MV014_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     4 = "Y & age - y ignored"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value MV015_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partly completed"
     6 = "Incapacitated"
     7 = "Other"
     ;
  value MV023_
     0 = "National"
     1 = "Country specific"
     ;
  value MV024_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value MV025_
     1 = "Urban"
     2 = "Rural"
     ;
  value MV026_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value MV035_
     0 = "No wives/partners"
     ;
  value MV803_
    95 = "95+"
    96 = "2+ visits"
    97 = "Inconsistent"
     ;
  value MV101_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value MV102_
     1 = "Urban"
     2 = "Rural"
     ;
  value MV104_
    95 = "Always"
    96 = "Visitor"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value MV105_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value MV106_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     ;
  value MV108_
     1 = "Reads easily"
     2 = "Reads with difficult"
     3 = "Cannot read"
     ;
  value MV109_
     0 = "No"
     1 = "Yes"
     ;
  value MV110_
     0 = "No"
     1 = "Yes"
     ;
  value MV112_
     0 = "No"
     1 = "Yes"
     ;
  value MV130_
     1 = "Islam"
     2 = "Christianity"
     3 = "Hinduism"
     4 = "Buddhism"
     5 = "Other"
     ;
  value MV133_
    97 = "Inconsistent"
     ;
  value MV134_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value MV135_
     1 = "Usual resident"
     2 = "Visitor"
     ;
  value MV148_
     0 = "No"
     1 = "Yes"
     ;
  value MV149_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     ;
  value MV150_
     1 = "Head"
     2 = "Husband"
     3 = "Son"
     4 = "Son-in-law"
     5 = "Grand-son"
     6 = "Father"
     7 = "Father-in-law"
     8 = "Brother"
    10 = "Other relative"
    11 = "Adopted/foster child"
    12 = "Not related"
    98 = "DK"
     ;
  value MV151_
     1 = "Male"
     2 = "Female"
     ;
  value MV301_
     0 = "Knows no method"
     1 = "Knows only folkloric"
     2 = "Knows only trad. mth"
     3 = "Knows modern method"
     ;
  value MV302_
     0 = "Never used"
     1 = "Used only folkloric"
     2 = "Used only trad. meth"
     3 = "Used modern method"
     ;
  value MV304A_
     1 = "Modern method"
     2 = "Traditional method"
     3 = "Folkloric method"
     ;
  value MV304_
     0 = "No"
     1 = "Yes, spontaneously"
     2 = "Yes, probed"
     8 = "Not asked"
     ;
  value MV305_
     0 = "No"
     1 = "Yes"
     ;
  value MV312_
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
  value MV313_
     0 = "No method"
     1 = "Folkloric method"
     2 = "Traditional method"
     3 = "Modern method"
     ;
  value MV361_
     1 = "Currently using"
     3 = "Ever used"
     4 = "Never used"
     ;
  value MV362_
     1 = "In next 12 months"
     2 = "Use later"
     3 = "Unsure about timing"
     4 = "Unsure about use"
     5 = "Does not intend"
     6 = "Never had sex"
     ;
  value MV363_
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
  value MV364_
     1 = "Using modern method"
     2 = "Using tradit. method"
     3 = "Non-user intend to"
     4 = "Does not intend to"
     5 = "Never had sex"
     ;
  value MV375A_
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
    32 = "Partner opposed"
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
    61 = "Up to woman to use"
    95 = "No reason"
    96 = "Other"
    98 = "DK"
     ;
  value MV376_
    11 = "Not married"
    20 = "FERTILITY-RELATED"
    22 = "Infrequent sex"
    23 = "Menopausal, hyster."
    24 = "Subfecund, infecund"
    25 = "Postpartum, breastf."
    26 = "Wants more children"
    30 = "OPPOSITION TO USE"
    31 = "Respondent opposed"
    32 = "Partner opposed"
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
    61 = "Up to woman to use"
    96 = "Other"
    98 = "DK"
     ;
  value MV384A_
     0 = "No"
     1 = "Yes"
     ;
  value MV384B_
     0 = "No"
     1 = "Yes"
     ;
  value MV384D_
     0 = "No"
     1 = "Yes"
     ;
  value MV501_
     0 = "Never married"
     1 = "Married"
     2 = "Living together"
     3 = "Widowed"
     4 = "Divorced"
     5 = "Not living together"
     ;
  value MV502_
     0 = "Never married"
     1 = "Currently married"
     2 = "Formerly married"
     ;
  value MV503_
     1 = "Once"
     2 = "More than once"
     ;
  value MV510_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value MV513_
     0 = "Never married"
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25-29"
     7 = "30+"
     ;
  value MV527_
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value MV532_
     0 = "No flag"
     1 = "After interview"
     2 = "After concep >= 1 yr"
     3 = "After concep < 1 yr"
     4 = "At marr, never marr"
     5 = "At marr,after concep"
     6 = "After marriage"
     ;
  value MV602_
     1 = "Have another"
     2 = "Undecided"
     3 = "No more"
     4 = "Sterilized"
     5 = "Wife declared infec"
     6 = "Never had sex"
     7 = "Man declared infec."
     ;
  value MV603_
   993 = "After marriage"
   994 = "Soon, now"
   996 = "Other, Non-numeric"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value MV604_
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
  value MV605_
     1 = "Wants within 2 years"
     2 = "Wants after 2+ years"
     3 = "Wants, unsure timing"
     4 = "Undecided"
     5 = "Wants no more"
     6 = "Sterilized"
     7 = "Declared infecund"
     8 = "Never had sex"
     ;
  value MV610_
     0 = "Disapproves"
     1 = "Approves"
     8 = "Don't know"
     ;
  value MV611_
     0 = "Never"
     1 = "Once or twice"
     2 = "More often"
     ;
  value MV612_
     0 = "Disapproves"
     1 = "Approves"
     8 = "Don't know"
     ;
  value MV613_
    95 = "Up to God"
    96 = "Non-numeric response"
    98 = "DK"
     ;
  value MV614_
     6 = "6+"
     7 = "Non-numeric response"
     ;
  value MV616_
   993 = "After marriage"
   994 = "Soon/now"
   995 = "Can't get pregnant"
   996 = "Other"
   998 = "DK"
     ;
  value MV621_
     1 = "Both want same"
     2 = "Wife wants more"
     3 = "Wife wants fewer"
     8 = "DK"
     ;
  value MV627_
    95 = "Up to God"
    96 = "Other"
     ;
  value MV628_
    95 = "Up to God"
    96 = "Other"
     ;
  value MV629_
    95 = "Up to God"
    96 = "Other"
     ;
  value MV714_
     0 = "No"
     1 = "Yes"
     ;
  value MV716_
     0 = "Not working"
     ;
  value MV717_
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
  value MV731_
     0 = "No"
     1 = "In the past year"
     2 = "Currently working"
     ;
  value MV740_
     0 = "Own land"
     1 = "Family land"
     2 = "Someone else's land"
     3 = "Rented land"
     8 = "Don't know"
     ;
  value MV762_
    11 = "Government hospital"
    12 = "Government health c."
    13 = "Family planning clin"
    14 = "Mobile clinic"
    15 = "Public field worker"
    16 = "Other public"
    21 = "Private hosp, clinic"
    22 = "Pharmacy"
    23 = "Private doctor"
    24 = "Private mobile clin"
    25 = "Private field worker"
    26 = "Other private"
    31 = "Shop"
    32 = "Church"
    33 = "Friends, relatives"
    96 = "Other"
    98 = "DK source for condom"
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
  value MSFORMA_
     1 = "Bengali"
     2 = "English"
     ;
  value MSADJUS_
     0 = "Not adjusted"
     1 = "Adjusted"
     ;
  value MS304_
     0 = "No"
     1 = "Yes"
     ;
  value MS309_
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
  value MS316_
     0 = "No"
     1 = "Yes"
     ;
  value MS317_
     0 = "Not using 2nd method"
     1 = "Pill"
     2 = "IUD"
     3 = "Injections"
     4 = "Diaphragm/Foam/Jelly"
     5 = "Condom"
     8 = "Periodic Abstinence"
     9 = "Withdrawal"
    10 = "Other"
    11 = "Norplant"
     ;
  value MS320S_
     0 = "No"
     1 = "Yes"
     ;
  value MS320_
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
  value MS320A_
     1 = "Wife keeps"
     2 = "Ran out"
     3 = "Other"
     ;
  value MS320C_
  9995 = "Partner obtained"
  9996 = "Free"
  9998 = "Don't know"
     ;
  value MS320D_
     0 = "No"
     1 = "Yes"
     ;
  value MS321F_
     1 = "Month and year"
     5 = "Year - m imp, a calc"
     7 = "Month - date placed"
     8 = "None - date placed"
     ;
  value MS321XA_
     1 = "<2"
     2 = "2-3"
     3 = "4-5"
     4 = "6-7"
     5 = "8-9"
     6 = "10+"
     ;
  value MS321XB_
     1 = "<25"
     2 = "25-29"
     3 = "30-34"
     4 = "35-39"
     5 = "40-44"
     6 = "45+"
     ;
  value MS321XC_
     0 = "Single,before marr."
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25+"
     ;
  value MS321XD_
     1 = "Bengali"
     2 = "English"
     ;
  value MS321XE_
    98 = "DK month"
     ;
  value MS321XF_
  9997 = "Inconsistent"
  9998 = "DK year"
     ;
  value MS322B_
     0 = "Does not regret"
     1 = "Wants another child"
     2 = "Partner wants child"
     3 = "Side effects"
     4 = "Other reason"
     ;
  value MS322C_
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
  value MS323_
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
  value MS323B_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value MS325_
     1 = "Respondent"
     2 = "Wife"
     3 = "Son/daughter"
     4 = "Other relative"
     5 = "Other"
     ;
  value MS325A_
     0 = "No"
     1 = "Yes"
     ;
  value MS325B_
     0 = "No"
     1 = "Yes"
     8 = "Cannot remember"
     ;
  value MS325C_
     0 = "No"
     1 = "Yes"
     8 = "Cannot remember"
     ;
  value MS325H_
     0 = "No"
     1 = "Yes"
     ;
  value MS325I_
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
     ;
  value MS325J_
     1 = "Respondent had most"
     2 = "Wife had most"
     3 = "Husb. and wife equal"
     4 = "Other relative"
     5 = "Other"
     ;
  value MS326_
     1 = "Recomm. FP worker"
     2 = "Recomm. friend, rel."
     3 = "Side effects others"
     4 = "Method easy to use"
     5 = "Access, availability"
     6 = "Cost"
     7 = "Wanted permanent m."
     8 = "Wife preferred"
     9 = "Wanted more effect."
    10 = "Other"
    98 = "DK"
     ;
  value MS328HI_
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
  value MS347A_
     1 = "Yes"
     0 = "No"
     8 = "DK"
     ;
  value MS347B_
     1 = "Yes"
     0 = "No"
     ;
  value MS347CA_
     0 = "No"
     1 = "Yes"
     ;
  value MS347CB_
     0 = "No"
     1 = "Yes"
     ;
  value MS347CC_
     0 = "No"
     1 = "Yes"
     ;
  value MS347CD_
     0 = "No"
     1 = "Yes"
     ;
  value MS347CE_
     0 = "No"
     1 = "Yes"
     ;
  value MS348C_
     0 = "No"
     1 = "Yes"
     ;
  value MS349A_
     0 = "No"
     1 = "Yes"
     ;
  value MS349B_
     0 = "No"
     1 = "Yes"
     ;
  value MS349C_
     0 = "No"
     1 = "Yes"
     ;
  value MS349D_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value MS349E_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value MS350_
     0 = "No visit"
     1 = "Yes, for Fmly Plng."
     2 = "Yes, for other rsn."
     8 = "DK"
     ;
  value MS352_
    98 = "DK"
     ;
  value MS353_
     0 = "Less than 1 mth ago"
     8 = "DK"
     ;
  value MS354_
     0 = "No"
     1 = "Yes"
     ;
  value MS355_
     1 = "Pills"
     2 = "Condoms"
     3 = "Injection"
     4 = "Other"
     ;
  value MS357A_
     0 = "Yes"
     1 = "No"
     ;
  value MS357B_
     0 = "Yes"
     1 = "No"
     ;
  value MS357C_
     0 = "Yes"
     1 = "No"
     ;
  value MS357D_
     0 = "Yes"
     1 = "No"
     ;
  value MS357E_
     0 = "Yes"
     1 = "No"
     ;
  value MS357F_
     0 = "Yes"
     1 = "No"
     ;
  value MS357G_
     0 = "Yes"
     1 = "No"
     ;
  value MS358_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value MS359_
     0 = "No"
     1 = "Yes"
     ;
  value MS360_
     0 = "No"
     1 = "Yes"
     ;
  value MS361_
     0 = "No"
     1 = "Yes"
     ;
  value MS408A_
     0 = "No"
     1 = "Yes"
     ;
  value MS408B_
     0 = "No"
     1 = "Yes"
     ;
  value MS408C_
     0 = "No"
     1 = "Yes"
     ;
  value MS408D_
     0 = "No"
     1 = "Yes"
     ;
  value MS508_
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
  value MS512_
     0 = "No"
     1 = "Yes"
     ;
  value MS514_
     0 = "Not allowed to go out"
     1 = "Yes, alone"
     2 = "Yes, with children"
     6 = "Other"
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
  attrib MCASEID  length=$15 label="Case Identification";
  attrib MV000    length=$3 label="Country code and phase";
  attrib MV001    length=8 label="Cluster number";
  attrib MV002    length=4 label="Household number";
  attrib MV003    length=4 label="Respondent's line number";
  attrib MV004    length=4 label="Ultimate area unit";
  attrib MV005    length=8 label="Sample weight";
  attrib MV006    length=4 label="Month of interview";
  attrib MV007    length=4 label="Year of interview";
  attrib MV008    length=4 label="Date of interview (CMC)";
  attrib MV009    length=4 label="Respondent's month of birth";
  attrib MV010    length=4 label="Respondent's year of birth";
  attrib MV011    length=4 label="Date of birth (CMC)";
  attrib MV012    length=4 label="Current age - respondent";
  attrib MV013    length=4 format=MV013_. label="Age 5-year groups";
  attrib MV014    length=4 format=MV014_. label="Completeness of information";
  attrib MV015    length=4 format=MV015_. label="Result of individual interview";
  attrib MV016    length=4 label="Day of interview";
  attrib MV021    length=4 label="Primary sampling unit";
  attrib MV022    length=4 label="Sample stratum number";
  attrib MV023    length=4 format=MV023_. label="Sample domain";
  attrib MV024    length=4 format=MV024_. label="Region";
  attrib MV025    length=4 format=MV025_. label="Type of place of residence";
  attrib MV026    length=4 format=MV026_. label="De facto place of residence";
  attrib MV027    length=4 label="Number of visits";
  attrib MV028    length=4 label="Interviewer identification";
  attrib MV029    length=4 label="Keyer identification";
  attrib MV030    length=4 label="Field supervisor - NA";
  attrib MV031    length=4 label="Field editor - NA";
  attrib MV032    length=4 label="Office editor - NA";
  attrib MV033    length=8 label="Ultimate area selection prob.";
  attrib MV034_1  length=4 label="Line numbers of wives";
  attrib MV034_2  length=4 label="Line numbers of wives";
  attrib MV034_3  length=4 label="Line numbers of wives";
  attrib MV034_4  length=4 label="Line numbers of wives";
  attrib MV034_5  length=4 label="Line numbers of wives";
  attrib MV034_6  length=4 label="Line numbers of wives";
  attrib MV034_7  length=4 label="Line numbers of wives";
  attrib MV034_8  length=4 label="Line numbers of wives";
  attrib MV035    length=4 format=MV035_. label="Number of wives, partners";
  attrib MV801    length=4 label="Time interview started";
  attrib MV802    length=4 label="Time interview ended";
  attrib MV803    length=4 format=MV803_. label="Length of interview in minutes";
  attrib MV101    length=4 format=MV101_. label="Region";
  attrib MV102    length=4 format=MV102_. label="Type of place of residence";
  attrib MV103    length=4 label="Childhood place of residen- NA";
  attrib MV104    length=4 format=MV104_. label="Years lived in place of res.";
  attrib MV105    length=4 format=MV105_. label="Type of place of previous resi";
  attrib MV106    length=4 format=MV106_. label="Highest educational level";
  attrib MV107    length=4 label="Highest year of education";
  attrib MV108    length=4 format=MV108_. label="Literacy";
  attrib MV109    length=4 format=MV109_. label="Reads newspaper once a week";
  attrib MV110    length=4 format=MV110_. label="Watches TV every week";
  attrib MV111    length=4 label="Listens to radio every da - NA";
  attrib MV112    length=4 format=MV112_. label="Listens to radio every week";
  attrib MV130    length=4 format=MV130_. label="Religion";
  attrib MV131    length=4 label="Ethnicity - NA";
  attrib MV133    length=4 format=MV133_. label="Education in single years";
  attrib MV134    length=4 format=MV134_. label="De facto place of residence";
  attrib MV135    length=4 format=MV135_. label="Usual resident or visitor";
  attrib MV136    length=4 label="Number of household members";
  attrib MV138    length=4 label="Number of eligible men in HH";
  attrib MV148    length=4 format=MV148_. label="Respondent still in school";
  attrib MV149    length=4 format=MV149_. label="Educational attainment";
  attrib MV150    length=4 format=MV150_. label="Relationship to household head";
  attrib MV151    length=4 format=MV151_. label="Sex of household head";
  attrib MV152    length=4 label="Age of household head";
  attrib MV201    length=4 label="Total children ever born";
  attrib MV202    length=4 label="Sons at home";
  attrib MV203    length=4 label="Daughters at home";
  attrib MV204    length=4 label="Sons elsewhere";
  attrib MV205    length=4 label="Daughters elsewhere";
  attrib MV206    length=4 label="Sons who have died";
  attrib MV207    length=4 label="Daughters who have died";
  attrib MV213    length=4 label="Partner currently pregnan - NA";
  attrib MV218    length=4 label="Number of living children";
  attrib MV225    length=4 label="Current pregnancy wanted - NA";
  attrib MV301    length=4 format=MV301_. label="Knowledge of any method";
  attrib MV302    length=4 format=MV302_. label="Ever use of any method";
  attrib MV304A01 length=4 format=MV304A_. label="Type of method";
  attrib MV304_01 length=4 format=MV304_. label="Knows method";
  attrib MV305_01 length=4 format=MV305_. label="Ever used method";
  attrib MV304A02 length=4 format=MV304A_. label="Type of method";
  attrib MV304_02 length=4 format=MV304_. label="Knows method";
  attrib MV305_02 length=4 format=MV305_. label="Ever used method";
  attrib MV304A03 length=4 format=MV304A_. label="Type of method";
  attrib MV304_03 length=4 format=MV304_. label="Knows method";
  attrib MV305_03 length=4 format=MV305_. label="Ever used method";
  attrib MV304A04 length=4 format=MV304A_. label="Type of method";
  attrib MV304_04 length=4 format=MV304_. label="Knows method";
  attrib MV305_04 length=4 format=MV305_. label="Ever used method";
  attrib MV304A05 length=4 format=MV304A_. label="Type of method";
  attrib MV304_05 length=4 format=MV304_. label="Knows method";
  attrib MV305_05 length=4 format=MV305_. label="Ever used method";
  attrib MV304A06 length=4 format=MV304A_. label="Type of method";
  attrib MV304_06 length=4 format=MV304_. label="Knows method";
  attrib MV305_06 length=4 format=MV305_. label="Ever used method";
  attrib MV304A07 length=4 format=MV304A_. label="Type of method";
  attrib MV304_07 length=4 format=MV304_. label="Knows method";
  attrib MV305_07 length=4 format=MV305_. label="Ever used method";
  attrib MV304A08 length=4 format=MV304A_. label="Type of method";
  attrib MV304_08 length=4 format=MV304_. label="Knows method";
  attrib MV305_08 length=4 format=MV305_. label="Ever used method";
  attrib MV304A09 length=4 format=MV304A_. label="Type of method";
  attrib MV304_09 length=4 format=MV304_. label="Knows method";
  attrib MV305_09 length=4 format=MV305_. label="Ever used method";
  attrib MV304A10 length=4 format=MV304A_. label="Type of method";
  attrib MV304_10 length=4 format=MV304_. label="Knows method";
  attrib MV305_10 length=4 format=MV305_. label="Ever used method";
  attrib MV304A11 length=4 format=MV304A_. label="Type of method";
  attrib MV304_11 length=4 format=MV304_. label="Knows method";
  attrib MV305_11 length=4 format=MV305_. label="Ever used method";
  attrib MV304A12 length=4 format=MV304A_. label="Type of method";
  attrib MV304_12 length=4 format=MV304_. label="Knows method";
  attrib MV305_12 length=4 format=MV305_. label="Ever used method";
  attrib MV304A13 length=4 format=MV304A_. label="Type of method";
  attrib MV304_13 length=4 format=MV304_. label="Knows method";
  attrib MV305_13 length=4 format=MV305_. label="Ever used method";
  attrib MV304A14 length=4 format=MV304A_. label="Type of method";
  attrib MV304_14 length=4 format=MV304_. label="Knows method";
  attrib MV305_14 length=4 format=MV305_. label="Ever used method";
  attrib MV304A15 length=4 format=MV304A_. label="Type of method";
  attrib MV304_15 length=4 format=MV304_. label="Knows method";
  attrib MV305_15 length=4 format=MV305_. label="Ever used method";
  attrib MV312    length=4 format=MV312_. label="Current contraceptive method";
  attrib MV313    length=4 format=MV313_. label="Current use by method type";
  attrib MV361    length=4 format=MV361_. label="Pattern of use";
  attrib MV362    length=4 format=MV362_. label="Intention to use";
  attrib MV363    length=4 format=MV363_. label="Preferred future method";
  attrib MV364    length=4 format=MV364_. label="Contraceptive use & intention";
  attrib MV366A   length=4 label="Acceptability of radio me - NA";
  attrib MV366B   length=4 label="Acceptability of TV messa - NA";
  attrib MV367    length=4 label="Wanted last child - NA";
  attrib MV375A   length=4 format=MV375A_. label="Main reason not using a method";
  attrib MV376    length=4 format=MV376_. label="Main reason not to use a meth.";
  attrib MV384A   length=4 format=MV384A_. label="Heard FP on radio last month";
  attrib MV384B   length=4 format=MV384B_. label="Heard FP on TV last month";
  attrib MV384C   length=4 label="Heard FP newspaper last mon-NA";
  attrib MV384D   length=4 format=MV384D_. label="Heard FP on poster last month";
  attrib MV384E   length=4 label="Heard FP brochures last m - NA";
  attrib MV501    length=4 format=MV501_. label="Current marital status";
  attrib MV502    length=4 format=MV502_. label="Currently/formerly/never marr.";
  attrib MV503    length=4 format=MV503_. label="Number of unions";
  attrib MV505    length=4 label="Number of wives, partners";
  attrib MV507    length=4 label="Month of first marriage";
  attrib MV508    length=4 label="Year of first marriage";
  attrib MV509    length=4 label="Date of first marriage (CMC)";
  attrib MV510    length=4 format=MV510_. label="Completeness of date inform.";
  attrib MV511    length=4 label="Age at first marriage";
  attrib MV512    length=4 label="Years since first marriage";
  attrib MV513    length=4 format=MV513_. label="Marital duration (grouped)";
  attrib MV525    length=4 label="Age at first intercourse - NA";
  attrib MV527    length=4 format=MV527_. label="Time since last intercourse";
  attrib MV531    length=4 label="Age at first intercourse - NA";
  attrib MV532    length=4 format=MV532_. label="Flag for MV531";
  attrib MV534    length=4 label="Have a regular sex partne - NA";
  attrib MV535    length=4 label="Have ever been married - NA";
  attrib MV602    length=4 format=MV602_. label="Fertility preference";
  attrib MV603    length=4 format=MV603_. label="Preferred waiting time";
  attrib MV604    length=4 format=MV604_. label="Preferred waiting time (grpd)";
  attrib MV605    length=4 format=MV605_. label="Desire for more children";
  attrib MV610    length=4 format=MV610_. label="Wife approves FP";
  attrib MV611    length=4 format=MV611_. label="Discussed FP with partner";
  attrib MV612    length=4 format=MV612_. label="Respondent approves FP";
  attrib MV613    length=4 format=MV613_. label="Ideal number of children";
  attrib MV614    length=4 format=MV614_. label="Ideal number of children (grp)";
  attrib MV616    length=4 format=MV616_. label="Time for future birth";
  attrib MV621    length=4 format=MV621_. label="Wifes desire for children";
  attrib MV627    length=4 format=MV627_. label="Ideal number of boys";
  attrib MV628    length=4 format=MV628_. label="Ideal number of girls";
  attrib MV629    length=4 format=MV629_. label="Ideal number of either sex";
  attrib MV630A   length=4 label="Discussed FP with partner - NA";
  attrib MV630B   length=4 label="Discussed FP with mother - NA";
  attrib MV630C   length=4 label="Discussed FP with father - NA";
  attrib MV630D   length=4 label="Discussed FP with sister( - NA";
  attrib MV630E   length=4 label="Discussed FP with brother - NA";
  attrib MV630F   length=4 label="Discussed FP with daughte - NA";
  attrib MV630G   length=4 label="Discussed FP with mother- - NA";
  attrib MV630H   length=4 label="Discussed FP with friends - NA";
  attrib MV630I   length=4 label="Discussed FP -country spe - NA";
  attrib MV630J   length=4 label="Discussed FP -country spe - NA";
  attrib MV630K   length=4 label="Discussed FP -country spe - NA";
  attrib MV630L   length=4 label="Discussed FP -country spe - NA";
  attrib MV630X   length=4 label="Discussed FP with others - NA";
  attrib MV714    length=4 format=MV714_. label="Respondent currently working";
  attrib MV716    length=4 format=MV716_. label="Respondent's occupation";
  attrib MV717    length=4 format=MV717_. label="Respondent's occupation";
  attrib MV719    length=4 label="Work for family, others, - NA";
  attrib MV731    length=4 format=MV731_. label="Worked in last 12 months - NA";
  attrib MV732    length=4 label="Employment all year/seaso - NA";
  attrib MV733    length=4 label="Months worked in last 12 - NA";
  attrib MV736    length=8 label="Earnings for work - NA";
  attrib MV740    length=4 format=MV740_. label="Type of land where resp. works";
  attrib MV751    length=4 label="Ever heard of AIDS - NA";
  attrib MV752A   length=4 label="AIDS: radio - NA";
  attrib MV752B   length=4 label="AIDS: television - NA";
  attrib MV752C   length=4 label="AIDS: newspapers/magazine - NA";
  attrib MV752D   length=4 label="AIDS: pamphlets/posters - NA";
  attrib MV752E   length=4 label="AIDS: clinic/health worke - NA";
  attrib MV752F   length=4 label="AIDS: churches/mosques - NA";
  attrib MV752G   length=4 label="AIDS: schools/teachers - NA";
  attrib MV752H   length=4 label="AIDS: community meetings - NA";
  attrib MV752I   length=4 label="AIDS: friends/relatives - NA";
  attrib MV752J   length=4 label="AIDS: work place - NA";
  attrib MV752K   length=4 label="AIDS: country specific - NA";
  attrib MV752L   length=4 label="AIDS: country specific - NA";
  attrib MV752M   length=4 label="AIDS: country specific - NA";
  attrib MV752N   length=4 label="AIDS: country specific - NA";
  attrib MV752O   length=4 label="AIDS: country specific - NA";
  attrib MV752X   length=4 label="AIDS: other responses - NA";
  attrib MV753    length=4 label="Ways to avoid AIDS - NA";
  attrib MV754A   length=4 label="AIDS: Safe sex - NA";
  attrib MV754B   length=4 label="AIDS: abstain from sex - NA";
  attrib MV754C   length=4 label="AIDS: use condoms during - NA";
  attrib MV754D   length=4 label="AIDS: only one sex partne - NA";
  attrib MV754E   length=4 label="AIDS: avoid sex prostitut - NA";
  attrib MV754F   length=4 label="AIDS: avoid sex homosexua - NA";
  attrib MV754G   length=4 label="AIDS: avoid blood transfu - NA";
  attrib MV754H   length=4 label="AIDS: avoid injections - NA";
  attrib MV754I   length=4 label="AIDS: avoid kissing - NA";
  attrib MV754J   length=4 label="AIDS: avoid mosquito bite - NA";
  attrib MV754K   length=4 label="AIDS: protection trad. he - NA";
  attrib MV754L   length=4 label="AIDS: country specific - NA";
  attrib MV754M   length=4 label="AIDS: country specific - NA";
  attrib MV754N   length=4 label="AIDS: country specific - NA";
  attrib MV754O   length=4 label="AIDS: country specific - NA";
  attrib MV754X   length=4 label="AIDS: other responses - NA";
  attrib MV754Z   length=4 label="AIDS: DK ways to avoid AI - NA";
  attrib MV755B   length=4 label="Safe sex: abstain from se - NA";
  attrib MV755C   length=4 label="Safe sex: use condoms dur - NA";
  attrib MV755D   length=4 label="Safe sex: only one partne - NA";
  attrib MV755E   length=4 label="Safe sex: avoid prostitut - NA";
  attrib MV755F   length=4 label="Safe sex: avoid homosexua - NA";
  attrib MV755L   length=4 label="Safe sex: country specifi - NA";
  attrib MV755M   length=4 label="Safe sex: country specifi - NA";
  attrib MV755N   length=4 label="Safe sex: country specifi - NA";
  attrib MV755O   length=4 label="Safe sex: country specifi - NA";
  attrib MV755X   length=4 label="Safe sex: other responses - NA";
  attrib MV755Z   length=4 label="Safe sex: DK meaning - NA";
  attrib MV756    length=4 label="Can a healthy person have - NA";
  attrib MV757    length=4 label="Is AIDS a fatal disease - NA";
  attrib MV758    length=4 label="Chances of getting AIDS - NA";
  attrib MV760A   length=4 label="Behavior: Did not start s - NA";
  attrib MV760B   length=4 label="Behavior: Stopped all sex - NA";
  attrib MV760C   length=4 label="Behavior: Started using c - NA";
  attrib MV760D   length=4 label="Behavior: Only one sex pa - NA";
  attrib MV760E   length=4 label="Behavior: Reduced partner - NA";
  attrib MV760F   length=4 label="Behavior: Ask spouse fait - NA";
  attrib MV760G   length=4 label="Behavior: No homosexual c - NA";
  attrib MV760I   length=4 label="Behavior: Stopped injecti - NA";
  attrib MV760L   length=4 label="Behavior: Country specifi - NA";
  attrib MV760M   length=4 label="Behavior: Country specifi - NA";
  attrib MV760N   length=4 label="Behavior: Country specifi - NA";
  attrib MV760O   length=4 label="Behavior: Country specifi - NA";
  attrib MV760P   length=4 label="Behavior: Avoid sex w. pr - NA";
  attrib MV760V   length=4 label="Behavior: No nonsexual ch - NA";
  attrib MV760W   length=4 label="Behavior: Other nonsexual - NA";
  attrib MV760X   length=4 label="Behavior: Other responses - NA";
  attrib MV760Y   length=4 label="Behavior: No change - NA";
  attrib MV760Z   length=4 label="Behavior: Don't know - NA";
  attrib MV761    length=4 label="Last intercourse used con - NA";
  attrib MV762    length=4 format=MV762_. label="Source for condoms";
  attrib MV763    length=4 label="Partners in last 12 month - NA";
  attrib MV764    length=4 label="Knowledge of condom - NA";
  attrib MV765    length=4 label="Ever use of condom - NA";
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
  attrib MSDISTR  length=4 label="District of residence";
  attrib MSFORMAT length=4 format=MSFORMA_. label="Type of date format used";
  attrib MSADJUST length=4 format=MSADJUS_. label="Month of interview adjusted 1m";
  attrib MSINTY   length=4 label="Year of interview(unadjusted)";
  attrib MSINTM   length=4 label="Month of interview(unadjusted)";
  attrib MS304_01 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_02 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_03 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_04 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_05 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_06 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_07 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_08 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_09 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_10 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_11 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_12 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_13 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_14 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_15 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_16 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_17 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_18 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_19 length=4 format=MS304_. label="Knows source of method";
  attrib MS304_20 length=4 format=MS304_. label="Knows source of method";
  attrib MS309    length=4 format=MS309_. label="First method used";
  attrib MS311    length=4 label="Living children at first use";
  attrib MS316    length=4 format=MS316_. label="Currently using second method";
  attrib MS317    length=4 format=MS317_. label="Second method currently used";
  attrib MS320S   length=4 format=MS320S_. label="Condom package seen";
  attrib MS320    length=4 format=MS320_. label="Brand of condoms shown";
  attrib MS320A   length=4 format=MS320A_. label="Reason can't show condom pack";
  attrib MS320C   length=4 format=MS320C_. label="Reason can't show condom pack";
  attrib MS320E   length=4 label="Times used condoms last month";
  attrib MS320D   length=4 format=MS320D_. label="Use condom every time have sex";
  attrib MS321M   length=4 label="Month of sterilization (imp.)";
  attrib MS321Y   length=4 label="Year of sterilization (imp.)";
  attrib MS321C   length=4 label="Date of sterilization (CMC)";
  attrib MS321F   length=4 format=MS321F_. label="Completeness of information";
  attrib MS321XA  length=4 format=MS321XA_. label="Years since sterilization";
  attrib MS321XB  length=4 format=MS321XB_. label="Age at sterilization";
  attrib MS321XC  length=4 format=MS321XC_. label="Marital duration at steriliz.";
  attrib MS321XD  length=4 format=MS321XD_. label="Wife's sterilization:date form";
  attrib MS321XE  length=4 format=MS321XE_. label="Wife's sterilization : month";
  attrib MS321XF  length=4 format=MS321XF_. label="Wife's sterilization: year";
  attrib MS322B   length=4 format=MS322B_. label="Sterilization regret";
  attrib MS322C   length=4 format=MS322C_. label="Method used most recently";
  attrib MS323    length=4 format=MS323_. label="Source of current method";
  attrib MS323B   length=4 format=MS323B_. label="Paid for use of contr.source";
  attrib MS325    length=4 format=MS325_. label="Who obtained pills/condoms";
  attrib MS325A   length=4 format=MS325A_. label="Got pls,cndm at HF,doctor,shop";
  attrib MS325B   length=4 format=MS325B_. label="Informed of side effects/probs";
  attrib MS325C   length=4 format=MS325C_. label="Informed of other methods";
  attrib MS325H   length=4 format=MS325H_. label="Got method he wanted";
  attrib MS325I   length=4 format=MS325I_. label="Method wanted at HF.";
  attrib MS325J   length=4 format=MS325J_. label="Who had most inflc. at 1st use";
  attrib MS326    length=4 format=MS326_. label="Reason for current method";
  attrib MS328HI  length=4 format=MS328HI_. label="Knows source for method";
  attrib MS347A   length=4 format=MS347A_. label="Satellite clinic last 3 months";
  attrib MS347B   length=4 format=MS347B_. label="Visited satellite clinic";
  attrib MS347CA  length=4 format=MS347CA_. label="Sat.Clinic: Family Plng.Meths.";
  attrib MS347CB  length=4 format=MS347CB_. label="Sat.Clinic: Immunization";
  attrib MS347CC  length=4 format=MS347CC_. label="Sat.Clinic: Ch.growth monitor.";
  attrib MS347CD  length=4 format=MS347CD_. label="Sat.Clinic: Other";
  attrib MS347CE  length=4 format=MS347CE_. label="Sat.Clinic: Does not know";
  attrib MS348C   length=4 format=MS348C_. label="Read about FP on Billboard";
  attrib MS349A   length=4 format=MS349A_. label="Inf. on radio acceptable:pill";
  attrib MS349B   length=4 format=MS349B_. label="Inf. on radio acct'ble:condoms";
  attrib MS349C   length=4 format=MS349C_. label="Inf. on radio acct'ble:injctns";
  attrib MS349D   length=4 format=MS349D_. label="Inf. on radio acct'ble:IUD's";
  attrib MS349E   length=4 format=MS349E_. label="Inf. on radio acct'ble:sterlzn";
  attrib MS350    length=4 format=MS350_. label="Visit by FPW in last 6 mnths";
  attrib MS352    length=4 format=MS352_. label="No. visits by FPW last 6 mnths";
  attrib MS353    length=4 format=MS353_. label="Months since last visit by FPW";
  attrib MS354    length=4 format=MS354_. label="Received supplies from FPW";
  attrib MS355    length=4 format=MS355_. label="Supplies received from FPW";
  attrib MS356    length=4 label="How many cycles/condoms rec'vd";
  attrib MS357A   length=4 format=MS357A_. label="Meths.disc.w FPW: Pills";
  attrib MS357B   length=4 format=MS357B_. label="Meths.disc.w FPW: IUD";
  attrib MS357C   length=4 format=MS357C_. label="Meths.disc.w FPW: Injection";
  attrib MS357D   length=4 format=MS357D_. label="Meths.disc.w FPW: Condom";
  attrib MS357E   length=4 format=MS357E_. label="Meths.disc.w FPW: Female Ster.";
  attrib MS357F   length=4 format=MS357F_. label="Meths.disc.w FPW: Male Ster.";
  attrib MS357G   length=4 format=MS357G_. label="Meths.disc.w FPW: Never disc.";
  attrib MS358    length=4 format=MS358_. label="Most men known by rsp use FP";
  attrib MS359    length=4 format=MS359_. label="Has recommended FP to someone";
  attrib MS360    length=4 format=MS360_. label="Visit Health Facility last 6m";
  attrib MS361    length=4 format=MS361_. label="Spoke with psn at HF about FP";
  attrib MS408A   length=4 format=MS408A_. label="Children under 10";
  attrib MS408B   length=4 format=MS408B_. label="Presence of wife";
  attrib MS408C   length=4 format=MS408C_. label="Other males";
  attrib MS408D   length=4 format=MS408D_. label="Other females";
  attrib MS508    length=4 format=MS508_. label="Source of preferred method";
  attrib MS512    length=4 format=MS512_. label="Discussed children";
  attrib MS514    length=4 format=MS514_. label="Wife allowed to go out alone";
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
Data BDCR31FL;
infile 'BDCR31FL.DAT' LRECL=2205 MISSOVER ;
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
    @1067 V401     1.0
    @1068 V404     1.0
    @1069 V405     1.0
    @1070 V406     1.0
    @1071 V407     2.0
    @1073 V408     2.0
    @1075 V409     1.0
    @1076 V409A    1.0
    @1077 V410     1.0
    @1078 V410A    1.0
    @1079 V411     1.0
    @1080 V411A    1.0
    @1081 V412     1.0
    @1082 V413A    1.0
    @1083 V413B    1.0
    @1084 V413C    1.0
    @1085 V413D    1.0
    @1086 V413     1.0
    @1087 V414A    1.0
    @1088 V414B    1.0
    @1089 V414C    1.0
    @1090 V414D    1.0
    @1091 V414E    1.0
    @1092 V414F    1.0
    @1093 V414G    1.0
    @1094 V414H    1.0
    @1095 V415     1.0
    @1096 V416     1.0
    @1097 V417     1.0
    @1098 V418     1.0
    @1099 V419     1.0
    @1100 V420     2.0
    @1102 V421     2.0
    @1104 V426     3.0
    @1107 V436     3.0
    @1110 V437     4.0
    @1114 V438     4.0
    @1118 V439     4.0
    @1122 V440     4.0
    @1126 V441     5.0
    @1131 V442     5.0
    @1136 V443     5.0
    @1141 V444     5.0
    @1146 V444A    4.0
    @1150 V445     4.0
    @1154 V446     4.0
    @1158 V447     1.0
    @1159 V448     1.0
    @1160 V449     1.0
    @1161 V450A    1.0
    @1162 V450B    1.0
    @1163 V450C    1.0
    @1164 V450D    1.0
    @1165 V450E    1.0
    @1166 V450F    1.0
    @1167 V450G    1.0
    @1168 V450H    1.0
    @1169 V450I    1.0
    @1170 V450J    1.0
    @1171 V450K    1.0
    @1172 V450L    1.0
    @1173 V450M    1.0
    @1174 V450X    1.0
    @1175 V450Z    1.0
    @1176 V451A    1.0
    @1177 V451B    1.0
    @1178 V451C    1.0
    @1179 V451D    1.0
    @1180 V451E    1.0
    @1181 V451F    1.0
    @1182 V451G    1.0
    @1183 V451H    1.0
    @1184 V451I    1.0
    @1185 V451J    1.0
    @1186 V451K    1.0
    @1187 V451X    1.0
    @1188 V451Z    1.0
    @1189 V501     1.0
    @1190 V502     1.0
    @1191 V503     1.0
    @1192 V504     1.0
    @1193 V505     2.0
    @1195 V506     2.0
    @1197 V507     2.0
    @1199 V508     2.0
    @1201 V509     4.0
    @1205 V510     1.0
    @1206 V511     2.0
    @1208 V512     2.0
    @1210 V513     1.0
    @1211 V525     2.0
    @1213 V527     3.0
    @1216 V528     2.0
    @1218 V529     3.0
    @1221 V530     1.0
    @1222 V531     2.0
    @1224 V532     1.0
    @1225 V534     1.0
    @1226 V535     1.0
    @1227 V602     1.0
    @1228 V603     3.0
    @1231 V604     1.0
    @1232 V605     1.0
    @1233 V606     1.0
    @1234 V610     1.0
    @1235 V611     1.0
    @1236 V612     1.0
    @1237 V613     2.0
    @1239 V614     1.0
    @1240 V616     3.0
    @1243 V618     1.0
    @1244 V621     1.0
    @1245 V623     1.0
    @1246 V624     2.0
    @1248 V625     1.0
    @1249 V626     2.0
    @1251 V627     2.0
    @1253 V628     2.0
    @1255 V629     2.0
    @1257 V630A    1.0
    @1258 V630B    1.0
    @1259 V630C    1.0
    @1260 V630D    1.0
    @1261 V630E    1.0
    @1262 V630F    1.0
    @1263 V630G    1.0
    @1264 V630H    1.0
    @1265 V630I    1.0
    @1266 V630J    1.0
    @1267 V630K    1.0
    @1268 V630L    1.0
    @1269 V630X    1.0
    @1270 V701     1.0
    @1271 V702     2.0
    @1273 V704     3.0
    @1276 V705     2.0
    @1278 V707     1.0
    @1279 V714     1.0
    @1280 V715     2.0
    @1282 V716     3.0
    @1285 V717     2.0
    @1287 V718     1.0
    @1288 V719     1.0
    @1289 V720     1.0
    @1290 V721     1.0
    @1291 V723     2.0
    @1293 V729     1.0
    @1294 V730     2.0
    @1296 V731     1.0
    @1297 V732     1.0
    @1298 V733     2.0
    @1300 V734     1.0
    @1301 V735     3.0
    @1304 V736     8.0
    @1312 V737     7.0
    @1319 V738     7.0
    @1326 V739     1.0
    @1327 V740     1.0
    @1328 V751     1.0
    @1329 V752A    1.0
    @1330 V752B    1.0
    @1331 V752C    1.0
    @1332 V752D    1.0
    @1333 V752E    1.0
    @1334 V752F    1.0
    @1335 V752G    1.0
    @1336 V752H    1.0
    @1337 V752I    1.0
    @1338 V752J    1.0
    @1339 V752K    1.0
    @1340 V752L    1.0
    @1341 V752M    1.0
    @1342 V752N    1.0
    @1343 V752O    1.0
    @1344 V752X    1.0
    @1345 V753     1.0
    @1346 V754A    1.0
    @1347 V754B    1.0
    @1348 V754C    1.0
    @1349 V754D    1.0
    @1350 V754E    1.0
    @1351 V754F    1.0
    @1352 V754G    1.0
    @1353 V754H    1.0
    @1354 V754I    1.0
    @1355 V754J    1.0
    @1356 V754K    1.0
    @1357 V754L    1.0
    @1358 V754M    1.0
    @1359 V754N    1.0
    @1360 V754O    1.0
    @1361 V754X    1.0
    @1362 V754Z    1.0
    @1363 V755B    1.0
    @1364 V755C    1.0
    @1365 V755D    1.0
    @1366 V755E    1.0
    @1367 V755F    1.0
    @1368 V755L    1.0
    @1369 V755M    1.0
    @1370 V755N    1.0
    @1371 V755O    1.0
    @1372 V755X    1.0
    @1373 V755Z    1.0
    @1374 V756     1.0
    @1375 V757     1.0
    @1376 V758     1.0
    @1377 V760A    1.0
    @1378 V760B    1.0
    @1379 V760C    1.0
    @1380 V760D    1.0
    @1381 V760E    1.0
    @1382 V760F    1.0
    @1383 V760G    1.0
    @1384 V760I    1.0
    @1385 V760L    1.0
    @1386 V760M    1.0
    @1387 V760N    1.0
    @1388 V760O    1.0
    @1389 V760P    1.0
    @1390 V760V    1.0
    @1391 V760W    1.0
    @1392 V760X    1.0
    @1393 V760Y    1.0
    @1394 V760Z    1.0
    @1395 V761     1.0
    @1396 V762     2.0
    @1398 V764     1.0
    @1399 V765     1.0
    @1400 V801     4.0
    @1404 V802     4.0
    @1408 V803     2.0
    @1410 V804     1.0
    @1411 V805     3.0
    @1414 V806     2.0
    @1416 V811     1.0
    @1417 V812     1.0
    @1418 V813     1.0
    @1419 V814     1.0
    @1420 MCASEID  $15.
    @1435 MV000    $3.
    @1438 MV001    8.0
    @1446 MV002    4.0
    @1450 MV003    3.0
    @1453 MV004    4.0
    @1457 MV005    8.0
    @1465 MV006    2.0
    @1467 MV007    2.0
    @1469 MV008    4.0
    @1473 MV009    2.0
    @1475 MV010    2.0
    @1477 MV011    4.0
    @1481 MV012    2.0
    @1483 MV013    2.0
    @1485 MV014    1.0
    @1486 MV015    1.0
    @1487 MV016    2.0
    @1489 MV021    4.0
    @1493 MV022    4.0
    @1497 MV023    2.0
    @1499 MV024    2.0
    @1501 MV025    1.0
    @1502 MV026    1.0
    @1503 MV027    1.0
    @1504 MV028    3.0
    @1507 MV029    2.0
    @1509 MV030    2.0
    @1511 MV031    2.0
    @1513 MV032    2.0
    @1515 MV033    8.0
    @1523 MV034_1  2.0
    @1525 MV034_2  2.0
    @1527 MV034_3  2.0
    @1529 MV034_4  2.0
    @1531 MV034_5  2.0
    @1533 MV034_6  2.0
    @1535 MV034_7  2.0
    @1537 MV034_8  2.0
    @1539 MV035    1.0
    @1540 MV801    4.0
    @1544 MV802    4.0
    @1548 MV803    2.0
    @1550 MV101    2.0
    @1552 MV102    1.0
    @1553 MV103    1.0
    @1554 MV104    2.0
    @1556 MV105    1.0
    @1557 MV106    1.0
    @1558 MV107    2.0
    @1560 MV108    1.0
    @1561 MV109    1.0
    @1562 MV110    1.0
    @1563 MV111    1.0
    @1564 MV112    1.0
    @1565 MV130    1.0
    @1566 MV131    2.0
    @1568 MV133    2.0
    @1570 MV134    1.0
    @1571 MV135    1.0
    @1572 MV136    2.0
    @1574 MV138    2.0
    @1576 MV148    1.0
    @1577 MV149    1.0
    @1578 MV150    2.0
    @1580 MV151    1.0
    @1581 MV152    2.0
    @1583 MV201    2.0
    @1585 MV202    2.0
    @1587 MV203    2.0
    @1589 MV204    2.0
    @1591 MV205    2.0
    @1593 MV206    2.0
    @1595 MV207    2.0
    @1597 MV213    1.0
    @1598 MV218    2.0
    @1600 MV225    1.0
    @1601 MV301    1.0
    @1602 MV302    1.0
    @1603 MV304A01 1.0
    @1604 MV304_01 1.0
    @1605 MV305_01 1.0
    @1606 MV304A02 1.0
    @1607 MV304_02 1.0
    @1608 MV305_02 1.0
    @1609 MV304A03 1.0
    @1610 MV304_03 1.0
    @1611 MV305_03 1.0
    @1612 MV304A04 1.0
    @1613 MV304_04 1.0
    @1614 MV305_04 1.0
    @1615 MV304A05 1.0
    @1616 MV304_05 1.0
    @1617 MV305_05 1.0
    @1618 MV304A06 1.0
    @1619 MV304_06 1.0
    @1620 MV305_06 1.0
    @1621 MV304A07 1.0
    @1622 MV304_07 1.0
    @1623 MV305_07 1.0
    @1624 MV304A08 1.0
    @1625 MV304_08 1.0
    @1626 MV305_08 1.0
    @1627 MV304A09 1.0
    @1628 MV304_09 1.0
    @1629 MV305_09 1.0
    @1630 MV304A10 1.0
    @1631 MV304_10 1.0
    @1632 MV305_10 1.0
    @1633 MV304A11 1.0
    @1634 MV304_11 1.0
    @1635 MV305_11 1.0
    @1636 MV304A12 1.0
    @1637 MV304_12 1.0
    @1638 MV305_12 1.0
    @1639 MV304A13 1.0
    @1640 MV304_13 1.0
    @1641 MV305_13 1.0
    @1642 MV304A14 1.0
    @1643 MV304_14 1.0
    @1644 MV305_14 1.0
    @1645 MV304A15 1.0
    @1646 MV304_15 1.0
    @1647 MV305_15 1.0
    @1648 MV312    2.0
    @1650 MV313    1.0
    @1651 MV361    1.0
    @1652 MV362    1.0
    @1653 MV363    2.0
    @1655 MV364    1.0
    @1656 MV366A   1.0
    @1657 MV366B   1.0
    @1658 MV367    1.0
    @1659 MV375A   2.0
    @1661 MV376    2.0
    @1663 MV384A   1.0
    @1664 MV384B   1.0
    @1665 MV384C   1.0
    @1666 MV384D   1.0
    @1667 MV384E   1.0
    @1668 MV501    1.0
    @1669 MV502    1.0
    @1670 MV503    1.0
    @1671 MV505    2.0
    @1673 MV507    2.0
    @1675 MV508    2.0
    @1677 MV509    4.0
    @1681 MV510    1.0
    @1682 MV511    2.0
    @1684 MV512    2.0
    @1686 MV513    1.0
    @1687 MV525    2.0
    @1689 MV527    3.0
    @1692 MV531    2.0
    @1694 MV532    1.0
    @1695 MV534    1.0
    @1696 MV535    1.0
    @1697 MV602    1.0
    @1698 MV603    3.0
    @1701 MV604    1.0
    @1702 MV605    1.0
    @1703 MV610    1.0
    @1704 MV611    1.0
    @1705 MV612    1.0
    @1706 MV613    2.0
    @1708 MV614    1.0
    @1709 MV616    3.0
    @1712 MV621    1.0
    @1713 MV627    2.0
    @1715 MV628    2.0
    @1717 MV629    2.0
    @1719 MV630A   1.0
    @1720 MV630B   1.0
    @1721 MV630C   1.0
    @1722 MV630D   1.0
    @1723 MV630E   1.0
    @1724 MV630F   1.0
    @1725 MV630G   1.0
    @1726 MV630H   1.0
    @1727 MV630I   1.0
    @1728 MV630J   1.0
    @1729 MV630K   1.0
    @1730 MV630L   1.0
    @1731 MV630X   1.0
    @1732 MV714    1.0
    @1733 MV716    3.0
    @1736 MV717    2.0
    @1738 MV719    1.0
    @1739 MV731    1.0
    @1740 MV732    1.0
    @1741 MV733    2.0
    @1743 MV736    8.0
    @1751 MV740    1.0
    @1752 MV751    1.0
    @1753 MV752A   1.0
    @1754 MV752B   1.0
    @1755 MV752C   1.0
    @1756 MV752D   1.0
    @1757 MV752E   1.0
    @1758 MV752F   1.0
    @1759 MV752G   1.0
    @1760 MV752H   1.0
    @1761 MV752I   1.0
    @1762 MV752J   1.0
    @1763 MV752K   1.0
    @1764 MV752L   1.0
    @1765 MV752M   1.0
    @1766 MV752N   1.0
    @1767 MV752O   1.0
    @1768 MV752X   1.0
    @1769 MV753    1.0
    @1770 MV754A   1.0
    @1771 MV754B   1.0
    @1772 MV754C   1.0
    @1773 MV754D   1.0
    @1774 MV754E   1.0
    @1775 MV754F   1.0
    @1776 MV754G   1.0
    @1777 MV754H   1.0
    @1778 MV754I   1.0
    @1779 MV754J   1.0
    @1780 MV754K   1.0
    @1781 MV754L   1.0
    @1782 MV754M   1.0
    @1783 MV754N   1.0
    @1784 MV754O   1.0
    @1785 MV754X   1.0
    @1786 MV754Z   1.0
    @1787 MV755B   1.0
    @1788 MV755C   1.0
    @1789 MV755D   1.0
    @1790 MV755E   1.0
    @1791 MV755F   1.0
    @1792 MV755L   1.0
    @1793 MV755M   1.0
    @1794 MV755N   1.0
    @1795 MV755O   1.0
    @1796 MV755X   1.0
    @1797 MV755Z   1.0
    @1798 MV756    1.0
    @1799 MV757    1.0
    @1800 MV758    1.0
    @1801 MV760A   1.0
    @1802 MV760B   1.0
    @1803 MV760C   1.0
    @1804 MV760D   1.0
    @1805 MV760E   1.0
    @1806 MV760F   1.0
    @1807 MV760G   1.0
    @1808 MV760I   1.0
    @1809 MV760L   1.0
    @1810 MV760M   1.0
    @1811 MV760N   1.0
    @1812 MV760O   1.0
    @1813 MV760P   1.0
    @1814 MV760V   1.0
    @1815 MV760W   1.0
    @1816 MV760X   1.0
    @1817 MV760Y   1.0
    @1818 MV760Z   1.0
    @1819 MV761    1.0
    @1820 MV762    2.0
    @1822 MV763    2.0
    @1824 MV764    1.0
    @1825 MV765    1.0
    @1826 SDFORMAT 1.0
    @1827 SADJUST  1.0
    @1828 SINTM    2.0
    @1830 SINTY    2.0
    @1832 SDIV     1.0
    @1833 SDISTR   2.0
    @1835 S114A    1.0
    @1836 S114B    1.0
    @1837 S114C    1.0
    @1838 S114D    1.0
    @1839 S114E    1.0
    @1840 S119     3.0
    @1843 S119A    3.0
    @1846 S123     2.0
    @1848 S124A    1.0
    @1849 S124B    1.0
    @1850 S124C    1.0
    @1851 S124D    1.0
    @1852 S124I    1.0
    @1853 S229M    2.0
    @1855 S229Y    4.0
    @1859 S233A    1.0
    @1860 S233B    1.0
    @1861 S233C    1.0
    @1862 S233DM   2.0
    @1864 S233DY   4.0
    @1868 MSDISTR  2.0
    @1870 MSFORMAT 1.0
    @1871 MSADJUST 1.0
    @1872 MSINTY   2.0
    @1874 MSINTM   2.0
    @1876 MS304_01 1.0
    @1877 MS304_02 1.0
    @1878 MS304_03 1.0
    @1879 MS304_04 1.0
    @1880 MS304_05 1.0
    @1881 MS304_06 1.0
    @1882 MS304_07 1.0
    @1883 MS304_08 1.0
    @1884 MS304_09 1.0
    @1885 MS304_10 1.0
    @1886 MS304_11 1.0
    @1887 MS304_12 1.0
    @1888 MS304_13 1.0
    @1889 MS304_14 1.0
    @1890 MS304_15 1.0
    @1891 MS304_16 1.0
    @1892 MS304_17 1.0
    @1893 MS304_18 1.0
    @1894 MS304_19 1.0
    @1895 MS304_20 1.0
    @1896 MS309    2.0
    @1898 MS311    2.0
    @1900 MS316    1.0
    @1901 MS317    2.0
    @1903 MS320S   1.0
    @1904 MS320    2.0
    @1906 MS320A   1.0
    @1907 MS320C   4.0
    @1911 MS320E   2.0
    @1913 MS320D   1.0
    @1914 MS321M   2.0
    @1916 MS321Y   2.0
    @1918 MS321C   4.0
    @1922 MS321F   1.0
    @1923 MS321XA  1.0
    @1924 MS321XB  1.0
    @1925 MS321XC  1.0
    @1926 MS321XD  1.0
    @1927 MS321XE  2.0
    @1929 MS321XF  4.0
    @1933 MS322B   1.0
    @1934 MS322C   2.0
    @1936 MS323    2.0
    @1938 MS323B   1.0
    @1939 MS325    1.0
    @1940 MS325A   1.0
    @1941 MS325B   1.0
    @1942 MS325C   1.0
    @1943 MS325H   1.0
    @1944 MS325I   2.0
    @1946 MS325J   1.0
    @1947 MS326    2.0
    @1949 MS328HI  2.0
    @1951 MS347A   1.0
    @1952 MS347B   1.0
    @1953 MS347CA  1.0
    @1954 MS347CB  1.0
    @1955 MS347CC  1.0
    @1956 MS347CD  1.0
    @1957 MS347CE  1.0
    @1958 MS348C   1.0
    @1959 MS349A   1.0
    @1960 MS349B   1.0
    @1961 MS349C   1.0
    @1962 MS349D   1.0
    @1963 MS349E   1.0
    @1964 MS350    1.0
    @1965 MS352    2.0
    @1967 MS353    1.0
    @1968 MS354    1.0
    @1969 MS355    1.0
    @1970 MS356    2.0
    @1972 MS357A   1.0
    @1973 MS357B   1.0
    @1974 MS357C   1.0
    @1975 MS357D   1.0
    @1976 MS357E   1.0
    @1977 MS357F   1.0
    @1978 MS357G   1.0
    @1979 MS358    1.0
    @1980 MS359    1.0
    @1981 MS360    1.0
    @1982 MS361    1.0
    @1983 MS408A   1.0
    @1984 MS408B   1.0
    @1985 MS408C   1.0
    @1986 MS408D   1.0
    @1987 MS508    2.0
    @1989 MS512    1.0
    @1990 MS514    1.0
    @1991 S304_01  1.0
    @1992 S304_02  1.0
    @1993 S304_03  1.0
    @1994 S304_04  1.0
    @1995 S304_05  1.0
    @1996 S304_06  1.0
    @1997 S304_07  1.0
    @1998 S304_08  1.0
    @1999 S304_09  1.0
    @2000 S304_10  1.0
    @2001 S304_11  1.0
    @2002 S304_12  1.0
    @2003 S304_13  1.0
    @2004 S304_14  1.0
    @2005 S304_15  1.0
    @2006 S309     2.0
    @2008 S310     2.0
    @2010 S316     1.0
    @2011 S317     2.0
    @2013 S318B    1.0
    @2014 S318C    2.0
    @2016 S318EA   1.0
    @2017 S318EB   1.0
    @2018 S318EC   1.0
    @2019 S318ED   1.0
    @2020 S318EE   1.0
    @2021 S318EF   1.0
    @2022 S318EG   1.0
    @2023 S318F    2.0
    @2025 S318HA   1.0
    @2026 S318HB   1.0
    @2027 S318HC   1.0
    @2028 S318HD   1.0
    @2029 S318HE   1.0
    @2030 S318HF   1.0
    @2031 S318HG   1.0
    @2032 S318HH   1.0
    @2033 S318HI   1.0
    @2034 S318I    1.0
    @2035 S318K    1.0
    @2036 S319     2.0
    @2038 S319B    1.0
    @2039 S320S    1.0
    @2040 S320     2.0
    @2042 S320A    1.0
    @2043 S320C    1.0
    @2044 S322C    2.0
    @2046 S320D    2.0
    @2048 S322D    2.0
    @2050 S323B    1.0
    @2051 S325     1.0
    @2052 S325A    1.0
    @2053 S325B    1.0
    @2054 S325C    1.0
    @2055 S325E    1.0
    @2056 S325G    1.0
    @2057 S325H    1.0
    @2058 S325I    2.0
    @2060 S325J    1.0
    @2061 S326     2.0
    @2063 S327     1.0
    @2064 S328PA   1.0
    @2065 S328PB   1.0
    @2066 S328PC   1.0
    @2067 S328PD   1.0
    @2068 S328PE   1.0
    @2069 S328PF   1.0
    @2070 S328PG   1.0
    @2071 S328PH   1.0
    @2072 S328PI   1.0
    @2073 S328PJ   1.0
    @2074 S328PK   1.0
    @2075 S328A    1.0
    @2076 S328B    1.0
    @2077 S328C    1.0
    @2078 S328DA   1.0
    @2079 S328DB   1.0
    @2080 S328DC   1.0
    @2081 S328DD   1.0
    @2082 S328DE   1.0
    @2083 S328DF   1.0
    @2084 S328DG   1.0
    @2085 S328DH   1.0
    @2086 S328DI   1.0
    @2087 S347A    1.0
    @2088 S347B    1.0
    @2089 S347CA   1.0
    @2090 S347CB   1.0
    @2091 S347CC   1.0
    @2092 S347CD   1.0
    @2093 S347CE   1.0
    @2094 S348C    1.0
    @2095 S349A    1.0
    @2096 S349B    1.0
    @2097 S349C    1.0
    @2098 S349D    1.0
    @2099 S349E    1.0
    @2100 S352     2.0
    @2102 S353     1.0
    @2103 S354     1.0
    @2104 S355     1.0
    @2105 S356     2.0
    @2107 S357MA   1.0
    @2108 S357MB   1.0
    @2109 S357MC   1.0
    @2110 S357MD   1.0
    @2111 S357ME   1.0
    @2112 S357MF   1.0
    @2113 S357MG   1.0
    @2114 S357A    1.0
    @2115 S357B    1.0
    @2116 S358     1.0
    @2117 S359     1.0
    @2118 S361     1.0
    @2119 S439     1.0
    @2120 S440     2.0
    @2122 S441     2.0
    @2124 S442A    1.0
    @2125 S442B_1  2.0
    @2127 S442C_1  1.0
    @2128 S442DA_1 1.0
    @2129 S442DB_1 1.0
    @2130 S442DC_1 1.0
    @2131 S442DD_1 1.0
    @2132 S442DE_1 1.0
    @2133 S442DF_1 1.0
    @2134 S442DG_1 1.0
    @2135 S442DH_1 1.0
    @2136 S442DI_1 1.0
    @2137 S442DJ_1 1.0
    @2138 S442DK_1 1.0
    @2139 S442EM_1 2.0
    @2141 S442FM_1 2.0
    @2143 S442EM_1 2.0
    @2145 S442FM_1 2.0
    @2147 S442B_2  2.0
    @2149 S442C_2  1.0
    @2150 S442DA_2 1.0
    @2151 S442DB_2 1.0
    @2152 S442DC_2 1.0
    @2153 S442DD_2 1.0
    @2154 S442DE_2 1.0
    @2155 S442DF_2 1.0
    @2156 S442DG_2 1.0
    @2157 S442DH_2 1.0
    @2158 S442DI_2 1.0
    @2159 S442DJ_2 1.0
    @2160 S442DK_2 1.0
    @2161 S442EM_2 2.0
    @2163 S442FM_2 2.0
    @2165 S442EM_2 2.0
    @2167 S442FM_2 2.0
    @2169 S442H    2.0
    @2171 S442IA   1.0
    @2172 S442IB   1.0
    @2173 S442IC   1.0
    @2174 S442ID   1.0
    @2175 S442IE   1.0
    @2176 S442IF   1.0
    @2177 S442IG   1.0
    @2178 S442J    2.0
    @2180 S442KA   1.0
    @2181 S442KB   1.0
    @2182 S442KC   1.0
    @2183 S442KD   1.0
    @2184 S442KE   1.0
    @2185 S442KF   1.0
    @2186 S442L    1.0
    @2187 S442MA   1.0
    @2188 S442MB   1.0
    @2189 S442MC   1.0
    @2190 S442MD   1.0
    @2191 S442ME   1.0
    @2192 S442MF   1.0
    @2193 S442MG   1.0
    @2194 S442MH   1.0
    @2195 S442MI   1.0
    @2196 S442N    1.0
    @2197 S442O    2.0
    @2199 S609     2.0
    @2201 S613     1.0
    @2202 S615     1.0
    @2203 S616     1.0
    @2204 S617     1.0
    @2205 S618     1.0
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
    if MV015    =      9 then MV015 = .;
    if MV026    =      9 then MV026 = .;
    if MV028    =    999 then MV028 = .;
    if MV029    =     99 then MV029 = .;
    if MV035    =      9 then MV035 = .;
    if MV801    =   9999 then MV801 = .;
    if MV802    =   9999 then MV802 = .;
    if MV803    =     99 then MV803 = .;
    if MV104    =     99 then MV104 = .;
    if MV105    =      9 then MV105 = .;
    if MV106    =      9 then MV106 = .;
    if MV107    =     99 then MV107 = .;
    if MV108    =      9 then MV108 = .;
    if MV109    =      9 then MV109 = .;
    if MV110    =      9 then MV110 = .;
    if MV112    =      9 then MV112 = .;
    if MV130    =      9 then MV130 = .;
    if MV133    =     99 then MV133 = .;
    if MV134    =      9 then MV134 = .;
    if MV135    =      9 then MV135 = .;
    if MV148    =      9 then MV148 = .;
    if MV149    =      9 then MV149 = .;
    if MV150    =     99 then MV150 = .;
    if MV152    =     99 then MV152 = .;
    if MV304_01 =      9 then MV304_01 = .;
    if MV305_01 =      9 then MV305_01 = .;
    if MV304_02 =      9 then MV304_02 = .;
    if MV305_02 =      9 then MV305_02 = .;
    if MV304_03 =      9 then MV304_03 = .;
    if MV305_03 =      9 then MV305_03 = .;
    if MV304_04 =      9 then MV304_04 = .;
    if MV305_04 =      9 then MV305_04 = .;
    if MV304_05 =      9 then MV304_05 = .;
    if MV305_05 =      9 then MV305_05 = .;
    if MV304_06 =      9 then MV304_06 = .;
    if MV305_06 =      9 then MV305_06 = .;
    if MV304_07 =      9 then MV304_07 = .;
    if MV305_07 =      9 then MV305_07 = .;
    if MV304_08 =      9 then MV304_08 = .;
    if MV305_08 =      9 then MV305_08 = .;
    if MV304_09 =      9 then MV304_09 = .;
    if MV305_09 =      9 then MV305_09 = .;
    if MV304_10 =      9 then MV304_10 = .;
    if MV305_10 =      9 then MV305_10 = .;
    if MV304_11 =      9 then MV304_11 = .;
    if MV305_11 =      9 then MV305_11 = .;
    if MV304_12 =      9 then MV304_12 = .;
    if MV305_12 =      9 then MV305_12 = .;
    if MV304_13 =      9 then MV304_13 = .;
    if MV305_13 =      9 then MV305_13 = .;
    if MV304_14 =      9 then MV304_14 = .;
    if MV305_14 =      9 then MV305_14 = .;
    if MV304_15 =      9 then MV304_15 = .;
    if MV305_15 =      9 then MV305_15 = .;
    if MV312    =     99 then MV312 = .;
    if MV313    =      9 then MV313 = .;
    if MV362    =      9 then MV362 = .;
    if MV363    =     99 then MV363 = .;
    if MV375A   =     99 then MV375A = .;
    if MV376    =     99 then MV376 = .;
    if MV384A   =      9 then MV384A = .;
    if MV384B   =      9 then MV384B = .;
    if MV384D   =      9 then MV384D = .;
    if MV501    =      9 then MV501 = .;
    if MV502    =      9 then MV502 = .;
    if MV503    =      9 then MV503 = .;
    if MV505    =     99 then MV505 = .;
    if MV527    =    999 then MV527 = .;
    if MV602    =      9 then MV602 = .;
    if MV603    =    999 then MV603 = .;
    if MV604    =      9 then MV604 = .;
    if MV605    =      9 then MV605 = .;
    if MV610    =      9 then MV610 = .;
    if MV611    =      9 then MV611 = .;
    if MV612    =      9 then MV612 = .;
    if MV613    =     99 then MV613 = .;
    if MV616    =    999 then MV616 = .;
    if MV621    =      9 then MV621 = .;
    if MV627    =     99 then MV627 = .;
    if MV628    =     99 then MV628 = .;
    if MV629    =     99 then MV629 = .;
    if MV714    =      9 then MV714 = .;
    if MV716    =    999 then MV716 = .;
    if MV717    =     99 then MV717 = .;
    if MV731    =      9 then MV731 = .;
    if MV740    =      9 then MV740 = .;
    if MV762    =     99 then MV762 = .;
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
    if MS304_01 =      9 then MS304_01 = .;
    if MS304_02 =      9 then MS304_02 = .;
    if MS304_03 =      9 then MS304_03 = .;
    if MS304_04 =      9 then MS304_04 = .;
    if MS304_05 =      9 then MS304_05 = .;
    if MS304_06 =      9 then MS304_06 = .;
    if MS304_07 =      9 then MS304_07 = .;
    if MS304_08 =      9 then MS304_08 = .;
    if MS304_09 =      9 then MS304_09 = .;
    if MS304_10 =      9 then MS304_10 = .;
    if MS304_11 =      9 then MS304_11 = .;
    if MS304_12 =      9 then MS304_12 = .;
    if MS304_13 =      9 then MS304_13 = .;
    if MS304_14 =      9 then MS304_14 = .;
    if MS304_15 =      9 then MS304_15 = .;
    if MS304_16 =      9 then MS304_16 = .;
    if MS304_17 =      9 then MS304_17 = .;
    if MS304_18 =      9 then MS304_18 = .;
    if MS304_19 =      9 then MS304_19 = .;
    if MS304_20 =      9 then MS304_20 = .;
    if MS309    =     99 then MS309 = .;
    if MS311    =     99 then MS311 = .;
    if MS316    =      9 then MS316 = .;
    if MS317    =     99 then MS317 = .;
    if MS320S   =      9 then MS320S = .;
    if MS320    =     99 then MS320 = .;
    if MS320A   =      9 then MS320A = .;
    if MS320C   =   9999 then MS320C = .;
    if MS320E   =     99 then MS320E = .;
    if MS320D   =      9 then MS320D = .;
    if MS321XE  =     99 then MS321XE = .;
    if MS321XF  =   9999 then MS321XF = .;
    if MS322B   =      9 then MS322B = .;
    if MS322C   =     99 then MS322C = .;
    if MS323    =     99 then MS323 = .;
    if MS323B   =      9 then MS323B = .;
    if MS325    =      9 then MS325 = .;
    if MS325A   =      9 then MS325A = .;
    if MS325B   =      9 then MS325B = .;
    if MS325C   =      9 then MS325C = .;
    if MS325H   =      9 then MS325H = .;
    if MS325I   =     99 then MS325I = .;
    if MS325J   =      9 then MS325J = .;
    if MS326    =     99 then MS326 = .;
    if MS328HI  =     99 then MS328HI = .;
    if MS347A   =      9 then MS347A = .;
    if MS347B   =      9 then MS347B = .;
    if MS347CA  =      9 then MS347CA = .;
    if MS347CB  =      9 then MS347CB = .;
    if MS347CC  =      9 then MS347CC = .;
    if MS347CD  =      9 then MS347CD = .;
    if MS347CE  =      9 then MS347CE = .;
    if MS348C   =      9 then MS348C = .;
    if MS349A   =      9 then MS349A = .;
    if MS349B   =      9 then MS349B = .;
    if MS349C   =      9 then MS349C = .;
    if MS349D   =      9 then MS349D = .;
    if MS349E   =      9 then MS349E = .;
    if MS350    =      9 then MS350 = .;
    if MS352    =     99 then MS352 = .;
    if MS353    =      9 then MS353 = .;
    if MS354    =      9 then MS354 = .;
    if MS355    =      9 then MS355 = .;
    if MS356    =     99 then MS356 = .;
    if MS357A   =      9 then MS357A = .;
    if MS357B   =      9 then MS357B = .;
    if MS357C   =      9 then MS357C = .;
    if MS357D   =      9 then MS357D = .;
    if MS357E   =      9 then MS357E = .;
    if MS357F   =      9 then MS357F = .;
    if MS357G   =      9 then MS357G = .;
    if MS358    =      9 then MS358 = .;
    if MS359    =      9 then MS359 = .;
    if MS360    =      9 then MS360 = .;
    if MS361    =      9 then MS361 = .;
    if MS408A   =      9 then MS408A = .;
    if MS408B   =      9 then MS408B = .;
    if MS408C   =      9 then MS408C = .;
    if MS408D   =      9 then MS408D = .;
    if MS508    =     99 then MS508 = .;
    if MS512    =      9 then MS512 = .;
    if MS514    =      9 then MS514 = .;
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
