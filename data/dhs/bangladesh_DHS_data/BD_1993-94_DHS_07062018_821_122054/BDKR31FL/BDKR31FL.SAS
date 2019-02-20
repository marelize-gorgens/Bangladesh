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
Data BDKR31FL;
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
  attrib BIDX     length=4 label="Birth column number";
  attrib BORD     length=4 label="Birth order number";
  attrib B0       length=4 format=B0_. label="Child is twin";
  attrib B1       length=4 label="Month of birth";
  attrib B2       length=4 label="Year of birth";
  attrib B3       length=4 label="Date of birth (CMC)";
  attrib B4       length=4 format=B4_. label="Sex of child";
  attrib B5       length=4 format=B5_. label="Child is alive";
  attrib B6       length=4 format=B6_. label="Age at death";
  attrib B7       length=4 label="Age at death (months-imputed)";
  attrib B8       length=4 label="Current age of child";
  attrib B9       length=4 format=B9_. label="Who child lives with";
  attrib B10      length=4 format=B10_. label="Completeness of information";
  attrib B11      length=4 label="Preceding birth interval";
  attrib B12      length=4 label="Succeeding birth interval";
  attrib B13      length=4 format=B13_. label="Flag for age at death";
  attrib B14      length=4 label="Birth interval >= 4 years -NA";
  attrib B15      length=4 label="Live birth between births -NA";
  attrib MIDX     length=4 label="Index to birth history";
  attrib M1       length=4 format=M1_. label="Tetanus injections bef. birth";
  attrib M2A      length=4 format=M2A_. label="Prenatal: doctor";
  attrib M2B      length=4 format=M2B_. label="Prenatal: nurse/midwife";
  attrib M2C      length=4 label="Prenatal: auxiliary midwife-NA";
  attrib M2D      length=4 format=M2D_. label="Prenatal: Family Welf.Visitor";
  attrib M2E      length=4 label="Prenatal: CS health profess-NA";
  attrib M2F      length=4 format=M2F_. label="Prenatal: trained birth att.";
  attrib M2G      length=4 format=M2G_. label="Prenatal: trad.birth attendant";
  attrib M2H      length=4 label="Prenatal: relative -NA";
  attrib M2I      length=4 label="Prenatal: CS other person -NA";
  attrib M2J      length=4 label="Prenatal: CS other person -NA";
  attrib M2K      length=4 format=M2K_. label="Prenatal: other resp (uncoded)";
  attrib M2L      length=4 label="Prenatal: CS other -NA";
  attrib M2M      length=4 label="Prenatal: CS other -NA";
  attrib M2N      length=4 format=M2N_. label="Prenatal: no one";
  attrib M3A      length=4 format=M3A_. label="Assistance: doctor";
  attrib M3B      length=4 format=M3B_. label="Assistance: nurse/midwife";
  attrib M3C      length=4 label="Delivery: auxiliary midwife-NA";
  attrib M3D      length=4 format=M3D_. label="Delivery: Family Welf.Visitor";
  attrib M3E      length=4 label="Assistance: CS health profe-NA";
  attrib M3F      length=4 format=M3F_. label="Assistance: trained birth att.";
  attrib M3G      length=4 format=M3G_. label="Assistance: trad.birth attend.";
  attrib M3H      length=4 format=M3H_. label="Assistance: relative, friend";
  attrib M3I      length=4 label="Assistance: CS other person-NA";
  attrib M3J      length=4 label="Assistance: CS other person-NA";
  attrib M3K      length=4 format=M3K_. label="Assistance: other resp (uncod)";
  attrib M3L      length=4 label="Assistance: CS other -NA";
  attrib M3M      length=4 format=M3M_. label="Assistance: CS other";
  attrib M3N      length=4 format=M3N_. label="Assistance: no one";
  attrib M4       length=4 format=M4_. label="Duration of breastfeeding";
  attrib M5       length=4 format=M5_. label="Months of breastfeeding";
  attrib M6       length=4 format=M6_. label="Duration of amenorrhea";
  attrib M7       length=4 format=M7_. label="Months of amenorrhea";
  attrib M8       length=4 format=M8_. label="Duration of abstinence";
  attrib M9       length=4 format=M9_. label="Months of abstinence";
  attrib M10      length=4 format=M10_. label="Time wanted pregnancy";
  attrib M11      length=4 label="Time would have waited -NA";
  attrib M13      length=4 format=M13_. label="Timing of 1st antenatal check";
  attrib M14      length=4 format=M14_. label="Antenatal visits for pregnancy";
  attrib M15      length=4 format=M15_. label="Place of delivery";
  attrib M17      length=4 label="Delivery by caesarian secti-NA";
  attrib M18      length=4 label="Size of child at birth -NA";
  attrib M19      length=4 label="Birth weight (kilos - 3 dec-NA";
  attrib M19A     length=4 label="Weight at birth recall -NA";
  attrib M21      length=4 format=M21_. label="Reason stopped breastfeeding";
  attrib M27      length=4 format=M27_. label="Flag for breastfeeding";
  attrib M28      length=4 format=M28_. label="Flag for amenorrhea";
  attrib M29      length=4 format=M29_. label="Flag for abstinence";
  attrib M30      length=4 label="At birth - prolonged labor -NA";
  attrib M31      length=4 label="At birth - excessive bleedi-NA";
  attrib M32      length=4 label="At birth - high fever/disch-NA";
  attrib M33      length=4 label="At birth - convulsions -NA";
  attrib M34      length=4 format=M34_. label="When child put to breast";
  attrib M35      length=4 format=M35_. label="Times breastfed during night";
  attrib M36      length=4 format=M36_. label="Times breastfed during day";
  attrib M37A     length=4 format=M37A_. label="Gave child plain water";
  attrib M37B     length=4 format=M37B_. label="Gave child sugar water";
  attrib M37C     length=4 format=M37C_. label="Gave child juice";
  attrib M37D     length=4 format=M37D_. label="Gave child herbal tea";
  attrib M37E     length=4 label="Gave child powder/tinned mi-NA";
  attrib M37F     length=4 format=M37F_. label="Gave child baby formula";
  attrib M37G     length=4 format=M37G_. label="Gave child fresh milk";
  attrib M37H     length=4 label="Country specific other liqu-NA";
  attrib M37I     length=4 label="Country specific other liqu-NA";
  attrib M37J     length=4 label="Country specific other liqu-NA";
  attrib M37K     length=4 label="Country specific other liqu-NA";
  attrib M37L     length=4 format=M37L_. label="Gave child other liquid";
  attrib M37M     length=4 label="Country specific other food-NA";
  attrib M37N     length=4 label="Country specific other food-NA";
  attrib M37O     length=4 label="Country specific other food-NA";
  attrib M37P     length=4 format=M37P_. label="Other solid, semi-solid foods";
  attrib M37Q     length=4 label="Food made from local grain -NA";
  attrib M37R     length=4 label="Food made from local tuber -NA";
  attrib M37S     length=4 label="Gave child eggs, fish, poul-NA";
  attrib M37T     length=4 format=M37T_. label="Gave child meat";
  attrib M38      length=4 label="Drank from bottle with nipp-NA";
  attrib M39      length=4 label="Times ate other food yester-NA";
  attrib M40A     length=4 label="Last 7 days - plain water -NA";
  attrib M40B     length=4 label="Last 7 days - milk -NA";
  attrib M40C     length=4 label="Last 7 days - other liquids-NA";
  attrib M40D     length=4 label="Last 7 days - local grain f-NA";
  attrib M40E     length=4 label="Last 7 days - local tuber f-NA";
  attrib M40F     length=4 label="Last 7 days - egg/fish/poul-NA";
  attrib M40G     length=4 label="Last 7 days - meat -NA";
  attrib M40H     length=4 label="Last 7 days - other solid f-NA";
  attrib M40I     length=4 label="Last 7 days - country speci-NA";
  attrib M40J     length=4 label="Last 7 days - country speci-NA";
  attrib M40K     length=4 label="Last 7 days - country speci-NA";
  attrib M40L     length=4 label="Last 7 days - country speci-NA";
  attrib M40M     length=4 label="Last 7 days - country speci-NA";
  attrib M40N     length=4 label="Last 7 days - country speci-NA";
  attrib M40O     length=4 label="Last 7 days - country speci-NA";
  attrib HIDX     length=4 label="Index to birth history";
  attrib H1       length=4 format=H1_. label="Has health card";
  attrib H2       length=4 format=H2_. label="Received BCG";
  attrib H2D      length=4 format=H2D_. label="BCG day";
  attrib H2M      length=4 format=H2M_. label="BCG month";
  attrib H2Y      length=4 format=H2Y_. label="BCG year";
  attrib H3       length=4 format=H3_. label="Received DPT 1";
  attrib H3D      length=4 format=H3D_. label="DPT 1 day";
  attrib H3M      length=4 format=H3M_. label="DPT 1 month";
  attrib H3Y      length=4 format=H3Y_. label="DPT 1 year";
  attrib H4       length=4 format=H4_. label="Received POLIO 1";
  attrib H4D      length=4 format=H4D_. label="POLIO 1 day";
  attrib H4M      length=4 format=H4M_. label="POLIO 1 month";
  attrib H4Y      length=4 format=H4Y_. label="POLIO 1 year";
  attrib H5       length=4 format=H5_. label="Received DPT 2";
  attrib H5D      length=4 format=H5D_. label="DPT 2 day";
  attrib H5M      length=4 format=H5M_. label="DPT 2 month";
  attrib H5Y      length=4 format=H5Y_. label="DPT 2 year";
  attrib H6       length=4 format=H6_. label="Received POLIO 2";
  attrib H6D      length=4 format=H6D_. label="POLIO 2 day";
  attrib H6M      length=4 format=H6M_. label="POLIO 2 month";
  attrib H6Y      length=4 format=H6Y_. label="POLIO 2 year";
  attrib H7       length=4 format=H7_. label="Received DPT 3";
  attrib H7D      length=4 format=H7D_. label="DPT 3 day";
  attrib H7M      length=4 format=H7M_. label="DPT 3 month";
  attrib H7Y      length=4 format=H7Y_. label="DPT 3 year";
  attrib H8       length=4 format=H8_. label="Received POLIO 3";
  attrib H8D      length=4 format=H8D_. label="POLIO 3 day";
  attrib H8M      length=4 format=H8M_. label="POLIO 3 month";
  attrib H8Y      length=4 format=H8Y_. label="POLIO 3 year";
  attrib H9       length=4 format=H9_. label="Received MEASLES";
  attrib H9D      length=4 format=H9D_. label="MEASLES day";
  attrib H9M      length=4 format=H9M_. label="MEASLES month";
  attrib H9Y      length=4 format=H9Y_. label="MEASLES year";
  attrib H0       length=4 label="Polio 0 -NA";
  attrib H0D      length=4 format=H0D_. label="Polio 0 day";
  attrib H0M      length=4 format=H0M_. label="POLIO 0 month";
  attrib H0Y      length=4 format=H0Y_. label="POLIO 0 year";
  attrib H10      length=4 format=H10_. label="Ever had vaccination";
  attrib H11      length=4 format=H11_. label="Had diarrhea recently";
  attrib H11B     length=4 format=H11B_. label="Blood in the stools";
  attrib H11C     length=4 label="Bowel movements in worst da-NA";
  attrib H12A     length=4 format=H12A_. label="Diarrhea: government hosp.";
  attrib H12B     length=4 format=H12B_. label="Diarrhea: govt health center";
  attrib H12C     length=4 label="Diarrhea: govt health post -NA";
  attrib H12D     length=4 label="Diarrhea: mobile clinic -NA";
  attrib H12E     length=4 format=H12E_. label="Diarrhea: comm.health worker";
  attrib H12F     length=4 format=H12F_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G     length=4 label="Diarrhea: CS public sector -NA";
  attrib H12H     length=4 label="Diarrhea: CS public sector -NA";
  attrib H12I     length=4 label="Diarrhea: CS public sector -NA";
  attrib H12J     length=4 format=H12J_. label="Diarrhea: private hosp/clinic";
  attrib H12K     length=4 format=H12K_. label="Diarrhea: private pharmacy";
  attrib H12L     length=4 format=H12L_. label="Diarrhea: private doctor";
  attrib H12M     length=4 label="Diarrhea: private mobile cl-NA";
  attrib H12N     length=4 label="Diarrhea: comm.health worke-NA";
  attrib H12O     length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12P     length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q     length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12R     length=4 label="Diarrhea: CS med.priv secto-NA";
  attrib H12S     length=4 format=H12S_. label="Diarrhea: shop";
  attrib H12T     length=4 format=H12T_. label="Diarrhea: traditional pract.";
  attrib H12U     length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V     length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W     length=4 label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X     length=4 format=H12X_. label="Diarrhea: Other";
  attrib H12Y     length=4 format=H12Y_. label="Diarrhea: no treatment";
  attrib H12Z     length=4 format=H12Z_. label="Diarrhea: medical treatment";
  attrib H13      length=4 format=H13_. label="Given oral rehydration";
  attrib H14      length=4 format=H14_. label="Given recommend. home solution";
  attrib H15      length=4 format=H15_. label="Given other pills or syrups";
  attrib H15A     length=4 label="Given antibiotics (CS) -NA";
  attrib H15B     length=4 format=H15B_. label="Given an injection";
  attrib H15C     length=4 format=H15C_. label="Given an intravenous (IV)";
  attrib H15D     length=4 format=H15D_. label="Given home remedy, herbal med.";
  attrib H15E     length=4 label="Given CS other treatment -NA";
  attrib H15F     length=4 label="Given CS other treatment -NA";
  attrib H15G     length=4 label="Given CS other treatment -NA";
  attrib H15H     length=4 label="Given CS other treatment -NA";
  attrib H16      length=4 format=H16_. label="Increase or decrease fluids";
  attrib H18      length=4 label="Increase or decrease in foo-NA";
  attrib H20      length=4 format=H20_. label="Given other treatment";
  attrib H21A     length=4 format=H21A_. label="Given no treatment";
  attrib H21      length=4 format=H21_. label="Received any treatment";
  attrib H22      length=4 label="Had fever in last two weeks-NA";
  attrib H31      length=4 format=H31_. label="Had cough in last two weeks";
  attrib H31B     length=4 format=H31B_. label="Short, rapid breaths";
  attrib H32A     length=4 format=H32A_. label="Fever/cough: government hosp.";
  attrib H32B     length=4 format=H32B_. label="Fever/cough: govt health cntr";
  attrib H32C     length=4 label="Fever/cough: govt health po-NA";
  attrib H32D     length=4 label="Fever/cough: mobile clinic -NA";
  attrib H32E     length=4 format=H32E_. label="Fever/cough: comm.health wrkr";
  attrib H32F     length=4 format=H32F_. label="Fever/cough: Satellite clinic";
  attrib H32G     length=4 label="Fever/cough: CS public sect-NA";
  attrib H32H     length=4 label="Fever/cough: CS public sect-NA";
  attrib H32I     length=4 label="Fever/cough: CS public sect-NA";
  attrib H32J     length=4 format=H32J_. label="Fever/cough: private hosp/clin";
  attrib H32K     length=4 format=H32K_. label="Fever/cough: private pharmacy";
  attrib H32L     length=4 format=H32L_. label="Fever/cough: private doctor";
  attrib H32M     length=4 label="Fever/cough: private mobile-NA";
  attrib H32N     length=4 label="Fever/cough: comm.health wr-NA";
  attrib H32O     length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32P     length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32Q     length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32R     length=4 label="Fever/cough: CS med.priv se-NA";
  attrib H32S     length=4 format=H32S_. label="Fever/cough: shop";
  attrib H32T     length=4 format=H32T_. label="Fever/cough: traditional pract";
  attrib H32U     length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32V     length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32W     length=4 label="Fever/cough: CS oth.priv se-NA";
  attrib H32X     length=4 format=H32X_. label="Fever/cough: Other";
  attrib H32Y     length=4 format=H32Y_. label="Fever/cough: no treatment";
  attrib H32Z     length=4 format=H32Z_. label="Fever/cough: medical treatment";
  attrib HWIDX    length=4 label="Index to birth history - NA";
  attrib HW1      length=4 label="Age in months - NA";
  attrib HW2      length=4 label="Weight in kilograms (1 de - NA";
  attrib HW3      length=4 label="Height in centimeters (1 - NA";
  attrib HW4      length=4 label="Ht/A Percentile - NA";
  attrib HW5      length=4 label="Ht/A Standard deviations - NA";
  attrib HW6      length=4 label="Ht/A Percent of ref. medi - NA";
  attrib HW7      length=4 label="Wt/A Percentile - NA";
  attrib HW8      length=4 label="Wt/A Standard deviations - NA";
  attrib HW9      length=4 label="Wt/A Percent of ref. medi - NA";
  attrib HW10     length=4 label="Wt/Ht Percentile - NA";
  attrib HW11     length=4 label="Wt/Ht Standard deviations - NA";
  attrib HW12     length=4 label="Wt/Ht Percent of ref. med - NA";
  attrib HW13     length=4 label="Reason not measured";
  attrib HW14     length=4 label="BCG scar on arm or should - NA";
  attrib HW15     length=4 label="Height: lying or standing - NA";
  attrib HW16     length=4 label="Day of birth of child - NA";
  attrib HW17     length=4 label="Date measured (day) - NA";
  attrib HW18     length=4 label="Date measured (month) - NA";
  attrib HW19     length=4 label="Date measured (year) - NA";
  attrib HW20     length=4 label="Arm circumference (cms - - NA";
  attrib HW21     length=4 label="Arm circ/A Percentile - NA";
  attrib HW22     length=4 label="Arm circ/A Standard devia - NA";
  attrib HW23     length=4 label="Arm c/A Percent of ref. m - NA";
  attrib HW24     length=4 label="Arm circ/Ht Percentile - NA";
  attrib HW25     length=4 label="Arm cir/Ht Standard devia - NA";
  attrib HW26     length=4 label="Arm cir/Ht Percent ref. m - NA";
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
infile 'BDKR31FL.DAT' LRECL=1018 MISSOVER ;
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
    @200  V201     2.0
    @202  V202     2.0
    @204  V203     2.0
    @206  V204     2.0
    @208  V205     2.0
    @210  V206     2.0
    @212  V207     2.0
    @214  V208     1.0
    @215  V209     1.0
    @216  V210     1.0
    @217  V211     4.0
    @221  V212     2.0
    @223  V213     1.0
    @224  V214     2.0
    @226  V215     3.0
    @229  V216     1.0
    @230  V217     1.0
    @231  V218     2.0
    @233  V219     2.0
    @235  V220     1.0
    @236  V221     3.0
    @239  V222     3.0
    @242  V223     1.0
    @243  V224     2.0
    @245  V225     1.0
    @246  V226     3.0
    @249  V227     1.0
    @250  V228     1.0
    @251  V229     2.0
    @253  V230     2.0
    @255  V231     4.0
    @259  V232     1.0
    @260  V233     2.0
    @262  V234     1.0
    @263  V235     2.0
    @265  V236     1.0
    @266  V237     1.0
    @267  V238     1.0
    @268  V310     2.0
    @270  V311     1.0
    @271  V312     2.0
    @273  V313     1.0
    @274  V314     1.0
    @275  V315     2.0
    @277  V316     2.0
    @279  V317     4.0
    @283  V318     1.0
    @284  V319     1.0
    @285  V320     1.0
    @286  V321     1.0
    @287  V322     1.0
    @288  V323     2.0
    @290  V325     6.0
    @296  V326     2.0
    @298  V327     1.0
    @299  V328     2.0
    @301  V337     3.0
    @304  V359     2.0
    @306  V360     2.0
    @308  V361     1.0
    @309  V362     1.0
    @310  V363     2.0
    @312  V364     1.0
    @313  V366A    1.0
    @314  V366B    1.0
    @315  V367     1.0
    @316  V369     2.0
    @318  V369B    1.0
    @319  V372     1.0
    @320  V375A    2.0
    @322  V376     2.0
    @324  V376A    1.0
    @325  V379     2.0
    @327  V380     1.0
    @328  V384A    1.0
    @329  V384B    1.0
    @330  V384C    1.0
    @331  V384D    1.0
    @332  V384E    1.0
    @333  V393     1.0
    @334  V394     1.0
    @335  V395     1.0
    @336  V396     1.0
    @337  V397     1.0
    @338  V398     1.0
    @339  V399     1.0
    @340  V401     1.0
    @341  V404     1.0
    @342  V405     1.0
    @343  V406     1.0
    @344  V407     2.0
    @346  V408     2.0
    @348  V409     1.0
    @349  V409A    1.0
    @350  V410     1.0
    @351  V410A    1.0
    @352  V411     1.0
    @353  V411A    1.0
    @354  V412     1.0
    @355  V413A    1.0
    @356  V413B    1.0
    @357  V413C    1.0
    @358  V413D    1.0
    @359  V413     1.0
    @360  V414A    1.0
    @361  V414B    1.0
    @362  V414C    1.0
    @363  V414D    1.0
    @364  V414E    1.0
    @365  V414F    1.0
    @366  V414G    1.0
    @367  V414H    1.0
    @368  V415     1.0
    @369  V416     1.0
    @370  V417     1.0
    @371  V418     1.0
    @372  V419     1.0
    @373  V420     2.0
    @375  V421     2.0
    @377  V426     3.0
    @380  V436     3.0
    @383  V437     4.0
    @387  V438     4.0
    @391  V439     4.0
    @395  V440     4.0
    @399  V441     5.0
    @404  V442     5.0
    @409  V443     5.0
    @414  V444     5.0
    @419  V444A    4.0
    @423  V445     4.0
    @427  V446     4.0
    @431  V447     1.0
    @432  V448     1.0
    @433  V449     1.0
    @434  V450A    1.0
    @435  V450B    1.0
    @436  V450C    1.0
    @437  V450D    1.0
    @438  V450E    1.0
    @439  V450F    1.0
    @440  V450G    1.0
    @441  V450H    1.0
    @442  V450I    1.0
    @443  V450J    1.0
    @444  V450K    1.0
    @445  V450L    1.0
    @446  V450M    1.0
    @447  V450X    1.0
    @448  V450Z    1.0
    @449  V451A    1.0
    @450  V451B    1.0
    @451  V451C    1.0
    @452  V451D    1.0
    @453  V451E    1.0
    @454  V451F    1.0
    @455  V451G    1.0
    @456  V451H    1.0
    @457  V451I    1.0
    @458  V451J    1.0
    @459  V451K    1.0
    @460  V451X    1.0
    @461  V451Z    1.0
    @462  V501     1.0
    @463  V502     1.0
    @464  V503     1.0
    @465  V504     1.0
    @466  V505     2.0
    @468  V506     2.0
    @470  V507     2.0
    @472  V508     2.0
    @474  V509     4.0
    @478  V510     1.0
    @479  V511     2.0
    @481  V512     2.0
    @483  V513     1.0
    @484  V525     2.0
    @486  V527     3.0
    @489  V528     2.0
    @491  V529     3.0
    @494  V530     1.0
    @495  V531     2.0
    @497  V532     1.0
    @498  V534     1.0
    @499  V535     1.0
    @500  V602     1.0
    @501  V603     3.0
    @504  V604     1.0
    @505  V605     1.0
    @506  V606     1.0
    @507  V610     1.0
    @508  V611     1.0
    @509  V612     1.0
    @510  V613     2.0
    @512  V614     1.0
    @513  V616     3.0
    @516  V618     1.0
    @517  V621     1.0
    @518  V623     1.0
    @519  V624     2.0
    @521  V625     1.0
    @522  V626     2.0
    @524  V627     2.0
    @526  V628     2.0
    @528  V629     2.0
    @530  V630A    1.0
    @531  V630B    1.0
    @532  V630C    1.0
    @533  V630D    1.0
    @534  V630E    1.0
    @535  V630F    1.0
    @536  V630G    1.0
    @537  V630H    1.0
    @538  V630I    1.0
    @539  V630J    1.0
    @540  V630K    1.0
    @541  V630L    1.0
    @542  V630X    1.0
    @543  V701     1.0
    @544  V702     2.0
    @546  V704     3.0
    @549  V705     2.0
    @551  V707     1.0
    @552  V714     1.0
    @553  V715     2.0
    @555  V716     3.0
    @558  V717     2.0
    @560  V718     1.0
    @561  V719     1.0
    @562  V720     1.0
    @563  V721     1.0
    @564  V723     2.0
    @566  V729     1.0
    @567  V730     2.0
    @569  V731     1.0
    @570  V732     1.0
    @571  V733     2.0
    @573  V734     1.0
    @574  V735     3.0
    @577  V736     8.0
    @585  V737     7.0
    @592  V738     7.0
    @599  V739     1.0
    @600  V740     1.0
    @601  BIDX     2.0
    @603  BORD     2.0
    @605  B0       1.0
    @606  B1       2.0
    @608  B2       2.0
    @610  B3       4.0
    @614  B4       1.0
    @615  B5       1.0
    @616  B6       3.0
    @619  B7       3.0
    @622  B8       2.0
    @624  B9       1.0
    @625  B10      1.0
    @626  B11      3.0
    @629  B12      3.0
    @632  B13      1.0
    @633  B14      1.0
    @634  B15      1.0
    @635  MIDX     1.0
    @636  M1       1.0
    @637  M2A      1.0
    @638  M2B      1.0
    @639  M2C      1.0
    @640  M2D      1.0
    @641  M2E      1.0
    @642  M2F      1.0
    @643  M2G      1.0
    @644  M2H      1.0
    @645  M2I      1.0
    @646  M2J      1.0
    @647  M2K      1.0
    @648  M2L      1.0
    @649  M2M      1.0
    @650  M2N      1.0
    @651  M3A      1.0
    @652  M3B      1.0
    @653  M3C      1.0
    @654  M3D      1.0
    @655  M3E      1.0
    @656  M3F      1.0
    @657  M3G      1.0
    @658  M3H      1.0
    @659  M3I      1.0
    @660  M3J      1.0
    @661  M3K      1.0
    @662  M3L      1.0
    @663  M3M      1.0
    @664  M3N      1.0
    @665  M4       2.0
    @667  M5       2.0
    @669  M6       2.0
    @671  M7       2.0
    @673  M8       2.0
    @675  M9       2.0
    @677  M10      1.0
    @678  M11      3.0
    @681  M13      2.0
    @683  M14      2.0
    @685  M15      2.0
    @687  M17      1.0
    @688  M18      1.0
    @689  M19      4.0
    @693  M19A     1.0
    @694  M21      2.0
    @696  M27      1.0
    @697  M28      1.0
    @698  M29      1.0
    @699  M30      1.0
    @700  M31      1.0
    @701  M32      1.0
    @702  M33      1.0
    @703  M34      3.0
    @706  M35      2.0
    @708  M36      2.0
    @710  M37A     1.0
    @711  M37B     1.0
    @712  M37C     1.0
    @713  M37D     1.0
    @714  M37E     1.0
    @715  M37F     1.0
    @716  M37G     1.0
    @717  M37H     1.0
    @718  M37I     1.0
    @719  M37J     1.0
    @720  M37K     1.0
    @721  M37L     1.0
    @722  M37M     1.0
    @723  M37N     1.0
    @724  M37O     1.0
    @725  M37P     1.0
    @726  M37Q     1.0
    @727  M37R     1.0
    @728  M37S     1.0
    @729  M37T     1.0
    @730  M38      1.0
    @731  M39      1.0
    @732  M40A     1.0
    @733  M40B     1.0
    @734  M40C     1.0
    @735  M40D     1.0
    @736  M40E     1.0
    @737  M40F     1.0
    @738  M40G     1.0
    @739  M40H     1.0
    @740  M40I     1.0
    @741  M40J     1.0
    @742  M40K     1.0
    @743  M40L     1.0
    @744  M40M     1.0
    @745  M40N     1.0
    @746  M40O     1.0
    @747  HIDX     1.0
    @748  H1       1.0
    @749  H2       1.0
    @750  H2D      2.0
    @752  H2M      2.0
    @754  H2Y      2.0
    @756  H3       1.0
    @757  H3D      2.0
    @759  H3M      2.0
    @761  H3Y      2.0
    @763  H4       1.0
    @764  H4D      2.0
    @766  H4M      2.0
    @768  H4Y      2.0
    @770  H5       1.0
    @771  H5D      2.0
    @773  H5M      2.0
    @775  H5Y      2.0
    @777  H6       1.0
    @778  H6D      2.0
    @780  H6M      2.0
    @782  H6Y      2.0
    @784  H7       1.0
    @785  H7D      2.0
    @787  H7M      2.0
    @789  H7Y      2.0
    @791  H8       1.0
    @792  H8D      2.0
    @794  H8M      2.0
    @796  H8Y      2.0
    @798  H9       1.0
    @799  H9D      2.0
    @801  H9M      2.0
    @803  H9Y      2.0
    @805  H0       1.0
    @806  H0D      2.0
    @808  H0M      2.0
    @810  H0Y      2.0
    @812  H10      1.0
    @813  H11      1.0
    @814  H11B     1.0
    @815  H11C     2.0
    @817  H12A     1.0
    @818  H12B     1.0
    @819  H12C     1.0
    @820  H12D     1.0
    @821  H12E     1.0
    @822  H12F     1.0
    @823  H12G     1.0
    @824  H12H     1.0
    @825  H12I     1.0
    @826  H12J     1.0
    @827  H12K     1.0
    @828  H12L     1.0
    @829  H12M     1.0
    @830  H12N     1.0
    @831  H12O     1.0
    @832  H12P     1.0
    @833  H12Q     1.0
    @834  H12R     1.0
    @835  H12S     1.0
    @836  H12T     1.0
    @837  H12U     1.0
    @838  H12V     1.0
    @839  H12W     1.0
    @840  H12X     1.0
    @841  H12Y     1.0
    @842  H12Z     1.0
    @843  H13      1.0
    @844  H14      1.0
    @845  H15      1.0
    @846  H15A     1.0
    @847  H15B     1.0
    @848  H15C     1.0
    @849  H15D     1.0
    @850  H15E     1.0
    @851  H15F     1.0
    @852  H15G     1.0
    @853  H15H     1.0
    @854  H16      1.0
    @855  H18      1.0
    @856  H20      1.0
    @857  H21A     1.0
    @858  H21      1.0
    @859  H22      1.0
    @860  H31      1.0
    @861  H31B     1.0
    @862  H32A     1.0
    @863  H32B     1.0
    @864  H32C     1.0
    @865  H32D     1.0
    @866  H32E     1.0
    @867  H32F     1.0
    @868  H32G     1.0
    @869  H32H     1.0
    @870  H32I     1.0
    @871  H32J     1.0
    @872  H32K     1.0
    @873  H32L     1.0
    @874  H32M     1.0
    @875  H32N     1.0
    @876  H32O     1.0
    @877  H32P     1.0
    @878  H32Q     1.0
    @879  H32R     1.0
    @880  H32S     1.0
    @881  H32T     1.0
    @882  H32U     1.0
    @883  H32V     1.0
    @884  H32W     1.0
    @885  H32X     1.0
    @886  H32Y     1.0
    @887  H32Z     1.0
    @888  HWIDX    1.0
    @889  HW1      2.0
    @891  HW2      3.0
    @894  HW3      4.0
    @898  HW4      4.0
    @902  HW5      4.0
    @906  HW6      5.0
    @911  HW7      4.0
    @915  HW8      4.0
    @919  HW9      5.0
    @924  HW10     4.0
    @928  HW11     4.0
    @932  HW12     5.0
    @937  HW13     1.0
    @938  HW14     1.0
    @939  HW15     1.0
    @940  HW16     2.0
    @942  HW17     2.0
    @944  HW18     2.0
    @946  HW19     2.0
    @948  HW20     3.0
    @951  HW21     4.0
    @955  HW22     4.0
    @959  HW23     5.0
    @964  HW24     4.0
    @968  HW25     4.0
    @972  HW26     5.0
    @977  SDFORMAT 1.0
    @978  SADJUST  1.0
    @979  SINTM    2.0
    @981  SINTY    2.0
    @983  SDIV     1.0
    @984  SDISTR   2.0
    @986  S114A    1.0
    @987  S114B    1.0
    @988  S114C    1.0
    @989  S114D    1.0
    @990  S114E    1.0
    @991  S119     3.0
    @994  S119A    3.0
    @997  S123     2.0
    @999  S124A    1.0
    @1000 S124B    1.0
    @1001 S124C    1.0
    @1002 S124D    1.0
    @1003 S124I    1.0
    @1004 S229M    2.0
    @1006 S229Y    4.0
    @1010 S233A    1.0
    @1011 S233B    1.0
    @1012 S233C    1.0
    @1013 S233DM   2.0
    @1015 S233DY   4.0
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
    if V215     =    999 then V215 = .;
    if V225     =      9 then V225 = .;
    if V226     =    999 then V226 = .;
    if V228     =      9 then V228 = .;
    if V229     =     99 then V229 = .;
    if V230     =     99 then V230 = .;
    if V231     =   9999 then V231 = .;
    if V233     =     99 then V233 = .;
    if V234     =      9 then V234 = .;
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
    if B6       =    999 then B6 = .;
    if B9       =      9 then B9 = .;
    if B14      =      9 then B14 = .;
    if B15      =      9 then B15 = .;
    if M1       =      9 then M1 = .;
    if M2A      =      9 then M2A = .;
    if M2B      =      9 then M2B = .;
    if M2C      =      9 then M2C = .;
    if M2D      =      9 then M2D = .;
    if M2E      =      9 then M2E = .;
    if M2F      =      9 then M2F = .;
    if M2G      =      9 then M2G = .;
    if M2H      =      9 then M2H = .;
    if M2I      =      9 then M2I = .;
    if M2J      =      9 then M2J = .;
    if M2K      =      9 then M2K = .;
    if M2N      =      9 then M2N = .;
    if M3A      =      9 then M3A = .;
    if M3B      =      9 then M3B = .;
    if M3D      =      9 then M3D = .;
    if M3F      =      9 then M3F = .;
    if M3G      =      9 then M3G = .;
    if M3H      =      9 then M3H = .;
    if M3K      =      9 then M3K = .;
    if M3M      =      9 then M3M = .;
    if M3N      =      9 then M3N = .;
    if M4       =     99 then M4 = .;
    if M5       =     99 then M5 = .;
    if M6       =     99 then M6 = .;
    if M7       =     99 then M7 = .;
    if M8       =     99 then M8 = .;
    if M9       =     99 then M9 = .;
    if M10      =      9 then M10 = .;
    if M13      =     99 then M13 = .;
    if M14      =     99 then M14 = .;
    if M15      =     99 then M15 = .;
    if M21      =     99 then M21 = .;
    if M34      =    999 then M34 = .;
    if M35      =     99 then M35 = .;
    if M36      =     99 then M36 = .;
    if M37A     =      9 then M37A = .;
    if M37B     =      9 then M37B = .;
    if M37C     =      9 then M37C = .;
    if M37D     =      9 then M37D = .;
    if M37E     =      9 then M37E = .;
    if M37F     =      9 then M37F = .;
    if M37G     =      9 then M37G = .;
    if M37H     =      9 then M37H = .;
    if M37I     =      9 then M37I = .;
    if M37J     =      9 then M37J = .;
    if M37K     =      9 then M37K = .;
    if M37L     =      9 then M37L = .;
    if M37P     =      9 then M37P = .;
    if M37R     =      9 then M37R = .;
    if M37T     =      9 then M37T = .;
    if M39      =      9 then M39 = .;
    if M40L     =      9 then M40L = .;
    if H1       =      9 then H1 = .;
    if H2       =      9 then H2 = .;
    if H2D      =     99 then H2D = .;
    if H2M      =     99 then H2M = .;
    if H2Y      =     99 then H2Y = .;
    if H3       =      9 then H3 = .;
    if H3D      =     99 then H3D = .;
    if H3M      =     99 then H3M = .;
    if H3Y      =     99 then H3Y = .;
    if H4       =      9 then H4 = .;
    if H4D      =     99 then H4D = .;
    if H4M      =     99 then H4M = .;
    if H4Y      =     99 then H4Y = .;
    if H5       =      9 then H5 = .;
    if H5D      =     99 then H5D = .;
    if H5M      =     99 then H5M = .;
    if H5Y      =     99 then H5Y = .;
    if H6       =      9 then H6 = .;
    if H6D      =     99 then H6D = .;
    if H6M      =     99 then H6M = .;
    if H6Y      =     99 then H6Y = .;
    if H7       =      9 then H7 = .;
    if H7D      =     99 then H7D = .;
    if H7M      =     99 then H7M = .;
    if H7Y      =     99 then H7Y = .;
    if H8       =      9 then H8 = .;
    if H8D      =     99 then H8D = .;
    if H8M      =     99 then H8M = .;
    if H8Y      =     99 then H8Y = .;
    if H9       =      9 then H9 = .;
    if H9D      =     99 then H9D = .;
    if H9M      =     99 then H9M = .;
    if H9Y      =     99 then H9Y = .;
    if H0D      =     99 then H0D = .;
    if H0M      =     99 then H0M = .;
    if H0Y      =     99 then H0Y = .;
    if H10      =      9 then H10 = .;
    if H11      =      9 then H11 = .;
    if H11B     =      9 then H11B = .;
    if H12A     =      9 then H12A = .;
    if H12B     =      9 then H12B = .;
    if H12D     =      9 then H12D = .;
    if H12E     =      9 then H12E = .;
    if H12F     =      9 then H12F = .;
    if H12G     =      9 then H12G = .;
    if H12J     =      9 then H12J = .;
    if H12K     =      9 then H12K = .;
    if H12L     =      9 then H12L = .;
    if H12S     =      9 then H12S = .;
    if H12T     =      9 then H12T = .;
    if H12X     =      9 then H12X = .;
    if H12Y     =      9 then H12Y = .;
    if H12Z     =      9 then H12Z = .;
    if H13      =      9 then H13 = .;
    if H14      =      9 then H14 = .;
    if H15      =      9 then H15 = .;
    if H15A     =      9 then H15A = .;
    if H15B     =      9 then H15B = .;
    if H15C     =      9 then H15C = .;
    if H15D     =      9 then H15D = .;
    if H15E     =      9 then H15E = .;
    if H15F     =      9 then H15F = .;
    if H16      =      9 then H16 = .;
    if H20      =      9 then H20 = .;
    if H21A     =      9 then H21A = .;
    if H21      =      9 then H21 = .;
    if H31      =      9 then H31 = .;
    if H31B     =      9 then H31B = .;
    if H32A     =      9 then H32A = .;
    if H32B     =      9 then H32B = .;
    if H32C     =      9 then H32C = .;
    if H32D     =      9 then H32D = .;
    if H32E     =      9 then H32E = .;
    if H32F     =      9 then H32F = .;
    if H32G     =      9 then H32G = .;
    if H32H     =      9 then H32H = .;
    if H32I     =      9 then H32I = .;
    if H32J     =      9 then H32J = .;
    if H32K     =      9 then H32K = .;
    if H32L     =      9 then H32L = .;
    if H32M     =      9 then H32M = .;
    if H32N     =      9 then H32N = .;
    if H32O     =      9 then H32O = .;
    if H32P     =      9 then H32P = .;
    if H32Q     =      9 then H32Q = .;
    if H32R     =      9 then H32R = .;
    if H32S     =      9 then H32S = .;
    if H32T     =      9 then H32T = .;
    if H32U     =      9 then H32U = .;
    if H32V     =      9 then H32V = .;
    if H32W     =      9 then H32W = .;
    if H32X     =      9 then H32X = .;
    if H32Y     =      9 then H32Y = .;
    if H32Z     =      9 then H32Z = .;
    if HW13     =      9 then HW13 = .;
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
