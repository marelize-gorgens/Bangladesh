libname user 'C:\COUNTRIES\BD31';
Proc format;
  value F00001_
     0 = "10-14"
     1 = "15-19"
     2 = "20-24"
     3 = "25-29"
     4 = "30-34"
     5 = "35-39"
     6 = "40-44"
     7 = "45-49"
     ;
  value F00002_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     4 = "Y & age - y ignored"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value F00003_
     1 = "Completed"
     2 = "Not at home"
     3 = "Postponed"
     4 = "Refused"
     5 = "Partly completed"
     6 = "Incapacitated"
     7 = "Other"
     ;
  value F00004_
     0 = "No calendar"
     ;
  value F00005_
     0 = "No calendar"
     ;
  value F00006_
     0 = "All woman sample"
     1 = "Ever married sample"
     ;
  value F00007_
     0 = "National"
     1 = "Country specific"
     ;
  value F00008_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value F00009_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00010_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value F00011_
     0 = "Husband not in HH"
     ;
  value F00012_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value F00013_
     1 = "Urban"
     2 = "Rural"
     ;
  value F00014_
    95 = "Always"
    96 = "Visitor"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00015_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value F00016_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     ;
  value F00017_
     1 = "Reads easily"
     2 = "Reads with difficult"
     3 = "Cannot read"
     ;
  value F00018_
     0 = "No"
     1 = "Yes"
     ;
  value F00019_
     0 = "No"
     1 = "Yes"
     ;
  value F00020_
     0 = "No"
     1 = "Yes"
     ;
  value F00021_
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
  value F00022_
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
  value F00023_
   996 = "On premises"
     ;
  value F00024_
    10 = "FLUSH TOILET"
    20 = "PIT TOILET LATRINE"
    30 = "NO FACILITY"
    96 = "OTHER"
     ;
  value F00025_
     0 = "No"
     1 = "Yes"
     ;
  value F00026_
     0 = "No"
     1 = "Yes"
     ;
  value F00027_
     0 = "No"
     1 = "Yes"
     ;
  value F00028_
     0 = "No"
     1 = "Yes"
     ;
  value F00029_
    10 = "NATURAL"
    11 = "Earth"
    20 = "RUDIMENTARY"
    21 = "Wood"
    30 = "FINISHED"
    31 = "Cement/concrete"
    96 = "OTHER"
     ;
  value F00030_
    10 = "NATURAL"
    11 = "Jute/bamboo/mud"
    20 = "RUDIMENTARY"
    21 = "Wood"
    30 = "FINISHED"
    31 = "Brick/cement"
    32 = "Tin"
    96 = "OTHER"
     ;
  value F00031_
    10 = "NATURAL"
    11 = "Jute/bamboo/mud"
    20 = "RUDIMENTARY"
    21 = "Tin"
    30 = "FINISHED"
    31 = "Cement/concrete"
    96 = "OTHER"
     ;
  value F00032_
     1 = "Islam"
     2 = "Christianity"
     3 = "Hinduism"
     4 = "Buddhism"
     5 = "Other"
     ;
  value F00033_
    97 = "Inconsistent"
     ;
  value F00034_
     0 = "Capital, large city"
     1 = "Small city"
     2 = "Town"
     3 = "Countryside"
     ;
  value F00035_
     1 = "Usual resident"
     2 = "Visitor"
     ;
  value F00036_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value F00037_
     0 = "Abroad"
     1 = "Urban"
     2 = "Rural"
     ;
  value F00038_
     0 = "Capital, large city"
     1 = "City"
     2 = "Town"
     3 = "Countryside"
     4 = "Abroad"
     ;
  value F00039_
     0 = "No"
     1 = "Yes"
     ;
  value F00040_
     0 = "No"
     1 = "Yes"
     ;
  value F00041_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     ;
  value F00042_
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
  value F00043_
     1 = "Male"
     2 = "Female"
     ;
  value F00044_
    96 = "96+"
     ;
  value F00045_
     0 = "No or unsure"
     1 = "Yes"
     ;
  value F00046_
   994 = "In menopause"
   995 = "Before last birth"
   996 = "Never menstruated"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00047_
     0 = "No"
     1 = "Yes"
     ;
  value F00048_
     6 = "6+"
     ;
  value F00049_
   996 = "Negative interval"
     ;
  value F00050_
     7 = "Month - exact date"
     8 = "None - date placed"
     ;
  value F00051_
     1 = "Then"
     2 = "Later"
     3 = "Not at all"
     ;
  value F00052_
   994 = "In menopause"
   995 = "Before last birth"
   996 = "Never menstruated"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00053_
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
  value F00054_
     0 = "No"
     1 = "Yes"
     ;
  value F00055_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00056_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00057_
  9997 = "Inconsistent"
  9998 = "DK  "
     ;
  value F00058_
     1 = "Month and year"
     2 = "From calendar"
     5 = "Year"
     7 = "Month"
     8 = "None"
     ;
  value F00059_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00060_
     0 = "No"
     1 = "Yes"
     ;
  value F00061_
     0 = "No prior child"
     ;
  value F00062_
     4 = "4+"
     5 = "Never used"
     ;
  value F00063_
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
  value F00064_
     0 = "No method"
     1 = "Folkloric method"
     2 = "Traditional method"
     3 = "Modern method"
     ;
  value F00065_
     1 = "Month and year"
     5 = "Year - m imp, a calc"
     7 = "Month - date placed"
     8 = "None - date placed"
     ;
  value F00066_
     1 = "<2"
     2 = "2-3"
     3 = "4-5"
     4 = "6-7"
     5 = "8-9"
     6 = "10+"
     ;
  value F00067_
     1 = "<25"
     2 = "25-29"
     3 = "30-34"
     4 = "35-39"
     5 = "40-44"
     6 = "45-49"
     ;
  value F00068_
     0 = "Single,before marr."
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25+"
     ;
  value F00069_
     5 = "5+"
     ;
  value F00070_
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
  value F00071_
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
  value F00072_
     1 = "Govt Clinical/Pharm"
     2 = "Govt Home/Comm deliv"
     3 = "NGO"
     4 = "Private Clin/Deliv"
     5 = "Private Pharmacy"
     6 = "Shop, church, friend"
     7 = "Other"
     8 = "Don't know"
     ;
  value F00073_
    95 = "All cal. {A core}"
    96 = "96+ {B core}"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00074_
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
  value F00075_
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
  value F00076_
     1 = "Currently using"
     2 = "Used since last brth"
     3 = "Used before last brt"
     4 = "Never used"
     ;
  value F00077_
     1 = "In next 12 months"
     2 = "Use later"
     3 = "Unsure about timing"
     4 = "Unsure about use"
     5 = "Does not intend"
     6 = "Never had sex"
     ;
  value F00078_
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
  value F00079_
     1 = "Using modern method"
     2 = "Using tradit. method"
     3 = "Non-user intend to"
     4 = "Does not intend to"
     5 = "Never had sex"
     ;
  value F00080_
     0 = "Not acceptable"
     1 = "Acceptable"
     8 = "Don't know"
     ;
  value F00081_
     1 = "Wanted then"
     2 = "Wanted later"
     3 = "Wanted no more"
     ;
  value F00082_
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
  value F00083_
     0 = "Package not seen"
     1 = "Package seen"
     ;
  value F00084_
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
  value F00085_
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
  value F00086_
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
  value F00087_
     1 = "Govt Clinical/Pharm"
     2 = "Govt Home/Comm deliv"
     3 = "NGO"
     4 = "Private Clin/Deliv"
     5 = "Private Pharmacy"
     6 = "Shop, church, friend"
     7 = "Other"
     8 = "Don't know"
     ;
  value F00088_
     0 = "No"
     1 = "Yes"
     ;
  value F00089_
     0 = "No"
     1 = "Yes"
     ;
  value F00090_
     0 = "No"
     1 = "Yes"
     ;
  value F00091_
     0 = "No"
     1 = "Yes - 6 months"
     8 = "Don't know"
     ;
  value F00092_
     0 = "No"
     1 = "Yes"
     ;
  value F00093_
     0 = "No"
     1 = "Yes"
     ;
  value F00094_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00095_
     0 = "No"
     1 = "Yes"
     ;
  value F00096_
     0 = "No"
     1 = "Yes"
     ;
  value F00097_
     0 = "No"
     1 = "Yes"
     ;
  value F00098_
     0 = "No"
     1 = "Yes"
     ;
  value F00099_
    96 = "On demand"
     ;
  value F00100_
    96 = "On demand"
     ;
  value F00101_
     0 = "No"
     1 = "Yes"
     ;
  value F00102_
     0 = "No"
     1 = "Yes"
     ;
  value F00103_
     0 = "No"
     1 = "Yes"
     ;
  value F00104_
     0 = "No"
     1 = "Yes"
     ;
  value F00105_
     0 = "No"
     1 = "Yes"
     ;
  value F00106_
     0 = "No"
     1 = "Yes"
     ;
  value F00107_
     0 = "No"
     1 = "Yes"
     ;
  value F00108_
     0 = "No"
     1 = "Yes"
     ;
  value F00109_
     0 = "No"
     1 = "Yes"
     ;
  value F00110_
     0 = "No"
     1 = "Yes"
     ;
  value F00111_
     0 = "No"
     1 = "Yes"
     ;
  value F00112_
     0 = "No"
     1 = "Yes"
     ;
  value F00113_
     0 = "Never heard of"
     1 = "Used ORS"
     2 = "Heard of ORS"
     ;
  value F00114_
     0 = "Immediately"
     ;
  value F00115_
     0 = "Never married"
     1 = "Married"
     2 = "Living together"
     3 = "Widowed"
     4 = "Divorced"
     5 = "Not living together"
     ;
  value F00116_
     0 = "Never married"
     1 = "Currently married"
     2 = "Formerly married"
     ;
  value F00117_
     1 = "Once"
     2 = "More than once"
     ;
  value F00118_
     1 = "Living with her"
     2 = "Staying elsewhere"
     ;
  value F00119_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value F00120_
     0 = "Never married"
     1 = "0-4"
     2 = "5-9"
     3 = "10-14"
     4 = "15-19"
     5 = "20-24"
     6 = "25-29"
     7 = "30+"
     ;
  value F00121_
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00122_
    31 = "31+ days"
    95 = "Within last 4 weeks"
    96 = "Before last birth"
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00123_
   995 = "Within last 4 weeks"
   996 = "Before last birth"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00124_
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
  value F00125_
     0 = "No"
     1 = "Formerly married"
     2 = "Lived with a man"
     ;
  value F00126_
     1 = "Have another"
     2 = "Undecided"
     3 = "No more"
     4 = "Sterilized"
     5 = "Declared infecund"
     6 = "Never had sex"
     ;
  value F00127_
   993 = "After marriage"
   994 = "Soon, now"
   996 = "Other, Non-numeric"
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00128_
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
  value F00129_
     1 = "Wants within 2 years"
     2 = "Wants after 2+ years"
     3 = "Wants, unsure timing"
     4 = "Undecided"
     5 = "Wants no more"
     6 = "Sterilized"
     7 = "Declared infecund"
     8 = "Never had sex"
     ;
  value F00130_
     1 = "Happy"
     2 = "Unhappy"
     3 = "Would not matter"
     8 = "DK"
     ;
  value F00131_
     0 = "Disapproves"
     1 = "Approves"
     8 = "Don't know"
     ;
  value F00132_
     0 = "Never"
     1 = "Once or twice"
     2 = "More often"
     ;
  value F00133_
     0 = "Disapproves"
     1 = "Approves"
     8 = "Don't know"
     ;
  value F00134_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value F00135_
     6 = "6+"
     7 = "Non-numeric response"
     ;
  value F00136_
   993 = "After marriage"
   994 = "Soon/now"
   995 = "Can't get pregnant"
   996 = "Other"
   998 = "DK "
     ;
  value F00137_
     0 = "No regret"
     1 = "Resp. wants another"
     2 = "Part. wants another"
     3 = "Side effects"
     4 = "Child died"
     6 = "Other reason"
     ;
  value F00138_
     1 = "Both want same"
     2 = "Husband wants more"
     3 = "Husband wants fewer"
     8 = "DK"
     ;
  value F00139_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value F00140_
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
  value F00141_
     0 = "Fecund"
     1 = "Pregnant"
     2 = "Amenorrheic"
     3 = "Infecund, menopausal"
     ;
  value F00142_
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
  value F00143_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value F00144_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value F00145_
    95 = "Up to God"
    96 = "Other"
    98 = "DK"
     ;
  value F00146_
     0 = "No education"
     1 = "Primary"
     2 = "Secondary"
     3 = "Higher"
     8 = "Don't know"
     ;
  value F00147_
    98 = "Don't know"
     ;
  value F00148_
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
  value F00149_
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
  value F00150_
     0 = "His land"
     1 = "Family land"
     2 = "Someone else's land"
     3 = "Rented land"
     8 = "Don't know"
     ;
  value F00151_
     0 = "No"
     1 = "Yes"
     ;
  value F00152_
    97 = "Inconsistent"
    98 = "Don't know"
     ;
  value F00153_
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
  value F00154_
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
  value F00155_
     0 = "Did not work"
     1 = "Paid employee, away"
     2 = "Paid employee, home"
     3 = "Self-employed, away"
     4 = "Self-employed, home"
     5 = "Unpaid worker, away"
     6 = "Unpaid worker, home"
     ;
  value F00156_
     1 = "For family member"
     2 = "For someone else"
     3 = "Self-employed"
     ;
  value F00157_
     0 = "No"
     1 = "Yes"
     ;
  value F00158_
     1 = "At home"
     2 = "Away"
     ;
  value F00159_
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
  value F00160_
     0 = "No education"
     1 = "Incomplete primary"
     2 = "Complete primary"
     3 = "Incomplete secondary"
     4 = "Complete secondary"
     5 = "Higher"
     8 = "Don't know"
     ;
  value F00161_
     1 = "All year"
     2 = "Seasonal"
     3 = "Occasional"
     ;
  value F00162_
     1 = "Respondent"
     2 = "Partner"
     3 = "Jointly with partner"
     4 = "Someone else"
     5 = "Jointly with someone"
     ;
  value F00163_
     0 = "Own land"
     1 = "Family land"
     2 = "Someone else's land"
     3 = "Rented land"
     8 = "Don't know"
     ;
  value F00164_
     0 = "Single birth"
     1 = "1st of multiple"
     2 = "2nd of multiple"
     3 = "3rd of multiple"
     4 = "4th of multiple"
     5 = "5th of multiple"
     ;
  value F00165_
     1 = "Male"
     2 = "Female"
     ;
  value F00166_
     0 = "No"
     1 = "Yes"
     ;
  value F00167_
   997 = "Inconsistent"
   998 = "Don't know"
     ;
  value F00168_
     0 = "Respondent"
     1 = "Father"
     2 = "Other relative"
     3 = "Someone else"
     4 = "Lives elsewhere"
     ;
  value F00169_
     1 = "Month and year"
     2 = "Month and age -y imp"
     3 = "Year and age - m imp"
     4 = "Y & age - y ignored"
     5 = "Year - a, m imp"
     6 = "Age - y, m imp"
     7 = "Month - a, y imp"
     8 = "None - all imp"
     ;
  value F00170_
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
  value F00171_
     0 = "Received no inject."
     7 = "7+"
     8 = "Don't know"
     ;
  value F00172_
     0 = "No"
     1 = "Yes: doctor"
     ;
  value F00173_
     0 = "No"
     1 = "Yes: nurse/midwife"
     ;
  value F00174_
     0 = "No"
     1 = "Yes: FW Visitor ."
     ;
  value F00175_
     0 = "No"
     1 = "Yes: trained birth A"
     ;
  value F00176_
     0 = "No"
     1 = "Yes: trad.birth att."
     ;
  value F00177_
     0 = "No"
     1 = "Yes: other responses"
     ;
  value F00178_
     0 = "No: some care"
     1 = "Yes: no one"
     ;
  value F00179_
     0 = "No"
     1 = "Yes: doctor"
     ;
  value F00180_
     0 = "No"
     1 = "Yes: nurse/midwife"
     ;
  value F00181_
     0 = "No"
     1 = "Yes: FW Visitor ."
     ;
  value F00182_
     0 = "No"
     1 = "Yes: trained birth A"
     ;
  value F00183_
     0 = "No"
     1 = "Yes: trad.birth att."
     ;
  value F00184_
     0 = "No"
     1 = "Yes: relative,friend"
     ;
  value F00185_
     0 = "No"
     1 = "Yes: other responses"
     ;
  value F00186_
     0 = "No"
     1 = "Yes: CS other"
     ;
  value F00187_
     0 = "No: some assistance"
     1 = "Yes: no one"
     ;
  value F00188_
    94 = "Never breastfed"
    95 = "Still breastfeeding"
    96 = "Breastfed until died"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00189_
    94 = "Never breastfed"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00190_
    96 = "Period not returned"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00191_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00192_
    96 = "Still abstaining"
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00193_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00194_
     1 = "Then"
     2 = "Later"
     3 = "No more"
     ;
  value F00195_
    98 = "DK"
     ;
  value F00196_
     0 = "No antenatal visits"
    98 = "DK"
     ;
  value F00197_
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
  value F00198_
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
  value F00199_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 mnth"
     3 = "> age at death"
     4 = "During preg in cal."
     ;
  value F00200_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 mnth"
     4 = "During preg in cal."
     ;
  value F00201_
     0 = "No flag"
     1 = "> interval"
     2 = "> interval by 1 mnth"
     4 = "During preg in cal."
     ;
  value F00202_
     0 = "Immediately"
   101 = "Hours: 1"
   201 = "Days: 1"
     ;
  value F00203_
    96 = "On demand"
     ;
  value F00204_
    96 = "On demand"
     ;
  value F00205_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00206_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00207_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00208_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00209_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00210_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00211_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00212_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00213_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00214_
     0 = "No card"
     1 = "Yes, seen"
     2 = "Yes, not seen"
     3 = "No longer has card"
     ;
  value F00215_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00216_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00217_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00218_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00219_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00220_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00221_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00222_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00223_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00224_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00225_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00226_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00227_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00228_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00229_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00230_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00231_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00232_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00233_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00234_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00235_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00236_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00237_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00238_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00239_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00240_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00241_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00242_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00243_
     0 = "No"
     1 = "Vacc. date on card"
     2 = "Reported by mother"
     3 = "Vacc. marked on card"
     8 = "DK"
     ;
  value F00244_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00245_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00246_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00247_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00248_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00249_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00250_
     0 = "No"
     1 = "Yes"
     8 = "Don't know"
     ;
  value F00251_
     0 = "No"
     1 = "Yes, last 24 hours"
     2 = "Yes, last two weeks"
     8 = "Don't know"
     ;
  value F00252_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00253_
     0 = "No"
     1 = "Government hospital"
     ;
  value F00254_
     0 = "No"
     1 = "Govt. health center"
     ;
  value F00255_
     0 = "No"
     1 = "Comm.health worker"
     ;
  value F00256_
     0 = "No"
     1 = "Satellite clinic"
     ;
  value F00257_
     0 = "No"
     1 = "Private hosp/clinic"
     ;
  value F00258_
     0 = "No"
     1 = "Private pharmacy"
     ;
  value F00259_
     0 = "No"
     1 = "Private doctor"
     ;
  value F00260_
     0 = "No"
     1 = "Shop"
     ;
  value F00261_
     0 = "No"
     1 = "Traditional pract."
     ;
  value F00262_
     0 = "No"
     1 = "Other"
     ;
  value F00263_
     0 = "No: received treatm."
     1 = "Yes: no treatment"
     ;
  value F00264_
     0 = "No"
     1 = "Yes: medical treatm."
     ;
  value F00265_
     0 = "No"
     1 = "Yes: ORS - spontan."
     2 = "Yes: ORS - probed"
     8 = "Don't know"
     ;
  value F00266_
     0 = "No"
     1 = "Yes: RHS - spontan."
     2 = "Yes: RHS - probed"
     8 = "Don't know"
     ;
  value F00267_
     0 = "No"
     1 = "Yes: oth pill, syrup"
     ;
  value F00268_
     0 = "No"
     1 = "Yes: injection"
     ;
  value F00269_
     0 = "No"
     1 = "Yes: intravenous (IV"
     ;
  value F00270_
     0 = "No"
     1 = "Yes: home rem, herb."
     ;
  value F00271_
     0 = "Same"
     1 = "Increase"
     2 = "Decrease"
     8 = "DK"
     ;
  value F00272_
     0 = "No"
     1 = "Yes"
     ;
  value F00273_
     0 = "No: received treatm."
     1 = "Yes: no treatment"
     8 = "DK"
     ;
  value F00274_
     0 = "No"
     1 = "Yes"
     ;
  value F00275_
     0 = "No"
     1 = "Yes, last 24 hours"
     2 = "Yes, last two weeks"
     8 = "Don't know"
     ;
  value F00276_
     0 = "No"
     1 = "Yes"
     8 = "DK"
     ;
  value F00277_
     0 = "No"
     1 = "Government hospital"
     ;
  value F00278_
     0 = "No"
     1 = "Govt. health center"
     ;
  value F00279_
     0 = "No"
     1 = "Community health wrk"
     ;
  value F00280_
     0 = "No"
     1 = "Satellite clinic"
     ;
  value F00281_
     0 = "No"
     1 = "Private hosp./clinic"
     ;
  value F00282_
     0 = "No"
     1 = "Private pharmacy"
     ;
  value F00283_
     0 = "No"
     1 = "Private doctor"
     ;
  value F00284_
     0 = "No"
     1 = "Shop"
     ;
  value F00285_
     0 = "No"
     1 = "Traditional pract."
     ;
  value F00286_
     0 = "No"
     1 = "Other"
     ;
  value F00287_
     0 = "No: received treatm."
     1 = "Yes: no treatment"
     ;
  value F00288_
     0 = "No"
     1 = "Yes: medical treatm."
     ;
  value F00289_
     1 = "Bengali"
     2 = "English"
     ;
  value F00290_
     0 = "Not adjusted"
     1 = "Adjusted"
     ;
  value F00291_
     1 = "Barishal"
     2 = "Chittagong"
     3 = "Dhaka"
     4 = "Khulna"
     5 = "Rajshani"
     ;
  value F00292_
     0 = "No"
     1 = "Yes"
     ;
  value F00293_
     0 = "No"
     1 = "Yes"
     ;
  value F00294_
     0 = "No"
     1 = "Yes"
     ;
  value F00295_
     0 = "No"
     1 = "Yes"
     ;
  value F00296_
     0 = "No"
     1 = "Yes"
     ;
  value F00297_
   996 = "On premises"
     ;
  value F00298_
     0 = "No children"
    11 = "Sep.tank/modern tlet"
    21 = "Water sld/Slab latr."
    22 = "Pit latrine"
    23 = "Open latrine"
    24 = "Hanging latrine"
    31 = "No facility,bush"
    96 = "Other"
     ;
  value F00299_
     0 = "No"
     1 = "Yes"
     ;
  value F00300_
     0 = "No"
     1 = "Yes"
     ;
  value F00301_
     0 = "No"
     1 = "Yes"
     ;
  value F00302_
     0 = "No"
     1 = "Yes"
     ;
  value F00303_
     0 = "No"
     1 = "Yes"
     ;
  value F00304_
    97 = "Inconsistent"
    98 = "DK"
     ;
  value F00305_
  9997 = "Inconsistent"
  9998 = "DK  "
     ;
  value F00306_
     0 = "No"
     1 = "Yes"
     ;
  value F00307_
     0 = "No"
     1 = "Yes"
     ;
  value F00308_
     1 = "Menstrual regulation"
     2 = "Induced abortion/D&C"
     3 = "Other"
     ;
  value F00309_
  1300 = "Bengali (Start)"
  1401 = "Bengali (end)"
  1900 = "English (Start)"
  1994 = "English(end)"
  9997 = "Inconsistent"
  9998 = "Don't Know"
     ;
Data user.BDBR31FL;
  attrib CASEID   length=$15 label="Case Identification";
  attrib BIDX     label="Birth column number";
  attrib V000     length=$3 label="Country code and phase";
  attrib V001     label="Cluster number";
  attrib V002     label="Household number";
  attrib V003     label="Respondent's line number";
  attrib V004     label="Ultimate area unit";
  attrib V005     label="Sample weight";
  attrib V006     label="Month of interview";
  attrib V007     label="Year of interview";
  attrib V008     label="Date of interview (CMC)";
  attrib V009     label="Respondent's month of birth";
  attrib V010     label="Respondent's year of birth";
  attrib V011     label="Date of birth (CMC)";
  attrib V012     label="Current age - respondent";
  attrib V013     format=F00001_. label="Age 5-year groups";
  attrib V014     format=F00002_. label="Completeness of information";
  attrib V015     format=F00003_. label="Result of individual interview";
  attrib V016     label="Day of interview";
  attrib V017     label="CMC start of calendar";
  attrib V018     format=F00004_. label="Row of month of interview";
  attrib V019     format=F00005_. label="Length of calendar";
  attrib V020     format=F00006_. label="Ever-married sample";
  attrib V021     label="Primary sampling unit";
  attrib V022     label="Sample stratum number";
  attrib V023     format=F00007_. label="Sample domain";
  attrib V024     format=F00008_. label="Region";
  attrib V025     format=F00009_. label="Type of place of residence";
  attrib V026     format=F00010_. label="De facto place of residence";
  attrib V027     label="Number of visits";
  attrib V028     label="Interviewer identification";
  attrib V029     label="Keyer identification";
  attrib V030     label="Field supervisor -NA";
  attrib V031     label="Field editor -NA";
  attrib V032     label="Office editor -NA";
  attrib V033     label="Ultimate area selection prob.";
  attrib V034     format=F00011_. label="Line number of husband";
  attrib V101     format=F00012_. label="Region";
  attrib V102     format=F00013_. label="Type of place of residence";
  attrib V103     label="Childhood place of residen -NA";
  attrib V104     format=F00014_. label="Years lived in place of res.";
  attrib V105     format=F00015_. label="Type of place of previous res.";
  attrib V106     format=F00016_. label="Highest educational level";
  attrib V107     label="Highest year of education";
  attrib V108     format=F00017_. label="Literacy";
  attrib V109     format=F00018_. label="Reads newspaper once a week";
  attrib V110     format=F00019_. label="Watches TV every week";
  attrib V111     label="Listens to radio every day -NA";
  attrib V112     format=F00020_. label="Listens to radio every week";
  attrib V113     format=F00021_. label="Source of drinking water";
  attrib V114     format=F00022_. label="Source of non-drinking water";
  attrib V115     format=F00023_. label="Time to get to water source";
  attrib V116     format=F00024_. label="Type of toilet facility";
  attrib V119     format=F00025_. label="Has electricity";
  attrib V120     format=F00026_. label="Has radio";
  attrib V121     format=F00027_. label="Has television";
  attrib V122     label="Has refrigerator -NA";
  attrib V123     format=F00028_. label="Has bicycle";
  attrib V124     label="Has motorcycle -NA";
  attrib V125     label="Has car -NA";
  attrib V127     format=F00029_. label="Main floor material";
  attrib V128     format=F00030_. label="Main wall material";
  attrib V129     format=F00031_. label="Main roof material";
  attrib V130     format=F00032_. label="Religion";
  attrib V131     label="Ethnicity -NA";
  attrib V133     format=F00033_. label="Education in single years";
  attrib V134     format=F00034_. label="De facto place of residence";
  attrib V135     format=F00035_. label="Usual resident or visitor";
  attrib V136     label="Number of household members";
  attrib V137     label="Number of children 5 and under";
  attrib V138     label="Number of eligible women in HH";
  attrib V139     format=F00036_. label="De jure region of residence";
  attrib V140     format=F00037_. label="De jure type of place of res.";
  attrib V141     format=F00038_. label="De jure place of residence";
  attrib V142     format=F00039_. label="Same source of water";
  attrib V144     label="Communities lived in >Jan.8-NA";
  attrib V148     format=F00040_. label="Respondent still in school";
  attrib V149     format=F00041_. label="Educational attainment";
  attrib V150     format=F00042_. label="Relationship to household head";
  attrib V151     format=F00043_. label="Sex of household head";
  attrib V152     format=F00044_. label="Age of household head";
  attrib V153     label="Has telephone -NA";
  attrib V154     label="Reason stop attending schoo-NA";
  attrib AWFACTT  label="All woman factor - total";
  attrib AWFACTU  label="All woman factor - urban/rural";
  attrib AWFACTR  label="All woman factor - regional";
  attrib AWFACTE  label="All woman factor - educational";
  attrib V201     label="Total children ever born";
  attrib V202     label="Sons at home";
  attrib V203     label="Daughters at home";
  attrib V204     label="Sons elsewhere";
  attrib V205     label="Daughters elsewhere";
  attrib V206     label="Sons who have died";
  attrib V207     label="Daughters who have died";
  attrib V208     label="Births in last five years";
  attrib V209     label="Births in past year";
  attrib V210     label="Births in month of interview";
  attrib V211     label="Date of first birth (CMC)";
  attrib V212     label="Age of respondent at 1st birth";
  attrib V213     format=F00045_. label="Currently pregnant";
  attrib V214     label="Duration of current pregnancy";
  attrib V215     format=F00046_. label="Time since last menstrual perd";
  attrib V216     format=F00047_. label="Menstruated in last six weeks";
  attrib V217     label="Knowledge of ovulatory cycl-NA";
  attrib V218     label="Number of living children";
  attrib V219     label="Living children + current preg";
  attrib V220     format=F00048_. label="Living children + curr preg 6+";
  attrib V221     format=F00049_. label="Marriage to first birth int.";
  attrib V222     label="Last birth to interview";
  attrib V223     format=F00050_. label="Completeness of curr preg. inf";
  attrib V224     label="Entries in birth history";
  attrib V225     format=F00051_. label="Current pregnancy wanted";
  attrib V226     format=F00052_. label="Time since last period (comp)";
  attrib V227     format=F00053_. label="Flag for last period";
  attrib V228     format=F00054_. label="Ever had a terminated preg.";
  attrib V229     format=F00055_. label="Month pregnancy ended";
  attrib V230     format=F00056_. label="Year pregnancy ended";
  attrib V231     format=F00057_. label="CMC pregnancy ended";
  attrib V232     format=F00058_. label="Completeness of last term. inf";
  attrib V233     format=F00059_. label="Months when pregnancy ended";
  attrib V234     format=F00060_. label="Other such pregnancies";
  attrib V235     format=F00061_. label="Index last child prior to cal.";
  attrib V236     label="Last birth interval >= 4 ye-NA";
  attrib V237     label="Birth between last & interv-NA";
  attrib V238     label="Births in last three years";
  attrib V310     label="Living children at first use";
  attrib V311     format=F00062_. label="Children at first use (grpd)";
  attrib V312     format=F00063_. label="Current contraceptive method";
  attrib V313     format=F00064_. label="Current use by method type";
  attrib V314     label="Method of periodic abstinen-NA";
  attrib V315     label="Month of sterilization";
  attrib V316     label="Year of sterilization";
  attrib V317     label="Date of sterilization (CMC)";
  attrib V318     format=F00065_. label="Completeness of information";
  attrib V319     format=F00066_. label="Years since sterilization";
  attrib V320     format=F00067_. label="Age at sterilization";
  attrib V321     format=F00068_. label="Marital duration at steriliz.";
  attrib V322     format=F00069_. label="Parity at sterilization";
  attrib V323     format=F00070_. label="Brand of pill used";
  attrib V325     label="Cost of pills";
  attrib V326     format=F00071_. label="Last source for current users";
  attrib V327     format=F00072_. label="Last source for users by type";
  attrib V328     label="Main reason use current sou-NA";
  attrib V337     format=F00073_. label="Months of use of current meth";
  attrib V359     format=F00074_. label="Last method disc. last 5 years";
  attrib V360     format=F00075_. label="Reason of last discontinuation";
  attrib V361     format=F00076_. label="Pattern of use";
  attrib V362     format=F00077_. label="Intention to use";
  attrib V363     format=F00078_. label="Preferred future method";
  attrib V364     format=F00079_. label="Contraceptive use & intention";
  attrib V366A    format=F00080_. label="Acceptability of radio message";
  attrib V366B    label="Acceptability of TV message-NA";
  attrib V367     format=F00081_. label="Wanted last child";
  attrib V369     format=F00082_. label="First method used";
  attrib V369B    label="First use to space or limit-NA";
  attrib V372     format=F00083_. label="Shown pill package";
  attrib V375A    format=F00084_. label="Main reason not using a method";
  attrib V376     format=F00085_. label="Main reason not to use a meth.";
  attrib V376A    label="Would ever use method if ma-NA";
  attrib V379     format=F00086_. label="Source known for any method";
  attrib V380     format=F00087_. label="Source known for any method";
  attrib V384A    format=F00088_. label="Heard FP on radio last months";
  attrib V384B    format=F00089_. label="Heard FP on TV last months";
  attrib V384C    label="Heard FP newspaper last mon-NA";
  attrib V384D    format=F00090_. label="Heard FP on poster last months";
  attrib V384E    label="Heard FP brochures last mon-NA";
  attrib V393     format=F00091_. label="Visited by FP worker last 6m";
  attrib V394     format=F00092_. label="Visited health facil. last 12m";
  attrib V395     format=F00093_. label="At health facility, told of FP";
  attrib V396     format=F00094_. label="Breastfeeding affect pregnancy";
  attrib V397     label="Pregnancy risk of breastfee-NA";
  attrib V398     label="Relied breastfeed. avoid pr-NA";
  attrib V399     label="Currently rely on breastfee-NA";
  attrib V401     format=F00095_. label="Last birth ceasarean sectio-NA";
  attrib V404     format=F00096_. label="Currently breastfeeding";
  attrib V405     format=F00097_. label="Currently amenorrheic";
  attrib V406     format=F00098_. label="Currently abstaining";
  attrib V407     format=F00099_. label="Times breastfed during night";
  attrib V408     format=F00100_. label="Times breastfed during day";
  attrib V409     format=F00101_. label="Gave child plain water";
  attrib V409A    format=F00102_. label="Gave child sugar water";
  attrib V410     format=F00103_. label="Gave child juice";
  attrib V410A    format=F00104_. label="Gave child herbal tea";
  attrib V411     label="Gave child powder/tinned mi-NA";
  attrib V411A    format=F00105_. label="Gave child baby formula";
  attrib V412     format=F00106_. label="Gave child fresh milk";
  attrib V413A    label="Country specific other liqu-NA";
  attrib V413B    label="Country specific other liqu-NA";
  attrib V413C    label="Country specific other liqu-NA";
  attrib V413D    label="Country specific other liqu-NA";
  attrib V413     format=F00107_. label="Gave child other liquid";
  attrib V414A    label="Country specific other food-NA";
  attrib V414B    label="Country specific other food-NA";
  attrib V414C    label="Country specific other food-NA";
  attrib V414D    format=F00108_. label="Other solid, semi-solid foods";
  attrib V414E    format=F00109_. label="Food made from local grain -NA";
  attrib V414F    format=F00110_. label="Food made from local tuber -NA";
  attrib V414G    format=F00111_. label="Gave child eggs, fish, poul-NA";
  attrib V414H    format=F00112_. label="Gave child meat";
  attrib V415     label="Drank from bottle with nipp-NA";
  attrib V416     format=F00113_. label="Heard of oral rehydration";
  attrib V417     label="Entries in maternity table";
  attrib V418     label="Entries in health table";
  attrib V419     label="Entries in height/weight ta-NA";
  attrib V420     label="Measurer's code -NA";
  attrib V421     label="Assistant measurer's code -NA";
  attrib V426     format=F00114_. label="When child put to breast";
  attrib V436     label="Arm circumference (cms-1d) -NA";
  attrib V437     label="Respondent's weight (kilos--NA";
  attrib V438     label="Respondent's height (cms-1d-NA";
  attrib V439     label="Ht/A Percentile (resp.) -NA";
  attrib V440     label="Ht/A Standard deviations (r-NA";
  attrib V441     label="Ht/A Percent ref. median (r-NA";
  attrib V442     label="Wt/Ht Percent ref. median (-NA";
  attrib V443     label="Wt/Ht Percent ref. median (-NA";
  attrib V444     label="Wt/Ht Percent ref. median (-NA";
  attrib V444A    label="Wt/Ht Std deviations(resp) -NA";
  attrib V445     label="Body mass index for respond-NA";
  attrib V446     label="Rohrer's index for responde-NA";
  attrib V447     label="Result of measurement of re-NA";
  attrib V448     label="Drinking pattern with diarr-NA";
  attrib V449     label="Eating pattern with diarrhe-NA";
  attrib V450A    label="Diarrhea: repeat watery sto-NA";
  attrib V450B    label="Diarrhea: Any watery stools-NA";
  attrib V450C    label="Diarrhea: Repeated vomiting-NA";
  attrib V450D    label="Diarrhea: Any vomiting -NA";
  attrib V450E    label="Diarrhea: Blood in stools -NA";
  attrib V450F    label="Diarrhea: Fever -NA";
  attrib V450G    label="Diarrhea: Marked thirst -NA";
  attrib V450H    label="Diarrhea: Not eating/drinki-NA";
  attrib V450I    label="Diarrhea: Getting sicker -NA";
  attrib V450J    label="Diarrhea: Not getting bette-NA";
  attrib V450K    label="Diarrhea: Country specific -NA";
  attrib V450L    label="Diarrhea: Country specific -NA";
  attrib V450M    label="Diarrhea: Country specific -NA";
  attrib V450X    label="Diarrhea: Other responses -NA";
  attrib V450Z    label="Diarrhea: Does not know -NA";
  attrib V451A    label="Cough: Fast breathing -NA";
  attrib V451B    label="Cough: Difficult breathing -NA";
  attrib V451C    label="Cough: Noisy breathing -NA";
  attrib V451D    label="Cough: Fever -NA";
  attrib V451E    label="Cough: Unable to drink -NA";
  attrib V451F    label="Cough: Not eating/drinking -NA";
  attrib V451G    label="Cough: Getting sicker -NA";
  attrib V451H    label="Cough: Not getting better -NA";
  attrib V451I    label="Cough: Country specific -NA";
  attrib V451J    label="Cough: Country specific -NA";
  attrib V451K    label="Cough: Country specific -NA";
  attrib V451X    label="Cough: Other responses -NA";
  attrib V451Z    label="Knows no sign of illness -NA";
  attrib V501     format=F00115_. label="Current marital status";
  attrib V502     format=F00116_. label="Currently/formerly/never marr.";
  attrib V503     format=F00117_. label="Number of unions";
  attrib V504     format=F00118_. label="Husband lives in house";
  attrib V505     label="Number of other wives - NA";
  attrib V506     label="Wife rank number - NA";
  attrib V507     label="Month of first marriage";
  attrib V508     label="Year of first marriage";
  attrib V509     label="Date of first marriage (CMC)";
  attrib V510     format=F00119_. label="Completeness of date inform.";
  attrib V511     label="Age at first marriage";
  attrib V512     label="Years since first marriage";
  attrib V513     format=F00120_. label="Marital duration (grouped)";
  attrib V525     label="Age at first intercourse - NA";
  attrib V527     format=F00121_. label="Time since last intercourse";
  attrib V528     format=F00122_. label="Time since last intercourse(g)";
  attrib V529     format=F00123_. label="Time since last intercourse(im";
  attrib V530     format=F00124_. label="Flag for V529";
  attrib V531     label="Age at first intercourse - NA";
  attrib V532     label="Flag for V531 -NA";
  attrib V534     label="Have a regular sex partne - NA";
  attrib V535     format=F00125_. label="Have ever been married";
  attrib V602     format=F00126_. label="Fertility preference";
  attrib V603     format=F00127_. label="Preferred waiting time";
  attrib V604     format=F00128_. label="Preferred waiting time (grpd)";
  attrib V605     format=F00129_. label="Desire for more children";
  attrib V606     format=F00130_. label="Attitude toward becoming preg.";
  attrib V610     format=F00131_. label="Husband approves FP";
  attrib V611     format=F00132_. label="Discussed FP with partner";
  attrib V612     format=F00133_. label="Respondent approves FP";
  attrib V613     format=F00134_. label="Ideal number of children";
  attrib V614     format=F00135_. label="Ideal number of children (grp)";
  attrib V616     format=F00136_. label="Time for future birth";
  attrib V618     format=F00137_. label="Reason regrets sterilization";
  attrib V621     format=F00138_. label="Husbands desire for children";
  attrib V623     format=F00139_. label="Exposure";
  attrib V624     format=F00140_. label="Unmet need";
  attrib V625     format=F00141_. label="Exposure (definition 2)";
  attrib V626     format=F00142_. label="Unmet need (definition 2)";
  attrib V627     format=F00143_. label="Ideal number of boys";
  attrib V628     format=F00144_. label="Ideal number of girls";
  attrib V629     format=F00145_. label="Ideal number of either sex";
  attrib V630A    label="Discussed FP with partner - NA";
  attrib V630B    label="Discussed FP with mother - NA";
  attrib V630C    label="Discussed FP with father - NA";
  attrib V630D    label="Discussed FP with sister( - NA";
  attrib V630E    label="Discussed FP with brother - NA";
  attrib V630F    label="Discussed FP with daughte - NA";
  attrib V630G    label="Discussed FP with mother- - NA";
  attrib V630H    label="Discussed FP with friends - NA";
  attrib V630I    label="Discussed FP -country spe - NA";
  attrib V630J    label="Discussed FP -country spe - NA";
  attrib V630K    label="Discussed FP -country spe - NA";
  attrib V630L    label="Discussed FP -country spe - NA";
  attrib V630X    label="Discussed FP with others - NA";
  attrib V701     format=F00146_. label="Partner's education level";
  attrib V702     format=F00147_. label="Highest year of education";
  attrib V704     format=F00148_. label="Partner's occupation";
  attrib V705     format=F00149_. label="Partner's occupation";
  attrib V707     format=F00150_. label="Type of land where works";
  attrib V714     format=F00151_. label="Respondent currently working";
  attrib V715     format=F00152_. label="Husbands education-single yrs";
  attrib V716     format=F00153_. label="Respondent's occupation";
  attrib V717     format=F00154_. label="Respondent's occupation";
  attrib V718     format=F00155_. label="Current type of employment";
  attrib V719     format=F00156_. label="Work for family, others, self";
  attrib V720     format=F00157_. label="Earns cash for work";
  attrib V721     format=F00158_. label="Works at home or away";
  attrib V723     format=F00159_. label="Child minder";
  attrib V729     format=F00160_. label="Partner's educational attainm.";
  attrib V730     label="Partners age -NA";
  attrib V731     label="Worked in last 12 months - NA";
  attrib V732     format=F00161_. label="Employment all year/seasonal";
  attrib V733     label="Months worked in last 12 - NA";
  attrib V734     label="Days in a week normally w - NA";
  attrib V735     label="Days worked in last 12 mo - NA";
  attrib V736     label="Earnings for work - NA";
  attrib V737     label="Daily earnings for work - NA";
  attrib V738     label="Yearly earnings for work - NA";
  attrib V739     format=F00162_. label="Who decides how to spend money";
  attrib V740     format=F00163_. label="Type of land where resp. works";
  attrib BORD     label="Birth order number";
  attrib B0       format=F00164_. label="Child is twin";
  attrib B1       label="Month of birth";
  attrib B2       label="Year of birth";
  attrib B3       label="Date of birth (CMC)";
  attrib B4       format=F00165_. label="Sex of child";
  attrib B5       format=F00166_. label="Child is alive";
  attrib B6       format=F00167_. label="Age at death";
  attrib B7       label="Age at death (months-imputed)";
  attrib B8       label="Current age of child";
  attrib B9       format=F00168_. label="Who child lives with";
  attrib B10      format=F00169_. label="Completeness of information";
  attrib B11      label="Preceding birth interval";
  attrib B12      label="Succeeding birth interval";
  attrib B13      format=F00170_. label="Flag for age at death";
  attrib B14      label="Birth interval >= 4 years -NA";
  attrib B15      label="Live birth between births -NA";
  attrib MIDX     label="Index to birth history";
  attrib M1       format=F00171_. label="Tetanus injections bef. birth";
  attrib M2A      format=F00172_. label="Prenatal: doctor";
  attrib M2B      format=F00173_. label="Prenatal: nurse/midwife";
  attrib M2C      label="Prenatal: auxiliary midwife-NA";
  attrib M2D      format=F00174_. label="Prenatal: Family Welf.Visitor";
  attrib M2E      label="Prenatal: CS health profess-NA";
  attrib M2F      format=F00175_. label="Prenatal: trained birth att.";
  attrib M2G      format=F00176_. label="Prenatal: trad.birth attendant";
  attrib M2H      label="Prenatal: relative -NA";
  attrib M2I      label="Prenatal: CS other person -NA";
  attrib M2J      label="Prenatal: CS other person -NA";
  attrib M2K      format=F00177_. label="Prenatal: other resp (uncoded)";
  attrib M2L      label="Prenatal: CS other -NA";
  attrib M2M      label="Prenatal: CS other -NA";
  attrib M2N      format=F00178_. label="Prenatal: no one";
  attrib M3A      format=F00179_. label="Assistance: doctor";
  attrib M3B      format=F00180_. label="Assistance: nurse/midwife";
  attrib M3C      label="Delivery: auxiliary midwife-NA";
  attrib M3D      format=F00181_. label="Delivery: Family Welf.Visitor";
  attrib M3E      label="Assistance: CS health profe-NA";
  attrib M3F      format=F00182_. label="Assistance: trained birth att.";
  attrib M3G      format=F00183_. label="Assistance: trad.birth attend.";
  attrib M3H      format=F00184_. label="Assistance: relative, friend";
  attrib M3I      label="Assistance: CS other person-NA";
  attrib M3J      label="Assistance: CS other person-NA";
  attrib M3K      format=F00185_. label="Assistance: other resp (uncod)";
  attrib M3L      label="Assistance: CS other -NA";
  attrib M3M      format=F00186_. label="Assistance: CS other";
  attrib M3N      format=F00187_. label="Assistance: no one";
  attrib M4       format=F00188_. label="Duration of breastfeeding";
  attrib M5       format=F00189_. label="Months of breastfeeding";
  attrib M6       format=F00190_. label="Duration of amenorrhea";
  attrib M7       format=F00191_. label="Months of amenorrhea";
  attrib M8       format=F00192_. label="Duration of abstinence";
  attrib M9       format=F00193_. label="Months of abstinence";
  attrib M10      format=F00194_. label="Time wanted pregnancy";
  attrib M11      label="Time would have waited -NA";
  attrib M13      format=F00195_. label="Timing of 1st antenatal check";
  attrib M14      format=F00196_. label="Antenatal visits for pregnancy";
  attrib M15      format=F00197_. label="Place of delivery";
  attrib M17      label="Delivery by caesarian secti-NA";
  attrib M18      label="Size of child at birth -NA";
  attrib M19      label="Birth weight (kilos - 3 dec-NA";
  attrib M19A     label="Weight at birth recall -NA";
  attrib M21      format=F00198_. label="Reason stopped breastfeeding";
  attrib M27      format=F00199_. label="Flag for breastfeeding";
  attrib M28      format=F00200_. label="Flag for amenorrhea";
  attrib M29      format=F00201_. label="Flag for abstinence";
  attrib M30      label="At birth - prolonged labor -NA";
  attrib M31      label="At birth - excessive bleedi-NA";
  attrib M32      label="At birth - high fever/disch-NA";
  attrib M33      label="At birth - convulsions -NA";
  attrib M34      format=F00202_. label="When child put to breast";
  attrib M35      format=F00203_. label="Times breastfed during night";
  attrib M36      format=F00204_. label="Times breastfed during day";
  attrib M37A     format=F00205_. label="Gave child plain water";
  attrib M37B     format=F00206_. label="Gave child sugar water";
  attrib M37C     format=F00207_. label="Gave child juice";
  attrib M37D     format=F00208_. label="Gave child herbal tea";
  attrib M37E     label="Gave child powder/tinned mi-NA";
  attrib M37F     format=F00209_. label="Gave child baby formula";
  attrib M37G     format=F00210_. label="Gave child fresh milk";
  attrib M37H     label="Country specific other liqu-NA";
  attrib M37I     label="Country specific other liqu-NA";
  attrib M37J     label="Country specific other liqu-NA";
  attrib M37K     label="Country specific other liqu-NA";
  attrib M37L     format=F00211_. label="Gave child other liquid";
  attrib M37M     label="Country specific other food-NA";
  attrib M37N     label="Country specific other food-NA";
  attrib M37O     label="Country specific other food-NA";
  attrib M37P     format=F00212_. label="Other solid, semi-solid foods";
  attrib M37Q     label="Food made from local grain -NA";
  attrib M37R     label="Food made from local tuber -NA";
  attrib M37S     label="Gave child eggs, fish, poul-NA";
  attrib M37T     format=F00213_. label="Gave child meat";
  attrib M38      label="Drank from bottle with nipp-NA";
  attrib M39      label="Times ate other food yester-NA";
  attrib M40A     label="Last 7 days - plain water -NA";
  attrib M40B     label="Last 7 days - milk -NA";
  attrib M40C     label="Last 7 days - other liquids-NA";
  attrib M40D     label="Last 7 days - local grain f-NA";
  attrib M40E     label="Last 7 days - local tuber f-NA";
  attrib M40F     label="Last 7 days - egg/fish/poul-NA";
  attrib M40G     label="Last 7 days - meat -NA";
  attrib M40H     label="Last 7 days - other solid f-NA";
  attrib M40I     label="Last 7 days - country speci-NA";
  attrib M40J     label="Last 7 days - country speci-NA";
  attrib M40K     label="Last 7 days - country speci-NA";
  attrib M40L     label="Last 7 days - country speci-NA";
  attrib M40M     label="Last 7 days - country speci-NA";
  attrib M40N     label="Last 7 days - country speci-NA";
  attrib M40O     label="Last 7 days - country speci-NA";
  attrib HIDX     label="Index to birth history";
  attrib H1       format=F00214_. label="Has health card";
  attrib H2       format=F00215_. label="Received BCG";
  attrib H2D      format=F00216_. label="BCG day";
  attrib H2M      format=F00217_. label="BCG month";
  attrib H2Y      format=F00218_. label="BCG year";
  attrib H3       format=F00219_. label="Received DPT 1";
  attrib H3D      format=F00220_. label="DPT 1 day";
  attrib H3M      format=F00221_. label="DPT 1 month";
  attrib H3Y      format=F00222_. label="DPT 1 year";
  attrib H4       format=F00223_. label="Received POLIO 1";
  attrib H4D      format=F00224_. label="POLIO 1 day";
  attrib H4M      format=F00225_. label="POLIO 1 month";
  attrib H4Y      format=F00226_. label="POLIO 1 year";
  attrib H5       format=F00227_. label="Received DPT 2";
  attrib H5D      format=F00228_. label="DPT 2 day";
  attrib H5M      format=F00229_. label="DPT 2 month";
  attrib H5Y      format=F00230_. label="DPT 2 year";
  attrib H6       format=F00231_. label="Received POLIO 2";
  attrib H6D      format=F00232_. label="POLIO 2 day";
  attrib H6M      format=F00233_. label="POLIO 2 month";
  attrib H6Y      format=F00234_. label="POLIO 2 year";
  attrib H7       format=F00235_. label="Received DPT 3";
  attrib H7D      format=F00236_. label="DPT 3 day";
  attrib H7M      format=F00237_. label="DPT 3 month";
  attrib H7Y      format=F00238_. label="DPT 3 year";
  attrib H8       format=F00239_. label="Received POLIO 3";
  attrib H8D      format=F00240_. label="POLIO 3 day";
  attrib H8M      format=F00241_. label="POLIO 3 month";
  attrib H8Y      format=F00242_. label="POLIO 3 year";
  attrib H9       format=F00243_. label="Received MEASLES";
  attrib H9D      format=F00244_. label="MEASLES day";
  attrib H9M      format=F00245_. label="MEASLES month";
  attrib H9Y      format=F00246_. label="MEASLES year";
  attrib H0       label="Polio 0 -NA";
  attrib H0D      format=F00247_. label="Polio 0 day";
  attrib H0M      format=F00248_. label="POLIO 0 month";
  attrib H0Y      format=F00249_. label="POLIO 0 year";
  attrib H10      format=F00250_. label="Ever had vaccination";
  attrib H11      format=F00251_. label="Had diarrhea recently";
  attrib H11B     format=F00252_. label="Blood in the stools";
  attrib H11C     label="Bowel movements in worst da-NA";
  attrib H12A     format=F00253_. label="Diarrhea: government hosp.";
  attrib H12B     format=F00254_. label="Diarrhea: govt health center";
  attrib H12C     label="Diarrhea: govt health post -NA";
  attrib H12D     label="Diarrhea: mobile clinic -NA";
  attrib H12E     format=F00255_. label="Diarrhea: comm.health worker";
  attrib H12F     format=F00256_. label="Diarrhea: Satellite clinic -CS";
  attrib H12G     label="Diarrhea: CS public sector -NA";
  attrib H12H     label="Diarrhea: CS public sector -NA";
  attrib H12I     label="Diarrhea: CS public sector -NA";
  attrib H12J     format=F00257_. label="Diarrhea: private hosp/clinic";
  attrib H12K     format=F00258_. label="Diarrhea: private pharmacy";
  attrib H12L     format=F00259_. label="Diarrhea: private doctor";
  attrib H12M     label="Diarrhea: private mobile cl-NA";
  attrib H12N     label="Diarrhea: comm.health worke-NA";
  attrib H12O     label="Diarrhea: CS med.priv secto-NA";
  attrib H12P     label="Diarrhea: CS med.priv secto-NA";
  attrib H12Q     label="Diarrhea: CS med.priv secto-NA";
  attrib H12R     label="Diarrhea: CS med.priv secto-NA";
  attrib H12S     format=F00260_. label="Diarrhea: shop";
  attrib H12T     format=F00261_. label="Diarrhea: traditional pract.";
  attrib H12U     label="Diarrhea: CS oth.priv secto-NA";
  attrib H12V     label="Diarrhea: CS oth.priv secto-NA";
  attrib H12W     label="Diarrhea: CS oth.priv secto-NA";
  attrib H12X     format=F00262_. label="Diarrhea: Other";
  attrib H12Y     format=F00263_. label="Diarrhea: no treatment";
  attrib H12Z     format=F00264_. label="Diarrhea: medical treatment";
  attrib H13      format=F00265_. label="Given oral rehydration";
  attrib H14      format=F00266_. label="Given recommend. home solution";
  attrib H15      format=F00267_. label="Given other pills or syrups";
  attrib H15A     label="Given antibiotics (CS) -NA";
  attrib H15B     format=F00268_. label="Given an injection";
  attrib H15C     format=F00269_. label="Given an intravenous (IV)";
  attrib H15D     format=F00270_. label="Given home remedy, herbal med.";
  attrib H15E     label="Given CS other treatment -NA";
  attrib H15F     label="Given CS other treatment -NA";
  attrib H15G     label="Given CS other treatment -NA";
  attrib H15H     label="Given CS other treatment -NA";
  attrib H16      format=F00271_. label="Increase or decrease fluids";
  attrib H18      label="Increase or decrease in foo-NA";
  attrib H20      format=F00272_. label="Given other treatment";
  attrib H21A     format=F00273_. label="Given no treatment";
  attrib H21      format=F00274_. label="Received any treatment";
  attrib H22      label="Had fever in last two weeks-NA";
  attrib H31      format=F00275_. label="Had cough in last two weeks";
  attrib H31B     format=F00276_. label="Short, rapid breaths";
  attrib H32A     format=F00277_. label="Fever/cough: government hosp.";
  attrib H32B     format=F00278_. label="Fever/cough: govt health cntr";
  attrib H32C     label="Fever/cough: govt health po-NA";
  attrib H32D     label="Fever/cough: mobile clinic -NA";
  attrib H32E     format=F00279_. label="Fever/cough: comm.health wrkr";
  attrib H32F     format=F00280_. label="Fever/cough: Satellite clinic";
  attrib H32G     label="Fever/cough: CS public sect-NA";
  attrib H32H     label="Fever/cough: CS public sect-NA";
  attrib H32I     label="Fever/cough: CS public sect-NA";
  attrib H32J     format=F00281_. label="Fever/cough: private hosp/clin";
  attrib H32K     format=F00282_. label="Fever/cough: private pharmacy";
  attrib H32L     format=F00283_. label="Fever/cough: private doctor";
  attrib H32M     label="Fever/cough: private mobile-NA";
  attrib H32N     label="Fever/cough: comm.health wr-NA";
  attrib H32O     label="Fever/cough: CS med.priv se-NA";
  attrib H32P     label="Fever/cough: CS med.priv se-NA";
  attrib H32Q     label="Fever/cough: CS med.priv se-NA";
  attrib H32R     label="Fever/cough: CS med.priv se-NA";
  attrib H32S     format=F00284_. label="Fever/cough: shop";
  attrib H32T     format=F00285_. label="Fever/cough: traditional pract";
  attrib H32U     label="Fever/cough: CS oth.priv se-NA";
  attrib H32V     label="Fever/cough: CS oth.priv se-NA";
  attrib H32W     label="Fever/cough: CS oth.priv se-NA";
  attrib H32X     format=F00286_. label="Fever/cough: Other";
  attrib H32Y     format=F00287_. label="Fever/cough: no treatment";
  attrib H32Z     format=F00288_. label="Fever/cough: medical treatment";
  attrib HWIDX    label="Index to birth history - NA";
  attrib HW1      label="Age in months - NA";
  attrib HW2      label="Weight in kilograms (1 de - NA";
  attrib HW3      label="Height in centimeters (1 - NA";
  attrib HW4      label="Ht/A Percentile - NA";
  attrib HW5      label="Ht/A Standard deviations - NA";
  attrib HW6      label="Ht/A Percent of ref. medi - NA";
  attrib HW7      label="Wt/A Percentile - NA";
  attrib HW8      label="Wt/A Standard deviations - NA";
  attrib HW9      label="Wt/A Percent of ref. medi - NA";
  attrib HW10     label="Wt/Ht Percentile - NA";
  attrib HW11     label="Wt/Ht Standard deviations - NA";
  attrib HW12     label="Wt/Ht Percent of ref. med - NA";
  attrib HW13     label="Reason not measured";
  attrib HW14     label="BCG scar on arm or should - NA";
  attrib HW15     label="Height: lying or standing - NA";
  attrib HW16     label="Day of birth of child - NA";
  attrib HW17     label="Date measured (day) - NA";
  attrib HW18     label="Date measured (month) - NA";
  attrib HW19     label="Date measured (year) - NA";
  attrib HW20     label="Arm circumference (cms - - NA";
  attrib HW21     label="Arm circ/A Percentile - NA";
  attrib HW22     label="Arm circ/A Standard devia - NA";
  attrib HW23     label="Arm c/A Percent of ref. m - NA";
  attrib HW24     label="Arm circ/Ht Percentile - NA";
  attrib HW25     label="Arm cir/Ht Standard devia - NA";
  attrib HW26     label="Arm cir/Ht Percent ref. m - NA";
  attrib SDFORMAT format=F00289_. label="Date format of questionnaire";
  attrib SADJUST  format=F00290_. label="Month of interview adjusted 1m";
  attrib SINTM    label="Month of interview(unadjusted)";
  attrib SINTY    label="Year of interview(unadjusted)";
  attrib SDIV     format=F00291_. label="Division";
  attrib SDISTR   label="District of residence";
  attrib S114A    format=F00292_. label="Grameen Bank";
  attrib S114B    format=F00293_. label="BRAC";
  attrib S114C    format=F00294_. label="BRDP";
  attrib S114D    format=F00295_. label="Mothers club";
  attrib S114E    format=F00296_. label="Other";
  attrib S119     format=F00297_. label="Time to get to water (mins)";
  attrib S119A    label="Time to wait to get water:mins";
  attrib S123     format=F00298_. label="Toilet facility : children";
  attrib S124A    format=F00299_. label="Almirah";
  attrib S124B    format=F00300_. label="Table/chair/bench";
  attrib S124C    format=F00301_. label="Watch/clock";
  attrib S124D    format=F00302_. label="Cot/bed";
  attrib S124I    format=F00303_. label="Agricultural land";
  attrib S229M    format=F00304_. label="Month pregnancy ended";
  attrib S229Y    format=F00305_. label="Year pregnancy ended";
  attrib S233A    format=F00306_. label="Ever had unwanted pregnancy";
  attrib S233B    format=F00307_. label="Pregancy aborted/menst.reg'ltn";
  attrib S233C    format=F00308_. label="Pregnancy ended by MR/D&C";
  attrib S233DM   label="When pr. ended : Month";
  attrib S233DY   format=F00309_. label="When pregnancy ended :Year";
infile 'C:\COUNTRIES\BD31\BDBR31FL.DAT' LRECL=1018 MISSOVER ;
    input
    @1    CASEID   $15.
    @16   BIDX     2.0
    @18   V000     $3.
    @21   V001     8.0
    @29   V002     4.0
    @33   V003     3.0
    @36   V004     4.0
    @40   V005     8.0
    @48   V006     2.0
    @50   V007     2.0
    @52   V008     4.0
    @56   V009     2.0
    @58   V010     2.0
    @60   V011     4.0
    @64   V012     2.0
    @66   V013     1.0
    @67   V014     1.0
    @68   V015     1.0
    @69   V016     2.0
    @71   V017     4.0
    @75   V018     2.0
    @77   V019     2.0
    @79   V020     1.0
    @80   V021     4.0
    @84   V022     4.0
    @88   V023     2.0
    @90   V024     2.0
    @92   V025     1.0
    @93   V026     1.0
    @94   V027     1.0
    @95   V028     3.0
    @98   V029     2.0
    @100  V030     2.0
    @102  V031     2.0
    @104  V032     2.0
    @106  V033     8.0
    @114  V034     2.0
    @116  V101     2.0
    @118  V102     1.0
    @119  V103     1.0
    @120  V104     2.0
    @122  V105     1.0
    @123  V106     1.0
    @124  V107     2.0
    @126  V108     1.0
    @127  V109     1.0
    @128  V110     1.0
    @129  V111     1.0
    @130  V112     1.0
    @131  V113     2.0
    @133  V114     2.0
    @135  V115     3.0
    @138  V116     2.0
    @140  V119     1.0
    @141  V120     1.0
    @142  V121     1.0
    @143  V122     1.0
    @144  V123     1.0
    @145  V124     1.0
    @146  V125     1.0
    @147  V127     2.0
    @149  V128     2.0
    @151  V129     2.0
    @153  V130     1.0
    @154  V131     2.0
    @156  V133     2.0
    @158  V134     1.0
    @159  V135     1.0
    @160  V136     2.0
    @162  V137     2.0
    @164  V138     2.0
    @166  V139     2.0
    @168  V140     1.0
    @169  V141     1.0
    @170  V142     1.0
    @171  V144     1.0
    @172  V148     1.0
    @173  V149     1.0
    @174  V150     2.0
    @176  V151     1.0
    @177  V152     2.0
    @179  V153     1.0
    @180  V154     2.0
    @182  AWFACTT  5.0
    @187  AWFACTU  5.0
    @192  AWFACTR  5.0
    @197  AWFACTE  5.0
    @202  V201     2.0
    @204  V202     2.0
    @206  V203     2.0
    @208  V204     2.0
    @210  V205     2.0
    @212  V206     2.0
    @214  V207     2.0
    @216  V208     1.0
    @217  V209     1.0
    @218  V210     1.0
    @219  V211     4.0
    @223  V212     2.0
    @225  V213     1.0
    @226  V214     2.0
    @228  V215     3.0
    @231  V216     1.0
    @232  V217     1.0
    @233  V218     2.0
    @235  V219     2.0
    @237  V220     1.0
    @238  V221     3.0
    @241  V222     3.0
    @244  V223     1.0
    @245  V224     2.0
    @247  V225     1.0
    @248  V226     3.0
    @251  V227     1.0
    @252  V228     1.0
    @253  V229     2.0
    @255  V230     2.0
    @257  V231     4.0
    @261  V232     1.0
    @262  V233     2.0
    @264  V234     1.0
    @265  V235     2.0
    @267  V236     1.0
    @268  V237     1.0
    @269  V238     1.0
    @270  V310     2.0
    @272  V311     1.0
    @273  V312     2.0
    @275  V313     1.0
    @276  V314     1.0
    @277  V315     2.0
    @279  V316     2.0
    @281  V317     4.0
    @285  V318     1.0
    @286  V319     1.0
    @287  V320     1.0
    @288  V321     1.0
    @289  V322     1.0
    @290  V323     2.0
    @292  V325     6.0
    @298  V326     2.0
    @300  V327     1.0
    @301  V328     2.0
    @303  V337     3.0
    @306  V359     2.0
    @308  V360     2.0
    @310  V361     1.0
    @311  V362     1.0
    @312  V363     2.0
    @314  V364     1.0
    @315  V366A    1.0
    @316  V366B    1.0
    @317  V367     1.0
    @318  V369     2.0
    @320  V369B    1.0
    @321  V372     1.0
    @322  V375A    2.0
    @324  V376     2.0
    @326  V376A    1.0
    @327  V379     2.0
    @329  V380     1.0
    @330  V384A    1.0
    @331  V384B    1.0
    @332  V384C    1.0
    @333  V384D    1.0
    @334  V384E    1.0
    @335  V393     1.0
    @336  V394     1.0
    @337  V395     1.0
    @338  V396     1.0
    @339  V397     1.0
    @340  V398     1.0
    @341  V399     1.0
    @342  V401     1.0
    @343  V404     1.0
    @344  V405     1.0
    @345  V406     1.0
    @346  V407     2.0
    @348  V408     2.0
    @350  V409     1.0
    @351  V409A    1.0
    @352  V410     1.0
    @353  V410A    1.0
    @354  V411     1.0
    @355  V411A    1.0
    @356  V412     1.0
    @357  V413A    1.0
    @358  V413B    1.0
    @359  V413C    1.0
    @360  V413D    1.0
    @361  V413     1.0
    @362  V414A    1.0
    @363  V414B    1.0
    @364  V414C    1.0
    @365  V414D    1.0
    @366  V414E    1.0
    @367  V414F    1.0
    @368  V414G    1.0
    @369  V414H    1.0
    @370  V415     1.0
    @371  V416     1.0
    @372  V417     1.0
    @373  V418     1.0
    @374  V419     1.0
    @375  V420     2.0
    @377  V421     2.0
    @379  V426     3.0
    @382  V436     3.0
    @385  V437     4.0
    @389  V438     4.0
    @393  V439     4.0
    @397  V440     4.0
    @401  V441     5.0
    @406  V442     5.0
    @411  V443     5.0
    @416  V444     5.0
    @421  V444A    4.0
    @425  V445     4.0
    @429  V446     4.0
    @433  V447     1.0
    @434  V448     1.0
    @435  V449     1.0
    @436  V450A    1.0
    @437  V450B    1.0
    @438  V450C    1.0
    @439  V450D    1.0
    @440  V450E    1.0
    @441  V450F    1.0
    @442  V450G    1.0
    @443  V450H    1.0
    @444  V450I    1.0
    @445  V450J    1.0
    @446  V450K    1.0
    @447  V450L    1.0
    @448  V450M    1.0
    @449  V450X    1.0
    @450  V450Z    1.0
    @451  V451A    1.0
    @452  V451B    1.0
    @453  V451C    1.0
    @454  V451D    1.0
    @455  V451E    1.0
    @456  V451F    1.0
    @457  V451G    1.0
    @458  V451H    1.0
    @459  V451I    1.0
    @460  V451J    1.0
    @461  V451K    1.0
    @462  V451X    1.0
    @463  V451Z    1.0
    @464  V501     1.0
    @465  V502     1.0
    @466  V503     1.0
    @467  V504     1.0
    @468  V505     2.0
    @470  V506     2.0
    @472  V507     2.0
    @474  V508     2.0
    @476  V509     4.0
    @480  V510     1.0
    @481  V511     2.0
    @483  V512     2.0
    @485  V513     1.0
    @486  V525     2.0
    @488  V527     3.0
    @491  V528     2.0
    @493  V529     3.0
    @496  V530     1.0
    @497  V531     2.0
    @499  V532     1.0
    @500  V534     1.0
    @501  V535     1.0
    @502  V602     1.0
    @503  V603     3.0
    @506  V604     1.0
    @507  V605     1.0
    @508  V606     1.0
    @509  V610     1.0
    @510  V611     1.0
    @511  V612     1.0
    @512  V613     2.0
    @514  V614     1.0
    @515  V616     3.0
    @518  V618     1.0
    @519  V621     1.0
    @520  V623     1.0
    @521  V624     2.0
    @523  V625     1.0
    @524  V626     2.0
    @526  V627     2.0
    @528  V628     2.0
    @530  V629     2.0
    @532  V630A    1.0
    @533  V630B    1.0
    @534  V630C    1.0
    @535  V630D    1.0
    @536  V630E    1.0
    @537  V630F    1.0
    @538  V630G    1.0
    @539  V630H    1.0
    @540  V630I    1.0
    @541  V630J    1.0
    @542  V630K    1.0
    @543  V630L    1.0
    @544  V630X    1.0
    @545  V701     1.0
    @546  V702     2.0
    @548  V704     3.0
    @551  V705     2.0
    @553  V707     1.0
    @554  V714     1.0
    @555  V715     2.0
    @557  V716     3.0
    @560  V717     2.0
    @562  V718     1.0
    @563  V719     1.0
    @564  V720     1.0
    @565  V721     1.0
    @566  V723     2.0
    @568  V729     1.0
    @569  V730     2.0
    @571  V731     1.0
    @572  V732     1.0
    @573  V733     2.0
    @575  V734     1.0
    @576  V735     3.0
    @579  V736     8.0
    @587  V737     7.0
    @594  V738     7.0
    @601  V739     1.0
    @602  V740     1.0
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
Run;
