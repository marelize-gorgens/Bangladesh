libname user ' ';
Proc format;
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
Data BDMR31FL;
infile 'BDMR31FL.DAT' LRECL=530 MISSOVER ;
    input
    @1    MCASEID  $15.
    @16   MV000    $3.
    @19   MV001    8.0
    @27   MV002    4.0
    @31   MV003    3.0
    @34   MV004    4.0
    @38   MV005    8.0
    @46   MV006    2.0
    @48   MV007    2.0
    @50   MV008    4.0
    @54   MV009    2.0
    @56   MV010    2.0
    @58   MV011    4.0
    @62   MV012    2.0
    @64   MV013    2.0
    @66   MV014    1.0
    @67   MV015    1.0
    @68   MV016    2.0
    @70   MV021    4.0
    @74   MV022    4.0
    @78   MV023    2.0
    @80   MV024    2.0
    @82   MV025    1.0
    @83   MV026    1.0
    @84   MV027    1.0
    @85   MV028    3.0
    @88   MV029    2.0
    @90   MV030    2.0
    @92   MV031    2.0
    @94   MV032    2.0
    @96   MV033    8.0
    @104  MV034_1  2.0
    @106  MV034_2  2.0
    @108  MV034_3  2.0
    @110  MV034_4  2.0
    @112  MV034_5  2.0
    @114  MV034_6  2.0
    @116  MV034_7  2.0
    @118  MV034_8  2.0
    @120  MV035    1.0
    @121  MV801    4.0
    @125  MV802    4.0
    @129  MV803    2.0
    @131  MV101    2.0
    @133  MV102    1.0
    @134  MV103    1.0
    @135  MV104    2.0
    @137  MV105    1.0
    @138  MV106    1.0
    @139  MV107    2.0
    @141  MV108    1.0
    @142  MV109    1.0
    @143  MV110    1.0
    @144  MV111    1.0
    @145  MV112    1.0
    @146  MV130    1.0
    @147  MV131    2.0
    @149  MV133    2.0
    @151  MV134    1.0
    @152  MV135    1.0
    @153  MV136    2.0
    @155  MV138    2.0
    @157  MV148    1.0
    @158  MV149    1.0
    @159  MV150    2.0
    @161  MV151    1.0
    @162  MV152    2.0
    @164  MV201    2.0
    @166  MV202    2.0
    @168  MV203    2.0
    @170  MV204    2.0
    @172  MV205    2.0
    @174  MV206    2.0
    @176  MV207    2.0
    @178  MV213    1.0
    @179  MV218    2.0
    @181  MV225    1.0
    @182  MV301    1.0
    @183  MV302    1.0
    @184  MV304A01 1.0
    @185  MV304_01 1.0
    @186  MV305_01 1.0
    @187  MV304A02 1.0
    @188  MV304_02 1.0
    @189  MV305_02 1.0
    @190  MV304A03 1.0
    @191  MV304_03 1.0
    @192  MV305_03 1.0
    @193  MV304A04 1.0
    @194  MV304_04 1.0
    @195  MV305_04 1.0
    @196  MV304A05 1.0
    @197  MV304_05 1.0
    @198  MV305_05 1.0
    @199  MV304A06 1.0
    @200  MV304_06 1.0
    @201  MV305_06 1.0
    @202  MV304A07 1.0
    @203  MV304_07 1.0
    @204  MV305_07 1.0
    @205  MV304A08 1.0
    @206  MV304_08 1.0
    @207  MV305_08 1.0
    @208  MV304A09 1.0
    @209  MV304_09 1.0
    @210  MV305_09 1.0
    @211  MV304A10 1.0
    @212  MV304_10 1.0
    @213  MV305_10 1.0
    @214  MV304A11 1.0
    @215  MV304_11 1.0
    @216  MV305_11 1.0
    @217  MV304A12 1.0
    @218  MV304_12 1.0
    @219  MV305_12 1.0
    @220  MV304A13 1.0
    @221  MV304_13 1.0
    @222  MV305_13 1.0
    @223  MV304A14 1.0
    @224  MV304_14 1.0
    @225  MV305_14 1.0
    @226  MV304A15 1.0
    @227  MV304_15 1.0
    @228  MV305_15 1.0
    @229  MV312    2.0
    @231  MV313    1.0
    @232  MV361    1.0
    @233  MV362    1.0
    @234  MV363    2.0
    @236  MV364    1.0
    @237  MV366A   1.0
    @238  MV366B   1.0
    @239  MV367    1.0
    @240  MV375A   2.0
    @242  MV376    2.0
    @244  MV384A   1.0
    @245  MV384B   1.0
    @246  MV384C   1.0
    @247  MV384D   1.0
    @248  MV384E   1.0
    @249  MV501    1.0
    @250  MV502    1.0
    @251  MV503    1.0
    @252  MV505    2.0
    @254  MV507    2.0
    @256  MV508    2.0
    @258  MV509    4.0
    @262  MV510    1.0
    @263  MV511    2.0
    @265  MV512    2.0
    @267  MV513    1.0
    @268  MV525    2.0
    @270  MV527    3.0
    @273  MV531    2.0
    @275  MV532    1.0
    @276  MV534    1.0
    @277  MV535    1.0
    @278  MV602    1.0
    @279  MV603    3.0
    @282  MV604    1.0
    @283  MV605    1.0
    @284  MV610    1.0
    @285  MV611    1.0
    @286  MV612    1.0
    @287  MV613    2.0
    @289  MV614    1.0
    @290  MV616    3.0
    @293  MV621    1.0
    @294  MV627    2.0
    @296  MV628    2.0
    @298  MV629    2.0
    @300  MV630A   1.0
    @301  MV630B   1.0
    @302  MV630C   1.0
    @303  MV630D   1.0
    @304  MV630E   1.0
    @305  MV630F   1.0
    @306  MV630G   1.0
    @307  MV630H   1.0
    @308  MV630I   1.0
    @309  MV630J   1.0
    @310  MV630K   1.0
    @311  MV630L   1.0
    @312  MV630X   1.0
    @313  MV714    1.0
    @314  MV716    3.0
    @317  MV717    2.0
    @319  MV719    1.0
    @320  MV731    1.0
    @321  MV732    1.0
    @322  MV733    2.0
    @324  MV736    8.0
    @332  MV740    1.0
    @333  MV751    1.0
    @334  MV752A   1.0
    @335  MV752B   1.0
    @336  MV752C   1.0
    @337  MV752D   1.0
    @338  MV752E   1.0
    @339  MV752F   1.0
    @340  MV752G   1.0
    @341  MV752H   1.0
    @342  MV752I   1.0
    @343  MV752J   1.0
    @344  MV752K   1.0
    @345  MV752L   1.0
    @346  MV752M   1.0
    @347  MV752N   1.0
    @348  MV752O   1.0
    @349  MV752X   1.0
    @350  MV753    1.0
    @351  MV754A   1.0
    @352  MV754B   1.0
    @353  MV754C   1.0
    @354  MV754D   1.0
    @355  MV754E   1.0
    @356  MV754F   1.0
    @357  MV754G   1.0
    @358  MV754H   1.0
    @359  MV754I   1.0
    @360  MV754J   1.0
    @361  MV754K   1.0
    @362  MV754L   1.0
    @363  MV754M   1.0
    @364  MV754N   1.0
    @365  MV754O   1.0
    @366  MV754X   1.0
    @367  MV754Z   1.0
    @368  MV755B   1.0
    @369  MV755C   1.0
    @370  MV755D   1.0
    @371  MV755E   1.0
    @372  MV755F   1.0
    @373  MV755L   1.0
    @374  MV755M   1.0
    @375  MV755N   1.0
    @376  MV755O   1.0
    @377  MV755X   1.0
    @378  MV755Z   1.0
    @379  MV756    1.0
    @380  MV757    1.0
    @381  MV758    1.0
    @382  MV760A   1.0
    @383  MV760B   1.0
    @384  MV760C   1.0
    @385  MV760D   1.0
    @386  MV760E   1.0
    @387  MV760F   1.0
    @388  MV760G   1.0
    @389  MV760I   1.0
    @390  MV760L   1.0
    @391  MV760M   1.0
    @392  MV760N   1.0
    @393  MV760O   1.0
    @394  MV760P   1.0
    @395  MV760V   1.0
    @396  MV760W   1.0
    @397  MV760X   1.0
    @398  MV760Y   1.0
    @399  MV760Z   1.0
    @400  MV761    1.0
    @401  MV762    2.0
    @403  MV763    2.0
    @405  MV764    1.0
    @406  MV765    1.0
    @407  MSDISTR  2.0
    @409  MSFORMAT 1.0
    @410  MSADJUST 1.0
    @411  MSINTY   2.0
    @413  MSINTM   2.0
    @415  MS304_01 1.0
    @416  MS304_02 1.0
    @417  MS304_03 1.0
    @418  MS304_04 1.0
    @419  MS304_05 1.0
    @420  MS304_06 1.0
    @421  MS304_07 1.0
    @422  MS304_08 1.0
    @423  MS304_09 1.0
    @424  MS304_10 1.0
    @425  MS304_11 1.0
    @426  MS304_12 1.0
    @427  MS304_13 1.0
    @428  MS304_14 1.0
    @429  MS304_15 1.0
    @430  MS304_16 1.0
    @431  MS304_17 1.0
    @432  MS304_18 1.0
    @433  MS304_19 1.0
    @434  MS304_20 1.0
    @435  MS309    2.0
    @437  MS311    2.0
    @439  MS316    1.0
    @440  MS317    2.0
    @442  MS320S   1.0
    @443  MS320    2.0
    @445  MS320A   1.0
    @446  MS320C   4.0
    @450  MS320E   2.0
    @452  MS320D   1.0
    @453  MS321M   2.0
    @455  MS321Y   2.0
    @457  MS321C   4.0
    @461  MS321F   1.0
    @462  MS321XA  1.0
    @463  MS321XB  1.0
    @464  MS321XC  1.0
    @465  MS321XD  1.0
    @466  MS321XE  2.0
    @468  MS321XF  4.0
    @472  MS322B   1.0
    @473  MS322C   2.0
    @475  MS323    2.0
    @477  MS323B   1.0
    @478  MS325    1.0
    @479  MS325A   1.0
    @480  MS325B   1.0
    @481  MS325C   1.0
    @482  MS325H   1.0
    @483  MS325I   2.0
    @485  MS325J   1.0
    @486  MS326    2.0
    @488  MS328HI  2.0
    @490  MS347A   1.0
    @491  MS347B   1.0
    @492  MS347CA  1.0
    @493  MS347CB  1.0
    @494  MS347CC  1.0
    @495  MS347CD  1.0
    @496  MS347CE  1.0
    @497  MS348C   1.0
    @498  MS349A   1.0
    @499  MS349B   1.0
    @500  MS349C   1.0
    @501  MS349D   1.0
    @502  MS349E   1.0
    @503  MS350    1.0
    @504  MS352    2.0
    @506  MS353    1.0
    @507  MS354    1.0
    @508  MS355    1.0
    @509  MS356    2.0
    @511  MS357A   1.0
    @512  MS357B   1.0
    @513  MS357C   1.0
    @514  MS357D   1.0
    @515  MS357E   1.0
    @516  MS357F   1.0
    @517  MS357G   1.0
    @518  MS358    1.0
    @519  MS359    1.0
    @520  MS360    1.0
    @521  MS361    1.0
    @522  MS408A   1.0
    @523  MS408B   1.0
    @524  MS408C   1.0
    @525  MS408D   1.0
    @526  MS508    2.0
    @528  MS512    1.0
    @529  MS514    1.0
    ;
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
