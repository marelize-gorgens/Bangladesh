DATA LIST FILE='bdwi31fl.DAT' RECORDS = 1
 /
  WHHID       1-12   (A)
  WLTHINDF   13-23   (5)
  WLTHIND5   24-28  
 .
VARIABLE LABELS
  WHHID    "Household ID"
 /WLTHINDF "Wealth Index from factor scores"
 /WLTHIND5 "Quintiles of wealth index"
 .
VALUE LABELS
  WLTHIND5
      0 ""
      1 "Lowest quintile"
      2 "Second quintile"
      3 "Middle quintile"
      4 "Fourth quintile"
      5 "Highest quintile"
 .
 EXECUTE.
execute.
save outfile='bdwi31fl.SAV'.
