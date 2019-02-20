libname user 'C:\COUNTRIES\WEALTHIDX';
Proc format;
  value F00001_
     1 = "Lowest quintile"
     2 = "Second quintile"
     3 = "Middle quintile"
     4 = "Fourth quintile"
     5 = "Highest quintile"
     ;
Data user.BDWI31FL;
  attrib WHHID    length=$12 label="Household ID";
  attrib WLTHINDF label="Wealth Index from factor scores";
  attrib WLTHIND5 format=F00001_. label="Quintiles of wealth index";
infile 'C:\COUNTRIES\WEALTHIDX\BDWI31FL.DAT' LRECL=28 MISSOVER ;
    input
    @1    WHHID    $12.
    @13   WLTHINDF 11.5
    @24   WLTHIND5 5.0
    ;
Run;
