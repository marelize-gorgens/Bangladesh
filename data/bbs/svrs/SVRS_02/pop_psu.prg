clea
SET TALK OFF
set stat on
*SET SCOREBOARD OFF
SET DATE BRIT
R=4
MPAGE=1
USE c:\svrs_06\tafsl_2p.DBF
INDEX ON PSU_NO+hh_no TO PSU_hh.NDX
GO TOP
CLEAR
******
SET DEVICE TO file pop_psu.txt
DO HEAD
MTOTPOP=0
MHH=0
MHHno=0
mpsu=0
mpop=0
mpsu=val(psu_no)
mhh=val(hh_no)
DO WHILE .NOT. EOF()
If mpsu#val(psu_no)    
 do print
MPOP=1
MHHno=1
endif
*******
If VAL(psu_no)=mpsu 
 mpop=mpop+1
endif
if VAL(psu_no)=mpsu and VAL(hh_no)#mhh
  mhhno=mhhno+1
endif
***
MPSU=VAL(PSU_NO)
MHH=VAL(HH_NO)
mtotpop=mtotpop+1
skip
*********
if eof()
  *DO L_PRINT
  r=r+1
  @ r,35 say "TOT POPULATION"
  @ r,50 say mtotpop pict '999999999'
  endif
**********
ENDDO
CLOSE DATABASE
SET DEVICE TO SCREEN
SET PRINTER OFF
set TALK ON
RETURN
**********
proced print
 IF R>56
  DO HEAD
  R=4
 ENDIF
SKIP -1
@ r,6 say  psu_no pict '9999'
@ r,13 say zIla pict "99"
@ r,19 say UPZA pict '99'
@ r,26 say union pict '99'
@ r,34 say rmo pict '9'
@ r,40 say mhhno pict '999'
@ r,53 say mpop pict '9999'
 skip 1
 R=R+1
 return
*******
proced L_print
SKIP -1
@ r,6 say  psu_no pict '9999'
@ r,13 say zIla pict "99"
@ r,19 say UPZA pict '99'
@ r,26 say union pict '99'
@ r,34 say rmo pict '9'
@ r,40 say mhhno pict '999'
@ r,53 say mpop pict '9999'
return
*********
PROCE HEAD
	@ 1,5 say "Distribution of Population by PSUs,2002"
	@ 2,5  SAY "PSU_NO ZILA  THANA  UNION   RMO   TOT_HH   TOTAL POP" 
	@ 3,5  SAY "-----  ----  -----  -----   ---   ------    ---------"  
RETURN



