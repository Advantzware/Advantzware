/*------------------------------------------------------------------------
    File        : ven1099.p
    Purpose     :  Vendor 1099 Report

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendor1099Report NO-UNDO
FIELD ven1099 AS CHAR
FIELD a AS CHAR.
DEFINE DATASET dsVendor1099Report FOR ttVendor1099Report .

    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginVendor    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndVendor      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegdt          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEnddt          AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmrddate         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmincld          AS CHARACTER NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendor1099Report.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser           = ?  THEN ASSIGN prmUser           = "".
    IF prmAction         = ?  THEN ASSIGN prmAction         = "".
    IF prmOut            = ?  THEN ASSIGN prmOut            = "".
    IF prmBeginVendor    = ?  THEN ASSIGN prmBeginVendor    = "".
    IF prmEndVendor      = ?  THEN ASSIGN prmEndVendor      = "".
    IF prmBegdt          = ?  THEN ASSIGN prmBegdt          = "".
    IF prmEnddt          = ?  THEN ASSIGN prmEnddt          = "".
    IF prmrddate         = ?  THEN ASSIGN prmrddate         = "".
    IF prmincld          = ?  THEN ASSIGN prmincld          = "".
   
                

    

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-txt-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEFINE VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR v-VERSION AS CHAR NO-UNDO. 
{custom/xprint.i}
{custom/vxprint.i}
{sys/inc/var.i new shared}

DEF STREAM excel.

DEF VAR prmComp AS CHAR NO-UNDO.
 DEFINE VAR custcount AS CHAR NO-UNDO.
 DEF VAR  tmp-path AS CHAR NO-UNDO. 

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEFINE VARIABLE begin_vend      AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_vend        AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE begin_date      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 NO-UNDO.
DEFINE VARIABLE end_date        AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE rs-date         AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_zero-ven     AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page  AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO         NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no      NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vaging.csv" NO-UNDO.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

assign
 cocode = prmComp
 locode = usercomp.loc
 tb_excel   = IF prmOut = "Yes" THEN TRUE ELSE FALSE
 v-today = TODAY   . 


DEF VAR lv-default-comp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO INIT 0.

FOR EACH usercomp WHERE usercomp.USER_id = USERID("nosweat") AND  usercomp.loc = "" NO-LOCK :
    v-count = v-count + 1 .
END.
FIND FIRST usercomp WHERE usercomp.USER_id = USERID("nosweat") AND
                                  usercomp.company_default NO-LOCK NO-ERROR.
ASSIGN     
lv-default-comp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

def new shared var v-s-vend like vend.vend-no.
def new shared var v-e-vend like vend.vend-no.
def new shared var v-idate  as   log format "Invoice/Posting" init yes.
def new shared var v-sort   as   log format "Code/Name"       init yes.
def new shared var v-dtl    as   log format "Detail/Summary"  init yes.
def new shared var v-s-type AS CHAR NO-UNDO.
def new shared var v-e-type AS CHAR NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR v-days AS INTE NO-UNDO EXTENT 4.
DEF VAR v-refnum AS CHAR NO-UNDO.
DEF VAR v-check-date AS DATE NO-UNDO.

def buffer xperiod for period.

def var v-num-per like company.num-per no-undo.

 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .

   FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
   IF AVAIL company THEN v-num-per = company.num-per.
   
   ASSIGN
       begin_date = ?
       end_date   = ?.


 

IF prmAction = "ven1099" THEN DO:
   
    ASSIGN
      begin_vend   =  prmBeginVendor
      end_vend     =  prmEndVendor    
      begin_date   =  date(prmBegdt)      
      end_date     =  date(prmEnddt)      
      rs-date      =  prmrddate      
      tb_zero-ven  =  IF prmincld = "True" THEN TRUE ELSE FALSE. 

    
        assign
        init-dir    = v-webrootpath
        lv-txt-file =  'Vend1099' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'Vend1099' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .
    
        run run-report.
        
        CREATE ttVendor1099Report.

        IF prmOut = "yes" THEN
        ASSIGN ttVendor1099Report.ven1099 = v-excel-file.
        ELSE
            ASSIGN ttVendor1099Report.ven1099 = lv-txt-file .

END. /* IF prmAction = "Trans" THEN DO: */

    
    
PROCEDURE run-report :
/* ------------------------------------------ ap/rep/ap-1099.p 11/00 FWK ---- */
/* Extracting Vendor Information                                              */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}

def var save_id as recid.
def var svend like vend.vend-no.
def var evend like vend.vend-no init "zzzzzzzz".
def var sdate like ap-inv.inv-date.
def var edate like ap-inv.inv-date.
def var v-zero as log initial no no-undo.

def var v-vend-tot as dec FORMAT "->>,>>>,>>9.99" no-undo.
def var v-grand-tot as dec FORMAT "->>,>>>,>>9.99" no-undo.
DEF VAR excelheader AS CHAR NO-UNDO.

form  header skip(1)
      "Vendor" at 1 
      "Amount" to 80 skip
      fill("-",80) format "x(80)" skip
      with frame r-top.


assign
 str-tit2 = "Vendor 1099 Report"
 {sys/inc/ctrtext.i str-tit2 56}
 
 svend  = begin_vend
 evend  = end_vend
 sdate  = begin_date
 edate  = end_date
 v-zero = tb_zero-ven. 

/*{sys/inc/print1.i}*/
   if tmp-dir = "" then tmp-dir = v-webrootpath .
   assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.
       

{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Vend #,Tax ID#,Name,Address 1,Address 2,City,State,Zip,Amount".
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.

display "" with frame r-top.
 

IF rs-date EQ "Invoice" THEN DO:
   for each vend FIELDS(company vend-no code-1099 NAME tax-id add1
         add2 city state zip) WHERE
         vend.company   eq cocode AND
         vend.vend-no   ge svend  AND
         vend.vend-no   le evend  AND
         vend.code-1099 eq "y"
         NO-LOCK,
         EACH ap-inv FIELDS(company vend-no inv-date posted) WHERE
              ap-inv.company EQ cocode AND
              ap-inv.vend-no EQ vend.vend-no AND
              ap-inv.inv-date GE sdate  AND
              ap-inv.inv-date LE edate AND
              ap-inv.posted EQ YES
              NO-LOCK,
         each ap-payl FIELDS(amt-paid amt-disc) WHERE
              ap-payl.inv-no   eq ap-inv.inv-no AND
              ap-payl.vend-no  eq ap-inv.vend-no AND
              ap-payl.posted   eq YES
              NO-LOCK
         break by vend.vend-no:

       v-vend-tot = v-vend-tot + (ap-payl.amt-paid - ap-payl.amt-disc).
 
       if last-of(vend.vend-no) then 
       do:
           
          if v-vend-tot ne 0 or v-zero then
          DO:
              
            display vend.vend-no
                    v-vend-tot      to 80                  skip
                    "Tax ID#:"
                    vend.tax-id                            skip
                    vend.name                              skip
                    vend.add1       when (vend.add1 ne "") skip
                    vend.add2       when (vend.add2 ne "") skip
                    vend.city ", " vend.state "  " vend.zip skip(1)
            with frame frm-1099 no-labels no-box no-attr-space stream-io down.

            IF tb_excel THEN
               PUT STREAM excel UNFORMATTED
                  '"' vend.vend-no                          '",'
                  '"' vend.tax-id                           '",'
                  '"' vend.NAME                             '",'
                  '"' vend.add1                             '",'
                  '"' vend.add2                             '",'
                  '"' vend.city                             '",'
                  '"' vend.state                            '",'
                  '"' STRING(vend.zip,"xxxxx-xxxx")         '",'
                  '"' STRING(v-vend-tot,"->>,>>>,>>9.99")   '",'
               SKIP.
          END.
            
         assign
          v-grand-tot = v-grand-tot + v-vend-tot
          v-vend-tot  = 0.
       end.
    
       if last(vend.vend-no) then
       DO:
          put "===============" to 80 skip
              "GRAND TOTAL:"    to 60
              v-grand-tot       to 80 skip.

          IF tb_excel THEN
             PUT STREAM excel UNFORMATTED
                 SKIP(1)
                 '"' ""                                   '",'
                 '"' ""                                   '",'
                 '"' ""                                   '",'
                 '"' ""                                   '",'
                 '"' ""                                   '",'
                 '"' ""                                   '",'
                 '"' ""                                   '",'
                 '"' "GRAND TOTAL:"                       '",'
                 '"' STRING(v-grand-tot,"->>,>>>,>>9.99") '",'
              SKIP.
       END.
   end.
 END.
 ELSE IF rs-date EQ "check" THEN 
 DO: /* Check*/
     
   for each vend FIELDS(company vend-no code-1099 NAME tax-id add1
         add2 city state zip) WHERE
         vend.company   eq cocode AND
         vend.vend-no   ge svend AND
         vend.vend-no   le evend AND
         vend.code-1099 eq "y"
         no-lock,
         each ap-pay FIELDS(company vend-no check-date posted) WHERE
              ap-pay.company    eq cocode AND
              ap-pay.vend-no    eq vend.vend-no AND
              ap-pay.check-date ge sdate  AND
              ap-pay.check-date le edate  AND
              ap-pay.posted     eq yes
              no-lock,
         each ap-payl FIELDS(amt-paid amt-disc) WHERE
              ap-payl.c-no eq ap-pay.c-no AND
              ap-payl.memo eq no
              no-lock
         break by vend.vend-no:
    
       v-vend-tot = v-vend-tot + (ap-payl.amt-paid - ap-payl.amt-disc).
      
       if last-of(vend.vend-no) then 
       do:
         if v-vend-tot ne 0 or v-zero then
         DO:
           display vend.vend-no
                   v-vend-tot      to 80                  skip
                   "Tax ID#:"
                   vend.tax-id                            skip
                   vend.name                              skip
                   vend.add1       when (vend.add1 ne "") skip
                   vend.add2       when (vend.add2 ne "") skip
                   vend.city ", " vend.state "  " vend.zip skip(1)
            with frame frm-1020 no-labels no-box no-attr-space stream-io down.

           IF tb_excel THEN
              PUT STREAM excel UNFORMATTED
                  '"' vend.vend-no                          '",'
                  '"' vend.tax-id                           '",'
                  '"' vend.NAME                             '",'
                  '"' vend.add1                             '",'
                  '"' vend.add2                             '",'
                  '"' vend.city                             '",'
                  '"' vend.state                            '",'
                  '"' STRING(vend.zip,"xxxxx-xxxx")         '",'
                  '"' STRING(v-vend-tot,"->>,>>>,>>9.99")   '",'
               SKIP.
         END.
            
         assign
          v-grand-tot = v-grand-tot + v-vend-tot
          v-vend-tot  = 0.
       end.
    
       if last(vend.vend-no) then
       DO:
          put "===============" to 80 skip
              "GRAND TOTAL:"    to 60
              v-grand-tot       to 80 skip.

         IF tb_excel THEN
            PUT STREAM excel UNFORMATTED
                SKIP(1)
                '"' ""                                   '",'
                '"' ""                                   '",'
                '"' ""                                   '",'
                '"' ""                                   '",'
                '"' ""                                   '",'
                '"' ""                                   '",'
                '"' ""                                   '",'
                '"' "GRAND TOTAL:"                       '",'
                '"' STRING(v-grand-tot,"->>,>>>,>>9.99") '",'
                SKIP.
       END.
   end. /* for each */
 END. 
 IF tb_excel THEN DO:
     OUTPUT STREAM excel CLOSE.
     
  END.                   
/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

