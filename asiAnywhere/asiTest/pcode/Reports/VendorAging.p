/*------------------------------------------------------------------------
    File        : VendorAging.p
    Purpose     :  Print Order

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttVendorAgingReport NO-UNDO
FIELD vendoraging AS CHAR
FIELD a AS CHAR.
DEFINE DATASET dsVendorAgingReport FOR ttVendorAgingReport .

    DEFINE INPUT PARAMETER prmUser           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCompany      AS CHARACTER  NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginVendor      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndVendor        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeginCurr       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCurr        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginType      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndType        AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmAsof           AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmInvPost        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmDay1           AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmDay2           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDay3           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDay4           AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmSort           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDetail         AS CHARACTER NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorAgingReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser            = ?  THEN ASSIGN prmUser          = "".
    IF prmAction          = ?  THEN ASSIGN prmAction        = "".
    IF prmOut             = ?  THEN ASSIGN prmOut           = "".
    IF prmBeginCompany    = ?  THEN ASSIGN prmBeginCompany   = "".
    IF prmEndCompany      = ?  THEN ASSIGN prmEndCompany     = "".
    IF prmBeginVendor     = ?  THEN ASSIGN prmBeginVendor    = "".
    IF prmEndVendor       = ?  THEN ASSIGN prmEndVendor      = "".
    IF prmBeginCurr       = ?  THEN ASSIGN prmBeginCurr      = "".
    IF prmEndCurr         = ?  THEN ASSIGN prmEndCurr        = "".
    IF prmBeginType       = ?  THEN ASSIGN prmBeginType      = "".
    IF prmEndType         = ?  THEN ASSIGN prmEndType        = "".
    IF prmAsof            = ?  THEN ASSIGN prmAsof           = "".
    IF prmInvPost         = ?  THEN ASSIGN prmInvPost        = "".
    IF prmDay1            = ?  THEN ASSIGN prmDay1           = "".  
    IF prmDay2            = ?  THEN ASSIGN prmDay2           = "". 
    IF prmDay3            = ?  THEN ASSIGN prmDay3           = "".   
    IF prmDay4            = ?  THEN ASSIGN prmDay4            = "".
    IF prmSort            = ?  THEN ASSIGN prmSort           = "".  
    IF prmDetail          = ?  THEN ASSIGN prmDetail         = "".  
                

    

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
DEFINE VARIABLE  as_of_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
 DEFINE VARIABLE begin_comp AS CHARACTER FORMAT "X(3)"  NO-UNDO.
 DEFINE VARIABLE begin_curr AS CHARACTER FORMAT "X(3)" NO-UNDO.
 DEFINE VARIABLE begin_type AS CHARACTER FORMAT "X(8)"         NO-UNDO.
 DEFINE VARIABLE begin_vend AS CHARACTER FORMAT "X(8)"         NO-UNDO.
 DEFINE VARIABLE end_comp AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" NO-UNDO.
 DEFINE VARIABLE end_curr AS CHARACTER FORMAT "X(3)":U INITIAL "zzz" NO-UNDO.
 DEFINE VARIABLE end_type AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
 DEFINE VARIABLE end_vend AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
 DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-vaging.csv" NO-UNDO.
 DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99      NO-UNDO.
 DEFINE VARIABLE period-days-1 AS INTEGER FORMAT ">,>>9":U INITIAL 9999  NO-UNDO.
 DEFINE VARIABLE period-days-2 AS INTEGER FORMAT ">,>>9":U INITIAL 9999  NO-UNDO.
 DEFINE VARIABLE period-days-3 AS INTEGER FORMAT ">,>>9":U INITIAL 9999  NO-UNDO.
 DEFINE VARIABLE period-days-4 AS INTEGER FORMAT ">,>>9":U INITIAL 9999  NO-UNDO.
 DEFINE VARIABLE rd_date AS CHARACTER INITIAL "Invoice" NO-UNDO.
 DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Code"    NO-UNDO.
 DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no      NO-UNDO.
 DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO         NO-UNDO.
 DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no      NO-UNDO.
 DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no     NO-UNDO.
 DEF VAR v-terms AS CHAR NO-UNDO.

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

DEF TEMP-TABLE tt-vend NO-UNDO
    FIELD curr-code LIKE vend.curr-code
    FIELD sorter    LIKE vend.vend-no
    FIELD row-id    AS   ROWID
    INDEX tt-vend curr-code sorter.


DEF TEMP-TABLE w-sort NO-UNDO field w-int as int.

 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .


 

IF prmAction = "Vendor" THEN DO:
   
    ASSIGN
      as_of_date =       DATE(prmAsof)
      begin_comp =      prmBeginCompany   
      begin_curr =      prmBeginCurr  
      begin_type =        prmBeginType 
      begin_vend =      prmBeginVendor   
      end_comp   =     prmEndCompany      
      end_curr   =     prmEndCurr  
      end_type   =    prmEndType     
      end_vend   =   prmEndVendor 
      period-days-1  =    int(prmDay1)
      period-days-2  =    int(prmDay2)        
      period-days-3  =    int(prmDay3)       
      period-days-4  =    int(prmDay4)         
      rd_date        =    prmInvPost        
      rd_sort        =    prmSort               
      tb_detailed    =  IF  prmDetail = "True" THEN TRUE ELSE FALSE.
      






    
        assign
        init-dir    = v-webrootpath
        lv-txt-file =  'VendorAging' + STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) +  ".txt" .
       
        
        v-excel-file =  'VendorAging' +
             STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".
        ASSIGN
            fi_file = init-dir + "\" + v-excel-file .
    
        run run-report.
        
        CREATE ttVendorAgingReport.

        IF prmOut = "Yes" THEN
        ASSIGN ttVendorAgingReport.vendoraging = v-excel-file.
        ELSE
            ASSIGN ttVendorAgingReport.vendoraging = lv-txt-file .

END. /* IF prmAction = "Trans" THEN DO: */

    
    
PROCEDURE run-report :
/* ---------------------------------------------------- ap/ap-aged.p  4/94 RM */
/* Vendor Aging Report Program - A/P Module                                   */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def var v-date like ap-inv.inv-date NO-UNDO.
def var d as int label "Days" NO-UNDO.
def var ni as INT NO-UNDO.
def var cust-t as dec format "->>>,>>>,>>9.99" extent 5 NO-UNDO.
def var curr-t as dec format "$->>>,>>>,>>9.99" extent 6 NO-UNDO.
def var grand-t as dec format "$->>>,>>>,>>9.99" extent 6 NO-UNDO.
def var s as INT NO-UNDO.
def var v-amt as dec format "->,>>>,>>>,>>9.99" NO-UNDO.
def var ag like v-amt format "->>>,>>>,>>9.99" extent 5 NO-UNDO.
def var t1 as dec format "$->>,>>>,>>9.99" NO-UNDO.
def var t2 as dec format "$->>,>>>,>>9.99" NO-UNDO.
def var t3 as dec format "$->>,>>>,>>9.99" NO-UNDO.
def var m1 as char format "x(20)" NO-UNDO.
def var m2 as char format "x(15)" NO-UNDO.
def var m3 as char format "(999) 999-9999" NO-UNDO .
def var first-time as log init YES NO-UNDO.
def var time_stamp as CHAR NO-UNDO.
def var op as char format "x" init "D" NO-UNDO.
DEF VAR ll-mult-curr AS LOG NO-UNDO.
DEF VAR lv-page-break AS CHAR NO-UNDO.
DEF VAR lv-f-bot-hdr AS CHAR FORMAT "x(12)" NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.

def buffer xap-ledger for ap-ledger.

FORM HEADER SKIP(1)
     lv-page-break FORMAT "x(200)"
WITH PAGE-TOP FRAME r-top-1 STREAM-IO WIDTH 200 NO-BOX.

FORM HEADER SKIP(1)
     "VENDOR#  NAME  " SKIP
     "PHONE-   TYPE" skip
     "  INVOICE#      DATE                  AMOUNT #DAYS             0-" + string(period-days-1) + FILL(" ",11) +   
                                                                           string(period-days-1 + 1) + "-" + string(period-days-2) + FILL(" ",11) +
                                                                           string(period-days-2 + 1) + "-" + string(period-days-3) + FILL(" ",11) +
                                                                           string(period-days-3 + 1) + "-" + string(period-days-4) + FILL(" ",9) +
                                                                           string(period-days-4 + 1) + "+"  + FILL(" ",10) FORM "x(131)"
     SKIP FILL("_",131) FORMAT "x(131)"
WITH PAGE-TOP FRAME r-top-2 STREAM-IO WIDTH 200 NO-BOX.

FORM HEADER
     SPACE(10)
     lv-f-bot-hdr
     "          0 -"  string(period-days-1) + FILL(" ",10) +   
                      string(period-days-1 + 1) + " - " + string(period-days-2) + FILL(" ",10) +
                      string(period-days-2 + 1) + " - " + string(period-days-3) + FILL(" ",10) +
                      string(period-days-3 + 1) + " - " + string(period-days-4) + FILL(" ",10) +
                      string(period-days-4 + 1) + "+"  + FILL(" ",10) +
     "Total Payables" FORM "x(131)"
     SKIP SPACE(10) FILL("_",131) FORMAT "x(120)"
WITH FRAME f-bot DOWN STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-UNDERLINE.




assign
 str-tit2 = "Vendor Aging"
 {sys/inc/ctrtext.i str-tit2 112}
 
 v-s-vend  = begin_vend
 v-e-vend  = end_vend
 v-s-type  = begin_type
 v-e-type  = end_type
 v-idate   = rd_date eq "Invoice"
 v-sort    = rd_sort eq "Name"
 v-dtl     = tb_detailed
 v-date    = as_of_date
 v-days[1] = period-days-1
 v-days[2] = period-days-2
 v-days[3] = period-days-3
 v-days[4] = period-days-4

 str-tit3 = "Company From: " + STRING(begin_comp) + " To: " + STRING(end_comp) + "      " + "As of Date: " + STRING(v-date)
 {sys/inc/ctrtext.i str-tit3 132}.
 

/*-do with frame {&frame-name}:
 EMPTY TEMP-TABLE w-sort.

  do li = 1 to 4:
    create w-sort.
    w-int = v-days[li].
  end.
  li = 0.
  for each w-sort by w-int:
    ASSIGN
       li = li + 1
       v-days[li] = w-int.
    if i gt 3 then leave.
  end.
  assign
   period-days-1:screen-value = string(v-days[1])
   period-days-1
   period-days-2:screen-value = string(v-days[2])
   period-days-2
   period-days-3:screen-value = string(v-days[3])
   period-days-3
   period-days-4:screen-value = string(v-days[4])
   period-days-4.
end.*/

/*{sys/inc/print1.i}*/


if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + lv-txt-file
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

if td-show-parm then run show-param.

VIEW FRAME r-top.

  ASSIGN grand-t = 0.

  EMPTY TEMP-TABLE tt-vend.

  FOR EACH company WHERE
       company.company GE begin_comp AND
       company.company LE end_comp
       NO-LOCK,

    EACH vend NO-LOCK
      WHERE vend.company EQ company.company
        AND vend.vend-no GE v-s-vend
        AND vend.vend-no LE v-e-vend
        AND vend.TYPE    GE v-s-type
        AND vend.TYPE    LE v-e-type
        AND ((vend.curr-code GE begin_curr    AND
              vend.curr-code LE end_curr)           OR
             (vend.curr-code EQ ""            AND
              company.curr-code GE begin_curr AND
              company.curr-code LE end_curr)):

    FOR EACH ap-inv
        WHERE ap-inv.company   EQ company.company
          AND ap-inv.vend-no   EQ vend.vend-no
          AND ap-inv.posted    EQ YES
          AND (ap-inv.inv-date LE as_of_date OR NOT v-idate)
        USE-INDEX ap-inv NO-LOCK,
    
        FIRST ap-ledger
        WHERE ap-ledger.company  EQ company.company
          AND ap-ledger.vend-no  EQ ap-inv.vend-no
          AND ap-ledger.ref-date EQ ap-inv.inv-date
          AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
          AND (ap-ledger.tr-date LE as_of_date OR v-idate)
        USE-INDEX ap-ledger NO-LOCK:

      CREATE tt-vend.
      ASSIGN
       tt-vend.curr-code = IF vend.curr-code EQ "" THEN company.curr-code
                                                   ELSE vend.curr-code
       tt-vend.sorter    = IF v-sort THEN vend.name ELSE vend.vend-no
       tt-vend.row-id    = ROWID(vend).

      IF tt-vend.curr-code NE company.curr-code THEN ll-mult-curr = YES.

      LEAVE.
    END.
  END.

  IF tb_excel THEN DO:
     OUTPUT STREAM excel TO VALUE(fi_file).
    
     IF ll-mult-curr THEN
        excelheader = excelheader + "CURRENCY,".

     excelheader = excelheader
                 + "VENDOR#,VENDOR NAME,PHONE,TYPE,INVOICE#,DATE,AMOUNT,#DAYS,"
                 + "0-" + STRING(period-days-1) + "," + STRING(period-days-1 + 1) + "-" 
                        + STRING(period-days-2) + "," + STRING(period-days-2 + 1) + "-" 
                        + STRING(period-days-3) + "," + STRING(period-days-3 + 1) + "-" 
                        + STRING(period-days-4) + "," + STRING(period-days-4 + 1) + "+" 
                        + ",Total Payables".

     PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
  END.

  FOR EACH tt-vend,
      FIRST vend WHERE ROWID(vend) EQ tt-vend.row-id NO-LOCK
      BREAK BY tt-vend.curr-code
            BY tt-vend.sorter:

    IF FIRST-OF(tt-vend.curr-code) THEN DO:
      lv-page-break = "Currency: " + TRIM(tt-vend.curr-code).

      IF FIRST(tt-vend.curr-code) THEN DO:
        IF ll-mult-curr THEN VIEW FRAME r-top-1.
        VIEW FRAME r-top-2.
      END.

      IF ll-mult-curr OR FIRST(tt-vend.curr-code) THEN PAGE.
    END.
    
    {ap/ap-aged.i}

    IF LAST-OF(tt-vend.curr-code) THEN DO:
       IF ll-mult-curr THEN DO:
          PUT SKIP(2).
         
          ASSIGN
           lv-f-bot-hdr = " CURR TOTALS"
           curr-t[6]    = 0.
         
          DO i = 1 TO 5:
            curr-t[6] = curr-t[6] + curr-t[i].
          END.
         
          VIEW FRAME f-bot.
          DOWN.
         
          DISPLAY SPACE(10) lv-f-bot-hdr
                  curr-t[1]
                  curr-t[2]
                  curr-t[3]
                  curr-t[4]
                  curr-t[5]
                  curr-t[6]
                  SKIP
            
              WITH FRAME bot1 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200.
         
          DISPLAY "PERCENTAGE COMPOSITION" SPACE(2)
                  (curr-t[1] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[2] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[3] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[4] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
                  (curr-t[5] / t2) * 100 FORMAT "->>>>>>>>>>9.99%"
            
              WITH FRAME bot2 STREAM-IO WIDTH 200 NO-LABELS NO-BOX NO-ATTR-SPACE.
         
          IF tb_excel THEN
          DO:
            PUT STREAM excel UNFORMATTED
                SKIP(1).
         
            IF ll-mult-curr THEN
               PUT STREAM excel UNFORMATTED
                   '"' "" '",'.
         
            PUT STREAM excel UNFORMATTED
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' "CURR TOTALS"                       '",'
                '"' ""                                  '",'
                '"' STRING(curr-t[1],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[2],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[3],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[4],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[5],"$->>>,>>>,>>9.99") '",'
                '"' STRING(curr-t[6],"$->>>,>>>,>>9.99") '",'
                SKIP.
         
            IF ll-mult-curr THEN
               PUT STREAM excel UNFORMATTED
                   '"' "" '",'.
         
            PUT STREAM excel UNFORMATTED
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' ""                                  '",'
                '"' "PERCENTAGE COMPOSITION"            '",'
                '"' ""                                  '",'
                '"' STRING((curr-t[1] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[2] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[3] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[4] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                '"' STRING((curr-t[5] / t2) * 100,"->>>>>>>>>>9.99%") '",'
                SKIP(1).
          END.
       END.
      
       DO i = 1 TO 5:
         grand-t[i] = grand-t[i] + curr-t[i].
       END.
       ASSIGN
        curr-t = 0
        t3     = t3 + t2
        t2     = 0.
    END.
  END.

  IF ll-mult-curr THEN DO:
    HIDE FRAME r-top-1 NO-PAUSE.
    HIDE FRAME r-top-2 NO-PAUSE.
    PAGE.
  END.

  PUT SKIP(2).
  
  ASSIGN
   lv-f-bot-hdr = "GRAND TOTALS"
   grand-t[6]   = 0.

  DO i = 1 TO 5:
     grand-t[6] = grand-t[6] + grand-t[i].
  END.
  
  VIEW FRAME f-bot.
  DOWN.

  DISPLAY SPACE(10) lv-f-bot-hdr
          grand-t[1]
          grand-t[2]
          grand-t[3]
          grand-t[4]
          grand-t[5]
          grand-t[6]
          skip
          
      WITH FRAME bot3 NO-BOX NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH 200.

  display "PERCENTAGE COMPOSITION" space(2)
          (grand-t[1] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[2] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[3] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[4] / t3) * 100 format "->>>>>>>>>>9.99%"
          (grand-t[5] / t3) * 100 format "->>>>>>>>>>9.99%"
          
      with frame b4 stream-io width 200 no-labels no-box no-attr-space.

  IF tb_excel THEN
     DO:
       PUT STREAM excel UNFORMATTED
           SKIP(1).

       IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'.

       PUT STREAM excel UNFORMATTED
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",' 
           '"' "GRAND TOTALS"                      '",'
           '"' ""                                  '",'
           '"' STRING(grand-t[1],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[2],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[3],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[4],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[5],"$->>>,>>>,>>9.99") '",'
           '"' STRING(grand-t[6],"$->>>,>>>,>>9.99") '",'
           SKIP.

       PUT STREAM excel UNFORMATTED
           SKIP(1).

       IF ll-mult-curr THEN
          PUT STREAM excel UNFORMATTED
              '"' "" '",'.

       PUT STREAM excel UNFORMATTED
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' ""                                  '",'
           '"' "PERCENTAGE COMPOSITION"            '",'
           '"' ""                                  '",'
           '"' STRING((grand-t[1] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[2] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[3] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[4] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           '"' STRING((grand-t[5] / t3) * 100,"->>>>>>>>>>9.99%") '",'
           SKIP.
     END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
  
END.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
