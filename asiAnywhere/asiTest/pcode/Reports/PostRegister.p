/*------------------------------------------------------------------------
    File        : PostRegister.p
    Purpose     : 
    Main File   : glrep/r-postrg.w

    Syntax      :

    Description : Return a Dataset of Inventory Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE PostRegisterEntries NO-UNDO
    FIELD postregister AS CHAR.
  
DEFINE DATASET dsPostRegisterEntries FOR PostRegisterEntries.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegRun     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndRun     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate    AS CHAR NO-UNDO.
    
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPostRegisterEntries .

    IF prmUser       = ? THEN ASSIGN prmUser         = "".
    IF prmBegRun     = ? THEN ASSIGN prmBegRun       = "0".
    IF prmEndRun     = ? THEN ASSIGN prmEndRun       = "0".
    IF prmBegDate    = ? THEN ASSIGN prmBegDate      = "".
    IF prmEndDate    = ? THEN ASSIGN prmEndDate      = "".
    IF prmOut        = ? THEN ASSIGN prmOut          = "".
   
    

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE end_run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999 NO-UNDO.
    

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL "c:\Inetpub\wwwroot\pdfs\Commission.csv" */  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes     NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no   NO-UNDO.

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.

/*{custom/xprint.i}*/
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF STREAM s-temp.

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN 
    cocode = prmComp .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

IF prmAction = "PostReg" THEN DO:


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    assign
    v-today       =  TODAY
    cocode        =   prmComp
    begin_run-no  = int(prmBegRun)
    end_run-no    = int(prmEndRun)
    begin_date    = date(prmBegDate)
    end_date      = date(prmEndDate)
     .

    tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

    assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "PostRegister" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "PostRegister" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "PostRegister" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".



  RUN run-report. 

    CREATE PostRegisterEntries.
    IF tb_excel  THEN
        ASSIGN PostRegisterEntries.postregister = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN PostRegisterEntries.postregister = vTextFile .

  

END.

PROCEDURE run-report :
/* --------------------------------------------------- gl/gl-run.p 06/00 FWK  */
/* GL Transaction Run Report by Run and Account                               */
/* -------------------------------------------------------------------------- */

DEF VAR v-export     AS LOGICAL.
DEF VAR v-excel-hdr  AS CHAR.
DEF VAR v-exp-name   AS CHAR FORMAT "x(40)" INIT "c:\tmp\r-postrg.csv".
DEF VAR v-tr-num     AS CHAR. 

{sys/form/r-topw.f}

def var v-s-run like glhist.tr-num format ">>>>>>" init 0.
def var v-e-run like v-s-run init 999999.
def var v-s-dat like glhist.tr-date format "99/99/9999" init 01/01/0001.
def var v-e-dat like v-s-dat init 12/31/9999.

def var tot-all  as dec format "(>>>,>>>,>>>,>>9.99)".
def var tot-tot  as dec format "(>>>,>>>,>>>,>>9.99)".
def var v-print as log init no.
def var tmp-dscr like gltrans.tr-dscr.
def var str-tit4 as char no-undo.
def var str-tit5 as char no-undo.


ASSIGN
   str-tit2 = "GL Posting Register" 
   {sys/inc/ctrtext.i str-tit2 112}
 
   str-tit4 = "Run#        Account Number  Account Description           Journal      Reference" +
            "                             Date            Balance"
   v-s-run = begin_run-no
   v-e-run = end_run-no
   v-s-dat = begin_date
   v-e-dat = end_date
   uperiod = period 
   v-export = tb_excel
   v-exp-name = fi_file
   v-excel-hdr = "Run#,Account Number,Account Description,Journal, Reference,Date,Balance".


if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}



   
DISPLAY 
   str-tit4      FORMAT "x(132)"
   FILL("-",132) FORMAT "x(132)" skip
   WITH FRAME r-top.

IF v-export THEN DO:
   OUTPUT STREAM s-temp TO VALUE(v-exp-name).
   PUT STREAM s-temp UNFORMATTED 
      v-excel-hdr                 
   SKIP.
END. 

FOR EACH glhist WHERE 
         glhist.company EQ cocode
        AND glhist.tr-num  ge v-s-run
        AND glhist.tr-num  le v-e-run
        AND glhist.tr-date ge v-s-dat
        AND glhist.tr-date le v-e-dat 
      NO-LOCK
      BREAK BY glhist.tr-num 
            BY glhist.actnum:

   ASSIGN
      v-print = yes
      tot-all = tot-all + glhist.tr-amt.

   DISPLAY 
      glhist.tr-num WHEN FIRST-OF(glhist.tr-num) FORMAT "9999999" SPACE(5) 
      glhist.actnum SPACE(5)
      glhist.jrnl SPACE(5)
      glhist.tr-dscr
      /* tmp-dscr*/
      glhist.tr-date SPACE(6)
      glhist.tr-amt
      WITH FRAME yyy NO-LABELS STREAM-IO WIDTH 200 NO-BOX.
    
   IF FIRST-OF(glhist.tr-num) THEN
      v-tr-num = STRING(glhist.tr-num).
   ELSE
      v-tr-num = "".       
   
   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         '"' v-tr-num        '",'
         '"' glhist.actnum   '",'
         '"' glhist.jrnl     '",'
         '"' glhist.tr-dscr  '",'
         '"' glhist.tr-date  '",'
         '"' glhist.tr-amt   '",'
         SKIP .

   IF LAST-OF(glhist.tr-num) THEN DO:
      PUT "---------------------" TO 120 SKIP
          "Total:" AT 81 tot-all  TO 120 SKIP(1).
          
      ASSIGN
         tot-tot = tot-tot + tot-all
         tot-all = 0.
   END.
END.

FOR EACH gltrans WHERE
         gltrans.company EQ cocode
     AND gltrans.trnum   GE v-s-run
     AND gltrans.trnum   LE v-e-run
     AND gltrans.tr-date GE v-s-dat
     AND gltrans.tr-date LE v-e-dat
     NO-LOCK,
   FIRST account WHERE
         account.company EQ cocode 
     AND account.actnum  EQ gltrans.actnum
     NO-LOCK
     BREAK BY gltrans.trnum
           BY gltrans.actnum:

   ASSIGN
      v-print = YES
      tot-all = tot-all + gltrans.tr-amt.

   DISPLAY 
      gltrans.trnum WHEN FIRST-OF(gltrans.trnum) FORMAT "9999999" SPACE(5) 
      gltrans.actnum  FORMAT 'x(14)'  SPACE (2)
      account.dscr    FORMAT 'x(28)'  SPACE (2) 
      gltrans.jrnl                    SPACE (5)
      gltrans.tr-dscr                 SPACE (2)
      /* tmp-dscr*/
      gltrans.tr-date                 
      gltrans.tr-amt
      WITH FRAME aaa NO-LABELS STREAM-IO WIDTH 200 NO-BOX.
     
   IF FIRST-OF(gltrans.trnum) THEN
      v-tr-num = STRING(gltrans.trnum).
   ELSE
      v-tr-num = "". 

   IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         '"' v-tr-num         '",'
         '"' gltrans.actnum   '",'
         '"' account.dscr    '",'
         '"' gltrans.jrnl     '",'
         '"' gltrans.tr-dscr  '",'
         '"' gltrans.tr-date  '",'
         '"' gltrans.tr-amt   '",'
         SKIP .   
  
   IF LAST-OF(gltrans.trnum) THEN DO:
      PUT "-------------"       TO 132 SKIP
          "Total:"              AT 109
          tot-all               TO 132
          SKIP(1).
         
   ASSIGN
      tot-tot = tot-tot + tot-all
      tot-all = 0.
   END.
END.
  
IF v-print THEN
   PUT "============"      TO 132 SKIP
       "Grand Total:"      AT 109 
       tot-tot             TO 132
       SKIP(1).


      /* rtc 08/11/2008 */
IF v-export THEN DO:
   OUTPUT STREAM s-temp close.
   
END.



/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
