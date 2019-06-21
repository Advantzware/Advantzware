

/*------------------------------------------------------------------------
    File        : glchart.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttGlChartReport NO-UNDO
        FIELD chart AS CHAR
        FIELD post AS CHAR.

DEFINE DATASET dsGlChartReport FOR ttGlChartReport.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmchart         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPost          AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGlChartReport.

     IF prmUser       = ? THEN ASSIGN     prmUser     = "".   
     IF prmAction     = ? THEN ASSIGN     prmAction   = "". 
     IF prmchart      = ? THEN ASSIGN     prmchart    = "". 
     IF prmOut        = ? THEN ASSIGN     prmOut      = "". 
     IF prmPost       = ? THEN ASSIGN     prmPost     = "".  
     IF cError        = ? THEN ASSIGN     cError      = "".  
     
    


DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.

DEF VAR list-name as cha no-undo.
DEF VAR init-dir AS CHA NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR post-line-error AS LOGICAL NO-UNDO INITIAL NO.
DEF VAR v-jrnal-2 AS CHAR NO-UNDO.
DEF VAR lv-audit-dir AS CHAR NO-UNDO.

DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_user AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

def var v-unline as char format "x(80)" init
  "--------------- ------------------------- ------- ----------- ---" NO-UNDO.
def var time_stamp as ch NO-UNDO.
DEF VAR v-invalid AS LOG NO-UNDO.


def var save_id as recid NO-UNDO.
def var tbal as dec format "->>,>>>,>>9.99" NO-UNDO.
def var deb  as dec label "DEBIT"  format "->>,>>>,>>9.99" NO-UNDO.
def var cred as dec label "CREDIT" format "->>,>>>,>>9.99" NO-UNDO.
DEF VAR lv-rev AS CHAR LABEL " " NO-UNDO.
def var tot-deb  as dec format "->>,>>>,>>9.99" NO-UNDO.
def var tot-cred like tot-deb NO-UNDO.
def var ad like tot-deb NO-UNDO.
def var ac like tot-deb NO-UNDO.
def var gc like tot-deb NO-UNDO.
def var gd like tot-deb NO-UNDO.
def var curjnl as char format "x(8)" NO-UNDO.
def var fjrnl as int label "  From Journal Number" NO-UNDO.
def var tjrnl as int label "  Thru Journal Number" NO-UNDO.
def var xtrnum as int NO-UNDO.
def var sort-by-acct as log NO-UNDO.
DEF VAR v-postable AS LOG NO-UNDO.
DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF BUFFER bgl-jrn  FOR gl-jrn.
DEF BUFFER bgl-jrnl FOR gl-jrnl.

DEF TEMP-TABLE tt-gl-jrn  LIKE gl-jrn.
DEF TEMP-TABLE tt-gl-jrnl LIKE gl-jrnl.

DEF STREAM s-temp.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEF VAR ip-post AS LOG NO-UNDO.
ASSIGN ip-post = YES.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .


assign
 cocode = prmComp 
 v-today   = TODAY 
 g_company = cocode
 g_user    = prmUser .

ASSIGN tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .


DEF STREAM excel.

  IF prmAction = "chart" THEN DO:
    
        

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "chart" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "chart" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .


        run run-report.


   
  CREATE ttGlChartReport.
  IF prmOut = "Yes" THEN
    ASSIGN ttGlChartReport.chart = vExcalFile .
  ELSE
    ASSIGN ttGlChartReport.chart = vTextFile .

  END.
/*****************************************************************************************/

  PROCEDURE run-report :
/* --------------------------------------------------- gl/gl-chart.p  4/94 RM */
/* Chart Of Accounts Report Program - G/L Module                              */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

{sys/form/r-top.f}

form account.actnum
     account.type    format "x(4)"
     account.dscr
     
     header "Account Number            Type Description" skip
            fill("-",80) format "x(80)"
            
    with frame p-chart no-box no-labels no-underline stream-io down.
       
assign
 str-tit2 = "Chart of Accounts" 
 {sys/inc/ctrtext.i str-tit2 56}.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir .


{sys/inc/outprint.i value(lines-per-page)}

IF tb_excel THEN DO:
    OUTPUT STREAM excel TO VALUE(fi_file).
    ASSIGN excelheader = "Account Number,Type,Description".
    PUT STREAM excel UNFORMATTED excelheader SKIP.
END.





  display "" with frame r-top.

  for each account where account.company = cocode with frame p-chart:
    display account.actnum
             "  " + account.type + " "  @ account.type
             account.dscr.
    IF tb_excel THEN
        PUT STREAM excel UNFORMATTED replace(account.actnum,","," ") "," 
            replace(account.TYPE,","," ") "," 
            replace(account.dscr,","," ") SKIP.
    down.
  end.

IF tb_excel THEN 
DO:
  OUTPUT STREAM excel CLOSE.
END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
