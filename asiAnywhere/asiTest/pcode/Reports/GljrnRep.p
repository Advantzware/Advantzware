/*------------------------------------------------------------------------
    File        : GljrnRep.p
    Purpose     : 
    Main File   : gl/r-gljrn.w

    Syntax      :

    Description : Return a Dataset of Inventory Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGljrnRepEntries NO-UNDO
    FIELD gljfile AS CHAR.
DEFINE DATASET dsGljrnRepEntries FOR ttGljrnRepEntries.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegJur     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndJur     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPost        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGljrnRepEntries.

    IF prmUser     = ? THEN ASSIGN prmUser       = "".
    IF prmAction   = ? THEN ASSIGN prmAction     = "".
    IF prmBegJur   = ? THEN ASSIGN prmBegJur     = "0".
    IF prmEndJur   = ? THEN ASSIGN prmEndJur     = "0".
    IF prmBegDate  = ? THEN ASSIGN prmBegDate    = "".
    IF prmEndDate  = ? THEN ASSIGN prmEndDate    = "".
    IF prmSort     = ? THEN ASSIGN prmSort       = "".
    IF prmOut      = ? THEN ASSIGN prmOut        = "".
   
    

DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 1  NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Account#" NO-UNDO.
    

DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL "c:\Inetpub\wwwroot\pdfs\Commission.csv" */  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes     NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no   NO-UNDO.

DEF VAR post-line-error AS LOGICAL NO-UNDO INITIAL NO.
DEF VAR v-jrnal-2 AS CHAR NO-UNDO.

DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR  ip-post AS LOG NO-UNDO.
ASSIGN
    ip-post = ? .

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
DEF VAR glBalanceCheck AS LOG INIT YES NO-UNDO .
DEF VAR glOutOfBalance AS LOG NO-UNDO .
DEF BUFFER bgl-jrn  FOR gl-jrn.
DEF BUFFER bgl-jrnl FOR gl-jrnl.

DEF TEMP-TABLE tt-gl-jrn  LIKE gl-jrn.
DEF TEMP-TABLE tt-gl-jrnl LIKE gl-jrnl.

DEF STREAM s-temp.
DEF VAR tmp-dir AS cha NO-UNDO.

/*{custom/xprint.i}*/
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.


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

IF prmAction = "journal" THEN DO:


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
    begin_date    = DATE(prmBegDate)
    begin_j-no    = INT(prmBegJur)
    end_date      = DATE(prmEndDate)
    end_j-no      = INT(prmEndJur)
    rd_sort       = prmSort
     .
   tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

    assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "JournalEntri" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "JournalEntri" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "JournalEntri" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".

IF ip-post THEN DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
    /* gdm - 11050906 */
    REPEAT:
      FIND FIRST gl-ctrl EXCLUSIVE-LOCK
        WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
      IF AVAIL gl-ctrl THEN DO:
        ASSIGN xtrnum        = gl-ctrl.trnum + 1
               gl-ctrl.trnum = xtrnum.
        FIND CURRENT gl-ctrl NO-LOCK.
        LEAVE.
      END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
  END.

  RUN run-report. 

    CREATE ttGljrnRepEntries.
    IF tb_excel  THEN
        ASSIGN ttGljrnRepEntries.gljfile = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttGljrnRepEntries.gljfile = vTextFile .

  IF ip-post THEN
    IF v-postable THEN DO:        
      /*MESSAGE "Post Journals?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE lv-post.*/

      IF prmPost = "Yes" THEN do:
        RUN post-gl.
        RUN copy-report-to-audit-dir.
        cError = "Posting Complete" .
      END.
      ELSE RUN undo-trnum.
    END.

    ELSE DO:
      cError = "No Journals available for posting..." .
      RUN undo-trnum.
    END.


END.




PROCEDURE run-report :
DEF VAR v-excel-hdr AS   CHAR.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir.

  {sys/inc/outprint.i VALUE(lines-per-page)}

  time_stamp = string(time, "HH:MMam").
    tmpstore   = fill("_",125).
    ASSIGN fjrnl = begin_j-no
           tjrnl = END_j-no
           sort-by-acct = rd_sort EQ "Account#"          
           .

SESSION:SET-WAIT-STATE ("general").

{sys/form/r-topw.f}
   
    str-tit  = coname + " - " + loname.
    str-tit2 = "JOURNAL ENTRIES" +
             IF ip-post NE ? THEN ("  -  EDIT REGISTER " + STRING(xtrnum))
                             ELSE "".
    str-tit3 = "Journal " + string(fjrnl) + " Through " + string(tjrnl) + "  " +
               "Trans Date " + string(begin_date) + " Through " + string(end_date).
    x = (112 - length(str-tit)) / 2.
    str-tit  = fill(" ",x) + str-tit.
    x = (114 - length(str-tit2)) / 2.
    str-tit2 = fill(" ",x) + str-tit2.
    x = (132 - length(str-tit3)) / 2.
    str-tit3 = fill(" ",x) + str-tit3.
  
    DISPLAY str-tit3 format "x(130)"  skip(1) with frame r-top STREAM-IO.

    VIEW FRAME r-top.

    /* excel title rtc 08/12/08 */
    ASSIGN
        v-excel-hdr =  "Journal,Description,Date,Account,Description,Debit,Credit" 
        tbal = 0. /* 9508 CAH */

    v-postable = NO.

    IF tb_excel THEN DO:
        OUTPUT STREAM s-temp TO VALUE(fi_file).
        PUT STREAM s-temp UNFORMATTED 
            STRING(TODAY) + str-tit     SKIP
            time_stamp    + str-tit2    SKIP
            str-tit3                    SKIP
            v-excel-hdr                 SKIP.
    END.

    IF sort-by-acct THEN DO:
        {gl/gl-jreg.i "by gl-jrnl.actnum" 1}
    END.
    ELSE DO:
        {gl/gl-jreg.i "by gl-jrn.j-no by gl-jrnl.line" 2}
    END.
    
    /* rtc 08/11/2008 */
    IF tb_excel THEN DO:
        OUTPUT STREAM s-temp close.
       /* IF tb_runExcel THEN
            OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/
    END.
    
END PROCEDURE.

PROCEDURE undo-trnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* gdm - 11050906 */
  REPEAT:
    FIND FIRST gl-ctrl EXCLUSIVE-LOCK
      WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
    IF AVAIL gl-ctrl THEN DO:  
      if gl-ctrl.trnum = xtrnum THEN gl-ctrl.trnum = xtrnum - 1.
      FIND CURRENT gl-ctrl NO-LOCK.
      RELEASE gl-ctrl.
      LEAVE.
    END. /* IF AVAIL gl-ctrl */
  END. /* REPEAT */

END PROCEDURE.
