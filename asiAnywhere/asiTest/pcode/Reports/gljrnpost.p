

/*------------------------------------------------------------------------
    File        : gljrnpost.p
    Purpose     : 
    main pro    :      Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/


    
{sys/inc/var.i new shared}
    
    DEFINE TEMP-TABLE ttgljrnposting NO-UNDO
        FIELD gljrn AS CHAR
        FIELD post AS CHAR.

DEFINE DATASET dsgljrnposting FOR ttgljrnposting.
    DEFINE INPUT PARAMETER  prmUser          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmAction        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBeginjou      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEndjou        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmBegindate     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmEnddate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmsort          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER  prmPost          AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError           AS CHAR NO-UNDO.


 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsgljrnposting.

     IF prmUser         = ? THEN ASSIGN     prmUser        = "".   
     IF prmAction       = ? THEN ASSIGN     prmAction      = "". 
     IF prmBeginjou     = ? THEN ASSIGN     prmBeginjou    = "". 
     IF prmEndjou       = ? THEN ASSIGN     prmEndjou      = "". 
     IF prmBegindate    = ? THEN ASSIGN     prmBegindate   = "".  
     IF prmEnddate      = ? THEN ASSIGN     prmEnddate     = "".  
     IF prmOut          = ? THEN ASSIGN     prmOut         = "". 
     IF prmsort         = ? THEN ASSIGN     prmsort        = "". 
    

DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE begin_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 1        NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE end_j-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999   NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\tmp~\r-gljrn.csv" NO-UNDO.
DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Account#" NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.

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
DEF VAR glBalanceCheck AS LOG NO-UNDO .
DEF VAR glOutOfBalance AS LOG NO-UNDO.
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


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
   ASSIGN  
    init-dir    = v-webrootpath .




  IF prmAction = "journal" THEN DO:
    
        ASSIGN
       begin_j-no    = int(prmBeginjou)
       end_j-no      = int(prmEndjou)
       begin_date    = date(prmBegindate)
       end_date      = date(prmEnddate)
       rd_sort      = prmsort     . 
       tb_excel      = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

        DEFINE VAR vTextFile AS CHAR NO-UNDO.
        DEFINE VAR vExcalFile AS CHAR NO-UNDO.
        vTextFile = "Journalpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt" .

        vExcalFile =  "Journalpost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv" .
        
        fi_file  = init-dir + vExcalFile .


   IF prmPost = "Yes" THEN DO TRANSACTION:       /** GET next G/L TRANS. POSTING # **/
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

  IF prmPost = "Yes" THEN
    IF v-postable THEN DO:        
     
      
        RUN post-gl.
        RUN copy-report-to-audit-dir.
        cError =  "Posting Complete" .
    END.

    ELSE DO:
      cError = "No Journals available for posting..." .
      RUN undo-trnum.
      
    END.

   
  CREATE ttgljrnposting.
  IF prmOut = "Yes" THEN
    ASSIGN ttgljrnposting.gljrn = vExcalFile .
  ELSE
    ASSIGN ttgljrnposting.gljrn = vTextFile .

  END.
/*****************************************************************************************/

PROCEDURE run-report :
DEF VAR v-excel-hdr AS   CHAR.

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir
    .

  {sys/inc/outprint.i VALUE(lines-per-page)}

/*  IF td-show-parm THEN RUN show-param.*/
  
    time_stamp = string(time, "HH:MMam").
    tmpstore   = fill("_",125).
    ASSIGN fjrnl = begin_j-no
           tjrnl = END_j-no
           sort-by-acct = rd_sort EQ "Account#"          
           .


{sys/form/r-topw.f}
   
    str-tit  = coname + " - " + loname.
    str-tit2 = "JOURNAL ENTRIES" +  ("  -  EDIT REGISTER " + STRING(xtrnum)) .
                             
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

   /* RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").*/

END PROCEDURE.

PROCEDURE post-gl :
FOR EACH tt-gl-jrn:
  DELETE tt-gl-jrn.
END.

FOR EACH tt-gl-jrnl:
  DELETE tt-gl-jrnl.
END.

DO TRANSACTION:
   FOR EACH gl-jrn
       WHERE gl-jrn.company EQ cocode
         AND gl-jrn.posted  EQ NO
         /*AND gl-jrn.period  EQ tran-period*/
         AND gl-jrn.recur   EQ NO
         AND gl-jrn.journal GE fjrnl
         AND gl-jrn.journal LE tjrnl
             AND gl-jrn.tr-date GE begin_date
         AND gl-jrn.tr-date LE end_date
       BREAK BY gl-jrn.journal:

      RELEASE tt-gl-jrn.
      IF gl-jrn.reverse THEN DO:
        CREATE tt-gl-jrn.
        BUFFER-COPY gl-jrn TO tt-gl-jrn.
      END.

      /* if gl-jrn.tr-amt ne 0 then next. */
      FOR EACH gl-jrnl OF gl-jrn:
        IF AVAIL tt-gl-jrn THEN DO:
          CREATE tt-gl-jrnl.
          BUFFER-COPY gl-jrnl TO tt-gl-jrnl.
        END.

       find first bank
           where bank.company eq gl-jrn.company
             and bank.actnum  eq gl-jrnl.actnum
           no-error.
        if not available bank then do:
         create gltrans.
         assign
            gltrans.jrnl    = "GENERAL" /* 9508 CAH was STRING(JRN#,"99999") */
            gltrans.tr-date = gl-jrn.tr-date
            gltrans.tr-dscr = gl-jrnl.dscr + "JRN#" + string(gl-jrn.journal,"9999999")
            gltrans.actnum  = gl-jrnl.actnum
            gltrans.company = cocode
            gltrans.tr-amt  = gl-jrnl.tr-amt
            gltrans.period  = gl-jrn.period
            gltrans.trnum   = xtrnum.
/*
            gltrans.trnum   = gl-jrn.journal. /* 9508 CAH was jrnl.line */
*/
        end.
        if available bank then do:
         create gltrans.
         assign
            gltrans.jrnl    = "GENERAL" /* 9508 CAH was STRING(JRN#,"99999") */
            gltrans.tr-date = gl-jrn.tr-date
            gltrans.tr-dscr = gl-jrnl.dscr + "JRN#" + string(gl-jrn.journal,"9999999")
            gltrans.actnum  = gl-jrnl.actnum
            gltrans.company = cocode
            gltrans.tr-amt  = gl-jrnl.tr-amt
            gltrans.period  = gl-jrn.period
            gltrans.trnum   = xtrnum.
/*
            gltrans.trnum   = gl-jrn.journal. /* 9508 CAH was jrnl.line */
*/
         assign bank.bal = bank.bal + gl-jrnl.tr-amt.
        end.
      end.

      gl-jrn.posted = true.
   end.

   FOR EACH tt-gl-jrn:
     CREATE gl-jrn.
     BUFFER-COPY tt-gl-jrn EXCEPT j-no journal rec_key TO gl-jrn
     ASSIGN
      gl-jrn.reverse      = NO
      gl-jrn.from-reverse = YES
      gl-jrn.orig-jnl     = tt-gl-jrn.j-no.

     FOR EACH period                   
         WHERE period.company EQ cocode
           AND period.pst     GT tt-gl-jrn.tr-date
         BY period.yr
         BY period.pst:
       ASSIGN
        gl-jrn.tr-date = period.pst
        gl-jrn.period  = period.pnum.
       LEAVE.
     END.

     FOR EACH tt-gl-jrnl OF tt-gl-jrn:
       CREATE gl-jrnl.
       BUFFER-COPY tt-gl-jrnl EXCEPT rec_key TO gl-jrnl
       ASSIGN
        gl-jrnl.j-no   = gl-jrn.j-no
        gl-jrnl.tr-amt = tt-gl-jrnl.tr-amt * -1.

       DELETE tt-gl-jrnl.
     END.

     DELETE tt-gl-jrn.
   END.
END.

END PROCEDURE.

PROCEDURE copy-report-to-audit-dir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR targetfile AS CHAR FORMAT "X(50)" NO-UNDO.
  DEF VAR dirname1 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname2 AS CHAR FORMAT "X(20)" NO-UNDO.
  DEF VAR dirname3 AS CHAR FORMAT "X(20)" NO-UNDO.
  
  ASSIGN targetfile = lv-audit-dir + "\GL\GU2\Run#"
                    + STRING(xtrnum) + ".txt"
         dirname1 = lv-audit-dir
         dirname2 = lv-audit-dir + "\GL"
         dirname3 = lv-audit-dir + "\GL\GU2".

  OS-COPY VALUE(list-name) VALUE (targetfile).

  IF SEARCH(targetfile) EQ ? THEN DO:
    OS-CREATE-DIR VALUE(dirname1).
    OS-CREATE-DIR VALUE(dirname2).
    OS-CREATE-DIR VALUE(dirname3).
    OS-COPY VALUE(list-name) VALUE (targetfile).
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
