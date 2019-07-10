/*------------------------------------------------------------------------
    File        : PostckRep.p
    Purpose     : 
    Main File   : glrep/r-postck.w

    Syntax      :

    Description : Return a Dataset of Inventory Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttPostckRepEntries NO-UNDO
    FIELD postckfile AS CHAR.
DEFINE DATASET dsPostckRepEntries FOR ttPostckRepEntries.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegRun     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndRun     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate    AS CHAR NO-UNDO.
    
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPostckRepEntries.

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

IF prmAction = "PostCheck" THEN DO:


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
       fi_file = init-dir + "PostCheck" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "PostCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "PostCheck" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".



  RUN run-report. 

    CREATE ttPostckRepEntries.
    IF tb_excel  THEN
        ASSIGN ttPostckRepEntries.postckfile = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttPostckRepEntries.postckfile = vTextFile .

  

END.


PROCEDURE run-report :
/* -------------------------------------------------- gl/gl-check.p 09/01 JLF */
/* GL Transaction - Check each Journal Posting                                */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.f}
DEF VAR v-export     AS LOGICAL.
DEF VAR v-excel-hdr  AS CHAR.
DEF VAR v-exp-name   AS CHAR FORMAT "x(40)" INIT "c:\tmp\r-postrg.csv".
DEF VAR v-tr-num     AS CHAR. 

def var v-s-run like glhist.tr-num format ">>>>>>" init 0.
def var v-e-run like v-s-run init 999999.
def var v-s-dat like glhist.tr-date format "99/99/9999" init 01/01/0001.
def var v-e-dat like v-s-dat init 12/31/9999.

def var v-tot   as   dec format "->>,>>>,>>9.99".

form header skip(1) with frame r-top.

SESSION:SET-WAIT-STATE ("general").

assign
 str-tit2 = "GL Postings Check"
 {sys/inc/ctrtext.i str-tit2 56}
 
 v-s-run = begin_run-no
 v-e-run = end_run-no
 v-s-dat = begin_date
 v-e-dat = end_date
 
 uperiod = period
 v-export = tb_excel
 v-exp-name = fi_file
 v-excel-hdr = "Trans Date,Run #,Journal,Balance".

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

IF v-export THEN DO:
   OUTPUT STREAM s-temp TO VALUE(v-exp-name).
   PUT STREAM s-temp UNFORMATTED 
      v-excel-hdr                 
   SKIP.
END. 

display "" with frame r-top.

  for each glhist
      where glhist.company eq cocode
        and glhist.tr-num  ge v-s-run
        and glhist.tr-num  le v-e-run
        and glhist.tr-date ge v-s-dat
        and glhist.tr-date le v-e-dat
      no-lock
       
      break by glhist.tr-date
            by glhist.tr-num:
            
    v-tot = v-tot + glhist.tr-amt.

    if last-of(glhist.tr-num) then do:
      display glhist.tr-date    label "Trans Date"
              space(3)
              glhist.tr-num     label "Run #"       format "9999999"
              space(3)
              glhist.jrnl       label "Journal"
              space(3)
              v-tot             label "Balance"
              string(v-tot eq 0,"/Out of Balance")
                 format "x(14)" no-label
             
        with frame f-1 stream-io width 200 down no-box no-attr-space.
      
      IF v-export THEN
         PUT STREAM s-temp UNFORMATTED
            '"' glhist.tr-date   '",'
            '"' glhist.tr-num    '",'
            '"' glhist.jrnl      '",'
            '"' v-tot            '",'
            SKIP .
      v-tot = 0.
    end.
    
    if last-of(glhist.tr-date) then put skip(1).
  end.

  for each gltrans
      where gltrans.company eq cocode
        and gltrans.trnum   ge v-s-run
        and gltrans.trnum   le v-e-run
        and gltrans.tr-date ge v-s-dat
        and gltrans.tr-date le v-e-dat
      no-lock
       
      break by gltrans.tr-date
            by gltrans.trnum:
            
    v-tot = v-tot + gltrans.tr-amt.

    if last-of(gltrans.trnum) then do:
      display gltrans.tr-date   label "Trans Date"
              space(3)
              gltrans.trnum     label "Run #"       format "9999999"
              space(3)
              gltrans.jrnl      label "Journal"
              space(3)
              v-tot             label "Balance"
              string(v-tot eq 0,"/Out of Balance")
                 format "x(14)" no-label
             
        with frame f-2 stream-io width 200 down no-box no-attr-space.
    IF v-export THEN
      PUT STREAM s-temp UNFORMATTED
         '"' gltrans.tr-date        '",'
         '"' gltrans.trnum   '",'
         '"' gltrans.jrnl     '",'
         '"' v-tot  '",'
         '"' string(v-tot eq 0,"/Out of Balance") '",'
         SKIP .
      v-tot = 0.
    END.
    
    if last-of(gltrans.tr-date) then put skip(1).
  end.


IF v-export THEN DO:
   OUTPUT STREAM s-temp close.
   /*IF tb_runExcel THEN
      OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(v-exp-name)).*/
END.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
