/*------------------------------------------------------------------------
    File        : PstchkRep.p
    Purpose     : 
    Main File   : gl/r-pstchk.w

    Syntax      :

    Description : Return a Dataset of Inventory Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttPstchkRepEntries NO-UNDO
    FIELD pstfile AS CHAR.
DEFINE DATASET dsPstchkRepEntries FOR ttPstchkRepEntries.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegRun     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndRun     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegPdate   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPdate   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPstchkRepEntries.

    IF prmUser       = ? THEN ASSIGN prmUser         = "".
    IF prmBegRun     = ? THEN ASSIGN prmBegRun       = "0".
    IF prmEndRun     = ? THEN ASSIGN prmEndRun       = "0".
    IF prmBegDate    = ? THEN ASSIGN prmBegDate      = "".
    IF prmEndDate    = ? THEN ASSIGN prmEndDate      = "".
    IF prmBegPdate   = ? THEN ASSIGN prmBegPdate     = "".
    IF prmEndPdate   = ? THEN ASSIGN prmEndPdate     = "".
    IF prmOut        = ? THEN ASSIGN prmOut          = "".
   
    

DEFINE VARIABLE begin_pdate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE begin_rdate AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_run-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE end_pdate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE end_rdate AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO.
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

IF prmAction = "PostDate" THEN DO:


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
    begin_run-no    = int(prmBegRun)
    end_run-no     = int(prmEndRun)
    begin_rdate      = date(prmBegDate)
    end_rdate      = date(prmEndDate)
    begin_pdate       = date(prmBegPdate)
    end_pdate         = DATE(prmEndPdate) .

    tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE.

    assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "PostingDate" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "PostingDate" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "PostingDate" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".



  RUN run-report. 

    CREATE ttPstchkRepEntries.
    IF tb_excel  THEN
        ASSIGN ttPstchkRepEntries.pstfile = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttPstchkRepEntries.pstfile = vTextFile .

  

END.


PROCEDURE run-report :
{sys/form/r-top.f}.

DEF VAR ll AS LOG NO-UNDO.
DEF VAR v-tr-num AS CHAR NO-UNDO.

FORM HEADER SKIP(1) WITH FRAME r-top.

assign
 str-tit2 = "Reference Date vs Posting Date"
 {sys/inc/ctrtext.i str-tit2 56}.

/*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\" + vTextFile
       init-dir = tmp-dir.


{sys/inc/outprint.i value(lines-per-page)}

def var v-excel-hdr       as   char init
"Run#,Post Date,Ref Date,AP Description,Amount" no-undo.

if tb_excel then do:
  output stream s-temp to value(fi_file).
  put stream s-temp unformatted v-excel-hdr skip.
end.


  display "" with frame r-top.

  FOR EACH ar-ledger
      WHERE ar-ledger.company  EQ cocode
        AND ar-ledger.ref-date GE begin_rdate
        AND ar-ledger.ref-date LE end_rdate
        AND ar-ledger.tr-date  GE begin_pdate
        AND ar-ledger.tr-date  LE end_pdate
        AND ar-ledger.tr-num   GE begin_run-no
        AND ar-ledger.tr-num   LE end_run-no
        AND CAN-FIND(FIRST period
                     WHERE period.company EQ ar-ledger.company
                       AND period.pst     LE ar-ledger.ref-date
                       AND period.pend    GE ar-ledger.ref-date)
        AND CAN-FIND(FIRST period
                     WHERE period.company EQ ar-ledger.company
                       AND period.pst     LE ar-ledger.tr-date
                       AND period.pend    GE ar-ledger.tr-date)
        AND NOT CAN-FIND(FIRST period
                         WHERE period.company EQ ar-ledger.company
                           AND period.pst     LE ar-ledger.ref-date
                           AND period.pst     LE ar-ledger.tr-date
                           AND period.pend    GE ar-ledger.ref-date
                           AND period.pend    GE ar-ledger.tr-date)
      NO-LOCK BREAK BY ar-ledger.tr-num:
   
    IF FIRST-OF(ar-ledger.tr-num) THEN
       v-tr-num = STRING(ar-ledger.tr-num).
    ELSE
       v-tr-num = "".

    DISPLAY ar-ledger.tr-num    WHEN FIRST-OF(ar-ledger.tr-num)
                                FORMAT ">>>>>>>>>"
                                LABEL "Run#"
            ar-ledger.tr-date   WHEN FIRST-OF(ar-ledger.tr-num)
                                LABEL "Post Date"
            ar-ledger.ref-date  LABEL "Ref Date"
            ar-ledger.ref-num   LABEL "AR Description"
            ar-ledger.amt       FORMAT "->>>,>>>,>>9.99"
                                LABEL "Amount"
                                (TOTAL BY ar-ledger.tr-num)

        WITH NO-BOX FRAME ar DOWN STREAM-IO width 132.
    if tb_excel then 
        put stream s-temp unformatted
            TRIM(v-tr-num)                      + "," +
            trim(string(ar-ledger.tr-date))     + "," +
            trim(string(ar-ledger.ref-date))    + "," +
            trim(string(ar-ledger.ref-num))      + "," +
            trim(string(ar-ledger.amt, "->>>>>>>>9.99"))     
            skip.
    ll = YES.
  END.

  IF ll THEN PAGE.

  FOR EACH ap-ledger
      WHERE ap-ledger.company  EQ cocode
        AND ap-ledger.ref-date GE begin_rdate
        AND ap-ledger.ref-date LE end_rdate
        AND ap-ledger.tr-date  GE begin_pdate
        AND ap-ledger.tr-date  LE end_pdate
        AND ap-ledger.trnum    GE begin_run-no
        AND ap-ledger.trnum    LE end_run-no
        AND CAN-FIND(FIRST period
                     WHERE period.company EQ ap-ledger.company
                       AND period.pst     LE ap-ledger.ref-date
                       AND period.pend    GE ap-ledger.ref-date)
        AND CAN-FIND(FIRST period
                     WHERE period.company EQ ap-ledger.company
                       AND period.pst     LE ap-ledger.tr-date
                       AND period.pend    GE ap-ledger.tr-date)
        AND NOT CAN-FIND(FIRST period
                         WHERE period.company EQ ap-ledger.company
                           AND period.pst     LE ap-ledger.ref-date
                           AND period.pst     LE ap-ledger.tr-date
                           AND period.pend    GE ap-ledger.ref-date
                           AND period.pend    GE ap-ledger.tr-date)
      NO-LOCK BREAK BY ap-ledger.trnum:

    IF FIRST-OF(ap-ledger.trnum) THEN
       v-tr-num = STRING(ap-ledger.trnum).
    ELSE
       v-tr-num = "".

    DISPLAY ap-ledger.trnum     WHEN FIRST-OF(ap-ledger.trnum)
                                FORMAT ">>>>>>>>>"
                                LABEL "Run#"
            ap-ledger.tr-date   WHEN FIRST-OF(ap-ledger.trnum)
                                LABEL "Post Date"
            ap-ledger.ref-date  LABEL "Ref Date"
            ap-ledger.refnum    LABEL "AP Description"
            ap-ledger.amt       FORMAT "->>>,>>>,>>9.99"
                                LABEL "Amount"
                                (TOTAL BY ap-ledger.trnum)

        WITH NO-BOX FRAME ap DOWN STREAM-IO width 132.

    if tb_excel then 
        put stream s-temp unformatted
            trim(v-tr-num)                      + "," +
            trim(string(ap-ledger.tr-date))     + "," +
            trim(string(ap-ledger.ref-date))    + "," +
            trim(string(ap-ledger.refnum))      + "," +
            trim(string(ap-ledger.amt, "->>>>>>>>9.99"))     
            skip.
  END.
  
  /* rtc 08/11/2008 */
  IF tb_excel THEN DO:
    OUTPUT STREAM s-temp close.
    /*IF tb_runExcel THEN
        OS-COMMAND NO-WAIT START excel.exe VALUE(SEARCH(fi_file)).*/
  END.


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.
