

/*------------------------------------------------------------------------
    File        : openordrep.p
    Purpose     :  Open Order Report

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttOpenOrdReport NO-UNDO
    FIELD vExFile AS CHAR
        FIELD vFile AS CHAR.

    DEFINE DATASET dsOpenOrdRep FOR ttOpenOrdReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust         AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegOrdate       AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrdate       AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegPo           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPo           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegJob          AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndJob          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegJob2         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndJob2         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegItem         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem         AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegCad          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCad          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDue          AS DATE NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndDue          AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBegUser         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndUser         AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegSman         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSman         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSortcust        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSecondary       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmJobStat         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOrdStat         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDuedate         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmWipqty          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintjob        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDroporder       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmInorder         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmInqty           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmInact           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prminjob           AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOpenOrdRep.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


       IF  prmUser         = ?        THEN ASSIGN     prmUser          = "".
       IF  prmBeginCust    = ?        THEN ASSIGN     prmBeginCust     = "".
       IF  prmEndCust      = ?        THEN ASSIGN     prmEndCust       = "".
       IF  prmBegPo        = ?        THEN ASSIGN     prmBegPo         = "".
       IF  prmEndPo        = ?        THEN ASSIGN     prmEndPo         = "".
       IF  prmBegJob       = ?        THEN ASSIGN     prmBegJob        = "".
       IF  prmEndJob       = ?        THEN ASSIGN     prmEndJob        = "".
       IF  prmBegJob2      = ?        THEN ASSIGN     prmBegJob2       = "".
       IF  prmEndJob2      = ?        THEN ASSIGN     prmEndJob2       = "".
       IF  prmBegItem      = ?        THEN ASSIGN     prmBegItem       = "".
       IF  prmEndItem      = ?        THEN ASSIGN     prmEndItem       = "".
       IF  prmBegCad       = ?        THEN ASSIGN     prmBegCad        = "".
       IF  prmEndCad       = ?        THEN ASSIGN     prmEndCad        = "".
       IF  prmBegUser      = ?        THEN ASSIGN     prmBegUser       = "".
       IF  prmEndUser      = ?        THEN ASSIGN     prmEndUser       = "".
       IF  prmBegSman      = ?        THEN ASSIGN     prmBegSman       = "".
       IF  prmEndSman      = ?        THEN ASSIGN     prmEndSman       = "".
       IF    prmSortcust   = ?        THEN ASSIGN     prmSortcust      = "".
       IF    prmSecondary  = ?        THEN ASSIGN     prmSecondary     = "".
       IF    prmJobStat    = ?        THEN ASSIGN     prmJobStat       = "".
       IF    prmOrdStat    = ?        THEN ASSIGN     prmOrdStat       = "".
       IF    prmDuedate    = ?        THEN ASSIGN     prmDuedate       = "".
       IF    prmWipqty     = ?        THEN ASSIGN     prmWipqty        = "".
       IF    prmPrintjob   = ?        THEN ASSIGN     prmPrintjob      = "".
       IF    prmDroporder  = ?        THEN ASSIGN     prmDroporder     = "".
       IF    prmInorder    = ?        THEN ASSIGN     prmInorder       = "".
       IF    prmInqty      = ?        THEN ASSIGN     prmInqty         = "".
       IF    prmInact      = ?        THEN ASSIGN     prmInact         = "".
       IF    prminjob      = ?        THEN ASSIGN     prminjob         = "".
       IF    prmOutexcel   = ?        THEN ASSIGN     prmOutexcel      = "".
                                                                       

     

 

    DEFINE VARIABLE begin_cad-no    AS CHARACTER FORMAT "X(15)" NO-UNDO .                                                              
    DEFINE VARIABLE begin_cust-no   AS CHARACTER FORMAT "X(8)"   NO-UNDO.                                                              
    DEFINE VARIABLE begin_due-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.                                           
    DEFINE VARIABLE begin_i-no      AS CHARACTER FORMAT "X(15)":U   NO-UNDO .
    DEFINE VARIABLE begin_job-no    AS CHARACTER FORMAT "X(6)":U    NO-UNDO .
    DEFINE VARIABLE begin_job-no2   AS CHARACTER FORMAT "-99":U INITIAL "00" NO-UNDO.
    DEFINE VARIABLE begin_ord-date  AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001   NO-UNDO  .
    DEFINE VARIABLE begin_po-no     AS CHARACTER FORMAT "X(6)":U   NO-UNDO.
    DEFINE VARIABLE begin_slsmn     AS CHARACTER FORMAT "XXX"      NO-UNDO .
    DEFINE VARIABLE begin_userid    AS CHARACTER FORMAT "X(8)":U   NO-UNDO .
    DEFINE VARIABLE end_cad-no      AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz" NO-UNDO  .
    DEFINE VARIABLE end_cust-no     AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"    NO-UNDO.
    DEFINE VARIABLE end_due-date    AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 NO-UNDO  .
    DEFINE VARIABLE end_i-no        AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO   .
    DEFINE VARIABLE end_job-no      AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz"  NO-UNDO .
    DEFINE VARIABLE end_job-no2     AS CHARACTER FORMAT "-99":U INITIAL "99"       NO-UNDO .
    DEFINE VARIABLE end_ord-date    AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO  .
    DEFINE VARIABLE end_po-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO  .
    DEFINE VARIABLE end_slsmn       AS CHARACTER FORMAT "XXX" INITIAL "zzz"    NO-UNDO .
    DEFINE VARIABLE end_userid      AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz"   NO-UNDO.
    DEFINE VARIABLE lbl_due-date    AS CHARACTER FORMAT "X(256)":U INITIAL "Due Date?"        NO-UNDO.
    DEFINE VARIABLE lbl_jstat       AS CHARACTER FORMAT "X(256)":U INITIAL "Job Status?"      NO-UNDO.
    DEFINE VARIABLE lbl_ostat       AS CHARACTER FORMAT "X(256)":U INITIAL "Order Status?"    NO-UNDO.
    DEFINE VARIABLE lbl_sort        AS CHARACTER FORMAT "X(256)":U INITIAL "Secondary Sort?"  NO-UNDO .
    DEFINE VARIABLE lbl_sort-1      AS CHARACTER FORMAT "X(256)":U INITIAL "Primary Sort?"    NO-UNDO .
    DEFINE VARIABLE lbl_wip-qty     AS CHARACTER FORMAT "X(256)":U INITIAL "WIP Qty?"         NO-UNDO.
    DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(256)":U   /*INITIAL "C:\Inetpub\wwwroot\pdfs\openord.csv" */       NO-UNDO.
    DEFINE VARIABLE rd_due-date     AS CHARACTER INITIAL "Line"  NO-UNDO.
    DEFINE VARIABLE rd_jstat        AS CHARACTER INITIAL "All"   NO-UNDO .
    DEFINE VARIABLE rd_ostat        AS CHARACTER INITIAL "All"   NO-UNDO.
    DEFINE VARIABLE rd_sort         AS CHARACTER INITIAL "PO#"   NO-UNDO.
    DEFINE VARIABLE rd_sort-1       AS CHARACTER INITIAL "Customer#" NO-UNDO.
    DEFINE VARIABLE rd_wip-qty      AS CHARACTER INITIAL "1"         NO-UNDO.
    DEFINE VARIABLE tb_0-bal        AS LOGICAL INITIAL yes           NO-UNDO.
    DEFINE VARIABLE tb_0-wip        AS LOGICAL INITIAL yes           NO-UNDO.
    DEFINE VARIABLE tb_excel        AS LOGICAL INITIAL YES            NO-UNDO.
    DEFINE VARIABLE tb_job-qoh      AS LOGICAL INITIAL no            NO-UNDO.
    DEFINE VARIABLE tb_under        AS LOGICAL INITIAL no            NO-UNDO .
    DEFINE VARIABLE tb_job-qty AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_0-avl AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_est-count AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_est-pallets AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE vtextfile1       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile2       AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VAR tb_sch AS LOG INIT NO NO-UNDO.
    DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.



FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
   AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


    assign
 cocode = prmComp
 locode = usercomp.loc
 v-today = TODAY . 

DEF VAR v-program AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-report-title AS CHAR NO-UNDO.
DEF VAR v-sort AS CHAR NO-UNDO.
DEF VAR v-ordl AS LOG NO-UNDO.
DEF VAR v-q-onh LIKE itemfg.q-onh NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-due-date LIKE oe-ordl.req-date NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report
    FIELD q-onh    LIKE itemfg.q-onh
    FIELD q-shp    LIKE itemfg.q-onh
    FIELD q-rel    LIKE itemfg.q-onh
    FIELD q-wip    LIKE itemfg.q-onh
    FIELD q-avl    LIKE itemfg.q-onh
    FIELD po-no    LIKE oe-ord.po-no
    FIELD inv      AS   LOG
    FIELD inv-no   LIKE ar-invl.inv-no
    FIELD cad-no   LIKE itemfg.cad-no
    FIELD row-id   AS ROWID
    FIELD due-date LIKE oe-ordl.req-date
    FIELD unit-count LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    INDEX row-id row-id.

DEF TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.
DEF STREAM st-excel.

DEF VAR v-prompt-excel AS LOG NO-UNDO.

find first sys-ctrl WHERE
     sys-ctrl.company eq cocode AND
     sys-ctrl.name    eq "OR16"
     no-lock no-error.

if not avail sys-ctrl then
   do transaction:
      create sys-ctrl.
      assign
        sys-ctrl.company = cocode
        sys-ctrl.name    = "OR16"
        sys-ctrl.descrip = "Prompt for Excel Filename?".
   end.

v-prompt-excel = sys-ctrl.log-fld.
  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    init-dir    = v-webrootpath.
    lv-pdf-file = init-dir + 'OPEN ORDER' .
    lv-pdf-file = lv-pdf-file + prmBeginCust + STRING(TIME) .
    vPdfFile   = 'OPEN ORDER' + prmBeginCust + STRING(TIME) + '.pdf'.
    
/*IF  NOT tb_excel  THEN 
       DO:
       RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
    END.*/
    ASSIGN
            init-dir    = v-webrootpath
        v-excel-file = init-dir + "openord" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "openord" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
              
run run-report.
 IF   tb_excel  THEN  DO:
     CREATE ttOpenOrdReport.
           ASSIGN ttOpenOrdReport.vFile = vPdfFile.
 
 
 
 END.
  
    /*****************************************PROCEDURE run-report :*****************************************************/
 PROCEDURE run-report :
 {sys/form/r-top3w.f}

DEF BUFFER b-tt-report FOR tt-report.
DEF BUFFER b-oe-rell FOR oe-rell.

def var v-cust  like oe-ord.cust-no  extent 2 init ["","zzzzzzzz"].
def var v-date  like ar-inv.inv-date format "99/99/9999"
                                     extent 2 init [today, 12/31/9999].
def var v-po    like oe-relh.po-no   extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-job   like oe-ord.job-no   extent 2 init ["","zzzzzz"].
def var v-job2  like oe-ord.job-no2  format "99" extent 2 init [0,99].
def var v-item  like oe-ordl.i-no    extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-inc   as   log             format "Yes/No" init yes.
def var v-stat  as   char format "!" init "A".
def var v-ostat as   char format "!" init "A".
def var v-jobq  as   log             format "Yes/No" init no.

def var v-dat     as   date format "99/99/99".
def var v-bal-qty as   int format ">>>,>>>,>>9".
def var v-inv-amt as   int format ">>>>,>>9.99".
def var v-cust-no like cust.cust-no.
def var v-name    like cust.name.
def var v-sman    like sman.sman.
def var v-sname   like sman.sname.
def var v-field1  as   char format "x(36)".
def var v-ord-no  as   char.
def var v-q-onh   like itemfg.q-onh NO-UNDO.
def var v-q-shp   like v-q-onh NO-UNDO.
def var v-q-rel   like v-q-onh NO-UNDO.
def var v-q-wip   like v-q-onh NO-UNDO.
def var v-q-avl   like v-q-onh NO-UNDO.
DEF VAR lv-slsmn  AS CHAR NO-UNDO.

def var v-time as int.
v-time = time.
DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.
DEF VAR lv-job-qty AS DEC NO-UNDO.
DEF VAR lv-rec-qty AS DEC NO-UNDO.
ASSIGN
    begin_job-no   = prmBegJob
    begin_job-no2  = prmBegJob2
    begin_due-date = prmBegDue
    end_due-date   = prmEndDue
    begin_userid   = prmBegUser   
    end_userid     = prmEndUser
    begin_slsmn    = prmBegSman   
    end_slsmn      = prmEndSman  . 
    /*rd_sort-1      = prmSortcust  */
   /* rd_sort        = prmSecondary*/
    IF prmWipqty  = "O" THEN ASSIGN  rd_wip-qty = "Order Qty - OH - Ship" .
   IF prmWipqty  = "J" THEN ASSIGN  rd_wip-qty = "Order Qty - OH - Ship" .
    tb_job-qty     = IF prmPrintjob  = "yes" THEN TRUE ELSE FALSE.
    tb_under       = IF prmDroporder = "yes" THEN TRUE ELSE FALSE.
    tb_0-bal       = IF prmInorder   = "yes" THEN TRUE ELSE FALSE.  
    tb_0-wip       = IF prmInqty     = "yes" THEN TRUE ELSE FALSE.  
    tb_0-avl       = IF prmInact     = "yes" THEN TRUE ELSE FALSE.       
    tb_job-qoh     = IF prminjob     = "yes" THEN TRUE ELSE FALSE.     
    tb_excel       = IF prmOutexcel  = "yes" THEN TRUE ELSE FALSE.   
    IF prmOrdStat = "O" THEN ASSIGN rd_ostat = "Open".
    IF prmOrdStat = "C" THEN ASSIGN rd_ostat = "Closed".
    IF prmOrdStat = "A" THEN ASSIGN rd_ostat = "All".
    IF prmJobStat = "O" THEN ASSIGN rd_jstat = "Open".
    IF prmJobStat = "C" THEN ASSIGN rd_jstat = "Closed".
    IF prmJobStat = "A" THEN ASSIGN rd_jstat = "All".
    IF prmDuedate = "L" THEN ASSIGN rd_due-date = "Line".
    IF prmDuedate = "R" THEN ASSIGN rd_due-date = "Release".
IF prmSortcust = "C" THEN
        ASSIGN rd_sort-1 = "Customer#".
    IF prmSortcust = "D" THEN
        ASSIGN rd_sort-1 = "Due Date".
    IF prmSortcust = "S" THEN
        ASSIGN rd_sort-1 = "Salesman".


    IF prmSecondary = "P" THEN
        ASSIGN rd_sort = "PO".
    IF prmSecondary = "I" THEN
        ASSIGN rd_sort = "It".
    IF prmSecondary = "C" THEN
        ASSIGN rd_sort = "Cu".
    IF prmSecondary = "F" THEN
        ASSIGN rd_sort = "FG".
    IF prmSecondary = "O" THEN
        ASSIGN rd_sort = "Or".
    IF prmSecondary = "D" THEN
        ASSIGN rd_sort = "D".
    IF prmSecondary = "CA" THEN
        ASSIGN rd_sort = "CA".
    
   


FORMAT HEADER
       SKIP(1)
       "Cust#   "
       "Due Date"
       "Cust Part#     "
       "Item Description              "
       "FG Item #      "
       "Order#"
       "CAD#         "
       "PO#       "
       " Order Qty"
       "Qty OnHand"
       "Qty Shippd"
       "Qty ActRel"
       "   Qty WIP"
       " Qty Avail"
       SKIP
       "--------"
       "--------"
       "---------------"
       "------------------------------"
       "---------------"
       "------"
       "-------------"
       "----------"
       "----------"
       "----------"
       "----------"
       "----------"
       "----------"
       "----------"
       
    WITH FRAME r-top3 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP STREAM-IO WIDTH 200.

ASSIGN 
 str-tit2 = "Open Order Report"
 str-tit3 = (IF rd_sort-1 EQ "Customer#" THEN ""
                                         ELSE "By " + TRIM(rd_sort-1) + " ") +
            "By Customer# " +
            "By " + TRIM(rd_sort)
 {sys/inc/ctrtext.i str-tit2 112}
{sys/inc/ctrtext.i str-tit3 132}

 v-cust[1]  = prmBeginCust
 v-cust[2]  = prmEndCust
 v-date[1]  = prmBegOrdate
 v-date[2]  = prmEndOrdate
 v-po[1]    = prmBegPo
 v-po[2]    = prmEndPo
 v-job[1]   = fill(" ",6 - length(trim(begin_job-no))) +
              trim(begin_job-no) + string(int(begin_job-no2),"99")
 v-job[2]   = fill(" ",6 - length(trim(end_job-no)))   +
              trim(end_job-no)   + string(int(end_job-no2),"99")
 v-item[1]  = prmBegItem
 v-item[2]  = prmEndItem
 v-sort     = substr(rd_sort,1,2)
 v-inc      = IF prmInorder = "Yes" THEN TRUE ELSE FALSE
 v-stat     = substr(rd_jstat,1,1)
  v-ostat    = substr(rd_ostat,1,1)
 v-jobq     = tb_job-qty .

/* sys/inc/print1.i */

if tmp-dir = "" then tmp-dir = "C:\Inetpub\wwwroot\pdfs".
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.

/*{sys/inc/outprint.i "value(lines-per-page) append" }                                  */
{sys/inc/outprint.i value(0)}
    PUT "<PDF=DIRECT><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><P5><ADJUST=LPI><CPI = 38><P5>" FORM "x(165)" SKIP.
  
    IF tb_excel THEN DO:
   OUTPUT STREAM st-excel TO VALUE(v-excel-file).
   PUT STREAM st-excel
       "Cust#   ," 
       "Due Date,"
       "Cust Part#,"
       "Item Description,"
       "FG Item #," 
       "Order#,"
       "CAD#,"
       "PO#,"
       " Order Qty,"
       "Qty OnHand,"
       "Qty Shippd,"
       "Qty ActRel,"
       "   Qty WIP,"
       " Qty Avail" SKIP.
END.

VIEW FRAME r-top.

FOR EACH tt-report:
  DELETE tt-report.
END.
EMPTY TEMP-TABLE tt-fg-bin.

{oerep/r-ordopn.i}
    END.
/*******************************************************PROCEDURE build-tt :******************************************/
PROCEDURE build-tt :
  DEF INPUT PARAM ip-date AS DATE NO-UNDO.
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.

  DEF BUFFER b-ar-invl FOR ar-invl.
  DEF BUFFER b-inv-head FOR inv-head.
  DEF BUFFER b-inv-line FOR inv-line.
  DEF BUFFER b-oe-rell FOR oe-rell.

  DEF VAR v-po-no LIKE oe-ord.po-no NO-UNDO.
  
  v-po-no = oe-ordl.po-no.

  CREATE tt-report.

  FIND FIRST itemfg
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK NO-ERROR.
  IF AVAIL itemfg THEN tt-report.cad-no = itemfg.cad-no.

  IF tt-report.cad-no EQ "" THEN DO:
    RELEASE eb.
    IF TRIM(oe-ordl.est-no) NE "" THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.est-no   EQ oe-ordl.est-no
          AND eb.stock-no EQ oe-ordl.i-no
          AND eb.cad-no   NE ""
        USE-INDEX est-no NO-LOCK NO-ERROR.
    IF NOT AVAIL eb THEN
    FIND FIRST eb
        WHERE eb.company  EQ oe-ordl.company
          AND eb.stock-no EQ oe-ordl.i-no
          AND eb.cad-no   NE ""
        USE-INDEX stock NO-LOCK NO-ERROR.
    IF AVAIL eb THEN tt-report.cad-no = eb.cad-no.
  END.

  RELEASE eb.

  IF TRIM(oe-ordl.est-no) NE "" THEN
  DO:
     FIND FIRST eb WHERE
          eb.company  EQ oe-ordl.company AND
          eb.est-no   EQ oe-ordl.est-no AND
          eb.stock-no EQ oe-ordl.i-no AND
          eb.form-no  EQ oe-ordl.form-no AND
          eb.blank-no EQ oe-ordl.blank-no
          NO-LOCK NO-ERROR.

     IF AVAIL eb THEN
     DO:
        ASSIGN
          tt-report.unit-count = eb.cas-cnt
          tt-report.units-pallet = eb.cas-pal.
        RELEASE eb.
     END.
  END.

  ASSIGN
   tt-report.term-id  = ""
   tt-report.key-01   = IF rd_sort-1 EQ "Due Date" THEN
                          STRING(YEAR(lv-due-date),"9999") +
                          STRING(MONTH(lv-due-date),"99")  +
                          STRING(DAY(lv-due-date),"99")
                        ELSE
                        IF rd_sort-1 EQ "Salesman" THEN
                          oe-ordl.s-man[1]         ELSE ""
   tt-report.key-02   = oe-ord.cust-no
   tt-report.key-03   = IF v-sort EQ "PO" THEN v-po-no
                        ELSE 
                        IF v-sort EQ "It" THEN
                          (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                        ELSE
                        IF v-sort EQ "Cu" THEN
                          (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "FG" THEN
                          (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                        IF v-sort EQ "Or" THEN
                          (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                        ELSE
                        IF v-sort EQ "CA" THEN
                          (STRING(tt-report.cad-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE
                          (STRING(YEAR(lv-due-date),"9999") +
                           STRING(MONTH(lv-due-date),"99")  +
                           STRING(DAY(lv-due-date),"99")    +
                           STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
   tt-report.key-04   = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) +
                        TRIM(oe-ordl.job-no) + "-" +
                        STRING(oe-ordl.job-no2,"99")
   tt-report.key-05   = STRING(oe-ord.ord-no,"99999999999")
   tt-report.key-06   = oe-ordl.i-no
   tt-report.key-07   = STRING(YEAR(ip-date),"9999") +
                        STRING(MONTH(ip-date),"99")  +
                        STRING(DAY(ip-date),"99")
   tt-report.po-no    = v-po-no
   tt-report.rec-id   = ip-recid
   tt-report.row-id   = ROWID(oe-ordl)
   tt-report.due-date = lv-due-date
   v-ordl             = NO.

  FIND b-ar-invl WHERE RECID(b-ar-invl) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL b-ar-invl THEN
    ASSIGN
     tt-report.q-shp  = b-ar-invl.ship-qty
     tt-report.inv    = YES
     tt-report.inv-no = b-ar-invl.inv-no.

  FIND b-inv-line WHERE RECID(b-inv-line) EQ ip-recid NO-LOCK NO-ERROR.
  IF AVAIL b-inv-line THEN DO:
    FIND FIRST b-inv-head WHERE b-inv-head.r-no EQ b-inv-line.r-no NO-LOCK.
    ASSIGN
     tt-report.q-shp  = b-inv-line.ship-qty
     tt-report.inv    = YES
     tt-report.inv-no = b-inv-head.inv-no.
  END.

  FIND b-oe-rell WHERE RECID(b-oe-rell) EQ ip-recid NO-LOCK NO-ERROR.
  IF NOT tt-report.inv AND
     AVAIL b-oe-rell   THEN tt-report.q-rel = b-oe-rell.qty.

END PROCEDURE.
/*************************************PROCEDURE calc-qoh :*********************************************/
PROCEDURE calc-qoh :

  DEF VAR vdat          AS   DATE.
  DEF VAR v-curr        AS   LOG.
  DEF VAR v-q-or-v      AS   LOG.

  DEF VAR v-qohj        AS   DEC                EXTENT 6.
  DEF VAR v-qohi        LIKE v-qohj.
  DEF VAR v-qty         AS   INT.
  DEF VAR v-qty1        LIKE v-qty.
  DEF VAR v-qtyc        LIKE v-qty.
  DEF VAR v-red         LIKE v-qty.
  DEF VAR v             AS   INT.
  DEF VAR v-val         AS   DEC                EXTENT 4.
  DEF VAR v-cst         AS   DEC                EXTENT 4.
  DEF VAR v-u-val       AS   DEC.
  DEF VAR v-u-cst       AS   DEC.
  DEF VAR v-date        AS   DATE.

  DEF BUFFER b-f-rc for fg-rcpth.
  DEF BUFFER b-f-rd for fg-rdtlh.


  ASSIGN
   vdat     = TODAY
   v-curr   = YES
   v-q-or-v = YES.
  
  FOR EACH itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-LOCK,
      EACH fg-bin
      WHERE fg-bin.company EQ itemfg.company
        AND fg-bin.i-no    EQ itemfg.i-no
      NO-LOCK:

    CREATE tt-fg-bin.
    BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
  END.

END PROCEDURE.

