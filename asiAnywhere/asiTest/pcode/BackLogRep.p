

/*------------------------------------------------------------------------
    File        : BackLogRep.p
    Purpose     :  Print Order
    Main File   : oerep\r-backl.w 

    Syntax      :

    Description : Return a Dataset of Request For Order Backlog

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

{custom/xprint.i}
{oe/rep/backlog1.i}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}
DEFINE TEMP-TABLE ttBackLogRep NO-UNDO
FIELD backFile AS CHAR
FIELD her AS CHAR.

DEFINE DATASET dsBackLogRep FOR ttBackLogRep .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAct        AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBegsman    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndsman    AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBegord     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndord     AS INTEGER NO-UNDO. 
    DEFINE INPUT PARAMETER prmBeitem     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEnditem    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegdate    AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate    AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBeuser     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEnduser    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPrint      AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prminjob      AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmDet        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmInIt       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmSub        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmReLa       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmSort       AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmPrip       AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmQty        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmDis        AS CHARACTER NO-UNDO.      
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBackLogRep.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.                
    
    IF prmUser         = ?  THEN ASSIGN prmUser         = "".
    IF prmAct          = ?  THEN ASSIGN prmAct          = "".
    IF prmBeginCust    = ?  THEN ASSIGN prmBeginCust    = "".
    IF prmEndCust      = ?  THEN ASSIGN prmEndCust      = "".
    IF prmBegsman      = ?  THEN ASSIGN prmBegsman      = "". 
    IF prmEndsman      = ?  THEN ASSIGN prmEndsman      = "". 
    IF prmBegord       = ?  THEN ASSIGN prmBegord       = 0. 
    IF prmEndord       = ?  THEN ASSIGN prmEndord       = 0. 
    IF prmBeitem       = ?  THEN ASSIGN prmBeitem       = "". 
    IF prmEnditem      = ?  THEN ASSIGN prmEnditem      = "". 
    IF prmBeuser       = ?  THEN ASSIGN prmBeuser       = "". 
    IF prmEnduser      = ?  THEN ASSIGN prmEnduser      = "". 
    IF prmPrint        = ?  THEN ASSIGN prmPrint        = "".
    IF prminjob        = ?  THEN ASSIGN prminjob        = "".
    IF prmDet          = ?  THEN ASSIGN prmDet          = "".
    IF prmInIt         = ?  THEN ASSIGN prmInIt         = "".
    IF prmSub          = ?  THEN ASSIGN prmSub          = "".
    IF prmReLa         = ?  THEN ASSIGN prmReLa         = "".
    IF prmSort         = ?  THEN ASSIGN prmSort         = "".
    IF prmPrip         = ?  THEN ASSIGN prmPrip         = "".
    IF prmQty          = ?  THEN ASSIGN prmQty          = "".
    IF prmDis          = ?  THEN ASSIGN prmDis          = "".
   /* assign prmEndCust = prmBeginCust.*/
    
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL YES NO-UNDO. 
    DEF BUFFER xoe-ord FOR oe-ord.
    DEF VAR list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
    DEF VAR lv-pdf-file AS cha NO-UNDO.
    DEFINE VAR vPdfFile AS CHAR NO-UNDO.          
    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
    DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U  NO-UNDO.
    DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
    DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX"  NO-UNDO.
    DEFINE VARIABLE begin_user AS CHARACTER FORMAT "x(8)"  NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
    DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
    DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
    DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999  NO-UNDO.
    DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz"  NO-UNDO.
    DEFINE VARIABLE end_user AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL "C:\Inetpub\wwwroot\pdfs\backlog.csv" */  NO-UNDO.
    DEFINE VARIABLE rd_date AS CHARACTER INITIAL "Rel"  NO-UNDO.
    DEFINE VARIABLE rd_qoh AS CHARACTER INITIAL "FG"  NO-UNDO.
    DEFINE VARIABLE rd_show AS CHARACTER INITIAL "Status"  NO-UNDO.
    DEFINE VARIABLE rd_show2 AS CHARACTER INITIAL "Order#"  NO-UNDO.
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#"  NO-UNDO.
    DEFINE VARIABLE tb_jobs AS LOGICAL INITIAL yes  NO-UNDO.
    DEFINE VARIABLE tb_po-no AS LOGICAL INITIAL yes  NO-UNDO.
    DEFINE VARIABLE tb_qohgt0 AS LOGICAL INITIAL no  NO-UNDO.
    DEFINE VARIABLE tb_subt AS LOGICAL INITIAL no  NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no  NO-UNDO.
    DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.


DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF TEMP-TABLE tt-report LIKE report.
Define VAR custcount as char no-undo.
DEFINE STREAM excel.

DEF VAR prmComp AS CHAR NO-UNDO.
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
    ASSIGN cError = "Invalid Customer for user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid  Customer for user.....".
    RETURN.
END.

assign
 cocode = prmComp
 locode = usercomp.loc.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

IF prmAct = "printlog" THEN DO:
 ASSIGN
    v-today         = TODAY
    begin_cust-no   = prmBeginCust
    end_cust-no    = prmEndCust
    begin_slsmn    = prmBegsman
    end_slsmn      = prmEndsman
    begin_ord-no   = prmBegord
    end_ord-no     = prmEndord 
    begin_i-no     = prmBeitem
    end_i-no       = prmEnditem
    begin_due-date = prmBegdate
    end_due-date   = prmEndDate
    begin_user     = prmBeuser
    end_user       = prmEnduser
    rd_date        = IF  prmReLa   = "Yes" THEN "Rel" ELSE "Ship"
    tb_po-no       = IF  prmPrint  = "Yes" THEN TRUE ELSE FALSE 
    tb_jobs        = IF  prminjob  = "Yes" THEN TRUE ELSE FALSE 
    tb_detailed    = IF  prmDet    = "Yes" THEN TRUE ELSE FALSE
    tb_qohgt0      = IF  prmInIt   = "Yes" THEN TRUE ELSE FALSE
    tb_subt        = IF  prmSub    = "Yes" THEN TRUE ELSE FALSE
    rd_qoh         = IF prmQty = "Yes" THEN "FG" ELSE "Job"
    rd_show2       = IF prmDis = "Yes" THEN "Order#" ELSE "Job#"
       .

   IF prmSort = "C"  THEN  rd_sort = "Customer#" .
   IF prmSort = "O"  THEN rd_sort = "Due Date".
   IF prmSort = "S"  THEN rd_sort = "Salesman#" .

   IF prmPrip = "P"  THEN rd_show = "Price/Sales$" .
   IF prmPrip = "S"  THEN rd_show = "Status" .
   IF prmPrip = "PO" THEN rd_show = "PO Receipt Qty" .   
        
    ASSIGN
        tb_excel   = IF prmOut = "Yes" THEN TRUE ELSE FALSE .
   
   assign
    init-dir    =  v-webrootpath 
    fi_file = init-dir + "backlog" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
    vPdfFile   = "backlog" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".   
    DEFINE VAR vTxtFile AS CHAR NO-UNDO.
    vTxtFile = "backlog" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt". 
            
 run run-report.
 
    CREATE ttBackLogRep.
    IF tb_excel THEN DO:
    ASSIGN ttBackLogRep.backFile = vPdfFile.
    END.
     IF NOT tb_excel THEN DO:
          ASSIGN ttBackLogRep.backFile = vTxtFile .
     END.

END.

/*****************************************************PROCEDURE run-report :***************************************************************/
PROCEDURE run-report :
{sys/form/r-topw.f}

def var v-fcust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-fslsm like oe-ord.sman extent 2 init ["","zzz"].
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-fdate as date format "99/99/9999" extent 2 init [01/01/01,today].
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ponum as log init yes.
def var v-sort as char format "!" init "C".
def var v-sumdet as log format "Summary/Detail" init yes.
def var v-sub-item as log.
def var v-price AS INT.
def var v-jobs as log init yes.
def var v-all as log init no.
def var v-fg-qty as log init yes format "FG/Job".
def var v-ord-job as log init yes format "Order/Job".

def var v-profit as log init yes.
def var v-password as char no-undo.
def var security-flag as log no-undo.

def var v-qty as int extent 2.
def var v-cost as dec.
def var v-tot-qty as int format "->>>,>>>,>>9" extent 2.
def var v-tot-cost as dec format  "->>>,>>>,>>9.99" extent 2.
def var v-tot-sales as dec format "->>>,>>>,>>9.99" extent 2.
def var v-tot-pct as dec format "->>,>>9.99".
def var v-head as char format "x(132)" extent 4.
def var v-gpdollar as dec format  "->>>,>>>,>>9.99".
def var v-gp as dec.
def var v-uom as char format "x(4)".
def var v-qty-pal as int.
def var fstat like oe-ord.stat.
def var tstat like fstat init "".
def var v-job-no as char format "x(9)".
DEF VAR li-qty AS INT EXTENT 2 NO-UNDO.
DEF VAR v-date AS CHAR NO-UNDO.
DEF VAR lv-tmp-string AS CHAR NO-UNDO.

DEFINE VARIABLE chrDummy AS CHARACTER  NO-UNDO INIT "".
DEFINE VARIABLE chrSalesCustAndName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE excelheader AS CHARACTER  NO-UNDO.

format header
  v-head[1] skip
  v-head[2]
  fill("_",132) format "x(132)"
  with no-labels no-box no-underline stream-io width 132 frame f-top page-top.

format header
  fill("_",127) format "x(127)"
  with no-labels no-box no-underline stream-io width 128 frame f-topd page-top.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no    
  w-ord.po-num
  w-ord.i-name  format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  w-ord.price
  space(0)
  v-uom
  w-ord.t-price skip
  with frame ordhead-po no-labels no-box no-underline stream-io width 150.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  w-ord.price
  space(0)
  v-uom
  w-ord.t-price skip
  with frame ordhead no-labels no-box no-underline stream-io width 132.
  
format
  w-ord.due-date
  w-ord.ord-date
  v-job-no           format "x(9)"
  w-ord.po-num
  w-ord.i-name       format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  space(3)
  w-ord.stat
  space(3)
  w-ord.rel-date
  space(3)
  w-ord.rel-stat
  skip
  with frame ordhead-po-s no-labels no-box no-underline stream-io width 150.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no /* was w-ord.ord-no */
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  space(3)
  w-ord.stat
  space(3)
  w-ord.rel-date
  space(3)
  w-ord.rel-stat
  skip
  with frame ordhead-s no-labels no-box no-underline stream-io width 132.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no /* was v-job-no */          format "x(9)"
  w-ord.po-num
  w-ord.i-name       format "x(23)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9"
  w-ord.qty-due
  space(6)
  w-ord.po-received
  skip
  with frame ordhead-po-q no-labels no-box no-underline stream-io width 150.

format
  w-ord.due-date
  w-ord.ord-date
  v-job-no /* was w-ord.ord-no */
  w-ord.i-name at 32 format "x(25)"
  w-ord.i-no
  w-ord.qty-onh      format "->,>>>,>>9.9<<<"
  w-ord.qty-due
  space(6)
  w-ord.po-received
  skip
  with frame ordhead-q no-labels no-box no-underline stream-io width 132.

format
  skip(1)
  v-job-no label "Order No." colon 10 space(1)
  w-ord.due-date label "Date" space(1)
  w-ord.cust-no label "Customer" "-"
  w-ord.cust-name no-labels format "x(25)" space(1)
  w-ord.sman label "Sales Rep" skip(1)
 with frame detailhead side-labels no-box no-underline stream-io width 127.

format
  skip(1)
  v-job-no label "Order No." colon 10 space(1)
  w-ord.due-date label "Date" space(1)
  w-ord.cust-no label "Customer" "-"
  w-ord.cust-name no-labels format "x(25)" space(1)
  w-ord.sman label "Sales Rep" space(1)
  w-ord.po-num label "PO #" skip(1)
  with frame detailhead-po side-labels no-box no-underline stream-io width 127.

format
  w-ord.i-name label "Description" format "x(25)"
  w-ord.est-no label "Estimate#" format "x(8)"
  w-ord.qty-due label "Qty Due"
  w-ord.stat label "Status"
  w-ord.cost label "Std. Cost"
  w-ord.t-price label "Sales"
  v-gpdollar label "GP Dollars"
  v-gp label "GP%"
  with frame ordline no-box no-underline down stream-io width 125.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BACKLOG"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BACKLOG"
   sys-ctrl.descrip = "Print Cost & Profit on Order Backlog Report?".
  /*MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.*/
end.
v-profit = sys-ctrl.log-fld.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

assign
 str-tit2 = "Back Log Report"
 {sys/inc/ctrtext.i str-tit2 112}

 v-fcust[1]   = begin_cust-no
 v-fcust[2]   = end_cust-no
 v-fslsm[1]   = begin_slsmn
 v-fslsm[2]   = end_slsmn
 v-ford-no[1] = begin_ord-no
 v-ford-no[2] = end_ord-no
 v-fitem[1]   = begin_i-no
 v-fitem[2]   = end_i-no
 v-fdate[1]   = begin_due-date
 v-fdate[2]   = end_due-date
 v-ponum      = tb_po-no
 v-sort       = if rd_sort eq "Customer#" then "C" else
                if rd_sort eq "Due Date"  then "D" else "S"
 v-sumdet     = not tb_detailed
 v-sub-item   = tb_subt
 v-price      = lookup(rd_show,"Price/Sales$,Status,PO Receipt Qty")
 v-jobs       = tb_jobs
 v-all        = tb_qohgt0
 v-fg-qty     = rd_qoh eq "FG"
 v-ord-job    = rd_show2 EQ "order#"
 v-date       = rd_date.

if v-price EQ 1 then do:
 /* IF NOT security-flag THEN RUN passwd (3, OUTPUT security-flag).*/
    security-flag = YES.
end.


IF v-date EQ "Rel" THEN
   lv-tmp-string = "     Price          Sales $,  Status  Rel Date & Stat,   PO Qty Received".
ELSE
   lv-tmp-string = "     Price          Sales $,  Status  L Shp Dt & Stat,   PO Qty Received".

assign
 v-head[4] = if v-ord-job then "Order" else " Job "
 v-head[3] = ENTRY(v-price,lv-tmp-string).
     
if not v-ord-job then
  assign
   v-job-no:label in frame detailhead    = "Job#"
   v-job-no:label in frame detailhead-po = "Job#".

if v-sumdet and v-ponum then
  assign v-head[1] =
    "Due      Order    " + v-head[4] + "     PO                                "
 + "      Item                   Qty          Qty                              "
           v-head[2] =
    "Date     Date     Number    Number          Description             Number"
 + "             On-hand          Due" + v-head[3].

else if v-sumdet and not v-ponum then
  assign v-head[1] =
    "Due      Order    " + v-head[4] + "                                  Item "
    + "                    Qty          Qty                         "
           v-head[2] =
    "Date     Date     Number       Description               Number "
    + "              On-hand          Due" + v-head[3].

ELSE
   ASSIGN v-head[1] = ""
   v-head[2] = "".

/*{sys/inc/print1.i}*/
   IF tb_excel THEN DO:
       if tmp-dir = "" then tmp-dir = v-webrootpath .
       assign list-name = tmp-dir + "tmp" + string(time)
           init-dir = tmp-dir.
   END.
   IF NOT tb_excel THEN DO:
      if tmp-dir = "" then tmp-dir = v-webrootpath .
       assign list-name = tmp-dir + vTxtFile
           init-dir = tmp-dir.
   END.

{sys/inc/outprint.i value(lines-per-page)}

display "" with frame r-top.
if v-sumdet then 
  display "" with frame f-top.
else 
  display "" with frame f-topd.

/* 
FOR EACH tt-report:
  DELETE tt-report.
END.
 */

EMPTY TEMP-TABLE tt-report.

IF tb_excel THEN
  OUTPUT STREAM excel TO VALUE(fi_file).

{oerep/r-backl.i} 




end procedure.



/**********************************************************************/
PROCEDURE passwd:
DEF INPUT  PARAMETER ip-type      AS INT         NO-UNDO.
DEF OUTPUT PARAMETER op-validated AS LOG INIT NO NO-UNDO.


IF ip-type EQ 5 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "JOBPASS"
    NO-LOCK NO-ERROR.

ELSE
IF ip-type EQ 2 THEN
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "CUSTPASS"
    NO-LOCK NO-ERROR.

ELSE
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME    EQ "SECURITY"
    NO-LOCK NO-ERROR.
    
IF AVAIL sys-ctrl THEN DO:
  IF ip-type EQ 3 AND NOT sys-ctrl.log-fld THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE
  IF ip-type = 4 AND sys-ctrl.int-fld <> 1 THEN DO:
    op-validated = YES.
    RETURN.
  END.
  ELSE
  IF (ip-type = 1 OR ip-type = 2 OR ip-type = 5) AND
     NOT sys-ctrl.log-fld THEN DO:
    op-validated = NO.
    RETURN.
  END.
END.

/*ELSE DO:
  MESSAGE "No System Control record exists for " +
          IF ip-type EQ 5 THEN "JOBPASS"  ELSE
          IF ip-type EQ 2 THEN "CUSTPASS" ELSE "SECURITY..."
          VIEW-AS ALERT-BOX ERROR.
  RETURN.*/
END.


