/*------------------------------------------------------------------------
    File        : CommissionRep.p
    Purpose     : Commission Report

    Syntax      :

    Description : Return a Dataset of Commission Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCommissionRep NO-UNDO
    FIELD commrepfile AS CHAR.
DEFINE DATASET dsCommissionRep FOR ttCommissionRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER CommRep       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPtdYtd     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPeriod     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBegDate    AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndDate    AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBegSales   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSales   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBegCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCategory   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDetailed   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrepCharg  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCostProfit AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCost       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintCustPart AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCommissionRep.
    IF prmUser = ?      THEN ASSIGN prmUser = "".
    IF CommRep = ?      THEN ASSIGN CommRep = "".
    IF prmPtdYtd = ?    THEN ASSIGN prmPtdYtd = "".
    IF prmPeriod = ?    THEN ASSIGN prmPeriod = 0.
    IF prmBegSales = ?  THEN ASSIGN prmBegSales = "".
    IF prmEndSales = ?  THEN ASSIGN prmEndSales = "".
    IF prmBegCust = ?   THEN ASSIGN prmBegCust = "".
    IF prmEndCust = ?   THEN ASSIGN prmEndCust = "".
    IF prmCategory = ?  THEN ASSIGN prmCategory = "".
    IF prmDetailed = ?  THEN ASSIGN prmDetailed = "".
    IF prmPrepCharg = ? THEN ASSIGN prmPrepCharg = "".
    IF prmCost = ?      THEN ASSIGN prmCost = "".
    
    
DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_period AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX"  NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz"  NO-UNDO.
DEFINE VARIABLE fg-cat AS CHARACTER FORMAT "X(5)":U  NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" /* INITIAL "c:\Inetpub\wwwroot\pdfs\Commission.csv" */  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE rd_cost1 AS CHARACTER INITIAL "FG"  NO-UNDO.
DEFINE VARIABLE rd_ptd AS CHARACTER INITIAL "PTD"   NO-UNDO.
DEFINE VARIABLE tb_detailed AS LOGICAL INITIAL no   NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes     NO-UNDO.
DEFINE VARIABLE tb_prep AS LOGICAL INITIAL no       NO-UNDO.
DEFINE VARIABLE tb_printcost AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no   NO-UNDO.
DEFINE VARIABLE rd_part-fg AS CHARACTER INITIAL "Cust Part#" NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
/*{custom/xprint.i}*/
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

def var v-per-rpt   as   log format "PTD/YTD" init yes.
def var v-period    as   int init 1.
def var v-cat       like itemfg.procat.
def var v-sman      as   char format "x(3)" extent 2 init ["", "zzz"].
def var v-date      as   date extent 2 init [01/01/01, today].
def var v-cust      as   character extent 2 init ["", "zzzzzzzz"].
def var v-sumdet    as   log format "Summary/Detail" init yes.
def var v-cost1     as   char.
def var v-year      as   integer.

def workfile w-comm    no-undo
       field sman    as   char
       field samt    like ar-invl.amt
       field camt    like ar-invl.amt
       field cost    like ar-invl.amt.

DEF TEMP-TABLE tt-report LIKE report
    FIELD row-id AS ROWID.

DEF VAR v-print-fmt AS CHARACTER NO-UNDO.
DEF VAR is-xprint-form AS LOGICAL.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF STREAM st-excell.


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




FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBegCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
IF CommRep = "Commission" THEN DO:
    assign
    v-today       =  TODAY
    cocode        =   prmComp
    locode        =   usercomp.loc
    rd_ptd        =   prmPtdYtd    
    begin_period  =   prmPeriod    
    begin_date    =   prmBegDate   
    end_date      =   prmEndDate   
    begin_slsmn   =   prmBegSales  
    end_slsmn     =   prmEndSales  
    begin_cust-no =   prmBegCust   
    end_cust-no   =   prmEndCust   
    fg-cat        =   prmCategory
    rd_cost1      =   prmCost
    rd_part-fg    =   prmPrintCustPart     . 
   
   tb_detailed  =  IF  prmDetailed ="Yes" THEN TRUE ELSE FALSE.
   tb_prep  =  IF  prmPrepCharg  ="Yes" THEN TRUE ELSE FALSE.
   tb_printcost  = IF prmCostProfit = "Yes" THEN TRUE ELSE FALSE.
   tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
   assign
       init-dir    = v-webrootpath
       fi_file = init-dir + "Commission" +
                     STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "Commission" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
       DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "Commission" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".

run run-report.
   
    CREATE ttCommissionRep.
    IF tb_excel  THEN
        ASSIGN ttCommissionRep.commrepfile = vPdfFile.
    IF NOT  tb_excel  THEN
        ASSIGN ttCommissionRep.commrepfile = vTextFile .
END.

PROCEDURE run-report :
/* ------------------------------------------------ oe/rep/sa-comm.p 3/96 JLF */
/* Commission Summary / Detail Report                                         */
/* -------------------------------------------------------------------------- */
{sys/form/r-top3w.f}

DEF BUFFER b-ar-invl FOR ar-invl.
DEF BUFFER b-ar-cashl FOR ar-cashl.

def var v-frst      as   log extent 2.
def var p-sman      as   char format "x(3)".
def var v-camt      like ar-invl.amt.
def var v-prof      like ar-invl.amt.
def var v-comm      as   dec format ">>9.99".
def var v-gp        as   dec format ">>9.99".
def var v-slsm      like ar-invl.sman extent 1.
def var v-slsc      like ar-invl.s-comm extent 1.
def var v-slsp      like ar-invl.s-pct extent 1.
def var v-inv-no    like ar-invl.inv-no.
def var v-procat    like itemfg.procat.
def var v-qty       as   dec.
def var v-amt       like ar-invl.amt.
def var v-cost      like ar-invl.t-cost.
def var v-cust-part like ar-invl.part-no no-undo.
def var v-ord-no    like ar-invl.ord-no.
def var v-job-no    like job.job-no.
def var v-job-no2   like job.job-no2.
def var v-i-no      like ar-invl.i-no.
DEF VAR v-basis     LIKE sman.commbasis NO-UNDO.
DEF VAR ld-inv-pct  AS   DEC NO-UNDO.
DEF VAR ld-csh-pct  AS   DEC NO-UNDO.
DEF VAR ll-comp     AS   LOG NO-UNDO.
DEF VAR v-print-cost AS  LOG NO-UNDO.
DEF VAR ll-secure    AS  LOG NO-UNDO INIT NO.

def var v-tot-samt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-camt  as   dec format "->>>>>>>9.99" extent 3.
def var v-tot-cost  as   dec format "->>>>>>>9.99" extent 3.

def var v-head      as   character format "x(200)" extent 3.
DEF VAR v-exp-head AS cha FORM "x(132)" NO-UNDO.
DEF VAR v-comma AS cha FORM "x" INIT "," NO-UNDO.
DEF VAR v-part-fg LIKE v-cust-part NO-UNDO.

FORMAT HEADER
       v-head[1] SKIP
       v-head[2] SKIP
       v-head[3]

    WITH FRAME r-top WIDTH 200.




ASSIGN
 str-tit2 = "Commission Report" + " (O-R-6)"
 {sys/inc/ctrtext.i str-tit2 112}
 
 
 v-per-rpt   = rd_ptd EQ "PTD"
 v-period    = begin_period
 v-date[1]   = begin_date
 v-date[2]   = end_date
 v-cat       = fg-cat
 v-sman[1]   = begin_slsmn
 v-sman[2]   = end_slsmn
 v-cust[1]   = begin_cust-no
 v-cust[2]   = end_cust-no
 v-sumdet    = NOT tb_detailed
 v-cost1     = SUBSTR(rd_cost1,1,1)
 v-print-cost = tb_printcost.

/*IF v-print-cost THEN DO: 
  IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).
  v-print-cost = ll-secure. 
END.*/


FOR EACH tt-report:
  DELETE tt-report.
END.
FOR EACH w-comm:
  DELETE w-comm.
END.

IF tb_excel THEN do:
  OUTPUT STREAM st-excell TO VALUE(fi_file).
END.


 if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile .
      
  {sys/inc/outprint.i value(lines-per-page)}

 
{oe/rep/sa-comm.i}


end procedure.
