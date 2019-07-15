/*------------------------------------------------------------------------
    File        : backlogpartRep.p
    Purpose     :  back Log Part Report

    Syntax      :

    Description : Return a Dataset For BackLog Part Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
/*{sys/inc/var.i new shared}*/

DEFINE TEMP-TABLE ttBackLogPartRep NO-UNDO
FIELD vBackLog AS CHAR .
DEFINE DATASET dsBackLogPartRep FOR ttBackLogPartRep .
    DEFINE INPUT PARAMETER prmUser           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBackLog        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeCust        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndCust      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeSalesman        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndSalesman     AS CHARACTER NO-UNDO.
    
    DEFINE INPUT PARAMETER prmBeOrder      AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrder      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBeItem        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmEndItem      AS CHARACTER      NO-UNDO.
    DEFINE INPUT PARAMETER prmBemDue          AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER pemEndDue       AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintpo      AS CHARACTER      NO-UNDO.
    DEFINE INPUT PARAMETER prmIncJob      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmItemSub        AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmExcludeCo      AS CHARACTER      NO-UNDO.
    DEFINE INPUT PARAMETER prmPriceSale          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pemExcludeTr    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmSort         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pemQtyOn        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER cError         AS CHAR NO-UNDO .

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBackLogPartRep.

            IF prmUser      = ?  THEN ASSIGN      prmUser      = "".
            IF prmBackLog   = ?  THEN ASSIGN      prmBackLog   = "".
            IF prmBeCust    = ?  THEN ASSIGN      prmBeCust    = "".
            IF prmEndCust    = ?  THEN ASSIGN     prmEndCust    = "". 
            IF prmBeSalesman = ?  THEN ASSIGN     prmBeSalesman = "". 
            IF prmEndSalesman = ?  THEN ASSIGN     prmEndSalesman = "". 
            IF prmBeOrder     = ?  THEN ASSIGN    prmBeOrder   = 0.  
            IF prmEndOrder    = ?  THEN ASSIGN    prmEndOrder  = 0.  
            IF prmBeItem      = ?  THEN ASSIGN    prmBeItem    = "".  
            IF prmEndItem      = ? THEN ASSIGN    prmEndItem   = "". 
            IF prmPrintpo     = ?  THEN ASSIGN    prmPrintpo = "".  
            IF prmIncJob      = ?  THEN ASSIGN    prmIncJob  = "".  
            IF prmItemSub     = ?  THEN ASSIGN    prmItemSub = "".  
            IF prmExcludeCo   = ?  THEN ASSIGN    prmExcludeCo = "". 
            IF prmPriceSale   = ?  THEN ASSIGN    prmPriceSale = "". 
            IF pemExcludeTr   = ?  THEN ASSIGN    pemExcludeTr = "". 
            IF prmSort        = ?  THEN ASSIGN    prmSort      = "". 
            IF pemQtyOn       = ?  THEN ASSIGN    pemQtyOn     = "". 


DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)"  NO-UNDO.
DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE begin_slsmn AS CHARACTER FORMAT "XXX"  NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999"  NO-UNDO.
DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)" INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_slsmn AS CHARACTER FORMAT "XXX" INITIAL "zzz"  NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "C:\Inetpub\wwwroot\pdfs\BackLogPart.csv"  NO-UNDO.
DEFINE VARIABLE lbl_qoh AS CHARACTER FORMAT "X(256)" INITIAL "Qty On-hand Source?"  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 99  NO-UNDO.
DEFINE VARIABLE rd_qoh AS CHARACTER INITIAL "Job"  NO-UNDO.
DEFINE VARIABLE rs_sort AS CHARACTER INITIAL "Item"  NO-UNDO.
DEFINE VARIABLE tb_comp AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_exclude-transfer AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_jobs AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_po-no AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_price AS LOGICAL INITIAL yes NO-UNDO.
DEFINE VARIABLE tb_runExcel AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_subt AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.

def workfile w-ord
  field ord-no as char format "x(9)"
  field est-no like oe-ord.est-no
  field ship-date like oe-ordl.req-date format "99/99/99"
  field due-date like oe-ordl.prom-date format "99/99/99"
  field ord-date like oe-ord.ord-date
  field cust-no like oe-ord.cust-no
  field cust-name like oe-ord.cust-name
  field i-no like oe-ordl.i-no
  field i-name like oe-ordl.i-name
  field part-no like oe-ordl.part-no
  field qty-due as int format "->>>,>>>,>>9"
  field qty-onh like itemfg.q-onh
  field qty like oe-ordl.qty
  field cost like oe-ordl.cost
  field price like oe-ordl.price format ">>>,>>>9.99"
  field uom like oe-ordl.pr-uom
  field disc like oe-ordl.disc
  field t-price like oe-ordl.t-price format ">>>>,>>>9.99"
  field sman as char format "x(11)"
  field po-num like oe-ordl.po-no
  field job-no like oe-ordl.job-no
  field job-no2 like oe-ordl.job-no2
  field stat like oe-ordl.prom-code format "x(4)"
  field pallets as dec format "->>>,>>>,>>9"
  field rel-date like oe-relh.rel-date
  field rel-stat as char
  FIELD po-received AS INT FORMAT "->>>,>>>,>>>".                          .
  

DEFINE NEW SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHAR NO-UNDO.
def var list-name as cha no-undo.
DEF VAR v-program AS CHAR NO-UNDO.

def var security-flag as log no-undo.

DEF TEMP-TABLE tt-report LIKE report.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF STREAM excel.
DEF VAR v-only-transfer-release AS LOG NO-UNDO.
DEFINE NEW SHARED VARIABLE g_company AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
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

    ASSIGN
        g_company = cocode
        g_loc     = locode .



FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeCust NO-ERROR.
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

IF prmBackLog = "BackLogPart"  THEN  DO:
    ASSIGN
        v-today = TODAY
        cocode = prmComp
        locode = usercomp.loc
        begin_cust-no  = prmBeCust
        begin_due-date = prmBemDue
        begin_i-no     = prmBeItem
        begin_ord-no   = prmBeOrder
        begin_slsmn    = prmBeSalesman
        end_cust-no    = prmEndCust
        end_due-date   = pemEndDue
        end_i-no       = prmEndItem
        end_ord-no     = prmEndOrder
        end_slsmn      = prmEndSalesman
        rs_sort        =  prmSort
        rd_qoh         =  pemQtyOn  
        tb_comp        = IF  prmExcludeCo  = "Yes" THEN TRUE ELSE FALSE
        tb_exclude-transfer = IF pemExcludeTr = "Yes"   THEN  TRUE ELSE FALSE
        tb_jobs = IF  prmIncJob = "Yes" THEN TRUE ELSE FALSE 
        tb_po-no = IF  prmPrintpo = "Yes" THEN TRUE ELSE FALSE
        tb_price = IF prmPriceSale = "Yes" THEN TRUE ELSE FALSE
        tb_subt = IF prmItemSub  = "Yes"   THEN TRUE ELSE FALSE
        tb_excel = IF prmOut = "Yes" THEN TRUE ELSE FALSE.
        

    ASSIGN 
   /* vPdfFile   = 'BackLogPart' + '.csv'.*/
      init-dir    = v-webrootpath
        fi_file = init-dir + "BackLogPart" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "BackLogPart" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".       
         
     DEFINE VAR vTextFile AS CHAR NO-UNDO .
       ASSIGN
           vTextFile = "BackLogPart" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".


    run run-report.
    
    CREATE ttBackLogPartRep.
     IF tb_excel  THEN
         ASSIGN ttBackLogPartRep.vBackLog = vPdfFile.
      IF NOT tb_excel  THEN
          ASSIGN ttBackLogPartRep.vBackLog = vTextFile .
END.




PROCEDURE run-report :
/* ---------------------------------------------- oe/rep/backlog2.p 03/01 JLF */
/* Order Backlog Summary by Customer Part                                     */
/* -------------------------------------------------------------------------- */

{sys/form/r-top3w.f}

def buffer xoe-ord for oe-ord.

def var v-fcust like oe-ord.cust-no extent 2 init ["","zzzzzzzz"].
def var v-fslsm like oe-ord.sman extent 2 init ["","zzz"].
def var v-ford-no as int format ">>>>>>" extent 2 init [0,999999].
def var v-fdate as date format "99/99/9999" extent 2 init [01/01/01,today].
def var v-fitem as char format "x(15)" extent 2 init ["","zzzzzzzzzzzzzzz"].
def var v-ponum as log init yes.
DEF VAR v-sort AS CHAR NO-UNDO.
def var v-sub-item as log init no.
def var v-priceflag as log init yes.
def var v-jobs as log init no.
def var v-fg-qty as log format "FG/Job" init no.
def var v-profit as log init yes.

def var v-qty as int extent 2.
def var v-cost as dec.
def var v-tot-qty as int format "->>>,>>>,>>9" extent 3.
def var v-tot-cost as dec format  "->>>,>>>,>>9.99" extent 3.
def var v-tot-sales as dec format "->>>,>>>,>>9.99" extent 3.
def var v-tot-pct as dec format "->>,>>9.99".
def var v-head as char format "x(132)" extent 2.
def var v-gpdollar as dec format  "->>>,>>>,>>9.99".
def var v-gp as dec.
def var v-uom as char format "x(4)".
def var fstat like oe-ord.stat.
def var tstat like fstat init "".
def var v-qty-pal as int.
def var v-get-qty as log.
def var v-name like cust.name.
DEF VAR li-qty AS INT EXTENT 2 NO-UNDO.
DEF VAR excelheader AS CHAR NO-UNDO.    
DEF VAR po-num-excel AS CHAR NO-UNDO.
DEF VAR lv-total-label AS CHAR NO-UNDO.
DEF VAR lv-top-label AS CHAR INIT "Customer:" NO-UNDO.
DEF VAR v-exclude-transfers AS LOG NO-UNDO.
DEFINE VARIABLE v-code AS CHARACTER  NO-UNDO.

IF rs_sort EQ "Sales" THEN
   lv-top-label = "Salesrep:".

format header
       skip(1)
       lv-top-label FORMAT "X(9)"
       v-name
            
    with frame r-top-1 stream-io width 132 no-labels
         no-box no-underline page-top.

format header
       skip(1)
       v-head[1] skip
       v-head[2]
       fill("-",132) format "x(132)"
       
    with frame r-top-2 stream-io width 132 no-labels
         no-box no-underline page-top.
         
format w-ord.due-date     format "99/99/99"
       w-ord.ord-date     format "99/99/99"
       w-ord.ord-no
       w-ord.po-num
       w-ord.part-no
       w-ord.pallets
       w-ord.qty-onh      format "->,>>>,>>9.9<<<"
       w-ord.qty-due
       w-ord.price        format ">>>,>>>9.99<<<"
       space(0)
       v-uom
       w-ord.t-price      format ">>,>>>,>>9.99"
       
    with frame ordhead-po down no-labels no-box stream-io width 132.

format w-ord.due-date     format "99/99/99"
       w-ord.ord-date     format "99/99/99"
       w-ord.ord-no
       w-ord.part-no
       w-ord.pallets
       w-ord.qty-onh      format "->,>>>,>>9.9<<<"
       w-ord.qty-due
       w-ord.price        format ">>>,>>>9.99<<<"
       space(0)
       v-uom
       w-ord.t-price      format ">>,>>>,>>9.99"  
       
    with frame ordhead down no-labels no-box stream-io width 132.


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
 str-tit2 = "Order BackLog By Part#"
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
 v-sort       = rs_sort
 v-sub-item   = tb_subt
 v-priceflag  = tb_price
 v-jobs       = tb_jobs
 v-fg-qty     = rd_qoh eq "FG"
 v-exclude-transfers = tb_exclude-transfer 
 
 str-tit3 = (if v-sort = "Cust" then "By Customer# "
             else IF v-sort = "Sales" THEN "By Salesrep# " ELSE "") +
            "By Customer Part Number"
 {sys/inc/ctrtext.i str-tit3 132}
 
 v-tot-qty   = 0
 v-tot-cost  = 0
 v-tot-sales = 0
 lv-total-label = IF v-sort EQ "Item" THEN ""
                  ELSE IF v-sort EQ "Cust" THEN "   CUST TOTALS:"
                  ELSE "SALESREP TOTALS".

/*IF v-priceflag AND NOT security-flag THEN DO:
  RUN passwd (3, OUTPUT security-flag).
  v-priceflag = security-flag.
END.*/
    ASSIGN
        security-flag = v-priceflag .
                       
if v-ponum then
  assign
   v-head[1] =
       "Due      Order    Order     PO              Customer               " +
       "               Qty          Qty                             "
   v-head[2] =
       "Date     Date     Number    Number          Part Number          Pa" +
       "llets      On-hand          Due       Price            Sales".

else
  assign
   v-head[1] =
       "Due      Order    Order     Customer               " +
       "               Qty          Qty                             "
   v-head[2] =
       "Date     Date     Number    Part Number          Pa" +
       "llets      On-hand          Due       Price            Sales".
  


 if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + vTextFile .
      
  {sys/inc/outprint.i value(lines-per-page)}

 
IF tb_excel THEN DO:
  OUTPUT STREAM excel TO VALUE(fi_file).
  excelheader = "Due Date,Order Date,Order Number,".
  IF v-ponum THEN
    excelheader = excelheader + "PO Number,".
  excelheader = excelheader + "Customer Part Number,"
              + "Pallets,Qty On-hand,Qty Due,Price,Sales".
  
  PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' SKIP.
END.



FOR EACH tt-report:
  DELETE tt-report.
END.

  display with frame r-top.
  
  if v-sort EQ "Item" then display with frame r-top-2.
  
  {oerep/r-backl1.i}
                                                                  

end procedure.                                                    

                                                                  

/**********************************************************************************************************/
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
