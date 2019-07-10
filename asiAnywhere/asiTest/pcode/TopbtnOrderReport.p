/*------------------------------------------------------------------------
    File        : TopbtnOrderReport.p
    Purpose     : Order Report

    Syntax      :

    Description : Return a Dataset of all Order Report

    Author(s)   : 
    Created     : july 24 2009
    Notes       : 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTopbtnOrderRep NO-UNDO
    FIELD vFile AS CHAR
    FIELD vvsdsads AS CHAR.
DEFINE DATASET dsTopbtnOrderReport FOR ttTopbtnOrderRep.

    DEFINE INPUT PARAMETER prmUser          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmopenclose     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOrder         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCust          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOrderDate     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFgItem        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmItemName      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPo        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOrderQty      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmProdQty       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmShipQty       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOnHandQty     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSellPrice     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUom           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUnitCost      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPalletCount   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSkids         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmStatus        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmDueDate       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustName      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEst           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmJob           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCad           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmInvoiceQty    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmActRelQty     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmProdBal       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOU            AS CHAR NO-UNDO.

 DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopbtnOrderReport.
 DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser         = ?  THEN ASSIGN prmUser        = "".
    IF prmAction       = ?  THEN ASSIGN prmAction      = "".
    IF prmOut          = ?  THEN ASSIGN prmOut         = "".
    IF prmopenclose    = ?  THEN ASSIGN prmopenclose   = "".
    IF prmBeginCust    = ?  THEN ASSIGN prmBeginCust   = "".
    IF prmEndCust      = ?  THEN ASSIGN prmEndCust     = "".
    IF prmOrder        = ?  THEN ASSIGN prmOrder       = "".
    IF prmCust         = ?  THEN ASSIGN prmCust        = "".
    IF prmFgItem       = ?  THEN ASSIGN prmFgItem      = "".
    IF prmCustPart     = ?  THEN ASSIGN prmCustPart    = "".
    IF prmItemName     = ?  THEN ASSIGN prmItemName    = "".
    IF prmCustPo       = ?  THEN ASSIGN prmCustPo      = "".
    IF prmOrderQty     = ?  THEN ASSIGN prmOrderQty    = "".
    IF prmProdQty      = ?  THEN ASSIGN prmProdQty     = "".
    IF prmShipQty      = ?  THEN ASSIGN prmShipQty     = "".
    IF prmOnHandQty    = ?  THEN ASSIGN prmOnHandQty   = "".
    IF prmSellPrice    = ?  THEN ASSIGN prmSellPrice   = "".
    IF prmUom          = ?  THEN ASSIGN prmUom         = "".
    IF prmUnitCost     = ?  THEN ASSIGN prmUnitCost    = "".
    IF prmPalletCount  = ?  THEN ASSIGN prmPalletCount = "".
    IF prmSkids        = ?  THEN ASSIGN prmSkids       = "".
    IF prmStatus       = ?  THEN ASSIGN prmStatus      = "".
    IF prmCustName     = ?  THEN ASSIGN prmCustName    = "".
    IF prmEst          = ?  THEN ASSIGN prmEst         = "".
    IF prmJob          = ?  THEN ASSIGN prmJob         = "".
    IF prmCad          = ?  THEN ASSIGN prmCad         = "".
    IF prmInvoiceQty   = ?  THEN ASSIGN prmInvoiceQty  = "".
    IF prmActRelQty    = ?  THEN ASSIGN prmActRelQty   = "".
    IF prmProdBal      = ?  THEN ASSIGN prmProdBal     = "".
    IF prmOU           = ?  THEN ASSIGN prmOU          = "".
    IF prmOrderDate    = ?  THEN ASSIGN prmOrderDate   = "".
    IF prmDueDate      = ?  THEN ASSIGN prmDueDate     = "".

   

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

{custom/xprint.i}
{sys/inc/var.i new shared}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 44.

DEF STREAM excel.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-order.csv"  NO-UNDO.
DEFINE VARIABLE RS-open-closed AS INTEGER INITIAL 1 NO-UNDO.
DEFINE VARIABLE tb_excel AS LOGICAL INITIAL NO  NO-UNDO.
DEFINE VARIABLE TG_act-rel-qty AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_cad-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_case-count AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_cust-name AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_cust-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_cust-part-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_cust-po AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_due-date AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_est-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_i-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_invoice-qty AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_item-name AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_job-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_o-u-percent AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_on-hand-qty AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_ord-date AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_ord-no AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_ordered-qty AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_pallet-count AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_prod-qty AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_sell-price AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_shipped-qty AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_skid-count AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_status AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_uom AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_wip-qty AS LOGICAL INITIAL no  NO-UNDO.

DEFINE VAR tmp-dir AS CHAR NO-UNDO.

DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
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
    ASSIGN cError = "Please enter the right customer.....".
    RETURN.
END.


FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmEndCust  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Please enter the right customer.....".
    RETURN.
END.

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

    ASSIGN
        begin_cust-no = prmBeginCust
        end_cust-no =   prmEndCust
        RS-open-closed = INT(prmopenclose)
        TG_act-rel-qty = IF prmActRelQty = "Yes" THEN TRUE ELSE FALSE
        TG_cad-no = IF prmCad = "Yes" THEN TRUE ELSE FALSE
        TG_case-count = IF prmUnitCost = "Yes" THEN TRUE ELSE FALSE
        TG_cust-name = IF prmCustName = "Yes" THEN TRUE ELSE FALSE
        TG_cust-no = IF prmCust = "Yes" THEN TRUE ELSE FALSE
        TG_cust-part-no = IF prmCustPart = "Yes" THEN TRUE ELSE FALSE
        TG_cust-po = IF prmCustPo = "Yes" THEN TRUE ELSE FALSE
        TG_due-date = IF prmDueDate = "Yes" THEN TRUE ELSE FALSE
        TG_est-no =   IF prmEst = "Yes" THEN TRUE ELSE FALSE
        TG_i-no =     IF prmFgItem  = "Yes" THEN TRUE ELSE FALSE
        TG_invoice-qty = IF prmInvoiceQty  = "Yes" THEN TRUE ELSE FALSE
        TG_item-name =  IF prmItemName = "Yes" THEN TRUE ELSE FALSE
        TG_job-no =    IF prmJob = "Yes" THEN TRUE ELSE FALSE
        TG_o-u-percent =   IF prmOU = "Yes" THEN TRUE ELSE FALSE
        TG_on-hand-qty =   IF prmOnHandQty = "Yes" THEN TRUE ELSE FALSE
        TG_ord-date =   IF prmOrderDate = "Yes" THEN TRUE ELSE FALSE
        TG_ord-no =   IF prmOrder = "Yes" THEN TRUE ELSE FALSE
        TG_ordered-qty =   IF prmOrderQty = "Yes" THEN TRUE ELSE FALSE
        TG_pallet-count =   IF prmPalletCount  = "Yes" THEN TRUE ELSE FALSE
        TG_prod-qty =   IF prmProdQty  = "Yes" THEN TRUE ELSE FALSE
        TG_sell-price =   IF prmSellPrice = "Yes" THEN TRUE ELSE FALSE
        TG_shipped-qty =   IF prmShipQty = "Yes" THEN TRUE ELSE FALSE
        TG_skid-count =   IF prmSkids = "Yes" THEN TRUE ELSE FALSE
        TG_status =   IF prmStatus = "Yes" THEN TRUE ELSE FALSE
        TG_uom =   IF prmUom = "Yes" THEN TRUE ELSE FALSE
        TG_wip-qty =   IF prmProdBal  = "Yes" THEN TRUE ELSE FALSE
        .

    assign
 cocode = prmComp
 locode = usercomp.loc
 tb_excel  = IF prmOut = "Yes" THEN TRUE ELSE FALSE 
 v-today    = TODAY   . 
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    
IF prmAction = "Order" THEN DO:
  IF prmOut = "No" THEN DO:
   ASSIGN  
    init-dir    = v-webrootpath
    lv-pdf-file = init-dir + 'Order' 
    lv-pdf-file = lv-pdf-file + prmBeginCust + STRING(TIME)
    vPdfFile   = 'Order' + prmBeginCust + STRING(TIME) + '.pdf'.
    run run-report.

    RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
    
    CREATE ttTopbtnOrderRep.
    ASSIGN ttTopbtnOrderRep.vFile = vPdfFile.
    END.

    IF prmOut = "Yes" THEN DO:

        ASSIGN
              init-dir    = v-webrootpath
              v-excel-file = init-dir + "Order" +
              STRING(YEAR(v-today),"9999")
                + STRING(MONTH(v-today),"99")
                + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

               vPdfFile   = "Order" +
                   STRING(YEAR(v-today),"9999")
                    + STRING(MONTH(v-today),"99")
                    + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
               
               run run-report.
               
               CREATE ttTopbtnOrderRep.
               ASSIGN ttTopbtnOrderRep.vFile = vPdfFile.
    END.

END.









 PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR v-fcust LIKE oe-ord.cust-no EXTENT 2 INIT ["","zzzzzzzz"].

DEF VAR lv-tmp-string AS CHAR NO-UNDO.

DEF VAR v-excelheader AS CHAR NO-UNDO.
DEF VAR v-excel-detail-lines AS CHAR NO-UNDO.

DEF VAR v-prod-qty AS INT NO-UNDO.
DEF VAR v-bal AS INT NO-UNDO.
DEF VAR v-act-rel-qty AS INT NO-UNDO.
DEF VAR v-wip-qty AS INT NO-UNDO.
DEF VAR v-pct     AS INT NO-UNDO.
DEF VAR v-fgitem AS CHAR NO-UNDO.
DEF VAR v-stat AS CHAR NO-UNDO.
DEF VAR v-qoh AS INT NO-UNDO.
DEF VAR v-open-closed AS LOGICAL NO-UNDO.
DEF VAR v-case-count AS DECI NO-UNDO.
DEF VAR v-pallet-count AS DECI NO-UNDO.
DEF VAR v-skid-count   AS DECI NO-UNDO.
DEF VAR v-cnt          AS INT NO-UNDO.

ASSIGN
   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-excelheader = "".

IF TG_ord-no = YES THEN
   v-excelheader = v-excelheader + "Order#,".
IF TG_cust-no = YES THEN
   v-excelheader = v-excelheader + "Customer#,".
IF TG_ord-date = YES THEN
   v-excelheader = v-excelheader + "Order Date,".
IF TG_i-no = YES THEN
   v-excelheader = v-excelheader + "FG Item#,".   
IF TG_cust-part-no = YES THEN
   v-excelheader = v-excelheader + "Cust Part#,".  
IF TG_item-name = YES THEN
   v-excelheader = v-excelheader + "Item Name,".
IF TG_cust-po = YES THEN
   v-excelheader = v-excelheader + "Cust PO#,".  
IF TG_ordered-qty = YES THEN
   v-excelheader = v-excelheader + "Ordered Qty,". 
IF TG_prod-qty = YES THEN
   v-excelheader = v-excelheader + "Prod. Qty,". 
IF TG_shipped-qty = YES THEN
   v-excelheader = v-excelheader + "Shipped Qty,".
IF TG_on-hand-qty = YES THEN
   v-excelheader = v-excelheader + "On Hand Qty,".
IF TG_sell-price = YES THEN
   v-excelheader = v-excelheader + "Sell Price,".
IF TG_uom = YES THEN
   v-excelheader = v-excelheader + "UOM,".
IF TG_case-count = YES THEN
   v-excelheader = v-excelheader + "Unit Count,".
IF TG_pallet-count = YES THEN
   v-excelheader = v-excelheader + "Pallet Count,".
IF TG_skid-count = YES THEN
   v-excelheader = v-excelheader + "Skids,".
IF TG_status = YES THEN
   v-excelheader = v-excelheader + "Status,".
IF TG_due-date = YES THEN
   v-excelheader = v-excelheader + "Due Date,".
IF TG_cust-name = YES THEN
   v-excelheader = v-excelheader + "Customer Name,".
IF TG_est-no = YES THEN
   v-excelheader = v-excelheader + "Est#,".
IF TG_job-no = YES THEN
   v-excelheader = v-excelheader + "Job#,".
IF TG_cad-no = YES THEN
   v-excelheader = v-excelheader + "CAD#,".
IF TG_invoice-qty = YES THEN
   v-excelheader = v-excelheader + "Invoice Qty,".
IF TG_act-rel-qty = YES THEN
   v-excelheader = v-excelheader + "Act. Rel. Quantity,".
IF TG_wip-qty = YES THEN
   v-excelheader = v-excelheader + "Production Balance,".
IF TG_o-u-percent = YES THEN
   v-excelheader = v-excelheader + "O/U%,". 
       


/* {sys/inc/print1.i}                         */
/* {sys/inc/outprint.i value(lines-per-page)} */

IF RS-open-closed = 1 THEN
   v-open-closed = YES.
ELSE
   v-open-closed = NO.
   


IF tb_excel THEN
   OUTPUT STREAM excel TO VALUE(v-excel-file).
    
list-name = init-dir + "tmp" + string(time).
{sys/inc/print1.i}
{sys/inc/outprint.i value(lines-per-page)}
 PUT "<PDF=DIRECT><OLANDSCAPE><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf><CPI13.3><p9>" FORM "x(320)". 


      IF v-excelheader NE "" THEN
         PUT STREAM excel UNFORMATTED v-excelheader SKIP.

   FOR EACH oe-ordl NO-LOCK WHERE oe-ordl.company EQ cocode
                              AND oe-ordl.cust-no >= begin_cust-no 
                              AND oe-ordl.cust-no <= end_cust-no
                              AND oe-ordl.opened EQ v-open-closed
                              AND oe-ordl.stat NE "C",
      FIRST oe-ord OF oe-ordl WHERE oe-ord.stat NE "W"  
                          USE-INDEX ord-no NO-LOCK, 
      FIRST itemfg NO-LOCK WHERE itemfg.company EQ oe-ordl.company 
                             AND itemfg.i-no EQ oe-ordl.i-no:

                             IF LOOKUP(oe-ordl.cust-no, custcount) = 0 THEN NEXT.
      ASSIGN 
         v-prod-qty = 0
         v-bal = 0
         v-act-rel-qty = 0
         v-pct = 0
         v-fgitem = ""
         v-excel-detail-lines = ""
         v-case-count = 0
         v-pallet-count = 0
         v-skid-count = 0
         v-cnt = v-cnt + 1.

        DEF VAR li AS INT NO-UNDO.

      IF AVAIL oe-ordl THEN DO:
         IF oe-ordl.job-no NE "" THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK WHERE fg-rcpth.company   EQ cocode
                                                               AND fg-rcpth.job-no    EQ oe-ordl.job-no
                                                               AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                                                               AND fg-rcpth.i-no      EQ oe-ordl.i-no
                                                               AND fg-rcpth.rita-code EQ "R"
                                                        USE-INDEX job,
               EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                                   AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
               v-prod-qty = v-prod-qty + fg-rdtlh.qty.
            END.
         ELSE DO:
            FOR EACH job-hdr FIELDS(job-no job-no2) WHERE job-hdr.company EQ cocode 
                                                      AND job-hdr.ord-no EQ oe-ordl.ord-no 
                                                      AND job-hdr.i-no EQ oe-ordl.i-no
                                                USE-INDEX ord-no NO-LOCK,
               EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK WHERE fg-rcpth.company   EQ cocode
                                                              AND fg-rcpth.job-no    EQ job-hdr.job-no
                                                              AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                                                              AND fg-rcpth.i-no      EQ oe-ordl.i-no
                                                              AND fg-rcpth.rita-code EQ "R"
                                                        USE-INDEX job,
               EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                                                   AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                v-prod-qty = v-prod-qty + fg-rdtlh.qty.
            END.
         END.
      END.

      IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN
         FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
                                   AND fg-bin.job-no  EQ oe-ordl.job-no
                                   AND fg-bin.job-no2 EQ oe-ordl.job-no2
                                   AND fg-bin.i-no    EQ oe-ordl.i-no:
            v-bal = v-bal + fg-bin.qty.
         END.
      IF AVAIL oe-ordl THEN
         FOR EACH oe-rel WHERE oe-rel.company EQ cocode 
                           AND oe-rel.ord-no  EQ oe-ordl.ord-no 
                           AND oe-rel.i-no    EQ oe-ordl.i-no 
                           AND oe-rel.line    EQ oe-ordl.LINE NO-LOCK:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

            IF INDEX("A,B,P",v-stat) > 0 THEN
               v-act-rel-qty = v-act-rel-qty + oe-rel.qty.
         END.
         
      FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

      v-wip-qty = oe-ordl.qty - (v-bal + oe-ordl.ship-qty).
      IF v-wip-qty LT 0 OR
         v-wip-qty LT oe-ordl.qty * b-oe-ordl.under-pct / 100 THEN
         v-wip-qty = 0.

/*       IF v-cnt < 5 THEN DO:                              */
/*          MESSAGE "oe-ordl.cas-cnt " oe-ordl.cas-cnt SKIP */
/*                  "oe-ordl.qty " oe-ordl.qty              */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.           */
/*                                                          */
/*       END.                                               */
     
      v-case-count   = oe-ordl.cas-cnt.
      IF v-case-count NE 0 THEN
         {sys/inc/roundup.i v-case-count}

      ASSIGN
         v-pallet-count = IF oe-ordl.cases-unit = 0 THEN 1 ELSE oe-ordl.cases-unit
         v-pallet-count = v-pallet-count * v-case-count.
     
      IF v-case-count NE 0 AND oe-ordl.qty NE 0 THEN 
         v-skid-count   = oe-ordl.qty / v-pallet-count.
      IF v-skid-count NE 0 THEN 
          {sys/inc/roundup.i v-skid-count}

      IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
         v-pct = ((v-prod-qty / oe-ordl.qty) - 1) * 100.
         IF v-pct EQ 0 THEN v-pct = 100.
         IF v-pct EQ -100 THEN v-pct = 0.
      END.
      v-fgitem =  oe-ordl.i-no.

      IF TG_ord-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(oe-ordl.ord-no) + '",'.
      IF TG_cust-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + oe-ordl.cust-no + '",'.
      IF TG_ord-date = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(oe-ord.ord-date) + '",'.
      IF TG_i-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + REPLACE(oe-ordl.i-no, '"', '') + '",'.   
      IF TG_cust-part-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + REPLACE(oe-ordl.part-no, '"', '') + '",'.  
      IF TG_item-name = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + REPLACE(oe-ordl.i-name, '"', '') + '",'.
      IF TG_cust-po = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + REPLACE(oe-ordl.po-no, '"', '') + '",'.  
      IF TG_ordered-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(oe-ordl.qty) + '",'. 
      IF TG_prod-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-prod-qty) + '",'. 
      IF TG_shipped-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(oe-ordl.ship-qty) + '",'.
      IF TG_on-hand-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-bal) + '",'.
      IF TG_sell-price = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(asi.oe-ordl.price) + '",'.
      IF TG_uom = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + oe-ordl.pr-uom + '",'.
      IF TG_case-count = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-case-count) + '",'.
      IF TG_pallet-count = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-pallet-count) + '",'.
      IF TG_pallet-count = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-skid-count) + '",'.
      IF TG_status = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + oe-ord.stat + '",'.
      IF TG_due-date = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(oe-ordl.req-date) + '",'.
      IF TG_cust-name = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + oe-ord.cust-name + '",'.
      IF TG_est-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + oe-ordl.est-no + '",'.
      IF TG_job-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99") + '",'.
      IF TG_cad-no = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + itemfg.cad-no + '",'.
      IF TG_invoice-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(oe-ordl.inv-qty) + '",'.
      IF TG_act-rel-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-act-rel-qty)   + '",'.
      IF TG_wip-qty = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-wip-qty) + '",'.
      IF TG_o-u-percent = YES THEN
         v-excel-detail-lines = v-excel-detail-lines + '"' + STRING(v-pct) + '",'. 
    
      PUT STREAM excel UNFORMATTED 
          v-excel-detail-lines
         SKIP.
   END.

IF tb_excel THEN DO:
   OUTPUT STREAM excel CLOSE.
   
END.

/*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).*/

END PROCEDURE.
    



