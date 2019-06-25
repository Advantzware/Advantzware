/*------------------------------------------------------------------------
    File        : SchedRelWOShip.p
    Purpose     :  Print Scheduled Release by Ship-to

    Syntax      :

    Description : Return a Dataset for Scheduled Releases W/Job

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
{sys/inc/var.i new shared}

DEFINE TEMP-TABLE ttSchedRelWOShip NO-UNDO
    FIELD schedrelfile AS CHAR.
DEFINE DATASET dsSchedRelWoShip FOR ttSchedRelWOShip.
    DEFINE INPUT PARAMETER prmUser               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER SchedRel              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginCust            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vEndCust              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginOrder           AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER vEndOrder             AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER vBeginItem            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vEndItem              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginLoc             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vEndLoc               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginSalesMan        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vEndSalesMan          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBeginDate            AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER vEndDate              AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER vBeginCarrier         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vEndCarrier           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vScheduled            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vActual               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vLate                 AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vBackOrder            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPastLastShip         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPosted               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vCompleted            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vInvoice              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPrintComp            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPrintQtyOnHand       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vSubTotal             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPrintLastShipDate    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vSort                 AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPrintMsf             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPrint                AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vRdPrint3             AS CHAR NO-UNDO.   
    DEFINE INPUT PARAMETER vBeginProdCat         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vEndProdCat           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER vPrintDue             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut                AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSchedRelWoShip.
    DEFINE OUTPUT PARAMETER cError               AS CHAR NO-UNDO.    
    
    IF prmUser = ?              THEN ASSIGN          prmUser = "".
    IF vBeginCust = ?           THEN ASSIGN          vBeginCust= "".
    IF SchedRel = ?             THEN ASSIGN          SchedRel = "".
    IF vEndCust = ?             THEN ASSIGN          vEndCust = "".
    IF vBeginOrder = ?          THEN ASSIGN          vBeginOrder = 0.
    IF vEndOrder = ?            THEN ASSIGN          vEndOrder = 0.
    IF vBeginItem = ?           THEN ASSIGN          vBeginItem = "".
    IF vEndItem = ?             THEN ASSIGN          vEndItem = "".
    IF vBeginLoc = ?            THEN ASSIGN          vBeginLoc = "".
    IF vEndLoc = ?              THEN ASSIGN          vEndLoc = "".
    IF vBeginSalesMan = ?       THEN ASSIGN          vBeginSalesMan = "".
    IF vEndSalesMan = ?         THEN ASSIGN          vEndSalesMan = "".
    IF vBeginCarrier = ?        THEN ASSIGN          vBeginCarrier = "".
    IF vEndCarrier = ?          THEN ASSIGN          vEndCarrier = "".
    IF vScheduled = ?           THEN ASSIGN          vScheduled = "".
    IF vActual = ?              THEN ASSIGN          vActual = "".
    IF vLate = ?                THEN ASSIGN          vLate = "".
    IF vBackOrder = ?           THEN ASSIGN          vBackOrder = "".
    IF vPastLastShip = ?        THEN ASSIGN          vPastLastShip = "".
    IF vPosted = ?              THEN ASSIGN          vPosted = "".
    IF vCompleted = ?           THEN ASSIGN          vCompleted = "".
    IF vInvoice = ?             THEN ASSIGN          vInvoice = "".
    IF vPrintComp = ?           THEN ASSIGN          vPrintComp = "".
    IF vPrintQtyOnHand = ?      THEN ASSIGN          vPrintQtyOnHand = "".
    IF vSubTotal = ?            THEN ASSIGN          vSubTotal = "".
    IF vPrintLastShipDate = ?   THEN ASSIGN          vPrintLastShipDate = "".
    IF vSort = ?                THEN ASSIGN          vSort = "".
    IF vPrintMsf = ?            THEN ASSIGN          vPrintMsf = "".
    IF vPrint = ?               THEN ASSIGN          vPrint = "".
    IF vRdPrint3 = ?            THEN ASSIGN          vRdPrint3 = "".    
    IF vBeginProdCat = ?        THEN ASSIGN          vBeginProdCat = "".
    IF vEndProdCat = ?          THEN ASSIGN          vEndProdCat = "".
    IF vPrintDue = ?            THEN ASSIGN          vPrintDue = "". 
    IF prmOut = ?               THEN ASSIGN          prmOut = "".

DEFINE VARIABLE begin_carr      AS CHARACTER FORMAT "X(5)":U  NO-UNDO.
DEFINE VARIABLE begin_cat       AS CHARACTER FORMAT "X(5)":U  NO-UNDO.
DEFINE VARIABLE begin_cust-no   AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE begin_date      AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_i-no      AS CHARACTER FORMAT "X(15)":U NO-UNDO.
DEFINE VARIABLE begin_loc       AS CHARACTER FORMAT "X(5)":U   NO-UNDO.
DEFINE VARIABLE begin_ord-no    AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE begin_slsmn     AS CHARACTER FORMAT "XXX"    NO-UNDO.
DEFINE VARIABLE end_carr        AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz"  NO-UNDO.
DEFINE VARIABLE end_cat         AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz"  NO-UNDO.
DEFINE VARIABLE end_cust-no     AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_date        AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE end_i-no        AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_loc         AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz"  NO-UNDO.
DEFINE VARIABLE end_ord-no      AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_slsmn       AS CHARACTER FORMAT "XXX" INITIAL "zzz"  NO-UNDO.
DEFINE VARIABLE fi_file         AS CHARACTER FORMAT "X(30)" INITIAL "c:\Inetpub\wwwroot\pdfs\ScheduleRelWOShip.csv"  NO-UNDO.
DEFINE VARIABLE lines-per-page  AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
DEFINE VARIABLE rd_print2       AS CHARACTER INITIAL "MSF/Style"   NO-UNDO.
DEFINE VARIABLE rd_print3       AS CHARACTER INITIAL "FG Item#"    NO-UNDO.
DEFINE VARIABLE rd_sort         AS CHARACTER INITIAL "Customer#"   NO-UNDO.
DEFINE VARIABLE rs-item-option  AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_actual       AS LOGICAL INITIAL yes             NO-UNDO.
DEFINE VARIABLE tb_backordered  AS LOGICAL INITIAL yes             NO-UNDO.
DEFINE VARIABLE tb_completed    AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tb_comps        AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tb_excel        AS LOGICAL INITIAL yes             NO-UNDO.
DEFINE VARIABLE tb_invoice      AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tb_invoiceable  AS LOGICAL INITIAL yes             NO-UNDO.
DEFINE VARIABLE tb_late         AS LOGICAL INITIAL yes             NO-UNDO.
DEFINE VARIABLE tb_posted       AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tb_prt-last     AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tb_prt-qoh      AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tb_scheduled    AS LOGICAL INITIAL yes             NO-UNDO.
DEFINE VARIABLE tb_subt         AS LOGICAL INITIAL no              NO-UNDO.
DEFINE VARIABLE tg-print-due    AS LOGICAL INITIAL yes             NO-UNDO. 
DEFINE VARIABLE v-today         AS DATETIME FORMAT "9999/99/99" NO-UNDO.


def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
    

DEF VAR tb_po-no AS LOG NO-UNDO.
DEF VAR rs_qty AS CHAR INIT "TotalQty" NO-UNDO.
DEF VAR tb_stats AS LOG NO-UNDO.
DEF VAR tb_notes AS LOG NO-UNDO.
DEF VAR begin_spec AS CHAR NO-UNDO.
DEF VAR end_spec AS CHAR NO-UNDO.
DEF VAR rd_print AS CHAR NO-UNDO INIT "Item Name".
DEF VAR rd_rel AS CHAR NO-UNDO INIT "Print Release".
DEF VAR tb_show-val AS LOG NO-UNDO.
DEF BUFFER b-itemfg FOR itemfg.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
def {1} SHARED var v-print-fmt  as char NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.

DEF TEMP-TABLE tt-report LIKE report FIELD qty LIKE oe-rell.qty.

DEF STREAM excel.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
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

{oe/rep/schdrel1.i 9}

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = vBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = vEndCust  OR vEndCust = "zzzzzzzz" ) NO-ERROR.
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

IF SchedRel = "ShipWoRel" THEN DO:

    ASSIGN
        v-today         = TODAY   
        begin_carr      = vBeginCarrier 
        begin_cat       = vBeginProdCat
        begin_cust-no   = vBeginCust
        begin_date      = vBeginDate      
        begin_i-no      = vBeginItem 
        begin_loc       = vBeginLoc      
        begin_ord-no    = vBeginOrder 
        begin_slsmn     = vBeginSalesMan      
        end_carr        = vEndCarrier 
        end_cat         = vEndProdCat
        end_cust-no     = vEndCust      
        end_date        = vEndDate 
        end_i-no        = vEndItem      
        end_loc         = vEndLoc 
        end_ord-no      = vEndOrder      
        end_slsmn       = vEndSalesMan 
        rd_print2       = vPrintMsf 
        rd_print3       = vRdPrint3 
        rs-item-option  = vPrint
        rd_sort         = vSort. 
        tb_actual       = IF vActual = "Yes" THEN TRUE ELSE FALSE.
        tb_backordered  = IF vBackOrder = "Yes" THEN TRUE ELSE FALSE.
        tb_completed    = IF vCompleted = "Yes" THEN TRUE ELSE FALSE.
        tb_comps        = IF vPrintComp = "Yes" THEN TRUE ELSE FALSE.
        tb_invoice      = IF vInvoice   = "Yes" THEN TRUE ELSE FALSE.
        tb_invoiceable  = IF vPastLastShip = "Yes"  THEN TRUE ELSE FALSE.     
        tb_late         = IF vLate         = "Yes"  THEN TRUE ELSE FALSE.
        tb_posted       = IF vPosted       = "Yes"  THEN TRUE ELSE FALSE.
        tb_prt-last     = IF vPrintLastShipDate = "Yes"  THEN TRUE ELSE FALSE.
        tb_prt-qoh      = IF vPrintQtyOnHand    = "Yes"  THEN TRUE ELSE FALSE.  
        tb_scheduled    = IF vScheduled         = "Yes"  THEN TRUE ELSE FALSE.
        tb_subt         = IF vSubTotal          = "Yes"  THEN TRUE ELSE FALSE.
        tg-print-due    = IF vPrintDue          = "Yes"  THEN TRUE ELSE FALSE.

        ASSIGN
            tb_excel = IF prmOut = "yes" THEN TRUE ELSE FALSE.                       


    ASSIGN          
        init-dir    = v-webrootpath
        fi_file = init-dir + "ScheduleRelWOShip" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        vPdfFile   = "ScheduleRelWOShip" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".

        DEFINE VAR vTextFile AS CHAR NO-UNDO .
        ASSIGN
            vTextFile = "ScheduleRelWOShip" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt".
              
run run-report.
      
    CREATE ttSchedRelWOShip.
    IF tb_excel THEN
        ASSIGN ttSchedRelWOShip.schedrelfile = vPdfFile.
    IF NOT  tb_excel THEN
        ASSIGN ttSchedRelWOShip.schedrelfile = vTextFile .
    
    
END.
/****************************************************************************************************/
PROCEDURE run-report :

v-comps = tb_comps.

  
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR lv-qty LIKE oe-rell.qty NO-UNDO.
  DEF VAR lv-subt AS CHAR NO-UNDO.
  DEF VAR lv-cr-rating LIKE cust.cr-rating NO-UNDO.
  DEF VAR ll-show-top-only AS LOG NO-UNDO.
  DEF VAR ld-palls AS DEC NO-UNDO.
  DEF VAR excelheader AS CHAR NO-UNDO.

  FORM HEADER
      SKIP(1)

    WITH FRAME r-top.

  FORM HEADER
      "Credit Rating:"
      lv-cr-rating
      SKIP(1)

    WITH FRAME r-top2 PAGE-TOP NO-ATTR-SPACE NO-BOX WIDTH 180 STREAM-IO.

  ASSIGN v-tot-qty = 0
         v-tot-msf = 0
         v-tot-val = 0.  

  assign
   str-tit2 = "Scheduled Releases"
   {sys/inc/ctrtext.i str-tit2 112}

   v-fcust[1]   = begin_cust-no
   v-fcust[2]   = end_cust-no
   v-fsman[1]   = begin_slsmn
   v-fsman[2]   = end_slsmn
   v-ford-no[1] = begin_ord-no
   v-ford-no[2] = end_ord-no
   v-fitem[1]   = begin_i-no
   v-fitem[2]   = end_i-no
   v-floc[1]    = begin_loc
   v-floc[2]    = end_loc
   v-fdate[1]   = begin_date
   v-fdate[2]   = end_date
   v-fcarr[1]   = begin_carr
   v-fcarr[2]   = end_carr
   v-ponum      = tb_po-no
   v-sort       = if rd_sort eq "Customer#"     then "C"  else
                  if rd_sort eq "Release Date"  then "R"  else
                  if rd_sort eq "Item#"         then "I"  else
                  if rd_sort eq "Item Name"     then "N"  else
                  if rd_sort eq "Territory"     then "T"  else
                  if rd_sort eq "Credit Rating" then "CR" else "A"
   v-print      = substr(rd_print,1,1)
   v-qty-opt    = rs_qty
   v-types      = string(tb_posted,"P/")      + string(tb_actual,"A/")      +
                  string(tb_late,"L/")        + string(tb_scheduled,"S/")   +
                  string(tb_backordered,"B/") + string(tb_invoiceable,"I/") +
                  string(tb_completed,"C/")   + string(tb_invoice,"Z/")

   str-tit3 = if v-sort eq "C"  then "By Customer By Date"      else
              if v-sort eq "R"  then "By Date By Customer"      else
              if v-sort eq "I"  then "By Item By Date"          else
              if v-sort eq "N"  then "By Item Name By Date"     else
              if v-sort eq "A"  then "By Carrier By Date"       else
              if v-sort eq "CR" then "By Credit Rating By Date" else
                                     "By Territory By Date"
  {sys/inc/ctrtext.i str-tit3 132}.
   
  /*{sys/inc/print1.i}*/

  if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.

  {sys/inc/outprint.i value(lines-per-page)}    

  IF tb_excel THEN 
  DO:
      IF chosen EQ 2 THEN 
      DO:
          if v-sort eq "N" THEN
          DO: 
              if v-ponum THEN
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,") 
                            + "Item Name,PO Number,Order Number,Rel Num,"
                            + "Item,Order Quantity,Quantity Shipped,Release Quantity,Release Date,Status,Due Alert,Carrier".
              ELSE
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                            + "Item Name,Order Number,Rel Num,"
                            + "Item,Order Quantity,Quantity,Shipped,Release Quantity,Release Date,Status,Due Alert,Carrier".
          END.
          ELSE if v-print eq "I" THEN 
          DO:
              if v-sort eq "T" then 
                 excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                             + "Customer Name,Ship To,Job No,Ter,Del Zone,Order Number,"
                             + "Rel Num,Item,Description,Release Quantity,Release Date,Status,Due Alert,Carrier".
              ELSE IF v-ponum THEN
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                            + "Customer Name,Ship To,PO Number,Order Number,"
                            + "Rel Num,Item,Description,Release Quantity,Release Date,Status,Due Alert,Carrier".
              ELSE
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                            + "Customer Name,Ship To,Order Number,"
                            + "Rel Num,Item,Description,Release Quantity,Release Date,Status,Due Alert,Carrier".
          END.
          ELSE if v-print eq "S" then 
          do:
              if v-sort eq "T" THEN
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                            + "Customer Name,Ship To,Job No,Ter,Del Zone,Order Number,"
                            + "Rel Num,Item,Release Quantity,Release Date,Status,Sales Value,MSF,Due Alert,"
                            + "Carrier".
              ELSE if v-ponum then
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                            + "Customer Name,Ship To,PO Number,Order Number,"
                            + "Rel Num,Item,Release Quantity,Release Date,Status,Sales Value,MSF,Due Alert,"
                            + "Carrier".
              ELSE
                excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                            + "Customer Name,Ship To,Order Number,"
                            + "Rel Num,Item,Release Quantity,Release Date,Status,Sales Value,MSF,Due Alert,"
                            + "Carrier".
          END.
          ELSE if v-sort eq "T" THEN
            excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                        + "Customer Name,Job No,Ter,Del Zone,Order Number,"
                        + "Rel Num,Item,Order Quantity,Quantity Shipped,Release Quantity,Due Alert,"
                        + "Release Date,Status,Due Alert,Carrier".
          ELSE IF v-ponum THEN 
            excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                        + "Customer Name,PO Number,Order Number,"
                        + "Rel Num,Item,Order Quantity,Quantity Shipped,Release Quantity,"
                        + "Release Date,Status,Due Alert,Carrier".
          ELSE
            excelheader = (IF v-qty-opt NE "Job#" THEN "Quantity On Hand," ELSE "Job No,")
                        + "Customer Name,Order Number,"
                        + "Rel Num,Item,Order Quantity,Quantity Shipped,Release Quantity,"
                        + "Release Date,Status,Due Alert,Carrier".
      END.
      ELSE 
      DO:
          IF tb_prt-qoh THEN 
          DO:
              IF tb_prt-last THEN
                excelheader = "Quantity On Hand,Customer Name,Release Date,Status,"
                            + "LastShip Date,PO Number,Job No,Part Number,Item,Description,"
                            + "Release Quantity,"
                            + (IF rd_print2 BEGINS "P" THEN "Pallet Qty" ELSE "MSF,Style").
              ELSE
                excelheader = "Quantity On Hand,Customer Name,Release Date,Status,"
                            + "PO Number,Job No,Part Number,Item,Description,"
                            + "Release Quantity,"
                            + (IF rd_print2 BEGINS "P" THEN "Pallet Qty" ELSE "MSF,Style").
          END.
          ELSE DO:
              IF tb_prt-last THEN
                excelheader = "Customer Name,Release Date,Status,LastShip Date,"
                            + "PO Number,Job No,Part Number,Item,Description,"
                            + "Release Quantity,"
                            + (IF rd_print2 BEGINS "P" THEN "Pallet Qty" ELSE "MSF,Style").
              ELSE
                excelheader = "Customer Name,Release Date,Status,"
                            + "PO Number,Job No,Part Number,Item,Description,"
                            + "Release Quantity,"
                            + (IF rd_print2 BEGINS "P" THEN "Pallet Qty" ELSE "MSF,Style").  
          END.
      END.

    OUTPUT STREAM excel TO VALUE(fi_file).
    PUT STREAM excel UNFORMATTED '"' REPLACE(excelheader,',','","') '"' skip.
  END.

  VIEW FRAME r-top.

  EMPTY TEMP-TABLE tt-report.

  FOR EACH w-ord:
      DELETE w-ord.
  END.

  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ cocode
        AND oe-ordl.opened  EQ YES
        AND oe-ordl.ord-no  GE v-ford-no[1]
        AND oe-ordl.ord-no  LE v-ford-no[2]
        AND oe-ordl.i-no    GE v-fitem[1]
        AND oe-ordl.i-no    LE v-fitem[2]
        AND ((oe-ordl.s-man[1] GE v-fsman[1] AND
              oe-ordl.s-man[1] LE v-fsman[2]) OR
             (oe-ordl.s-man[2] GE v-fsman[1] AND
              oe-ordl.s-man[2] LE v-fsman[2]) OR
             (oe-ordl.s-man[3] GE v-fsman[1] AND
              oe-ordl.s-man[3] LE v-fsman[2]))
        AND NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
                         USE-INDEX ord-no)
      USE-INDEX opened NO-LOCK,
      FIRST b-itemfg WHERE
            b-itemfg.company EQ cocode AND
            b-itemfg.i-no    EQ oe-ordl.i-no AND
            b-itemfg.procat  GE begin_cat AND
            b-itemfg.procat  LE end_cat
            NO-LOCK,
      FIRST oe-ord
      WHERE oe-ord.company EQ oe-ordl.company
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND oe-ord.cust-no GE v-fcust[1]
        AND oe-ord.cust-no LE v-fcust[2]
        AND LOOKUP(oe-ord.cust-no, custcount) <> 0
      NO-LOCK,
      
      FIRST cust
      WHERE cust.company EQ oe-ord.company
        AND cust.cust-no EQ oe-ord.cust-no
      NO-LOCK:    

    RUN oe/cleanrel.p (ROWID(oe-ordl)).

    for each oe-rel no-lock
        where oe-rel.company   eq oe-ordl.company
          and oe-rel.ord-no    eq oe-ordl.ord-no
          and oe-rel.i-no      eq oe-ordl.i-no
          and oe-rel.line      eq oe-ordl.line
          and oe-rel.rel-date  ge v-fdate[1]
          and oe-rel.rel-date  le v-fdate[2]
          and oe-rel.carrier   ge v-fcarr[1]
          and oe-rel.carrier   le v-fcarr[2]
        use-index ord-item:
      
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).

      if index("AB",v-type) gt 0 then next.
    
      if index(v-types,v-type) gt 0 then do:
        create tt-report.
        assign
         tt-report.term-id = ""
         tt-report.key-01  = if v-sort eq "R" then
                               (string(year(oe-rel.rel-date),"9999") +
                                string(month(oe-rel.rel-date),"99")  +
                                string(day(oe-rel.rel-date),"99"))
                             else
                             if v-sort eq "N" then oe-ordl.i-name
                             else ""
         tt-report.key-02  = if v-sort eq "I" or v-sort eq "D" then oe-rel.i-no
                             else
                             if v-sort eq "T" then cust.terr
                             else
                             if v-sort eq "A" then oe-rel.carrier
                             else
                             if v-sort eq "CR" then cust.cr-rating
                             else oe-rel.cust-no
         tt-report.key-03  = if v-sort ne "R" then
                               (string(year(oe-rel.rel-date),"9999") +
                                string(month(oe-rel.rel-date),"99")  +
                                string(day(oe-rel.rel-date),"99"))
                             else ""
         tt-report.key-04  = string(if v-sort eq "A" then oe-rel.cust-no
                                                     else " ","x(10)") +
                             string(oe-ord.ord-no,"9999999999")
         tt-report.key-05  = string(index(v-types,v-type),"99")
         tt-report.key-06  = v-type
         tt-report.rec-id  = recid(oe-rel).
      end.
    end.
    
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-ordl.company
          AND oe-rell.ord-no  EQ oe-ordl.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line
          AND ((oe-rell.b-ord-no NE 0 AND INDEX(v-types,"B") GT 0) OR
               (oe-rell.b-ord-no EQ 0 AND INDEX(v-types,"A") GT 0))
        USE-INDEX ord-no,

        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no     EQ oe-rell.r-no
          AND oe-relh.posted   EQ NO
          AND oe-relh.deleted  EQ NO
          AND oe-relh.rel-date GE v-fdate[1]
          AND oe-relh.rel-date LE v-fdate[2]
          AND oe-relh.carrier  GE v-fcarr[1]
          AND oe-relh.carrier  LE v-fcarr[2]
        USE-INDEX r-no

      BREAK BY oe-rell.r-no
            BY oe-rell.ord-no
            BY oe-rell.i-no
            BY oe-rell.line
            BY oe-rell.rel-no
            BY oe-rell.b-ord-no
            BY oe-rell.po-no:

    IF FIRST-OF(oe-rell.po-no) THEN lv-qty = 0.

    lv-qty = lv-qty + oe-rell.qty.

    IF LAST-OF(oe-rell.po-no) THEN DO:
      create tt-report.
      assign
       tt-report.term-id = ""
       tt-report.key-01  = if v-sort eq "R" then
                             (string(year(oe-relh.rel-date),"9999") +
                              string(month(oe-relh.rel-date),"99")  +
                              string(day(oe-relh.rel-date),"99"))
                           else
                           if v-sort eq "N" then oe-ordl.i-name
                           else ""
       tt-report.key-02  = if v-sort eq "I" or v-sort eq "D" then oe-rell.i-no
                           else
                           if v-sort eq "T" then cust.terr
                           else
                           if v-sort eq "A" then oe-relh.carrier
                           else
                           if v-sort eq "CR" then cust.cr-rating
                           else oe-relh.cust-no
       tt-report.key-03  = if v-sort ne "R" then
                             (string(year(oe-relh.rel-date),"9999") +
                              string(month(oe-relh.rel-date),"99")  +
                              string(day(oe-relh.rel-date),"99"))
                           else ""
       tt-report.key-04  = string(if v-sort eq "A" then oe-relh.cust-no
                                                   else " ","x(10)") +
                           string(oe-ord.ord-no,"9999999999")
       tt-report.key-05  = string(index(v-types,v-type),"99")
       tt-report.key-06  = if oe-rell.b-ord-no eq 0 then "A" else "B"
       tt-report.qty     = lv-qty
       tt-report.rec-id  = recid(oe-rell).
    END.

    /* new start */
    END.
    /* new end */
  end.

  IF NOT CAN-FIND(FIRST tt-report WHERE tt-report.term-id EQ "") THEN DO:
    CREATE tt-report.
    ASSIGN
     tt-report.term-id = ""
     ll-show-top-only  = YES.
  END.

  RELEASE tt-report.

  for each tt-report where tt-report.term-id eq ""
      break by tt-report.key-01
            by tt-report.key-02
            by tt-report.key-03
            by tt-report.key-04:

    IF v-sort EQ "CR" AND FIRST-OF(tt-report.key-02) THEN DO:
      lv-cr-rating = tt-report.key-02.
      IF FIRST(tt-report.key-02) THEN VIEW FRAME r-top2.
      PAGE.
    END.

    ELSE
      IF FIRST(tt-report.key-01) THEN PAGE.

    release oe-rel.
    release oe-rell.
    release oe-relh.
    release oe-ord.
    release oe-ordl.

    find first oe-rel 
        where recid(oe-rel) eq tt-report.rec-id 
        no-lock no-error.

    v-rel-no = 0.

    if avail oe-rel then do:
      FOR EACH oe-rell NO-LOCK
          WHERE oe-rell.company  EQ cocode
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
          USE-INDEX ord-no,
          FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:

        v-rel-no = oe-relh.release#.

        IF oe-relh.posted EQ NO AND oe-relh.deleted EQ NO THEN
          tt-report.rec-id = recid(oe-rell).
        ELSE RELEASE oe-relh.

        LEAVE.
      END.
    
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rel.ord-no
            and oe-ordl.i-no    eq oe-rel.i-no
            and oe-ordl.line    eq oe-rel.line
          no-lock.
    end.
    
    find oe-rell where recid(oe-rell) eq tt-report.rec-id no-lock no-error.
    if avail oe-rell then do:    
      if index("SLI",tt-report.key-06) gt 0 then
        tt-report.key-06 = if oe-rell.b-ord-no eq 0 then "A" else "B" .

      find first oe-relh
          where oe-relh.company eq cocode
            and oe-relh.r-no    eq oe-rell.r-no
          use-index r-no no-lock.
      v-rel-no = IF AVAIL oe-relh THEN oe-relh.release# ELSE v-rel-no.

      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq oe-rell.ord-no
            and oe-ordl.i-no    eq oe-rell.i-no
            and oe-ordl.line    eq oe-rell.line
          no-lock.
    end.

    find first oe-ord of oe-ordl no-lock no-error.
    
    if avail oe-ord then
    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-ord.cust-no
        no-lock no-error.

    if avail oe-relh then
      assign
       v-qty     = IF tt-report.qty NE 0 THEN tt-report.qty ELSE oe-rell.qty
       v-date    = oe-relh.rel-date 
       v-po-no   = oe-rell.po-no
       v-rel-no  = IF rd_rel begins "N" OR v-rel-no EQ 0 THEN oe-rell.rel-no
                                                         ELSE v-rel-no
       v-ship-id = oe-relh.ship-id
       v-carrier = oe-relh.carrier.
    else
    if avail oe-rel then
      assign
       v-qty     = oe-rel.qty 
       v-date    = oe-rel.rel-date
       v-po-no   = oe-rel.po-no
       v-rel-no  = IF rd_rel BEGINS "N" OR v-rel-no EQ 0 THEN oe-rel.rel-no
                                                         ELSE v-rel-no
       v-ship-id = oe-rel.ship-id
       v-carrier = oe-rel.carrier.

    create w-ord.

    if avail oe-ordl then do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq oe-ordl.i-no
          no-lock.

      assign
       w-ord.ord-no        = oe-ord.ord-no
       w-ord.cust-no       = oe-ord.cust-no
       w-ord.cust-name     = oe-ord.cust-name
       w-ord.part-no       = oe-ordl.part-no
       w-ord.i-no          = oe-ordl.i-no
       w-ord.i-name        = oe-ordl.i-name
       w-ord.qty           = oe-ordl.qty
       w-ord.cost          = oe-ordl.cost
       w-ord.price         = oe-ordl.t-price / oe-ordl.qty
       w-ord.rel-qty       = v-qty
       w-ord.t-price       = w-ord.price * w-ord.rel-qty
       w-ord.rel-date      = string(v-date) + tt-report.key-06
       w-ord.xls-rel-date  = v-date
       w-ord.xls-status    = tt-report.key-06
       w-ord.rel-no        = v-rel-no
       w-ord.ship-id       = v-ship-id
       w-ord.job-no        = oe-ordl.job-no
       w-ord.job-no2       = oe-ordl.job-no2
       w-ord.job           = if w-ord.job-no eq "" then "" else
                               (trim(w-ord.job-no) + "-" +
                                string(w-ord.job-no2,"99"))
       w-ord.po-num        = v-po-no
       w-ord.ord-qty       = oe-ordl.qty
       w-ord.shp-qty       = oe-ordl.ship-qty
       w-ord.msf           = w-ord.rel-qty * itemfg.t-sqft / 1000
       w-ord.prom-code     = oe-ordl.prom-code
       w-ord.last-date     = oe-ord.last-date
       w-ord.carrier       = v-carrier
       w-ord.is-a-component = oe-ordl.is-a-component
       ld-palls            = w-ord.rel-qty /
                              ((IF oe-ordl.cas-cnt    EQ 0 THEN 1 ELSE oe-ordl.cas-cnt) *
                              (IF oe-ordl.cases-unit EQ 0 THEN 1 ELSE oe-ordl.cases-unit)).

      {sys/inc/roundup.i ld-palls}

      IF ld-palls LT 0 THEN ld-palls = ld-palls * -1.

      w-ord.palls = w-ord.palls + ld-palls.

      IF NOT FIRST-OF(tt-report.key-02) AND v-sort EQ "C" THEN w-ord.cust-name = "".

      IF v-comps AND itemfg.isaset THEN DO:
        RUN fg/fullset.p (ROWID(itemfg)).

        FOR EACH tt-fg-set,
            FIRST itemfg
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ tt-fg-set.part-no
            NO-LOCK:

          CREATE b-w-ord.
          BUFFER-COPY w-ord TO b-w-ord
          ASSIGN
           b-w-ord.component = 1
           b-w-ord.cust-name = ""
           b-w-ord.part-no   = itemfg.part-no
           b-w-ord.i-no      = tt-fg-set.part-no
           b-w-ord.i-name    = itemfg.i-name
           b-w-ord.price     = 0
           b-w-ord.cost      = 0
           b-w-ord.t-price   = 0
           b-w-ord.job       = ""
           b-w-ord.po-num    = ""
           b-w-ord.qty       = w-ord.qty     * tt-fg-set.part-qty-dec
           b-w-ord.rel-qty   = w-ord.rel-qty * tt-fg-set.part-qty-dec
           b-w-ord.ord-qty   = w-ord.ord-qty * tt-fg-set.part-qty-dec
           b-w-ord.shp-qty   = w-ord.shp-qty * tt-fg-set.part-qty-dec
           b-w-ord.msf       = b-w-ord.rel-qty * itemfg.t-sqft / 1000.
        END.
      END.
    END.

    FOR EACH w-ord
        BREAK BY w-ord.component
              BY w-ord.i-no:

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-ord.i-no
          NO-LOCK NO-ERROR.

      IF AVAIL itemfg OR ll-show-top-only THEN DO:
        if v-qty-opt EQ "JobQty" and w-ord.component eq 0 then
        for each fg-bin FIELDS(qty)
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq oe-ordl.i-no
              and fg-bin.job-no  eq oe-ordl.job-no
              and fg-bin.job-no2 eq oe-ordl.job-no2
              AND fg-bin.loc     GE v-floc[1]
              AND fg-bin.loc     LE v-floc[2]
            use-index job no-lock:
          w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
        end.
        else /* w-ord.onh-qty = itemfg.q-onh. */
        for each fg-bin FIELDS(qty)
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq oe-ordl.i-no
              AND fg-bin.loc     GE v-floc[1]
              AND fg-bin.loc     LE v-floc[2]
            use-index i-no no-lock:
          w-ord.onh-qty = w-ord.onh-qty + fg-bin.qty.
        end.
        
        {oe/rep/schdrel.i}
      END.
    END.

    IF NOT ll-show-top-only THEN DO:
      FIND FIRST w-ord.

      assign
       v-tot-qty[1] = v-tot-qty[1] + 1
       v-tot-msf[1] = v-tot-msf[1] + w-ord.msf
       v-tot-val[1] = v-tot-val[1] + w-ord.t-price.

      IF LAST-OF(tt-report.key-02) THEN DO:
        IF v-sort EQ "CR" OR (tb_subt AND v-sort EQ "C") THEN DO:
          lv-subt = CAPS(rd_sort).
          IF v-print EQ "S" THEN
            PUT SKIP(1)
                FILL(" ",26 - LENGTH(TRIM(lv-subt) + " SALES VALUE:")) +
                    TRIM(lv-subt) + " SALES VALUE: " FORMAT "x(27)" TO 115
                v-tot-val[1] TO 130.

          IF chosen NE 2 OR v-print EQ "S" THEN
            PUT SKIP(1)
                FILL(" ",18 - LENGTH(TRIM(lv-subt) + " MSF:")) +
                TRIM(lv-subt) + " MSF: " FORMAT "x(19)" TO 115
                v-tot-msf[1] TO 130.

          PUT SKIP(1)
              FILL(" ",35 - LENGTH(TRIM(lv-subt) + " NUMBER OF RELEASES:")) +
                  TRIM(lv-subt) + " NUMBER OF RELEASES: " FORMAT "x(36)" TO 115
              v-tot-qty[1] TO 130
              SKIP(1).
        END.

        assign
         v-tot-qty[2] = v-tot-qty[2] + v-tot-qty[1]
         v-tot-val[2] = v-tot-val[2] + v-tot-val[1]
         v-tot-msf[2] = v-tot-msf[2] + v-tot-msf[1]
         v-tot-qty[1] = 0
         v-tot-val[1] = 0
         v-tot-msf[1] = 0.
      end.

      if last(tt-report.key-01) then do:
        if v-print eq "S" then
          put skip(1)
              "TOTAL SALES VALUE: " to 115
              v-tot-val[2] to 130.

        if chosen ne 2 or v-print eq "S" then
          put skip(1)
              "TOTAL MSF: " to 115
              v-tot-msf[2] to 130.

        put skip(1)
            "TOTAL NUMBER OF RELEASES: " to 115
            v-tot-qty[2] to 130
            skip(1).
      end.
    END.

    FOR EACH w-ord:
      DELETE w-ord.
    END.
  end. /* each tt-report */

  
end procedure.

