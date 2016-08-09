&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaOE.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cocode AS CHARACTER NO-UNDO.

/* BOL Packing List.rpa */
DEFINE TEMP-TABLE ttBOLPackingList NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD bolNo      AS INTEGER   LABEL "BOL No"       FORMAT ">>>>>>>9"
    FIELD shipDate   AS DATE      LABEL "Shipped"      FORMAT "99/99/9999"
    FIELD pickTicket AS CHARACTER LABEL "Pick Ticket"  FORMAT "x(8)"
    FIELD custName   AS CHARACTER LABEL "Customer"     FORMAT "x(30)"
    FIELD custNo     AS CHARACTER LABEL "Cust No"      FORMAT "x(8)"
    FIELD itemNo     AS CHARACTER LABEL "Item ID"      FORMAT "x(15)"
    FIELD poNo       AS INTEGER   LABEL "PO No"        FORMAT ">>>>>9"
    FIELD relNo      AS INTEGER   LABEL "Release No"   FORMAT ">>>>>9"
    FIELD orderNo    AS INTEGER   LABEL "Order No"     FORMAT ">>>>>9"
    FIELD jobNo      AS CHARACTER LABEL "Job No"       FORMAT "x(6)"
    FIELD ticket     AS CHARACTER LABEL "Ticket"       FORMAT "x(8)"
    FIELD tagDate    AS DATE      LABEL "Prod Date"    FORMAT "99/99/9999"
    FIELD qtyCase    AS INTEGER   LABEL "Cases"        FORMAT "->>>,>>9"
    FIELD caseBundle AS INTEGER   LABEL "Cartons/Case" FORMAT ">>>,>>9"
    FIELD partial    AS DECIMAL   LABEL "Partials"     FORMAT ">>>,>>9"
    FIELD weight     AS INTEGER   LABEL "Wgt/Case"     FORMAT ">>>>9"
    FIELD prntr      AS CHARACTER LABEL "Printer"      FORMAT "x(8)"
    FIELD xxSort     AS CHARACTER LABEL "Sort"         FORMAT "x(50)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
/* BOL Packing List.rpa */

/* Orders Booked.rpa */
DEFINE TEMP-TABLE ttOrdersBooked NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD dueDate      AS DATE      LABEL "Due Date"       FORMAT 99/99/9999
    FIELD orderNo      AS INTEGER   LABEL "Order No"       FORMAT ">>>>>>>"
    FIELD custNo       AS CHARACTER LABEL "Cust No"        FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD salesRep     AS CHARACTER LABEL "Sales Rep"      FORMAT "X(3)"
    FIELD salesRepName AS CHARACTER LABEL "Sales Name"     FORMAT "X(30)"
    FIELD commPer      AS DECIMAL   LABEL "Comm "          FORMAT ">>>>>9.99"
    FIELD prodCode     AS CHARACTER LABEL "Prod Code"      FORMAT "x(8)"
    FIELD fgItemNo     AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD fgItemName   AS CHARACTER LABEL "FG Item Name"   FORMAT "X(30)"
    FIELD qtyOrdEa     AS INTEGER   LABEL "Qty Ordered EA" FORMAT ">,>>>,>>>"
    FIELD sqFit        AS DECIMAL   LABEL "Sq Ft"          FORMAT ">>,>>>.999"
    FIELD totalSqfit   AS DECIMAL   LABEL "Total Sq Ft M"  FORMAT "->,>>>.999"
    FIELD msfPrice     AS DECIMAL   LABEL "Msf"            FORMAT "->>,>>9.99"
    FIELD price        AS DECIMAL   LABEL "Price"          FORMAT ">>>,>>9.99<<<<"
    FIELD orderAmount  AS DECIMAL   LABEL "Order Amount"   FORMAT "->,>>>,>>9.99"
    FIELD profitPer    AS DECIMAL   LABEL "Profit"         FORMAT "->>,>>9.9"
    FIELD totalTons    AS DECIMAL   LABEL "Total Tons"     FORMAT "->,>>>.9"
    FIELD ton          AS DECIMAL   LABEL "Ton"            FORMAT "->>,>>9.99"
    FIELD vUserID      AS CHARACTER LABEL "Id"             FORMAT "x(8)"
    FIELD custPartNo   AS CHARACTER LABEL "Cust Part"      FORMAT "x(15)" 
    FIELD dieNo        AS CHARACTER LABEL "Die No"         FORMAT "x(15)"
    .

DEFINE TEMP-TABLE wkrecap NO-UNDO    /* recap by product category */
    FIELD procat      LIKE itemfg.procat 
    FIELD t-sqft      LIKE itemfg.t-sqft   EXTENT 2 
    FIELD t-tons      AS   DECIMAL         EXTENT 2 
    FIELD revenue     LIKE oe-ordl.t-price EXTENT 2 
    FIELD price-per-m AS   DECIMAL         EXTENT 2
    FIELD price-per-t AS   DECIMAL         EXTENT 2
    FIELD num-of-ord  AS   INTEGER 
    .

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD ord-no  LIKE oe-ord.ord-no
    FIELD line    LIKE oe-ordl.line
    FIELD sman    AS   CHARACTER 
    FIELD item-n  LIKE itemfg.i-name 
    FIELD procat  LIKE itemfg.procat 
    FIELD qty     LIKE oe-ordl.qty   
    FIELD sqft    LIKE itemfg.t-sqft 
    FIELD t-sqft  LIKE itemfg.t-sqft 
    FIELD t-tons  AS   DECIMAL         
    FIELD price   LIKE oe-ordl.price 
    FIELD revenue LIKE oe-ordl.t-price 
    FIELD misc    AS   LOGICAL
    FIELD cost    AS   DECIMAL
    FIELD comm    AS   DECIMAL 
    FIELD margin  AS   DECIMAL
    . 

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no       AS   INTEGER 
    FIELD chk-inv      AS   LOGICAL INIT YES
    FIELD q-onh        LIKE itemfg.q-onh
    FIELD q-shp        LIKE itemfg.q-onh
    FIELD q-rel        LIKE itemfg.q-onh
    FIELD q-wip        LIKE itemfg.q-onh
    FIELD q-avl        LIKE itemfg.q-onh
    FIELD po-no        LIKE oe-ord.po-no
    FIELD inv          AS   LOGICAL
    FIELD cad-no       LIKE itemfg.cad-no
    FIELD row-id       AS   ROWID 
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    .
/* Orders Booked.rpa */ 

/* Order Acknowledgements Report.rpa */
DEFINE TEMP-TABLE ttOrderAcknowledgements NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD custNo        AS CHARACTER LABEL "Customer ID"        FORMAT "x(8)"
    FIELD ackNo         AS INTEGER   LABEL  "Acknowledgemwnt"   FORMAT ">>>>>>>>"
    FIELD phoneNo       AS CHARACTER LABEL "Telephone"          FORMAT "(999)999-9999"
    FIELD faxNo         AS CHARACTER LABEL "Fax"                FORMAT "(999) 999-9999"
    FIELD contact       AS CHARACTER LABEL "Contact"            FORMAT "x(30)"
    FIELD ordDate       AS DATE      LABEL "Order Date"         FORMAT 99/99/9999
    FIELD custPONo      AS CHARACTER LABEL "Customer PO"        FORMAT "x(15)"
    FIELD CustName      AS CHARACTER LABEL "Bill Customer Name" FORMAT "x(30)"
    FIELD add1          AS CHARACTER LABEL "BillAddress1"       FORMAT "x(30)"
    FIELD add2          AS CHARACTER LABEL "BillAddress2"       FORMAT "x(30)"
    FIELD city          AS CHARACTER LABEL "Billcity"           FORMAT "x(15)"
    FIELD state         AS CHARACTER LABEL "Billstate"          FORMAT "x(5)"
    FIELD zip           AS CHARACTER LABEL "Billzip"            FORMAT "x(8)"
    FIELD soldcustName  AS CHARACTER LABEL "ShipCustomer Name"  FORMAT "x(30)"
    FIELD soldAdd1      AS CHARACTER LABEL "ShipAddress1"       FORMAT "x(30)"
    FIELD soldAdd2      AS CHARACTER LABEL "ShipAddress2"       FORMAT "x(30)"
    FIELD soldCity      AS CHARACTER LABEL "Shipcity"           FORMAT "x(15)"
    FIELD soldState     AS CHARACTER LABEL "ShipState"          FORMAT "x(5)"
    FIELD soldZip       AS CHARACTER LABEL "ShipZip"            FORMAT "x(8)"
    FIELD reqDate       AS DATE      LABEL "Req Date"           FORMAT 99/99/9999
    FIELD fob           AS CHARACTER LABEL "FOB"                FORMAT "x(11)"
    FIELD shipVia       AS CHARACTER LABEL "Ship Via"           FORMAT "x(30)"
    FIELD terms         AS CHARACTER LABEL "Terms"              FORMAT "x(30)"
    FIELD salesRep      AS CHARACTER LABEL "Sales Rep"          FORMAT "x(10)"
    FIELD quoteNo       AS INTEGER   LABEL "Quote No"           FORMAT ">>>>>>>"
    FIELD custPartNo    AS CHARACTER LABEL "Customer Part"      FORMAT "x(15)"
    FIELD fgItem        AS CHARACTER LABEL "FG Item"            FORMAT "x(15)"
    FIELD itemName      AS CHARACTER LABEL "Desc Name"          FORMAT "x(30)"
    FIELD desc1         AS CHARACTER LABEL "Description"        FORMAT "x(30)"
    FIELD desc2         AS CHARACTER LABEL "Description2"       FORMAT "x(30)"
    FIELD qtyOrd        AS INTEGER   LABEL "Order Qty"          FORMAT "->>,>>>,>>9"
    FIELD unitPrice     AS DECIMAL   LABEL "Unit Price"         FORMAT ">>>,>>9.99<<<<"
    FIELD uom           AS CHARACTER LABEL "UOM"                FORMAT "x(3)" 
    FIELD extPrice      AS DECIMAL   LABEL "Ext Price"          FORMAT ">>>,>>9.99<<<<"
    FIELD compAdd1      AS CHARACTER LABEL "Company Address1"   FORMAT "x(30)" 
    FIELD compAdd2      AS CHARACTER LABEL "Company Address2"   FORMAT "x(30)"
    FIELD compCity      AS CHARACTER LABEL "Company City"       FORMAT "x(15)"
    FIELD compState     AS CHARACTER LABEL "Company State"      FORMAT "x(5)"
    FIELD compZip       AS CHARACTER LABEL "Company Zip"        FORMAT "x(8)"
    FIELD compPhone     AS CHARACTER LABEL "Company Phone"      FORMAT "(999)999-9999"
    FIELD compFax       AS CHARACTER LABEL "Company Fax"        FORMAT "(999) 999-9999" 
    FIELD compCustName  AS CHARACTER LABEL "Company  Name"      FORMAT "x(30)"
    FIELD compEmail     AS CHARACTER LABEL "Company email"      FORMAT "x(30)"
    FIELD estNo         AS CHARACTER LABEL "Estimate"           FORMAT "x(8)"
    FIELD prvOrder      AS DECIMAL   LABEL "Previous Order"     FORMAT ">>>>>>>>"
    FIELD csr           AS CHARACTER LABEL "CSR"                FORMAT "x(8)"
    FIELD over          AS DECIMAL   LABEL "Over"               FORMAT ">>9.99"
    FIELD under         AS DECIMAL   LABEL "Under"              FORMAT ">>9.99"
    FIELD poOrder       AS CHARACTER      LABEL "Purchase Order"     FORMAT "x(15)"
    FIELD mfgDate       AS DATE      LABEL "Requested Mfg Date" FORMAT 99/99/9999
    FIELD frtCharge     AS CHARACTER LABEL "Freight Charge"     FORMAT "x(8)"
    FIELD pallet        AS DECIMAL   LABEL "Pallet"             FORMAT ">>>,>>9.99<<<<"
    FIELD billInst1     AS CHARACTER LABEL "Bill Instrucrion1"  FORMAT "x(90)"
    FIELD billInst2     AS CHARACTER LABEL "Bill Instrucrion2"  FORMAT "x(90)"
    FIELD billInst3     AS CHARACTER LABEL "Bill Instrucrion3"  FORMAT "x(90)"
    FIELD billInst4     AS CHARACTER LABEL "Bill Instrucrion4"  FORMAT "x(90)"
    FIELD notes         AS CHARACTER LABEL "Notes"              FORMAT "x(500)"
    FIELD LINE          AS INTEGER   LABEL "line"               FORMAT ">>9"
    FIELD lineTotal     AS DECIMAL   LABEL "Line Total"
    .
/* Order Acknowledgements Report.rpa */

/* Open Order Report.rpa */ 
DEFINE TEMP-TABLE ttOpenOrderReport NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD salesRep    AS CHARACTER LABEL "Rep"              FORMAT "x(3)"
    FIELD custNo      AS CHARACTER LABEL "Cust No"          FORMAT "x(8)"
    FIELD lineDueDate AS DATE      LABEL "Line Due Dt"      FORMAT 99/99/9999
    FIELD relDueDate  AS DATE      LABEL "Rel Due Dt"       FORMAT 99/99/9999
    FIELD custPartNo  AS CHARACTER LABEL "Cust Part"        FORMAT "x(15)"
    FIELD fgItemName  AS CHARACTER LABEL "Item Description" FORMAT "x(30)"
    FIELD fgItemNo    AS CHARACTER LABEL "FG Item "         FORMAT "x(15)"
    FIELD orderNo     AS INTEGER   LABEL "Order No"         FORMAT ">>>>>>"
    FIELD cadNo       AS CHARACTER LABEL "CAD"              FORMAT "x(15)"
    FIELD poNo        AS CHARACTER LABEL "PO No"            FORMAT "x(10)"
    FIELD qtyOrd      AS INTEGER   LABEL "Order Qty"        FORMAT "->,>>>,>>9"
    FIELD qtyOnhand   AS INTEGER   LABEL "Qty Onhand"       FORMAT "->,>>>,>>9"
    FIELD qtyShipped  AS INTEGER   LABEL "Qty Shipped"      FORMAT "->,>>>,>>9"
    FIELD qtyActRel   AS INTEGER   LABEL "Qty ActRel"       FORMAT "->,>>>,>>9"
    FIELD qtyWIP      AS INTEGER   LABEL "Qty WIP"          FORMAT "->,>>>,>>9"
    FIELD qtyAvail    AS INTEGER   LABEL "Qty Avail"        FORMAT "->,>>>,>>9"
    FIELD unit        AS INTEGER   LABEL "Unit"             FORMAT ">>>>9"
    FIELD pallet      AS INTEGER   LABEL "Pallet"           FORMAT ">>9"
    .
/* Open Order Report.rpa */ 

/* Orders Booked By Order No.rpa */
DEFINE TEMP-TABLE ttOrdersBookedByOrderNo NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD orderNo      AS INTEGER   LABEL "OrderNo"        FORMAT ">>>>>>"  
    FIELD estNo        AS CHARACTER LABEL "EstNo"          FORMAT "X(8)"
    FIELD jobNo        AS CHARACTER LABEL "JobNo"          FORMAT "X(9)"
    FIELD orddate      AS DATE      LABEL "Date"           FORMAT 99/99/9999
    FIELD custNo       AS CHARACTER LABEL "CustNo"         FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD fgItem       AS CHARACTER LABEL "Item"           FORMAT "X(15)"
    FIELD fgItemName   AS CHARACTER LABEL "Description"    FORMAT "X(30)"
    FIELD fgOrderQty   AS INTEGER   LABEL "FG Order Qty"   FORMAT "->>>,>>>,>>9"
    FIELD fgCost       AS DECIMAL   LABEL "FG Cost"        FORMAT "->>>,>>>,>>9.99"
    FIELD price        AS DECIMAL   LABEL "Price"          FORMAT ">>,>>>,>>9.99"
    FIELD uom          AS CHARACTER LABEL "UOM"            FORMAT "X(3)"
    FIELD extPrice     AS DECIMAL   LABEL "Ext Price"      FORMAT "->,>>>,>>9.99"
    FIELD fgItemProfit AS DECIMAL   LABEL "FG Item Profit" FORMAT "->,>>>,>>9.99"
    FIELD poMsf        AS DECIMAL   LABEL "PO MSF"         FORMAT ">>>9.99"
    FIELD fgShipped    AS INTEGER   LABEL "FG Shipped"     FORMAT "->>,>>>,>>9"
    FIELD poProfit     AS DECIMAL   LABEL "PO Profit"      FORMAT ">,>>>,>>9.99"
    FIELD poNo         AS INTEGER   LABEL "PO No"          FORMAT ">>>>>>>>9"
    FIELD poQty        AS INTEGER   LABEL "PO Qty"         FORMAT "->>,>>>,>>9"
    FIELD poCost       AS DECIMAL   LABEL "PO Cost"        FORMAT ">>>>,>>9.99"
    FIELD poTotalCost  AS DECIMAL   LABEL "PO Total Cost"  FORMAT "->,>>>,>>9.99"
    FIELD poReceived   AS INTEGER   LABEL "PO Received"    FORMAT "->>,>>>,>>9"
    FIELD orderProfit  AS DECIMAL   LABEL "Order Profit"   FORMAT "->>>>>,>>>99%"
    FIELD msfReceived  AS DECIMAL   LABEL "MSF Recvd"      FORMAT "->,>>9.99"
    FIELD fgShipDate   AS DATE      LABEL "FG Ship Date"   FORMAT 99/99/9999
    FIELD poRecDate    AS DATE      LABEL "PO Rec Date"    FORMAT 99/99/9999
    FIELD fgExtPrice   AS DECIMAL   LABEL "FG Ext Price"   FORMAT "->>>>,>>9.99"
    FIELD poRecCost    AS DECIMAL   LABEL "PO Rec Cost"    FORMAT  "->>>,>>9.99"
    FIELD profSold     AS DECIMAL   LABEL "Profit  Sold"   FORMAT "->>>>>,>>9.99"
    FIELD profSoldp    AS DECIMAL   LABEL "Profit  Sold"   FORMAT "->>>>>,>>>99%"
    FIELD unitBoard    AS INTEGER   LABEL "Units Board"    FORMAT "->>>>>>,>>9"
    FIELD unitWaste    AS DECIMAL   LABEL "Unit Waste"     FORMAT "->>>>,>>9.99"
    FIELD lossp        AS DECIMAL   LABEL "Loss"           FORMAT ">>9.99%"
    FIELD bolNo        AS INTEGER   LABEL "BOL No"         FORMAT ">>>>>>>>"
    FIELD invoiceNo    AS INTEGER   LABEL "Invoice No"     FORMAT ">>>>>>"
    .
/* Orders Booked By Order No.rpa */ 

DEFINE TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.

{sys/ref/CustList.i NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOpenOrderReport Procedure 
FUNCTION fOpenOrderReport RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrderAcknowledgements) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrderAcknowledgements Procedure 
FUNCTION fOrderAcknowledgements RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrdersBooked Procedure 
FUNCTION fOrdersBooked RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOrdersBookedByOrderNo Procedure 
FUNCTION fOrdersBookedByOrderNo RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 18.14
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-build-tt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tt Procedure 
PROCEDURE build-tt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-date         AS DATE               NO-UNDO.
    DEFINE INPUT PARAMETER ip-recid        AS RECID              NO-UNDO.
    DEFINE INPUT PARAMETER ip-cPrimarySort AS CHARACTER          NO-UNDO.
    DEFINE INPUT PARAMETER ip-sort         AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE lv-due-date             LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE lv-due-date2            LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE v-po-no                 LIKE oe-ord.po-no     NO-UNDO.
    DEFINE VARIABLE dShipQty                AS DECIMAL            NO-UNDO.
    
    DEFINE BUFFER b-ar-invl FOR ar-invl.
    DEFINE BUFFER b-inv-head FOR inv-head.
    DEFINE BUFFER b-inv-line FOR inv-line.
    DEFINE BUFFER b-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-boll FOR oe-boll.
   
    v-po-no = oe-ordl.po-no.
    CREATE tt-report.
    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no EQ oe-ordl.i-no NO-ERROR.
    IF AVAILABLE itemfg THEN tt-report.cad-no = itemfg.cad-no.

    IF tt-report.cad-no EQ "" THEN DO:
        RELEASE eb.
        IF TRIM(oe-ordl.est-no) NE "" THEN
            FIND FIRST eb NO-LOCK WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.stock-no EQ oe-ordl.i-no
            AND eb.cad-no   NE ""
            USE-INDEX est-no NO-ERROR.
        IF NOT AVAILABLE eb THEN
            FIND FIRST eb NO-LOCK WHERE eb.company  EQ oe-ordl.company
            AND eb.stock-no EQ oe-ordl.i-no
            AND eb.cad-no   NE ""
            USE-INDEX stock NO-ERROR.
        IF AVAILABLE eb THEN tt-report.cad-no = eb.cad-no.
    END. /*IF tt-report.cad-no*/
    RELEASE eb.

    IF TRIM(oe-ordl.est-no) NE "" THEN DO:
        FIND FIRST eb NO-LOCK 
            WHERE eb.company  EQ oe-ordl.company AND
            eb.est-no   EQ oe-ordl.est-no AND
            eb.stock-no EQ oe-ordl.i-no AND
            eb.form-no  EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-ERROR.
        IF AVAILABLE eb THEN DO:
            ASSIGN
                tt-report.unit-count = eb.cas-cnt
                tt-report.units-pallet = eb.cas-pal.
            RELEASE eb.
        END.  /*IF AVAIL eb*/
    END. /*IF TRIM(oe-ordl.est-no)*/

    ASSIGN
        tt-report.term-id  = ""
        tt-report.key-01   = IF ip-cPrimarySort EQ "Due Date" OR ip-cPrimarySort EQ "Rel Date" THEN
            STRING(YEAR(lv-due-date),"9999") +
            STRING(MONTH(lv-due-date),"99")  +
            STRING(DAY(lv-due-date),"99")
            ELSE
                IF ip-cPrimarySort EQ "Salesman" THEN oe-ordl.s-man[1]
                ELSE ""
                    tt-report.key-02   = oe-ord.cust-no
                    tt-report.key-03   = IF ip-sort EQ "PO" THEN v-po-no
                        ELSE 
                            IF ip-sort EQ "It" THEN
                                (STRING(oe-ordl.i-no,"x(15)") + v-po-no)
                            ELSE IF ip-sort EQ "Cu" THEN
                                (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                            ELSE IF ip-sort EQ "FG" THEN
                                (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                            ELSE IF ip-sort EQ "Or" THEN
                                (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                            ELSE IF ip-sort EQ "CA" THEN
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
                                    /*v-ordl             = NO*/ .
                                                
    FIND b-ar-invl NO-LOCK WHERE RECID(b-ar-invl) EQ ip-recid NO-ERROR.
    IF AVAILABLE b-ar-invl THEN
        ASSIGN
        tt-report.q-shp  = b-ar-invl.ship-qty
        tt-report.inv    = YES
        tt-report.inv-no = b-ar-invl.inv-no.
    FIND b-inv-line  NO-LOCK WHERE RECID(b-inv-line) EQ ip-recid NO-ERROR.

    IF AVAILABLE b-inv-line THEN DO:
        FIND FIRST b-inv-head NO-LOCK WHERE b-inv-head.r-no EQ b-inv-line.r-no NO-ERROR.
        ASSIGN
            tt-report.q-shp  = b-inv-line.ship-qty
            tt-report.inv    = YES
            tt-report.inv-no = b-inv-head.inv-no.
    END. /*IF AVAIL b-inv-line*/

    FIND b-oe-rell NO-LOCK WHERE RECID(b-oe-rell) EQ ip-recid NO-ERROR.

    IF NOT tt-report.inv THEN DO:
        IF AVAILABLE b-oe-rell THEN tt-report.q-rel = b-oe-rell.qty.
    END. /*IF NOT tt-report.inv*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLPackingList Procedure 
PROCEDURE pBOLPackingList :
/*------------------------------------------------------------------------------
  Purpose:     BOL Packing List.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pBOLPackingList.i}
    
    /* local variables */

    /* subject business logic */
    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ ipcCompany
          AND oe-bolh.bol-no  GE iStartBOL
          AND oe-bolh.bol-no  LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no GE cStartCustNo
          AND oe-bolh.cust-no LE cEndCustNo,
        EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
          AND oe-boll.ord-no  GE iStartOrderNo
          AND oe-boll.ord-no  LE iEndOrderNo
        BREAK BY oe-bolh.company
              BY oe-bolh.cust-no
        :
        CREATE ttBOLPackingList.
        ASSIGN
            ttBOLPackingList.bolNo       = oe-boll.bol-no
            ttBOLPackingList.shipDate    = oe-boll.bol-date
            ttBOLPackingList.pickTicket  = ""
            ttBOLPackingList.custNo      = IF oe-boll.cust-no NE "" THEN oe-boll.cust-no
                                           ELSE oe-bolh.cust-no
            ttBOLPackingList.itemNo      = oe-boll.i-no
            ttBOLPackingList.relNo       = oe-bolh.release#
            ttBOLPackingList.orderNo     = oe-boll.ord-no
            ttBOLPackingList.jobNo       = oe-boll.job-no + STRING(oe-boll.job-no2,"99")
            ttBOLPackingList.prntr       = cPrinter
            ttBOLPackingList.xxSort      = ""
            .
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ ipcCompany
               AND cust.cust-no EQ ttBOLPackingList.custNo
             NO-ERROR.
        IF AVAILABLE cust THEN
        ttBOLPackingList.custName = cust.name. 
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ oe-boll.company
               AND loadtag.item-type EQ NO
               AND loadtag.tag-no    EQ oe-boll.tag
               AND loadtag.job-no    EQ oe-boll.job-no
               AND loadtag.i-no      EQ oe-boll.i-no
             NO-ERROR.
        IF NOT AVAILABLE loadtag THEN NEXT.
        ASSIGN
            ttBOLPackingList.poNo       = loadtag.po-no
            ttBOLPackingList.tagDate    = loadtag.tag-date
            ttBOLPackingList.qtyCase    = loadtag.qty-case
            ttBOLPackingList.caseBundle = loadtag.case-bundle
            ttBOLPackingList.partial    = loadtag.partial
            .
        FIND FIRST rfidtag NO-LOCK
             WHERE rfidtag.company   EQ loadtag.company
               AND rfidtag.item-type EQ loadtag.item-type
               AND rfidtag.tag-no    EQ loadtag.tag-no
             NO-ERROR.
        IF AVAILABLE rfidtag AND LENGTH(rfidtag.rfidtag) GT 5 THEN
        ttBOLPackingList.ticket = SUBSTR(rfidtag.rfidtag,LENGTH(rfidtag.rfidtag) - 5).
    END. /* each oe-bolh */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pBuildCustList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildCustList Procedure 
PROCEDURE pBuildCustList :
/*------------------------------------------------------------------------------
  Purpose:     BOL Packing List.rpa
  Parameters:  Company, Use List?, Start Cust, End Cust, ID
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplList      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcStartCust AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEndCust   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcID        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bCust FOR cust.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.

    IF iplList THEN
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    ELSE DO:
        FOR EACH bCust NO-LOCK
            WHERE bCust.company EQ ipcCompany
              AND bCust.cust-no GE ipcStartCust
              AND bCust.cust-no LE ipcEndCust
            :
            CREATE ttCustList.
            ASSIGN 
                ttCustList.cust-no = bCust.cust-no
                ttCustList.log-fld = YES
                .
        END. /* each bcust */
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCalcPoMSF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcPoMSF Procedure 
PROCEDURE pCalcPoMSF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opTotalMsf AS DECIMAL                  NO-UNDO.

    DEFINE VARIABLE v-basis-w   AS DECIMAL                          NO-UNDO. /* for po/po-adder2.p */
    DEFINE VARIABLE v-len       LIKE po-ordl.s-len              NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-wid              NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len              NO-UNDO.
    DEFINE VARIABLE v-ord-qty   LIKE po-ordl.ord-qty            NO-UNDO.
    DEFINE VARIABLE lv-orig-uom AS cha                          NO-UNDO.
    DEFINE VARIABLE factor#     AS DECIMAL                      NO-UNDO.
    DEFINE VARIABLE ll-ea       AS LOG INIT NO                  NO-UNDO.
    DEFINE VARIABLE lv-uom      LIKE po-ordl.pr-qty-uom INIT NO NO-UNDO.
    DEFINE VARIABLE fg-uom-list AS CHARACTER                         NO-UNDO.
    DEFINE VARIABLE v-out-qty   AS INTEGER                          NO-UNDO.
    
    FIND sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ "poprint" 
        NO-ERROR.
    factor# = IF AVAILABLE sys-ctrl AND CAN-DO("Premier,Middlesx,16th's",sys-ctrl.char-fld) THEN .16 ELSE 1.
    {ce/msfcalc.i}
    
    FIND FIRST item
        NO-LOCK WHERE item.company EQ oe-ordl.company
        AND item.i-no EQ po-ordl.i-no
        NO-ERROR.
    ASSIGN
        v-basis-w   = IF AVAILABLE item THEN item.basis-w ELSE v-basis-w
        v-dep       = IF AVAILABLE item THEN item.s-dep ELSE v-dep
        v-len       = (po-ordl.s-len)
        v-wid       = (po-ordl.s-wid)
        v-ord-qty   = (po-ordl.ord-qty)
        lv-orig-uom = po-ordl.pr-qty-uom 
        {po/calc10.i v-len} 
        {po/calc10.i v-wid}.
    IF NOT AVAILABLE item THEN
    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
        AND itemfg.i-no EQ po-ordl.i-no
        NO-ERROR.
    IF AVAILABLE itemfg THEN
        RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT ll-ea).
    IF ll-ea THEN ASSIGN lv-uom = po-ordl.pr-qty-uom. 

    IF v-len EQ 0 AND AVAILABLE ITEM AND
        ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN DO:
        v-len = 12.
        IF lv-orig-uom EQ "ROLL" THEN DO:
            FIND FIRST uom NO-LOCK WHERE uom.uom EQ "ROLL" NO-ERROR.
            IF AVAILABLE uom THEN ASSIGN v-ord-qty = v-ord-qty * uom.mult.
        END.  /*IF lv-orig-uom*/ 
    END.  /*IF v-len EQ 0*/

    RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

    IF po-ordl.pr-qty-uom{2} EQ "EA" OR
        (NOT po-ordl.item-type AND
         LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) GT 0) THEN
        opTotalMsf = IF v-corr THEN ((v-len * v-wid * .007 * DEC(po-ordl.ord-qty{2})) / 1000)
            ELSE ((((v-len * v-wid) / 144) * DEC(po-ordl.ord-qty{2})) / 1000).
                 ELSE DO:
                     /*convert whatever the UOM is into "EACH" first*/
                     opTotalMsf = 0.
                     IF po-ordl.pr-qty-uom NE "EA" THEN DO:
                         opTotalMsf = 0.
                         RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                                                "EA",
                                                v-basis-w,
                                                v-len,
                                                v-wid,
                                                v-dep,
                                                v-ord-qty,
                                                OUTPUT v-out-qty).
                         /*now convert from "EACH" into MSF*/   
                         opTotalMsf = IF v-corr THEN
                             ((v-len * v-wid * .007 * v-out-qty) / 1000)
                             ELSE
                                 ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                                 IF po-ordl.pr-qty-uom EQ "ROLL" THEN
                                     opTotalMsf = OpTotalMsf * (12 / v-len).
                     END.  /*IF po-ordl.pr-qty-uom NE "EA"*/
                 END.  /*else do:*/ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCalcQOH) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcQOH Procedure 
PROCEDURE pCalcQOH :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vdat     AS   DATE    NO-UNDO.
    DEFINE VARIABLE v-curr   AS   LOGICAL NO-UNDO.
    DEFINE VARIABLE v-q-or-v AS   LOGICAL NO-UNDO.
    
    ASSIGN
        vdat     = TODAY
        v-curr   = YES
        v-q-or-v = YES.

    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
          AND itemfg.i-no    EQ oe-ordl.i-no,
        EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
        :
        CREATE tt-fg-bin.
        BUFFER-COPY fg-bin EXCEPT rec_key TO tt-fg-bin.
    END. /*FOR EACH itemfg*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOpenOrderReport Procedure 
PROCEDURE pOpenOrderReport :
/*------------------------------------------------------------------------------
Purpose:     Open Order Report.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pOpenOrderReport.i}
    
    /* local variables */
    DEFINE VARIABLE lInc       AS    LOGICAL          NO-UNDO INITIAL YES.
    DEFINE VARIABLE cStat      AS    CHARACTER        NO-UNDO INITIAL "A".
    DEFINE VARIABLE iBalQty    AS    INTEGER          NO-UNDO.
    DEFINE VARIABLE iQOH       LIKE  itemfg.q-onh     NO-UNDO.
    DEFINE VARIABLE iQtyShp    LIKE  iQOH             NO-UNDO.
    DEFINE VARIABLE iQtyRel    LIKE  iQOH             NO-UNDO.
    DEFINE VARIABLE cComma     AS    CHARACTER        NO-UNDO INITIAL ",".
    DEFINE VARIABLE dJobQty    AS    DECIMAL          NO-UNDO.
    DEFINE VARIABLE dRecQty    AS    DECIMAL          NO-UNDO.
    DEFINE VARIABLE cSort      AS    CHARACTER        NO-UNDO.
    DEFINE VARIABLE lOrderLine AS    LOGICAL          NO-UNDO.
    DEFINE VARIABLE cStat2     AS    CHARACTER        NO-UNDO.
    DEFINE VARIABLE dtDueDate  LIKE  oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE dtDueDate2 LIKE  oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE lSched     AS    LOGICAL          NO-UNDO.

    DEFINE BUFFER bOERell FOR oe-rell.
        
    ASSIGN
        cStartJobNo = FILL(" ",6 - LENGTH(TRIM(cStartJobNo))) + TRIM(cStartJobNo) + STRING(INT(iStartJobNo2),"99")
        cEndJobNo   = FILL(" ",6 - LENGTH(TRIM(cEndJobNo))) + TRIM(cEndJobNo) + STRING(INT(iEndJobNo2),"99") 
        cStat       = SUBSTR(cJobStatus,1,1) 
        cSort       = cPrimarySort-2 .

    FIND FIRST oe-ctrl NO-LOCK
         WHERE oe-ctrl.company EQ ipcCompany
         NO-ERROR.

    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company  EQ ipcCompany
          AND oe-ord.cust-no  GE cStartCustNo
          AND oe-ord.cust-no  LE cEndCustNo
          AND oe-ord.ord-date GE dtStartOrderDate
          AND oe-ord.ord-date LE dtEndOrderDate
          AND oe-ord.user-id  GE cStartUserID
          AND oe-ord.user-id  LE cEndUserID
          AND (cOrderStatus   EQ "A" 
           OR (oe-ord.opened AND cOrderStatus    EQ "O")
           OR (NOT oe-ord.opened AND cOrderStatus EQ "C"))
        USE-INDEX ordate,

        EACH oe-ordl OF oe-ord NO-LOCK
        WHERE oe-ordl.i-no     GE cStartItemNo
          AND oe-ordl.i-no     LE cEndItemNo
          AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) + TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") GE cStartJobNo
          AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) + TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") LE cEndJobNo
          AND oe-ordl.po-no    GE cStartPONumber
          AND oe-ordl.po-no    LE cEndPONumber
          AND oe-ordl.s-man[1] GE cStartSalesRep
          AND oe-ordl.s-man[1] LE cEndSalesRep
          AND (cOrderStatus    EQ "A"
           OR (oe-ordl.stat    NE "C"
          AND cOrderStatus     EQ "O")
           OR (oe-ordl.stat    EQ "C"
          AND cOrderStatus     EQ "C"))
        :
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany 
               AND itemfg.i-no    EQ oe-ordl.i-no
             NO-ERROR.
        IF AVAILABLE itemfg AND itemfg.stat NE "A" AND NOT lIncludeInactiveItems THEN NEXT.
        {aoaAppSrv/iOpenOrderReport.i}
    END. /*  each oe-ord  */

    FOR EACH tt-report NO-LOCK 
       WHERE tt-report.term-id EQ "",
        FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ tt-report.row-id,
        FIRST oe-ord OF oe-ordl 
        BREAK BY tt-report.row-id
        BY tt-report.key-07:
        
        ASSIGN
            iQtyShp = iQtyShp + tt-report.q-shp
            iQtyRel = iQtyRel + tt-report.q-rel.

        IF LAST-OF(tt-report.row-id) THEN DO:
            IF NOT CAN-FIND(FIRST tt-fg-bin
                            WHERE tt-fg-bin.company EQ ipcCompany
                            AND tt-fg-bin.i-no    EQ oe-ordl.i-no) THEN
                RUN pCalcQOH (ipcCompany).

            FOR EACH tt-fg-bin
                WHERE tt-fg-bin.company EQ oe-ordl.company
                AND tt-fg-bin.i-no    EQ oe-ordl.i-no
                AND tt-fg-bin.job-no  EQ oe-ordl.job-no
                AND tt-fg-bin.job-no2 EQ oe-ordl.job-no2:
                ASSIGN
                    tt-report.q-onh  = tt-report.q-onh + tt-fg-bin.qty
                    tt-fg-bin.ord-no = oe-ord.ord-no.
            END. /*  end of for each tt-fg-bin */

            IF lIncludeJobsQOH THEN
            FOR EACH job-hdr NO-LOCK WHERE job-hdr.company  EQ oe-ordl.company
                AND job-hdr.ord-no   EQ oe-ordl.ord-no
                AND job-hdr.i-no     EQ oe-ordl.i-no
                AND (job-hdr.job-no  NE oe-ordl.job-no OR
                     job-hdr.job-no2 NE oe-ordl.job-no2)
                BREAK BY job-hdr.job-no
                BY job-hdr.job-no2
                BY job-hdr.i-no:
                IF FIRST-OF(job-hdr.i-no) THEN
                FOR EACH tt-fg-bin WHERE tt-fg-bin.company EQ job-hdr.company
                    AND tt-fg-bin.i-no    EQ job-hdr.i-no
                    AND tt-fg-bin.job-no  EQ job-hdr.job-no
                    AND tt-fg-bin.job-no2 EQ job-hdr.job-no2:
                    ASSIGN
                        tt-report.q-onh  = tt-report.q-onh + tt-fg-bin.qty
                        tt-fg-bin.ord-no = oe-ord.ord-no.
                END.  /*end of for each tt-fg-bin */
            END.  /*end of  for each job-hdr */

            ASSIGN
                tt-report.q-shp = iQtyShp
                tt-report.q-rel = iQtyRel.
            IF cWIPQty EQ "1" THEN
                tt-report.q-wip = oe-ordl.qty - (tt-report.q-onh + tt-report.q-shp).
            ELSE  DO:
                ASSIGN
                    dJobQty = 0
                    dRecQty = 0.
                FIND FIRST job NO-LOCK 
                     WHERE job.company EQ ipcCompany AND 
                           job.job-no  EQ oe-ordl.job-no AND
                           job.job-no2 EQ oe-ordl.job-no2
                     NO-ERROR.
                IF AVAILABLE job THEN DO:
                    IF NOT job.opened THEN
                        tt-report.q-wip = 0.
                    ELSE DO:
                        FOR EACH job-hdr FIELDS(qty) NO-LOCK WHERE
                            job-hdr.company  EQ oe-ordl.company AND
                            job-hdr.ord-no   EQ oe-ordl.ord-no  AND
                            job-hdr.i-no     EQ oe-ordl.i-no    AND
                            job-hdr.job-no   EQ oe-ordl.job-no  AND
                            job-hdr.job-no2  EQ oe-ordl.job-no2
                            :
                            dJobQty = dJobQty + job-hdr.qty.
                        END.  /* end of for each job-hdr */

                        FIND FIRST itemfg NO-LOCK 
                             WHERE itemfg.company EQ job.company AND
                                   itemfg.i-no    EQ oe-ordl.i-no NO-ERROR.

                        IF AVAILABLE itemfg THEN DO:
                            IF itemfg.isaset AND itemfg.alloc THEN
                            FOR EACH fg-act FIELDS(qty) NO-LOCK WHERE
                                fg-act.company EQ job.company AND
                                fg-act.job-no  EQ oe-ordl.job-no AND
                                fg-act.job-no2 EQ oe-ordl.job-no2 AND
                                fg-act.i-no    EQ oe-ordl.i-no
                                :
                                dRecQty = dRecQty + fg-act.qty.
                            END. /* end  for each fg-act  */

                            FOR EACH fg-rcpth FIELDS(r-no rita-code company) NO-LOCK WHERE
                                fg-rcpth.company   EQ job.company AND
                                fg-rcpth.i-no      EQ oe-ordl.i-no AND
                                fg-rcpth.job-no    EQ oe-ordl.job-no AND
                                fg-rcpth.job-no2   EQ oe-ordl.job-no2 AND
                                fg-rcpth.rita-code EQ "R"
                                ,
                                EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE
                                fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                                fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                BREAK BY fg-rcpth.company:
                                IF FIRST(fg-rcpth.company) THEN dRecQty = 0.
                                dRecQty = dRecQty + fg-rdtlh.qty.
                            END. /* end of for each fg-rcpth */

                            RELEASE itemfg.
                        END. /*IF AVAIL itemfg*/
                    END. /*ELSE DO*/
                    RELEASE job.
                    tt-report.q-wip = dJobQty - dRecQty.
                END. /*IF AVAIL job*/
            END. /*ELSE DO*/
            IF tt-report.q-wip LT 0 OR
                tt-report.q-wip LT oe-ordl.qty * oe-ordl.under-pct / 100 THEN
                tt-report.q-wip = 0.
            tt-report.q-avl = tt-report.q-onh + tt-report.q-wip - tt-report.q-rel.
            IF tt-report.q-avl LT 0 THEN tt-report.q-avl = 0.
            ASSIGN
                iQtyShp = 0
                iQtyRel = 0.
        END. /*IF LAST-OF(tt-report.row-id)*/
        ELSE DELETE tt-report.
    END. /*FOR EACH tt-report*/
    
    FOR EACH tt-report NO-LOCK 
       WHERE tt-report.term-id EQ ""
        AND tt-report.cad-no   GE cStartCAD 
        AND tt-report.cad-no   LE cEndCAD   
        AND (lIncludeZeroQtyWIPItems OR tt-report.q-wip GT 0)
        AND (lIncludeZeroQtyActReleaseQty OR tt-report.q-avl GT 0 OR tt-report.q-rel GT 0) ,
        FIRST itemfg
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no      EQ tt-report.key-06 ,
        FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
        AND cust.cust-no   EQ tt-report.key-02 ,
        FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ tt-report.row-id ,
        FIRST oe-ord OF oe-ordl 
        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.key-04
        BY tt-report.key-05
        BY tt-report.key-06
        BY tt-report.row-id
        BY tt-report.key-07:
        ASSIGN dtDueDate2 = ? .
         
        FOR EACH oe-rel NO-LOCK 
           WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            BY oe-rel.rel-date DESCENDING:
            dtDueDate2 = IF AVAILABLE oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
            LEAVE.
        END. /* end of for each oe-rel */

        CREATE ttOpenOrderReport.
        ASSIGN
            ttOpenOrderReport.salesRep    = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.s-man[1]) ELSE ""
            ttOpenOrderReport.custNo      = STRING(cust.cust-no)
            ttOpenOrderReport.lineDueDate = oe-ordl.req-date
            ttOpenOrderReport.relDueDate  = dtDueDate2 
            ttOpenOrderReport.custPartNo  = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.part-no) ELSE "" 
            ttOpenOrderReport.fgItemName  = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.i-name) ELSE "" 
            ttOpenOrderReport.fgItemNo    = IF AVAILABLE oe-ordl THEN STRING(oe-ordl.i-no) ELSE "" 
            ttOpenOrderReport.orderNo     = IF AVAILABLE oe-ordl THEN (oe-ordl.ord-no) ELSE 0 
            ttOpenOrderReport.cadNo       = STRING(tt-report.cad-no) 
            ttOpenOrderReport.poNo        = STRING(tt-report.po-no)
            ttOpenOrderReport.qtyOrd      = oe-ordl.qty 
            ttOpenOrderReport.qtyOnhand   = tt-report.q-onh
            ttOpenOrderReport.qtyShipped  = tt-report.q-shp
            ttOpenOrderReport.qtyActRel   = tt-report.q-rel
            ttOpenOrderReport.qtyWIP      = tt-report.q-wip
            ttOpenOrderReport.qtyAvail    = tt-report.q-avl
            ttOpenOrderReport.unit        = tt-report.unit-count
            ttOpenOrderReport.pallet      = tt-report.units-pallet
            .
            DELETE tt-report.
    END. /* each tt-report */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrderAcknowledgements) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrderAcknowledgements Procedure 
PROCEDURE pOrderAcknowledgements :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBooked Procedure 
PROCEDURE pOrdersBooked :
/*------------------------------------------------------------------------------
Purpose:     OrdersBooked.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pOrdersBooked.i}

    /* local variables */
    DEFINE VARIABLE i             AS   INTEGER             NO-UNDO.
    DEFINE VARIABLE J             AS   INTEGER             NO-UNDO.
    DEFINE VARIABLE K             AS   INTEGER             NO-UNDO.
    DEFINE VARIABLE ii            LIKE i                   NO-UNDO.
    DEFINE VARIABLE cv-code       AS   CHARACTER           NO-UNDO.
    DEFINE VARIABLE lPrt-Sqft     AS   LOGICAL INITIAL YES NO-UNDO.
    DEFINE VARIABLE dtMdate       AS   DATE                NO-UNDO.
    DEFINE VARIABLE i-per-days    AS   INTEGER EXTENT 2    NO-UNDO INITIAL 0.
    DEFINE VARIABLE v-n-lines     AS   INTEGER             NO-UNDO.
    DEFINE VARIABLE cSalesRep     AS   CHARACTER           NO-UNDO.
    DEFINE VARIABLE lExclude      AS   LOGICAL             NO-UNDO.
    DEFINE VARIABLE lMisc         AS   LOGICAL             NO-UNDO.
    DEFINE VARIABLE dPriceAmount  LIKE oe-ord.t-revenue    NO-UNDO.
    DEFINE VARIABLE dPct          AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE dTotalSqft    LIKE itemfg.t-sqft       NO-UNDO.
    DEFINE VARIABLE dTotTons      AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE dOrdQty       LIKE oe-ordl.qty         NO-UNDO.
    DEFINE VARIABLE dMsfPrice     AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE dPricePerTon  AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE dRevenue      LIKE oe-ordl.t-price     NO-UNDO.
    DEFINE VARIABLE dProfitPer    AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE dMargin       AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE cSalesName    LIKE sman.sname.       
    DEFINE VARIABLE v             AS   INTEGER             NO-UNDO.
    DEFINE VARIABLE qm            AS   DECIMAL             NO-UNDO.
    DEFINE VARIABLE tb_sortby     AS   LOGICAL INIT NO     NO-UNDO.
    DEFINE VARIABLE dtTrandate    LIKE dtStartOrderDate    NO-UNDO.
    
    DEFINE BUFFER b-itemfg FOR itemfg.
    FIND FIRST w-data NO-ERROR.    

    FIND FIRST ce-ctrl NO-LOCK 
         WHERE ce-ctrl.company EQ ipcCompany.
    
    FIND FIRST period NO-LOCK
        WHERE period.company EQ ipcCompany
        AND period.pst       LE dtEndOrderDate
        AND period.pend      GE dtEndOrderDate
        NO-ERROR.
    dtTrandate = IF AVAILABLE period THEN MINIMUM(dtStartOrderDate,period.pst) ELSE dtStartOrderDate.
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company  EQ ipcCompany
        AND oe-ord.cust-no  GE cStartCustNo
        AND oe-ord.cust-no  LE cEndCustNo
        AND oe-ord.ord-date GE dtTrandate
        AND oe-ord.ord-date LE dtEndOrderDate
        AND oe-ord.type     NE "T"
        AND oe-ord.stat     NE "D"
        BY oe-ord.company 
        BY oe-ord.ord-date 
        BY oe-ord.ord-no:
        
        IF lRelOrd THEN DO:
            IF oe-ord.TYPE EQ "T" THEN NEXT.
            cv-code = "".
            FOR EACH oe-rel FIELDS(r-no) NO-LOCK 
                WHERE oe-rel.company EQ oe-ord.company 
                AND oe-rel.ord-no  EQ oe-ord.ord-no,
                FIRST reftable NO-LOCK 
                WHERE reftable.reftable EQ "oe-rel.s-code" 
                AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999") 
                AND reftable.CODE EQ "T":
                cv-code = "T".
                LEAVE.
            END.  /*FOR EACH oe-rel*/
            IF cv-code EQ "T" THEN NEXT.
        END.  /* if lRelOrd */

        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ ipcCompany
            AND oe-ordl.ord-no  EQ oe-ord.ord-no
            AND (oe-ordl.is-a-component EQ NO OR lSetCom EQ NO),
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipcCompany
            AND itemfg.i-no    EQ oe-ordl.i-no
            AND itemfg.procat  GE  cStartProdCategory
            AND itemfg.procat  LE cEndProdCategory
            BREAK BY oe-ordl.line:
            lExclude = YES.
            
            DO i = 1 TO 3:
                IF lExclude 
                    AND oe-ordl.s-man[i] GE cStartSalesRep 
                    AND oe-ordl.s-man[i] LE cEndSalesRep 
                    THEN lExclude = NO .
            END.  /* do i.. */
            
            IF lExclude THEN NEXT.
            lMisc = FALSE.
            DO i = 1 TO 3:
                IF lMisc THEN LEAVE.
                IF oe-ordl.s-man[i] LT cStartSalesRep 
                    OR oe-ordl.s-man[i] GT cEndSalesRep THEN NEXT.
                /* if no salesman number then assign to misc, ie, blank no */
                IF i EQ 1 
                    AND oe-ordl.s-man[1] EQ "" 
                    AND oe-ordl.s-man[2] EQ "" 
                    AND oe-ordl.s-man[3] EQ "" THEN cSalesRep = "MISC".
                ELSE   /* if blank salesman # then ignore */
                    IF oe-ordl.s-man[i] EQ "" THEN NEXT.
                    /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                    ELSE cSalesRep = oe-ordl.s-man[i].
                IF oe-ord.ord-date GE  dtStartOrderDate 
                    AND oe-ord.ord-date LE dtEndOrderDate THEN DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.term-id  = ""
                        tt-report.key-01   = cSalesRep
                        tt-report.key-02   = IF tb_sortby THEN STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
                        tt-report.key-03   = STRING(i,"9")
                        tt-report.rec-id   = RECID(oe-ordl).           
                END.  /* if oe-ord.ord-date */
                ASSIGN
                    dPct  = oe-ordl.s-pct[i] / 100
                    dOrdQty  = oe-ordl.qty * dPct
                    dTotalSqft = itemfg.t-sqft * dOrdQty / 1000
                    dTotTons = itemfg.weight-100 * dOrdQty / 100 / 2000
                    dPriceAmount  = oe-ordl.t-price * dPct.
                FIND FIRST wkrecap NO-LOCK
                     WHERE wkrecap.procat EQ IF AVAILABLE itemfg THEN itemfg.procat ELSE ? NO-ERROR.
                IF NOT AVAILABLE wkrecap THEN DO:
                    CREATE wkrecap.
                    ASSIGN
                        wkrecap.procat     = IF AVAILABLE itemfg THEN itemfg.procat ELSE ?
                        wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
                END.  /*if not avail wkrecap*/
                ELSE wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
                j = IF oe-ord.ord-date GE  dtStartOrderDate 
                    AND oe-ord.ord-date LE  dtEndOrderDate THEN 1 ELSE 2.
               k = IF AVAILABLE period AND oe-ord.ord-date GE period.pst  
                   AND oe-ord.ord-date LE period.pend THEN 2 ELSE 1.
               IF j LE k THEN DO ii = j TO k:
                   ASSIGN
                       wkrecap.t-sqft[ii]  = wkrecap.t-sqft[ii] + dTotalSqft
                       wkrecap.t-tons[ii]  = wkrecap.t-tons[ii] + dTotTons
                       wkrecap.revenue[ii] = wkrecap.revenue[ii] + dPriceAmount.
               END.  /*if j le k then*/
            END.  /* do i = 1 to 3... */

            IF oe-ord.ord-date NE dtMdate THEN DO:
                dtMdate = oe-ord.ord-date.
                IF oe-ord.ord-date GE  dtStartOrderDate 
                    AND oe-ord.ord-date LE  dtEndOrderDate THEN
                    i-per-days[1] = i-per-days[1] + 1.
                IF AVAILABLE period AND oe-ord.ord-date GE period.pst  
                    AND oe-ord.ord-date LE period.pend THEN
                    i-per-days[2] = i-per-days[2] + 1.
            END.  /*if oe-ord.ord-date ne dtMdate then do:*/
        END.  /*for each oe-ordl no-lock*/ 

        IF lMiscChg THEN
        FOR EACH oe-ordm NO-LOCK
            WHERE oe-ordm.company EQ ipcCompany
            AND oe-ordm.ord-no  EQ oe-ord.ord-no:
            lExclude = YES.
            DO i = 1 TO 3:
                IF lExclude AND
                    oe-ordm.s-man[i] GE cStartSalesRep AND
                    oe-ordm.s-man[i] LE cEndSalesRep THEN lExclude = NO.
            END.  /* do i.. */

            IF lExclude THEN NEXT.
            /* At this point we have either 1, 2 or 3 valid salesman, in any  */
            /* combination of the array. */
            lMisc = FALSE.
            DO i = 1 TO 3:
                IF lMisc THEN LEAVE.
                IF oe-ordm.s-man[i] LT cStartSalesRep OR
                    oe-ordm.s-man[i] GT cEndSalesRep THEN NEXT.
                /* if no salesman number then assign to misc, ie, blank no */
                IF i EQ 1 AND
                    oe-ordm.s-man[1] EQ "" AND
                    oe-ordm.s-man[2] EQ "" AND
                    oe-ordm.s-man[3] EQ "" THEN cSalesRep = "MISC".
                ELSE   /* if blank salesman # then ignore */
                    IF oe-ordm.s-man[i] EQ "" THEN NEXT.
                    /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                    ELSE cSalesRep = oe-ordm.s-man[i].
                    IF oe-ord.ord-date GE  dtStartOrderDate 
                        AND oe-ord.ord-date LE  dtEndOrderDate THEN DO:
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = ""
                            tt-report.key-01  = cSalesRep
                            tt-report.key-02  = IF tb_sortby THEN STRING(oe-ord.ord-no,">>>>>>>>>>") ELSE ""
                            tt-report.key-03  = STRING(i,"9")
                            tt-report.rec-id  = RECID(oe-ordm).
                    END.  /*IF oe-ord.ord-date*/
                    ASSIGN
                        dPct = oe-ordm.s-pct[i] / 100
                        dPriceAmount = oe-ordm.amt * dPct.
                    FIND FIRST wkrecap NO-LOCK WHERE wkrecap.procat EQ "P/M" NO-ERROR.
                    IF NOT AVAILABLE wkrecap THEN DO:
                        CREATE wkrecap.
                        ASSIGN
                            wkrecap.procat     = "P/M"
                            wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
                    END.  /*IF NOT AVAIL wkrecap*/ 
                    ELSE wkrecap.num-of-ord = wkrecap.num-of-ord + 1.
                    j = IF oe-ord.ord-date GE dtStartOrderDate 
                        AND oe-ord.ord-date LE dtEndOrderDate THEN 1 ELSE 2.
                    k = IF AVAILABLE period AND oe-ord.ord-date GE period.pst  
                        AND oe-ord.ord-date LE period.pend THEN 2 ELSE 1.
                    /* We cannot disturb loop variable i from within loop,so use ii: */

                    IF j LE k THEN DO ii = j TO k:
                        wkrecap.revenue[ii] = wkrecap.revenue[ii] + dPriceAmount.
                    END.  /*IF j LE k*/
            END.  /*do i = 1 to 3:*/
        END.  /*for each oe-ordm no-lock*/   
    END.  /* for each oe-ord */
    
    FOR EACH tt-report WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01 BY tt-report.key-02:
        FIND FIRST oe-ordm NO-LOCK
            WHERE RECID(oe-ordm) EQ tt-report.rec-id NO-ERROR.
        IF AVAILABLE oe-ordm THEN DO:
            FIND FIRST oe-ord OF oe-ordm NO-LOCK.
            ASSIGN
                i            = INT(tt-report.key-03)
                dPct         = oe-ordm.s-pct[i] / 100
                dPriceAmount = oe-ordm.amt * dPct.
            ASSIGN
                dRevenue     = dPriceAmount
                dProfitPer   = (dRevenue - (oe-ordm.cost * dPct)) / dRevenue * 100
                .
        END.  /*if avail oe-ordm then do:*/
        ELSE DO:
            FIND FIRST oe-ordl NO-LOCK 
            WHERE RECID(oe-ordl) EQ tt-report.rec-id NO-ERROR.
            ASSIGN
                i             = INT(tt-report.key-03)
                dPct          = oe-ordl.s-pct[i] / 100
                dOrdQty       = oe-ordl.qty * dPct
                dPriceAmount  = oe-ordl.t-price * dPct
                qm            = oe-ordl.qty / 1000 .
            ASSIGN
                dRevenue      = dPriceAmount
                dProfitPer    = (dRevenue - (oe-ordl.cost * qm)) / dRevenue * 100
                .
        END.  /*ELSE DO:*/
        IF dProfitPer EQ ? THEN dProfitPer  = 0.
        IF lPrt-Sqft THEN DO:       
            /*==== new with selectable columns ====*/
            IF lUnder AND lOver THEN DO:
                IF dProfitPer GE iUnderValue AND dProfitPer LE iOverValue THEN 
                    DELETE tt-report .
            END. /*IF lUnder AND lOver THEN DO:*/
            ELSE IF lUnder AND NOT lOver THEN DO:
                IF dProfitPer GE iUnderValue THEN DELETE tt-report.
            END.  /*ELSE IF lUnder AND NOT lOver THEN DO:*/
            ELSE IF lOver AND NOT lUnder THEN DO:
                IF dProfitPer LE iOverValue THEN DELETE tt-report.
            END.  /*ELSE IF lOver AND NOT lUnder THEN DO:*/
        END.  /* if lPrt-Sqft then do */
        ELSE  DO:
            IF lUnder AND dProfitPer GT iUnderValue THEN DELETE tt-report.
            IF lOver AND dProfitPer LT iOverValue THEN DELETE tt-report.
        END.  /* end of else do lPrt-Sqft */
    END.  /* for each tt-report */
    
    FOR EACH tt-report WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01 BY tt-report.key-02:
        {aoaAppSrv/iOrdersBooked.i}
        FIND FIRST oe-ordl NO-LOCK 
         WHERE RECID(oe-ordl) EQ tt-report.rec-id NO-ERROR.
        IF FIRST-OF(tt-report.key-01) THEN DO:
            FIND FIRST sman NO-LOCK
                WHERE sman.company EQ ipcCompany
                AND sman.sman      EQ w-data.sman
                NO-ERROR.
            cSalesName = IF AVAILABLE sman THEN sman.sname
                ELSE "* NOT IN SALES REP FILE *".
        END.  /*IF FIRST-OF(tt-report.key-01) THEN DO:*/
        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ ipcCompany
            AND oe-ord.ord-no    EQ w-data.ord-no
            NO-ERROR.
        FIND cust OF oe-ord NO-LOCK NO-ERROR.
        ASSIGN
            dRevenue     = w-data.revenue
            dMsfPrice    = dRevenue / w-data.t-sqft
            dPricePerTon = dRevenue / w-data.t-tons
            dProfitPer   = (dRevenue - w-data.cost) / dRevenue * 100
            dMargin      = w-data.margin.
        IF dMsfPrice    EQ ? THEN dMsfPrice    = 0.
        IF dPricePerTon EQ ? THEN dPricePerTon = 0.
        IF dProfitPer   EQ ? THEN dProfitPer   = 0.
        IF dMargin      EQ ? THEN dMargin      = 0.
        ACCUMULATE
            w-data.t-sqft (TOTAL BY tt-report.key-01)
            w-data.t-tons (TOTAL BY tt-report.key-01)
            dRevenue      (TOTAL BY tt-report.key-01)
            w-data.cost   (TOTAL BY tt-report.key-01).

        IF AVAILABLE oe-ordl THEN do:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ oe-ordl.i-no NO-ERROR.
            
            FIND FIRST eb NO-LOCK WHERE eb.company EQ cocode
                AND eb.est-no  EQ oe-ordl.est-no
                AND eb.stock-no EQ oe-ordl.i-no NO-ERROR .
        END. /* avail oe-ordl  */

        /*==== new with selectable columns ====*/
        IF lUnder AND lOver THEN DO:
            IF dProfitPer GE iUnderValue AND dProfitPer LE iOverValue THEN NEXT.
        END. /*IF lUnder AND lOver THEN DO:*/
        ELSE IF lUnder AND NOT lOver THEN DO:
            IF dProfitPer GE iUnderValue THEN NEXT.
        END. /*ELSE IF lUnder AND NOT lOver THEN DO:*/
        ELSE IF lOver AND NOT lUnder THEN DO:
            IF dProfitPer LE iOverValue THEN NEXT.
        END. /*ELSE IF lOver AND NOT lUnder THEN DO:*/

        PUT UNFORMATTED "ttOrdersBooked" SKIP.
        CREATE ttOrdersBooked.
        ASSIGN 
            /*ttOrdersBooked.rowtype    =   oe-ord.due-date*/              
            ttOrdersBooked.dueDate      =   oe-ord.due-date                    
            ttOrdersBooked.orderNo      =   w-data.ord-no                      
            ttOrdersBooked.custName     =   cust.name                   
            ttOrdersBooked.custNo       =   cust.cust-no
            ttOrdersBooked.salesRep     =   IF AVAILABLE sman THEN sman.sman ELSE ""
            ttOrdersBooked.salesRepName =   IF AVAILABLE sman THEN sman.sname ELSE "" 
            ttOrdersBooked.commPer      =   w-data.comm                      
            ttOrdersBooked.prodCode     =   w-data.procat 
            ttOrdersBooked.fgItemNo     =   oe-ordl.i-no                       
            ttOrdersBooked.fgItemName   =   w-data.item-n                      
            ttOrdersBooked.qtyOrdEa     =   w-data.qty                         
            ttOrdersBooked.sqFit        =   w-data.sqft                     
            ttOrdersBooked.totalSqfit   =   w-data.t-sqft                          
            ttOrdersBooked.msfPrice     =   dMsfPrice                   
            ttOrdersBooked.price        =   w-data.price                     
            ttOrdersBooked.orderAmount  =   dRevenue                       
            ttOrdersBooked.profitPer    =   dProfitPer                       
            ttOrdersBooked.totalTons    =   w-data.t-tons                          
            ttOrdersBooked.ton          =   dPricePerTon  
            ttOrdersBooked.vUserID      =   oe-ord.user-id                  
            ttOrdersBooked.custPartNo   =   oe-ordl.part-no
            ttOrdersBooked.dieNo        =   IF AVAILABLE itemfg AND itemfg.die-no NE "" THEN STRING(itemfg.die-no,"x(15)") ELSE IF AVAILABLE eb THEN STRING(eb.die-no,"x(15)") ELSE "" .
            . 
        DELETE w-data.
        DELETE tt-report.
    END. /* for each tt-report */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBookedByOrderNo Procedure 
PROCEDURE pOrdersBookedByOrderNo :
/*------------------------------------------------------------------------------
Purpose:     Orders Booked by Order No.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pOrdersBookedByOrderNo.i}

    /* local variables */
    cocode = ipcCompany .
    DEFINE VARIABLE lv-ord-qty        AS   LOGICAL  INITIAL YES.
    DEFINE VARIABLE dv-tot-ord        AS   DECIMAL  EXTENT 2.
    DEFINE VARIABLE dv-tax-rate       AS   DECIMAL .
    DEFINE VARIABLE lv-ship           AS   LOGICAL INITIAL NO.
    DEFINE VARIABLE dv-tot-tax        LIKE oe-ord.tax.
    DEFINE VARIABLE dv-tot-freight    LIKE oe-ord.t-freight.
    DEFINE VARIABLE iv-qty-lft        LIKE oe-ordl.qty.
    DEFINE VARIABLE dv-ext-price      LIKE oe-ordl.t-price.
    DEFINE VARIABLE lv-prt-cont       AS   LOGICAL INITIAL NO.
    DEFINE VARIABLE dv-margin         AS   DECIMAL.
    DEFINE VARIABLE dv-margin-tot     AS   DECIMAL .
    DEFINE VARIABLE dv-ext-cost       AS   DECIMAL.
    DEFINE VARIABLE dv-orderedMsf     AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE iv-jobShipQty     AS   INTEGER NO-UNDO.
    DEFINE VARIABLE dv-boardProfit    AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE iv-boardPO        AS   INTEGER NO-UNDO.
    DEFINE VARIABLE iv-boardpoQty     AS   INTEGER NO-UNDO.
    DEFINE VARIABLE dv-boardCost      AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dv-boardTotalCost AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE iv-boardTotalQty  AS   INTEGER NO-UNDO.
    DEFINE VARIABLE dv-Order%Profit   AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dv-MSFRec         AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dtv-FGShipDate    AS   DATE    NO-UNDO.
    DEFINE VARIABLE dt-PORecDate      AS   DATE    NO-UNDO.
    DEFINE VARIABLE dv-FGExtPrice     AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dv-PORecCost      AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dv-ProfitSold$    AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dv-ProfitSold%    AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE iv-UnitsBoard     AS   INTEGER NO-UNDO.
    DEFINE VARIABLE dv-UnitLoss$      AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE dv-Loss%          AS   DECIMAL NO-UNDO.
    DEFINE VARIABLE iv-bol#           AS   INTEGER NO-UNDO.
    DEFINE VARIABLE iv-inv#           AS   INTEGER NO-UNDO.
    DEFINE BUFFER   b-oe-ordl         FOR oe-ordl.

    
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company EQ ipcCompany
        AND oe-ord.ord-no   GE iStartOrderNo
        AND oe-ord.ord-no   LE iEndOrderNo
        AND oe-ord.cust-no  GE cStartCustNo
        AND oe-ord.cust-no  LE cEndCustNo
        AND oe-ord.ord-date GE dtStartOrderDate
        AND oe-ord.ord-date LE dtEndOrderDate
        AND oe-ord.stat     NE "D"
        AND oe-ord.type     NE "T"
        USE-INDEX ord-no ,
        FIRST b-oe-ordl NO-LOCK
        WHERE b-oe-ordl.company EQ ipcCompany
        AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
        AND b-oe-ordl.i-no    GE cStartItemNo
        AND b-oe-ordl.i-no    LE cEndItemNo
        ,
        FIRST cust NO-LOCK WHERE (cust.company EQ ipcCompany) 
        AND cust.cust-no EQ oe-ord.cust-no BREAK BY oe-ord.ord-no:
        dv-tot-ord[1] = 0.

        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ oe-ord.company
            AND oe-ordl.ord-no    EQ oe-ord.ord-no
            AND oe-ordl.i-no      GE cStartItemNo
            AND oe-ordl.i-no      LE cEndItemNo
            BREAK BY oe-ordl.ord-no:
            IF NOT FIRST(oe-ordl.ord-no) AND FIRST-OF(oe-ordl.ord-no) THEN PUT SKIP(1).
            ASSIGN
                lv-ship      = oe-ordl.stat NE "I" AND oe-ordl.stat NE "B"
                iv-qty-lft   = oe-ordl.qty - (IF lv-ord-qty THEN 0 ELSE oe-ordl.inv-qty)
                dv-ext-price = 0.
            IF iv-qty-lft LT 0 THEN iv-qty-lft = 0.
            FIND FIRST itemfg NO-LOCK WHERE (itemfg.company EQ ipcCompany ) 
                AND itemfg.i-no EQ oe-ordl.i-no NO-ERROR  .

                RUN  oe/GetPriceTotal.p(INPUT oe-ordl.qty, INPUT oe-ordl.price, INPUT oe-ordl.pr-uom,
                                        INPUT ( IF AVAILABLE itemfg THEN itemfg.case-count ELSE 0),
                                        INPUT oe-ordl.disc,
                                        OUTPUT dv-ext-price).  /* task 01241601 */ 
            dv-tot-freight = dv-tot-freight +
                (ROUND(oe-ordl.t-freight / oe-ordl.qty, 2) * iv-qty-lft).
            /** CALCULATE TAX CHARGES **/
            IF oe-ordl.tax AND dv-tax-rate GT 0 THEN
                dv-tot-tax = dv-tot-tax + ROUND((dv-ext-price * dv-tax-rate) / 100,2).
            /*if lv-prt-cont then */
            ASSIGN
                dv-ext-cost = (oe-ordl.cost * oe-ordl.qty) / 1000
                dv-margin = dv-ext-price - dv-ext-cost.
            ASSIGN 
                iv-boardTotalQty  = 0
                iv-boardPO        = 0
                iv-boardpoQty     = 0
                dv-boardCost      = 0
                dv-boardTotalCost = 0
                dv-boardProfit    = 0
                dv-orderedMsf     = 0
                .
            IF oe-ordl.po-no-po NE 0 THEN DO:
                FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ oe-ordl.company 
                    AND po-ordl.po-no EQ oe-ordl.po-no-po
                    AND ((po-ordl.item-type EQ YES AND TRIM(oe-ordl.job-no) NE ""
                    AND po-ordl.job-no EQ oe-ordl.job-no AND po-ordl.job-no2 EQ oe-ordl.job-no2)   
                    OR (po-ordl.item-type EQ NO AND po-ordl.i-no EQ oe-ordl.i-no)) NO-ERROR.
                ASSIGN
                    iv-boardTotalQty  = IF AVAILABLE po-ordl THEN po-ordl.t-rec-qty ELSE 0
                    iv-boardPO        = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po  ELSE 0
                    iv-boardpoQty     = IF AVAILABLE po-ordl THEN po-ordl.ord-qty   ELSE 0
                    dv-boardCost      = IF AVAILABLE po-ordl THEN po-ordl.cost      ELSE 0
                    dv-boardTotalCost = IF AVAILABLE po-ordl THEN po-ordl.t-cost    ELSE 0
                    .
               RUN pCalcPoMSF (OUTPUT dv-orderedMsf).
               IF dv-orderedMsf EQ ? THEN dv-orderedMsf = 0.
               dt-PORecDate = ?.

               IF oe-ordl.po-no-po NE 0 THEN
               FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company EQ oe-ordl.company 
                   AND fg-rcpth.rita-code EQ "R"
                   AND ( (fg-rcpth.trans-date GE dtStartReceiptDate 
                   AND fg-rcpth.trans-date LE dtStartReceiptDate AND lUseReceiptDate) OR NOT lUseReceiptDate)
                   AND fg-rcpth.po-no EQ STRING(oe-ordl.po-no-po),
                   EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                   AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code BY fg-rcpth.trans-date:      
                   ASSIGN dt-PORecDate = fg-rcpth.trans-date.
                   LEAVE.
               END.  /* end of for each fg-rcpth */

            END.  /*IF oe-ordl.po-no-po*/
            dv-boardProfit = dv-ext-price - dv-boardTotalCost .

            FIND itemfg NO-LOCK WHERE itemfg.company  EQ ipcCompany 
                AND itemfg.i-no EQ oe-ordl.i-no NO-ERROR.
            ASSIGN
                iv-jobShipQty  = 0
                dtv-FGShipDate = ?.

            IF oe-ordl.job-no NE "" THEN
            FOR EACH fg-rcpth OF itemfg NO-LOCK WHERE fg-rcpth.rita-code EQ "S"
                AND fg-rcpth.job-no EQ oe-ordl.job-no 
                AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                AND ((fg-rcpth.trans-date GE dtStartShipDate
                AND fg-rcpth.trans-date LE dtEndShipDate AND lUseShipDate)
                OR NOT lUseShipDate)
                ,
                EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
                BY fg-rcpth.trans-date:      
                ASSIGN
                    iv-jobShipQty  = iv-jobShipQty + fg-rdtlh.qty
                    dtv-FGShipDate = IF dtv-FGShipDate EQ ? THEN fg-rcpth.trans-date ELSE dtv-FGShipDate.
            END.  /* end of for each fg-rcpth */ 

            IF (oe-ordl.po-no-po NE 0 AND
                NOT CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.company EQ oe-ordl.company 
                             AND fg-rcpth.rita-code EQ "R"
                             AND fg-rcpth.trans-date GE dtStartReceiptDate
                             AND fg-rcpth.trans-date LE dtEndReceiptDate
                             AND fg-rcpth.po-no EQ STRING(oe-ordl.po-no-po) )
                AND lUseReceiptDate) OR
                (oe-ordl.job-no NE "" AND 
                 NOT CAN-FIND(FIRST fg-rcpth OF itemfg NO-LOCK WHERE fg-rcpth.rita-code EQ "S"
                              AND fg-rcpth.job-no EQ oe-ordl.job-no 
                              AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
                              AND fg-rcpth.trans-date GE dtStartShipDate
                              AND fg-rcpth.trans-date LE dtEndShipDate)
                 AND lUseShipDate )
                THEN NEXT.
            /* ==== new for Selectable columns =====*/
            FIND FIRST oe-boll NO-LOCK WHERE oe-boll.company EQ oe-ordl.company 
                AND oe-boll.ord-no EQ oe-ordl.ord-no
                AND oe-boll.i-no EQ oe-ordl.i-no NO-ERROR.
            iv-bol# = IF AVAILABLE oe-boll THEN oe-boll.bol-no ELSE 0.

            FIND FIRST ar-invl NO-LOCK WHERE ar-invl.company EQ oe-ordl.company
                AND ar-invl.ord-no EQ oe-ordl.ord-no
                AND ar-invl.i-no EQ oe-ordl.i-no NO-ERROR.
            iv-inv# = IF AVAILABLE ar-invl THEN ar-invl.inv-no ELSE 0.

            IF iv-inv# EQ 0  THEN DO:
                FIND FIRST inv-line NO-LOCK WHERE inv-line.company EQ oe-ordl.company
                    AND inv-line.ord-no EQ oe-ordl.ord-no
                    AND inv-line.i-no EQ oe-ordl.i-no NO-ERROR.
                iv-inv# = IF AVAILABLE inv-line THEN inv-line.inv-no ELSE 0.
            END.  /*IF iv-inv# = 0 */
            ASSIGN
                dv-Order%Profit = dv-boardProfit / dv-ext-price
                dv-MSFRec       = dv-orderedMsf / iv-boardpoQty * iv-boardTotalQty              
                dv-FGExtPrice   = oe-ordl.price / 1000 * iv-jobShipQty
                dv-PORecCost    = dv-boardTotalCost / iv-boardpoQty * iv-boardTotalQty
                dv-ProfitSold$  = dv-FGExtPrice - dv-PORecCost 
                dv-ProfitSold%  = dv-ProfitSold$ / dv-FGExtPrice
                iv-UnitsBoard   = iv-qty-lft / iv-boardpoQty
                dv-UnitLoss$    = iv-boardTotalQty * iv-UnitsBoard - iv-jobShipQty
                dv-Loss%        = dv-UnitLoss$ / (iv-boardTotalQty * iv-UnitsBoard )
                .
            IF dv-Order%Profit  EQ ? THEN dv-Order%Profit = 0.
            IF dv-MSFRec        EQ ? THEN dv-MSFRec = 0.
            IF dv-PORecCost     EQ ? THEN dv-PORecCost = 0.
            IF dv-ProfitSold$   EQ ? THEN dv-ProfitSold$ = 0.
            IF dv-ProfitSold%   EQ ? THEN dv-ProfitSold% = 0.
            IF iv-UnitsBoard    EQ ? THEN iv-UnitsBoard = 0.
            IF dv-UnitLoss$     EQ ? THEN dv-UnitLoss$ = 0.
            IF dv-Loss%         EQ ? THEN dv-Loss% = 0.

            PUT UNFORMATTED "ttOrdersBookedByOrderNo" SKIP.

            CREATE ttOrdersBookedByOrderNo .
            ASSIGN
                /* ttOrdersBookedByOrderNo.rowType          =*/
                ttOrdersBookedByOrderNo.orderNo          = oe-ord.ord-no         
                ttOrdersBookedByOrderNo.estNo            = oe-ordl.est-no        
                ttOrdersBookedByOrderNo.jobNo            = oe-ordl.job-no        
                ttOrdersBookedByOrderNo.orddate          = oe-ord.ord-date       
                ttOrdersBookedByOrderNo.custNo           = oe-ord.cust-no        
                ttOrdersBookedByOrderNo.custName         = oe-ord.cust-name      
                ttOrdersBookedByOrderNo.fgItem           = oe-ordl.i-no          
                ttOrdersBookedByOrderNo.fgItemName       = oe-ordl.i-name        
                ttOrdersBookedByOrderNo.fgOrderQty       = iv-qty-lft             
                ttOrdersBookedByOrderNo.fgCost           = oe-ordl.cost          
                ttOrdersBookedByOrderNo.price            = oe-ordl.price         
                ttOrdersBookedByOrderNo.uom              = oe-ordl.pr-uom        
                ttOrdersBookedByOrderNo.extPrice         = dv-ext-price           
                ttOrdersBookedByOrderNo.fgItemProfit     = dv-margin              
                ttOrdersBookedByOrderNo.poMsf            = dv-orderedMsf          
                ttOrdersBookedByOrderNo.fgShipped        = iv-jobShipQty          
                ttOrdersBookedByOrderNo.poProfit         = dv-boardProfit         
                ttOrdersBookedByOrderNo.poNo             = iv-boardPO             
                ttOrdersBookedByOrderNo.poQty            = iv-boardpoQty          
                ttOrdersBookedByOrderNo.poCost           = dv-boardCost           
                ttOrdersBookedByOrderNo.poTotalCost      = dv-boardTotalCost      
                ttOrdersBookedByOrderNo.poReceived       = iv-boardTotalQty       
                ttOrdersBookedByOrderNo.orderProfit      = dv-Order%Profit        
                ttOrdersBookedByOrderNo.msfReceived      = dv-MSFRec              
                ttOrdersBookedByOrderNo.fgShipDate       = dtv-FGShipDate          
                ttOrdersBookedByOrderNo.poRecDate        = dt-PORecDate           
                ttOrdersBookedByOrderNo.fgExtPrice       = dv-FGExtPrice          
                ttOrdersBookedByOrderNo.poRecCost        = dv-PORecCost           
                ttOrdersBookedByOrderNo.profSold         = dv-ProfitSold$         
                ttOrdersBookedByOrderNo.profSoldp        = dv-ProfitSold%         
                ttOrdersBookedByOrderNo.unitBoard        = iv-UnitsBoard          
                ttOrdersBookedByOrderNo.unitWaste        = dv-UnitLoss$           
                ttOrdersBookedByOrderNo.lossp            = dv-Loss%               
                ttOrdersBookedByOrderNo.bolNo            = iv-bol#                
                ttOrdersBookedByOrderNo.invoiceNo        = iv-inv#  .
            /*===== end of new ===== */
            IF lv-prt-cont THEN DO:
                IF dv-margin NE ? THEN dv-margin-tot = dv-margin-tot + dv-margin.
            END.  /*IF lv-prt-cont*/
            dv-tot-ord[1] = dv-tot-ord[1] + dv-ext-price.
        END.  /* each oe-ordl */

        IF lPrintMiscCharges THEN
        FOR EACH oe-ordm NO-LOCK WHERE oe-ordm.company EQ oe-ord.company
            AND oe-ordm.ord-no  EQ oe-ord.ord-no
            BREAK BY oe-ordm.ord-no:
            IF oe-ordm.bill EQ "Y" THEN DO:
                dv-tot-ord[1] = dv-tot-ord[1] + oe-ordm.amt.
            IF oe-ordm.tax AND dv-tax-rate EQ 0 THEN
              dv-tot-tax = dv-tot-tax + ROUND((oe-ordm.amt * dv-tax-rate) / 100,2).
            END.  /*IF oe-ordm.tax*/
        END.  /* each oe-ordm */   

        dv-tot-ord[2] = dv-tot-ord[2] + dv-tot-ord[1].    
    END.  /* each oe-ord */ 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBOLPackingList Procedure 
FUNCTION fBOLPackingList RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  BOL Packing List.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttBOLPackingList.

    RUN pBOLPackingList (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttBOLPackingList:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* BOL Packing List.rpa */
        WHEN "bolpcklst." THEN
        RETURN TEMP-TABLE ttBOLPackingList:HANDLE.
        /* Print Order Acknowledgements.rpa */
        WHEN "r-acknow." THEN
        RETURN TEMP-TABLE ttOrderAcknowledgements:HANDLE.
        /* Order Booked.rpa */
        WHEN "r-booked." THEN 
        RETURN TEMP-TABLE ttOrdersBooked:HANDLE.
        /* Order Booked By Order No.rpa */
        WHEN "r-booko#." THEN
        RETURN TEMP-TABLE ttOrdersBookedByOrderNo:HANDLE.
        /* Open Order Repoer.rpa */
        WHEN "r-ordopn." THEN
        RETURN TEMP-TABLE ttOpenOrderReport:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOpenOrderReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOpenOrderReport Procedure 
FUNCTION fOpenOrderReport RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose:  Open Order  Report.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOpenOrderReport.
    
    RUN pOpenOrderReport (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOpenOrderReport:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrderAcknowledgements) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrderAcknowledgements Procedure 
FUNCTION fOrderAcknowledgements RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose: OrderAcknowledgements.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrderAcknowledgements.
    
    RUN pOrderAcknowledgements (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrderAcknowledgements:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrdersBooked Procedure 
FUNCTION fOrdersBooked RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose:  Order Booked.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBooked.
    
    RUN pOrdersBooked (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrdersBooked:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOrdersBookedByOrderNo Procedure 
FUNCTION fOrdersBookedByOrderNo RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose:  Orders Booked By Order No.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBookedByOrderNo.
    
    RUN pOrdersBookedByOrderNo (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttOrdersBookedByOrderNo:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

