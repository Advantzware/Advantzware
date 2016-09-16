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

/* Invoice Post Update GL.rpa */
{aoaAppSrv/includes/ttInvoicePostUpdateGL.i}
/* Invoice Post Update GL.rpa */

/* Orders Booked.rpa */
DEFINE TEMP-TABLE ttOrdersBooked NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD salesRep     AS CHARACTER LABEL "Sales Rep"      FORMAT "X(3)"
    FIELD salesRepName AS CHARACTER LABEL "Sales Rep Name" FORMAT "X(30)"
    FIELD dueDate      AS DATE      LABEL "Due Date"       FORMAT 99/99/9999
    FIELD orderNo      AS INTEGER   LABEL "Order No"       FORMAT ">>>>>>>"
    FIELD custNo       AS CHARACTER LABEL "Cust No"        FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD commPer      AS DECIMAL   LABEL "Comm Pct"       FORMAT ">>>>>9.99"
    FIELD prodCode     AS CHARACTER LABEL "Prod Code"      FORMAT "x(8)"
    FIELD qtyOrdEa     AS INTEGER   LABEL "Qty Ordered"    FORMAT ">,>>>,>>9"
    FIELD custPartNo   AS CHARACTER LABEL "Customer Part"  FORMAT "x(15)" 
    FIELD fgItemNo     AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD fgItemName   AS CHARACTER LABEL "FG Item Name"   FORMAT "X(30)"
    FIELD sqFt         AS DECIMAL   LABEL "Sq Ft"          FORMAT ">>,>>>.999"
    FIELD totalSqft    AS DECIMAL   LABEL "Total SqFt"     FORMAT "->,>>>.999"
    FIELD msfPrice     AS DECIMAL   LABEL "MSF"            FORMAT "->>,>>9.99"
    FIELD price        AS DECIMAL   LABEL "Price"          FORMAT ">,>>>,>>9.99<<<<"
    FIELD orderAmount  AS DECIMAL   LABEL "Order Amount"   FORMAT "->,>>>,>>9.99"
    FIELD profitPer    AS DECIMAL   LABEL "Profit"         FORMAT "->,>>>,>>9.9"
    FIELD totalTons    AS DECIMAL   LABEL "Total Tons"     FORMAT "->,>>>.9"
    FIELD ton          AS DECIMAL   LABEL "Ton"            FORMAT "->>,>>9.99"
    FIELD custPO       AS CHARACTER LABEL "Cust PO"        FORMAT "x(15)"
    FIELD orderDate    AS DATE      LABEL "Order Date"     FORMAT 99/99/9999
    FIELD dieNo        AS CHARACTER LABEL "Die No"         FORMAT "x(15)"
    FIELD vUserID      AS CHARACTER LABEL "User ID"        FORMAT "x(8)"
    FIELD xxSort       AS CHARACTER LABEL "Sort"           FORMAT "x(100)"
        INDEX ttOrdersBooked IS PRIMARY rowType xxSort
        .

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD ord-no  LIKE oe-ord.ord-no
    FIELD line    LIKE oe-ordl.line
    FIELD sman    AS   CHARACTER 
    FIELD item-n  LIKE itemfg.i-name 
    FIELD proCat  LIKE itemfg.proCat 
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
    FIELD chk-inv      AS   LOGICAL INITIAL YES
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
    FIELD ackNo         AS INTEGER   LABEL "Acknowledgemwnt"    FORMAT ">>>>>>>>"
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
    FIELD compPhone     AS CHARACTER LABEL "Company Phone"      FORMAT "(999) 999-9999"
    FIELD compFax       AS CHARACTER LABEL "Company Fax"        FORMAT "(999) 999-9999" 
    FIELD compCustName  AS CHARACTER LABEL "Company  Name"      FORMAT "x(30)"
    FIELD compEmail     AS CHARACTER LABEL "Company email"      FORMAT "x(30)"
    FIELD estNo         AS CHARACTER LABEL "Estimate"           FORMAT "x(8)"
    FIELD prvOrder      AS DECIMAL   LABEL "Previous Order"     FORMAT ">>>>>>>>"
    FIELD csr           AS CHARACTER LABEL "CSR"                FORMAT "x(8)"
    FIELD over          AS DECIMAL   LABEL "Over"               FORMAT ">>9.99"
    FIELD under         AS DECIMAL   LABEL "Under"              FORMAT ">>9.99"
    FIELD poOrder       AS CHARACTER LABEL "Purchase Order"     FORMAT "x(15)"
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

/* Post BOL Create Invoice.rpa */
{aoaAppSrv/includes/ttPostBolCreateInvoice.i}
/* Post BOL Create Invoice.rpa */

/* Recap Product Category.rpa */
DEFINE TEMP-TABLE ttRecapProductCategory NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD proCat          AS CHARACTER LABEL "Cat"                  FORMAT "x(5)"
    FIELD catDscr         AS CHARACTER LABEL "Category Description" FORMAT "x(20)"
    FIELD numOrders       AS INTEGER   LABEL "Orders"               FORMAT ">>>>9"
    FIELD amountCurrent   AS DECIMAL   LABEL "Amount Current"       FORMAT "->>>,>>>,>>9.99"
    FIELD sqFtCurrent     AS DECIMAL   LABEL "SqFt Current"         FORMAT ">>>9.999<<<"
    FIELD priceMSFCurrent AS DECIMAL   LABEL "MSF Current"          FORMAT "->>>,>>>,>>9.99"
    FIELD amountPeriod    AS DECIMAL   LABEL "Amount Period"        FORMAT "->>>,>>>,>>9.99"
    FIELD sqFtPeriod      AS DECIMAL   LABEL "SqFt Period"          FORMAT ">>>9.999<<<"
    FIELD priceMSFPeriod  AS DECIMAL   LABEL "MSF Period"           FORMAT "->>>,>>>,>>9.99"
    FIELD dscr            AS CHARACTER LABEL "Description"          FORMAT "x(20)"
        INDEX ttRecapProductCategory IS PRIMARY rowType proCat
        .
/* Recap Product Category.rpa */

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

&IF DEFINED(EXCLUDE-fInvoicePostUpdateGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fInvoicePostUpdateGL Procedure 
FUNCTION fInvoicePostUpdateGL RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

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

&IF DEFINED(EXCLUDE-fPostBOLCreateInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPostBOLCreateInvoice Procedure 
FUNCTION fPostBOLCreateInvoice RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fRecapProductCategory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fRecapProductCategory Procedure 
FUNCTION fRecapProductCategory RETURNS HANDLE
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
         HEIGHT             = 23.57
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pBOLPackingList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBOLPackingList Procedure 
PROCEDURE pBOLPackingList :
/*------------------------------------------------------------------------------
  Purpose:     BOL Packing List.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/includes/pBOLPackingList.i}
    
    /* local variables */

    /* subject business logic */
    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company  EQ ipcCompany
          AND oe-bolh.bol-no   GE iStartBOL
          AND oe-bolh.bol-no   LE iEndBOL
          AND oe-bolh.bol-date GE dtStartBOLDate
          AND oe-bolh.bol-date LE dtEndBOLDate
          AND oe-bolh.cust-no  GE cStartCustNo
          AND oe-bolh.cust-no  LE cEndCustNo,
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

&IF DEFINED(EXCLUDE-pBuildttReport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pBuildttReport Procedure 
PROCEDURE pBuildttReport :
/*------------------------------------------------------------------------------
  Purpose:     Open Order Report.rpa
  Parameters:  Date, RecID, Primary Sort, Sort
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdtDate       AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER iprRecID       AS RECID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrimarySort AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSort        AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dtDueDate  LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE dtDueDate2 LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE cPONo      LIKE oe-ord.po-no     NO-UNDO.
    
    DEFINE BUFFER bARInvl  FOR ar-invl.
    DEFINE BUFFER bInvHead FOR inv-head.
    DEFINE BUFFER bInvLine FOR inv-line.
    DEFINE BUFFER bOERell  FOR oe-rell.
    DEFINE BUFFER bOEBoll  FOR oe-boll.
   
    cPONo = oe-ordl.po-no.
    CREATE tt-report.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ oe-ordl.company
           AND itemfg.i-no    EQ oe-ordl.i-no
         NO-ERROR.
    IF AVAILABLE itemfg THEN
    tt-report.cad-no = itemfg.cad-no.

    IF tt-report.cad-no EQ "" THEN DO:
        RELEASE eb.
        IF TRIM(oe-ordl.est-no) NE "" THEN
        FIND FIRST eb NO-LOCK
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.stock-no EQ oe-ordl.i-no
               AND eb.cad-no   NE ""
             USE-INDEX est-no NO-ERROR.
        IF NOT AVAILABLE eb THEN
        FIND FIRST eb NO-LOCK
             WHERE eb.company  EQ oe-ordl.company
               AND eb.stock-no EQ oe-ordl.i-no
               AND eb.cad-no   NE ""
             USE-INDEX stock NO-ERROR.
        IF AVAILABLE eb THEN
        tt-report.cad-no = eb.cad-no.
    END. /*IF tt-report.cad-no*/
    RELEASE eb.

    IF TRIM(oe-ordl.est-no) NE "" THEN DO:
        FIND FIRST eb NO-LOCK 
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.stock-no EQ oe-ordl.i-no
               AND eb.form-no  EQ oe-ordl.form-no
               AND eb.blank-no EQ oe-ordl.blank-no
             NO-ERROR.
        IF AVAILABLE eb THEN DO:
            ASSIGN
                tt-report.unit-count   = eb.cas-cnt
                tt-report.units-pallet = eb.cas-pal
                .
            RELEASE eb.
        END.  /*IF AVAIL eb*/
    END. /*IF TRIM(oe-ordl.est-no)*/

    ASSIGN
        tt-report.term-id  = ""
        tt-report.key-01   = IF ipcPrimarySort EQ "Due Date" OR ipcPrimarySort EQ "Rel Date" THEN
                             STRING(YEAR(dtDueDate),"9999")
                           + STRING(MONTH(dtDueDate),"99")
                           + STRING(DAY(dtDueDate),"99")
                        ELSE IF ipcPrimarySort EQ "Salesman" THEN oe-ordl.s-man[1]
                        ELSE ""
        tt-report.key-02   = oe-ord.cust-no
        tt-report.key-03   = IF ipcSort EQ "PO" THEN cPONo
                        ELSE IF ipcSort EQ "It" THEN (STRING(oe-ordl.i-no,"x(15)") + cPONo)
                        ELSE IF ipcSort EQ "Cu" THEN (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE IF ipcSort EQ "FG" THEN (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE IF ipcSort EQ "Or" THEN (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                        ELSE IF ipcSort EQ "CA" THEN (STRING(tt-report.cad-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE (STRING(YEAR(dtDueDate),"9999")
                           + STRING(MONTH(dtDueDate),"99")
                           + STRING(DAY(dtDueDate),"99")
                           + STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
        tt-report.key-04   = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no)))
                           + TRIM(oe-ordl.job-no) + "-"
                           + STRING(oe-ordl.job-no2,"99")
        tt-report.key-05   = STRING(oe-ord.ord-no,"99999999999")
        tt-report.key-06   = oe-ordl.i-no
        tt-report.key-07   = STRING(YEAR(ipdtDate),"9999")
                           + STRING(MONTH(ipdtDate),"99")
                           + STRING(DAY(ipdtDate),"99")
        tt-report.po-no    = cPONo
        tt-report.rec-id   = iprRecID
        tt-report.row-id   = ROWID(oe-ordl)
        tt-report.due-date = dtDueDate
        .
                                                
    FIND FIRST bARInvl NO-LOCK
         WHERE RECID(bARInvl) EQ iprRecID
         NO-ERROR.

    IF AVAILABLE bARInvl THEN
    ASSIGN
        tt-report.q-shp  = bARInvl.ship-qty
        tt-report.inv    = YES
        tt-report.inv-no = bARInvl.inv-no
        .

    FIND FIRST bInvLine  NO-LOCK
         WHERE RECID(bInvLine) EQ iprRecID
         NO-ERROR.

    IF AVAILABLE bInvLine THEN DO:
        FIND FIRST bInvHead NO-LOCK
             WHERE bInvHead.r-no EQ bInvLine.r-no
             NO-ERROR.
        ASSIGN
            tt-report.q-shp  = bInvLine.ship-qty
            tt-report.inv    = YES
            tt-report.inv-no = bInvHead.inv-no
            .
    END. /*IF AVAIL bInvLine*/

    FIND bOERell NO-LOCK
         WHERE RECID(bOERell) EQ iprRecID
         NO-ERROR.

    IF NOT tt-report.inv AND AVAILABLE bOERell THEN
    tt-report.q-rel = bOERell.qty.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCalcPOMSF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcPOMSF Procedure 
PROCEDURE pCalcPOMSF :
/*------------------------------------------------------------------------------
  Purpose:     Orders Booked by Order No.rpa
  Parameters:  Company, output Total MSF
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opTotalMsf AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dBasisW    AS DECIMAL            NO-UNDO. /* for po/po-adder2.p */
    DEFINE VARIABLE dLength  LIKE po-ordl.s-len      NO-UNDO.
    DEFINE VARIABLE dWidth   LIKE po-ordl.s-wid      NO-UNDO.
    DEFINE VARIABLE dv-dep   LIKE po-ordl.s-len      NO-UNDO.
    DEFINE VARIABLE iOrdQty  LIKE po-ordl.ord-qty    NO-UNDO.
    DEFINE VARIABLE cOrigUOM   AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE factor#    AS DECIMAL            NO-UNDO.
    DEFINE VARIABLE lEach      AS LOGICAL            NO-UNDO INITIAL NO.
    DEFINE VARIABLE iUOM     LIKE po-ordl.pr-qty-uom NO-UNDO INITIAL NO.
    DEFINE VARIABLE cFGUOMList AS CHARACTER          NO-UNDO.
    DEFINE VARIABLE iOutQty    AS INTEGER            NO-UNDO.
    DEFINE VARIABLE cocode     AS CHARACTER          NO-UNDO.
    
    FIND sys-ctrl NO-LOCK
         WHERE sys-ctrl.company EQ ipcCompany
           AND sys-ctrl.name    EQ "poprint" 
         NO-ERROR.
    factor# = IF AVAILABLE sys-ctrl AND CAN-DO("Premier,Middlesx,16th's",sys-ctrl.char-fld) THEN .16 ELSE 1.
    cocode = ipcCompany.
    {ce/msfcalc.i}
    
    FIND FIRST item NO-LOCK
         WHERE item.company EQ oe-ordl.company
           AND item.i-no    EQ po-ordl.i-no
         NO-ERROR.
    ASSIGN
        dBasisW  = IF AVAILABLE item THEN item.basis-w ELSE dBasisW
        dv-dep   = IF AVAILABLE item THEN item.s-dep   ELSE dv-dep
        dLength  = po-ordl.s-len
        dWidth   = po-ordl.s-wid
        iOrdQty  = po-ordl.ord-qty
        cOrigUOM = po-ordl.pr-qty-uom 
        {po/calc10.i dLength} 
        {po/calc10.i dWidth}
        .
    IF NOT AVAILABLE item THEN
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ ipcCompany
           AND itemfg.i-no    EQ po-ordl.i-no
         NO-ERROR.
    IF AVAILABLE itemfg THEN
    RUN sys/ref/ea-um-fg.p (po-ordl.pr-qty-uom, OUTPUT lEach).
    IF lEach THEN ASSIGN iUOM = po-ordl.pr-qty-uom. 

    IF dLength EQ 0 AND AVAILABLE item AND
       ITEM.i-code EQ "R" AND item.r-wid GT 0 THEN DO:
        dLength = 12.
        IF cOrigUOM EQ "ROLL" THEN DO:
            FIND FIRST uom NO-LOCK
                 WHERE uom.uom EQ "ROLL"
                 NO-ERROR.
            IF AVAILABLE uom THEN
            iOrdQty = iOrdQty * uom.mult.
        END.  /*IF cOrigUOM*/ 
    END.  /*IF dLength EQ 0*/

    RUN sys/ref/uom-fg.p (?, OUTPUT cFGUOMList).

    IF po-ordl.pr-qty-uom{2} EQ "EA" OR
      (NOT po-ordl.item-type AND
       LOOKUP(po-ordl.pr-qty-uom,cFGUOMList) GT 0) THEN
    opTotalMsf = IF v-corr THEN ((dLength * dWidth * .007 * DEC(po-ordl.ord-qty{2})) / 1000)
                 ELSE ((((dLength * dWidth) / 144) * DEC(po-ordl.ord-qty{2})) / 1000).
    ELSE DO:
        /*convert whatever the UOM is into "EACH" first*/
        opTotalMsf = 0.
        IF po-ordl.pr-qty-uom NE "EA" THEN DO:
            opTotalMsf = 0.
            RUN sys/ref/convquom.p
                (po-ordl.pr-qty-uom,
                 "EA",
                 dBasisW,
                 dLength,
                 dWidth,
                 dv-dep,
                 iOrdQty,
                 OUTPUT iOutQty
                 ).
            /*now convert from "EACH" into MSF*/   
            opTotalMsf = IF v-corr THEN ((dLength * dWidth * .007 * iOutQty) / 1000)
                         ELSE ((((dLength * dWidth) / 144) * iOutQty) / 1000).
            IF po-ordl.pr-qty-uom EQ "ROLL" THEN
            opTotalMsf = OpTotalMsf * (12 / dLength).
        END. /* if po-ordl.pr-qty-uom ne */
    END. /* else */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pCalcQOH) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcQOH Procedure 
PROCEDURE pCalcQOH :
/*------------------------------------------------------------------------------
  Purpose:     Open Order Report.rpa
  Parameters:  Company
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

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

&IF DEFINED(EXCLUDE-pInvoicePostUpdateGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInvoicePostUpdateGL Procedure 
PROCEDURE pInvoicePostUpdateGL :
/*------------------------------------------------------------------------------
  Purpose:     Invoice Post Update GL.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/aoaInputDefParams.i}
    
    /* local variables */

    /* subject business logic */
    RUN aoaAppSrv/aoaBL/r-inve&pb.p (OUTPUT TABLE ttInvoicePostUpdateGL, ipcCompany, ipiBatch, ipcUserID).

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
    {aoaAppSrv/includes/pOpenOrderReport.i}

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

    /* subject business logic */
    ASSIGN
        cStartJobNo = FILL(" ",6 - LENGTH(TRIM(cStartJobNo))) + TRIM(cStartJobNo) + STRING(INT(iStartJobNo2),"99")
        cEndJobNo   = FILL(" ",6 - LENGTH(TRIM(cEndJobNo))) + TRIM(cEndJobNo) + STRING(INT(iEndJobNo2),"99") 
        cStat       = SUBSTRING(cJobStatus,1,2)
        cSort        = SUBSTRING(cPrimarySort-2,1,2)
        cOrderStatus = SUBSTRING(cOrderStatus,1,1)
        lInc         = lIncludeZeroOrderBalanceItems  
        .

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
           OR (oe-ord.opened
          AND cOrderStatus    EQ "O")
           OR (NOT oe-ord.opened
          AND cOrderStatus    EQ "C"))
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
        {aoaAppSrv/includes/iOpenOrderReport.i}
    END. /*  each oe-ord  */

    FOR EACH tt-report NO-LOCK 
        WHERE tt-report.term-id EQ "",
        FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl) EQ tt-report.row-id,
        FIRST oe-ord OF oe-ordl 
        BREAK BY tt-report.row-id
              BY tt-report.key-07
        :
        ASSIGN
            iQtyShp = iQtyShp + tt-report.q-shp
            iQtyRel = iQtyRel + tt-report.q-rel
            .

        IF LAST-OF(tt-report.row-id) THEN DO:
            IF NOT CAN-FIND(FIRST tt-fg-bin
                            WHERE tt-fg-bin.company EQ ipcCompany
                              AND tt-fg-bin.i-no    EQ oe-ordl.i-no) THEN
            RUN pCalcQOH (ipcCompany).

            FOR EACH tt-fg-bin
                WHERE tt-fg-bin.company EQ oe-ordl.company
                  AND tt-fg-bin.i-no    EQ oe-ordl.i-no
                  AND tt-fg-bin.job-no  EQ oe-ordl.job-no
                  AND tt-fg-bin.job-no2 EQ oe-ordl.job-no2
                :
                ASSIGN
                    tt-report.q-onh  = tt-report.q-onh + tt-fg-bin.qty
                    tt-fg-bin.ord-no = oe-ord.ord-no
                    .
            END. /*  end of for each tt-fg-bin */

            IF lIncludeJobsQOH THEN
            FOR EACH job-hdr NO-LOCK 
                WHERE job-hdr.company EQ oe-ordl.company
                  AND job-hdr.ord-no  EQ oe-ordl.ord-no
                  AND job-hdr.i-no    EQ oe-ordl.i-no
                  AND (job-hdr.job-no NE oe-ordl.job-no
                   OR job-hdr.job-no2 NE oe-ordl.job-no2)
                BREAK BY job-hdr.job-no
                      BY job-hdr.job-no2
                      BY job-hdr.i-no
                :
                IF FIRST-OF(job-hdr.i-no) THEN
                FOR EACH tt-fg-bin 
                    WHERE tt-fg-bin.company EQ job-hdr.company
                      AND tt-fg-bin.i-no    EQ job-hdr.i-no
                      AND tt-fg-bin.job-no  EQ job-hdr.job-no
                      AND tt-fg-bin.job-no2 EQ job-hdr.job-no2
                    :
                    ASSIGN
                        tt-report.q-onh  = tt-report.q-onh + tt-fg-bin.qty
                        tt-fg-bin.ord-no = oe-ord.ord-no
                        .
                END. /* each tt-fg-bin */
            END. /* each job-hdr */

            ASSIGN
                tt-report.q-shp = iQtyShp
                tt-report.q-rel = iQtyRel
                .
            IF cWIPQty EQ "1" THEN
            tt-report.q-wip = oe-ordl.qty - (tt-report.q-onh + tt-report.q-shp).
            ELSE DO:
                ASSIGN
                    dJobQty = 0
                    dRecQty = 0
                    .
                FIND FIRST job NO-LOCK 
                     WHERE job.company EQ ipcCompany
                       AND job.job-no  EQ oe-ordl.job-no
                       AND job.job-no2 EQ oe-ordl.job-no2
                     NO-ERROR.
                IF AVAILABLE job THEN DO:
                    IF NOT job.opened THEN
                    tt-report.q-wip = 0.
                    ELSE DO:
                        FOR EACH job-hdr FIELDS(qty) NO-LOCK
                            WHERE job-hdr.company EQ oe-ordl.company
                              AND job-hdr.ord-no  EQ oe-ordl.ord-no
                              AND job-hdr.i-no    EQ oe-ordl.i-no
                              AND job-hdr.job-no  EQ oe-ordl.job-no
                              AND job-hdr.job-no2 EQ oe-ordl.job-no2
                            :
                            dJobQty = dJobQty + job-hdr.qty.
                        END.  /* each job-hdr */
                        FIND FIRST itemfg NO-LOCK 
                             WHERE itemfg.company EQ job.company
                               AND itemfg.i-no    EQ oe-ordl.i-no
                             NO-ERROR.
                        IF AVAILABLE itemfg THEN DO:
                            IF itemfg.isaset AND itemfg.alloc THEN
                            FOR EACH fg-act FIELDS(qty) NO-LOCK
                                WHERE fg-act.company EQ job.company
                                  AND fg-act.job-no  EQ oe-ordl.job-no
                                  AND fg-act.job-no2 EQ oe-ordl.job-no2
                                  AND fg-act.i-no    EQ oe-ordl.i-no
                                :
                                dRecQty = dRecQty + fg-act.qty.
                            END. /* each fg-act */
                            FOR EACH fg-rcpth FIELDS(r-no rita-code company) NO-LOCK
                                WHERE fg-rcpth.company   EQ job.company
                                  AND fg-rcpth.i-no      EQ oe-ordl.i-no
                                  AND fg-rcpth.job-no    EQ oe-ordl.job-no
                                  AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                                  AND fg-rcpth.rita-code EQ "R",
                                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                                WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                  AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                BREAK BY fg-rcpth.company
                                :
                                IF FIRST(fg-rcpth.company) THEN dRecQty = 0.
                                dRecQty = dRecQty + fg-rdtlh.qty.
                            END. /* each fg-rcpth */
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
                iQtyRel = 0
                .
        END. /* IF LAST-OF(tt-report.row-id) */
        ELSE DELETE tt-report.
    END. /* each tt-report*/

    FOR EACH tt-report NO-LOCK 
        WHERE tt-report.term-id EQ ""
          AND tt-report.cad-no  GE cStartCAD 
          AND tt-report.cad-no  LE cEndCAD   
          AND (lIncludeZeroQtyWIPItems
           OR tt-report.q-wip   GT 0)
          AND (lIncludeZeroQtyActReleaseQty
           OR tt-report.q-avl   GT 0
           OR tt-report.q-rel   GT 0),
        FIRST itemfg
        WHERE itemfg.company    EQ ipcCompany
        AND itemfg.i-no         EQ tt-report.key-06,
        FIRST cust NO-LOCK
        WHERE cust.company      EQ ipcCompany
          AND cust.cust-no      EQ tt-report.key-02,
        FIRST oe-ordl NO-LOCK 
        WHERE ROWID(oe-ordl)    EQ tt-report.row-id,
        FIRST oe-ord OF oe-ordl 
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.key-04
              BY tt-report.key-05
              BY tt-report.key-06
              BY tt-report.row-id
              BY tt-report.key-07
        :
        ASSIGN dtDueDate2 = ?.
        FOR EACH oe-rel NO-LOCK 
           WHERE oe-rel.company EQ oe-ordl.company
             AND oe-rel.ord-no  EQ oe-ordl.ord-no
             AND oe-rel.i-no    EQ oe-ordl.i-no
             AND oe-rel.line    EQ oe-ordl.line
           BY oe-rel.rel-date DESCENDING
           :
           dtDueDate2 = IF AVAILABLE oe-relh THEN oe-relh.rel-date
                                             ELSE oe-rel.rel-date.
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
    {aoaAppSrv/includes/pOrdersBooked.i}
    {aoaAppSrv/includes/pOrdersBookedLogic.i}

    lPrtSqft = CAN-DO(cSelectedColumns,"sqFt").
    RUN pOrdersBooked1 (ipcCompany,
                        lPrtSqft,
                        lPrintOrderUnderPct,
                        lPrintOrderOverPct,
                        iUnderValue,
                        iOverValue
                        ).
    RUN pOrdersBooked2 (ipcCompany,
                        lPrtSqft,
                        lPrintOrderUnderPct,
                        lPrintOrderOverPct,
                        iUnderValue,
                        iOverValue
                        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBooked1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBooked1 Procedure 
PROCEDURE pOrdersBooked1 :
/*------------------------------------------------------------------------------
Purpose:     OrdersBooked.rpa
Parameters:  Company, PrtSqft, PrintOrderUnderPct, PrintOrderOverPct, UnderValue, OverValue
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrtSqft            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderUnderPct AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderOverPct  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiUnderValue         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOverValue          AS INTEGER   NO-UNDO.

    /* local variables */
    DEFINE VARIABLE i            AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dPct         AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dRevenue     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE dProfitPer   AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dQM          AS   DECIMAL          NO-UNDO.
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01
              BY tt-report.key-02
        :
        FIND FIRST oe-ordm NO-LOCK
             WHERE RECID(oe-ordm) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-ordm THEN
        ASSIGN
            i            = INTEGER(tt-report.key-03)
            dPct         = oe-ordm.s-pct[i] / 100
            dPriceAmount = oe-ordm.amt * dPct
            dRevenue     = dPriceAmount
            dProfitPer   = (dRevenue - (oe-ordm.cost * dPct)) / dRevenue * 100
            .
        ELSE DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE RECID(oe-ordl) EQ tt-report.rec-id
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
            ASSIGN
                i            = INTEGER(tt-report.key-03)
                dPct         = oe-ordl.s-pct[i] / 100
                dOrdQty      = oe-ordl.qty * dPct
                dPriceAmount = oe-ordl.t-price * dPct
                dQM          = oe-ordl.qty / 1000
                dRevenue     = dPriceAmount
                dProfitPer   = (dRevenue - (oe-ordl.cost * dQM)) / dRevenue * 100
                .
        END. /* else do */
        
        IF dProfitPer EQ ? THEN dProfitPer = 0.
        
        IF iplPrintOrderUnderPct AND iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue AND dProfitPer LE ipiOverValue THEN DELETE tt-report.
        END.
        ELSE IF iplPrintOrderUnderPct AND NOT iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue THEN DELETE tt-report.
        END.
        ELSE IF iplPrintOrderOverPct AND NOT iplPrintOrderUnderPct THEN DO:
            IF dProfitPer LE ipiOverValue THEN DELETE tt-report.
        END.
    END.  /* for each tt-report */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBooked2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBooked2 Procedure 
PROCEDURE pOrdersBooked2 :
/*------------------------------------------------------------------------------
Purpose:     OrdersBooked.rpa
Parameters:  Company, PrtSqft, PrintOrderUnderPct, PrintOrderOverPct, UnderValue, OverValue
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrtSqft            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderUnderPct AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderOverPct  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiUnderValue         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOverValue          AS INTEGER   NO-UNDO.

    /* local variables */
    DEFINE VARIABLE i            AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dPct         AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dTons        AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dSqft        AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE iLines       AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dQM          AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dRevenue     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE dMSFPrice    AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPricePerTon AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dProfitPer   AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dMargin      AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE idx AS INTEGER     NO-UNDO.

    DEFINE BUFFER bItemFG FOR itemfg.
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01
              BY tt-report.key-02
        :
        FIND FIRST oe-ordm NO-LOCK
             WHERE RECID(oe-ordm) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-ordm THEN DO:
            FIND FIRST oe-ord OF oe-ordm NO-LOCK.
            ASSIGN
                i            = INTEGER(tt-report.key-03)
                dPct         = oe-ordm.s-pct[i] / 100
                dPriceAmount = oe-ordm.amt * dPct
                .
            CREATE w-data.
            ASSIGN
                w-data.sman    = tt-report.key-01
                w-data.ord-no  = oe-ordm.ord-no
                w-data.line    = oe-ordm.line
                w-data.misc    = YES
                w-data.proCat  = "P/M"
                w-data.qty     = 0
                w-data.sqft    = 0
                w-data.t-sqft  = 0
                w-data.t-tons  = 0
                w-data.item-n  = oe-ordm.dscr
                w-data.cost    = oe-ordm.cost * dPct
                w-data.price   = dPriceAmount
                w-data.revenue = dPriceAmount
                w-data.comm    = oe-ordm.s-comm[i]
                .
            FIND FIRST prep NO-LOCK
                 WHERE prep.company EQ oe-ordm.company
                   AND prep.code    EQ oe-ordm.charge
                 NO-ERROR.
            IF AVAILABLE prep THEN
            w-data.proCat = IF prep.fgcat NE "" THEN prep.fgcat ELSE "P".
            ELSE w-data.proCat = "M".
        END. /* avail oe-ordm */
        ELSE DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE RECID(oe-ordl) EQ tt-report.rec-id
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN DO:
                FIND FIRST oe-ord OF oe-ordl NO-LOCK.
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ ipcCompany
                       AND itemfg.i-no    EQ oe-ordl.i-no
                     NO-ERROR.
                ASSIGN
                    i            = INTEGER(tt-report.key-03)
                    dPct         = oe-ordl.s-pct[i] / 100
                    dOrdQty      = oe-ordl.qty * dPct
                    dPriceAmount = oe-ordl.t-price * dPct
                    dTons        = IF AVAILABLE itemfg THEN (itemfg.weight-100 * dOrdQty / 100 / 2000) ELSE 0
                    .
                IF AVAILABLE itemfg AND itemfg.isaset THEN DO:
                   dSqft = 0.
                   FOR EACH fg-set FIELDS(part-no part-qty) NO-LOCK
                       WHERE fg-set.company EQ itemfg.company
                         AND fg-set.set-no  EQ itemfg.i-no,
                       FIRST bItemFG FIELDS(t-sqft) NO-LOCK
                       WHERE bItemFG.company EQ itemfg.company
                         AND bItemFG.i-no EQ fg-set.part-no
                       :
                       dSqft = dSqft + (dOrdQty
                             * (IF fg-set.part-qty GE 0 THEN fg-set.part-qty
                                ELSE (-1 / fg-set.part-qty))
                             * bItemFG.t-sqft / 1000).
                   END. /* each fg-set */
                END.
                ELSE dSqft = IF AVAILABLE itemfg THEN (itemfg.t-sqft * dOrdQty / 1000) ELSE 0.
                CREATE w-data.
                ASSIGN
                    w-data.sman   = tt-report.key-01
                    w-data.ord-no = oe-ordl.ord-no
                    w-data.line   = oe-ordl.line
                    w-data.misc   = NO
                    iLines        = iLines + 1
                    dQM           = oe-ordl.qty / 1000
                    w-data.proCat = IF AVAILABLE itemfg THEN itemfg.proCat ELSE ""
                    w-data.item-n = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
                    w-data.qty    = dOrdQty
                    w-data.margin = oe-ordl.q-qty
                    .
                IF NOT oe-ordl.is-a-component THEN
                ASSIGN
                    w-data.sqft    = IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE 0
                    w-data.t-sqft  = dSqft
                    w-data.t-tons  = dTons
                    w-data.price   = oe-ordl.price
                    w-data.revenue = dOrdQty
                    w-data.cost    = oe-ordl.cost * dQM
                    w-data.comm    = oe-ordl.s-comm[i]
                    .
            END. /* avail oe-ordl */
        END. /* else do */

        FIND FIRST oe-ordl NO-LOCK
             WHERE RECID(oe-ordl) EQ tt-report.rec-id
             NO-ERROR.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ ipcCompany
               AND oe-ord.ord-no  EQ w-data.ord-no
             NO-ERROR.
        FIND cust OF oe-ord NO-LOCK NO-ERROR.
        
        ASSIGN
            dRevenue     = w-data.revenue
            dMSFPrice    = dRevenue / w-data.t-sqft
            dPricePerTon = dRevenue / w-data.t-tons
            dProfitPer   = (dRevenue - w-data.cost) / dRevenue * 100
            dMargin      = w-data.margin
            .
        IF dMSFPrice    EQ ? THEN dMSFPrice    = 0.
        IF dPricePerTon EQ ? THEN dPricePerTon = 0.
        IF dProfitPer   EQ ? THEN dProfitPer   = 0.
        IF dMargin      EQ ? THEN dMargin      = 0.
        
        ACCUMULATE
            w-data.t-sqft (TOTAL BY tt-report.key-01)
            w-data.t-tons (TOTAL BY tt-report.key-01)
            dRevenue      (TOTAL BY tt-report.key-01)
            w-data.cost   (TOTAL BY tt-report.key-01)
            .
        IF AVAILABLE oe-ordl THEN DO:
            RELEASE eb.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ipcCompany
                   AND itemfg.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            IF NOT AVAILABLE itemfg OR itemfg.die-no EQ "" THEN
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ ipcCompany
                   AND eb.est-no   EQ oe-ordl.est-no
                   AND eb.stock-no EQ oe-ordl.i-no
                 NO-ERROR.
        END. /* avail oe-ordl  */

        IF iplPrintOrderUnderPct AND iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue AND dProfitPer LE ipiOverValue THEN NEXT.
        END.
        ELSE IF iplPrintOrderUnderPct AND NOT iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue THEN NEXT.
        END.
        ELSE IF iplPrintOrderOverPct AND NOT iplPrintOrderUnderPct THEN DO:
            IF dProfitPer LE ipiOverValue THEN NEXT.
        END.

        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ ipcCompany
               AND sman.sman    EQ w-data.sman
             NO-ERROR.

        CREATE ttOrdersBooked.
        ASSIGN 
            ttOrdersBooked.dueDate      = oe-ord.due-date                    
            ttOrdersBooked.orderNo      = w-data.ord-no                      
            ttOrdersBooked.custName     = IF AVAILABLE cust THEN cust.name ELSE ""
            ttOrdersBooked.custNo       = oe-ord.cust-no
            ttOrdersBooked.salesRep     = IF AVAILABLE sman THEN sman.sman ELSE ""
            ttOrdersBooked.salesRepName = IF AVAILABLE sman THEN sman.sname ELSE "" 
            ttOrdersBooked.commPer      = w-data.comm                      
            ttOrdersBooked.prodCode     = w-data.proCat 
            ttOrdersBooked.fgItemNo     = IF AVAILABLE oe-ordl THEN oe-ordl.i-no ELSE ""
            ttOrdersBooked.fgItemName   = w-data.item-n
            ttOrdersBooked.qtyOrdEa     = w-data.qty                         
            ttOrdersBooked.sqFt         = w-data.sqft                     
            ttOrdersBooked.totalSqft    = w-data.t-sqft                          
            ttOrdersBooked.msfPrice     = dMSFPrice                   
            ttOrdersBooked.price        = w-data.price                     
            ttOrdersBooked.orderAmount  = dRevenue                       
            ttOrdersBooked.profitPer    = dProfitPer                       
            ttOrdersBooked.totalTons    = w-data.t-tons                          
            ttOrdersBooked.ton          = dPricePerTon
            ttOrdersBooked.custPO       = IF AVAILABLE oe-ordl AND oe-ordl.cust-no NE "" THEN oe-ordl.po-no ELSE oe-ord.po-no
            ttOrdersBooked.orderDate    = oe-ord.ord-date
            ttOrdersBooked.vUserID      = oe-ord.user-id                  
            ttOrdersBooked.custPartNo   = IF AVAILABLE oe-ordl THEN oe-ordl.part-no ELSE ""
            ttOrdersBooked.dieNo        = IF AVAILABLE itemfg AND itemfg.die-no NE "" THEN itemfg.die-no
                                     ELSE IF AVAILABLE eb THEN eb.die-no
                                     ELSE ""
            ttOrdersBooked.xxSort       = ttOrdersBooked.salesRep
                                        + STRING(ttOrdersBooked.dueDate,"99/99/9999")
                                        + STRING(ttOrdersBooked.orderNo)
            . 
        DELETE w-data.
    END.  /* for each tt-report */

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
    {aoaAppSrv/includes/pOrdersBookedByOrderNo.i}

    /* local variables */
    DEFINE VARIABLE lOrdQty         AS LOGICAL          NO-UNDO INITIAL YES.
    DEFINE VARIABLE dTotOrd         AS DECIMAL          NO-UNDO EXTENT 2.
    DEFINE VARIABLE dTaxRate        AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE lShip           AS LOGICAL          NO-UNDO INITIAL NO.
    DEFINE VARIABLE dTotTax       LIKE oe-ord.tax       NO-UNDO.
    DEFINE VARIABLE dTotFreight   LIKE oe-ord.t-freight NO-UNDO.
    DEFINE VARIABLE iQtyLft       LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dExtPrice     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE lPrtCont        AS LOGICAL          NO-UNDO INITIAL NO.
    DEFINE VARIABLE dMargin         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dMarginTot      AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dExtCost        AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrderedMSF     AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iJobShipQty     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE dBoardProfit    AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iBoardPO        AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iBoardPOQty     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE dBoardCost      AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dBoardTotalCost AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iBoardTotalQty  AS INTEGER          NO-UNDO.
    DEFINE VARIABLE dOrder%Profit   AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dMSFRec         AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dtFGShipDate    AS DATE             NO-UNDO.
    DEFINE VARIABLE dtPORecDate     AS DATE             NO-UNDO.
    DEFINE VARIABLE dFGExtPrice     AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPORecCost      AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dProfitSold$    AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dProfitSold%    AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iUnitsBoard     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE dUnitLoss$      AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dLoss%          AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE iBOL#           AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iInv#           AS INTEGER          NO-UNDO.

    DEFINE BUFFER bOEOrdl FOR oe-ordl.

    /* subject business logic */
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company  EQ ipcCompany
          AND oe-ord.ord-no   GE iStartOrderNo
          AND oe-ord.ord-no   LE iEndOrderNo
          AND oe-ord.cust-no  GE cStartCustNo
          AND oe-ord.cust-no  LE cEndCustNo
          AND oe-ord.ord-date GE dtStartOrderDate
          AND oe-ord.ord-date LE dtEndOrderDate
          AND oe-ord.stat     NE "D"
          AND oe-ord.type     NE "T"
        USE-INDEX ord-no,
        FIRST bOEOrdl NO-LOCK
        WHERE bOEOrdl.company EQ ipcCompany
          AND bOEOrdl.ord-no  EQ oe-ord.ord-no
          AND bOEOrdl.i-no    GE cStartItemNo
          AND bOEOrdl.i-no    LE cEndItemNo,
        FIRST cust NO-LOCK
        WHERE (cust.company EQ ipcCompany) 
          AND cust.cust-no  EQ oe-ord.cust-no
        BREAK BY oe-ord.ord-no
        :
        dTotOrd[1] = 0.

        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ oe-ord.company
              AND oe-ordl.ord-no  EQ oe-ord.ord-no
              AND oe-ordl.i-no    GE cStartItemNo
              AND oe-ordl.i-no    LE cEndItemNo
            BREAK BY oe-ordl.ord-no
            :
            ASSIGN
                lShip      = oe-ordl.stat NE "I" AND oe-ordl.stat NE "B"
                iQtyLft   = oe-ordl.qty - (IF lOrdQty THEN 0 ELSE oe-ordl.inv-qty)
                dExtPrice = 0
                .
            IF iQtyLft LT 0 THEN iQtyLft = 0.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ipcCompany
                   AND itemfg.i-no EQ oe-ordl.i-no
                 NO-ERROR.
            RUN oe/GetPriceTotal.p
                (oe-ordl.qty,
                 oe-ordl.price,
                 oe-ordl.pr-uom,
                (IF AVAILABLE itemfg THEN itemfg.case-count ELSE 0),
                 oe-ordl.disc,
                 OUTPUT dExtPrice
                 ).
            dTotFreight = dTotFreight
                           + (ROUND(oe-ordl.t-freight / oe-ordl.qty, 2)
                           * iQtyLft).
            /** CALCULATE TAX CHARGES **/
            IF oe-ordl.tax AND dTaxRate GT 0 THEN
            dTotTax = dTotTax + ROUND((dExtPrice * dTaxRate) / 100,2).
            /*if lPrtCont then */
            ASSIGN
                dExtCost        = (oe-ordl.cost * oe-ordl.qty) / 1000
                dMargin         = dExtPrice - dExtCost
                iBoardTotalQty  = 0
                iBoardPO        = 0
                iBoardPOQty     = 0
                dBoardCost      = 0
                dBoardTotalCost = 0
                dBoardProfit    = 0
                dOrderedMSF     = 0
                .
            IF oe-ordl.po-no-po NE 0 THEN DO:
                FIND FIRST po-ordl NO-LOCK
                     WHERE po-ordl.company      EQ oe-ordl.company 
                       AND po-ordl.po-no        EQ oe-ordl.po-no-po
                       AND ((po-ordl.item-type  EQ YES
                       AND TRIM(oe-ordl.job-no) NE ""
                       AND po-ordl.job-no       EQ oe-ordl.job-no
                       AND po-ordl.job-no2      EQ oe-ordl.job-no2)   
                        OR (po-ordl.item-type   EQ NO
                       AND po-ordl.i-no         EQ oe-ordl.i-no))
                     NO-ERROR.
                ASSIGN
                    iBoardTotalQty  = IF AVAILABLE po-ordl THEN po-ordl.t-rec-qty ELSE 0
                    iBoardPO        = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po  ELSE 0
                    iBoardPOQty     = IF AVAILABLE po-ordl THEN po-ordl.ord-qty   ELSE 0
                    dBoardCost      = IF AVAILABLE po-ordl THEN po-ordl.cost      ELSE 0
                    dBoardTotalCost = IF AVAILABLE po-ordl THEN po-ordl.t-cost    ELSE 0
                    .
               RUN pCalcPoMSF (ipcCompany,OUTPUT dOrderedMSF).
               IF dOrderedMSF EQ ? THEN dOrderedMSF = 0.
               dtPORecDate = ?.

               IF oe-ordl.po-no-po NE 0 THEN
               FOR EACH fg-rcpth NO-LOCK
                   WHERE fg-rcpth.company      EQ oe-ordl.company 
                     AND fg-rcpth.rita-code    EQ "R"
                     AND ((fg-rcpth.trans-date GE dtStartReceiptDate 
                     AND fg-rcpth.trans-date   LE dtStartReceiptDate AND lUseReceiptDate) OR NOT lUseReceiptDate)
                     AND fg-rcpth.po-no        EQ STRING(oe-ordl.po-no-po),
                   EACH fg-rdtlh NO-LOCK
                   WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                     AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                   BY fg-rcpth.trans-date
                   :
                   ASSIGN dtPORecDate = fg-rcpth.trans-date.
                   LEAVE.
               END.  /* end of for each fg-rcpth */
            END.  /*IF oe-ordl.po-no-po*/
            dBoardProfit = dExtPrice - dBoardTotalCost.

            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ipcCompany 
                   AND itemfg.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            ASSIGN
                iJobShipQty  = 0
                dtFGShipDate = ?
                .
            IF oe-ordl.job-no NE "" THEN
            FOR EACH fg-rcpth OF itemfg NO-LOCK
                WHERE fg-rcpth.rita-code    EQ "S"
                  AND fg-rcpth.job-no       EQ oe-ordl.job-no 
                  AND fg-rcpth.job-no2      EQ oe-ordl.job-no2
                  AND ((fg-rcpth.trans-date GE dtStartShipDate
                  AND fg-rcpth.trans-date   LE dtEndShipDate
                  AND lUseShipDate)
                   OR NOT lUseShipDate),
                EACH fg-rdtlh NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                  AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
                BY fg-rcpth.trans-date
                : 
                ASSIGN
                    iJobShipQty  = iJobShipQty + fg-rdtlh.qty
                    dtFGShipDate = IF dtFGShipDate EQ ? THEN fg-rcpth.trans-date
                                                            ELSE dtFGShipDate
                    .
            END.  /* end of for each fg-rcpth */ 

            IF (oe-ordl.po-no-po NE 0 AND
                NOT CAN-FIND(FIRST fg-rcpth
                             WHERE fg-rcpth.company    EQ oe-ordl.company 
                               AND fg-rcpth.rita-code  EQ "R"
                               AND fg-rcpth.trans-date GE dtStartReceiptDate
                               AND fg-rcpth.trans-date LE dtEndReceiptDate
                               AND fg-rcpth.po-no      EQ STRING(oe-ordl.po-no-po))
                               AND lUseReceiptDate)
                                OR (oe-ordl.job-no     NE "" AND 
                NOT CAN-FIND(FIRST fg-rcpth OF itemfg
                             WHERE fg-rcpth.rita-code  EQ "S"
                               AND fg-rcpth.job-no     EQ oe-ordl.job-no 
                               AND fg-rcpth.job-no2    EQ oe-ordl.job-no2
                               AND fg-rcpth.trans-date GE dtStartShipDate
                               AND fg-rcpth.trans-date LE dtEndShipDate)
                               AND lUseShipDate) THEN NEXT.
            /* ==== new for Selectable columns =====*/
            FIND FIRST oe-boll NO-LOCK
                 WHERE oe-boll.company EQ oe-ordl.company 
                   AND oe-boll.ord-no  EQ oe-ordl.ord-no
                   AND oe-boll.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            iBOL# = IF AVAILABLE oe-boll THEN oe-boll.bol-no ELSE 0.

            FIND FIRST ar-invl NO-LOCK
                 WHERE ar-invl.company EQ oe-ordl.company
                   AND ar-invl.ord-no  EQ oe-ordl.ord-no
                  AND ar-invl.i-no     EQ oe-ordl.i-no
                 NO-ERROR.
            iInv# = IF AVAILABLE ar-invl THEN ar-invl.inv-no ELSE 0.

            IF iInv# EQ 0  THEN DO:
                FIND FIRST inv-line NO-LOCK
                     WHERE inv-line.company EQ oe-ordl.company
                       AND inv-line.ord-no  EQ oe-ordl.ord-no
                       AND inv-line.i-no    EQ oe-ordl.i-no
                     NO-ERROR.
                iInv# = IF AVAILABLE inv-line THEN inv-line.inv-no ELSE 0.
            END.  /*IF iInv# = 0 */
            ASSIGN
                dOrder%Profit = dBoardProfit / dExtPrice
                dMSFRec       = dOrderedMSF / iBoardPOQty * iBoardTotalQty              
                dFGExtPrice   = oe-ordl.price / 1000 * iJobShipQty
                dPORecCost    = dBoardTotalCost / iBoardPOQty * iBoardTotalQty
                dProfitSold$  = dFGExtPrice - dPORecCost 
                dProfitSold%  = dProfitSold$ / dFGExtPrice
                iUnitsBoard   = iQtyLft / iBoardPOQty
                dUnitLoss$    = iBoardTotalQty * iUnitsBoard - iJobShipQty
                dLoss%        = dUnitLoss$ / (iBoardTotalQty * iUnitsBoard )
                .
            IF dOrder%Profit EQ ? THEN dOrder%Profit = 0.
            IF dMSFRec       EQ ? THEN dMSFRec       = 0.
            IF dPORecCost    EQ ? THEN dPORecCost    = 0.
            IF dProfitSold$  EQ ? THEN dProfitSold$  = 0.
            IF dProfitSold%  EQ ? THEN dProfitSold%  = 0.
            IF iUnitsBoard   EQ ? THEN iUnitsBoard   = 0.
            IF dUnitLoss$    EQ ? THEN dUnitLoss$    = 0.
            IF dLoss%        EQ ? THEN dLoss%        = 0.

            CREATE ttOrdersBookedByOrderNo.
            ASSIGN
                ttOrdersBookedByOrderNo.orderNo      = oe-ord.ord-no         
                ttOrdersBookedByOrderNo.estNo        = oe-ordl.est-no        
                ttOrdersBookedByOrderNo.jobNo        = oe-ordl.job-no        
                ttOrdersBookedByOrderNo.orddate      = oe-ord.ord-date       
                ttOrdersBookedByOrderNo.custNo       = oe-ord.cust-no        
                ttOrdersBookedByOrderNo.custName     = oe-ord.cust-name      
                ttOrdersBookedByOrderNo.fgItem       = oe-ordl.i-no          
                ttOrdersBookedByOrderNo.fgItemName   = oe-ordl.i-name        
                ttOrdersBookedByOrderNo.fgOrderQty   = iQtyLft             
                ttOrdersBookedByOrderNo.fgCost       = oe-ordl.cost          
                ttOrdersBookedByOrderNo.price        = oe-ordl.price         
                ttOrdersBookedByOrderNo.uom          = oe-ordl.pr-uom        
                ttOrdersBookedByOrderNo.extPrice     = dExtPrice           
                ttOrdersBookedByOrderNo.fgItemProfit = dMargin              
                ttOrdersBookedByOrderNo.poMsf        = dOrderedMSF          
                ttOrdersBookedByOrderNo.fgShipped    = iJobShipQty          
                ttOrdersBookedByOrderNo.poProfit     = dBoardProfit         
                ttOrdersBookedByOrderNo.poNo         = iBoardPO             
                ttOrdersBookedByOrderNo.poQty        = iBoardPOQty          
                ttOrdersBookedByOrderNo.poCost       = dBoardCost           
                ttOrdersBookedByOrderNo.poTotalCost  = dBoardTotalCost      
                ttOrdersBookedByOrderNo.poReceived   = iBoardTotalQty       
                ttOrdersBookedByOrderNo.orderProfit  = dOrder%Profit        
                ttOrdersBookedByOrderNo.msfReceived  = dMSFRec              
                ttOrdersBookedByOrderNo.fgShipDate   = dtFGShipDate          
                ttOrdersBookedByOrderNo.poRecDate    = dtPORecDate           
                ttOrdersBookedByOrderNo.fgExtPrice   = dFGExtPrice          
                ttOrdersBookedByOrderNo.poRecCost    = dPORecCost           
                ttOrdersBookedByOrderNo.profSold     = dProfitSold$         
                ttOrdersBookedByOrderNo.profSoldp    = dProfitSold%         
                ttOrdersBookedByOrderNo.unitBoard    = iUnitsBoard          
                ttOrdersBookedByOrderNo.unitWaste    = dUnitLoss$           
                ttOrdersBookedByOrderNo.lossp        = dLoss%               
                ttOrdersBookedByOrderNo.bolNo        = iBOL#                
                ttOrdersBookedByOrderNo.invoiceNo    = iInv#
                .
            /*===== end of new ===== */
            IF lPrtCont THEN
                IF dMargin NE ? THEN
                dMarginTot = dMarginTot + dMargin.
            dTotOrd[1] = dTotOrd[1] + dExtPrice.
        END.  /* each oe-ordl */

        IF lPrintMiscCharges THEN
        FOR EACH oe-ordm NO-LOCK
            WHERE oe-ordm.company EQ oe-ord.company
              AND oe-ordm.ord-no  EQ oe-ord.ord-no
            BREAK BY oe-ordm.ord-no
            :
            IF oe-ordm.bill EQ "Y" THEN DO:
                dTotOrd[1] = dTotOrd[1] + oe-ordm.amt.
                IF oe-ordm.tax AND dTaxRate EQ 0 THEN
                dTotTax = dTotTax + ROUND((oe-ordm.amt * dTaxRate) / 100,2).
            END.  /*IF oe-ordm.tax*/
        END.  /* each oe-ordm */   

        dTotOrd[2] = dTotOrd[2] + dTotOrd[1].    
    END.  /* each oe-ord */ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pPostBOLCreateInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPostBOLCreateInvoice Procedure 
PROCEDURE pPostBOLCreateInvoice :
/*------------------------------------------------------------------------------
  Purpose:     Post BOL Create Invoice.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/aoaInputDefParams.i}
    
    /* local variables */

    /* subject business logic */
    RUN aoaAppSrv/aoaBL/r-bolpst.p (OUTPUT TABLE ttPostBOLCreateInvoice, ipcCompany, ipiBatch, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pRecapProductCategory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRecapProductCategory Procedure 
PROCEDURE pRecapProductCategory :
/*------------------------------------------------------------------------------
Purpose:     Recap Product Category.rpa
Parameters:  Company, Batch Seq, User ID
Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/includes/pRecapProductCategory.i}

    {aoaAppSrv/includes/pOrdersBookedLogic.i}

    FOR EACH ttRecapProductCategory:
        ASSIGN
            ttRecapProductCategory.priceMSFCurrent = ttRecapProductCategory.amountCurrent / ttRecapProductCategory.sqFtCurrent
            ttRecapProductCategory.priceMSFPeriod  = ttRecapProductCategory.amountPeriod  / ttRecapProductCategory.sqFtPeriod
            .
        IF ttRecapProductCategory.priceMSFCurrent EQ ? THEN ttRecapProductCategory.priceMSFCurrent = 0.
        IF ttRecapProductCategory.priceMSFPeriod  EQ ? THEN ttRecapProductCategory.priceMSFPeriod  = 0.
    END. /* each ttrecap */

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
        /* Post BOL Create Invoice.rpa */
        WHEN "r-bolpst." THEN
        RETURN TEMP-TABLE ttPostBOLCreateInvoice:HANDLE.
        /* Invoice Post Update GL.rpa */
        WHEN "r-inve&p." THEN
        RETURN TEMP-TABLE ttInvoicePostUpdateGL:HANDLE.
        /* Recap Product Category.rpa */
        WHEN "recappc." THEN
        RETURN TEMP-TABLE ttRecapProductCategory:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fInvoicePostUpdateGL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fInvoicePostUpdateGL Procedure 
FUNCTION fInvoicePostUpdateGL RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose:  Post BOL Create Invoice.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttInvoicePostUpdateGL.
    
    RUN pInvoicePostUpdateGL (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttInvoicePostUpdateGL:HANDLE .

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
    EMPTY TEMP-TABLE ttRecapProductCategory.
    
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

&IF DEFINED(EXCLUDE-fPostBOLCreateInvoice) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPostBOLCreateInvoice Procedure 
FUNCTION fPostBOLCreateInvoice RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose:  Post BOL Create Invoice.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttPostBOLCreateInvoice.
    
    RUN pPostBOLCreateInvoice (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttPostBOLCreateInvoice:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fRecapProductCategory) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fRecapProductCategory Procedure 
FUNCTION fRecapProductCategory RETURNS HANDLE
    ( ipcCompany AS CHARACTER,
      ipiBatch   AS INTEGER,
      ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
Purpose:  Recap Product Category.rpa
Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttOrdersBooked.
    EMPTY TEMP-TABLE ttRecapProductCategory.
    
    RUN pRecapProductCategory (ipcCompany, ipiBatch, ipcUserID).
    
    RETURN TEMP-TABLE ttRecapProductCategory:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

