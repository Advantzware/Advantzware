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

/* Orders Booked by Order No.rpa */
DEFINE TEMP-TABLE ttOrderBookByOrderNo NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD orderNo      AS INTEGER   LABEL "OrderNo"        FORMAT ">>>>>>"  
    FIELD estNo        AS CHARACTER LABEL "EstNo"          FORMAT "X(8)"
    FIELD jobNo        AS CHARACTER LABEL "JobNo"          FORMAT "X(9)"
    FIELD orddate      AS DATE      LABEL "Date"           FORMAT 99/99/9999
    FIELD cusNo        AS CHARACTER LABEL "CustNo"         FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Name"           FORMAT "X(30)"
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
    FIELD bolNo        AS INTEGER   LABEL "BOL#"           FORMAT ">>>>>>>>"
    FIELD invoiceNo    AS INTEGER   LABEL "Invoice#"       FORMAT ">>>>>>"
    .
/* Orders Booked by Order No.rpa */

/* Orders Booked.rpa */
DEFINE TEMP-TABLE ttOrdersBooked NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD dueDate      AS DATE      LABEL "Due Date"       FORMAT 99/99/9999
    FIELD orderNo      AS INTEGER   LABEL "Order"          FORMAT ">>>>>>>"
    FIELD custNo       AS CHARACTER LABEL "Customer"       FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD salesRep     AS CHARACTER LABEL "Sales Rep"      FORMAT "X(3)"
    FIELD salesRepName AS CHARACTER LABEL "Sales Rep Name" FORMAT "X(30)"
    FIELD commPer      AS DECIMAL   LABEL "Comm"           FORMAT ">>>>>9.99"
    FIELD prodCode     AS CHARACTER LABEL "ProdCode"       FORMAT "x(8)"
    FIELD fgItemNo     AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD fgItemName   AS CHARACTER LABEL "FG Item Name"   FORMAT "X(30)"
    FIELD qtyOrdEa     AS INTEGER   LABEL "Qty Ordered EA" FORMAT ">,>>>,>>>"
    FIELD sqFit        AS DECIMAL   LABEL "SqFeet"         FORMAT ">>,>>>.999"
    FIELD totalSqfit   AS DECIMAL   LABEL "Total Sq Ft M"  FORMAT "->,>>>.999"
    FIELD msfPrice     AS DECIMAL   LABEL "MSF"            FORMAT "->>,>>9.99"
    FIELD price        AS DECIMAL   LABEL "Price"          FORMAT ">>>,>>9.99<<<<"
    FIELD orderAmount  AS DECIMAL   LABEL "Order Amt"      FORMAT "->,>>>,>>9.99"
    FIELD profitPer    AS DECIMAL   LABEL "Profit"         FORMAT "->>,>>9.9"
    FIELD totalTons    AS DECIMAL   LABEL "Total Tons"     FORMAT "->,>>>.9"
    FIELD ton          AS DECIMAL   LABEL "Ton"            FORMAT "->>,>>9.99"
    FIELD vUserID      AS CHARACTER LABEL "UserID"         FORMAT "x(8)"
    FIELD custPartNo   AS CHARACTER LABEL "Cust Part No"   FORMAT "x(15)"
    .

DEFINE TEMP-TABLE wkrecap NO-UNDO /* recap by product category */
    FIELD procat      LIKE itemfg.procat   LABEL "Cat"
    FIELD t-sqft      LIKE itemfg.t-sqft   LABEL "Sq Ft"     EXTENT 2 FORMAT ">>,>>>.999"
    FIELD t-tons      AS DECIMAL           LABEL "Tons"      EXTENT 2 FORMAT "->,>>>.9"
    FIELD revenue     LIKE oe-ordl.t-price LABEL "Amount"    EXTENT 2 
    FIELD price-per-m AS DECIMAL           LABEL "PriceMSF"  EXTENT 2
    FIELD price-per-t AS DECIMAL           LABEL "PriceTON"  EXTENT 2
    FIELD num-of-ord  AS INTEGER           LABEL "No Orders"
    .

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD ord-no  LIKE oe-ord.ord-no
    FIELD line    LIKE oe-ordl.line
    FIELD sman    AS CHARACTER FORMAT "x(3)"
    FIELD item-n  LIKE itemfg.i-name   LABEL "Item Description" FORMAT "x(27)"
    FIELD procat  LIKE itemfg.procat   LABEL "Prod Code"
    FIELD qty     LIKE oe-ordl.qty     LABEL "QTY Ordered/EA"   FORMAT ">,>>>,>>>"
    FIELD sqft    LIKE itemfg.t-sqft   LABEL "Sq Ft"            FORMAT ">>,>>>.999"
    FIELD t-sqft  LIKE itemfg.t-sqft   LABEL "Tot Sq Ft/M"      FORMAT "->,>>>.999"
    FIELD t-tons  AS DECIMAL           LABEL "Tot Tons"         FORMAT "->,>>>.9"
    FIELD price   LIKE oe-ordl.price                            FORMAT ">>>,>>9.99<<<<"
    FIELD revenue LIKE oe-ordl.t-price LABEL "Order Amount"
    FIELD misc    AS LOGICAL
    FIELD cost    AS DECIMAL
    FIELD comm    AS DECIMAL           LABEL "Comm Pct"
    FIELD margin  AS DECIMAL
    .
/* Orders Booked.rpa */

/* Open Order Report.rpa */  
DEFINE TEMP-TABLE ttOpenOrderReport NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD rep         AS CHARACTER LABEL "Rep"              FORMAT "x(3)"
    FIELD custNo      AS CHARACTER LABEL "Cust No"          FORMAT "x(8)"
    FIELD lineDueDate AS DATE      LABEL "Line Due Dt"      FORMAT 99/99/9999
    FIELD relDueDate  AS DATE      LABEL "Rel Due Dt"       FORMAT 99/99/9999
    FIELD custPartNo  AS CHARACTER LABEL "Cust Part"        FORMAT "x(15)"
    FIELD itemDesc    AS CHARACTER LABEL "Item Description" FORMAT "x(30)"
    FIELD fgItemNo    AS CHARACTER LABEL "FG Item "         FORMAT "x(15)"
    FIELD orderNo     AS INTEGER   LABEL "Order No"         FORMAT ">>>>>>"
    FIELD cadNo       AS CHARACTER LABEL "CAD"              FORMAT "x(15)"
    FIELD poNo        AS CHARACTER LABEL "PO No"            FORMAT "x(10)"
    FIELD orderQty    AS INTEGER   LABEL "Order Qty"        FORMAT "->,>>>,>>9"
    FIELD qtyOnhand   AS INTEGER   LABEL "Qty OnHand"       FORMAT "->,>>>,>>9"
    FIELD qtyShipped  AS INTEGER   LABEL "Qty Shipped"      FORMAT "->,>>>,>>9"
    FIELD qtyActRel   AS INTEGER   LABEL "Qty ActRel"       FORMAT "->,>>>,>>9"
    FIELD qtyWIP      AS INTEGER   LABEL "Qty WIP"          FORMAT "->,>>>,>>9"
    FIELD qtyAvail    AS INTEGER   LABEL "Qty Avail"        FORMAT "->,>>>,>>9"
    FIELD unit        AS INTEGER   LABEL "Unit"             FORMAT ">>>>9"
    FIELD pallet      AS INTEGER   LABEL "Pallet"           FORMAT ">>9"
    .
DEFINE TEMP-TABLE tt-fg-bin NO-UNDO LIKE fg-bin.
/* Open Order Report.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

    /* subject business logic */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBooked) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBooked Procedure 
PROCEDURE pOrdersBooked :
/*------------------------------------------------------------------------------
  Purpose:     Orders Booked.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pOrdersBooked.i}
    
    /* local variables */

    /* subject business logic */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pOrdersBookedByOrderNo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pOrdersBookedByOrderNo Procedure 
PROCEDURE pOrdersBookedByOrderNo :
/*------------------------------------------------------------------------------
  Purpose:     Orders Booked by Order No.rpa
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pOrdersBookedByOrderNo.i}
    
    /* local variables */

    /* subject business logic */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Order Booked by Order No.rpa */
        WHEN "r-booko#." THEN
        RETURN TEMP-TABLE ttOrderBookByOrderNo:HANDLE.
        /* Order Booked.rpa */
        WHEN "r-booked." THEN
        RETURN TEMP-TABLE ttOrdersBooked:HANDLE.
        /* Open Order Report.rpa */
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
    Purpose:  Open Order Report.rpa
      Notes:  
  ------------------------------------------------------------------------------*/
      EMPTY TEMP-TABLE ttOpenOrderReport.

      RUN pOpenOrderReport (ipcCompany, ipiBatch, ipcUserID).

      RETURN TEMP-TABLE ttOpenOrderReport:HANDLE .

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
    Purpose:  Orders Booked.rpa
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
    Purpose:  Orders Booked by Order No.rpa
      Notes:  
  ------------------------------------------------------------------------------*/
      EMPTY TEMP-TABLE ttOrderBookByOrderNo.

      RUN pOrdersBookedByOrderNo (ipcCompany, ipiBatch, ipcUserID).

      RETURN TEMP-TABLE ttOrderBookByOrderNo:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

